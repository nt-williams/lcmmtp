#' Title
#'
#' @param data
#' @param treatment
#' @param outcome
#' @param mediator
#' @param competingRisks
#' @param baselineConfounders
#' @param timeVaryConfounders
#' @param mediatorOutcomeConfounders
#' @param censoring
#' @param d_prime
#' @param d_star
#' @param id
#' @param control
#'
#' @returns
#' @export
#'
#' @examples
lcmmtp <- function(data,
                   treatment,
                   outcome,
                   mediator,
                   competingRisks,
                   baselineConfounders,
                   timeVaryConfounders,
                   mediatorOutcomeConfounders,
                   censoring,
                   d_prime, d_star,
                   id = NULL,
                   control = .lcmmtp_control()) {

    # Create variables object
    variables <- Variables$new(
        baselineConfounders = baselineConfounders,
        timeVaryConfounders = timeVaryConfounders,
        treatment = treatment,
        mediatorOutcomeConfounders = mediatorOutcomeConfounders,
        mediator = mediator,
        outcome = outcome,
        competingRisks = competingRisks,
        censoring = censoring
    )

    # Create a task object
    task <- EstimationTask$new(data, variables, id, d_prime, d_star)

    # Create a folds object
    folds <- CrossFitFolds$new(nrow(data), control$folds, id)

    for (time in variables$timeHorizon:1) {
        # Estimate outcome regression
        OutcomeRegression(task, time, folds, control)
        # Estimate Riesz Representers
        CrossFitDensityRatios(task, time, folds, control)
        # DR transformation of outcome regression
        task$augmented[[g("lcmmtp_D_L{time}")]] <- D_Lt(task$augmented, time, variables$timeHorizon, control$trim)

        # Integrate out the mediator-outcome confounder through regression
        MarginalizeMediatorOutcomeConfounder(task, time, folds, d_prime, control)

        # DR transformation of the mediator-outcome confounder regression
        task$augmented[[g("lcmmtp_D_Z{time}")]] <- D_Zt(task$augmented, time, variables$timeHorizon, control$trim)

        # Estimate mediator regression
        MediatorRegression(task, time, folds, d_star, control)
        # DR transformation of the conditional mediator expectation
        task$augmented[[g("lcmmtp_D_M{time}")]] <- D_Mt(task$augmented, time, variables$timeHorizon, variables$mediator, control$trim)
    }

    bar_M <- expand.grid(lapply(1:variables$timeHorizon, function(t) task$uniqueMediatorValues()))
    names(bar_M) <- g("lcmmtp_med_{1:variables$timeHorizon}")
    data.table::setDT(bar_M)

    nuis <- slider::slide(bar_M, function(bar_m) {
        comp <- lapply(1:folds$numberFolds, function(v) {
            validation <- folds$validation(data.table::merge.data.table(bar_m, task$augmented, all.x = TRUE), v)
            list(
                lambda_v = mean(validation[["lcmmtp_D_M1"]]),
                theta_v = mean(validation[["lcmmtp_D_Z1"]])
            )
        })

        dat <- merge(task$augmented, bar_m)
        theta <- mean(vapply(comp, function(x) x$theta_v, FUN.VALUE = 1))
        lambda <- mean(vapply(comp, function(x) x$lambda_v, FUN.VALUE = 1))

        S <- (dat$lcmmtp_D_Z1 - theta) * lambda + (dat$lcmmtp_D_M1 - lambda) * theta

        return(list(theta = theta, lambda = lambda, S = S))
    })

    S <- rowSums(sapply(nuis, function(m) m$S))
    S <- split(S, task$data$lcmmtp_ID)
    j <- length(S)

    ans <- list(
        theta = sum(vapply(nuis, function(m) m$theta * m$lambda, 1)),
        var = sqrt(var(vapply(S, function(x) mean(x), 1)) / j),
        S = as.vector(unlist(S))
    )

    # also output all the density ratios
    ife::ife(ans$theta, as.vector(unlist(S)))
}
