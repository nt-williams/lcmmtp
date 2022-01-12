#' Causal Mediation with Longitudinal Mediators, Treatments, and Confounders
#'
#' @param data A data frame
#' @param Npsem An `lcm_Npsem` object mapping observed variables to the assumed NPSEM
#' @param lrnrs An object inhering from `Lrnr_base` from the sl3 package
#' @param V The number of folds for cross-fitting
#'
#' @return An object of class `lcm`
#' @export
#'
#' @examples
#' TO DO
lcm <- function(data, a_prime, a_star, Npsem, lrnrs, V) {
    checkmate::assertDataFrame(data[, Npsem$all_vars()], any.missing = FALSE)
    checkmate::assertR6(Npsem, "lcm_Npsem")
    checkmate::assertR6(lrnrs, "Lrnr_base")
    checkmate::assertNumber(V, lower = 2, upper = nrow(data) - 1)

    Task <- lcm_Task$new(data, Npsem)
    Folds <- lcm_Folds$new(nrow(data), V)

    for (t in Npsem$tau:1) {
        CrossFit_D_Lt   (Task, t, a_prime, a_star, Folds, lrnrs)
        CrossFit_D_Zt_Mt(Task, t, a_prime, a_star, Folds, lrnrs)
    }

    bar_M <- expand.grid(lapply(1:Npsem$tau, function(t) Task$unique_M()))
    names(bar_M) <- g("*lcm_med_{1:Npsem$tau}*")
    data.table::setDT(bar_M)

    nuis <- slider::slide(bar_M, function(bar_m) {
        comp <- lapply(1:Folds$V, function(v) {
            P_v <- Folds$P(data.table::merge.data.table(bar_m, Task$augmented, all.x = TRUE), v)
            list(
                lambda_v = mean(P_v[["lcm_D_M1"]]),
                rho_v = mean(P_v[["lcm_D_Z1"]]) # SHOULD THIS BE L1 INSTEAD OF Z1?
            )
        })

        list(
            rho = mean(vapply(comp, function(x) x$rho_v, FUN.VALUE = 1)),
            lambda = mean(vapply(comp, function(x) x$lambda_v, FUN.VALUE = 1))
        )
    })

    theta <- sum(vapply(nuis, function(m) m$rho * m$lambda, 1))
    theta

    # add variance
    # add confidence intervals
    # n = 500, 5000
    # compute bias * root n -> 0
    # coverage is nominal at 0.95

}
