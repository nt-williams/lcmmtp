#' Causal Mediation with Longitudinal Data Using Modified Treatment Policies
#'
#' @param data A data frame containing all necessary variables
#' @param vars An `lcmmtp_variables` object mapping observed variables to
#'  the assumed variable structure.
#' @param d_prime
#' @param d_star
#' @param learners
#' @param folds The number of folds for cross-fitting
#'
#' @return An object of class `lcmmtp`
#' @export
#'
#' @examples
lcmmtp <- function(data, vars, d_prime, d_star, learners, folds) {
    checkmate::assertDataFrame(data[, vars$all_vars()])
    checkmate::assertR6(vars, "lcmmtp_variables")
    checkmate::assertNumber(folds, lower = 1, upper = nrow(data) - 1)
    checkmate::assertFunction(d_prime, nargs = 2)
    checkmate::assertFunction(d_star, nargs = 2)

    require("mlr3superlearner")

    task <- lcmmtp_task$new(data, vars, d_prime, d_star)
    Folds <- lcmmtp_folds$new(nrow(data), folds)

    for (t in vars$tau:1) {
        CrossFit_D_Lt(task, t, Folds, learners)
        CrossFit_D_Zt_Mt(task, d_prime, d_star, t, Folds, learners)
    }

    bar_M <- expand.grid(lapply(1:vars$tau, function(t) task$unique_M()))
    names(bar_M) <- g("lcmmtp_med_{1:vars$tau}")
    data.table::setDT(bar_M)

    nuis <- slider::slide(bar_M, function(bar_m) {
        comp <- lapply(1:Folds$V, function(v) {
            P_v <- Folds$P(data.table::merge.data.table(bar_m, task$augmented, all.x = TRUE), v)
            list(
                lambda_v = mean(P_v[["lcmmtp_D_M1"]]),
                theta_v = mean(P_v[["lcmmtp_D_Z1"]])
            )
        })

        dat    <- merge(task$augmented, bar_m)
        theta  <-  mean(vapply(comp, function(x) x$theta_v, FUN.VALUE = 1))
        lambda <-  mean(vapply(comp, function(x) x$lambda_v, FUN.VALUE = 1))
        S <- (dat$lcmmtp_D_Z1 - theta) * lambda + (dat$lcmmtp_D_M1 - lambda) * theta

        return(list(theta = theta, lambda = lambda, S = S))
    })

    S <- rowSums(sapply(nuis, function(m) m$S))

    ans <- list(
        theta = sum(vapply(nuis, function(m) m$theta * m$lambda, 1)),
        var = var(S) / task$n,
        S = S
    )

    class(ans) <- "lcmmtp"
    ans
}
