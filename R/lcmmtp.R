#' Causal Mediation with Longitudinal Data Using Modified Treatment Policies
#'
#' @param data A data frame
#' @param vars An `lcm_Vars` object mapping observed variables to the assumed NPSEM
#' @param a_prime Value of a', 0 or 1.
#' @param a_star Value of a*, 0 or 1.
#' @param learners
#' @param folds The number of folds for cross-fitting
#'
#' @return An object of class `lcm`
#' @export
#'
#' @examples
lcmmtp <- function(data, a_prime, a_star, Npsem, learners, folds) {
    checkmate::assertDataFrame(data[, Npsem$all_vars()], any.missing = FALSE)
    checkmate::assertR6(Npsem, "lcm_Vars")
    checkmate::assertNumber(folds, lower = 1, upper = nrow(data) - 1)
    require("mlr3superlearner")

    Task <- lcm_Task$new(data, Npsem)
    Folds <- lcm_Folds$new(nrow(data), folds)

    for (t in Npsem$tau:1) {
        CrossFit_D_Lt   (Task, t, a_prime[t], a_star[t], Folds, learners)
        CrossFit_D_Zt_Mt(Task, t, a_prime[t], a_star[t], Folds, learners)
    }

    bar_M <- expand.grid(lapply(1:Npsem$tau, function(t) Task$unique_M()))
    names(bar_M) <- g("lcm_med_{1:Npsem$tau}")
    data.table::setDT(bar_M)

    nuis <- slider::slide(bar_M, function(bar_m) {
        comp <- lapply(1:Folds$V, function(v) {
            P_v <- Folds$P(data.table::merge.data.table(bar_m, Task$augmented, all.x = TRUE), v)
            list(
                lambda_v = mean(P_v[["lcm_D_M1"]]),
                theta_v = mean(P_v[["lcm_D_Z1"]])
            )
        })

        dat    <- merge(Task$augmented, bar_m)
        theta  <-  mean(vapply(comp, function(x) x$theta_v, FUN.VALUE = 1))
        lambda <-  mean(vapply(comp, function(x) x$lambda_v, FUN.VALUE = 1))
        S <- (dat$lcm_D_Z1 - theta) * lambda + (dat$lcm_D_M1 - lambda) * theta

        return(list(theta = theta, lambda = lambda, S = S))
    })

    S <- rowSums(sapply(nuis, function(m)m$S))

    ans <- list(
        theta = sum(vapply(nuis, function(m) m$theta * m$lambda, 1)),
        var = var(S) / Task$n,
        S = S
    )

    class(ans) <- "lcm"
    ans
}
