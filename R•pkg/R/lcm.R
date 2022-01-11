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
lcm <- function(data, Npsem, lrnrs, V) {
    checkmate::assertDataFrame(data[, Npsem$all_vars()], any.missing = FALSE)
    checkmate::assertR6(Npsem, "lcm_Npsem")
    checkmate::assertR6(lrnrs, "Lrnr_base")
    checkmate::assertNumber(V, lower = 2, upper = nrow(data) - 1)

    Task <- lcm_Task$new(data, Npsem)
    Folds <- lcm_Folds$new(nrow(data), V)
    # Rv <- lcm_Rv$new(Task)

    for (t in Npsem$tau:1) {
        CrossFit_D_Lt   (Task, t, Folds, lrnrs)
        CrossFit_D_Zt_Mt(Task, t, Folds, lrnrs)
    }

    # Task$data[
    #     apply(
    #         Task$data[, Npsem$M] == Task$data[, g("*lcm_med_{1:Npsem$tau}*")],
    #         1, all
    #     ),
    # ]

    bar_M <- expand.grid(lapply(1:Npsem$tau, function(t) Task$unique_M()))
    names(bar_M) <- Npsem$M

    nuis <- apply(bar_M, 1, function(bar_m) {
        comp <- lapply(1:Folds$V, function(v) {
            P_v <- Folds$P(merge(bar_m, Task$data, all.x = TRUE), v)
            list(
                lambda_v = mean(P_v[["lcm_D_M1"]]),
                rho_v = mean(P_v[["lcm_D_Z1"]])
            )
        })

        list(
            rho = mean(vapply(comp, function(x) x$rho_v, FUN.VALUE = 1)),
            lambda = mean(vapply(comp, function(x) x$lambda_v, FUN.VALUE = 1))
        )
    }, simplify = FALSE)

    # nuis <- lapply(Task$unique_M(), function(m) { # NEED TO ASK ABOUT THIS SUBSETTING
    #     comp <- lapply(1:Folds$V, function(v) {
    #         P_v <- Folds$P(Task$data, v)
    #         list(
    #             lambda_v = mean(P_v[P_v[, Npsem$M[1]] == m, "lcm_D_M1"]),
    #             rho_v = mean(P_v[P_v[, Npsem$M[1]] == m, "lcm_D_Z1"])
    #         )
    #     })
    #
    #     list(
    #         rho = mean(vapply(comp, function(x) x$rho_v, FUN.VALUE = 1)),
    #         lambda = mean(vapply(comp, function(x) x$lambda_v, FUN.VALUE = 1))
    #     )
    # })

    theta <- sum(vapply(nuis, function(m) m$rho * m$lambda, 1))
    theta
}
