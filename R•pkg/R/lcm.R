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

}
