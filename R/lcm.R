lcm <- function(data, Npsem, lrnrs, V) {
    checkmate::assertDataFrame(data[, Npsem$all_vars()], any.missing = FALSE)
    checkmate::assertR6(Npsem, "lcm_Npsem")
    checkmate::assertR6(lrnrs, "Lrnr_base")
    checkmate::assertNumber(V, lower = 2, upper = nrow(data) - 1)

    Task <- lcm_Task$new(data, Npsem)
    Folds <- lcm_Folds$new(nrow(data), V)
    Rv <- lcm_Rv$new(Task)

    for (t in Npsem$tau:1) {
        CrossFit_D_Lt(Task, t, Rv, Folds, lrnrs)
    }

}
