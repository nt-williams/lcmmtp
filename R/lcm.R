lcm <- function(data, Npsem, lrnrs, V) {

    assertthat::assert_that(V > 1, msg = "Number of folds must be greater than 1!")

    Task <- lcm_Task$new(data, Npsem)
    Folds <- lcm_Folds$new(nrow(data), V)
    Rv <- lcm_Rv$new(Task)

    for (t in Npsem$tau:1) {
        inner_loop_1(Task, t, Rv, Folds, lrnrs)
    }

}
