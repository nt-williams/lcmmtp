draw_coefs <- function(cats, n_vars, seed, lower = -0.5, upper = 0.5) {
    set.seed(seed)
    matrix(runif((cats - 1) * n_vars, lower, upper), ncol = cats - 1)
}

coefs <- data.frame(
    cats = c(3, 2, 3, 3, 3, 2, 3, 3, 2),
    n_vars = c(1, 1, 2, 3, 4, 5, 6, 7, 8),
    seed = c(8656, 982365, 9865, 5109865, 323714, 42353, 65, 10965, 2453),
    lower = c(rep(-0.5, 8), -3),
    upper = c(rep(0.5, 8), 3)
) |> purrr::pmap(draw_coefs)

saveRDS(coefs, "simulation/data/coefs.rds")
