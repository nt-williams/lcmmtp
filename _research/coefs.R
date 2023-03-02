draw_coefs <- function(cats, n_vars, seed, lower = -0.2, upper = 0.2) {
    set.seed(seed)
    # coefs <- matrix(rnorm((cats - 1) * n_vars, 0, 0.05), ncol = cats - 1)
    # pmin(pmax(coefs, lower), upper)
    matrix(runif((cats - 1) * n_vars, lower, upper), ncol = cats - 1)
}

no_int <- \(vars) ncol(combn(vars, 2))

coefs <- data.frame(cats = c(3, 2, 3, 3, 3, 2, 3, 3, 2),
                    n_vars = c(1, 1, 2 + no_int(2), 3 + no_int(3), 4 + no_int(4), 5 + no_int(5), 6 + no_int(6), 7 + no_int(7), 8 + no_int(8)),
                    seed = sample(1e5, 9),
                    lower = rep(-0.2, 9),
                    upper = rep(0.2, 9))

coefs <- purrr::pmap(coefs, draw_coefs)

saveRDS(coefs, "_research/data/coefs2.rds")
