draw_coefs <- function(cats, n_vars, seed, lower = -0.5, upper = 0.5) {
    set.seed(seed)
    coefs <- matrix(rnorm((cats - 1) * n_vars, 0, 0.5), ncol = cats - 1)
    coefs <- pmin(pmax(coefs, lower), upper)
    return(coefs)
}

coefs <- data.frame(
    cats = c(3, 2, 3, 3, 3, 2, 3, 3, 2),
    n_vars = c(1, 1, 2, 3, 4, 5, 6, 7, 8),
    seed = sample(1e5, 9),
    ## lower = c(rep(-0.5, 8), -3),
    ## upper = c(rep(0.5, 8), 3)
    lower = rep(-0.5, 9),
    upper = rep(0.5, 9)
) %>% purrr::pmap(draw_coefs)

saveRDS(coefs, "simulation/data/coefs.rds")
