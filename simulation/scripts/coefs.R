draw_coefs <- function(cats, n_vars, seed) {
    set.seed(seed)
    matrix(runif((cats - 1) * n_vars, -0.5, 0.5), ncol = cats - 1)
}

coefs <- data.frame(
    cats = c(3, 2, 3, 4, 3, 2, 3, 4, 2),
    n_vars = c(1, 1, 2, 3, 4, 5, 6, 7, 8),
    seed = c(3234, 982365, 9865, 5109865, 323714, 42353, 65, 10965, 51865)
) %>% pmap(draw_coefs)

saveRDS(coefs, "simulation/data/coefs.rds")
