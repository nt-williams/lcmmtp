draw_coefs <- function(cats, n_vars, seed, lower = -0.2, upper = 0.2) {
    set.seed(seed)
    # coefs <- matrix(rnorm((cats - 1) * n_vars, 0, 0.05), ncol = cats - 1)
    # pmin(pmax(coefs, lower), upper)
    matrix(runif((cats - 1) * n_vars, lower, upper), ncol = cats - 1)
}

tmp <- expand.grid(L1 = c(1, 2, 3),
                   A1 = c(0, 1),
                   Z1 = c(1, 2, 3),
                   M1 = c(1, 2, 3),
                   L2 = c(1, 2, 3),
                   A2 = c(0, 1),
                   Z2 = c(1, 2, 3),
                   M2 = c(1, 2, 3),
                   Y = c(0, 1))

tmp <- mutate(tmp, across(c("L1", "Z1", "M1", "L2", "Z2", "M2"), as.factor))
nvarY <- ncol(model.matrix(Y ~ .^2, data = tmp))
nvarM2 <- ncol(model.matrix(M2 ~ (L1 + A1 + Z1 + M1 + L2 + A2 + Z2)^2, data = tmp))
nvarZ2 <- ncol(model.matrix(Z2 ~ (L1 + A1 + Z1 + M1 + L2 + A2)^2, data = tmp))
nvarA2 <- ncol(model.matrix(A2 ~ (L1 + A1 + Z1 + M1 + L2)^2, data = tmp))
nvarL2 <- ncol(model.matrix(L2 ~ (L1 + A1 + Z1 + M1)^2, data = tmp))
nvarM1 <- ncol(model.matrix(M1 ~ (L1 + A1 + Z1)^2, data = tmp))
nvarZ1 <- ncol(model.matrix(Z1 ~ (L1 + A1)^2, data = tmp))
nvarA1 <- ncol(model.matrix(A1 ~ L1, data = tmp))
nvarL1 <- ncol(model.matrix(L1 ~ 1, data = tmp))

coefs <- data.frame(cats = c(3, 2, 3, 3, 3, 2, 3, 3, 2),
                    n_vars = c(nvarL1, nvarA1, nvarZ1, nvarM1, nvarL2, nvarA2, nvarZ2, nvarM2, nvarY),
                    seed = sample(1e5, 9),
                    lower = rep(-0.2, 9),
                    upper = rep(0.2, 9))

coefs <- purrr::pmap(coefs, draw_coefs)

saveRDS(coefs, "_research/data/coefs3.rds")
