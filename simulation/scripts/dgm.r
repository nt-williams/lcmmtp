library(tidyverse)
library(simcausal)

draw_coefs <- function(cats, n_vars, seed) {
    set.seed(seed)
    matrix(runif((cats - 1) * n_vars, -1, 1), ncol = cats - 1)
}

coefs <- data.frame(
    cats = c(3, 2, 3, 4, 3, 2, 3, 4, 2),
    n_vars = c(1, 1, 2, 3, 4, 5, 6, 7, 8),
    seed = c(3234, 982365, 9865, 5109865, 323714, 825, 65, 10965, 51865)
) %>% pmap(draw_coefs)

ord_prob <- function(coefs_n, ...) {
    coefs <- coefs[[coefs_n]]
    vars  <- data.frame(...)
    lin_pred <- cbind(as.matrix(vars) %*% coefs, 0)
    probs <- exp(lin_pred) / rowSums(exp(lin_pred))
    if(nrow(probs) == 1) {
        return(probs[1, ])
    }
    probs
}

D <- DAG.empty() +
    node("L", t = 1, distr = "rcat.b1",
         probs = ord_prob(1, 1)) +
    node("A", t = 1, distr = "rcat.b0",
         probs = ord_prob(2, L[t])) +
    node("Z", t = 1, distr = "rcat.b1",
         probs = ord_prob(3, L[t], A[t])) +
    node("M", t = 1, distr = "rcat.b1",
         probs = ord_prob(4, Z[t], L[t], A[t])) +
    node("L", t = 2, distr = "rcat.b1",
         probs = ord_prob(5, M[t-1], Z[t-1], L[t-1], A[t-1])) +
    node("A", t = 2, distr = "rcat.b0",
         probs = ord_prob(6, L[t], M[t-1], Z[t-1], L[t-1], A[t-1])) +
    node("Z", t = 2, distr = "rcat.b1",
         probs = ord_prob(7, L[t], A[t], M[t-1], Z[t-1], L[t-1], A[t-1])) +
    node("M", t = 2, distr = "rcat.b1",
         probs = ord_prob(8, Z[t], L[t], A[t], M[t-1], Z[t-1], L[t-1], A[t-1])) +
    node("Y", t = 3, distr = "rcat.b0",
         probs = ord_prob(9, M[t-1], Z[t-1], L[t-1], A[t-1], M[t-2], Z[t-2], L[t-2], A[t-2]))

D <- set.DAG(D, vecfun = "ord_prob")

datagen <- function(n, seed) {
    data <- sim(D, n = as.integer(n), rndseed = seed)
    names(data)[substr(names(data), 1, 1) == 'Y'] <- 'Y'
    data
}

true <- function() {

    data_points <- expand.grid(L_1 = 1:3, A_1 = 0:1, Z_1 = 1:3, M_1 = 1:4,
                               L_2 = 1:3, A_2 = 0:1, Z_2 = 1:3, M_2 = 1:4)

    datx <- mutate(data_points,
                   QL2 = ord_prob(9, M_2, Z_2, L_2, A_2, M_1, Z_1, L_1, A_1)[, 2])

    pZ2 <- with(datx, ord_prob(7, L_2, A_2, M_1, Z_1, L_1, A_1))

    datx <- datx %>%
        mutate(pZ2 = case_when(Z_2 == 1 ~ pZ2[, 1],
                               Z_2 == 2 ~ pZ2[, 2],
                               Z_2 == 3 ~ pZ2[, 3])) %>%
        group_by(L_1, A_1, Z_1, M_1, L_2, A_2, M_2) %>%
        summarise(QZ2 = sum(QL2 * pZ2))

    pL2 <- with(datx, ord_prob(5, M_1, Z_1, L_1, A_1))

    datx  <- datx %>% ungroup() %>%
        mutate(pL2 = case_when(L_2 == 1 ~ pL2[, 1],
                               L_2 == 2 ~ pL2[, 2],
                               L_2 == 3 ~ pL2[, 3])) %>%
        group_by(L_1, A_1, Z_1, M_1, A_2, M_2) %>%
        summarise(QL1 = sum(QZ2 * pL2))

    pZ1 <- with(datx, ord_prob(3, L_1, A_1))

    datx <- datx %>%  ungroup() %>%
        mutate(pZ1 = case_when(Z_1 == 1 ~ pZ1[, 1],
                               Z_1 == 2 ~ pZ1[, 2],
                               Z_1 == 3 ~ pZ1[, 3])) %>%
        group_by(L_1, A_1, M_1, A_2, M_2) %>%
        summarise(QZ1 = sum(QL1 * pZ1))

    pL1 <- ord_prob(1, 1)

    datx  <- datx %>% ungroup() %>%
        mutate(pL1 = case_when(L_1 == 1 ~ pL1[1],
                               L_1 == 2 ~ pL1[2],
                               L_1 == 3 ~ pL1[3])) %>%
        group_by(A_1, M_1, A_2, M_2) %>%
        summarise(QL0 = sum(QZ1 * pL1))

    QL0 <- datx

    pM2 <- with(data_points, ord_prob(8, Z_2, L_2, A_2, M_1, Z_1, L_1, A_1))

    datx <- data_points %>%
        mutate(pM2 = case_when(M_2 == 1 ~ pM2[, 1],
                               M_2 == 2 ~ pM2[, 2],
                               M_2 == 3 ~ pM2[, 3],
                               M_2 == 4 ~ pM2[, 4]),
               pZ2 = case_when(Z_2 == 1 ~ pZ2[, 1],
                               Z_2 == 2 ~ pZ2[, 2],
                               Z_2 == 3 ~ pZ2[, 3])) %>%
        group_by(L_1, A_1, Z_1, M_1, L_2, A_2, M_2) %>%
        summarise(QM2 = sum(pM2 * pZ2)) %>%
        ungroup() %>%
        mutate(pL2 = case_when(L_2 == 1 ~ pL2[, 1],
                               L_2 == 2 ~ pL2[, 2],
                               L_2 == 3 ~ pL2[, 3])) %>%
        group_by(L_1, A_1, Z_1, M_1, A_2, M_2) %>%
        summarise(QM2 = sum(QM2 * pL2))

    pM1 <- with(datx, ord_prob(4, Z_1, L_1, A_1))

    datx <- datx %>% ungroup() %>%
        mutate(pM1 = case_when(M_1 == 1 ~ pM1[, 1],
                               M_1 == 2 ~ pM1[, 2],
                               M_1 == 3 ~ pM1[, 3],
                               M_1 == 4 ~ pM1[, 4]),
               pZ1 = case_when(Z_1 == 1 ~ pZ1[, 1],
                               Z_1 == 2 ~ pZ1[, 2],
                               Z_1 == 3 ~ pZ1[, 3])) %>%
        group_by(L_1, A_1, M_1, A_2, M_2) %>%
        summarise(QM1 = sum(QM2 * pZ1 * pM1)) %>%
        ungroup() %>%
        mutate(pL1 = case_when(L_1 == 1 ~ pL1[1],
                               L_1 == 2 ~ pL1[2],
                               L_1 == 3 ~ pL1[3])) %>%
        group_by(A_1, M_1, A_2, M_2) %>%
        summarise(QM0 = sum(QM1 * pL1))

    QM0 <- datx

    QL0_1 <- QL0 %>% ungroup() %>% filter(A_1 == 1, A_2 == 1) %>% select(-A_1, -A_2)
    QL0_0 <- QL0 %>% ungroup() %>% filter(A_1 == 0, A_2 == 0) %>% select(-A_1, -A_2)
    QM0_1 <- QM0 %>% ungroup() %>% filter(A_1 == 1, A_2 == 1) %>% select(-A_1, -A_2)
    QM0_0 <- QM0 %>% ungroup() %>% filter(A_1 == 0, A_2 == 0) %>% select(-A_1, -A_2)

    theta11 <- merge(QL0_1, QM0_1) %>% summarise(theta11 = sum(QL0 * QM0))
    theta10 <- merge(QL0_1, QM0_0) %>% summarise(theta10 = sum(QL0 * QM0))
    theta01 <- merge(QL0_0, QM0_1) %>% summarise(theta01 = sum(QL0 * QM0))
    theta00 <- merge(QL0_0, QM0_0) %>% summarise(theta00 = sum(QL0 * QM0))

    c(theta11, theta10, theta01, theta00)
}