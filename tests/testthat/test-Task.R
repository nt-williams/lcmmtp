n <- 50
tmp <- data.frame(
    W = rbinom(n, 1, 0.5),
    L_1 = rbinom(n, 1, 0.5),
    A_1 = rbinom(n, 1, 0.5),
    Z_1 = rbinom(n, 1, 0.5),
    M_1 = rbinom(n, 1, 0.5),
    L_2 = rbinom(n, 1, 0.5),
    A_2 = rbinom(n, 1, 0.5),
    Z_2 = rbinom(n, 1, 0.5),
    M_2 = rbinom(n, 1, 0.5),
    Y = rbinom(n, 1, 0.5)
)

Np <- Npsem$new(
    W = c("W"),
    L = list(c("L_1"), c("L_2")),
    A = c("A_1", "A_2"),
    Z = list(c("Z_1"), c("Z_2")),
    M = c("M_1", "M_2"),
    Y = "Y"
)

Folds <- lcm_Folds$new(tmp, 10)
Task <- lcm_Task$new(tmp, Np)

Task$augment(Task$data, 2)

Tr   <- Folds$Tr(Task$data, 1)
P    <- Folds$P(Task$data, 1)
Tr_a <- Task$augment(Tr, 2)                                          # line 7 in algorithm
P_a  <- Task$augment(P, 2)

t <- 2
lrnrs <- sl3::make_learner(sl3::Lrnr_glm)

g_t  <- CrossFit(Tr, P, Task$Npsem$A[t], Task$Npsem$history("A", t), "binomial", lrnrs) # line 10
g_Mt <- CrossFit(Tr, P, Task$Npsem$M[t], Task$Npsem$history("M", t), "binomial", lrnrs) # line 11

Gp_At <- G(P[[Task$Npsem$A[t]]], g_t, 1)                             # line 12
Gs_At <- G(P[[Task$Npsem$A[t]]], 1 - g_t, 0)                             # line 13
G_Mt  <- G(P[[Task$Npsem$M[t]]], g_Mt, 1)

Folds$valid_augmented_idx(Task, 2, 1)
