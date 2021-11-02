inner_loop_1 <- function(Task, t, Rv, Folds, lrnrs) {
    for (v in seq_along(Folds$V)) {
        Tr   <- Folds$Tr(Task$data, v)
        P    <- Folds$P(Task$data, v)
        Tr_a <- Task$augment(Tr, t)                                                             # line 7 in algorithm
        P_a  <- Task$augment(P, t)                                                              # line 8

        Tr_a[["*lcm_tmp_D_Lt_outcome*"]] <- Rv$D_Zt[[t + 1]]

        Q_Lt <- CrossFit(                                                                       # line 9
            Tr_a["*tmp_lcm_mediator_var*" == Task$Npsem$M[t], ],                                # subset operation
            P_a, "*lcm_tmp_D_Lt_outcome*",
            c(Task$Npsem$M[t], Task$Npsem$history("M", t))
        )

        g_t  <- CrossFit(Tr, P, Task$Npsem$A[t], Task$Npsem$history("A", t), "binomial", lrnrs) # line 10
        g_Mt <- CrossFit(Tr, P, Task$Npsem$M[t], Task$Npsem$history("M", t), "binomial", lrnrs) # line 11

        Gp_At <- G(P[[Task$Npsem$A[t]]], g_t, 1)                                                # line 12
        Gs_At <- G(P[[Task$Npsem$A[t]]], 1 - g_t, 0)                                            # line 13
        G_Mt  <- G(P[[Task$Npsem$M[t]]], g_Mt, 1)                                               # line 14

        D_Lt <- D_L()

        Rv$update("D_Lt", D_Lt, t, Folds$valid_augmented_idx(Task, t, v))
    }
}
