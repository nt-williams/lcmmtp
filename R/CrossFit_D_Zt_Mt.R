CrossFit_D_Zt_Mt <- function(Task, t, Rv, Folds, lrnrs) {
    for (v in seq_along(Folds$V)) {
        Tr   <- Folds$Tr(Task$data, v)
        P    <- Folds$P(Task$data, v)
        Tr_a <- Task$augment(Tr, t)                          # line 7 in algorithm
        P_a  <- Task$augment(P, t)                           # line 8

        Tr_a[["*lcm_tmp_D_Lt_outcome*"]] <- Rv$D_Lt[[t]]
        Tr_a[["*lcm_tmp_D_Lt_outcome*"]] <- Rv$D_Lt[[t]]

        Q_Zt <- CrossFit(                                    # line 20 __
            Tr_a[Tr_a[[Task$Npsem$A[t]]] == 1],              # subset operation
            P_a, "*lcm_tmp_D_Lt_outcome*",
            c(Task$Npsem$M[t], Task$Npsem$history("M", t)),
            "continuous", lrnrs
        )

        Q_Mt <- CrossFit(                                    # line 21 __
            Tr_a[Tr_a[[Task$Npsem$A[t]]] == 0],              # subset operation
            P_a, "*lcm_tmp_D_Lt_outcome*",
            c(Task$Npsem$M[t], Task$Npsem$history("M", t)),
            "continuous", lrnrs
        )

        D_Lt <- D_L()

        Rv$update("D_Lt", D_Lt, t, Folds$valid_augmented_idx(Task, t, v))
    }
}
