CrossFit_D_Lt <- function(Task, t, Rv, Folds, lrnrs) {
    for (v in seq_along(Folds$V)) {
        Tr   <- Folds$Tr(Task$data, v)
        Tr_a <- Task$augment(Tr, t)                                                               # line 7 in algorithm
        P_a  <- Task$augment(Folds$P(Task$data, v), t)                                            # line 8 __

        Tr_a[["*lcm_tmp_D_Zt_outcome*"]] <-
            Rv$D_Zt[Folds$Tr_augmented_idx(Task, t, v), t + 1]

        if (t == Task$Npsem$tau) {
            covars <- Task$Npsem$history("M", t)
        } else {
            covars <- c(
                glue::glue("*tmp_lcm_mediator_var_{Task$Npsem$tau:(t + 1)}*"),
                Task$Npsem$history("M", t)
            )
        }

        Q_Lt <- CrossFit(                                                                        # line 9 __
            Tr_a[
                Tr_a[[glue::glue("*tmp_lcm_mediator_var_{t}*")]] == Tr_a[[Task$Npsem$M[t]]],
            ],                                                                                   # subset operation
            P_a, "*lcm_tmp_D_Zt_outcome*",
            covars,
            Task$type, lrnrs
        )

        g_t  <- CrossFit(Tr, P_a, Task$Npsem$A[t], Task$Npsem$history("A", t), "binomial", lrnrs) # line 10 __
        g_Mt <- CrossFit(Tr, P_a, Task$Npsem$M[t], Task$Npsem$history("M", t), "binomial", lrnrs) # line 11 __

        Gp_At <- G(P_a[[Task$Npsem$A[t]]], g_t, 1)                                                # line 12 __
        Gs_At <- G(P_a[[Task$Npsem$A[t]]], 1 - g_t, 0)                                            # line 13 __
        G_Mt  <- G(P_a[[Task$Npsem$M[t]]], g_Mt, 1)                                               # line 14 __

        D_Lt <- D_L()
        Rv$update("D_Lt", D_Lt, t, Folds$P_augmented_idx(Task, t, v))
    }
}
