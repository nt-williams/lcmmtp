CrossFit_D_Zt_Mt <- function(Task, t, Rv, Folds, lrnrs) {
    for (v in seq_along(Folds$V)) {
        Tr   <- Folds$Tr(Task$data, v)
        Tr_a <- Task$augment(Tr, t)                                                               # line 7 in algorithm
        P_a  <- Task$augment(Folds$P(Task$data, v), t)                                            # line 8 __

        Tr_a[["*lcm_tmp_D_Lt_outcome*"]] <-
            Rv$D_Lt[Folds$Tr_augmented_idx(Task, t, v), t]

        Tr_a[["*lcm_tmp_D_Mt_outcome*"]] <-
            as.numeric(Tr_a[[glue::glue("*tmp_lcm_mediator_var_{t}*")]] == Tr_a[[Task$Npsem$M[t]]]) *
            Rv$D_Mt[Folds$Tr_augmented_idx(Task, t, v), t + 1]

        if (t == Task$Npsem$tau) {
            covars_Q_Mt <- Task$Npsem$history("A", t)
        } else {
            covars_Q_Mt <- c(
                glue::glue("*tmp_lcm_mediator_var_{Task$Npsem$tau:(t + 1)}*"),
                Task$Npsem$history("A", t)
            )
        }

        Q_Zt <- CrossFit(                                    # line 20 __
            Tr_a[Tr_a[[Task$Npsem$A[t]]] == 1, ],              # subset operation
            P_a, "*lcm_tmp_D_Lt_outcome*",
            c(glue::glue("*tmp_lcm_mediator_var_{Task$Npsem$tau:t}*"),
              Task$Npsem$history("A", t)),
            "continuous", lrnrs
        )

        Q_Mt <- CrossFit(                                    # line 21 __
            Tr_a[Tr_a[[Task$Npsem$A[t]]] == 0, ],              # subset operation
            P_a, "*lcm_tmp_D_Mt_outcome*",
            covars_Q_Mt,
            ifelse(t == Task$Npsem$tau, "binomial", "continuous"),
            lrnrs
        )

        D_Zt <- D_Z()
        D_Mt <- D_M()

        Rv$update("D_Zt", D_Zt, t, Folds$valid_augmented_idx(Task, t, v))
        Rv$update("D_Mt", D_Mt, t, Folds$valid_augmented_idx(Task, t, v))
    }
}
