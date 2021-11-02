inner_loop_1 <- function(Task, t, Rv, Folds, lrnrs) {
    for (v in seq_along(Folds$V)) {
        Tr   <- Folds$Tr(Task$data, v)
        P    <- Folds$P(Task$data, v)
        Tr_a <- Task$augment(Tr, t) # line 7 in algorithm
        P_a  <- Task$augment(P, t)  # line 8

        Tr_a[["*lcm_tmp_D_Lt_outcome*"]] <- Rv$D_Zt[[t + 1]]

        Q_Lt <- CrossFit(                                          # line 9
            Tr_a["*tmp_lcm_mediator_var*" == Npsem$M[t], ],        # subset operation
            P_a, "*lcm_tmp_D_Lt_outcome*",
            c(Npsem$M[t], Npsem$history("M", t))
        )

        g_t  <- CrossFit(Tr, P, Npsem$A[t], Npsem$history("A", t)) # line 10
        g_Mt <- CrossFit(Tr, P, Npsem$M[t], Npsem$history("M", t)) # line 11

        Gp_At <- G(P[[Task$Npsem$A[t]]], g_t, 1)  # line 12
        Gs_At <- G(P[[Task$Npsem$A[t]]], g_t, 0)  # line 13
        G_Mt  <- G(P[[Task$Npsem$M[t]]], g_Mt, 1) # line 14

        D_Lt <- D_L()

        # mapping the valid indices from non-augmented data to the equivalent
        #  indices in the augmented data set
        idx <- do.call(c, lapply(
            Folds$valid_idx(v),
            function(i) which(rep(1:Task$n, rep(Task$seq[[t]], Task$n)) == i)
        ))

        Rv$update("D_Lt", D_Lt, t, idx)
    }
}
