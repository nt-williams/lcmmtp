CrossFit_D_Lt <- function(Task, t, Folds, lrnrs) {
    if (t == Task$Npsem$tau) {
        Task$data[[g("lcm_D_Z{t+1}")]] <- Task$data[[Task$Npsem$Y]]
        Task$data[[g("lcm_Q_Z{t+1}")]] <- Task$data[[Task$Npsem$Y]]
    }

    cfd <- list()
    for (v in 1:Folds$V) {
        Tr   <- Folds$Tr(Task$data, v)
        Tr_a <- Task$augment(Tr, t)                                           # line 7 in algorithm
        P_a  <- Task$augment(Folds$P(Task$data, v), t)                        # line 8 __

        if (t == Task$Npsem$tau) {
            covars <- Task$Npsem$history("M", t)
        } else {
            covars <- c(
                g("*lcm_med_{Task$Npsem$tau:(t+1)}*"),
                Task$Npsem$history("M", t)
            )
        }

        Q_Lt <- CrossFit(                                                                        # line 9 __
            Tr_a[
                Tr_a[[g("*lcm_med_{t}*")]] == Tr_a[[Task$Npsem$M[t]]],
            ],                                                                                   # subset operation
            P_a, g("lcm_D_Z{t+1}"),
            covars,
            Task$type, lrnrs
        )

        P_a[[g("lcm_Q_L{t}")]] <- Q_Lt

        g_t  <- CrossFit(Tr, P_a, Task$Npsem$A[t], Task$Npsem$history("A", t), "binomial", lrnrs) # line 10 __
        g_Mt <- CrossFit(Tr, P_a, Task$Npsem$M[t], Task$Npsem$history("M", t), "binomial", lrnrs) # line 11 __

        P_a[[g("lcm_Gp_A{t}")]] <- G(P_a[[Task$Npsem$A[t]]], g_t, 1)     # line 12 __
        P_a[[g("lcm_Gs_A{t}")]] <- G(P_a[[Task$Npsem$A[t]]], 1 - g_t, 0) # line 13 __
        P_a[[g("lcm_G_M{t}")]] <- G(P_a[[Task$Npsem$M[t]]], g_Mt, t)     # line 14 __

        P_a[[g("lcm_D_L{t}")]] <- D_L(P_a, t, Task$Npsem$tau)
        cfd[[v]] <- P_a
    }

    cfd <- Reduce(rbind, cfd)
    data.table::setorder(cfd, "*lcm_ID*")

    Task$data <- cfd
}
