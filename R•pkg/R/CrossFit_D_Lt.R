CrossFit_D_Lt <- function(Task, t, a_prime, a_star, Folds, lrnrs) {
    if (t == Task$Npsem$tau) {
        Task$augmented[[g("lcm_D_Z{t+1}")]] <- Task$augmented[[Task$Npsem$Y]]
        Task$augmented[[g("lcm_Q_Z{t+1}")]] <- Task$augmented[[Task$Npsem$Y]]
    }

    cfd <- list()
    for (v in 1:Folds$V) {
        Tr   <- Folds$Tr(Task$augmented, v)
        Tr_a <- Task$augment(Tr, t)                                                # line 7 in algorithm
        P_a  <- Task$augment(Folds$P(Task$augmented, v), t)                        # line 8 __

        if (t == Task$Npsem$tau) {
            covars <- Task$Npsem$history("M", t)
        } else {
            covars <- c(
                g("*lcm_med_{(t+1):Task$Npsem$tau}*"),
                Task$Npsem$history("M", t)
            )
        }

        P_a[[g("lcm_Q_L{t}")]] <- CrossFit(                                                      # line 9 __
            Tr_a[
                Tr_a[[g("*lcm_med_{t}*")]] == Tr_a[[Task$Npsem$M[t]]],
            ],                                                                                   # subset operation
            P_a, g("lcm_D_Z{t+1}"),
            covars,
            Task$type, lrnrs
        )

        g_t  <- CrossFit(                                                                        # line 10 __
            Folds$Tr(Task$data, v), P_a,
            Task$Npsem$A[t], Task$Npsem$history("A", t), "binomial", lrnrs
        )

        # Created pooled data for g_Mt fit
        m_fit_data <- Task$augment(Folds$Tr(Task$data, v), t)
        m_fit_data[["*lcm_pseudo_m_fit*"]] <-
            as.numeric(m_fit_data[[g("*lcm_med_{t}*")]] == m_fit_data[[Task$Npsem$M[t]]])

        g_Mt <- CrossFit(                                                                        # line 11 __ # This needs to be a classification
            m_fit_data, P_a, "*lcm_pseudo_m_fit*",
            c(Task$Npsem$history("M", t), g("*lcm_med_{t}*")), "binomial", lrnrs
        )

        P_a[[g("lcm_Gp_A{t}")]] <- G(P_a[[Task$Npsem$A[t]]], ifelse(a_prime == 1, g_t, 1 - g_t), a_prime)                             # line 12 __
        P_a[[g("lcm_Gs_A{t}")]] <- G(P_a[[Task$Npsem$A[t]]], ifelse(a_star == 1, g_t, 1 - g_t), a_star)                         # line 13 __
        P_a[[g("lcm_G_M{t}")]] <- G(P_a[[Task$Npsem$M[t]]], g_Mt, P_a[[g("*lcm_med_{t}*")]])     # line 14 __

        P_a[[g("lcm_D_L{t}")]] <- D_L(P_a, t, Task$Npsem$tau)
        cfd[[v]] <- P_a
    }

    cfd <- Reduce(rbind, cfd)
    data.table::setorder(cfd, "*lcm_ID*")

    Task$augmented <- cfd
}
