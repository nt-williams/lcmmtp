CrossFit_D_Lt <- function(Task, t, Folds, lrnrs) {
    if (t == Task$vars$tau) {
        Task$augmented[[g("lcmmtp_D_Z{t+1}")]] <- Task$augmented[[Task$vars$Y]]
        Task$augmented[[g("lcmmtp_Q_Z{t+1}")]] <- Task$augmented[[Task$vars$Y]]
    }

    cfd <- list()
    for (v in 1:Folds$V) {
        Tr   <- Folds$Tr(Task$augmented, v)
        Tr_a <- Task$augment(Tr, t)
        P_a  <- Task$augment(Folds$P(Task$augmented, v), t)

        # mU indicates m with an underbar
        `(mU_t+1,H_M,t)` <- c(g("lcmmtp_med_{t:Task$vars$tau}"), Task$vars$history("M", t))

        P_a[[g("lcmmtp_Q_L{t}")]] <- CrossFit(
            Tr_a[Tr_a[[g("lcmmtp_med_{t}")]] == Tr_a[[Task$vars$M[t]]], ],
            P_a, g("lcmmtp_D_Z{t+1}"),
            `(mU_t+1,H_M,t)`,
            ifelse(t == Task$vars$tau, Task$type, "continuous"),
            lrnrs
        )

        g_Apt <- CrossFit(Task$stack_data(Folds$Tr(Task$data, v),
                                          Folds$Tr(Task$shifted_aprime, v), t),
                          P_a,
                          "tmp_lcmmtp_stack_indicator",
                          c(Task$vars$history("A", t), Task$vars$A[t]),
                          "binomial",
                          lrnrs)

        g_Ast <- CrossFit(Task$stack_data(Folds$Tr(Task$data, v),
                                          Folds$Tr(Task$shifted_astar, v), t),
                          P_a,
                          "tmp_lcmmtp_stack_indicator",
                          c(Task$vars$history("A", t), Task$vars$A[t]),
                          "binomial",
                          lrnrs)

        # Create pooled data for g_Mt fit
        m_fit_data <- Task$augment(Folds$Tr(Task$data, v), t)
        m_fit_data[["lcmmtp_pseudo_m_fit"]] <-
            as.numeric(m_fit_data[[g("lcmmtp_med_{t}")]] == m_fit_data[[Task$vars$M[t]]])

        g_Mt <- CrossFit(
            m_fit_data, P_a, "lcmmtp_pseudo_m_fit",
            c(Task$vars$history("M", t), g("lcmmtp_med_{t}")),
            "binomial", lrnrs
        )


        P_a[[g("lcmmtp_Gp_A{t}")]] <- density_ratios(g_Apt)
        P_a[[g("lcmmtp_Gs_A{t}")]] <- density_ratios(g_Ast)
        P_a[[g("lcmmtp_G_M{t}")]] <- G(P_a[[Task$vars$M[t]]], g_Mt, P_a[[g("lcmmtp_med_{t}")]])

        P_a[[g("lcmmtp_D_L{t}")]] <- D_Lt(P_a, t, Task$vars$tau)
        cfd[[v]] <- P_a
    }

    cfd <- Reduce(rbind, cfd)
    data.table::setorder(cfd, "lcmmtp_ID")

    Task$augmented <- cfd
}
