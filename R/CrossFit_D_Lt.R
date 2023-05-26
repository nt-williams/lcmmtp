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

        Tr <- Tr[Task$at_risk(Tr, t), ]
        Tr_a <- Tr_a[Task$at_risk(Tr_a, t), ]

        r <- Task$at_risk(P_a, t)

        g_Mt <- g_Ast <- g_Apt <- vector("numeric", nrow(P_a))

        P_a[[g("lcmmtp_Q_L{t}")]][r] <- CrossFit(
            Tr_a[Tr_a[[g("lcmmtp_med_{t}")]] == Tr_a[[Task$vars$M[t]]], ],
            P_a[r, ],
            g("lcmmtp_D_Z{t+1}"),
            c(g("lcmmtp_med_{t:Task$vars$tau}"), Task$vars$history("M", t)),
            ifelse(t == Task$vars$tau, Task$type, "continuous"),
            lrnrs
        )
        P_a[[g("lcmmtp_Q_L{t}")]][!r] <- 0

        g_Apt[r] <- CrossFit(Task$stack_data(Folds$Tr(Task$data, v),
                                             Folds$Tr(Task$shifted_aprime, v), t),
                             P_a[r, ],
                             "tmp_lcmmtp_stack_indicator",
                             c(Task$vars$history("A", t), Task$vars$A[t]),
                             "binomial",
                             lrnrs)

        g_Ast[r] <- CrossFit(Task$stack_data(Folds$Tr(Task$data, v),
                                             Folds$Tr(Task$shifted_astar, v), t),
                             P_a[r, ],
                             "tmp_lcmmtp_stack_indicator",
                             c(Task$vars$history("A", t), Task$vars$A[t]),
                             "binomial",
                             lrnrs)

        # Create pooled data for g_Mt fit
        m_fit_data <- Task$augment(Folds$Tr(Task$data, v), t)
        m_fit_data <- m_fit_data[Task$at_risk(m_fit_data, t), ]
        m_fit_data[["lcmmtp_pseudo_m_fit"]] <-
            as.numeric(m_fit_data[[g("lcmmtp_med_{t}")]] == m_fit_data[[Task$vars$M[t]]])

        g_Mt[r] <- CrossFit(m_fit_data,
                            P_a[r, ],
                            "lcmmtp_pseudo_m_fit",
                            c(Task$vars$history("M", t), g("lcmmtp_med_{t}")),
                            "binomial",
                            lrnrs)

        P_a[[g("lcmmtp_Gp_A{t}")]] <- density_ratios(g_Apt, r)
        P_a[[g("lcmmtp_Gs_A{t}")]] <- density_ratios(g_Ast, r)
        P_a[[g("lcmmtp_G_M{t}")]] <- G(P_a[[Task$vars$M[t]]],
                                       g_Mt,
                                       P_a[[g("lcmmtp_med_{t}")]],
                                       r)

        P_a[[g("lcmmtp_D_L{t}")]] <- D_Lt(P_a, t, Task$vars$tau)
        cfd[[v]] <- P_a
    }

    cfd <- Reduce(rbind, cfd)
    data.table::setorder(cfd, "lcmmtp_ID")

    Task$augmented <- cfd
}
