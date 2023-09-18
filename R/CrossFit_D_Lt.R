CrossFit_D_Lt <- function(x, t, Folds, control) {
    if (t == x$vars$tau) {
        x$augmented[[g("lcmmtp_D_Z{t+1}")]] <- x$augmented[[x$vars$Y]]
        x$augmented[[g("lcmmtp_Q_Z{t+1}")]] <- x$augmented[[x$vars$Y]]
    }

    cfd <- list()
    for (v in 1:Folds$V) {
        Tr   <- Folds$Tr(x$augmented, v)
        Tr_a <- x$augment(Tr, t)
        P_a  <- x$augment(Folds$P(x$augmented, v), t)

        Tr <- Tr[x$at_risk(Tr, t), ]
        Tr_a <- Tr_a[x$at_risk(Tr_a, t) & x$observed(Tr_a, t), ]

        r <- x$at_risk(P_a, t)
        o <- x$observed(P_a, t, T)

        g_Mt <- g_Ast <- g_Apt <- g_Ct <- vector("numeric", nrow(P_a))

        P_a[[g("lcmmtp_Q_L{t}")]][r & o] <- CrossFit(
            Tr_a[Tr_a[[g("lcmmtp_med_{t}")]] == Tr_a[[x$vars$M[t]]], ],
            P_a[r & o, ],
            g("lcmmtp_D_Z{t+1}"),
            c(g("lcmmtp_med_{t:x$vars$tau}"), x$vars$history("M", t)),
            ifelse(t == x$vars$tau, x$type, "continuous"),
            control$learners_QZ,
            control$folds_QZ
        )
        P_a[[g("lcmmtp_Q_L{t}")]][!r] <- 0

        Tr_Ap <- x$stack_data(Folds$Tr(x$data, v), Folds$Tr(x$shifted_aprime, v), t)

        g_Apt[r & o] <- CrossFit(Tr_Ap[x$at_risk(Tr_Ap, t) & x$observed(Tr_Ap, t, T), ],
                                 P_a[r & o, ],
                                 "tmp_lcmmtp_stack_indicator",
                                 c(x$vars$history("A", t), x$vars$A[t]),
                                 "binomial",
                                 control$learners_trt,
                                 control$folds_trt)

        Tr_As <- x$stack_data(Folds$Tr(x$data, v), Folds$Tr(x$shifted_astar, v), t)

        g_Ast[r & o] <- CrossFit(Tr_As[x$at_risk(Tr_As, t) & x$observed(Tr_As, t, T), ],
                                 P_a[r & o, ],
                                 "tmp_lcmmtp_stack_indicator",
                                 c(x$vars$history("A", t), x$vars$A[t]),
                                 "binomial",
                                 control$learners_trt,
                                 control$folds_trt)

        # Create pooled data for g_Mt fit
        m_fit_data <- x$augment(Folds$Tr(x$data, v), t)
        m_fit_data <- m_fit_data[x$at_risk(m_fit_data, t) & x$observed(m_fit_data, t, T), ]
        m_fit_data[["lcmmtp_pseudo_m_fit"]] <-
            as.numeric(m_fit_data[[g("lcmmtp_med_{t}")]] == m_fit_data[[x$vars$M[t]]])

        g_Mt[r & o] <- CrossFit(m_fit_data,
                                P_a[r & o, ],
                                "lcmmtp_pseudo_m_fit",
                                c(x$vars$history("M", t), g("lcmmtp_med_{t}")),
                                "binomial",
                                control$learners_mediator,
                                control$folds_mediator)

        P_a[[g("lcmmtp_Gp_A{t}")]] <- density_ratios(g_Apt, r, x$observed(P_a, t))
        P_a[[g("lcmmtp_Gs_A{t}")]] <- density_ratios(g_Ast, r, x$observed(P_a, t))
        P_a[[g("lcmmtp_G_M{t}")]] <- G(P_a[[x$vars$M[t]]],
                                       g_Mt,
                                       P_a[[g("lcmmtp_med_{t}")]],
                                       r,
                                       x$observed(P_a, t))

        P_a[[g("lcmmtp_D_L{t}")]] <- D_Lt(P_a, t, x$vars$tau)
        cfd[[v]] <- P_a
    }

    cfd <- Reduce(rbind, cfd)
    data.table::setorder(cfd, "lcmmtp_ID")

    x$augmented <- cfd
}
