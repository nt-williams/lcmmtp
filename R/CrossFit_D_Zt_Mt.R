CrossFit_D_Zt_Mt <- function(x, d_prime, d_star, t, Folds, control) {
    if (t == x$vars$tau) {
        x$augmented[[g("lcmmtp_D_M{t+1}")]] <- 1
        x$augmented[[g("lcmmtp_Q_M{t+1}")]] <- 1
    }

    cfd <- list()
    for (v in 1:Folds$V) {
        Tr_a <- Folds$Tr(x$augmented, v)
        P_a  <- Folds$P(x$augmented, v)

        Tr_a <- Tr_a[x$at_risk(Tr_a, t) & x$observed(Tr_a, t, T), ]

        Tr_a2 <- Folds$Tr(x$augmented, v)
        Tr_a2 <- Tr_a2[x$at_risk(Tr_a2, t) & x$observed(Tr_a2, t), ]

        r <- x$at_risk(P_a, t)
        o <- x$observed(P_a, t, T)

        P_a[[g("lcmmtp_Q_Z{t}")]][r & o] <- CrossFit(
            Tr_a,
            x$shift_trt(P_a[r & o, ], x$vars$A[t], x$vars$cens[t], d_prime),
            g("lcmmtp_D_L{t}"),
            c(g("lcmmtp_med_{t:x$vars$tau}"),
              x$vars$history("A", t),
              x$vars$A[t]),
            "continuous",
            control$learners_QL,
            control$folds_QL
        )
        P_a[[g("lcmmtp_Q_Z{t}")]][!r] <- 0

        Tr_a2[[g("lcmmtp_D_M{t+1}")]] <-
            (Tr_a2[[g("lcmmtp_med_{t}")]] == Tr_a2[[x$vars$M[t]]]) *
            Tr_a2[[g("lcmmtp_D_M{t+1}")]]

        P_a[[g("lcmmtp_Q_M{t}")]][r & o] <- CrossFit(
            Tr_a2,
            x$shift_trt(P_a[r & o, ], x$vars$A[t], x$vars$cens[t], d_star),
            g("lcmmtp_D_M{t+1}"),
            c(g("lcmmtp_med_{t:x$vars$tau}"),
              x$vars$history("A", t),
              x$vars$A[t]),
            ifelse(t == x$vars$tau, "binomial", "continuous"),
            control$learners_QM,
            control$folds_QM
        )
        P_a[[g("lcmmtp_Q_M{t}")]][!r] <- 0

        P_a[[g("lcmmtp_D_Z{t}")]] <- D_Zt(P_a, t, x$vars$tau)
        P_a[[g("lcmmtp_D_M{t}")]] <- D_Mt(P_a, t, x$vars$tau, x$vars$M)

        cfd[[v]] <- P_a
    }

    cfd <- Reduce(rbind, cfd)
    data.table::setorder(cfd, "lcmmtp_ID")

    x$augmented <- cfd
}
