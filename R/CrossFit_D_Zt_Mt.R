CrossFit_D_Zt_Mt <- function(Task, d_prime, d_star, t, Folds, lrnrs) {
    if (t == Task$vars$tau) {
        Task$augmented[[g("lcmmtp_D_M{t+1}")]] <- 1
        Task$augmented[[g("lcmmtp_Q_M{t+1}")]] <- 1
    }

    cfd <- list()
    for (v in 1:Folds$V) {
        Tr_a <- Folds$Tr(Task$augmented, v)
        P_a  <- Folds$P(Task$augmented, v)

        Tr_a <- Tr_a[Task$at_risk(Tr_a, t) & Task$observed(Tr_a, t, T), ]

        Tr_a2 <- Folds$Tr(Task$augmented, v)
        Tr_a2 <- Tr_a2[Task$at_risk(Tr_a2, t) & Task$observed(Tr_a2, t), ]

        r <- Task$at_risk(P_a, t)
        o <- Task$observed(P_a, t, T)

        P_a[[g("lcmmtp_Q_Z{t}")]][r & o] <- CrossFit(
            Tr_a,
            Task$shift_trt(P_a[r & o, ], Task$vars$A[t], Task$vars$cens[t], d_prime),
            g("lcmmtp_D_L{t}"),
            c(g("lcmmtp_med_{t:Task$vars$tau}"),
              Task$vars$history("A", t),
              Task$vars$A[t]),
            "continuous", lrnrs
        )
        P_a[[g("lcmmtp_Q_Z{t}")]][!r] <- 0

        Tr_a2[[g("lcmmtp_D_M{t+1}")]] <-
            (Tr_a2[[g("lcmmtp_med_{t}")]] == Tr_a2[[Task$vars$M[t]]]) *
            Tr_a2[[g("lcmmtp_D_M{t+1}")]]

        P_a[[g("lcmmtp_Q_M{t}")]][r & o] <- CrossFit(
            Tr_a2,
            Task$shift_trt(P_a[r & o, ], Task$vars$A[t], Task$vars$cens[t], d_star),
            g("lcmmtp_D_M{t+1}"),
            c(g("lcmmtp_med_{t:Task$vars$tau}"),
              Task$vars$history("A", t),
              Task$vars$A[t]),
            ifelse(t == Task$vars$tau, "binomial", "continuous"),
            lrnrs
        )
        P_a[[g("lcmmtp_Q_M{t}")]][!r] <- 0

        P_a[[g("lcmmtp_D_Z{t}")]] <- D_Zt(P_a, t, Task$vars$tau)
        P_a[[g("lcmmtp_D_M{t}")]] <- D_Mt(P_a, t, Task$vars$tau, Task$vars$M)

        cfd[[v]] <- P_a
    }

    cfd <- Reduce(rbind, cfd)
    data.table::setorder(cfd, "lcmmtp_ID")

    Task$augmented <- cfd
}
