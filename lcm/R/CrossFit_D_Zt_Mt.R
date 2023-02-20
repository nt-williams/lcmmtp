CrossFit_D_Zt_Mt <- function(Task, t, a_prime, a_star, Folds, lrnrs) {
    if (t == Task$Npsem$tau) {
        Task$augmented[[g("lcm_D_M{t+1}")]] <- 1
        Task$augmented[[g("lcm_Q_M{t+1}")]] <- 1
    }

    cfd <- list()
    for (v in 1:Folds$V) {
        Tr_a <- Folds$Tr(Task$augmented, v)
        P_a  <- Folds$P(Task$augmented, v)

        P_a[[g("lcm_Q_Z{t}")]] <- CrossFit(                                    # line 20 __
            Tr_a[Tr_a[[Task$Npsem$A[t]]] == a_prime, ],                              # subset operation
            P_a, g("lcm_D_L{t}"),
            c(g("lcm_med_{t:Task$Npsem$tau}"), Task$Npsem$history("A", t)),
            "gaussian", lrnrs
        )

        Tr_a[[g("lcm_D_M{t+1}")]] <-
            (Tr_a[[g("lcm_med_{t}")]] == Tr_a[[Task$Npsem$M[t]]]) *        # line 21 __
            Tr_a[[g("lcm_D_M{t+1}")]]

        P_a[[g("lcm_Q_M{t}")]] <- CrossFit(                        # line 21 __
            Tr_a[Tr_a[[Task$Npsem$A[t]]] == a_star, ],                  # subset operation
            P_a, g("lcm_D_M{t+1}"),
            c(g("lcm_med_{t:Task$Npsem$tau}"), Task$Npsem$history("A", t)),
            ifelse(t == Task$Npsem$tau, "binomial", "gaussian"), lrnrs
        )

        P_a[[g("lcm_D_Z{t}")]] <- D_Zt(P_a, t, Task$Npsem$tau)
        P_a[[g("lcm_D_M{t}")]] <- D_Mt(P_a, t, Task$Npsem$tau, Task$Npsem$M)

        cfd[[v]] <- P_a
    }

    cfd <- Reduce(rbind, cfd)
    data.table::setorder(cfd, "lcm_ID")

    Task$augmented <- cfd
}
