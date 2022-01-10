CrossFit_D_Zt_Mt <- function(Task, t, Folds, lrnrs) {
    if (t == Task$Npsem$tau) {
        Task$data[[g("lcm_D_M{t+1}")]] <- 1
        Task$data[[g("lcm_Q_M{t+1}")]] <- 1
    }

    cfd <- list()
    for (v in 1:Folds$V) {
        Tr_a <- Folds$Tr(Task$data, v)
        P_a  <- Folds$P(Task$data, v)

        Tr_a[[g("lcm_D_M{t+1}")]] <-
            as.numeric(Tr_a[[g("*lcm_med_{t}*")]] == Tr_a[[Task$Npsem$M[t]]]) *        # line 21 __
            Tr_a[[g("lcm_D_M{t+1}")]]

        if (t == Task$Npsem$tau) {
            covars_Q_Mt <- Task$Npsem$history("A", t)
        } else {
            covars_Q_Mt <- c(
                g("*lcm_med_{Task$Npsem$tau:(t + 1)}*"),
                Task$Npsem$history("A", t)
            )
        }

        Q_Zt <- CrossFit(                                    # line 20 __
            Tr_a[Tr_a[[Task$Npsem$A[t]]] == 1, ],            # subset operation
            P_a, g("lcm_D_L{t}"),
            c(g("*lcm_med_{Task$Npsem$tau:t}*"), Task$Npsem$history("A", t)),
            "continuous", lrnrs
        )

        P_a[[g("lcm_Q_Z{t}")]] <- Q_Zt

        Q_Mt <- CrossFit(                                    # line 21 __
            Tr_a[Tr_a[[Task$Npsem$A[t]]] == 0, ],            # subset operation
            P_a, g("lcm_D_M{t+1}"),
            covars_Q_Mt,
            ifelse(t == Task$Npsem$tau, "binomial", "continuous"),
            lrnrs
        )

        P_a[[g("lcm_Q_M{t}")]] <- Q_Mt

        P_a[[g("lcm_D_Z{t}")]] <- D_Z(P_a, t, Task$Npsem$tau)
        P_a[[g("lcm_D_M{t}")]] <- D_M(P_a, t, Task$Npsem$tau, Task$Npsem$M)

        cfd[[v]] <- P_a
    }

    cfd <- Reduce(rbind, cfd)
    data.table::setorder(cfd, "*lcm_ID*")

    Task$data <- cfd
}
