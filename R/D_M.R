D_Mt <- function(P_a, t, tau, M) {
    summation_1 <- Sum(
        lapply(t:tau, function(s) {
            `K*_t,s` <- K_s(P_a, t, s)

            if ((s - 1) < t) {
                y_1 <- rep(1, nrow(P_a))
            } else {
                y_1 <- apply(
                    P_a[, g("lcmmtp_med_{t:(s-1)}"), drop = FALSE] == P_a[, M[t:(s - 1)], drop = FALSE]
                    , 1, prod
                )
            }

            y_2 <- (as.numeric(P_a[[g("lcmmtp_med_{s}")]] == P_a[[M[s]]]) *
                        P_a[[g("lcmmtp_Q_M{s+1}")]]) -
                P_a[[g("lcmmtp_Q_M{s}")]]

            y_1[is.na(y_1)] <- -999
            y_2[is.na(y_2)] <- -999

            w <- `K*_t,s`
            # w <- pmin(w, quantile(w, 0.99))
            w * y_1 * y_2
        })
    )

    `Q_M,t` <- P_a[[g("lcmmtp_Q_M{t}")]]

    # Formula (7)
    summation_1 + `Q_M,t`
}
