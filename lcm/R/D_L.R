D_Lt <- function(P_a, t, tau) {
    # First summation in formula (5)
    summation_1 <- Sum(
        lapply(t:tau, function(s) {
            `K'_t+1,s` <- K_p(P_a, t+1, s)
            `H_t,s` <- H(P_a, t, s)

            `Q_Z,s+1` <- P_a[[g("lcm_Q_Z{s+1}")]]
            `Q_L,s` <- P_a[[g("lcm_Q_L{s}")]]

            w <- `K'_t+1,s` * `H_t,s`
            # w <- pmin(w, quantile(w, 0.99))
            w * (`Q_Z,s+1` - `Q_L,s`)
        })
    )

    # Second summation in formula (5)
    if ((t + 1) <= tau) {
        summation_2 <- Sum(
            lapply((t+1):tau, function(s) {
                `K'_t+1,s` <- K_p(P_a, t + 1, s)
                `H_t,s-1` <- H(P_a, t, s - 1)

                `Q_Z,s` <- P_a[[g("lcm_Q_Z{s}")]]
                `Q_L,s` <- P_a[[g("lcm_Q_L{s}")]]

                w <- `K'_t+1,s` * `H_t,s-1`
                # w <- pmin(w, quantile(w, 0.99))
                w * (`Q_L,s`- `Q_Z,s`)
            })
        )
    } else {
        summation_2 <- 0
    }

    `Q_L,t` <- P_a[[g("lcm_Q_L{t}")]]

    # Formula (5) in paper
    summation_1 + summation_2 + `Q_L,t`
}
