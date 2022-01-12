D_Lt <- function(P_a, t, tau) {
    # Do first summation in equation (5)
    summation_1 <- Sum(
        lapply(t:tau, function(s) {
            `K'_t,s` <- K_p(P_a, t, s)
            `H_t,s` <- H(P_a, t, s)

            `Q_Z,s+1` <- P_a[[g("lcm_Q_Z{s+1}")]]
            `Q_L,s` <- P_a[[g("lcm_Q_L{s}")]]
            `K'_t,s` * `H_t,s` * (`Q_Z,s+1` - `Q_L,s`)
        })
    )

    # Do second summation in equation (5)
    if ((t + 1) <= tau) {
        summation_2 <- Sum(
            lapply((t+1):tau, function(s) {
                `K'_t+1,s` <- K_p(P_a, t + 1, s)
                `H_t+1,s-1` <- H(P_a, t + 1, s - 1)

                `Q_Z,s` <- P_a[[g("lcm_Q_Z{s}")]]
                `Q_L,s` <- P_a[[g("lcm_Q_L{s}")]]
                `K'_t+1,s` * `H_t+1,s-1` * (`Q_L,s`- `Q_Z,s`)
            })
        )
    } else {
        summation_2 <- 0
    }

    `Q_L,t` <- P_a[[g("lcm_Q_L{t}")]]

    summation_1 + summation_2 + `Q_L,t` # Formula (5) in paper
}
