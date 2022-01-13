D_Zt <- function(P_a, t, tau) {
    # First summation in formula (6)
    summation_1 <- Sum(
        lapply(t:tau, function(s) {
            `K'_t,s` <- K_p(P_a, t, s)
            `H_t,s` <- H(P_a, t, s)
            `Q_Z,s+1` <- P_a[[g("lcm_Q_Z{s+1}")]]
            `Q_L,s` <- P_a[[g("lcm_Q_L{s}")]]
            `K'_t,s` * `H_t,s` * (`Q_Z,s+1` - `Q_L,s`)
        })
    )

    # Second summation in formula (6)
    summation_2 <- Sum(
        lapply(t:tau, function(s) {
            `K'_t,s` <- K_p(P_a, t, s)
            `H_t+1,s-1` <- H(P_a, t + 1, s - 1)
            `Q_Z,s` <- P_a[[g("lcm_Q_Z{s}")]]
            `Q_L,s` <- P_a[[g("lcm_Q_L{s}")]]
            `K'_t,s` * `H_t+1,s-1` * (`Q_L,s`- `Q_Z,s`)
        })
    )

    `Q_Z,t` <- P_a[[g("lcm_Q_Z{t}")]]

    # Formula (6)
    summation_1 + summation_2 + `Q_Z,t`
}
