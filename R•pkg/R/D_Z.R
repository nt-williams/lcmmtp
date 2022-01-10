D_Z <- function(P_a, t, tau) {
    # Do first summation in equation (6)
    x_1 <- Reduce(`+`, lapply(t:tau, function(s) {
        `K'_t,s` <- K_p(P_a, t, s)
        `H_t,s` <- H(P_a, t, s)

        `Q_Z,s+1` <- P_a[[g("lcm_Q_Z{s+1}")]]
        `Q_L,s` <- P_a[[g("lcm_Q_L{s}")]]
        `K'_t,s` * `H_t,s` * (`Q_Z,s+1` - `Q_L,s`)
    }))

    # Do second summation in equation (6)
    x_2 <- Reduce(`+`, lapply(t:tau, function(s) {
        `K'_t,s` <- K_p(P_a, t, s)
        `H_t+1,s-1` <- H(P_a, t + 1, s - 1)

        `Q_Z,s` <- P_a[[g("lcm_Q_Z{s}")]]
        `Q_L,s` <- P_a[[g("lcm_Q_L{s}")]]
        `K'_t,s` * `H_t+1,s-1` * (`Q_L,s`- `Q_Z,s`)
    }))

    `Q_Z,t` <- P_a[[g("lcm_Q_Z{t}")]]
    x_1 + x_2 + `Q_Z,t`
}
