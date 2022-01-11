data("sim")

Np <- lcm_Npsem$new(
    L = list(c("L_1"), c("L_2")),
    A = c("A_1", "A_2"),
    Z = list(c("Z_1"), c("Z_2")),
    M = c("M_1", "M_2"),
    Y = "Y"
)

lrnrs <- sl3::make_learner(sl3::Lrnr_glm_fast)

lcm(sim, Np, lrnrs, 2)
