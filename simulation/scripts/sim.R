# -------------------------------------------------------------------------
# sim.R
# Nick Williams
#
# Runs a single simulation.
# Intended to be run with Rscript
# -------------------------------------------------------------------------

library(glue)

source("simulation/R/dgm.r")

id <- Sys.getenv("SGE_TASK_ID")

if (id == "undefined" || id == "") id <- 1

sl <- sl3::Lrnr_sl$new(
    learners = sl3::make_learner_stack(
        sl3::Lrnr_glm,
        sl3::Lrnr_earth,
        sl3::Lrnr_lightgbm
    ),
    metalearners = sl3::Lrnr_nnls$new()
)

sl <- sl3::Lrnr_xgboost$new()

simulate <- function(n, seed) {
    d <- datagen(n, seed)

    Np <- lcm::lcm_Npsem$new(
        L = list(c("L_1"), c("L_2")),
        A = c("A_1", "A_2"),
        Z = list(c("Z_1"), c("Z_2")),
        M = c("M_1", "M_2"),
        Y = "Y"
    )

    V <- ifelse(n >= 1e4, 2, 10)

    res_00 <- lcm::lcm(d, 0, 0, Np, sl, V)
    res_11 <- lcm::lcm(d, 1, 1, Np, sl, V)
    res_10 <- lcm::lcm(d, 1, 0, Np, sl, V)

    write.csv(
        data.frame(
            seed = seed,
            n = n,
            direct = res_10$theta - res_00$theta,
            var_direct = res_10$var + res_00$var,
            indirect = res_11$theta - res_10$theta,
            var_indirect = res_11$var + res_10$var,
            total = res_11$theta - res_00$theta,
            var_total = res_11$var + res_00$var
        ),
        glue("simulation/data/sims/{id}-{n}.csv"),
        row.names = FALSE
    )
}

args <- commandArgs(trailingOnly = TRUE)

simulate(as.numeric(args[[1]]), round(runif(1, 1, 1e5)))

quit("no")
