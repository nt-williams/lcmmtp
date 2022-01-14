# -------------------------------------------------------------------------
# sim.R
# Nick Williams
#
# Runs a single simulation.
# Intended to be run with Rscript
# -------------------------------------------------------------------------

source("simulation/R/dgm.R")

id <- Sys.getenv("SGE_TASK_ID")

if (id == "undefined" || id == "") id <- 1

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
    res <- lcm::lcm(d, 1, 1, Np, sl3::Lrnr_xgboost$new(), V)

    readr::write_csv(
        data.frame(seed = seed, n = n, theta = res$theta, var = res$var),
        "simulation/data/sims/results•13JAN22.csv",
        append = TRUE
    )
}

args <- commandArgs(trailingOnly = TRUE)

simulate(args$n, round(runif(1, 1, 1e5)))

quit("no")
