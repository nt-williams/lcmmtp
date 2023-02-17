library(glue)
source("_research/dgm.r")

id <- Sys.getenv("SGE_TASK_ID")

if (id == "undefined" || id == "") id <- 1

framework <- "sl"

simulate <- function(n, seed, V) {
    d <- datagen(n, seed)

    Np <- lcm::lcm_Npsem$new(
        L = list(c("L_1"), c("L_2")),
        A = c("A_1", "A_2"),
        Z = list(c("Z_1"), c("Z_2")),
        M = c("M_1", "M_2"),
        Y = "Y"
    )

    if (framework == "sl") {
        sl <- sl3::Lrnr_sl$new(
            learners = sl3::make_learner_stack(
                sl3::Lrnr_glm,
                sl3::Lrnr_lightgbm
            ),
            metalearners = sl3::Lrnr_nnls$new()
        )

        res_00 <- lcm::lcm(d, c(0, 0), c(0, 0), Np, sl, V)
        res_11 <- lcm::lcm(d, c(1, 1), c(1, 1), Np, sl, V)
        res_10 <- lcm::lcm(d, c(1, 1), c(0, 0), Np, sl, V)
    } else {
        res_00 <- lcm::lcm(d, c(0, 0), c(0, 0), Np, V)
        res_11 <- lcm::lcm(d, c(1, 1), c(1, 1), Np, V)
        res_10 <- lcm::lcm(d, c(1, 1), c(0, 0), Np, V)
    }

    write.csv(
        data.frame(
            seed = seed,
            n = n,
            theta_00 = res_00$theta,
            var_00 = var(res_00$S) / n,
            theta_10 = res_10$theta,
            var_10 = var(res_10$S) / n,
            theta_11 = res_11$theta,
            var_11 = var(res_11$S) / n,
            direct = res_10$theta - res_00$theta,
            var_direct = var(res_10$S - res_00$S) / n,
            indirect = res_11$theta - res_10$theta,
            var_indirect = var(res_11$S - res_10$S) / n,
            total = res_11$theta - res_00$theta,
            var_total = var(res_11$S - res_00$S) / n
        ),
        glue("_research/data/{id}-{n}-dgp2.csv"),
        row.names = FALSE
    )
}

args <- commandArgs(trailingOnly = TRUE)

n <- as.numeric(args[[1]])

simulate(as.numeric(args[[1]]), round(runif(1, 1, 1e5)), as.numeric(args[[2]]))

quit("no")
