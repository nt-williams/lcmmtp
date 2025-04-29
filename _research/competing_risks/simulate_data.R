library(simcausal)
library(dplyr)
library(purrr)

simulate_data <- function(n, prob_A = NULL) {
    dag <-
        DAG.empty() +
        # time 1
        node("L1", distr = "rbern", prob = 0.5) + {
            if (!is.null(prob_A)) {
                node("A1", distr = "rbern", prob = prob_A)
            } else {
                node("A1", distr = "rbern", prob = 0.4 + L1*0.1)
            }
        } +
        node("M1", distr = "rbern", prob = 0.6 - A1*0.1 + L1*0.1) +
        node("D2", distr = "rbern", prob = 0.01 + M1*0.05 + A1*0.02 - L1*0.001) +
        node("Y2", distr = "rbern", prob = D2*0 + (1-D2)*(0.3 + M1*0.05 + A1*0.2 - L1*0.001), EFU = TRUE) +

        # time 2
        node("L2", distr = "rbern", prob = 0.5) + {
            if (!is.null(prob_A)) {
                node("A2", distr = "rbern", prob = prob_A)
            } else {
                node("A2", distr = "rbern", prob = 0.4 + L2*0.1)
            }
        } +
        node("M2", distr = "rbern", prob = D2*Y2*0 + (1-D2)*(1-Y2)*(0.6 - A2*0.1 + L2*0.1)) +
        node("D3", distr = "rbern", prob = D2*1 + (1-D2)*(0.01 + M2*0.05 + A2*0.02 - L2*0.001)) +
        node("Y3", distr = "rbern", prob = D3*0 + (1-D3)*(0.3 + M2*0.05 + A2*0.2 - L2*0.001), EFU = TRUE) +

        # time 3
        node("L3", distr = "rbern", prob = 0.5) + {
            if (!is.null(prob_A)) {
                node("A3", distr = "rbern", prob = prob_A)
            } else {
                node("A3", distr = "rbern", prob = 0.4 + L3*0.1)
            }
        } +
        node("M3", distr = "rbern", prob = D3*Y3*0 + (1-D3)*(1-Y3)*(0.6 - A3*0.1 + L3*0.1)) +
        node("D4", distr = "rbern", prob = D3*1 + (1-D3)*(0.01 + M3*0.05 + A3*0.03 - L3*0.001)) +
        node("Y4", distr = "rbern", prob = D3*0 + (1-D3)*(0.3 + M3*0.05 + A3*0.3 - L3*0.001), EFU = TRUE)

    dag <- set.DAG(dag)
    simulated_data <- sim(dag, n = n, LTCF = "Y")

    mutate(simulated_data,
           D3 = ifelse(D2 == 1, D2, D3),
           D4 = ifelse(D3 == 1, D3, D4),
           Y2 = ifelse(D2 == 1, 0, Y2),
           Y3 = ifelse(D3 == 1, 0, Y3),
           Y4 = ifelse(D4 == 1, 0, Y4),
           Y3 = ifelse(Y2 == 1, Y2, Y3),
           Y4 = ifelse(Y3 == 1, Y3, Y4),
           M2 = ifelse(Y2 == 1, 0, M2),
           M3 = ifelse(Y3 == 1, 0, M3))
}

J <- 1
N <- 1e4

res <- map(seq_along(J), function(j) {
    set.seed(j)
    simulated_data <- simulate_data(N)
    lcmmtp(simulated_data,
           c("A1", "A2", "A3"),
           c("Y2", "Y3", "Y4"),
           c("M1", "M2", "M3"),
           c("D2", "D3", "D4"),
           NULL,
           list(c("L1"), c("L2"), c("L3")),
           list(NULL, NULL, NULL),
           NULL,
           function(data, trt) rep(1, length(data[[trt]])),
           function(data, trt) rep(1, length(data[[trt]])),
           id = NULL,
           control = .lcmmtp_control(folds = 1)) |>
        ife::tidy()
    }
) |>
    list_rbind()
