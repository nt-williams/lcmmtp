library(tidyverse)

source("simulation/R/read•zip.R")
source("simulation/R/dgm.r")

progressr::handlers(global = TRUE)

res <- map_dfr(c(500, 1000, 5000, 1e4), function(n) {
    bind_rows(read_zip(glue::glue("simulation/data/sims/hal-1-500-{n}-nS.zip")))
}, .id = "n")

truth <- true()
direct <- truth$theta10 - truth$theta00
indirect <- truth$theta11 - truth$theta10
total <- truth$theta11 - truth$theta00

group_by(res, n) |>
    summarise(across(c("direct", "indirect", "total"), var),
              across(paste0("var_", c("direct", "indirect", "total")), mean))

agg <-
    group_by(res, n) |>
    summarise(across(c("direct", "indirect", "total"), mean, .names = "{.col}_theta"),
              direct_bias = abs(mean(direct - (truth$theta10 - truth$theta00))),
              indirect_bias = abs(mean(indirect - (truth$theta11 - truth$theta10))),
              total_bias = abs(mean(total - (truth$theta11 - truth$theta00))),
              direct_coverage = coverage(direct, var_direct, (truth$theta10 - truth$theta00)),
              indirect_coverage = coverage(indirect, var_indirect, (truth$theta11 - truth$theta10)),
              total_coverage = coverage(total, var_total, truth$theta11 - truth$theta00)) |>
    mutate(n = c(500, 1000, 5000, 1e4)) |>
    pivot_longer(cols = starts_with(c("direct", "indirect", "total")),
                 names_to = c("effect", ".value"),
                 names_pattern = "(direct|indirect|total)_(.*)")

ggplot(agg, aes(x = sqrt(n), y = bias * sqrt(n), color = effect)) +
    geom_line()

coverage <- function(est, var, truth) {
    mean(
        purrr::map_lgl(
            purrr::map2(est, var, \(x, y)  x + c(-1, 1)*(sqrt(y))*qnorm(.975)),
            \(x) dplyr::between(truth, x[1], x[2])
        )
    )
}

coverage(res$direct, res$var_direct, direct)
coverage(res$indirect, res$var_indirect, indirect)
coverage(res$total, res$var_total, total)
