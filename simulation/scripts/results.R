library(tidyverse)

# options(collapse_mask = "manip")
# library(collapse)

source("simulation/R/read•zip.R")
source("simulation/R/dgm.r")

progressr::handlers(global = TRUE)

res <- map_dfr(c(`500` = 500, `1000` = 1000, `5000` = 5000, `1e4` = 1e4), function(n) {
    bind_rows(read_zip(glue::glue("simulation/data/sims/sl-1-500-{n}-updated.zip")))
}, .id = "n")

truth <- true()
truth$direct <- truth$theta10 - truth$theta00
truth$indirect <- truth$theta11 - truth$theta10
truth$total <- truth$theta11 - truth$theta00

# group_by(res, n) |>
#     summarise(across(c("direct", "indirect", "total"), var),
#               across(paste0("var_", c("direct", "indirect", "total")), mean))

coverage <- function(est, var, truth) {
    mean(
        map_lgl(
            map2(est, var, \(x, y)  x + c(-1, 1)*(sqrt(y))*qnorm(.975)),
            \(x) between(truth, x[1], x[2])
        )
    )
}

agg <-
    group_by(res, n) |>
    summarise(across(c("direct", "indirect", "total"), mean, .names = "{.col}_theta"),
              direct_bias = abs(mean(direct - truth$direct)),
              indirect_bias = abs(mean(indirect - truth$indirect)),
              total_bias = abs(mean(total - truth$total)),
              direct_coverage = coverage(direct, var_direct, truth$direct),
              indirect_coverage = coverage(indirect, var_indirect, truth$indirect),
              total_coverage = coverage(total, var_total, truth$total)) |>
    mutate(n = as.numeric(n)) |>
    pivot_longer(cols = starts_with(c("direct", "indirect", "total")),
                 names_to = c("effect", ".value"),
                 names_pattern = "(direct|indirect|total)_(.*)") |>
    arrange(n)

ggplot(agg, aes(x = sqrt(n), y = bias * sqrt(n), color = effect)) +
    geom_line()
