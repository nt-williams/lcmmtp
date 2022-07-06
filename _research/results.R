suppressPackageStartupMessages(library(tidyverse))

source("_research/dgm.r")

read_zip <- function(tar) {
    files <- unzip(tar, list = TRUE)$Name
    p <- progressr::progressor(along = 1:length(files))
    purrr::map(files, function(file) {
        p()
        con <- unz(tar, file)
        read.csv(con)
    })
}

res <- map_dfr(c(`500` = 500, `1000` = 1000, `5000` = 5000, `1e4` = 1e4),
               function(n) {
                   bind_rows(read_zip(glue::glue("_research/data/res-{n}.zip")))
               }, .id = "n")

truth <- true()
truth$direct <- truth$theta10 - truth$theta00
truth$indirect <- truth$theta11 - truth$theta10
truth$total <- truth$theta11 - truth$theta00

coverage <- function(est, var, truth) {
    mean(
        map_lgl(
            map2(est, var, \(x, y)  x + c(-1, 1)*(sqrt(y))*qnorm(.975)),
            \(x) between(truth, x[1], x[2])
        )
    )
}

group_by(res, n) |>
    summarise(theta00_bias = abs(mean(theta_00 - truth$theta00)),
              theta00_coverage = coverage(theta_00, var_00, truth$theta00),
              theta10_bias = abs(mean(theta_10 - truth$theta10)),
              theta10_coverage = coverage(theta_10, var_10, truth$theta10),
              theta11_bias = abs(mean(theta_11 - truth$theta11)),
              theta11_coverage = coverage(theta_11, var_00, truth$theta11)) |>
    mutate(n = as.numeric(n)) |>
    pivot_longer(cols = "theta_00",
                 names_to = c("effect", ".value"),
                 names_pattern = "(direct|indirect|total)_(.*)") |>
    mutate(rootn_bias = sqrt(n) * bias, .before = coverage,
           effect = stringr::str_to_title(effect)) |>
    arrange(n)

agg <-
    group_by(res, n) |>
    summarise(direct_bias = abs(mean(direct - truth$direct)),
              indirect_bias = abs(mean(indirect - truth$indirect)),
              total_bias = abs(mean(total - truth$total)),
              direct_coverage = coverage(direct, var_direct, truth$direct),
              indirect_coverage = coverage(indirect, var_indirect, truth$indirect),
              total_coverage = coverage(total, var_total, truth$total)) |>
    mutate(n = as.numeric(n)) |>
    pivot_longer(cols = starts_with(c("direct", "indirect", "total")),
                 names_to = c("effect", ".value"),
                 names_pattern = "(direct|indirect|total)_(.*)") |>
    mutate(rootn_bias = sqrt(n) * bias, .before = coverage,
           effect = stringr::str_to_title(effect)) |>
    arrange(n)

make_table <- function(data) {
    data$n <- as.character(data$n)
    data <- mutate(data, across(where(is.numeric), \(x) round(x, 3)))
    data <- format(as.data.frame(data), nsmall = 2, digits = 2)
    brew::brew("simulation/scripts/table.brew")
}

make_table(agg)
