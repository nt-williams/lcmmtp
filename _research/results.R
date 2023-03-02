suppressPackageStartupMessages(library(tidyverse))

source("_research/dgm.r")

framework <- "sl3"

read_zip <- function(tar) {
    files <- unzip(tar, list = TRUE)$Name
    p <- progressr::progressor(along = 1:length(files))
    purrr::map(files, function(file) {
        p()
        con <- unz(tar, file)
        read.csv(con)
    })
}

res <- map_dfr(c(`500` = 500, `1000` = 1000, `5000` = 5000),
               function(n) {
                   bind_rows(read_zip(glue::glue("_research/data/{framework}_{n}_dgp4.zip")))
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

agg <-
    filter(res, abs(direct) < 1, abs(indirect) < 1) |>
    group_by(n) |>
    summarise(direct_bias = abs(mean(direct - truth$direct)),
              indirect_bias = abs(mean(indirect - truth$indirect)),
              total_bias = abs(mean(total - truth$total)),
              across(c("direct", "indirect", "total"), var, .names = "{.col}_nvar"),
              across(c("var_direct", "var_indirect", "var_total"), mean, .names = "{gsub('var_', '', .col)}_meanvar"),
              direct_nmse = mean((direct - truth$direct)^2),
              indirect_nmse = mean((indirect - truth$indirect)^2),
              total_nmse = mean((total - truth$total)^2),
              direct_coverage = coverage(direct, var_direct, truth$direct),
              indirect_coverage = coverage(indirect, var_indirect, truth$indirect),
              total_coverage = coverage(total, var_total, truth$total)) |>
    mutate(n = as.numeric(n)) |>
    pivot_longer(cols = starts_with(c("direct", "indirect", "total")),
                 names_to = c("effect", ".value"),
                 names_pattern = "(direct|indirect|total)_(.*)") |>
    mutate(rootn_bias = sqrt(n) * bias, .before = coverage,
           across(c("nmse", "nvar", "meanvar"), \(x) x * n),
           effect = stringr::str_to_title(effect)) |>
    arrange(n) |>
    select(n, effect, bias, rootn_bias, nvar, nmse, coverage)

make_table <- function(data) {
    data$n <- as.character(data$n)
    data <- mutate(data, across(where(is.numeric), \(x) round(x, 3)))
    data <- format(as.data.frame(data), nsmall = 2, digits = 2)
    brew::brew("_research/table.brew")
}

make_table(agg)
