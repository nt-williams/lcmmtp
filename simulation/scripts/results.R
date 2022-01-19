source("simulation/R/read•zip.R")

progressr::handlers(global = TRUE)

tar <- "simulation/data/sims/results.zip"
res <- read_zip(tar)

res <- bind_rows(res)
