library(glue)
source("_research/dgm.R")

id <- Sys.getenv("SGE_TASK_ID")
if (id == "undefined" || id == "") id <- 1

quit("no")
