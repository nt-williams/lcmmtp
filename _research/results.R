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
