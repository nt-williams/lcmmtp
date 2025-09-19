# From https://github.com/herbps10/lmtp/blob/curve/R/isotonic_constraint.R
#' @importFrom OrdMonReg BoundedIsoMean
isotonic_constraint <- function(x, y) {
    na <- is.na(x) | is.na(y)
    x <- x[!na]
    y <- y[!na]

    o <- order(x)
    x <- x[o]
    y <- y[o]

    a <- rep(max(0, min(x)), length(x))
    b <- rep(max(0, min(1, max(x))), length(x))

    w <- rep(1, length(x))

    iso <- BoundedIsoMean(y, w, a, b)

    approxfun(x, iso, method = "constant", rule = 2, ties = mean)
}
