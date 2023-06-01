g <- glue::glue

G <- function(var, g, level, risk = TRUE, cens = TRUE) {
    out <- (as.numeric(var == level) * risk * cens) / g #b(g)
    out[is.na(out)] <- 0
    out
}

# for now, assume not a survival outcome
density_ratios <- function(pred, risk = TRUE, cens = TRUE) {
    (pred * risk * cens) / (1 - pmin(pred, 0.999))
}

K_p <- function(G, l, u) {
    if (l > u) {
        return(rep(1, nrow(G)))
    }

    out <- apply(as.matrix(G[, g("lcmmtp_Gp_A{l:u}")]), 1, prod)
    out
}

K_s <- function(G, l, u) {
    if (l > u) {
        return(rep(1, nrow(G)))
    }

    out <- apply(as.matrix(G[, g("lcmmtp_Gs_A{l:u}")]), 1, prod)
    out
}

H <- function(G, l, u) {
    if (l > u) {
        return(rep(1, nrow(G)))
    }

    out <- apply(as.matrix(G[, g("lcmmtp_G_M{l:u}")]), 1, prod)
    out
}

Sum <- function(x) Reduce(`+`, x)

b <- function(x) {
    pmax(x, .01)
}
