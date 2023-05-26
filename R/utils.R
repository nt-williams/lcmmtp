g <- glue::glue

G <- function(var, g, level, risk = TRUE) {
    (as.numeric(var == level) * risk) / g #b(g)
}

# for now, assume not a survival outcome
density_ratios <- function(pred, risk = TRUE) {
    (pred * risk) / (1 - pmin(pred, 0.999))
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

convert_to_surv <- function(x) {
    data.table::fcase(x == 0, 1,
                      x == 1, 0)
}
