g <- glue::glue

G <- function(var, g, level) {
    as.numeric(var == level) / b(g)
}

K_p <- function(G, l, u) {
    if (l > u) {
        return(rep(1, nrow(G)))
    }

    apply(as.matrix(G[, g("lcm_Gp_A{l:u}")]), 1, prod)
}

K_s <- function(G, l, u) {
    if (l > u) {
        return(rep(1, nrow(G)))
    }

    apply(as.matrix(G[, g("lcm_Gs_A{l:u}")]), 1, prod)
}

H <- function(G, l, u) {
    if (l > u) {
        return(rep(1, nrow(G)))
    }

    apply(as.matrix(G[, g("lcm_G_M{l:u}")]), 1, prod)
}

Sum <- function(x) Reduce(`+`, x)

b <- function(x) {
    pmax(x, .01)
}
