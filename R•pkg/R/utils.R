g <- glue::glue

G <- function(var, g, level) {
    as.numeric(var == level) / g
}

# G should be a matrix with rows for subjects and columns for time point
K_p <- function(G, l, u) {
    # apply(G[, l:u, drop = FALSE], 1, prod)
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

# Same as K, but G is the matrix for G_M instead of G_A' or G_A*
H <- function(G, l, u) {
    if (l > u) {
        return(rep(1, nrow(G)))
    }

    apply(as.matrix(G[, g("lcm_G_M{l:u}")]), 1, prod)
}
