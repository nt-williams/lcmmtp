G <- function(var, g, level) {
    as.numeric(var == level) / g
}

# G should be a matrix with rows for subjects and columns for time point
K <- function(G, l, u) {
    apply(G[, l:u], 1, prod)
}

# Same as K, but G is the matrix for G_M instead of G_A' or G_A*
H <- K
