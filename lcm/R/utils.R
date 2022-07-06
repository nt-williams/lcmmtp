g <- glue::glue

G <- function(var, g, level) {
    as.numeric(var == level) / b(g)
}

K_p <- function(G, l, u) {
    if (l > u) {
        return(rep(1, nrow(G)))
    }

    out <- apply(as.matrix(G[, g("lcm_Gp_A{l:u}")]), 1, prod)
    trim <- quantile(out[out != 0], 0.99)
    ## trim <- 1 / pmin(0.001, 0.01^log(u - l))
    pmin(out, trim)
}

K_s <- function(G, l, u) {
    if (l > u) {
        return(rep(1, nrow(G)))
    }

    out <- apply(as.matrix(G[, g("lcm_Gs_A{l:u}")]), 1, prod)
    trim <- quantile(out[out != 0], 0.99)
    ## trim <- 1 / pmin(0.001, 0.01^log(u - l))
    pmin(out, trim)
}

H <- function(G, l, u) {
    if (l > u) {
        return(rep(1, nrow(G)))
    }

    out <- apply(as.matrix(G[, g("lcm_G_M{l:u}")]), 1, prod)
    trim <- quantile(out[out != 0], 0.99)
    ## trim <- 1 / pmin(0.001, 0.01^log(u - l))
    pmin(out, trim)
}

Sum <- function(x) Reduce(`+`, x)

b <- function(x) {
    pmax(x, .01)
}
