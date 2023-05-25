stack_data <- function(natural, shifted, trt, cens, tau) {
    shifted_half <- natural

    if (length(trt) > 1 || tau == 1) {
        shifted_half[[trt[tau]]] <- shifted[[trt[tau]]]
    }

    if (!is.null(cens)) {
        shifted_half[[cens[tau]]] <- shifted[[cens[tau]]]
    }

    out <- rbind(natural, shifted_half)
    out[["tmp_lcmmtp_stack_indicator"]] <- rep(c(0, 1), each = nrow(natural))
    out
}
