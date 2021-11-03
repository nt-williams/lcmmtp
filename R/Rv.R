lcm_Rv <- R6::R6Class(
    "lcm_Rv",
    cloneable = FALSE,
    public = list(
        D_Lt = NULL,
        D_Zt = NULL,
        D_Mt = NULL,
        #Should I include a matrix for the Mss...?
        initialize = function(Task) {
            self$D_Zt <- self$D_Mt <-
                Matrix::Matrix(
                    nrow = Task$seq[[1]] * nrow(Task$data),
                    ncol = Task$Npsem$tau + 1,
                    data = 0, sparse = TRUE
                )

            self$D_Lt <-
                Matrix::Matrix(
                    nrow = Task$seq[[1]] * nrow(Task$data),
                    ncol = Task$Npsem$tau,
                    data = 0, sparse = TRUE
                )

            self$D_Zt[1:(Task$n * Task$seq[[Task$Npsem$tau]]), Task$Npsem$tau + 1] <-
                rep(Task$data[[Task$Npsem$Y]], rep(Task$seq[[Task$Npsem$tau]], nrow(Task$data)))

            self$D_Mt[1:(Task$n * Task$seq[[Task$Npsem$tau]]), Task$Npsem$tau + 1] <- 1
        },
        #' Update random variable matrices
        update = function(rv = c("D_Lt", "D_Zt", "D_Mt"), x, t, idx) {
            self[[match.arg(rv)]][idx, t] <- x
            invisible(self)
        }
    )
)
