# R6 class for a lcmmtp_Task
lcmmtp_Task <- R6::R6Class(
    "lcmmtp_Task",
    cloneable = FALSE,
    public = list(
        data = NULL,
        augmented = NULL,
        shifted_aprime = NULL,
        shifted_astar = NULL,
        vars = NULL,
        n = NULL,
        type = NULL,
        initialize = function(data, vars, d_prime, d_star) {
            self$data <- data.table::as.data.table(data[, vars$all_vars()])
            self$data[["lcmmtp_ID"]] <- seq.int(nrow(self$data))
            self$augmented <- self$data
            self$vars <- vars$clone()
            self$n <- nrow(data)
            self$type <- private$check_type()
            self$shifted_aprime <- self$shift_trt(self$data, self$vars$A, d_prime)
            self$shifted_astar <- self$shift_trt(self$data, self$vars$A, d_star)
        },
        # Create augmented data for pooled regressions
        augment = function(data, t) {
            m_underbar <- data.table::as.data.table(
                lapply(expand.grid(self$unique_M()), rep, nrow(data)
                )
            )

            names(m_underbar) <- g("lcmmtp_med_{t}")
            ans <- data.table::as.data.table(lapply(data, rep, rep(private$determine_k(), nrow(data))))
            cbind(ans, m_underbar)
        },
        unique_M = function() {
            unique(as.vector(as.matrix(self$data[, self$vars$M])))
        },
        stack_data = function(data, shifted, t) {
            shifted_half <- data

            if (length(self$vars$A) > 1 || t == 1) {
                shifted_half[[self$vars$A[t]]] <- shifted[[self$vars$A[t]]]
            }

            out <- rbind(data, shifted_half)
            out[["tmp_lcmmtp_stack_indicator"]] <- rep(c(0, 1), each = nrow(data))
            out
        },
        shift_trt = function(data, trt, .f) {
            for (a in trt) {
                data[[a]] <- .f(data, a)
            }
            data
        }
    ),
    private = list(
        determine_k = function() {
            length(self$unique_M())
        },
        check_type = function() {
            y <- self$data[[self$vars$Y]]
            checkmate::assertNumeric(y)
            if (all(y == 1 | y == 0)) return("binomial")
            "continuous"
        }
    )
)
