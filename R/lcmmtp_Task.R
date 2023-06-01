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
            tmp <- data.table::copy(data)

            if (!is.null(vars$risk)) {
                for (y in c(vars$risk, vars$Y)) {
                    data.table::set(tmp,
                                    j = y,
                                    value = private$convert_to_surv(tmp[[y]]))
                }
            }

            if (!is.null(vars$cens)) {
                for (y in c(vars$risk, vars$Y)) {
                    data.table::set(tmp,
                                    j = y,
                                    value = ifelse(is.na(tmp[[y]]), -999, tmp[[y]]))
                }
            }

            self$data <- data.table::as.data.table(tmp[, vars$all_vars()])
            self$data[["lcmmtp_ID"]] <- seq.int(nrow(self$data))
            self$augmented <- self$data
            self$vars <- vars$clone()
            self$n <- nrow(self$data)
            self$type <- private$check_type()
            self$shifted_aprime <- self$shift_trt(self$data, self$vars$A, self$vars$cens, d_prime)
            self$shifted_astar <- self$shift_trt(self$data, self$vars$A, self$vars$cens, d_star)
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
            M <- self$data[, self$vars$M]
            unique(as.vector(as.matrix(M[complete.cases(M), ])))
        },
        stack_data = function(data, shifted, t) {
            shifted_half <- data

            if (length(self$vars$A) > 1 || t == 1) {
                shifted_half[[self$vars$A[t]]] <- shifted[[self$vars$A[t]]]
            }

            if (!is.null(self$vars$cens)) {
                shifted_half[[self$vars$cens[t]]] <- shifted[[self$vars$cens[t]]]
            }

            out <- rbind(data, shifted_half)
            out[["tmp_lcmmtp_stack_indicator"]] <- rep(c(0, 1), each = nrow(data))
            out
        },
        shift_trt = function(data, trt, cens = NULL, .f) {
            for (a in trt) {
                data[[a]] <- .f(data, a)
            }

            if (is.null(cens)) {
                return(data)
            }

            for (cs in cens) {
                data[[cs]] <- 1
            }
            data
        },
        at_risk = function(data, t) {
            if (is.null(vars$risk)) {
                return(rep(TRUE, nrow(data)))
            }

            if (t == 1) {
                return(rep(TRUE, nrow(data)))
            }

            data[[vars$risk[t - 1]]] == 1 & !is.na(data[[vars$risk[t - 1]]])
        },
        observed = function(data, t, lag = FALSE) {
            if (is.null(self$vars$cens)) {
                return(rep(TRUE, nrow(data)))
            }

            if (!lag) {
                return(data[[self$vars$cens[t]]] == 1)
            }

            if (lag && t > 1) {
                return(data[[self$vars$cens[t - 1]]] == 1)
            }

            rep(TRUE, nrow(data))
        }
    ),
    private = list(
        determine_k = function() {
            length(self$unique_M())
        },
        check_type = function() {
            y <- self$data[[self$vars$Y]]
            checkmate::assertNumeric(y)
            if (all(y == 1 | y == 0, na.rm = T)) return("binomial")
            "continuous"
        },
        convert_to_surv = function(x) {
            data.table::fcase(x == 0, 1,
                              x == 1, 0)
        }
    )
)
