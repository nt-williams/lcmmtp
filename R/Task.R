#' R6 class for a lcm_Task
#'
#' @section Public fields:
#' * `data`: A data.table containing variables defined by an Npsem object
#' * `Npsem`: A Npsem object
#' * `seq`: A list of values defining how to augment data at a given time point
#' * `n`: Number of observations
lcm_Task <- R6::R6Class(
    "lcm_Task",
    cloneable = FALSE,
    public = list(
        data = NULL,
        Npsem = NULL,
        seq = NULL,
        n = NULL,
        type = NULL,
        initialize = function(data, Npsem) {
            self$data <- data.table::as.data.table(data[, self$Npsem$all_vars()])
            self$Npsem <- Npsem$clone()
            self$n <- nrow(data)
            k <- private$determine_k()
            self$seq <- lapply(1:self$Npsem$tau, \(t) k^((self$Npsem$tau + 1) - t))
            self$type <- private$check_type()
        },
        augment = function(data, t) {
            m_underbar <- data.table::as.data.table(lapply(
                expand.grid(lapply(1:length(t:self$Npsem$tau), function(x) private$unique_M())),
                rep, nrow(data)
            ))

            names(m_underbar) <- glue::glue("*tmp_lcm_mediator_var_{self$Npsem$tau:t}*")
            ans <- data.table::as.data.table(lapply(data, rep, rep(self$seq[[t]], nrow(data))))
            cbind(ans, m_underbar)
        }
    ),
    private = list(
        determine_k = function() {
            length(private$unique_M())
        },
        unique_M = function() {
            unique(as.vector(as.matrix(self$data[, self$Npsem$M])))
        },
        check_type = function() {
            y <- self$data[[self$Npsem$Y]]
            checkmate::assertNumeric(y)
            if (all(y == 1 | y == 0)) return("binomial")
            "continuous"
        }
    )
)
