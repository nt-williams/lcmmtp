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
        initialize = function(data, Npsem) {
            self$Npsem <- Npsem$clone()
            self$data <- data.table::as.data.table(data[, self$Npsem$all_vars()])
            self$n <- nrow(data)
            k <- private$determine_k()
            self$seq <- lapply(1:self$Npsem$tau, \(t) k^((self$Npsem$tau + 1) - t))
        },
        augment = function(data, t) {
            ans <- data.table::as.data.table(lapply(data, rep, rep(self$seq[[t]], nrow(data))))
            ans[["*tmp_lcm_mediator_var*"]] <- rep(private$unique_M(), nrow(data))
            ans
        }
    ),
    private = list(
        determine_k = function() {
            length(private$unique_M())
        },
        unique_M = function() {
            unique(as.vector(as.matrix(self$data[, self$Npsem$M])))
        }
    )
)
