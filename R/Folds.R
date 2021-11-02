#' Create a Folds object
#'
#' @export
lcm_Folds <- R6::R6Class(
    "lcm_Folds",
    cloneable = FALSE,
    public = list(
        folds = NULL,
        V = NULL,
        initialize = function(n, V) {
            self$folds <- origami::make_folds(n, V = V)
            self$V <- V
        },
        #' Get training data from a given fold index
        Tr = function(data, index) {
            origami::training(data, self$folds[[index]])
        },
        #' Get validation data from a given fold index
        P = function(data, index) {
            origami::validation(data, self$folds[[index]])
        },
        valid_idx = function(index) {
            self$folds[[index]]$validation_set
        }
    )
)
