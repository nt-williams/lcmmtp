#' Create a Folds object
#'
#' @export
Folds <- R6::R6Class(
    "Folds",
    cloneable = FALSE,
    public = list(
        folds = NULL,
        initialize = function(n) {
            self$folds <- origami::make_folds(n)
        },
        #' Get training data from a given fold index
        Tr = function(data, index) {
            origami::training(data, self$folds[[index]])
        },
        #' Get validation data from a given fold index
        P = function(data, index) {
            origami::validation(data, self$folds[[index]])
        }
    )
)
