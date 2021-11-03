#' R6 class for an lcm_Folds object
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
        #' Map validation set indices from non-augmented data to the equivalent
        #'  indices in the augmented data set
        P_augmented_idx = function(Task, t, index) {
            do.call(c, lapply(
                self$folds[[index]]$validation_set,
                function(i) which(rep(1:Task$n, rep(Task$seq[[t]], Task$n)) == i)
            ))
        },
        #' Map training set indices from non-augmented data to the equivalent
        #'  indices in the augmented data set
        Tr_augmented_idx = function(Task, t, index) {
            do.call(c, lapply(
                self$folds[[index]]$training_set,
                function(i) which(rep(1:Task$n, rep(Task$seq[[t]], Task$n)) == i)
            ))
        }
    )
)
