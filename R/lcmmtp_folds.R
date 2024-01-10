# R6 class for an lcm_Folds object
lcmmtp_folds <- R6::R6Class(
    "lcm_folds",
    cloneable = FALSE,
    public = list(
        folds = NULL,
        V = NULL,
        initialize = function(n, V, cluster_ids = NULL) {
            self$folds <- origami::make_folds(n, V = V, cluster_ids = cluster_ids)

            if (V == 1) {
                self$folds[[1]]$training_set <- self$folds[[1]]$validation_set
            }

            self$V <- V
        },
        # Get training data from a given fold index
        Tr = function(data, index) {
            data[data$`lcmmtp_ID` %in% self$folds[[index]]$training_set, ]
        },
        # Get validation data from a given fold index
        P = function(data, index) {
            data[data$`lcmmtp_ID` %in% self$folds[[index]]$validation_set, ]
        }
    )
)
