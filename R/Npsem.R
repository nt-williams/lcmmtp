#' Create an Npsem Object
#'
#' @export
Npsem <- R6::R6Class(
    "Npsem",
    cloneable = FALSE,
    public = list(
        W = NULL,
        L = NULL,
        A = NULL,
        M = NULL,
        Z = NULL,
        Y = NULL,
        initialize = function(W, L, A, Z, M, Y) {
            self$W <- W
            self$L <- L
            self$A <- A
            self$Z <- Z
            self$M <- M
            self$Y <- Y
        },
        #' Get all parent nodes for a variable
        history = function(var = c("L", "A", "Z", "M", "Y"), t) {
            switch(
                match.arg(var),
                L = private$parents_L(t),
                A = private$parents_A(t),
                Z = private$parents_Z(t),
                M = private$parents_M(t),
                Y = private$parents_Y()
            )
        }
    ),
    private = list(
        parents_L = function(t) {
            if (t == 1) {
                return(self$W)
            }

            c(private$parents_M(t - 1), self$M[t - 1])
        },
        parents_A = function(t) {
            c(private$parents_L(t), unlist(self$L[[t]]))
        },
        parents_Z = function(t) {
            c(private$parents_A(t), self$A[t])
        },
        parents_M = function(t) {
            c(private$parents_Z(t), unlist(self$Z[[t]]))
        },
        parents_Y = function() {
            c(self$W, unlist(self$L), self$A, unlist(self$Z), self$M)
        }
    )
)
