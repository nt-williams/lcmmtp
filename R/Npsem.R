#' R6 class for an lcm_Npsem
#'
#' @export
lcm_Npsem <- R6::R6Class(
    "lcm_Npsem",
    public = list(
        W = NULL,
        L = NULL,
        A = NULL,
        M = NULL,
        Z = NULL,
        Y = NULL,
        tau = NULL,
        initialize = function(W, L, A, Z, M, Y) {
            self$tau <- length(A)

            checkmate::assertCharacter(W)
            checkmate::assertCharacter(A, len = self$tau)
            checkmate::assertCharacter(M, len = self$tau)
            checkmate::assertCharacter(Y, len = 1)
            checkmate::assertList(L, types = "character", len = self$tau)
            checkmate::assertList(Z, types = "character", len = self$tau)

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
        },
        #' Return the names of all variables
        all_vars = function() {
            c(self$W, unlist(self$L), self$A, unlist(self$Z), self$M, self$Y)
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
