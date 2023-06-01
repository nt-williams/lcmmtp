#' R6 class for an lcmmtp_variables
#'
#' @export
lcmmtp_variables <- R6::R6Class(
    "lcmmtp_variables",
    public = list(
        W = NULL,
        L = NULL,
        A = NULL,
        M = NULL,
        Z = NULL,
        Y = NULL,
        risk = NULL,
        cens = NULL,
        tau = NULL,
        initialize = function(W, L, A, Z, M, Y, cens) {
            checkmate::assertCharacter(A)
            checkmate::assertCharacter(Y)

            self$A <- A
            self$Y <- Y[length(Y)]
            if (length(Y) > 1) {
                self$risk <- Y[1:(length(Y) - 1)]
            }

            self$tau <- private$what_is_tau()

            if (!missing(W)) {
                checkmate::assertCharacter(W)
                self$W <- W
            }

            if (!missing(cens)) {
                checkmate::assertCharacter(cens, len = self$tau)
                self$cens <- cens
            }

            checkmate::assertCharacter(M, len = self$tau)
            checkmate::assertList(L, types = "character", len = self$tau)
            checkmate::assertList(Z, types = c("character", "null"), len = self$tau)

            self$L <- L
            self$Z <- Z
            self$M <- M

            invisible(self)
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
            c(self$W, unlist(self$L), self$A, unlist(self$Z), self$M, self$risk, self$cens, self$Y)
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
        },
        what_is_tau = function() {
            if (is.null(self$risk)) {
                return(length(self$A))
            }
            length(self$risk) + 1
        }
    )
)
