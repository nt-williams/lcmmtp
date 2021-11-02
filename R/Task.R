lcm_Task <- R6::R6Class(
    "lcm_Task",
    cloneable = FALSE,
    public = list(
        data = NULL,
        Npsem = NULL,
        seq = NULL,
        n = NULL,
        initialize = function(data, Npsem) {
            self$data <- data.table::as.data.table(data[, Npsem$all_vars()])
            self$Npsem <- Npsem
            self$n <- nrow(data)
            k <- determine_k(data, Npsem)
            self$seq <- lapply(1:Npsem$tau, \(t) k^((Npsem$tau + 1) - t))
        },
        augment = function(data, t) {
            ans <- as.data.frame(lapply(data, rep, rep(self$seq[[t]], nrow(data))))
            ans[["*tmp_lcm_mediator_var*"]] <-
                rep(unique_M(data, self$Npsem), self$seq[[t]])
            ans
        }
    ),
    private = list(
        determine_k = function(data, Npsem) {
            length(unique_M(data, Npsem))
        },
        unique_M = function(data, Npsem) {
            unique(as.vector(as.matrix(data[, Npsem$M])))
        }
    )
)
