# # R6 class for a lcm_Rv object
# lcm_Rv <- R6::R6Class(
#     "lcm_Rv",
#     cloneable = FALSE,
#     public = list(
#         D_Lt = NULL,
#         D_Zt = NULL,
#         D_Mt = NULL,
#         Gp_At = NULL,
#         Gs_At = NULL,
#         G_Mt = NULL,
#         initialize = function(Task) {
#             self$D_Zt <- self$D_Mt <-
#                 lapply(c(Task$seq, Task$seq[[Task$Npsem$tau]]), \(x) vector("double", x * nrow(Task$data)))
#                 # sparseMatrix(Task$seq[[1]] * nrow(Task$data), Task$Npsem$tau + 1)
#
#             self$D_Lt <- lapply(Task$seq, \(x) vector("double", x * nrow(Task$data)))
#                 # sparseMatrix(Task$seq[[1]] * nrow(Task$data), Task$Npsem$tau)
#
#             # incorrect number of rows here! needs to be the number of augmented!
#             # Should be fixed, double check.
#             # Still don't think this is correct. We should predict and then augment on the predictions.
#             #  Otherwise, we can't do the products...
#             self$Gp_At <- self$Gs_At <- self$G_Mt <-
#                 lapply(Task$seq, \(x) vector("double", x * nrow(Task$data)))
#                 # sparseMatrix(nrow(Task$data), Task$Npsem$tau)
#
#             self$D_Zt[[Task$Npsem$tau + 1]] <-
#                 rep(Task$data[[Task$Npsem$Y]], rep(Task$seq[[Task$Npsem$tau]], nrow(Task$data)))
#             # self$D_Zt[1:(Task$n * Task$seq[[Task$Npsem$tau]]), Task$Npsem$tau + 1] <-
#             #     rep(Task$data[[Task$Npsem$Y]], rep(Task$seq[[Task$Npsem$tau]], nrow(Task$data)))
#
#             self$D_Mt[[Task$Npsem$tau + 1]] <- rep(1, Task$n * Task$seq[[Task$Npsem$tau]])
#             # self$D_Mt[1:(Task$n * Task$seq[[Task$Npsem$tau]]), Task$Npsem$tau + 1] <- 1
#         },
#         # Update random variable matrices
#         update = function(rv = c("D_Lt", "D_Zt", "D_Mt", "Gp_At", "Gs_At", "G_Mt"), x, t, idx) {
#             self[[match.arg(rv)]][[t]][idx] <- x
#             invisible(self)
#         }
#     )
# )
