#' Set Estimation Hyperparameters
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' .lcmmtp_control(folds = 5, learners_trt = c("glm", "earth"))
.lcmmtp_control <- function(...) {
    change <- list(...)
    params <- list(folds = 10L,
                   folds_trt = 10L,
                   folds_mediator = 10L,
                   folds_QL = 10L,
                   folds_QZ = 10L,
                   folds_QM = 10L,
                   learners_trt = "glm",
                   learners_mediator = "glm",
                   learners_QL = "glm",
                   learners_QZ = "glm",
                   learners_QM = "glm")

    if (length(change) == 0) return(params)
    change <- change[names(change) %in% names(params)]
    params[names(change)] <- change
    params
}
