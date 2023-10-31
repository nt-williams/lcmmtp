#' Perform Cross-fitting
#'
#' @param Tr training data
#' @param P prediction data
#' @param y outcome variable name
#' @param x covariate variable names
#' @param type outcome variable type (i.e, "binomial", "continuous")
#' @param learners
CrossFit <- function(Tr, P, y, x, type = c("binomial", "continuous"), learners, folds) {
    mlr3superlearner(data = Tr[, c("lcmmtp_ID", x, y)],
                     target = y,
                     library = learners,
                     outcome_type = match.arg(type),
                     folds = folds,
                     newdata = list(P),
                     group = "lcmmtp_ID",
                     discrete = FALSE)$preds[[1]]
}
