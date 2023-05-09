#' Perform Cross-fitting
#'
#' @param Tr training data
#' @param P prediction data
#' @param y outcome variable name
#' @param x covariate variable names
#' @param type outcome variable type (i.e, "binomial", "continuous")
#' @param learners
CrossFit <- function(Tr, P, y, x, type = c("binomial", "gaussian"), learners) {
    mlr3superlearner(data = Tr[, c("lcm_ID", x, y)],
                     target = y,
                     library = learners,
                     type = match.arg(type),
                     folds = 10,
                     newdata = list(P),
                     group = "lcm_ID")$preds[[1]]
}
