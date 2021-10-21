#' Perform Cross-fitting
#'
#' @param data the data, should contain training and prediction sets
#' @param y outcome variable name
#' @param x covariate variable names
#' @param type outcome variable type (i.e, "binomial", "continuous")
#' @param Folds an Folds object
#' @param fold_index index to be used for data sub-setting
#' @param lrnrs sl3 learners
CrossFit <- function(data, y, x, type = c("binomial", "continuous"), Folds, fold_index, lrnrs) {
    Tr <- Folds$Tr(data, fold_index)
    P <- Folds$P(data, fold_index)
    fit <- Regress(Tr, y, x, match.arg(type), lrnrs)
    Predict(fit, P)
}

#' Train an sl3 object
#'
#' @param Tr training data
#' @param y name of outcome
#' @param x names of covariates
#' @param type outcome type
#' @param lrnrs sl3 learners
Regress <- function(Tr, y, x, type, lrnrs) {
    task <- sl3::sl3_Task$new(
        data = Tr,
        covariates = x,
        outcome = y,
        outcome_type = type
    )

    lrnrs$train(task)
}

#' Predict from an sl3 fit
#'
#' @param fit a sl3 fit
#' @param P validation data
Predict <- function(fit, P) {
    task <- sl3::sl3_Task$new(
        data = P,
        covariates = fit$training_task$nodes$covariates
    )

    fit$predict(task)
}
