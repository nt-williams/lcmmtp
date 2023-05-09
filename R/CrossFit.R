#' Perform Cross-fitting
#'
#' @param Tr training data
#' @param P prediction data
#' @param y outcome variable name
#' @param x covariate variable names
#' @param type outcome variable type (i.e, "binomial", "continuous")
#' @param lrnrs sl3 learner
CrossFit <- function(Tr, P, y, x, type = c("binomial", "gaussian"), lrnrs) {
    fit <- Regress(Tr, y, x, match.arg(type), lrnrs)
    Predict(fit, P)
}

#' Train an sl3 object
#'
#' @param Tr training data
#' @param y name of outcome
#' @param x names of covariates
#' @param type outcome type
#' @param lrnrs sl3 learner
Regress <- function(Tr, y, x, type, lrnrs) {
    type <- ifelse(type == "gaussian", "continuous", type)
    task <- sl3::sl3_Task$new(
        data = Tr,
        covariates = x,
        outcome = y,
        id = "lcm_ID",
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
