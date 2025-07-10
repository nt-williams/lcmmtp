#' Perform Cross-fitting
#'
#' @param Tr training data
#' @param P prediction data
#' @param y outcome variable name
#' @param x covariate variable names
#' @param type outcome variable type (i.e, "binomial", "continuous")
#' @param learners
CrossFit <- function(Tr, P, y, x, type = c("binomial", "continuous"), learners, folds) {
    # browser()
    mlr3superlearner(data = Tr[, c("lcmmtp_ID", x, y)],
                     target = y,
                     library = learners,
                     outcome_type = match.arg(type),
                     folds = folds,
                     newdata = list(P),
                     group = "lcmmtp_ID",
                     discrete = TRUE)$preds[[1]]
}

cv_glmnet_formula <- function(formula, data, family, subset, ...) {
    y <- data[[as.character(formula[[2]])]]

    if (is.null(y)) {
        y <- get(as.character(formula[[2]]))
    }

    formula[[2]] <- NULL
    X <- model.matrix(formula, data = data)

    if (missing(family)) family <- "gaussian"

    if (!missing(subset)) {
        X <- X[subset, ]
        y <- y[subset]
    }

    out <- list(fit = cv.glmnet(X, y, family = family),
                formula = formula)
    class(out) <- "cv_glmnet_formula"
    out
}

predict.cv_glmnet_formula <- function(object, newdata, s = c("lambda.1se", "lambda.min"), ...) {
    X <- model.matrix(object$formula, data = newdata)
    predict(object$fit, newx = X, s = match.arg(s), type = "response", ...) |>
        as.vector()
}
