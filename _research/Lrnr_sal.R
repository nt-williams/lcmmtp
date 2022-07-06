sal <- function(X, y, family = c("gaussian", "binomial"), ratio = 0.3) {
    ind <- origami::make_folds(nrow(X), fold_fun = origami::folds_montecarlo, V = 1, pvalidation = ratio)[[1]]

    if (inherits(X, "data.frame")) {
        X <- as.matrix(X)
    }

    X_train <- X[ind$training_set, , drop = FALSE]
    y_train <- y[ind$training_set]
    X_val <- X[ind$validation_set, , drop = FALSE]
    y_val <- y[ind$validation_set]

    params <- tune(X_train, y_train, X_val, y_val, family)

    bases <- list()
    R <- y - mean(y)

    for (i in 1:params$k) {

        bases[[i]] <- lightgbm::lgb.train(
            list(
                max_depth = params$max_depth,
                min_data_in_leaf = 2
            ),
            data = lightgbm::lgb.Dataset(
                data = as.matrix(X),
                label = as.numeric(R)
            ),
            nrounds = 100,
            obj = "regression",
            verbose = -1
        )

        H <- predict_bases(bases, X)

        if (i == 1) {
            H <- cbind(1, H)
        }

        lasso <- glmnet::glmnet(H, y, lambda = params$lambda, family = match.arg(family))
        R <- y - predict(lasso, newx = H, type = "response")
    }

    fit <- list(bases = bases, lasso = lasso, k = params$k)
    class(fit) <- "sal"
    fit
}

predict_bases <- function(bases, X) {
    sapply(bases, function(base) predict(base, X), simplify = "array")
}

predict.sal <- function(fit, newdata) {
    if (inherits(newdata, "data.frame")) {
        newdata <- as.matrix(newdata)
    }

    H <- predict_bases(fit$bases, newdata)

    if (fit$k == 1) {
        H <- cbind(1, H)
    }

    predict(fit$lasso, newx = H, type = "response")[, 1]
}

tune <- function(X_train, y_train, X_val, y_val, family, eps = 1e-06, lambda_ratio = 0.1, max_depth_inc = 2) {
    sal_fit <- tune_k(X_train, y_train, X_val, y_val, family, eps, 0.01, 2)
    new_sal_fit <- tune_k(X_train, y_train, X_val, y_val, family, eps, 0.01 * lambda_ratio, 2 + max_depth_inc)

    while ((new_sal_fit$mse + eps) < sal_fit$mse) {
        sal_fit <- new_sal_fit
        new_sal_fit <- tune_k(
            X_train, y_train, X_val, y_val, family, eps,
            new_sal_fit$lambda * lambda_ratio,
            new_sal_fit$max_depth + max_depth_inc
        )
    }

    list(k = sal_fit$k, lambda = sal_fit$lambda, max_depth = sal_fit$max_depth)
}

tune_k <- function(X_train, y_train, X_val, y_val, family, eps = 1e-06, lambda, max_depth) {
    bases <- list()
    R_train <- y_train - mean(y_train)
    R_val <- y_val - mean(y_val)
    mse <- Inf
    new_mse <- mean(R_val^2)
    k <- 0

    while ((new_mse + eps) < mse) {
        k <- k + 1
        mse <- new_mse

        bases[[k]] <- lightgbm::lgb.train(
            list(
                max_depth = max_depth,
                min_data_in_leaf = 2
            ),
            data = lightgbm::lgb.Dataset(
                data = as.matrix(X_train),
                label = as.numeric(R_train)
            ),
            nrounds = 100,
            obj = "regression",
            verbose = -1
        )

        H_train <- predict_bases(bases, X_train)
        H_val <- predict_bases(bases, X_val)

        if (k == 1) {
            H_train <- cbind(1, H_train)
            H_val <- cbind(1, H_val)
        }

        lasso <- glmnet::glmnet(H_train, y_train, lambda = lambda, family = family)
        R_train <- y_train - predict(lasso, newx = H_train, type = "response")
        R_val <- y_val - predict(lasso, newx = H_val, type = "response")

        new_mse <- mean(R_val^2)
    }

    list(k = ifelse(k > 1, k - 1, k), mse = mse, lambda = lambda, max_depth = max_depth)
}

Lrnr_sal <- R6::R6Class(
    classname = "Lrnr_sal", inherit = sl3::Lrnr_base,
    portable = TRUE, class = TRUE,
    public = list(
        initialize = function(ratio = 0.2, ...) {
            params <- sl3::args_to_list()
            super$initialize(params = params, ...)
        }
    ),
    private = list(
        .properties = c("continuous", "binomial"),

        .train = function(task) {
            args <- self$params

            outcome_type <- self$get_outcome_type(task)

            if (outcome_type$type == "continuous") {
                args$family <- "gaussian"
            } else {
                args$family <- outcome_type$type
            }

            args$X <- as.matrix(task$X)
            args$y <- task$Y

            fit_object <- sl3:::call_with_args(sal, args)
            fit_object
        },

        .predict = function(task = NULL) {
            predict.sal(self$fit_object, as.matrix(task$X))
        }
    )
)
