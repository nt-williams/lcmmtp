glmnet3 <- function(X, y, family = c("gaussian", "binomial"), id = NULL) {
    if (!is.null(id)) {
        # need to match index with fold number for cv.glmnet
        folds <- origami::make_folds(
            nrow(X), fold_fun = origami::folds_vfold,
            cluster_ids = id, V = 10
        )

        foldid <- vector("numeric", nrow(X))
        for (i in 1:nrow(X)) {
            for (v in 1:10) {
                if (i %in% folds[[v]]$validation_set) {
                    foldid[i] <- v
                    break
                }
            }
        }

    } else {
        foldid <- NULL
    }

    ans <- list(covars = names(X))

    if (ncol(X) == 1) {
        x <- as.matrix(X)
        ans$fit <- glm(y ~ ., data = cbind(y = y, X), family = match.arg(family))
    } else {
        f <- as.formula(paste0("~ .^", ncol(X)))
        x <- model.matrix(f, X)[, -1]
        ans$fit <- glmnet::cv.glmnet(x, y, family = match.arg(family), foldid = foldid)
    }

    # x <- try(model.matrix(f, X)[, -1])
    # if (inherits(x, "try-error")) browser()
    # ans$fit <- glmnet::cv.glmnet(x, y, family = match.arg(family), foldid = foldid)

    ans
}

predict.glmnet3 <- function(object, newx) {
    if (inherits(object$fit, "glm")) {
        return(predict(object$fit, newx, type = "response"))
    }

    X <- newx[, object$covars, drop = TRUE]
    f <- as.formula(paste0("~ .^", ncol(X)))
    X <- model.matrix(f, X)[, -1]
    as.vector(predict(object$fit, X, type = "response")[, 1])
}

Lrnr_glmnet3 <- R6::R6Class(
    classname = "Lrnr_glmnet3", inherit = sl3::Lrnr_base,
    portable = TRUE, class = TRUE,
    public = list(
        initialize = function(id = NULL, ...) {
            params <- sl3:::args_to_list()
            super$initialize(params = params, ...)
        }
    ),
    private = list(
        .properties = c("continuous", "binomial", "ids"),
        .required_packages = c("glmnet"),

        .train = function(task) {
            args <- self$params

            outcome_type <- self$get_outcome_type(task)

            if (is.null(args$family)) {
                args$family <- outcome_type$glm_family()
            }

            # specify data
            args$X <- as.data.frame(task$X)
            args$y <- outcome_type$format(task$Y)

            # call a function that fits your algorithm
            # with the argument list you constructed
            fit_object <- sl3:::call_with_args(glmnet3, args)
            return(fit_object)
        },

        # .predict takes a task and returns predictions from that task
        .predict = function(task = NULL) {
            self$training_task
            self$training_outcome_type
            self$fit_object

            predictions <- predict.glmnet3(self$fit_object, as.data.frame(task$X))
            return(predictions)
        }
    )
)
