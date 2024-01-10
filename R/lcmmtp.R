#' Causal Mediation with Longitudinal Data Using Modified Treatment Policies
#'
#' @param data A data frame containing all necessary variables
#' @param vars An `lcmmtp_variables` object mapping observed variables to
#'  the assumed variable structure.
#' @param d_prime
#' @param d_star
#' @param control
#'
#' @return An object of class `lcmmtp`
#' @export
#'
#' @examples
#' vars <- lcmmtp_variables$new(
#'     L = list(c("L_1"), c("L_2")),
#'     A = c("A_1", "A_2"),
#'     Z = list(c("Z_1"), c("Z_2")),
#'     M = c("M_1", "M_2"),
#'     Y = "Y",
#'     cens = c("c1", "c2")
#' )
#'
#' d_ap <- function(data, trt) rep(1, length(data[[trt]]))
#' d_as <- function(data, trt) rep(0, length(data[[trt]]))
#'
#' lcmmtp(lcmmtp_foo, vars, d_ap, d_as, .lcmmtp_control(folds = 5))
lcmmtp <- function(data, vars, d_prime, d_star, id = NULL, control = .lcmmtp_control()) {
    checkmate::assertDataFrame(data[, vars$all_vars()])
    checkmate::assertR6(vars, "lcmmtp_variables")
    checkmate::assertNumber(control$folds, lower = 1, upper = nrow(data) - 1)
    checkmate::assertFunction(d_prime, nargs = 2)
    checkmate::assertFunction(d_star, nargs = 2)

    require("mlr3superlearner")

    task <- lcmmtp_task$new(data, vars, id, d_prime, d_star)
    Folds <- lcmmtp_folds$new(nrow(data), control$folds, id)

    for (t in vars$tau:1) {
        CrossFit_D_Lt(task, t, Folds, control)
        CrossFit_D_Zt_Mt(task, d_prime, d_star, t, Folds, control)
    }

    bar_M <- expand.grid(lapply(1:vars$tau, function(t) task$unique_M()))
    names(bar_M) <- g("lcmmtp_med_{1:vars$tau}")
    data.table::setDT(bar_M)

    nuis <- slider::slide(bar_M, function(bar_m) {
        comp <- lapply(1:Folds$V, function(v) {
            P_v <- Folds$P(data.table::merge.data.table(bar_m, task$augmented, all.x = TRUE), v)
            list(
                lambda_v = mean(P_v[["lcmmtp_D_M1"]]),
                theta_v = mean(P_v[["lcmmtp_D_Z1"]])
            )
        })

        dat <- merge(task$augmented, bar_m)
        theta <- mean(vapply(comp, function(x) x$theta_v, FUN.VALUE = 1))
        lambda <- mean(vapply(comp, function(x) x$lambda_v, FUN.VALUE = 1))
        S <- (dat$lcmmtp_D_Z1 - theta) * lambda + (dat$lcmmtp_D_M1 - lambda) * theta

        return(list(theta = theta, lambda = lambda, S = S))
    })

    S <- rowSums(sapply(nuis, function(m) m$S))
    S <- split(S, task$data$lcmmtp_ID)
    j <- length(S)

    ans <- list(
        theta = sum(vapply(nuis, function(m) m$theta * m$lambda, 1)),
        var = sqrt(var(vapply(S, function(x) mean(x), 1)) / j),
        S = as.vector(unlist(S))
    )

    class(ans) <- "lcmmtp"
    ans
}
