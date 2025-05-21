MediatorRegression <- function(task, time, folds, d_star, control) {
    if (time == task$variables$timeHorizon) {
        task$augmented[[g("lcmmtp_D_M{time+1}")]] <- 1
        task$augmented[[g("lcmmtp_Q_M{time+1}")]] <- 1
    }

    predictions <- list()
    for (v in 1:folds$numberFolds) {
        # Create augmented training subset from folds
        training <- folds$training(task$augmented, v)
        # Create augmented validation subset from folds
        validation <- folds$validation(task$augmented, v)

        # Create indicators for the subset of observations to use for training
        outcomeFree <- task$outcomeFree(training, time-1)
        competingRiskFree <- task$competingRiskFree(training, time-1)
        observed <- task$observed(training, time, TRUE)

        # Subset data
        training <- training[outcomeFree & competingRiskFree & observed, ]

        # Create indicators for the subset of observations to use for predictions
        outcomeFreeValidation <- task$outcomeFree(validation, time-1)
        competingRiskFreeValidation <- task$competingRiskFree(validation, time-1)
        observedValidation <- task$observed(validation, time, TRUE)
        atRisk <- outcomeFreeValidation & competingRiskFreeValidation

        # Create outcome variable
        training[[g("lcmmtp_D_M{time+1}")]] <- (training[[g("lcmmtp_med_{time}")]] == training[[task$variables$mediator[time]]]) * training[[g("lcmmtp_D_M{time+1}")]]

        # Figure out what the treatment variable is
        treatment_t <- task$currentTreatment(time)

        # Estimate the probability of M = m in the pooled data
        validation[[g("lcmmtp_Q_M{time}")]][atRisk & observedValidation] <- CrossFit(
            training,
            task$shiftTreatment(validation[atRisk & observedValidation, ], treatment_t, task$variables$censoring[time], d_star),
            g("lcmmtp_D_M{time+1}"),
            c(g("lcmmtp_med_{time:task$variables$timeHorizon}"), task$variables$history("A", time), treatment_t),
            ifelse(time == task$variables$timeHorizon, "binomial", "continuous"),
            control$learners_QM,
            control$folds_QM
        )

        # Assign deterministic probabilities for the value of the history of M
        # If the entire history of M is zero, the probability is 1
        # If the history of M contains any non-zero, the probability is 0
        historyIsZero <- apply(validation[, g("lcmmtp_med_{time:task$variables$timeHorizon}"), drop = FALSE] == 0, 1, prod)
        validation[[g("lcmmtp_Q_M{time}")]][!historyIsZero & !atRisk] <- 0
        validation[[g("lcmmtp_Q_M{time}")]][historyIsZero & !atRisk] <- 1

        predictions[[v]] <- validation
    }

    predictions <- Reduce(rbind, predictions)
    data.table::setorder(predictions, "lcmmtp_ID")
    task$augmented <- predictions

    return(invisible())
}
