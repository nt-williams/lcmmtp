OutcomeRegression <- function(task, time, folds, control) {
    # Create initial outcome for regressions
    if (time == task$variables$timeHorizon) {
        task$augmented[[g("lcmmtp_D_Z{time+1}")]] <- task$augmented[[task$variables$outcome]]
        task$augmented[[g("lcmmtp_Q_Z{time+1}")]] <- task$augmented[[task$variables$outcome]]
    }

    predictions <- list()
    for (v in 1:folds$numberFolds) {
        # Create augmented training subset from folds
        training <- task$augment(folds$training(task$augmented, v), time)
        # Create augmented validation subset from folds
        validation <- task$augment(folds$validation(task$augmented, v), time)

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

        # Subset the training data to M=m
        training <- training[training[[g("lcmmtp_med_{time}")]] == training[[task$variables$mediator[time]]], ]

        # Estimate the outcome regression
        fit <- CrossFit(
            training,
            validation[atRisk & observedValidation, ],
            g("lcmmtp_D_Z{time+1}"),
            c(g("lcmmtp_med_{time:task$variables$timeHorizon}"), task$variables$history("M", time)),
            ifelse(time == task$variables$timeHorizon, task$outcomeType, "continuous"),
            control$learners_QZ,
            control$folds_QZ
        )

        if (time == task$variables$timeHorizon || task$outcomeType == "continuous") {
            validation[[g("lcmmtp_Q_L{time}")]][atRisk & observedValidation] <- fit$preds[[1]]
        } else {
            validation[[g("lcmmtp_Q_L{time}")]][atRisk & observedValidation] <-
                isotonic_constraint(predict(fit, training),
                                    training[[g("lcmmtp_D_Z{time+1}")]])(fit$preds[[1]])
        }

        # Assign deterministic probabilities for those who experience the outcome and those who experienced the competing event
        validation[[g("lcmmtp_Q_L{time}")]][!outcomeFreeValidation] <- 0
        validation[[g("lcmmtp_Q_L{time}")]][!competingRiskFreeValidation] <- 1

        predictions[[v]] <- validation
    }

    predictions <- Reduce(rbind, predictions)
    data.table::setorder(predictions, "lcmmtp_ID")
    task$augmented <- predictions

    return(invisible())
}
