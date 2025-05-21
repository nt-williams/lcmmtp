CrossFitDensityRatios <- function(task, time, folds, control) {
    predictions <- list()
    for (v in 1:folds$numberFolds) {
        # Create augmented validation subset from folds
        validation <- folds$validation(task$augmented, v)

        # Create pre-allocated vectors to store predictions
        predictionsMediator <- predictionsAStar <- predictionsAPrime <- predictionsCensoring <- vector("numeric", nrow(validation))

        # Create stacked data for training under a-prime shift
        stackedData <- task$stackData(folds$training(task$data, v), folds$training(task$shiftedUnderAPrime, v), time)

        # Create indicators for the subset of observations to use for training
        outcomeFree <- task$outcomeFree(stackedData, time-1)
        competingRiskFree <- task$competingRiskFree(stackedData, time-1)
        observed <- task$observed(stackedData, time, TRUE)

        # Subset data
        stackedData <- stackedData[outcomeFree & competingRiskFree & observed, ]

        # Create indicators for the subset of observations to use for predictions
        outcomeFreeValidation <- task$outcomeFree(validation, time-1)
        competingRiskFreeValidation <- task$competingRiskFree(validation, time-1)
        observedValidation <- task$observed(validation, time, TRUE)
        atRisk <- outcomeFreeValidation & competingRiskFreeValidation

        # Figure out what the treatment variable is
        treatment_t <- task$currentTreatment(time)

        # Density ratio model predictions under the a-prime shift
        predictionsAPrime[atRisk & observedValidation] <-
            CrossFit(
            stackedData,
            validation[atRisk & observedValidation, ],
            "lcmmtp_stack_indicator",
            c(task$variables$history("A", time), treatment_t, task$variables$censoring),
            "binomial",
            control$learners_trt,
            control$folds_trt
        )

        # Create stacked data for training under a-star shift
        stackedData <- task$stackData(folds$training(task$data, v), folds$training(task$shiftedUnderAStar, v), time)

        # Subset data
        stackedData <- stackedData[outcomeFree & competingRiskFree & observed, ]

        # Density ratio model predictions under the a-star shift
        predictionsAStar[atRisk & observedValidation] <-
            CrossFit(
                stackedData,
                validation[atRisk & observedValidation, ],
                "lcmmtp_stack_indicator",
                c(task$variables$history("A", time), treatment_t, task$variables$censoring),
                "binomial",
                control$learners_trt,
                control$folds_trt
            )

        # Create pooled data for predicting M=m
        augmentedData <- folds$training(task$augmented, v)

        # Create indicators for the subset of observations to use for training
        outcomeFree <- task$outcomeFree(augmentedData, time-1)
        competingRiskFree <- task$competingRiskFree(augmentedData, time-1)
        observed <- task$observed(augmentedData, time, TRUE)

        # Subset data
        augmentedData <- augmentedData[outcomeFree & competingRiskFree & observed, ]

        # Create pseudo-outcome for training
        augmentedData[["lcmmtp_pseudo_m_fit"]] <- as.numeric(augmentedData[[g("lcmmtp_med_{time}")]] == augmentedData[[task$variables$mediator[time]]])

        # Estimate the probability M=m
        predictionsMediator[atRisk & observedValidation] <-
            CrossFit(
            augmentedData,
            validation[atRisk & observedValidation, ],
            "lcmmtp_pseudo_m_fit",
            c(task$variables$history("M", time), g("lcmmtp_med_{time}")),
            "binomial",
            control$learners_mediator,
            control$folds_mediator
        )

        # Create index for observation that were observed at the current time
        observedValidation <- task$observed(validation, time)

        # Create density ratios
        validation[[g("lcmmtp_Gp_A{time}")]] <- density_ratios(predictionsAPrime, atRisk, observedValidation)
        validation[[g("lcmmtp_Gs_A{time}")]] <- density_ratios(predictionsAStar, atRisk, observedValidation)
        validation[[g("lcmmtp_G_M{time}")]] <-
            G(validation[[task$variables$mediator[time]]], predictionsMediator, validation[[g("lcmmtp_med_{time}")]], atRisk, observedValidation)

        predictions[[v]] <- validation
    }

    predictions <- Reduce(rbind, predictions)
    data.table::setorder(predictions, "lcmmtp_ID")
    task$augmented <- predictions

    return(invisible())
}
