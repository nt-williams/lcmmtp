EstimationTask <- R6::R6Class(
    "EstimationTask",
    cloneable = FALSE,
    public = list(
        data = NULL,
        augmented = NULL,
        shiftedUnderAPrime = NULL,
        shiftedUnderAStar = NULL,
        variables = NULL,
        n = NULL,
        outcomeType = NULL,
        initialize = function(data, variables, id = NULL, d_prime, d_star) {
            dataCopy <- data.table::copy(data)

            # If a survival outcome, flip indicators
            if (!is.null(variables$risk)) {
                for (y in c(variables$risk, variables$outcome)) {
                    data.table::set(dataCopy, j = y, value = private$flipSurvivalIndicators(dataCopy[[y]]))
                }
            }

            # Convert to a data.table
            self$data <- data.table::as.data.table(dataCopy[, variables$allVariables()])

            if (!is.null(id)) {
                self$data[["lcmmtp_ID"]] <- id
            } else {
                self$data[["lcmmtp_ID"]] <- seq.int(nrow(self$data))
            }

            self$data[["lcmmtp_row_index"]] <- seq.int(nrow(self$data))

            self$augmented <- self$data
            self$variables <- variables$clone()
            self$n <- nrow(self$data)
            self$outcomeType <- private$getOutcomeType()

            # Create alternative data-sets with treatment modified according to shift functions
            self$shiftedUnderAPrime <- self$shiftTreatment(self$data, self$variables$treatment, self$variables$censoring, d_prime)
            self$shiftedUnderAStar <- self$shiftTreatment(self$data, self$variables$treatment, self$variables$censoring, d_star)
        },

        # Create augmented data for pooled regressions
        augment = function(data, time) {
            uniqueMediatorValues <- self$uniqueMediatorValues()
            k <- length(uniqueMediatorValues)

            m_underbar <- data.table::as.data.table(
                lapply(expand.grid(uniqueMediatorValues), rep, nrow(data))
            )

            names(m_underbar) <- g("lcmmtp_med_{time}")
            augmented <- data.table::as.data.table(lapply(data, rep, rep(k, nrow(data))))
            cbind(augmented, m_underbar)
        },

        # All unique values of the mediator
        uniqueMediatorValues = function() {
            M <- self$data[, self$variables$mediator, drop = FALSE]
            unique(as.vector(as.matrix(M[complete.cases(M), ])))
        },

        # Create augmented stacked data for density ratio estimation
        stackData = function(data, shifted, time) {
            dataCopy <- data

            if (length(self$variables$treatment) > 1 || time == 1) {
                dataCopy[[self$variables$treatment[time]]] <- shifted[[self$variables$treatment[time]]]
            }

            if (!is.null(self$variables$censoring)) {
                dataCopy[[self$variables$censoring[time]]] <- shifted[[self$variables$censoring[time]]]
            }

            stacked <- rbind(data, dataCopy)
            stacked[["lcmmtp_stack_indicator"]] <- rep(c(0, 1), each = nrow(data))
            stacked
        },

        shiftTreatment = function(data, treatment, censoring, .f) {
            for (a in treatment) {
                data[[a]] <- .f(data, a)
            }

            if (is.null(censoring)) {
                return(data)
            }

            for (cs in censoring) {
                data[[censoring]] <- 1
            }

            data
        },

        outcomeFree = function(data, time) {
            # If not a survival outcome, return TRUE for all observations
            if (is.null(self$variables$risk)) {
                return(rep(TRUE, nrow(data)))
            }

            # If time is 0, return TRUE for all observations
            if (time == 0) {
                return(rep(TRUE, nrow(data)))
            }

            data[[self$variables$risk[time]]] == 1 & !is.na(data[[self$variables$risk[time]]])
        },

        competingRiskFree = function(data, time) {
            # If time is less than or equal to 0, return TRUE for all observations
            if (time <= 0) {
                return(rep(TRUE, nrow(data)))
            }

            if (is.null(self$variables$competingRisks)) {
                competingRisk <- rep(0, nrow(data))
            } else {
                competingRisk <- data[[self$variables$competingRisks[time]]]
            }

            competingRisk == 0
        },

        atRisk = function(data, time) {
            self$outcomeFree(data, time) & self$competingRiskFree(data, time)
        },

        observed = function(data, time, lag = FALSE) {
            if (is.null(self$variables$censoring)) {
                return(rep(TRUE, nrow(data)))
            }

            if (!lag) {
                return(data[[self$variables$censoring[time]]] == 1)
            }

            if (lag && time > 1) {
                return(data[[self$variables$censoring[time - 1]]] == 1)
            }

            rep(TRUE, nrow(data))
        }
    ),
    private = list(
        # Derive outcome type, i.e., binary or continuous
        getOutcomeType = function() {
            y <- self$data[[self$variables$outcome]]
            assertNumeric(y)
            if (all(y == 1 | y == 0, na.rm = T)) return("binomial")
            "continuous"
        },

        flipSurvivalIndicators = function(x) {
            data.table::fcase(x == 0, 1, x == 1, 0)
        }
    )
)
