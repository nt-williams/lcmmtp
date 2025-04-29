# R6 class for the necessary variables
Variables <- R6::R6Class(
    "Variables",
    public = list(
        baselineConfounders = NULL,
        timeVaryConfounders = NULL,
        treatment = NULL,
        mediator = NULL,
        mediatorOutcomeConfounders = NULL,
        outcome = NULL,
        competingRisks = NULL,
        risk = NULL,
        censoring = NULL,
        timeHorizon = NULL,
        initialize = function(baselineConfounders, timeVaryConfounders, treatment,
                              mediatorOutcomeConfounders, mediator, outcome, censoring, competingRisks) {
            assertCharacter(treatment)
            assertCharacter(outcome)

            self$treatment <- treatment
            self$outcome <- outcome[length(outcome)]

            # If the outcome is a vector, assign all but the last outcome to risk
            if (length(outcome) > 1) {
                self$risk <- outcome[1:(length(outcome) - 1)]
            }

            self$timeHorizon <- private$findTimeHorizon()

            assertCharacter(baselineConfounders, null.ok = TRUE)
            assertCharacter(censoring, len = self$timeHorizon, null.ok = TRUE)
            assertCharacter(competingRisks, len = self$timeHorizon, null.ok = TRUE)
            assertCharacter(mediator, len = self$timeHorizon)
            assertList(timeVaryConfounders, types = "character", len = self$timeHorizon)
            assertList(mediatorOutcomeConfounders, types = c("character", "null"), len = self$timeHorizon)

            self$baselineConfounders <- baselineConfounders
            self$censoring <- censoring
            self$timeVaryConfounders <- timeVaryConfounders
            self$mediatorOutcomeConfounders <- mediatorOutcomeConfounders
            self$mediator <- mediator
            self$competingRisks <- competingRisks

            invisible(self)
        },

        # Get all parent nodes for a variable
        history = function(var = c("L", "A", "Z", "M", "Y"), time) {
            switch(
                match.arg(var),
                L = private$parentsTimeVary(time),
                A = private$parentsTreatment(time),
                Z = private$parentsMediatorOutcomeConfounders(time),
                M = private$parentsMediator(time),
                Y = private$parentsOutcome()
            )
        },

        # Return the names of all variables
        allVariables = function() {
            c(self$baselineConfounders, unlist(self$timeVaryConfounders), self$treatment, unlist(self$mediatorOutcomeConfounders), self$mediator, self$risk, self$censoring, self$competingRisks, self$outcome)
        }
    ),
    private = list(
        parentsTimeVary = function(time) {
            if (time <= 1) {
                return(self$baselineConfounders)
            }
            c(private$parentsMediator(time - 1), self$mediator[time - 1])
        },

        parentsTreatment = function(time) {
            if (time >= 1) {
                return(c(private$parentsTimeVary(time), unlist(self$timeVaryConfounders[[time]])))
            }
            private$parentsTimeVary(time)
        },

        parentsMediatorOutcomeConfounders = function(time) {
            c(private$parentsTreatment(time), self$treatment[time])
        },

        parentsMediator = function(time) {
            c(private$parentsMediatorOutcomeConfounders(time), unlist(self$mediatorOutcomeConfounders[[time]]))
        },

        parentsOutcome = function() {
            c(self$baselineConfounders, unlist(self$timeVaryConfounders), self$treatment, unlist(self$mediatorOutcomeConfounders), self$mediator)
        },

        # Figure out time horizon based on treatment/outcome vector
        findTimeHorizon = function() {
            if (is.null(self$risk)) {
                return(length(self$treatment))
            }
            length(self$risk) + 1
        }
    )
)
