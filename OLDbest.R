best <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        validState <- as.factor(data$State)
        if(!(state %in% validState)) {
                stop("invalid state")
        }
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        x <- state
        x
}