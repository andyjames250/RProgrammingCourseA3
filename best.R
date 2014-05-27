best <- function(state, outcome) {
        ## Set some values to be used below
        hospitalNameIndex <- 2
        validOutcome <- c("heart attack", "heart failure", "pneumonia")
        dataColumns <- c(11, 17, 23)
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        if(!(state %in% data$State)) {
                stop("invalid state")
        }

        outcomeIndex <- match(outcome, validOutcome)
        if(is.na(outcomeIndex)) {
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        dataIndex <- dataColumns[outcomeIndex]
        dataSubset <- data[which(data$State == state), c(hospitalNameIndex, dataIndex)]
        dataSubset[, 2] <- suppressWarnings(as.numeric(dataSubset[, 2]))
        dataSubset <- dataSubset[order(c(dataSubset[,2], dataSubset[,2])),]
        dataSubset[1, 1]
}