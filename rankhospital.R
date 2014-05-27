rankhospital <- function(state, outcome, num = "best") {
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
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        dataIndex <- dataColumns[outcomeIndex]
        dataSubset <- data[which(data$State == state), c(hospitalNameIndex, dataIndex)]
        dataSubset[, 2] <- suppressWarnings(as.numeric(dataSubset[, 2]))
        dataSubsetNoNA <- dataSubset[which(is.na(dataSubset[, 2]) == FALSE),]
#         hospitalCount <- nrow(dataSubsetNoNA)
        dataSubsetNoNAOrdered <- dataSubsetNoNA[order(dataSubsetNoNA[,2], dataSubsetNoNA[,1]),]
        if(num == "best") {
                rowIndex <- 1
        } else if(num == "worst") {
                rowIndex <- nrow(dataSubsetNoNAOrdered)
        } else {
                rowIndex <- num
        }
        dataSubsetNoNAOrdered[rowIndex, 1]
}