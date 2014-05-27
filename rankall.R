rankall <- function(outcome, num = "best") {
        ## Set some values to be used below
        hospitalNameIndex <- 2
        stateIndex <- 7
        validOutcome <- c("heart attack", "heart failure", "pneumonia")
        dataColumns <- c(11, 17, 23)
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that outcome is valid
        outcomeIndex <- match(outcome, validOutcome)
        if(is.na(outcomeIndex)) {
                stop("invalid outcome")
        }
        
        ## Return a data frame with the hospital names and the 
        ## (abbreviated) state names
        dataIndex <- dataColumns[outcomeIndex]
        dataSubset <- data[, c(stateIndex, hospitalNameIndex, dataIndex)]
        dataSubset[, 3] <- suppressWarnings(as.numeric(dataSubset[, 3]))
        dataSubset[, 1] <- suppressWarnings(as.factor(dataSubset[, 1]))        
        dataSubsetNoNA <- dataSubset[which(is.na(dataSubset[, 3]) == FALSE),]
        dataSubsetNoNAOrdered <- dataSubsetNoNA[order(dataSubsetNoNA[, 1], dataSubsetNoNA[, 3], dataSubsetNoNA[, 2]),]
        splitData <- split(dataSubsetNoNAOrdered[, 2:3], dataSubsetNoNAOrdered[, 1], )
        states <- names(splitData)
        hospitals <- character()
        for(state in states) {
                tempData <- splitData[state][[1]]
                if(num == "best") {
                        rowIndex <- 1
                } else if(num == "worst") {
                        rowIndex <- nrow(tempData)
                } else {
                        rowIndex <- num
                }
                hospital <- tempData[rowIndex, 1]
                hospitals <- rbind(hospitals, hospital)
        }
        result <- cbind.data.frame(hospitals, states, row.names = states)
        colnames(result) <- c("hospital", "state")
        result
}