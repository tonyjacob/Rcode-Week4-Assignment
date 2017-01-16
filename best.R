best <- function(state, outcome) {
        ## Read outcome data
        oData <- read.csv("outcome-of-care-measures.csv")
        
        ## Check that state and outcome are valid
        # Assign column value to outcome based on passed in argument after converting
        # outcome to lower case to catch and manage user entry error
        outcome <- tolower(outcome)
        
        if(outcome == "heart attack") outcomeCol <- 11
        else if(outcome == "heart failure") outcomeCol <- 17
        else if(outcome == "pneumonia") outcomeCol <- 23
        else stop("invalid outcome")
        
        # Convert state to upper case to catch user entry error
        state <- toupper(state)
        
        # Create unsorted subset by matching state and outcome, and remove rows with value 
        # = Not Available and NA. Subset contains hospital name, state and mortality value
        outcome_subset <-subset(oData, oData[,7] == state & oData[, outcomeCol] != 
                                        "Not Available", select = c(2,7,outcomeCol))
        outcome_subset <- na.omit(outcome_subset)
        
        # Convert mortlity values(outcome_subset[,3]) to numeric from integer, this is necessary 
        # to make the order() function works correctly in next step
        outcome_subset[,3] <- as.numeric(as.character(outcome_subset[,3]))
        
        ## Return hospital name in that state with lowest 30-day death rate
        # using order() function to order outcomes and hospital names in ascending order
        sortOutcome <- outcome_subset[order(outcome_subset[,3], outcome_subset[,1]),]
        return(sortOutcome[1,1])
}