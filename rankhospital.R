rankhospital <- function(state, outcome, num = "best") {
        
        ## Read outcome data and create list of states to check for state existence
        # in next block of code
        
        oData <- read.csv("outcome-of-care-measures.csv")
        stateList <- as.character(unique(oData[,7]))
        
        ## Check if state is valid
        # Convert state to upper case to catch and manage user entry error
        # Check if state value exists in list of states
        
        state <- toupper(state)
        
        if(state %in% stateList == FALSE) stop("invalid state")
        
        ## Check if outcome are valid
        # Assign column value to outcome based on passed in argument after converting
        # outcome to lower case to catch and manage user entry error
        
        outcome <- tolower(outcome)
        
        if(outcome == "heart attack") outcomeCol <- 11
        else if(outcome == "heart failure") outcomeCol <- 17
        else if(outcome == "pneumonia") outcomeCol <- 23
        else stop("invalid outcome")
        
        # Create unsorted subset by matching state and outcome, and remove rows with value 
        # = Not Available and NA. Subset contains hospital name, state and mortality value
        
        outcome_subset <-subset(oData, oData[,7] == state & oData[, outcomeCol] != 
                                        "Not Available", select = c(2,7,outcomeCol))
        outcome_subset <- na.omit(outcome_subset)
        
        # Convert mortlity values(outcome_subset[,3]) to numeric from integer, this is necessary 
        # to make the order() function work correctly in next step
        
        outcome_subset[,3] <- as.numeric(as.character(outcome_subset[,3]))
        
        ## Return hospital name in that state with lowest 30-day death rate
        # using order() function to order outcomes in ascending order and then order hospital 
        # names in ascending order
        
        sortOutcome <- outcome_subset[order(outcome_subset[,3], outcome_subset[,1]),]
        
        # Since num argument can have a character or numeric value type, the code below
        # handles them separately and returns hospital name for the value of num
        
        if(is.character(num) == TRUE){
                if(num == "best") return(sortOutcome[1,1])
                else if(num == "worst") return(sortOutcome[nrow(sortOutcome),1])
        }
        else{
                if(num > nrow(sortOutcome)) return(NA)
                else return(sortOutcome[num,1])
        }
}
