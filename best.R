# 2 Finding the best hospital in a state
# Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
# outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
# in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
# be one of "heart attack", "heart failure", or "pneumonia". Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.
# Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
# be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals "b", "c",
#                                                                                      and "f" are tied for best, then hospital "b" should be returned).
# The function should use the following template.
# best <- function(state, outcome) {
#         ## Read outcome data
#         ## Check that state and outcome are valid
#         ## Return hospital name in that state with lowest 30-day death
#         ## rate
# }
# The function should check the validity of its arguments. If an invalid state value is passed to best, the
# function should throw an error via the stop function with the exact message "invalid state". If an invalid
# outcome value is passed to best, the function should throw an error via the stop function with the exact
# message "invalid outcome".
# Here is some sample output from the function.
# > source("best.R")
# > best("TX", "heart attack")
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
# > best("TX", "heart failure")
# [1] "FORT DUNCAN MEDICAL CENTER"
# > best("MD", "heart attack")
# [1] "JOHNS HOPKINS HOSPITAL, THE"
# > best("MD", "pneumonia")
# [1] "GREATER BALTIMORE MEDICAL CENTER"
# > best("BB", "heart attack")
# Error in best("BB", "heart attack") : invalid state
# > best("NY", "hert attack")
# Error in best("NY", "hert attack") : invalid outcome
# >
#         2
# Save your code for this function to a file named best.R.

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