rankall <- function(outcome, num = "best") {
        
        # Load dplyr library to use group_by and mutate functions
        library(dplyr)
        
        ## Read outcome data
        oData <- read.csv("outcome-of-care-measures.csv")
        
        ## Check if outcome are valid
        # Assign column value to outcome based on passed in argument after converting
        # outcome to lower case to catch and manage user entry error
        
        outcome <- tolower(outcome)
        
        if(outcome == "heart attack") outcomeCol <- 11
        else if(outcome == "heart failure") outcomeCol <- 17
        else if(outcome == "pneumonia") outcomeCol <- 23
        else stop("invalid outcome")
        
        # Subset oData into new data frame based on outcome type and remove rows with outcome = "Not Applicable" or NA
        outcome_subset <- subset(oData, oData[, outcomeCol] != "Not Available", select = c(2,7, outcomeCol))
        outcome_subset <- na.omit(outcome_subset)
        
        # Rename outcomeCol to "outcome" for easier calls later on in program
        colnames(outcome_subset)[colnames(outcome_subset) == colnames(outcome_subset[3])] <- 'outcome'
        
        # Using the dplyr library's function called group_by to group outcome_subset data frame based on state column values
        grouped_subset <- group_by(outcome_subset, State)
        
        # The groped_subset data frame is piped into (%>%) the mutate function
        # Using the mutate function in dplyr to create new column and assigning ranks to them using row_number function
        ranked_susbet <- grouped_subset %>% mutate(rank = row_number(outcome))
        
        # The ranked_subset is then ordered using the order() function based on State and outcome values
        # rankedNordered data frame consists of a list of ranked hospitals in every state for a specific outcome value
        rankedNordered <- ranked_susbet[order(ranked_susbet$State, ranked_susbet$outcome, ranked_susbet$Hospital.Name),]
        
        if(is.character(num) == TRUE){
                
                if(num == "best"){
                        return(subset(rankedNordered, rankedNordered$rank == 1, select = c(1,2,4)))
                }
        }
        
        else{
                return(subset(rankedNordered, rankedNordered$rank == num, select = c(1,2,4)))
        }
        
}
                # results <- data.frame()
                # else if(num == "worst"){
                #         
                #         stateCheck <- NULL # used to check if there has been a change in state while looping through rankedNordered list
                #         maxRank <- NULL
                #         worstRow <- NULL
                #         
                #         for(i in nrow(rankedNordered)){
                #                 
                #                 # Assign stateCheck a value of state in first row
                #                 if(is.null(stateCheck) == TRUE){
                #                         
                #                         stateCheck <- rankedNordered[i,2]
                #                 }
                #                 
                #                 # Record the row number where the rank is maximum
                #                 else if(rankedNordered[i,2] == stateCheck){
                #                         
                #                         if(rankedNordered[i,4] > maxRank){
                #                                 maxRank <- rankedNordered[i,4]
                #                                 worstRow <- i
                #                         }
                #                 }
                #                 
                #                 # Create list called "results" that contain name of hospital and state where rank is max for that state
                #                 else if(rankedNordered[i,2] != stateCheck){
                #                         
                #                         results <- rbind(rankedNordered[worstRow, 1], rankedNordered[worstRow, 2])
                #                         maxRank <- NULL
                #                         stateCheck <- rankedNordered[i,2]
                #                 }
                #         }
                #         
                #         colnames(results) <- c("Hospital Name", "State")
                #         return(results)
                # }  
