# Takes a state abbreviation (2 characters), an outcome name, and a hospital ranking
# Returns name of hospital with the name of the hospital that has the given ranking

# Permissible outcome names: "heart attack", "heart failure", "pneumonia"
# Hospital ranking is the ranking of the hospital in the given state for the given outcome
### Lower rankings indicate better hospitals
### Permissible hospital rankings: an integer, "best", "worst" 

# Hospitals without a given outcome are excluded
# Ties are broken alphabetically by hospital name

rankhospital <- function(state, outcome, num = "best"){
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if(is.na(match(state, state.abb))){
        stop("invalid state")
    }
    
    outcomeList <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    if(is.na(match(outcome, labels(outcomeList)))){
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with the given rank 30-day death rate
    stateHospitals <- data[data$State == state & data[,outcomeList[outcome]] != "Not Available",]
    stateHospitals[,outcomeList[outcome]] <- as.numeric(stateHospitals[,outcomeList[outcome]])
    
    if(num == "best"){num <- 1}
    if(num == "worst"){num <- nrow(stateHospitals)}

    stateHospitals[order(stateHospitals[,outcomeList[outcome]], stateHospitals[,2])[num],2]
    
}