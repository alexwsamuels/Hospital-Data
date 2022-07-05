# Takes a state abbreviation (2 characters) and an outcome name
# Returns name of hospital with lowest 30-day mortality

# Permissible outcome names: "heart attack", "heart failure", "pneumonia"

# Hospitals without a given outcome are excluded
# Ties are broken alphabetically

best <- function(state, outcome){
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
    
    ## Return hospital name in state with lowest 30-day death rate
    data[,outcomeList[outcome]] <- suppressWarnings(as.numeric(data[,outcomeList[outcome]]))
    ## sets outcome to numeric
    
    minsByState <- tapply(data[,outcomeList[outcome]], data$State, function(x) min(x, na.rm=TRUE))
    ## creates list of minimums for each state
    
    names <- data[data[,outcomeList[outcome]] == minsByState[state] & data$State == state,][[2]]
    ## creates list of names of hospitals in state with minimum death rate
    
    names[order(names)[1]]
    ## returns name of hospital chosen alphabetically
}