# Takes an outcome name and a hospital ranking
# Returns dataframe containing hospital in each state with the specified ranking

# Permissible outcome names: "heart attack", "heart failure", "pneumonia"
# Permissible hospital rankings: an integer, "best", "worst" 
### Lower rankings indicate better hospitals

# Hospitals without a given outcome are excluded
# Ties are broken alphabetically by hospital name

rankall <- function(outcome, num = "best"){
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that outcome is valid
    outcomeList <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    if(is.na(match(outcome, labels(outcomeList)))){
        stop("invalid outcome")
    }
    
    ## For each state, find the hospital of the given rank
    outcomeByState <- split(data, data$State)
    orderByOutcome <- function(x){order(as.numeric(x[,outcomeList[outcome]]),x[,2], na.last = NA)}
        # Anonymous function to maintain reasonable column width
    hospitalRankings <- suppressWarnings(lapply(outcomeByState, orderByOutcome))
        # executes "order" on split data (tiebreaker = hospital name)--creates ordering of indices by state

    if(num == "best"){num <- 1}
    if(num == "worst"){
        hospitalIndex <- mapply(function(x,y) x[y], hospitalRankings, lapply(hospitalRankings, length))
            # uses length of list to find index
    } else {
        hospitalIndex <- lapply(hospitalRankings, function(x) x[num])
            # finds correct index for requested rank
    }
    
    hospital <- mapply(function(x,y) x[y,2], outcomeByState, hospitalIndex)
        # calculates list of hospitals matching requested rank
    
    ## Return a dataframe with the hospital names and the state name
    state <- labels(hospital)
    data.frame(cbind(hospital, state))
}