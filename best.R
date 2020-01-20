## This function reads the outcome-of-care-measures.csv and returns a character vector with
## the hosptital name that has the lowest 30-day mortality rate for a particular outcome.

best <- function (state, outcome) {
  ## 1. read outcome data
  ## 2. check state and outcome are valid
  ## 3. return hospital name in that state with lowest 30 day death rate
  outcometable <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if (!any(state == outcometable$State)){
    stop("invalid state")
    }
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop("invalid outcome")
    }
  ## filter for desired outcome and state
  stateoutcome <- subset(outcometable, State == state)
  if (outcome == "heart attack") {
    outcomeclm <- 11
  }
  else if (outcome == "heart failure") {
      outcomeclm <- 17
  }
  else if (outcome == "pneumonia") {
    outcomeclm <- 23
  }
  ## select best row, supress warnings
  bestrow <- suppressWarnings(which(as.numeric(stateoutcome[, outcomeclm]) == 
                  min(as.numeric(stateoutcome[, outcomeclm]), na.rm = TRUE)))
  statehospitals <- stateoutcome[bestrow,2]
  minhospital <- min(statehospitals)
  return(minhospital)
}
