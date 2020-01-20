## rankhospital ranks hospitals in a state by a specific outcome
rankhospital <- function(state, outcome, num="best") {
  ## read outcome data
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
  ## select rows with outcome and suppress warnings
  stateoutcome[ ,outcomeclm] <- suppressWarnings(as.numeric(stateoutcome[, outcomeclm]))
  ## remove any NAs
  cleanoutcome <- stateoutcome[(!is.na(stateoutcome[, outcomeclm])),]
  ## rank the rows
  rankoutcome  <- cleanoutcome[order(cleanoutcome[, outcomeclm], cleanoutcome[,2]),]
  ## if given best or worst
  if (num == "best") {
    num <- 1
  }
  if (num == "worst") {
    num <- nrow(rankoutcome)
  }
  return(rankoutcome[num, 2])
}
  