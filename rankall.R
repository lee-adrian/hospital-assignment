rankall <- function(outcome, num= "best") {
  ## read outcome data
  outcometable <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop("invalid outcome")
  }
  ## filter for desired outcome and state
  
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
  outcometable[ ,outcomeclm] <- suppressWarnings(as.numeric(outcometable[, outcomeclm]))
  ## remove any NAs
  outcometable <- outcometable[!is.na(outcometable[, outcomeclm]),]
  ## split state
  splitoutcome <- split(outcometable, outcometable$State)
  ## loop ordered list by specific outcome and hospital name
           sln <- lapply(splitoutcome, function(y, num) {
             y <- y[order(y[ , outcomeclm], y$Hospital.Name), ]
                 if(class(num) == "character") {
                   if(num == "worst") {
                     return (y$Hospital.Name[nrow(y)])
                   }
                   else if(num == "best") {
                     return (y$Hospital.Name[1])
                   }
                 }
                 else {
                   return (y$Hospital.Name[num])
                 }
               }, num)
           return ( data.frame(hospital=unlist(sln), state=names(sln)) )
}