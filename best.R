best <- function(state,outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  
  ## Check that state and outcome are valid
  states <- data[,7]
  outcomes <- c("heart attack","heart failure","pneumonia")
  if((state %in% states)==FALSE) {
    stop(print("invalid state"))
  }
  else if ((outcome %in% outcomes)==FALSE) {
    stop(princomp("invalid outcome"))
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  #1. Get subset of data of state
  new_data <- subset(data,State == state)
  #2. Get desired column from data file
  if(outcome == "heart attack") {
    outcome_column <- 11
  }
  else if(outcome == "heart failure") {
    outcome_column <- 17
  }
  else {
    outcome_column <- 23
  }
  
  #3. Get rid of NA
  required_columns <- as.numeric(new_data[,outcome_column])
  bad <- is.na(required_columns)
  desired_data <- new_data[!bad,]
  
  #4. find hospitals with minimum outcome values
  columns_considered <- as.numeric(desired_data[,outcome_column])
  desired_rows <- which(columns_considered == min(columns_considered))
  desired_hospitals <- desired_data[desired_rows,2]
  
  #5.sort desired_hospitals
  if(length(desired_hospitals) > 1) {
    hospital_sorted <- sort(desired_hospitals)
    hospital_sorted[1] #return first one
  }
  else {
    desired_hospitals
  }
}