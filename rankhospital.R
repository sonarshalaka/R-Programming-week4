# rankhospital <- function(state, outcome, num = "best") {
#   ## Read outcome data
#   data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
#   ## Check that state and outcome are valid
#   states <- data[,7]
#   outcomes <- c("heart attack","heart failure","pneumonia")
#   if((state %in% states)==FALSE) {
#     stop(print("invalid state"))
#   }
#   else if ((outcome %in% outcomes)==FALSE) {
#     stop(princomp("invalid outcome"))
#   }
#   
#   #get subset of data with desired state
#   new_data <- subset(data,State = state)
#   
#   #get column number from data file
#   if(outcome == "heart attack") {
#     outcome_column <- 11
#   }
#   else if (outcome == "heart failure") {
#     outcome_column <- 17
#   }
#   else {
#     outcome_column <- 23
#   }
#   
#   #if num > number of hospital return NA
#   if(is.numeric(num) == TRUE) {
#     if(length(data[,2]) < num) {
#       return(NA)
#     }
#   }
#   
#   #get rid of NA in desired column
#   new_data[,outcome_column] <- as.numeric(new_data[,outcome_column])
#   bad <- is.na(new_data[,outcome_column])
#   desired_data <- new_data[!bad,]
#   
#   #arrange the modified dataframe in ascending order of the outcome values
#   outcome_column_name <- names(desired_data)[outcome_column]
#   hostipal_column_name <- names(desired_data)[2]
#   index <- with(desired_data,order(desired_data[outcome_column_name],desired_data[hostipal_column_name]))
#   ordered_desired_data <- desired_data[index,]
#   
#   #if num is given as "best" or "worst"
#   if(is.character(num) == TRUE) {
#     if (num == "best") {
#       num = 1
#     }
#     else if (num == "worst") {
#       num = length(ordered_desired_data[,outcome_column])
#     }
#   }
#   
#   ## Return hospital name in that state with the given rank
#   ## 30-day death rate
#   ordered_desired_data[num,2]
# }

rankhospital <- function(state, outcome, rank = "best"){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  fd   <- as.data.frame(cbind(data[, 2],  # hospital
                              data[, 7],  # state
                              data[, 11],  # heart attack
                              data[, 17],  # heart failure
                              data[, 23]), # pneumonia
                        stringsAsFactors = FALSE)
  colnames(fd) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if (!state %in% fd[, "state"]) {
    stop('invalid state')
  } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else if (is.numeric(rank)) {
    si <- which(fd[, "state"] == state)
    ts <- fd[si, ]                     # extracting dataframe for the called state
    ts[, eval(outcome)] <- as.numeric(ts[, eval(outcome)])
    ts <- ts[order(ts[, eval(outcome)], ts[, "hospital"]), ]
    output <- ts[, "hospital"][rank]
  } else if (!is.numeric(rank)){
    if (rank == "best") {
      output <- best(state, outcome)
    } else if (rank == "worst") {
      si <- which(fd[, "state"] == state)
      ts <- fd[si, ]    
      ts[, eval(outcome)] <- as.numeric(ts[, eval(outcome)])
      ts <- ts[order(ts[, eval(outcome)], ts[, "hospital"], decreasing = TRUE), ]
      output <- ts[, "hospital"][1]
    } else {
      stop('invalid rank')
    }
  }
  return(output)
}
