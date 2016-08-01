best <- function(state, outcome){
        best <- match.fun(best)
        outcomecare <- read.csv("databases/outcome-of-care-measures.csv", colClasses = c("NULL", "character", "NULL", "NULL", "NULL", "NULL", "character", "NULL", "NULL", "NULL", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "character","NULL", "NULL", "NULL", "NULL", "NULL", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL"), na.string = "Not Available", nrows = 4707, comment.char = "")
       
        outcomecare <- outcomecare[outcomecare$State == state,]
        if (length(row.names(outcomecare)) != 0){
                
                          if (outcome == "heart failure"){
                                  ordercare <- outcomecare[order(as.numeric(outcomecare$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), outcomecare$Hospital.Name, decreasing = FALSE, na.last = NA),]
                                  print(ordercare[1,1])
                          }else if (outcome == "heart attack"){
                                 
                                  ordercare <- outcomecare[order(as.numeric(outcomecare$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), outcomecare$Hospital.Name, decreasing = FALSE, , na.last = NA),]
                                  print(ordercare[1,1])
                          }else if (outcome == "pneumonia"){
                                  ordercare <- outcomecare[order(as.numeric(outcomecare$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), outcomecare$Hospital.Name, decreasing = FALSE, , na.last = NA),]
                                  print(ordercare[1,1])
                                  
                          }else{
                                  stop("invalid outcome")
                                  
                          }
        }else{
                stop("invalid state")
                
        }
}

