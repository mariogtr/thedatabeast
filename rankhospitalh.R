rankhospital <- function(state, outcome, num){
        rankhospital <- match.fun(rankhospital)
        outcomecare <- read.csv("databases/outcome-of-care-measures.csv", colClasses = c("NULL", "character", "NULL", "NULL", "NULL", "NULL", "character", "NULL", "NULL", "NULL", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "character","NULL", "NULL", "NULL", "NULL", "NULL", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL"), na.string = "Not Available", nrows = 4707, comment.char = "")
        outcomecare <- outcomecare[outcomecare$State == state,]
        
        if (outcome == "heart failure"){
                ordercare <- outcomecare[order(as.numeric(outcomecare$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), outcomecare$Hospital.Name, decreasing = FALSE, na.last = NA),]
                
                
        }else if (outcome == "heart attack"){
                
                ordercare <- outcomecare[order(as.numeric(outcomecare$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), outcomecare$Hospital.Name, decreasing = FALSE, na.last = NA),]
                
        }else if (outcome == "pneumonia"){
                ordercare <- outcomecare[order(as.numeric(outcomecare$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), outcomecare$Hospital.Name, decreasing = FALSE, na.last = NA),]
                
                
        }else{                        
                stop("invalid outcome")
                
        }
        if(num == "best"){
                print(ordercare[1,1])
        }else if(num == "worst"){
                print(ordercare[length(row.names(ordercare)),1])
        }else if(num > 0 && num <= length(row.names(ordercare))){
                print(ordercare[num,1])
        }else{
                print("NA")
        }
        
}