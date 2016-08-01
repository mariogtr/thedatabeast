rankall <- function(outcome, num = 1){
        
        rankall <- match.fun(rankall)
        outcomecare <- read.csv("databases/outcome-of-care-measures.csv", colClasses = c("NULL", "character", "NULL", "NULL", "NULL", "NULL", "character", "NULL", "NULL", "NULL", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "character","NULL", "NULL", "NULL", "NULL", "NULL", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL"), na.string = "Not Available", nrows = 4707, comment.char = "", stringsAsFactors=FALSE)
        
        	
 
 
        if (outcome == "heart failure"){
                colindex = 4
        }else if (outcome == "heart attack"){
                colindex = 3
        }else if (outcome == "pneumonia"){
                colindex = 5
        }else{                        
                stop("invalid outcome")
        }
        
        outcomecare <- outcomecare[, c(1,2,colindex)]
        names(outcomecare) <-c ("hospital", "state", "outcome")	
        valid <- outcomecare[!is.na(outcomecare$outcome),]	
        sorted <- valid[order(valid$state, as.numeric(valid$outcome),valid$hospital),]
        bystate<- split(sorted,sorted$state)
        rankhos <- lapply(bystate, function(x,rank){
                
                if (class(rank)== "character"){
                        
                        if(rank == "best"){
                                #rethos  <- as.list(x[[1, 2]], x[[1, 1]])
                                
                                return(x$hospital[1])
                        }else if(rank == "worst"){
                                ##num <- length(row.names(data))
                                return(x$hospital[length(row.names(x))])
                        }
                }else {
                        return(x$hospital[rank])
                }
                
        },rank= num)
        
        data.frame(hospital = unlist(rankhos), state = names(rankhos))
}

