


best <- function(state,outcome){
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ##Check that the state and outcome are valid
    validState <- state %in% outcomeData[,7]
    validOutcome <- outcome %in% c("heart attack","heart failure", "pneumonia")
    if(!validState){
        stop("invalid state")
    }
    if(!validOutcome){
       stop("invalid outcome") 
    }
    
    ##Return hospital name in that state with the lowest 30 day death rate
    stateDataBool<-outcomeData[,7]==rep(state,length(outcomeData[,7]))
    stateData<-outcomeData[stateDataBool,]
    if(outcome=="heart attack"){
        outcome<-11
    }
    if(outcome=="heart failure"){
        outcome<-17
    }
    if(outcome=="pneumonia"){
        outcome<-23
    }
    stateData[,outcome]<-as.numeric(stateData[,outcome])
    stateData<-stateData[!is.na(stateData[,outcome]),]

    resultIndex<-which(stateData[,outcome]==min(stateData[,outcome]))

    output<-sort(stateData[resultIndex,2])

    output
    
}

