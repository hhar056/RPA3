


## Read the outcome

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
test<-outcome[,7]==rep("TX",length(outcome[,7]))
head(test)

## Simple histogram

outcome[, 11] <-as.numeric(outcome[,11])
hist(outcome[,11])

best("TX","heart attack")
best("TX","heart failure")
best("MD","heart attack")
best("MD","pneumonia")
best("BB","heart attack")
best("NY","hert attack")


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
    print(paste("min outcome: ",min(stateData[,outcome])))
    resultIndex<-which(stateData[,outcome]==min(stateData[,outcome]))

    ##print(paste("outcome: ",outcome,"result index:",resultIndex))
    output<-stateData[resultIndex,2]
    ##if(length(output)>1){
    output<-sort(output)
    ##}
    output
    
}

