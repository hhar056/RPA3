rankhospital<- function(state,outcome, num="best"){
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
    
    ## Return hospital name in that state with the given rank
    ## 30 day death rate
    
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
    
    
    sortedData<-stateData[order(stateData[,outcome],stateData[,2]),]
    
    simple<-cbind(sortedData[,c(2,outcome)],1:length(sortedData[,1]))
    colnames(simple) <- paste(c("Hospital.Name","Rate","Rank"), sep="")
    head(simple)
    if(num=="best"){
        output<-simple[1,1]
    }else if(num == "worst"){
        output<-simple[length(simple[,3]),1]
    }else if(num>length(simple[,3])){
        output<-NA
    }else{
        output<-simple[num,1]
    }
    output
}