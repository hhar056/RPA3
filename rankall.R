rankall<-function(outcome,num="best"){
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ##Check that the state and outcome are valid
    validOutcome <- outcome %in% c("heart attack","heart failure", "pneumonia")
    
    if(!validOutcome){
        stop("invalid outcome") 
    }
    
    ## For each state, find the hospital of the given rank
    
    if(outcome=="heart attack"){
        outcome<-11
    }
    if(outcome=="heart failure"){
        outcome<-17
    }
    if(outcome=="pneumonia"){
        outcome<-23
    }
    outcomeData[,outcome]<-as.numeric(outcomeData[,outcome])
    outcomeData<-outcomeData[!is.na(outcomeData[,outcome]),]
    
    
    sortedData<-outcomeData[order(outcomeData[,7],outcomeData[,outcome],outcomeData[,2]),]
    
    sortedData<-sortedData[,c(7,outcome,2)]
    
    colnames(sortedData) <- paste(c("State","Outcome","Hospital"), sep="")
    
    
    splitData<- split(sortedData, sortedData$State)
    
    myOutput<-lapply(splitData,rankingFun,num)
    
    ## Return a data frame with the hospital names and the abb
    ## reviated state name
    myDf<-data.frame(hospital=unlist(myOutput),state=names(myOutput),row.names=names(myOutput))
    
    
    
    
    
}


rankingFun<-function(simple,num){
    if(num=="best"){
        output<-simple[1,3]
    }else if(num == "worst"){
        output<-simple[length(simple[,3]),3]
    }else if(num>length(simple[,3])){
        output<-NA
    }else{
        output<-simple[num,3]
    }
    output
    
}
