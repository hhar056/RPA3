

source("best.R")
source("rankhospital.R")
source("rankall.R")

best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)


best("TX","heart attack")
best("TX","heart failure")
best("MD","heart attack")
best("MD","pneumonia")
best("BB","heart attack")
best("NY","hert attack")



rankhospital("TX", "heart failure", 4)
rankhospital("MD","heart attack", "worst")
rankhospital("MN","heart attack",5000)

head(rankall("heart attack",20),10)
tail(rankall("pneumonia","worst"),3)
tail(rankall("heart failure"),10)

