best <- function(state, outcome) {
  # Read outcome data
  data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  cau<-0
  if(outcome=="heart attack")
    cau<-11
  if(outcome=="heart failure")
    cau<-17
  if(outcome=="pneumonia")
    cau<-23
  
  ## Check that state and outcome are valid
  if(sum(unique(data[,7]==state))==0)
    stop("invalid state")
  if(cau==0)
    stop("invalid outcome")
  
  ## Return hospital name in that state with lowest 30-day death
  data[,cau]= as.numeric(data[, cau])
  sub<-subset(data,data[,7]==state)
  mi<-min(sub[,cau],na.rm=TRUE)
  ans<-sort(subset(sub[,2],sub[,cau]==mi))
  ans[1]
}