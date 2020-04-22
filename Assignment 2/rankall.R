rankhospital <- function(state, outcome, num = "best"){
  ## Read outcome data
  data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
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
  
  ## Return hospital name in that state with the given rank
  data[,cau]= as.numeric(data[, cau])
  sub<-subset(data,data[,7]==state)
  sub<-sub[order(sub[,cau],sub[,2]),c(2,7,cau)]
  sub<-sub[complete.cases(sub),]
  if(num=="best")
    mi<-1
  else if(num=="worst")
    mi<-length(sub[,3])
  else
    mi<-num
  sub[mi,1]
  
}
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  stt<-sort(unique(data[,7]))
  df<-data.frame(hospitals=character(),state=character())
  for(val in stt)
  {
    cb<-data.frame(t(c(rankhospital(val,outcome,num),val)))
    colnames(cb)<-c("hospital","state")
    rownames(cb)<-val
    df<-rbind(df,cb)
  }
  df
  
  
}