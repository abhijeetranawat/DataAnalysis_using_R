complete<-function(directory,idt=1:332){
  a=NULL
  id1<-sprintf("%03d",idt)
  for(i in 1:length(id1)){
    data<-read.csv(paste(id1[i],".csv",sep=""))
    cc=complete.cases(data)
    b=data.frame(id=idt[i],nobs=length(cc[cc==TRUE]))
    a=rbind(a,b)
    
  }
  print(a)
}