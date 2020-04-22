corr<-function(directory,threshold=0){
  a=NULL
  id1<-sprintf("%03d",1:332)
  for(i in 1:332){
    data<-read.csv(paste(id1[i],".csv",sep=""))
    ab<-complete.cases(data)
    if((length(ab[ab==TRUE]))>threshold){
      data<-data[complete.cases(data), ]
      
      p=data$sulfate
      q=data$nitrate
      ss<-cor(p,q)
      a=c(a,ss)
    }
    
  }
  a
}