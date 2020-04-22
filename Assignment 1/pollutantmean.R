pollutantmean<-function(directory,pollutant,id=1:332)
{
  id<-sprintf("%03d",id)
  mean1<-0
  valf<-NULL
  for(i in 1:length(id)){
    data<-read.csv(paste(id[i],".csv",sep=""))
    exp<-paste("data$",pollutant,sep="")
    val<-eval(parse(text=exp))
    val<-val[!is.na(val)]
    valf<-c(valf,val)
    }
  print(mean(valf))
}