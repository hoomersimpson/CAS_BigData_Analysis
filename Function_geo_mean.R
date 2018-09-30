geo_mean<-function(x){
  n<-length(x)
  out<-0
  for(i in n){
    out<-out+log(x[i])
  }
  return(out)
}