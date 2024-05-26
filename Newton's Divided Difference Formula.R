# Newton's Divided Difference Formula 
NDDF<-function(x,y,x0){
  sum<-0
  dx<-1
  del<-y
  l<-length(x)-1
  for(i in 1:l){
    dx<-dx*(x0-x[i])
    difx<-diff.default(x, lag=i, differences=1)
    dify<-diff.default(del,lag=1, differences=1)
    del<-dify/difx
    sum<-sum+dx*del[1]
  }
  return(y[1]+sum)
}
# An example
x<-c(2.0, 2.2, 2.4, 2.7, 2.8)
y<-c(0.9772499, 0.9860966, 0.9918025, 0.9965330, 0.9974449)
NDDF(x, y, 2.1)	#interpolation
# R output
[1] 	0.9821355112
NDDF(x, y, 2.75)	#extrapolation
# R output
[1] 	0.9970204143
