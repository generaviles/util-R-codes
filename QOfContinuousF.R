QOfContinuousF=function(datasetCF){
  #codigo para hacer las pruebas
  #datasetCF<- cbind.data.frame(DD1=d_preproc[,2],d_preproc[,10:13],d_preproc[,58:62])
  
  Feature<-colnames(datasetCF)
  Count<- apply(datasetCF,2,FUN = function(x) length(x)) 
  Miss<- apply(datasetCF,2,FUN = function(x)  sum(is.na(x)))
  Card<- apply(datasetCF,2,FUN = function(x) length(levels(factor(x))))
  Min<-apply(datasetCF,2,FUN = function(x) min(x))
  Qrt1<-apply(datasetCF,2,FUN = function(x) round(quantile(as.numeric(x),.25),digits = 2))
  Median<-apply(datasetCF,2,FUN = function(x) round(quantile(as.numeric(x),.5),digits = 2))
  Qrt3<-apply(datasetCF,2,FUN = function(x) round(quantile(as.numeric(x),.75),digits = 2))
  Max <-apply(datasetCF,2,FUN = function(x) max(x))
  Mean<-apply(datasetCF,2,FUN = function(x) round(sd(as.numeric(x)),digits = 2))
  Sdev<-apply(datasetCF,2,FUN = function(x) round(mean(as.numeric(x)),digits = 2))
  
  TQContinuousFeatures<-data.frame(Count,Miss,Card,Min,Qrt1,Median,Qrt3,Max,Mean,Sdev)
  
}