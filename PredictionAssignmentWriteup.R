library(caret)
library(lattice)
library(ggplot2)
library(reshape)
library(corrplot)
library(AppliedPredictiveModeling)

traininig<-read.csv("pml-traininig.csv",stringsAsFactor=FALSE,skip=0,fill=NA,comment.char="#")
testing<-read.csv("pml-testinging.csv")

var<-names(traininig)[apply(traininig,2,function(x) table(is.na(x))[1]==19622)]   
traininig2<-traininig[,var]
testing2<-testing[,var[-length(var)]]
var2<-melt(apply(traininig2,2,function(x) sum(ifelse(x=="",1,0)))==0)

select.var<-rownames(var2)[var2$value==TRUE]
traininig3<-traininig2[,select.var]
testing3<-testing2[,select.var[-length(select.var)]]
traininig4<-traininig3[,names(traininig3[-c(1:7,length(traininig3))])]
testing4<-testing3[,names(testing3[-c(1:7)])]
correlations <- cor(traininig4)
corrplot(correlations,order = "hclust",tl.cex = .5)
highCorr <- findCorrelation(correlations, cutoff = .75)
predictor <- traininig4[, -highCorr]
filtered.testing4 <- testing4[, -highCorr]
classe<-traininig3$classe
traininigData<-cbind(classe,predictor)
rfModel <- randomForest(classe ~ .,data = traininigData,importance = TRUE,ntrees = 10)

par(mar=c(3,4,4,4)) 
plot(rfModel)
varImpPlot(rfModel,cex=.5) 
out.testing<-predict(rfModel,filtered.testing4)
answers<- as.vector(out.testing)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answers)
