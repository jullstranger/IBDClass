args= commandArgs()
i = 1
while(args[i]!="--args"){
  cat(args[i])
  i=i+1
}


method<-args [i + 1]
typeI<-as.numeric (args[i + 2])

library(randomForest)

dir<-"~/Documents/IBDClass/Imporover_IBD_Challenge/"
setwd(dir)


types<-c(CD_vs_nonIBD = "CD_vs_nonIBD" , CD_vs_UC = "CD_vs_UC" ,
         IBD_vs_nonIBD = "IBD_vs_nonIBD" , UC_vs_nonIBD = "UC_vs_nonIBD")

exclude<-c(CD_vs_nonIBD = "UC" , CD_vs_UC= "nonIBD" ,
           IBD_vs_nonIBD = "NO" , UC_vs_nonIBD = "CD")

disease2train<-c(CD_vs_nonIBD = "CD" , CD_vs_UC= "CD" ,
           IBD_vs_nonIBD = "IBD" , UC_vs_nonIBD = "UC")


type<-types[typeI]
print(paste ("we will analyse" , method, "dataset for the type" , type , 
             "with exclude" , exclude[type],
             "disease to train" , disease2train[type] ,
             sep = " ") )

SCALE = TRUE

class<- read.table ("Class_He_Schi.txt" , header = TRUE , stringsAsFactors =  FALSE)

if(method == "Tax"){
  t<-read.table ("TaxAbund_genus_01.txt" , header = TRUE , stringsAsFactors =  FALSE)
  sum(rownames(t) != class$sampleID)
  t<-cbind (Disease = class$group , t)
}
if(method == "Path"){
  t<-read.table ("PathAbund_100.txt" , header = TRUE , stringsAsFactors =  FALSE)
  sum(rownames(t) != class$sampleID)
  if(SCALE){
    t<-data.frame(Disease = class$group , scale(t) )
  }else {
    t<-data.frame(Disease = class$group , t)
  }
}


if(exclude[type] == "NO"){
  # merge CD with UC and call them IBD
  tt<-t
  tt$Disease<-as.character (tt$Disease)
  tt$Disease[tt$Disease != "nonIBD"]<-"IBD"
  tt$Disease<-as.factor(tt$Disease)
}else{
  tt<-t [t$Disease != exclude[type] , ]
  tt$Disease<-droplevels(tt$Disease)
}

print(paste( unique(tt$Disease) , sep=" "))

error<-vector ( "numeric")
all.best<-character()
d2t<-disease2train[type]
set.seed(71)

for ( i in 1: 100 ) {
  training<- c(sample (which(tt$Disease ==d2t) , 0.8 * sum(tt$Disease == d2t)) ,
                     sample(which(tt$Disease != d2t) , 0.8 * sum(tt$Disease != d2t) )  
  )
                     
  tt.rf <- randomForest(Disease ~ ., data=tt[training,] , importance = TRUE)
  imp<-importance (tt.rf)[, 3]
  best10<-names(sort(imp , decreasing = TRUE)[1:10])
  if(i == 1){
    all.best<-best10
  }else{
    all.best<-union(all.best , best10)
  }
  
  print(paste( i, length(all.best) , sep = " "))
  
  tt.pred <- predict(tt.rf, tt[-training,])
  error[i] <- sum(tt.pred != tt$Disease [-training])/(nrow(tt) - length(training))
}


if (! file.exists(method)){
  dir.create(method)
}
subDir<-paste(method , "/" , type , "/" , sep="")
if ( ! file.exists(subDir )){
  dir.create(subDir)
}


ttt<-tt[ , colnames(tt) %in% c("Disease" , all.best) ]
ttt.rf <- randomForest(Disease~ ., ttt, keep.forest=FALSE , importance = TRUE , ntree = 500)
print(ttt.rf)


f2p<-paste(subDir , method, "_" , type , "_RF_" , length(all.best) ,  "_100_bestFeatures.pdf" , sep="")
pdf(f2p)
  plot(ttt.rf)
dev.off()

f2w<-paste(subDir , method, "_" , type , "_Importance_" , length(all.best) ,  "_100_bestFeatures.txt" , sep="")
write.table(file = f2w , importance (ttt.rf) , quote = FALSE )

f2p<-paste(subDir , method, "_" , type , "_Importance_" , length(all.best) ,  "_100_bestFeatures.pdf" , sep="")
pdf(f2p)
 varImpPlot ( ttt.rf )
dev.off()

f2p<-paste(subDir , method, "_" , type , "_100_Error.pdf" , sep="")
pdf(f2p)
plot(error , type = "l")
dev.off()


############################################################
# if(method == "Both"){
# 
#   tt<-read.table ("TaxAbund_genus_01.txt" , header = TRUE , stringsAsFactors =  FALSE)
#   sum(rownames(t) != class$sampleID)
#   
#   t<-read.table ("PathAbund_1000.txt" , header = TRUE , stringsAsFactors =  FALSE)
#   sum(rownames(t) != class$sampleID)
#   
#   if(SCALE){
#     t<-data.frame(Disease = class$group ,tt,  scale(t) )
#   }else {
#     t<-data.frame(Disease = class$group , tt , t)
#   }
# }


# t.rf <- randomForest(Disease ~ . , t , prox=TRUE)
# MDSplot( t.rf , t$Disease)
# MDSplot(t.rf, t$Disease, palette=rep(1, 3), pch=as.numeric(t$Disease))
# 
# 
# ind <- sample(2, nrow(t), replace = TRUE, prob=c(0.7, 0.3))
# t.rf <- randomForest(Disease ~ ., data=t[ind == 1,])
# t.pred <- predict(t.rf, t[ind == 2,])
# table(observed = t[ind==2, "Disease"], predicted = t.pred)

# it looks like I cannot predict UC, but well CD
# I will remove UC patients from the dataset, and look what is will be 
