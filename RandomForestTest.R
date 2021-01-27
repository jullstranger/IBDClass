args= commandArgs()
i = 1
while(args[i]!="--args"){
  cat(args[i])
  i=i+1
}


method<-args [i + 1]
typeI<-as.numeric (args[i + 2])
NTREE<-as.numeric(args [ i + 3])
FILTER<-as.numeric (args [ i + 4] )

library(randomForest)

dir<-"~/Documents/IBDClass/Imporover_IBD_Challenge/"
setwd(dir)


types<-c(CD_vs_nonIBD = "CD_vs_nonIBD" , CD_vs_UC = "CD_vs_UC" ,
         IBD_vs_nonIBD = "IBD_vs_nonIBD" , UC_vs_nonIBD = "UC_vs_nonIBD")

exclude<-c(CD_vs_nonIBD = "UC" , CD_vs_UC= "nonIBD" ,
           IBD_vs_nonIBD = "NO" , UC_vs_nonIBD = "CD")

disease2train<-c(CD_vs_nonIBD = "CD" , CD_vs_UC= "CD" ,
                 IBD_vs_nonIBD = "IBD" , UC_vs_nonIBD = "UC")

disease_other<-c(CD_vs_nonIBD = "nonIBD" , CD_vs_UC= "UC" ,
                 IBD_vs_nonIBD = "nonIBD" , UC_vs_nonIBD = "nonIBD")

nfeatures_path_1000<-c(CD_vs_nonIBD = 24, CD_vs_UC= 108,
                 IBD_vs_nonIBD = 37 , UC_vs_nonIBD = 64)

nfeatures_path_100<-c(CD_vs_nonIBD = 123, CD_vs_UC= 121,
                                   IBD_vs_nonIBD = 135 , UC_vs_nonIBD = 92)
                  
nfeatures_tax<-c(CD_vs_nonIBD = 31 , CD_vs_UC= 64,
                 IBD_vs_nonIBD = 34 , UC_vs_nonIBD = 45)

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
  
  test<-read.table("TestingDataset_TaxonomyAbundance_matrix.txt" , header = TRUE, stringsAsFactors =  FALSE)
  rows<-test$TaxID
  test<-t(test[, 2:ncol(test)])
  colnames(test)<-rows
  test<-test[ , colnames(t)]
  description<-read.table("TaxID_Description.txt" ,stringsAsFactors =  FALSE , header = TRUE, quote = "\"",  sep = "\t")
  nrow(description)
  
  nfeatures<-nfeatures_tax
  t<-cbind (Disease = class$group , t)
  
}
if(method == "Path"){
  f2r<-paste("PathAbund_" , FILTER, ".txt" , sep="")
  t<-read.table ( f2r , header = TRUE , stringsAsFactors =  FALSE)
  sum(rownames(t) != class$sampleID)
  test<-read.table("TestingDataset_PathwayAbundance_matrix.txt" , stringsAsFactors =  FALSE , header = TRUE, quote = "\"",  sep = "\t")
  rows<-test$PathID
  test<-t(test[, 2:ncol(test)])
  colnames(test)<-rows
  test<-test[ , colnames(t)]
  description<-read.table("PathID_Description.txt" , stringsAsFactors =  FALSE , header = TRUE, quote = "\"",  sep = "\t")
  nrow(description)
  
  nfeatures<-nfeatures_path_1000
  if(FILTER == 100){
    nfeatures <- nfeatures_path_100
  }
  
  if(SCALE){
    t<-data.frame(Disease = class$group , scale(t) )
    test<-scale(test)
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



if (! file.exists(method)){
  dir.create(method)
}
subDir<-paste(method , "/" , type , "/" , sep="")
if ( ! file.exists(subDir )){
  dir.create(subDir)
}

f2r<-paste(subDir , method, "_" , type , "_Importance_" , nfeatures[type] ,  "_bestFeatures.txt" , sep="")
if(FILTER == 100){
  f2r<-paste(subDir , method, "_" , type , "_Importance_" , nfeatures[type] ,  "_100_bestFeatures.txt" , sep="")
}
all.best <-read.table (f2r, header = TRUE, stringsAsFactors =  FALSE)

set.seed(71)
ttt<-tt[ , colnames(tt) %in% c("Disease" , rownames(all.best ) )  ]
ttt.rf <- randomForest(Disease~ ., ttt, importance = TRUE , ntree = NTREE, keep.forest = TRUE)
print(ttt.rf)


d2t<-disease2train[type]
test<-test[ , rownames(all.best)]
pred<-predict( ttt.rf , test ,type = "prob")
pred<-data.frame( SampleID = rownames(pred) , pred )
d2t<-disease2train[type]
od<-disease_other[type]

colnames(pred) <- c("SampleID" , 
                    paste("Confidence_Value_" , d2t , sep="") , 
                    paste("Confidence_Value_" , od , sep=""))

subDir<-paste( getwd() , "/SC2_140120_v2/" , sep="")
if ( ! file.exists(subDir )){
  dir.create(subDir)
}

imp<-importance(ttt.rf )


if(method == "Tax") {
  suffix = "Taxonomy"
  imp<-data.frame ( TaxID = rownames(imp) , Importance = imp[, 4])
  imp<-merge (imp , description[, 1:2] , by.x = "TaxID" , by.y = "TaxID" )
  colnames(imp)<-c("TaxonomyID" , "Importance_Optional" , "Description")
}else {
  suffix = "Pathways"
  imp<-data.frame ( PathID = rownames(imp) , Importance = imp[, 4])
  imp<-merge (imp , description , by.x = "PathID" , by.y = "PathID" )
  colnames(imp)<-c("PathwayID" , "Importance_Optional" , "Description")
}


f2p<-paste(subDir , "SC2-Processed_", suffix , "_", type , "_" , NTREE , "_RF_error.pdf" , sep="")
if(FILTER == 100){
  f2p<-paste(subDir , "SC2-Processed_", suffix , "_", type , "_" , NTREE , "_RF_error_100.pdf" , sep="")
}
pdf(f2p)
plot(ttt.rf , main = paste("Error OOB = " , round(min(ttt.rf$err.rate[,1 ]) * 100, digits =  2) , sep = "") )
dev.off()

f2w<-paste(subDir , "SC2-Processed_", suffix , "_", type , "_Prediction.txt" , sep="")
if(FILTER == 100){
  f2w<-paste(subDir , "SC2-Processed_", suffix , "_", type , "_Prediction_100.txt" , sep="")
}
write.table (file = f2w, pred , row.names = FALSE ,  quote = FALSE, sep = "\t")


f2w<- paste ( subDir ,"SC2-Processed_", suffix , "_", type , "_Features.txt" , sep="" )
if(FILTER == 100){
  f2w<- paste ( subDir ,"SC2-Processed_", suffix , "_", type , "_Features_100.txt" , sep="" )
}
write.table(file = f2w, imp , row.names = FALSE , quote = FALSE, sep = "\t")

#######################
# error<-matrix(ncol = 3, nrow = 500)
# for( i in 1:500){
#   ttt.rf <- randomForest(Disease~ ., ttt, importance = TRUE , ntree = i, keep.forest = TRUE)
#   error[i, 1]<-sum(ttt.rf$err.rate[ , 1])/nrow(ttt.rf$err.rate)
#   error[i, 2]<-sum(ttt.rf$err.rate[ , 2])/nrow(ttt.rf$err.rate)
#   error[i, 3]<-sum(ttt.rf$err.rate[ , 3])/nrow(ttt.rf$err.rate)
# }
# 
# 
# colnames(error) <- colnames(ttt.rf$err.rate)
# plot( error[, 1] , type = "l" , ylim = c(0, 0.5))
# points( error[, 2] , type = "l"  , col = "red")
# points( error[, 3] , type = "l" , col = "green")

 # plot( ttt.rf$err.rate[, 1] , type = "l" , ylim = c(0, 0.5) )
 # points( ttt.rf$err.rate[, 2] , type = "l"  , col = "red")
 # points( ttt.rf$err.rate[, 3] , type = "l" , col = "green")

# MDSplot (ttt.rf, ttt$Disease, palette = c("red" , "blue"))
# print(ttt.rf)
# plot(ttt.rf)

