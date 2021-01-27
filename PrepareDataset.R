dir<-"~/Documents/IBDClass/Imporover_IBD_Challenge/"
setwd(dir)

t.he<-read.table("TrainingHe_TaxonomyAbundance_matrix.txt", sep="\t" , stringsAsFactors =  FALSE , header = TRUE)
t.schi<-read.table("TrainingSchirmer_TaxonomyAbundance_matrix.txt", sep="\t" , stringsAsFactors = FALSE,  header = TRUE)


nrow(t.he)
# 7247
nrow(t.schi)
# 7247

ncol(t.he)
#117
ncol(t.schi)
# 55

sum(t.he$TaxID != t.schi$TaxID)
# [1] 0
# first He, then Schirmer

Class_labels_He <- read.delim("~/Documents/IBDClass/testset_subchallenge2_files/Class_labels_He.txt")
Class_labels_Schirmer <- read.delim("~/Documents/IBDClass/testset_subchallenge2_files/Class_labels_Schirmer.txt")

rownames(t.he)<-t.he$TaxID
rownames(t.schi)<-t.schi$TaxID

# merge datasets
t.he.schi<-rbind ( t(t.he[, 2:ncol(t.he)]) , t(t.schi[, 2:ncol(t.schi)]) )
nrow(t.he.schi)
# [1] 170
ncol(t.he.schi)
# [1] 7247

class<-rbind( data.frame( Study = "He" , Class_labels_He ) , data.frame( Study = "Schirmer" ,  Class_labels_Schirmer) )

# check 

sum(class$sampleID != rownames (t.he.schi ))
# 0

morethan0.1<-function(c){
  return (sum(c > 0.1))
}

ttt<-t.he.schi
abundant<- apply(ttt, 2 , morethan0.1) # here you calculate how many samples per each tax have abundancy 
# more than 10 % 

ttt.abundant<-ttt [ , which (abundant > nrow(ttt)/10) ] # select taxes for which at least 10 % of samples
# have abundancy more than 10 % 
abundant<- apply(ttt.abundant, 2 , morethan0.1)

# chose taxons at the level of Family
TaxID_Description <- read.delim("~/Documents/IBDClass/testset_subchallenge2_files/TaxID_Description.txt")
unique(TaxID_Description$Rank)
# [1] species      genus        family       order        class        phylum       superkingdom
# Levels: class family genus order phylum species superkingdom
ncol(ttt.abundant)
#[1] 276
rank = "genus"
indxs<-which (colnames (ttt.abundant) %in%  TaxID_Description$TaxID [TaxID_Description$Rank == rank ])
ttt.abundant.s<-ttt.abundant [, indxs]
ncol(ttt.abundant.s)
# [1] 67
f2w<-"Class_He_Schi.txt"
write.table( file = f2w , class, row.names = FALSE, quote = FALSE)

f2w<- paste("TaxAbund_" , rank , "_01.txt", sep="")
write.table (file= f2w, ttt.abundant.s , quote = FALSE)


#####################################################################################################################
p.he<-read.table ("TrainingHe_PathwayAbundance_matrix.txt" , sep="\t" , stringsAsFactors =  FALSE , header = TRUE)
p.schi<-read.table ("TrainingSchirmer_PathwayAbundance_matrix.txt" , sep="\t" , stringsAsFactors =  FALSE , header = TRUE)
rownames(p.he)<-p.he$PathID
rownames(p.schi)<-p.schi$PathID

p.he.schi<-rbind ( t(p.he[, 2:ncol(p.he)]) , t(p.schi[, 2:ncol(p.schi)]) )
nrow(p.he.schi)
# [1] 170
ncol(p.he.schi)
# [1] [1] 12648


# morethan10<-function(c){
#   return (sum(c > 10))
# }

morethan10<-function(c){
  return (sum(c > 100))
}

ttt<-p.he.schi
abundant<- apply(ttt, 2 , morethan10)
ppp.abundant<-ttt [ , which (abundant > nrow(ttt)/10) ]
abundant<- apply(ppp.abundant, 2 , morethan10)

ncol(ppp.abundant)
# [1] 426
sum(class$sampleID != rownames(ppp.abundant))
# [1] 0

f2w<- paste("PathAbund_100.txt", sep="")
write.table (file= f2w, ppp.abundant , quote = FALSE)

