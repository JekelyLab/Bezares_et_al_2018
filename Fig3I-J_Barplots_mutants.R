#!/usr/bin/Rscript;
#Description#####

#Author: Luis Bezares Calderon,written in April,2017

#Purpose: This script plots as stacked barplots the percentage of responding and non-responding larvae.

#Input data:Tables sorted by genotype and percentage of responding and non-responding larvae.

#Data location:~./SourceDataforR/Phenotyping/


##Publication: Bezares-Calderon et al, 2018. Results from this script were used to generate the graph shown in Figure 3I-J#####

##Packages required#####

library(ggplot2)
require(ggplot2)
require(reshape2)

####PKD2-1####

##Loading data####
PKD2phenotyperesults<-read.table("~./Phenotying_PKD2_T2.txt",header=TRUE,sep="\t",na.strings = "NA",fileEncoding="");
names(PKD2phenotyperesults)<-c("GenotypePKD2","Non-Responders", "Responders")
PKD2phenotyperesults<-as.data.frame(PKD2phenotyperesults)

###Using the function melt for easier plotting in ggplot

pkd2_molten<-melt(PKD2phenotyperesults,"GenotypePKD2")

##Reordering the factors from WT to transheterozygote genotypes
pkd2_molten$GenotypePKD2<-factor(pkd2_molten$GenotypePKD2,levels = c("WT/WT","WT/5bD","WT/137bD","5bD/5bD","137bD/137bD","137bD/5bD"))

##Plot
barpkd2 <-ggplot(data=pkd2_molten,aes(x=GenotypePKD2,y=value,fill=variable)) + geom_bar(stat="identity")
barpkd2+ theme_minimal() + scale_fill_grey()+labs(title = "pkd2-1",x="",y="% larvae") +theme(axis.text.x = element_text(angle = 90, hjust = 1),,legend.title=element_blank())


###PKD1-1####

##Loading data####
PKD1phenotyperesults<-read.table("~./Phenotying_PKD1_T1T5.txt",header=TRUE,sep="\t",na.strings = "NA",fileEncoding="");


PKD1phenotyperesults<-as.data.frame(PKD1phenotyperesults)


names(PKD1phenotyperesults)<-c("GenotypePKD1","Non-Responders", "Responders")


###Using the function melt for easier plotting in ggplot

pkd1_molten<-melt(PKD1phenotyperesults,"GenotypePKD1")

##Reordering the factors from WT to transheterozygote genotypes

pkd1_molten$GenotypePKD1<-factor(pkd1_molten$GenotypePKD1,levels = c("WT/WT","1bD/WT","i1/WT","1bD/1bD","i1/i1","1bD/i1"))


barPKD1 <-ggplot(data=pkd1_molten,aes(x=GenotypePKD1,y=value,fill=variable))+ geom_bar(stat="identity")
barPKD1 + theme_minimal() + scale_fill_grey()+labs(title = "pkd1-1",x="",y="% larvae") +theme(axis.text.x = element_text(angle = 90, hjust = 1),,legend.title=element_blank())


