#!/usr/bin/Rscript;
#Description#####
#Author: Luis Bezares-Calderon, written in September,2017

#Purpose: This script plots the paired predation rates obtained for WT and mutant larvae and performs a Wilcoxon-Pratt signed rank test on the data. 

#Input data: A table with the calculated predation rates.
#data location: The file ~./SourceDataforR/Predator_assay/MeltTablePRedRatesRed.txt
#Publication: Bezares-Calderon et al, 2018. Results from this script were used to generate the graph shown in Figure 3M #####

##Packages required#####

install.packages("exactRankTests")
install.packages("coin")
library(coin)
library(cowplot)
library(ggplot2)
require(ggplot2)
require(cowplot)
require(reshape2)
library(plyr)
library("cowplot")
library(exactRankTests)
library("gridExtra")
library("ggExtra")


###Loading data and calcuating stuff#####

MergedTable<-read.table("~./MeltTablePRedRatesRed.txt",header=TRUE,sep="\t",na.strings = "NA",fileEncoding=""); 


#Statistical test####

wilcoxsign_test(PredRates$Predation_Rate_Mutant~PredRates$Predation_Rate_WT,alternative="greater",distribution="exact",zero.method="Pratt")

##Reordering the factors###
MergedTable$Genotype<-factor(MergedTable$Genotype,levels = c("WT","PKD2"))

##Fig.3M Line plot##### Lines were individually recolored to arrive to fig. 3M.
Line<-ggplot(MergedTable,aes(x=Genotype,y=Predation_Rate))+geom_line(aes(group = Pair_ID),alpha=3/4,col="blue")+geom_point(alpha=1/2,size=5)
Line


