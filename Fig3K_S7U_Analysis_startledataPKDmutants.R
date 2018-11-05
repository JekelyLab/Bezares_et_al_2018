#!/usr/bin/Rscript;
#Description#####
#Author: Luis Bezares Calderon, written in March,2017#####

#Purpose: Same as detailed for 'Fig1-S1_Analysis_startledataWT.R', but it performs the calcuation on the data recorded from the PKD1 and PKD2 mutant larvae.
#Input data: A table with manual annotations of stimulus start, start of closures, start of parapodial elevation,etc.
#Data location:~./SourceDataforR/Kynematics_measurements/
##Publication: Bezares-Calderon et al, 2018. Results from this script were used to generate the plots shown in Figure 3K and and Figure S7U.#####

##Packages required#####
install.packages("gridExtra")
install.packages("cowplot")
install.packages("ggExtra")

library(ggplot2)
require(ggplot2)
require(reshape2)
library(plyr)
library("cowplot")


######Loading PKD1-1 data and calculating metrics####

PKD11Table<-read.table("~./Kinematics_measurementsPKD1.txt",header=TRUE,sep="\t",na.strings = "NA",fileEncoding="");

PKD11Table<-as.data.frame(PKD11Table)
PKD11Table$Closure_Prototroch<-as.factor(PKD11Table$Closure_Prototroch)


PKD11Table$Elev_parapodia<-as.factor(PKD11Table$Elev_parapodia)

DurationRest2Elevation<-(PKD11Table$Beginning.Max.Elev1stpara- PKD11Table$Beginning.1stPElev)*PKD11Table$TimeperFrame

PKD11Table$DurationRest2Elevation<-DurationRest2Elevation
ProbespeedP<-(PKD11Table$Max..frame2frame.Displacement.tip..before.PARAPODIAmaxElev..diagonal...um.)/PKD11Table$TimeperFrame

PKD11Table$ProbespeedP<-ProbespeedP

RoundDist<-paste(round(PKD11Table$Distance.probe..um./100)*100,"um",sep="")

PKD11Table$RoundDist<-RoundDist

######Loading PKD2-1 data and calculating metrics####

PKD21Table<-read.table("~./Kinematics_measurementsPKD2.txt",header=TRUE,sep="\t",na.strings = "NA",fileEncoding="");

PKD21Table<-as.data.frame(PKD21Table)
PKD21Table$Closure_Prototroch<-as.factor(PKD21Table$Closure_Prototroch)

PKD21Table$Elev_parapodia<-as.factor(PKD21Table$Elev_parapodia)

ProbespeedP<-(PKD21Table$Max..frame2frame.Displacement.tip..before.PARAPODIAmaxElev..diagonal...um.)/PKD21Table$TimeperFrame

PKD21Table$ProbespeedP<-ProbespeedP

RoundDist<-paste(round(PKD21Table$Distance.probe..um./100)*100,"um",sep="")

PKD21Table$RoundDist<-RoundDist

###Creating subgroups####

PKD1Head<- which(PKD11Table$Closest.body.part.to.probe == "Head")
PKD1Pygid<-which(PKD11Table$Closest.body.part.to.probe == "Tail")


PKD2Head<- which(PKD21Table$Closest.body.part.to.probe == "Head")
PKD2Pygid<-which(PKD21Table$Closest.body.part.to.probe == "Tail")

####Fig3K ggplot PKD1 and PKD2 head####

AbrdHeadPKD1<-data.frame(PKD11Table[PKD1Head,"ProbespeedP"],PKD11Table[PKD1Head,"Relative2max_Elevation"],PKD11Table[PKD1Head,"Closure_Prototroch"])

names(AbrdHeadPKD1)<-c("SpeedP","Relative2max_Elevation","Closure_Prototroch")

gene<-"PKD1"
AbrdHeadPKD1$gene<-gene

AbrdHeadPKD2<-data.frame(PKD21Table[PKD2Head,"ProbespeedP"],PKD21Table[PKD2Head,"Relative2max_Elevation"],PKD21Table[PKD2Head,"Closure_Prototroch"])

names(AbrdHeadPKD2)<-c("SpeedP","Relative2max_Elevation","Closure_Prototroch")

gene<-"PKD2"
AbrdHeadPKD2$gene<-gene

######Merging PKD1 and PKD2 head#

PKDtablemergedH<-merge(AbrdHeadPKD1,AbrdHeadPKD2,all = T)
MerPlotH<-ggplot(data =PKDtablemergedH,aes(x=SpeedP,y=Relative2max_Elevation,col=Closure_Prototroch))+theme(legend.title=element_blank(),axis.text.x = element_text(angle = 90, hjust = 1),legend.position="none",aspect.ratio = 0.4)+ scale_color_manual(values = c("magenta")) +   geom_point(stat="identity", size = 13,shape=20,alpha=3/4)+facet_grid(gene~.)+labs(x="Max.speed filament (um/ms)",y="Max.parapodial angle (a.u.)")+scale_x_continuous(limits=c(0,100),breaks = pretty(abridgedtableHead$SpeedP, n=5))+scale_y_continuous(limits = c(0,1),breaks = c(0,0.5,1))
MerPlotH

####Fig.S3U ggplot PKD1 and PKD2 Pygid####

AbrdPygidPKD1<-data.frame(PKD11Table[PKD1Pygid,"ProbespeedP"],PKD11Table[PKD1Pygid,"Relative2max_Elevation"],PKD11Table[PKD1Pygid,"Closure_Prototroch"])

names(AbrdPygidPKD1)<-c("SpeedP","Relative2max_Elevation","Closure_Prototroch")

gene<-"PKD1"
AbrdPygidPKD1$gene<-gene



AbrdPygidPKD2<-data.frame(PKD21Table[PKD2Pygid,"ProbespeedP"],PKD21Table[PKD2Pygid,"Relative2max_Elevation"],PKD21Table[PKD2Pygid,"Closure_Prototroch"])

names(AbrdPygidPKD2)<-c("SpeedP","Relative2max_Elevation","Closure_Prototroch")

gene<-"PKD2"
AbrdPygidPKD2$gene<-gene


######MergingPKD1 and PKD2 Pygid##

PKDtablemergedT<-merge(AbrdPygidPKD1,AbrdPygidPKD2,all = T)
MerPlotT<-ggplot(data =PKDtablemergedT,aes(x=SpeedP,y=Relative2max_Elevation,col=Closure_Prototroch))+theme(legend.title=element_blank(),axis.text.x = element_text(angle = 0, hjust = 1),legend.position="none",aspect.ratio = 0.4)+ scale_color_manual(values = c("magenta")) +   geom_point(stat="identity", size = 13,shape=20,alpha=3/4)+facet_grid(gene~.)+labs(x="Max.speed filament (um/ms)",y="Max.parapodial angle (a.u.)")+scale_x_continuous(limits=c(0,100),breaks = pretty(abridgedtableHead$SpeedP, n=2))+scale_y_continuous(limits = c(0,1),breaks = c(0,0.5,1))
MerPlotT
