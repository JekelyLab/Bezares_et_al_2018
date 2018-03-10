#!/usr/bin/Rscript;
#Description#####
#Author: Luis Bezares Calderon, written in July,2017
#Purpose:Script to calculate the speed and area of freely swimming larvae.
#Input data: Track coordinates and area values extracted from the imageJ macro FigS1_StartleFreelySwimming.ijm. 
#Location input data: ~./SourceDataforR/Freely_swimming_analysis
##Publication: Bezares-Calderon et al, 2018.
# Results from this script were used to generate the plots shown in suppl. Figure 1A-B

##Packages required#####
library(ggplot2)
require(ggplot2)
require(reshape2)
library(plyr)
library("cowplot")

library("gridExtra")
library("ggExtra")

###Import File list with information about stimulus start and size of track.####

FilePropList<-read.table("~./File_info15fps.txt",header = T)


###Mtrack results analysis#############

#import the list of Mtrack files. Each file is one track.
MtrackFile<-readLines("~./MtrackPoolfile15.txt");

#The directory path were the actual values for each track are stored.
MtrackPath<-"~./PooledMtrack15/"

####Create Data Ftame that will store the raw speed values for each track.####
FinalSpeed<-data.frame(matrix(ncol=length(FilePropList$Experiment_ID),nrow=79)) #nrow value is the final interval of all videos to be analyzed. Make sure all the videos are at least this number of frames long.
names(FinalSpeed)<-paste(FilePropList$Experiment_ID,FilePropList$ROI_ID,sep="_")

####This 'for' will extract the track coordinates and calculate speed and adjust the frame relative to the start of the stimulus.####

for(j in 1:length(FilePropList$Experiment_ID)){
  for(k in 1:length(MtrackFile)){
    MFullpath<-paste(MtrackPath,MtrackFile[k],sep="");
    if(grepl(FilePropList[j,"Experiment_ID"],MtrackFile[k])&&grepl(FilePropList[j,"ROI_ID"],MtrackFile[k])){
      print(MFullpath)
      print(FilePropList[j,"Experiment_ID"])
      print(MtrackFile[k])
      numbRows<-FilePropList[j,"Track_length"]
      Tracks<-read.table(MFullpath,skip=2, nrows = numbRows,header = F)
      Tracks<-as.data.frame(Tracks)
      Speed<-list();
      OfSet<-1 ###Important value to decide how rough or smooth is the speed calculation.
      
      
      #This for will calculate the pitagoraean distance change in time.
      for(i in 1:(length(Tracks$V1)-OfSet)){
        Vx=Tracks$V2[i+OfSet]-Tracks$V2[i];
        VxP=Vx/0.1077  #0.1077 this value is the distance in pixels.
        Vy=Tracks$V3[i+OfSet]-Tracks$V3[i];
        VyP=Vy/0.1077
        Speed[i]<-(sqrt(('^'(VxP,2))+('^'(VyP,2))))/(1000*OfSet*0.06)
      }
      RelativeFrame<-Tracks$V1-FilePropList[j,"Stimulus_Start_ROI.dependent"]
      Speed<-c(rep('NA',OfSet),Speed);
      Tracks$Speed<-Speed;
      Tracks$Speed<-as.numeric(Tracks$Speed)
      Tracks$RelativeFrame<-RelativeFrame;
      FinalSpeed[,j]<-Tracks[which(Tracks$RelativeFrame > -10 & Tracks$RelativeFrame < 70),"Speed"]  # This has to be adjusted depending on the lenght of the video analyzed.
    }
  }
}
#transforming from frames to seconds 
  time<-c(seq(-9,69))
  RealTime<-time*0.06 #change it according to recording speed.
  FinalSpeed$time<-RealTime
  
####normalizing track speeds####
  #This code will substract the prior average speed to individual values. 
  NormFinal<-FinalSpeed
  for(k in 1:(ncol(FinalSpeed)-1)){
    PriorAvg<-which(FinalSpeed$time < 0)
    MeanVal<-mean(FinalSpeed[PriorAvg,k],na.rm=T) #Calculate the arithmetic mean of the speed values prior to stimulation.
    print(MeanVal);
    NormFinal[,k]<-FinalSpeed[,k]-MeanVal
  }
    MtMean<-rowMeans(NormFinal[,1:21],na.rm=T)
  NormFinal$MtMean<-MtMean
  meltF<-melt(NormFinal,id.vars = "time")
  select2=grep("Mean",meltF$variable)
  
  ###Plotting Speed####
  ggplot(data = meltF,aes(x=time,y=value,col=variable))+geom_line(show.legend = F)+geom_line(data=meltF[select2,],size=3,show.legend = F)+scale_color_manual(values = c(rep("gray",21),"blue"))+scale_x_continuous(breaks = pretty(NormFinal$time,n=5))+scale_y_continuous(breaks = pretty(meltF$value,n=5))+labs(x="Time relative to stimulus (sec)",y="Relative (prior to stimulus) speed (mm/s)")+ geom_vline(xintercept = 0,linetype = "longdash")

###Particle results analysis#############
SummFile<-readLines("~./SumPoolfile15.txt");

SummaryPath<-"~./PooledSum15/"

SumFinal<-data.frame(matrix(ncol=length(FilePropList$Experiment_ID),nrow=79)) #nrow value is the final interval of all videos to be analyzed. Make sure all the videos are at least this number of frames long.
names(SumFinal)<-paste(FilePropList$Experiment_ID,FilePropList$ROI_ID,sep="_")

for(j in 1:length(FilePropList$Experiment_ID)){
  for(k in 1:length(SummFile)){
    SFullpath<-paste(SummaryPath,SummFile[k],sep="");
    if(grepl(FilePropList[j,"Experiment_ID"],SummFile[k])&&grepl(FilePropList[j,"ROI_ID"],SummFile[k])){
      print(FilePropList[j,"Experiment_ID"])
      print(SummFile[k])
      print(SFullpath)
      numbRows<-FilePropList[j,"Track_length"]
      Area<-read.table(SFullpath,sep="\t",header=F,colClasses = c(rep("NULL",3),rep("numeric",1),rep("NULL",11)), nrows = numbRows,skip = 1)
      Area<-as.data.frame(Area)
      Beg<-1-(FilePropList[j,"Stimulus_Start_ROI.dependent"])
      End<-length(Area$V4)-(FilePropList[j,"Stimulus_Start_ROI.dependent"])
      ReFrame<-seq(Beg,End)
      Area$ReFrame<-ReFrame
      SumFinal[,j]<-Area[which(Area$ReFrame > -10 & Area$ReFrame < 70),"V4"]
    }
  }
}
SumFinal$time<-RealTime
    
#####normalizing Area####

NormSumFinal<-SumFinal
for(k in 1:(ncol(SumFinal)-1)){
  PriorAvg<-which(SumFinal$time < 0)
  MeanVal<-mean(SumFinal[PriorAvg,k],na.rm=T) #Calculate the arithmetic mean of the speed values prior to stimulation.
  print(MeanVal);
  NormSumFinal[,k]<-SumFinal[,k]-MeanVal
}

###Calculating mean####
SMean<-rowMeans(NormSumFinal[,1:21],na.rm=T)
NormSumFinal$SMean<-SMean
meltSF<-melt(NormSumFinal,id.vars = "time")
select2=grep("Mean",meltSF$variable)

###Plotting Area####
ggplot(data = meltSF,aes(x=time,y=value,col=variable))+geom_line(show.legend = F)+geom_line(data=meltSF[select2,],size=3,show.legend = F)+scale_color_manual(values = c(rep("gray",21),"blue"))+scale_x_continuous(breaks = pretty(NormSumFinal$time,n=5))+scale_y_continuous(breaks = pretty(meltSF$value,n=5))+labs(x="Time relative to stimulus (sec)",y="Increase in Area (um2)")+ geom_vline(xintercept = 0,linetype = "longdash")

