#!/usr/bin/Rscript;
#Description#####
#Author: Luis Bezares Calderon, written in May,2017
#Purpose: This script quantifies and plots the dR/R values of multiple ROIs from raw intensity values extracted with the ImageJ macro 'Measure-intensityvaluechanges3.ijm'. The plots are aligned relative to the time of stimulation.    ##### 
#Input data: GCaMP and tdTomato pixel intensity values of specific ROIs obtained from time-lapse recordings. For each recording and channel a list of background values was obtained using the same ROI. Therefore, every experiment has to have 4 input files.
#Data location:~./SourceDataforR/Calcium_imaging/
##Publication: Bezares-Calderon et al, 2018. Results from this script were used to generate the plots shown in Figure 2G.#####

##Packages required#####
library(ggplot2)
require(ggplot2)
require(reshape2)
library(plyr)
library("cowplot")

library("gridExtra")
library("ggExtra")

###Loading Tables and lists####
OFTable<-read.table("~./filelist-hCR1MS1.txt",header = TRUE,sep="\t")   #This file should contain as a table format the unique file names and in the second column the frame where the stimulation starts.
#hCR1/MS1:filelist-hCR1MS1.txt
#hCR2:filelist-hCR2.txt

#Calling the list of cells to be analyzed.
CellList<-readLines("~./celllistCR1MS1.txt") #This file should contain the names given to the cells to be analyzed. The file names have to have the name of the cell to be analyzed respecting upper/lower cases. 
#hCR1/MS1:cell-listCR1MS1.txt
#hCR2:celllistCR2.txt

ResFList<-readLines("~./outMergedResCR1MS1.txt") #This file contains the list of all the .txt files with the intensity values for all the ROIs to be analyzed. For each ROI measurement of the cell of interest, there must be a measurement of the background and it has to have the 'bckg' word written in the file name.
#hCR1/MS1:outMergedResCR1MS1.txt
#hCR2:outMergedResCR2.txt

Resultspath<-"~./MergedCR1MS1/" ##Where the actual .txt listed above are.
#hCR1/MS1:MergedoutCR1MS1
#hCR2:MergedoutCR2

ResFList  #Checking the correct importing of the list

columns<-c("NumFrames","Tom_signal","Tom_bckg","GC_signal","GC_bckg","dR","CorrFrame")


###Creation of file tables with file and cell analyzed####
# This code will open each .txt for each cell and its corresponding background signal measured in the GCaMP and in the Tomato channel. With the four lists it will create a data frame with the name of the file and the cell measured. It will calculate the dR/R metric and store it in the variable 'dR'. The frame number relative to the stimulus start will be also set at this stage. 

Listsnames<-list();
ListOC<-list();
o=1;
 for (i in 1:length(OFTable$File_name))
  {
   for(j in 1:length(CellList))
   {
     FOUND=0
     CombFC<-paste(OFTable$File_name[i],CellList[j],sep="_")
     for(k in 1:length(ResFList))
      {
       if(grepl(CombFC,ResFList[k]))
       {
           Fullpath<-paste(Resultspath,ResFList[k],sep="");
           if(grepl("Tom",ResFList[k]))
             {
             if(grepl("bckg",ResFList[k]))
             {
               Tom_bckg<-read.table(Fullpath,sep = "\t",header=TRUE,colClasses = c(rep("NULL",2),rep("numeric",1),rep("NULL",1))) 
             }else{
               Tom_signal<-read.table(Fullpath,sep = "\t",header=TRUE,colClasses = c(rep("NULL",2),rep("numeric",1),rep("NULL",1))) 
               
             }
            }else
            {
              if(grepl("bckg",ResFList[k]))
             {
               GC_bckg<-read.table(Fullpath,sep = "\t",header=TRUE,colClasses = c(rep("NULL",2),rep("numeric",1),rep("NULL",1))) 
             }else{
               GC_signal<-read.table(Fullpath,sep = "\t",header=TRUE,colClasses = c(rep("NULL",2),rep("numeric",1),rep("NULL",1)))
               FOUND=1
             }
           }
       }
     }
     if(FOUND==1)
     {
         noframes<-read.table(Fullpath,sep = "\t",header=TRUE,colClasses = c(rep("numeric",1),rep("NULL",3))) 
      #STframeList[o]<-OFTable$Stimulation_Start[i];
       Correc_Tom=Tom_signal-Tom_bckg
       LowMeanlimit=OFTable$Stimulation_Start[i]-(trunc(OFTable$Stimulation_Start[i]/2))
       MaxMeanlimit=OFTable$Stimulation_Start[i]-1
       meanrange<-seq(LowMeanlimit,MaxMeanlimit)
       MeanTom=mean(Correc_Tom[meanrange,])
       Correc_GC=GC_signal-GC_bckg
       MeanGC=mean(Correc_GC[meanrange,])
       dR<-(Correc_GC*MeanTom)/(Correc_Tom*MeanGC)-1
       CorrFrame<-noframes-OFTable$Stimulation_Start[i]
       assign(CombFC,setNames(data.frame(noframes,Tom_signal,Tom_bckg,GC_signal,GC_bckg,dR,CorrFrame),columns))
       Listsnames[o]<-CombFC;
       o=o+1;
      }
   }
 }


###Creating table with subseting relative frame interval to be analyzed -####
#hCR1/MS1:-5 to 22
#hCR2:-5 to -5 to 15
#This code will trim all recordings to a pre-defined length. All recordings to be measured have to have the same length as they are stored in the single data frame 'Final'. 

ListOC<-list();
Final<-data.frame(matrix(ncol=length(Listsnames),nrow=21)) #nrow:hCR1/MS1:28,hCR2:21
names(Final)<-Listsnames
par(mfrow=c(1,1),bty='L')  
for (j in 1:length(CellList))
{
  for(i in 1:length(Listsnames)){
    Final[,i]<-subset(get(Listsnames[[i]])[6],get(Listsnames[[i]])[7] > -6 & get(Listsnames[[i]])[7] < 16) #change subset
  }
}

###Setting the time scale-#### 
##This simply is the conversion from frame to seconds.


IntervalFrame<-0.24
CorrFrame<-seq(-5,)*IntervalFrame  
#Change the subset
#hCR1/MS1:-5,22
#hCR2:-5,15
Final$CorrFrame<-CorrFrame


###Creating a table per cell####
##This will collect all the dR/R for each cell from the different videos recorded. It will also calculate the mean dR/R.

for (j in 1:length(CellList))
{
  FinXCell<-Final[,grep(CellList[j],names(Final))]
  y<-c("L21","L25","L30","L20") #Select the desired stimulation levels (only for recordings at 0.24 time interval).
  CellMean<-rowMeans(subset(FinXCell,select=grep(paste(y,collapse = "|"),names(FinXCell)))) #Calculate Mean across all rows (per timepoint)
  assign(CellList[j],data.frame(Final$CorrFrame,subset(Final,select=grep(CellList[j],names(Final))),CellMean))
}


###Extra-lines dots####
##This is a patch needed to add recordings with different frame rates.Calculate for each cell separately
#CellList[1]=hCR1l or hCR2l
#CellList[2]=hCR1r or hCR2r
#CellList[3]=MS1
Mltocell<-melt(get(CellList[1]),id.vars = "Final.CorrFrame")
x<-c("L30","L2","Level21")
select=grep(paste(x,collapse = "|"),Mltocell$variable)
select2=grep("CellMean",Mltocell$variable)
Mltocell[grep("LB01d",Mltocell$variable),"Final.CorrFrame"]<-rep((0.39-0.24)*seq(-5,22),2)+Mltocell[grep("LB01d",Mltocell$variable),"Final.CorrFrame"] ##Correcting diff.frame rate


##Interporlation for calculating mean if 01d series####
#This corrects the different frame rates by interpolating values.NumRecordSameFrame needs to be adjusted for the number of values and the corresponding cell: 
#hCR1l=14
#hCR1r=13
#MS1=16
NumRecordSameFrame=16
NumRecordDiffFrame=2
Mltocell[grep("CellMean",Mltocell$variable),"value"]<-(Mltocell[grep("CellMean",Mltocell$variable),"value"]*NumRecordSameFrame+approx(Mltocell[grep("LB01d2",Mltocell$variable),"Final.CorrFrame"],Mltocell[grep("LB01d2",Mltocell$variable),"value"],xout = CorrFrame)$y+approx(Mltocell[grep("LB01d3",Mltocell$variable),"Final.CorrFrame"],Mltocell[grep("LB01d3",Mltocell$variable),"value"],xout = CorrFrame)$y)/(NumRecordSameFrame+NumRecordDiffFrame)

##The code below plots the cell stored in Mltocell in the format shown in Fig2G and FigS3C.####

Lim<-seq(from=-0.11,to=0.75,by=0.1)
Limx<-seq(from=-2,to=5.3,by=1)
ggplot(Mltocell,aes(Final.CorrFrame,value,col=variable))+geom_point(data=Mltocell[select,],size=2,show.legend = F)+geom_line(data=Mltocell[select,],size=1,show.legend = F)+geom_line(data=Mltocell[select2,],size=3,show.legend = F)+scale_y_continuous(limits = c(-0.11,0.75),breaks = pretty(Lim, n=10))+scale_x_continuous(limits = c(-1.2,5.3),breaks = pretty(Limx, n=5))+labs(x="Time relative to stimulus (sec)",y="dR/R-1")+scale_color_manual(values = c("forestgreen",rep("gray",19)))+ geom_vline(xintercept = 0,linetype = "longdash")


####Optional#####
###Facetgriding### Only if you want all cells to be plotted in the same graph.

abridgedCell<-data.frame(get(CellList[1])[["CellMean"]],get(CellList[2])[["CellMean"]],get(CellList[3])[["CellMean"]])
names(abridgedCell)<-CellList
abridgedCell$CorrFrame<-CorrFrame
Mltcell<-melt(abridgedCell,id.vars = "CorrFrame")
Plotcell<-ggplot(data = Mltcell,aes(x=CorrFrame,y=value))+geom_line(size=3)+facet_grid(.~variable)+labs(x="Time relative to stimulus (sec)",y="dR/R")+scale_y_continuous(limits = c(-0.1,1))+scale_x_continuous(breaks = pretty(abridgedCell$CorrFrame, n=5))
Plotcell
 