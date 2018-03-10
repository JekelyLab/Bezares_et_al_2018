#!/usr/bin/Rscript;
#Description#####
#Author: Luis Bezares Calderon, written in March,2017, last modified in January, 2018#####

##Purpose: This script calculates a number of different metrics of the startle response using raw frame number annotations, and frame rates.It also forms groups based on these metrics and other annotations.
#Input data: A table with manual annotations of stimulus start, start of closures, start of parapodial elevation,etc.
#Data location:~./SourceDataforR/Kynematics_measurements/
##Publication: Bezares-Calderon et al, 2018. Resultswere to generate the plots of this script are shown in Figure 1 and Figure S1.#####

##Packages required#####
install.packages("gridExtra")
install.packages("cowplot")
install.packages("ggElevra")

library(ggplot2)
require(ggplot2)
require(reshape2)
library(plyr)
library("cowplot")


###Loading WT dataset and calcuating metrics#####

TableResults<-read.table("~./Kinematics_measurementsWT.txt",header=TRUE,sep="\t",na.strings = "NA",fileEncoding=""); 


TableResults<-as.data.frame(TableResults)

TableResults$Closure_Prototroch<-as.factor(TableResults$Closure_Prototroch)
TableResults$Elev_parapodia<-as.factor(TableResults$Elev_parapodia)

#Calculating Latency of max.parapodia Elevation and transforming to ms.

DurationRest2Elevation<-(TableResults$Beginning.Max.Elev1stpara- TableResults$Beginning.1stPElevMAIN)*TableResults$TimeperFrame
TableResults$DurationRest2Elevation<-DurationRest2Elevation

#Calculating Latency of closure start and transforming to ms.

LatencyClosure<-(TableResults$Beginning.Closure.PrototrochMAIN-TableResults$Beginning.stimul)*TableResults$TimeperFrame

TableResults$LatencyClosure<-LatencyClosure
#Bodytroch
LCloBody<-(TableResults$Beginning.Closure.BodytrochsMAIN-TableResults$Beginning.stimul)*TableResults$TimeperFrame

TableResults$LCloBody<-LCloBody
#Calculating Latency of Elevation start and transforming to ms.

LatencyElev<-(TableResults$Beginning.1stPElevMAIN-TableResults$Beginning.stimul)*TableResults$TimeperFrame

TableResults$LatencyElev<-LatencyElev

#Calculating speed of probe prior to parapodia Elevation and transforming to ms.

ProbespeedP<-(TableResults$Max..frame2frame.Displacement.tip..before.PARAPODIAmaxElev..diagonal...um.)/TableResults$TimeperFrame

TableResults$ProbespeedP<-ProbespeedP


#Calculating delay of Elevation to prototroch closure

Elev2Proto<-(TableResults$Beginning.1stPElevMAIN-TableResults$Beginning.Closure.PrototrochMAIN)

TableResults$Elev2Proto<-Elev2Proto

#Calculating delay of Elevation to bodytroch closure

Elev2Body<-(TableResults$Beginning.1stPElevMAIN-TableResults$Beginning.Closure.BodytrochsMAIN)

TableResults$Elev2Body<-Elev2Body

#Calculating delay Left-Right Elevation 1st parapodia Elevation

LRElev1stPa<-abs(TableResults$Beginning.1stPElevMAIN-TableResults$Beginning.1stPElev_Oppo)
TableResults$LRElev1stPa<-LRElev1stPa

#Calculating delay Segment coordination between segment 1 and segment 3.

Coord12<-(TableResults$Beginning.2ndPElev-TableResults$Beginning.1stPElevMAIN)
Coord13<-(TableResults$Beginning.3rdPElev-TableResults$Beginning.1stPElevMAIN)

TableResults$Coord12<-Coord12
TableResults$Coord13<-Coord13

#Giving an index of distance (by rounding the measured distance) of the probe for posterior sorting of data points.

RoundDist<-paste(round(TableResults$Distance.probe..um./100)*100,"um",sep="")

TableResults$RoundDist<-RoundDist

RoundSpeedP<-round(TableResults$ProbespeedP/20)*20
TableResults$RoundSpeedP<-RoundSpeedP


###Making subgroups based on stimulation site and distance#####

Head100<-which(TableResults$Distance.probe..um. < 150 & TableResults$Closest.body.part.to.probe == "Head")

Head100LowElev<-which(TableResults$Distance.probe..um. < 150 & TableResults$Closest.body.part.to.probe == "Head" & TableResults$Relative2max_Elevation < cut_off[1] & TableResults$Relative2max_Elevation > 0.001)

Head100HighElev<-which(TableResults$Distance.probe..um. < 150 & TableResults$Closest.body.part.to.probe == "Head" & TableResults$Relative2max_Elevation > cut_off[1])

Pygid100HighElev<-which(TableResults$Distance.probe..um. < 150 & TableResults$Closest.body.part.to.probe == "Pygid" & TableResults$Relative2max_elevation > cut_off[1])


Head<-which(TableResults$Closest.body.part.to.probe == "Head")
Pygid<-which(TableResults$Closest.body.part.to.probe == "Pygid")
Side<-which(TableResults$Closest.body.part.to.probe == "Side")

###Fig.1G Head100-scatterplot and histogram####
abridgedtableHead100<-data.frame(TableResults[Head100,"ProbespeedP"],TableResults[Head100,"Relative2max_Elevation"],TableResults[Head100,"Closure_Prototroch"],TableResults[Head100,"Elev_parapodia"])

names(abridgedtableHead100)<-c("SpeedP","Relative2max_Elevation","Closure_Prototroch","ElevationParapodia")

PlotHead100<-ggplot(data=abridgedtableHead100,aes(x=SpeedP,y=Relative2max_Elevation)) + geom_point(stat="identity",aes(col = factor(abridgedtableHead100$Closure_Prototroch)), size = 3.5,shape=20,alpha=3/4)   


###Scatterplot Head
PlotHead100 + labs(x="Max.speed filament (um/ms)",y="Max.parapodial angle (a.u.)")+scale_color_manual(values = c("forestgreen", "magenta"))+scale_x_continuous(breaks = pretty(abridgedtableHead100$SpeedP, n=10))+ theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))+theme(legend.title=element_blank(),legend.text=element_blank(), aspect.ratio=1.94,panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"))+geom_hline(yintercept =cut_off[1],color="black")+geom_hline(yintercept =cut_off[2],linetype = "longdash",color="black")+geom_hline(yintercept =cut_off[3],linetype = "longdash",color="black")

###Histogram Head (Refer to Startlethresholdestimation.R for the estimation of the threshold stored in the variable 'cut_off'###

HH100<-ggplot(data=abridgedtableHead100)+geom_histogram(aes(x=Relative2max_Elevation,fill=Closure_Prototroch),breaks=c(0,seq(0.01,1,by=0.1),1),show.legend = TRUE,alpha=3/4,col="black") + labs(x="Max.parapodial angle (a.u.)",y="# Observations")+scale_fill_manual(values=c("forestgreen","magenta")) +theme(legend.title=element_blank(),aspect.ratio =0.5155,panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"))+ scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0),limits = c(0,47))+geom_vline(xintercept =cut_off[1],color="black")+geom_vline(xintercept =cut_off[2],linetype = "longdash",color="black")+geom_vline(xintercept =cut_off[3],linetype = "longdash",color="black")
HH100


###Fig.1H Pygid-scatterplot and histogram####

abridgedtablePygid<-data.frame(TableResults[Pygid,"ProbespeedP"],TableResults[Pygid,"Relative2max_Elevation"],TableResults[Pygid,"Closure_Prototroch"],TableResults[Pygid,"Elev_parapodia"])

names(abridgedtablePygid)<-c("SpeedP","Relative2max_Elevation","Closure_Prototroch","ElevationParapodia")

PlotPygid<-ggplot(data=abridgedtablePygid,aes(x=SpeedP,y=Relative2max_Elevation)) + geom_point(stat="identity",aes(col = factor(abridgedtablePygid$Closure_Prototroch)), size = 3.5,shape=20,alpha=3/4)   


PlotPygid  + labs(x="Max.speed filament (um/ms)",y="Max.parapodial angle (a.u.)")+ scale_color_manual(values = c("forestgreen", "magenta"))+scale_x_continuous(breaks = pretty(abridgedtablePygid$SpeedP, n=10))+ theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))+theme(legend.title=element_blank(),panel.grid.major = element_blank(),aspect.ratio = 1.94, panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"))+geom_hline(yintercept =cut_off[1],color="black")+geom_hline(yintercept =cut_off[2],linetype = "longdash",color="black")+geom_hline(yintercept =cut_off[3],linetype = "longdash",color="black")

##Histogram Pygid

HT<-ggplot(data=abridgedtablePygid)+geom_histogram(aes(x=Relative2max_Elevation,fill=Closure_Prototroch),breaks=c(0,seq(0.01,1,by=0.1),1),show.legend = TRUE,alpha=3/4,col="black") + labs(x="Max.parapodial angle (a.u.)",y="# Observations")+scale_fill_manual(values=c("forestgreen","magenta")) +theme(legend.title=element_blank(),panel.grid.major = element_blank(), aspect.ratio = 0.5155,panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"))+ scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0),limits = c(0,40))+geom_vline(xintercept =cut_off[1],color="black")+geom_vline(xintercept =cut_off[2],linetype = "longdash",color="black")+geom_vline(xintercept =cut_off[3],linetype = "longdash",color="black")
HT

###Fig1I Distancetitration_StackedBarPlot####

SpeedvsResptypeH<-data.frame(TableResults[Head,"RespType"],TableResults[Head,"RoundDist"])
names(SpeedvsResptypeH)<-c("ElevationParapodia","RoundDist")
#Adjusting order factors
SpeedvsResptypeH$ElevationParapodia<-factor(SpeedvsResptypeH$ElevationParapodia,levels=c("ClosureWideE","ClosureLowE","ClosureNoElev","NoClosureWideE","NoResponse"))

BarplotTitration<-100*prop.table(table(SpeedvsResptypeH),2)
par(mar = rep(2, 4))
barplot(BarplotTitration,col=c("black","dark blue","forest green","magenta","white"),xlab = "Max.speed filament (um/ms)",ylab = "%Response type")
legend("topright",fill=c("black","dark blue","forest green","magenta","white"),inset=c(-1.2,0),legend = levels(SpeedvsResptypeH$ElevationParapodia))

###Fig.1J Comparing latency closure and Elevation in Pygid and head 100um####

boxplot(TableResults[Head100LowElev,"LatencyElev"],TableResults[Head100HighElev,"LatencyElev"],TableResults[Head100,"LatencyClosure"],TableResults[Head100,"LCloBody"],TableResults[Pygid,"LatencyClosure"],TableResults[Pygid,"LCloBody"],TableResults[Pygid100HighElev,"LatencyElev"],names =c("LowE","WideE","Prototroch","Bodytroch","Prototroch","Bodytroch ","WAE"),las=2,at=c(1,2,3,4,5.5,6.5,7.5),col = c(rep("grey",7)),ylab="Latency response (ms)",ylim=c(0,270),notch = TRUE,varwidth = TRUE,boxlwd=1,yaxt='n')
axis(side=2, at=seq(0, 270, by=25),las=1,yaxs="i")

####Fig.S1C Barplot proportion responses Head####
speedvsResptypeH100<-data.frame(TableResults[Head100,"RespType"],TableResults[Head100,"ProbespeedP"])
names(speedvsResptypeH100)<-c("ElevationParapodia","ProbespeedP")

bins=seq(0,80,by=5)

speedvsResptypeH100$ElevationParapodia<-factor(speedvsResptypeH100$ElevationParapodia,levels=c("ClosureWideE","ClosureLowE","ClosureNoElev","NoClosureWideE","NoResponse"))

PerPlot<-100*prop.table(table(speedvsResptypeH100$ElevationParapodia,cut(speedvsResptypeH100$ProbespeedP,breaks=bins,labels=as.character(seq(2.5,77.5,by=5)))),2)

barplot(PerPlot,col=c("black","dark blue","forest green","magenta","white"),xlab = "Max.speed filament (um/ms)",ylab = "%Response type")
legend("topright",fill=c("black","dark blue","forest green","magenta","white"),inset=c(0,0),legend = levels(speedvsResptypeH100$ElevationParapodia))

####Fig.S1D Barplot proportion responses Pygid####
speedvsResptypeT100<-data.frame(TableResults[Pygid,"RespType"],TableResults[Pygid,"ProbespeedP"])
names(speedvsResptypeT100)<-c("ElevationParapodia","ProbespeedP")
binsT=seq(0,120,by=20)

speedvsResptypeT100$ElevationParapodia<-factor(speedvsResptypeT100$ElevationParapodia,levels=c("ClosureWideE","ClosureLowE","ClosureNoElev","NoClosureWideE","NoResponse"))


#MiddlevalueTags
PerPlot<-100*prop.table(table(speedvsResptypeT100$ElevationParapodia,cut(speedvsResptypeT100$ProbespeedP,breaks=binsT,labels=as.character(seq(10,110,by=20)))),2)

#Plot
barplot(PerPlot,col=c("black","dark blue","forest green","magenta","white"),xlab = "Max.speed filament (um/ms)",ylab = "%Response type")
legend("topright",fill=c("black","dark blue","forest green","magenta","white"),inset=c(0,0),legend = levels(speedvsResptypeH100$ElevationParapodia))


##FigS1E Plotting speed distribution of Elevation responses#####

#Head-Pygid-High
boxplot(TableResults[Head100LowElev,"DurationRest2Elevation"],TableResults[Head100HighElev,"DurationRest2Elevation"],TableResults[Pygid,"DurationRest2Elevation"],names=c("HeadWide","Pygidium"),las=1,col = "grey",ylab="Duration rest to max. Elevation (ms)",ylim=c(0,900),boxlwd=1,yaxt='n',notch=F,varwidth = T)
axis(side=2, at=seq(0, 900, by=40),las=1)


###Fig.S1F Comparison Proto-Bodytroch Elev-Proto closure diff####

ClosabrH<-data.frame(TableResults[Head100,"LatencyClosure"],TableResults[Head100,"LCloBody"],TableResults[Head100,"TimeperFrame"])
names(ClosabrH)<-c("Proto","Body","Time")
ClosabrH$Time<-round(ClosabrH$Time,digits = 0)
ClosabrH$Time<-as.factor(ClosabrH$Time)
ClosemeltH<-melt(ClosabrH,"Time")

CloseplotH<-ggplot(ClosemeltH,aes(x=variable,y=value,col=Time))+geom_boxplot(notch=T,varwidth=T)+geom_jitter(size=3,alpha=0.3,width = 0.2,height=0)+labs(x="",y="Wide to Closure delay (frames)")+scale_y_continuous(breaks = pretty(ClosemeltH$value,n=10))+guides(col=guide_legend(title="time/frame"))+scale_color_manual(values = c("black", "blue"))
CloseplotH

###FigS1G Delay LR and segment Elevation####

AbrCoordH<-data.frame(TableResults[Head100HighElev,"LRElev1stPa"],TableResults[Head100HighElev,"Coord12"],TableResults[Head100HighElev,"Coord13"],TableResults[Head100HighElev,"TimeperFrame"])
names(AbrCoordH)<-c("Left<->Right","sg1->sg2","sg1->sg3","Time")
AbrCoordH$Time<-round(AbrCoordH$Time,digits = 0)
AbrCoordH$Time<-as.factor(AbrCoordH$Time)
CoordmeltH<-melt(AbrCoordH,"Time")


BoxCoorH<-ggplot(CoordmeltH,aes(x=variable,y = value,col=Time))+geom_jitter(size=1,alpha=0.3,width = 0.2,height=0)+theme(aspect.ratio=0.5,panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"))+labs(x="",y="Parapodial eleveation delay (frames)")+scale_y_continuous(breaks = pretty(CoordmeltH$value,n=10))+guides(col=guide_legend(title="time/frame"))+scale_color_manual(values = c("green", "blue"))
BoxCoorH