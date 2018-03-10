
#Written by Luis Bezares in August,2017 strongly based on http://marcchoisy.free.fr/fmm/index.html
##Purpose:Estimate the threshold value that splits the bimodal population of response profiles using  finite mixture model.
##Publication: Bezares-Calderon et al, 2018. Calculations of this scrpt were applied in the plots shown in Figure 1G #####

###Installing the necessary packages####
install.packages("devtools")
devtools::install_github("choisy/cutoff")
install.packages('Rcpp')
install.packages('RInside')
install.packages('bbmle')
library(cutoff)
library(RInside)
library(Rcpp)
library(bbmle)
require(cutoff)
require(RInside)
require(Rcpp)
require(bbmle)

##NOTE: This script can only work after loading the data from the script Fig1-S1_Analysis_startledataWT_FinalreducedForGithub_2#####

##Making a subgroup of head-stimulated animals from 100um away without 0 elevation values####

AbHeCa1<-which(abridgedtableHead100$Relative2max_extension >0 )

AbHistVals<-abridgedtableHead100[AbHeCa1,"Relative2max_extension"]
length(AbHistVals)
range(AbHistVals)

###Generating histogram with underlying density distribution####

hist(AbHistVals,c(0,seq(0.01,1,by=0.1),1),F,xlab="Frac.Max.Angle",ylab="density",xlim=c(0,1),ylim=c(0,5.5),main=NULL,col="grey")
lines(density(AbHistVals),lwd=1.5,col="blue")  

###Estimating the parameters of the Finite Mixture Model using the actual distribution.####

(HiVal_out <- em(AbHistVals,"normal","normal"))

###Estimating the 95% confidence interval####

confint(HiVal_out,nb=10,level=.95)

###Plotting the modeled distributions under the FMM####

hist(AbHistVals,c(0,seq(0.01,1,by=0.1),1),F,xlab="Frac.Max.Angle",ylab="Density",xlim=c(0,1),ylim=c(0,5.5),main=NULL,col="grey")
lines(HiVal_out,lwd=1.5,col="red")

###Identifying a cutoff value####

(cut_off<-cutoff(HiVal_out))

###Drawing the estimate and the confidence intervals on the plot.####

polygon(c(cut_off[-1],rev(cut_off[-1])),c(0,0,6,6),col=rgb(0,0,1,.2),border=NA)
abline(v=cut_off[-1],lty=2,col="blue")
abline(v=cut_off[1],col="blue")
