### Code Updated on 08 Feb 2023
### In this code we run the data that provided by Shaila  where the 
### goal is to examine the ground truth data and decide whether for each 
### signal and study we have a satisfactory fit to a normal curve or not

### This is the path where the data are
#setwd("/.../")
### Here we will load all the DATA

library(dplyr)
library(tidyr)

tmpD<-read.csv("ground_truth_data.csv",header=T,sep=",")
dim(tmpD)



Study<-factor(tmpD$Study,levels=c("sim1","sim2","tt1","OT","DS","SS","SST","SSO","SSTO"))

table(Study)


# unique(tmpD$Study)
# factor(tmpD$Study)
# table(Study)
# PPall<-tmpD$PP_Mean	
# HRall<-tmpD$HR_Mean
# BRall<-tmpD$BR_Mean
# 
# levels1=c("SIM1","SIM2","TT1","OFFICE_TASKS","DEADLINE_STUDY",
#           "SIM1___SIM2","SIM1___SIM2___TT1","SIM1___SIM2___OFFICE_TASKS","SIM1___SIM2___TT1___OFFICE_TASKS")
# 
# levels=c("sim1","sim2","tt1","OT","DS","SS","SST","SSO","SSTO")
# 
# for (i in 1:9) {
#   tmpD$Study[tmpD$Study == levels1[i]] = levels[i]
# }


#tmpD$PP_Normalized = log(tmpD$PP_Mean) - mean(log(tmpD$PP_Ground_Truth))

PPall<-tmpD$PP_Normalized	
HRall<-tmpD$HR_Normalized
BRall<-tmpD$BR_Normalized


D<-cbind(PPall,HRall,BRall)
dim(D)

rm(tmpD)

StudyLabels<-c("sim1","sim2","tt1","OT")#,"DS","SS","SST","SSO","SSTO")


pdf(file = "FK_PP_HR_BR_ground_truthing_conformance_to_normality_in_4_V2.pdf",
    width = 12, # The width of the plot in inches
    height = 12) # The height of the plot in inches

quartz()
par(mfrow=c(4,6),mar=c(2.5,2.5,3.5,2),mgp=c(1.2,0.5,0),oma=c(0.5,0.5,0.5,0.5))

for (k in StudyLabels){
  print(k)
  StudyInd<-Study==k
  tmpPP<-PPall[StudyInd]
  tmpHR<-HRall[StudyInd]
  tmpBR<-BRall[StudyInd]
  ### Here we will plot the PP

  hist(tmpPP,freq=F,xlab="",ylab="",main="" ,col ="green")
  lines(density(tmpPP),col="black",lty=1,lwd=1)
  qqnorm(tmpPP,main="", prob=TRUE, cex.lab=0.75, cex.axis=0.75, cex.main=1.5, cex.sub=1)
  qqline(tmpPP,col="red",lty=1,lwd=1)
  #abline(v=c(-0.5,0.5),lty=3)
  abline(h=c(mean(tmpPP)-0.5*sd(tmpPP),mean(tmpPP)+0.5*sd(tmpPP)),lty=3)
  #title(paste(k,"- PP" ),outer=TRUE, cex.main=2,col.main="blue", line=0)

  ### Here we will plot the HR
  #par(mfrow=c(1,2),mar=c(2.5,2.5,3.5,2),mgp=c(1.2,0.5,0),oma=c(0.5,0.5,0.5,0.5))
  hist(tmpHR,freq=F,xlab="",ylab="",main="",col ="green")
  lines(density(tmpHR),col="black",lty=1,lwd=1)
  qqnorm(tmpHR,main="", prob=TRUE, cex.lab=0.75, cex.axis=0.75, cex.main=1.5, cex.sub=1)
  qqline(tmpHR,col="red",lty=1,lwd=1)
  #abline(v=c(-0.5,0.5),lty=3)
  abline(h=c(mean(tmpHR)-0.5*sd(tmpHR),mean(tmpHR)+0.5*sd(tmpHR)),lty=3)
  #title(paste(k,"- HR" ),outer=TRUE, cex.main=2,col.main="blue", line=-2)

  ### Here we will plot the BR
  #par(mfrow=c(1,2),mar=c(2.5,2.5,3.5,2),mgp=c(1.2,0.5,0),oma=c(0.5,0.5,0.5,0.5))
  hist(tmpBR,freq=F,xlab="",ylab="",main="",col ="green")
  lines(density(tmpBR),col="black",lty=1,lwd=1)
  qqnorm(tmpBR,main="", prob=TRUE, cex.lab=0.75, cex.axis=0.75, cex.main=1.5, cex.sub=1)
  qqline(tmpBR,col="red",lty=1,lwd=1)
  #abline(v=c(-0.5,0.5),lty=3)
  abline(h=c(mean(tmpBR)-0.5*sd(tmpBR),mean(tmpBR)+0.5*sd(tmpBR)),lty=3)
  #title(paste(k,"- BR" ),outer=TRUE, cex.main=2,col.main="blue", line=-4)
}
mtext(paste("PP" ),outer=TRUE, cex.main=2,col.main="blue", line=-2 , at = 0.15)
mtext(paste("HR" ),outer=TRUE, cex.main=2,col.main="blue", line=-2 , at = 0.48)
mtext(paste("BR" ),outer=TRUE, cex.main=2,col.main="blue", line=-2 , at = 0.81)


mtext(paste("S1" ),outer=TRUE, cex.main=2,col.main="blue", line=-1, side =2, at = 0.85)
mtext(paste("S2" ),outer=TRUE, cex.main=2,col.main="blue", line=-1, side =2, at = 0.6)
mtext(paste("R" ),outer=TRUE, cex.main=2,col.main="blue", line=-1, side =2, at = 0.37)
mtext(paste("O" ),outer=TRUE, cex.main=2,col.main="blue", line=-1, side =2, at = 0.13)

dev.off() ### end the pdf creation file




fix xlims for DeltaPP, DeltaHR, DeltaBR.




# 
# # PP
# par(mfrow=c(4,2),mar=c(2.5,2.5,3.5,2),mgp=c(1.2,0.5,0),oma=c(0.5,0.5,0.5,0.5))
# title(paste("PP" ),outer=TRUE, cex.main=2,col.main="blue", line=0)
# for (k in StudyLabels){
#   print(k)
#   StudyInd<-Study==k
#   tmpPP<-PPall[StudyInd]
# 
#   ### Here we will plot the PP
#   
#   hist(tmpPP,freq=F,xlab="",ylab="",main="") 
#   lines(density(tmpPP),col="red",lty=1,lwd=1)
#   qqnorm(tmpPP,main="")
#   qqline(tmpPP,col="red",lty=1,lwd=1)
#   #abline(v=c(-0.5,0.5),lty=3)
#   abline(h=c(mean(tmpPP)-0.5*sd(tmpPP),mean(tmpPP)+0.5*sd(tmpPP)),lty=3)
#   #title(paste(k,"- PP" ),outer=TRUE, cex.main=2,col.main="blue", line=-2)
# }
# 
# 
# # HR
# par(mfrow=c(4,2),mar=c(2.5,2.5,3.5,2),mgp=c(1.2,0.5,0),oma=c(0.5,0.5,0.5,0.5))
# title(paste("HR" ),outer=TRUE, cex.main=2,col.main="blue", line=-2)
# for (k in StudyLabels){
#   print(k)
#   StudyInd<-Study==k
# 
#   tmpHR<-HRall[StudyInd]
# 
#   
#   ### Here we will plot the HR
#   #par(mfrow=c(1,2),mar=c(2.5,2.5,3.5,2),mgp=c(1.2,0.5,0),oma=c(0.5,0.5,0.5,0.5))
#   hist(tmpHR,freq=F,xlab="",ylab="",main="") 
#   lines(density(tmpHR),col="red",lty=1,lwd=1)
#   qqnorm(tmpHR,main="")
#   qqline(tmpHR,col="red",lty=1,lwd=1)
#   #abline(v=c(-0.5,0.5),lty=3)
#   abline(h=c(mean(tmpHR)-0.5*sd(tmpHR),mean(tmpHR)+0.5*sd(tmpHR)),lty=3)
# 
#   
# }
# 
# 
# # BR
# par(mfrow=c(4,2),mar=c(2.5,2.5,3.5,2),mgp=c(1.2,0.5,0),oma=c(0.5,0.5,0.5,0.5))
# title(paste("BR" ),outer=TRUE, cex.main=2,col.main="blue", line=-4)
# for (k in StudyLabels){
#   print(k)
#   StudyInd<-Study==k
#   tmpBR<-BRall[StudyInd]
# 
#   ### Here we will plot the BR
#   #par(mfrow=c(1,2),mar=c(2.5,2.5,3.5,2),mgp=c(1.2,0.5,0),oma=c(0.5,0.5,0.5,0.5))
#   hist(tmpBR,freq=F,xlab="",ylab="",main="") 
#   lines(density(tmpBR),col="red",lty=1,lwd=1)
#   qqnorm(tmpBR,main="")
#   qqline(tmpBR,col="red",lty=1,lwd=1)
#   #abline(v=c(-0.5,0.5),lty=3)
#   abline(h=c(mean(tmpBR)-0.5*sd(tmpBR),mean(tmpBR)+0.5*sd(tmpBR)),lty=3)
#   
# }
# 
# 
# 
# 
# dev.off() ### end the pdf creation file


