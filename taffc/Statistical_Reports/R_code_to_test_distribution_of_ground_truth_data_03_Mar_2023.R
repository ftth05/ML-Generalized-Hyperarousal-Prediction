### Code Updated on 08 Feb 2023
### In this code we run the data that provided by Shaila  where the 
### goal is to examine the ground truth data and decide whether for each 
### signal and study we have a satisfactory fit to a normal curve or not

### This is the path where the data are
#setwd("/.../")
### Here we will load all the DATA
tmpD<-read.csv("ground_truth_datα.csv",header=T,sep=",")
dim(tmpD)

Study<-factor(tmpD$Study,levels=c("sim1","sim2","tt1","OT","DS","SS","SST","SSO","SSTO"))
table(Study)
PPall<-tmpD$PP_Normalized	
HRall<-tmpD$HR_Normalized
BRall<-tmpD$BR_Normalized

D<-cbind(PPall,HRall,BRall)
dim(D)

rm(tmpD)

StudyLabels<-c("sim1","sim2","tt1","OT","DS","SS","SST","SSO","SSTO")


pdf(file = "PP_HR_BR_ground_truthing_conformance_to_normality_in_9_studies_03_Mar_2023.pdf",
    width = 12, # The width of the plot in inches
    height = 6) # The height of the plot in inches
for (k in StudyLabels){
  StudyInd<-Study==k
  tmpPP<-PPall[StudyInd]
  tmpHR<-HRall[StudyInd]
  tmpBR<-BRall[StudyInd]
  ### Here we will plot the PP
  par(mfrow=c(1,2),mar=c(2.5,2.5,3.5,2),mgp=c(1.2,0.5,0),oma=c(0.5,0.5,0.5,0.5))
  hist(tmpPP,freq=F,xlab="",ylab="",main="Histogram") 
  lines(density(tmpPP),col="red",lty=1,lwd=2)
  qqnorm(tmpPP,main="Normal Probability Plot")
  qqline(tmpPP,col="red",lty=1,lwd=2)
  #abline(v=c(-0.5,0.5),lty=3)
  abline(h=c(mean(tmpPP)-0.5*sd(tmpPP),mean(tmpPP)+0.5*sd(tmpPP)),lty=3)
  title(paste(k,"- PP" ),outer=TRUE, cex.main=2,col.main="blue", line=-2)
  ### Here we will plot the HR
  par(mfrow=c(1,2),mar=c(2.5,2.5,3.5,2),mgp=c(1.2,0.5,0),oma=c(0.5,0.5,0.5,0.5))
  hist(tmpHR,freq=F,xlab="",ylab="",main="Histogram") 
  lines(density(tmpHR),col="red",lty=1,lwd=2)
  qqnorm(tmpHR,main="Normal Probability Plot")
  qqline(tmpHR,col="red",lty=1,lwd=2)
  #abline(v=c(-0.5,0.5),lty=3)
  abline(h=c(mean(tmpHR)-0.5*sd(tmpHR),mean(tmpHR)+0.5*sd(tmpHR)),lty=3)
  title(paste(k,"- HR" ),outer=TRUE, cex.main=2,col.main="blue", line=-2)
  ### Here we will plot the BR
  par(mfrow=c(1,2),mar=c(2.5,2.5,3.5,2),mgp=c(1.2,0.5,0),oma=c(0.5,0.5,0.5,0.5))
  hist(tmpBR,freq=F,xlab="",ylab="",main="Histogram") 
  lines(density(tmpBR),col="red",lty=1,lwd=2)
  qqnorm(tmpBR,main="Normal Probability Plot")
  qqline(tmpBR,col="red",lty=1,lwd=2)
  #abline(v=c(-0.5,0.5),lty=3)
  abline(h=c(mean(tmpBR)-0.5*sd(tmpBR),mean(tmpBR)+0.5*sd(tmpBR)),lty=3)
  title(paste(k,"- BR" ),outer=TRUE, cex.main=2,col.main="blue", line=-2)
}
dev.off() ### end the pdf creation file


