### Code Updated on 07 Feb 2023
### In this code we run the the goal is to compare the AUC performance
### The analysis will be performed on four scenarios that involve
### various combinations of training and testing data sets.
### Precisely we will use:

### (1) Four studies for training and testing: (OT, SIM1, SIM2, TT1)
### Data_for_R_four_studies_OT_SIM1_SIM2_TT1_Single_112_cases_Feb_2023.csv

### (2) Three studies for training and testing: (SS, TT1, OT)
### Data_for_R_three_studies_SS_TT1_OT_63_cases_Feb_2023.csv

### (3) Two studies for training and testing: (SST, OT)
### Data_for_R_two_studies_SST_OT_realism_28_cases_Feb_2023.csv

### (4) Two studies for training and testing: (SSO, TT1)
### Data_for_R_two_studies_SSO_TT1_axis_28_cases_Feb_2023.csv

### In each of the above scenarios we will use the AUC as a response
### variable and we will use as fixed effects:
### Method: with 2 levels {rf, lstm}
### Signal: with 7 levels {BR, HR, PP, BR_HR, BR_PP, HR_PP, BR_HR__PP}
### refering to Breathing Rate, Heart Rate, Perinasal Perspiration signals
### and their combinations in pairs and all three.
### Method*Signal interaction
### In this study we will focus only in the scenarios where the testing
### and training study will be different form each other (i.e. we exclude
### all the Cross-Validation cases, which refer to the same study in 
### training and testing, i.e. we exclude the diagonal entries of the matrices)
### The different cases of (training, testing) pairs that are in the 
### non-diagonal matrix cells, will play the role of random effects
### (as on each case we perform multiple measurements with various
## methods and signals)



### This is the list of libraries loaded and needed for the code to run
library(lme4)
library(sjPlot)
library(lsmeans)
library(MASS)
library(lmerTest)

### This is the path where the data are
#setwd(".../.../...")





###########################################################################
### SCENARIO 1
### (1) Four studies for training and testing: (OT, SIM1, SIM2, TT1)
### Data_for_R_four_studies_OT_SIM1_SIM2_TT1_Single_112_cases_Feb_2023.csv
###########################################################################
SClabel<-"Four studies:"
### Here we will load the DATA
tmpD<-read.csv("Data_for_R_four_studies_OT_SIM1_SIM2_TT1_Single_112_cases_Feb_2023.csv",header=T,sep=",")
dim(tmpD)

### Here is the response variable:
AUC0<-tmpD$value
summary(AUC0)

### Here are the categorical variables acting as EXPLANATORY (FIXED EFFECTS) variables
### Method
Method0<-factor(tmpD$Model,levels = c("rf","lstm"))
Method0
table(Method0)
### Arousal Signal
Signal0<-factor(tmpD$Arousal_Signal,
               levels=c("BR","HR","PP","HR_BR","PP_BR","PP_HR","PP_HR_BR"))
Signal0
table(Signal0)

### Cross Validation Indicator ("CV" if the same data set used for Training  
### and Testing or "non_CV" otherwise)
CVind<-factor(tmpD$CV_ind)
CVind
table(CVind)

### Here are the Cases which indicate the pair of (Training, Testing) data sets that
### is used with every different Arousal_Signal and Method. Thus for these cases we 
### have multiple measurements and so they will play the role of RANDOM EFFECTS
C0<-factor(tmpD$Case,
          levels=c("C1","C2","C3","C4","C5","C6","C7","C8","C9",
                   "C10","C11","C12","C13","C14","C15","C16"))
table(C0)

### These are the non-CV cases, which we will study in this paper,
### i.e. we focus only on the non_CV cases where the training 
### and testing data sets are different
AUC<-AUC0[CVind=="non_CV"]
summary(AUC)
Method<-Method0[CVind=="non_CV"]
table(Method)
Signal<-Signal0[CVind=="non_CV"]
table(Signal)
C<-C0[CVind=="non_CV"]
table(C)

quartz()
par(mfrow=c(1,2),mar=c(4.5,3.2,2,1.5),mgp=c(3.2,0.6,0),oma=c(0.5,1.5,1.5,0.5))
boxplot(AUC~Method,col=c("blue","red"),las=2,xlab = "",ylab = "")
title(xlab = "Method", line = 3.5,cex.lab=1.3) 
title(ylab = "non-CV AUC", line = 2.2,cex.lab=1.3) 
boxplot(AUC~Signal,las=2,xlab = "",ylab = "")
title(xlab = "Signal", line = 3.5,cex.lab=1.3) 
title(ylab = "non-CV AUC", line = 2.2,cex.lab=1.3) 
title(paste(SClabel,"non-CV AUC vs Method and Signal"),outer=TRUE, cex.main=1.5,col.main="blue", line=-1)
quartz()
par(mfrow=c(1,1),mar=c(6.5,3.2,2,1.5),mgp=c(3.2,0.6,0),oma=c(0.5,1.5,1.5,0.5))
boxplot(AUC~Method*Signal,col=rep(c("blue","red"),7),las=2,xlab = "",ylab = "")
title(xlab = "Method:Signal", line = 5.5,cex.lab=1.3) 
title(ylab = "non-CV AUC", line = 2.2,cex.lab=1.3) 
title(paste(SClabel,"non-CV AUC vs Method*Signal"),outer=TRUE, cex.main=1.5,col.main="blue", line=-1)


### Here we will run the MIXED EFFECTS MODEL (MEM) on Method, Signal
### and Method*Signal interaction(fixed effects) while the C (i.e. Cases) 
### will be treated as random effect
fm00<-lmer(AUC ~ 1 + Method*Signal + (1|C))

### Here is the model's output
print("____________________________________________________")
print("Mixed Effects Model (MEM)")
print(summary(fm00))
print(anova(fm00))

quartz()
plot_model(fm00,"est",title = "non-CV AUC", show.values = TRUE, value.offset = .3,xlab="")#+font_size(labels.x=8,labels.y=8,title = 8,axis_title.x =8)
quartz()
plot_model(fm00,"re", show.values = TRUE, value.offset = .3,xlab="")+font_size(labels.x=8,labels.y=8,title = 8,axis_title.x =8)
quartz()
plot_model(fm00,"pred",title = "non-CV AUC")$Method#+font_size(labels.x=18,labels.y=18,title = 18,axis_title.x =18)
quartz()
plot_model(fm00,"pred",title = "non-CV AUC")$Signal#+font_size(labels.x=18,labels.y=18,title = 18,axis_title.x =18)
quartz()
plot_model(fm00,"int",title = "non-CV AUC")#+font_size(labels.x=18,labels.y=18,title = 18,axis_title.x =18)


### Tukey's Test
LSMmethod<-lsmeans(fm00, pairwise~Method, adjust="tukey")
LSMmethod
quartz()
plot(LSMmethod)
LSSignal<-lsmeans(fm00, pairwise~Signal, adjust="tukey")
LSSignal
quartz()
plot(LSSignal)
LSInteraction<-lsmeans(fm00, pairwise~Method*Signal, adjust="tukey")
LSInteraction
quartz()
plot(LSInteraction)








###########################################################################
### SCENARIO 2
### (2) Three studies for training and testing: (SS, TT1, OT)
### Data_for_R_three_studies_SS_TT1_OT_63_cases_Feb_2023.csv
###########################################################################
SClabel<-"Three studies:"
### Here we will load the DATA
tmpD<-read.csv("Data_for_R_three_studies_SS_TT1_OT_63_cases_Feb_2023.csv",header=T,sep=",")
dim(tmpD)

### Here is the response variable:
AUC0<-tmpD$value
summary(AUC0)

### Here are the categorical variables acting as EXPLANATORY (FIXED EFFECTS) variables
### Method
Method0<-factor(tmpD$Model,levels = c("rf","lstm"))
Method0
table(Method0)
### Arousal Signal
Signal0<-factor(tmpD$Arousal_Signal,
                levels=c("BR","HR","PP","HR_BR","PP_BR","PP_HR","PP_HR_BR"))
Signal0
table(Signal0)

### Cross Validation Indicator ("CV" if the same data set used for Training  
### and Testing or "non_CV" otherwise)
CVind<-factor(tmpD$CV_ind)
CVind
table(CVind)

### Here are the Cases which indicate the pair of (Training, Testing) data sets that
### is used with every different Arousal_Signal and Method. Thus for these cases we 
### have multiple measurements and so they will play the role of RANDOM EFFECTS
C0<-factor(tmpD$Case,
           levels=c("C1","C2","C3","C4","C5","C6","C7","C8","C9"))
table(C0)

### These are the non-CV cases, which we will study in this paper,
### i.e. we focus only on the non_CV cases where the training 
### and testing data sets are different
AUC<-AUC0[CVind=="non_CV"]
summary(AUC)
Method<-Method0[CVind=="non_CV"]
table(Method)
Signal<-Signal0[CVind=="non_CV"]
table(Signal)
C<-C0[CVind=="non_CV"]
table(C)

quartz()
par(mfrow=c(1,2),mar=c(4.5,3.2,2,1.5),mgp=c(3.2,0.6,0),oma=c(0.5,1.5,1.5,0.5))
boxplot(AUC~Method,col=c("blue","red"),las=2,xlab = "",ylab = "")
title(xlab = "Method", line = 3.5,cex.lab=1.3) 
title(ylab = "non-CV AUC", line = 2.2,cex.lab=1.3) 
boxplot(AUC~Signal,las=2,xlab = "",ylab = "")
title(xlab = "Signal", line = 3.5,cex.lab=1.3) 
title(ylab = "non-CV AUC", line = 2.2,cex.lab=1.3) 
title(paste(SClabel,"non-CV AUC vs Method and Signal"),outer=TRUE, cex.main=1.5,col.main="blue", line=-1)
quartz()
par(mfrow=c(1,1),mar=c(6.5,3.2,2,1.5),mgp=c(3.2,0.6,0),oma=c(0.5,1.5,1.5,0.5))
boxplot(AUC~Method*Signal,col=rep(c("blue","red"),7),las=2,xlab = "",ylab = "")
title(xlab = "Method:Signal", line = 5.5,cex.lab=1.3) 
title(ylab = "non-CV AUC", line = 2.2,cex.lab=1.3) 
title(paste(SClabel,"non-CV AUC vs Method*Signal"),outer=TRUE, cex.main=1.5,col.main="blue", line=-1)


### Here we will run the MIXED EFFECTS MODEL (MEM) on Method, Signal
### and Method*Signal interaction(fixed effects) while the C (i.e. Cases) 
### will be treated as random effect
fm00<-lmer(AUC ~ 1 + Method*Signal + (1|C))

### Here is the model's output
print("____________________________________________________")
print("Mixed Effects Model (MEM)")
print(summary(fm00))
print(anova(fm00))


quartz()
plot_model(fm00,"est",title = "non-CV AUC", show.values = TRUE, value.offset = .3,xlab="")#+font_size(labels.x=8,labels.y=8,title = 8,axis_title.x =8)
quartz()
plot_model(fm00,"re", show.values = TRUE, value.offset = .3,xlab="")+font_size(labels.x=8,labels.y=8,title = 8,axis_title.x =8)
quartz()
plot_model(fm00,"pred",title = "non-CV AUC")$Method#+font_size(labels.x=18,labels.y=18,title = 18,axis_title.x =18)
quartz()
plot_model(fm00,"pred",title = "non-CV AUC")$Signal#+font_size(labels.x=18,labels.y=18,title = 18,axis_title.x =18)
quartz()
plot_model(fm00,"int",title = "non-CV AUC")#+font_size(labels.x=18,labels.y=18,title = 18,axis_title.x =18)

### Tukey's Test
LSMmethod<-lsmeans(fm00, pairwise~Method, adjust="tukey")
LSMmethod
quartz()
plot(LSMmethod)
LSSignal<-lsmeans(fm00, pairwise~Signal, adjust="tukey")
LSSignal
quartz()
plot(LSSignal)
LSInteraction<-lsmeans(fm00, pairwise~Method*Signal, adjust="tukey")
LSInteraction
quartz()
plot(LSInteraction)








###########################################################################
### SCENARIO 3
### (3) Two studies for training and testing: (SST, OT)
### Data_for_R_two_studies_SST_OT_realism_28_cases_Feb_2023.csv
###########################################################################
SClabel<-"Two studies (realism):"
### Here we will load the DATA
tmpD<-read.csv("Data_for_R_two_studies_SST_OT_realism_28_cases_Feb_2023.csv",header=T,sep=",")
dim(tmpD)

### Here is the response variable:
AUC0<-tmpD$value
summary(AUC0)

### Here are the categorical variables acting as EXPLANATORY (FIXED EFFECTS) variables
### Method
Method0<-factor(tmpD$Model,levels = c("rf","lstm"))
Method0
table(Method0)
### Arousal Signal
Signal0<-factor(tmpD$Arousal_Signal,
                levels=c("BR","HR","PP","HR_BR","PP_BR","PP_HR","PP_HR_BR"))
Signal0
table(Signal0)

### Cross Validation Indicator ("CV" if the same data set used for Training  
### and Testing or "non_CV" otherwise)
CVind<-factor(tmpD$CV_ind)
CVind
table(CVind)

### Here are the Cases which indicate the pair of (Training, Testing) data sets that
### is used with every different Arousal_Signal and Method. Thus for these cases we 
### have multiple measurements and so they will play the role of RANDOM EFFECTS
C0<-factor(tmpD$Case,levels=c("C1","C2","C3","C4"))
table(C0)

### These are the non-CV cases, which we will study in this paper,
### i.e. we focus only on the non_CV cases where the training 
### and testing data sets are different
AUC<-AUC0[CVind=="non_CV"]
summary(AUC)
Method<-Method0[CVind=="non_CV"]
table(Method)
Signal<-Signal0[CVind=="non_CV"]
table(Signal)
C<-C0[CVind=="non_CV"]
table(C)

quartz()
par(mfrow=c(1,2),mar=c(4.5,3.2,2,1.5),mgp=c(3.2,0.6,0),oma=c(0.5,1.5,1.5,0.5))
boxplot(AUC~Method,col=c("blue","red"),las=2,xlab = "",ylab = "")
title(xlab = "Method", line = 3.5,cex.lab=1.3) 
title(ylab = "non-CV AUC", line = 2.2,cex.lab=1.3) 
boxplot(AUC~Signal,las=2,xlab = "",ylab = "")
title(xlab = "Signal", line = 3.5,cex.lab=1.3) 
title(ylab = "non-CV AUC", line = 2.2,cex.lab=1.3) 
title(paste(SClabel,"non-CV AUC vs Method and Signal"),outer=TRUE, cex.main=1.5,col.main="blue", line=-1)
quartz()
par(mfrow=c(1,1),mar=c(6.5,3.2,2,1.5),mgp=c(3.2,0.6,0),oma=c(0.5,1.5,1.5,0.5))
boxplot(AUC~Method*Signal,col=rep(c("blue","red"),7),las=2,xlab = "",ylab = "")
title(xlab = "Method:Signal", line = 5.5,cex.lab=1.3) 
title(ylab = "non-CV AUC", line = 2.2,cex.lab=1.3) 
title(paste(SClabel,"non-CV AUC vs Method*Signal"),outer=TRUE, cex.main=1.5,col.main="blue", line=-1)


### Here we will run the MIXED EFFECTS MODEL (MEM) on Method, Signal
### and Method*Signal interaction(fixed effects) while the C (i.e. Cases) 
### will be treated as random effect
fm00<-lmer(AUC ~ 1 + Method*Signal + (1|C))


### Here is the model's output
print("____________________________________________________")
print("Mixed Effects Model (MEM)")
print(summary(fm00))
print(anova(fm00))

quartz()
plot_model(fm00,"est",title = "non-CV AUC", show.values = TRUE, value.offset = .3,xlab="")#+font_size(labels.x=8,labels.y=8,title = 8,axis_title.x =8)
quartz()
plot_model(fm00,"re", show.values = TRUE, value.offset = .3,xlab="")+font_size(labels.x=8,labels.y=8,title = 8,axis_title.x =8)
quartz()
plot_model(fm00,"pred",title = "non-CV AUC")$Method#+font_size(labels.x=18,labels.y=18,title = 18,axis_title.x =18)
quartz()
plot_model(fm00,"pred",title = "non-CV AUC")$Signal#+font_size(labels.x=18,labels.y=18,title = 18,axis_title.x =18)
quartz()
plot_model(fm00,"int",title = "non-CV AUC")#+font_size(labels.x=18,labels.y=18,title = 18,axis_title.x =18)

### Tukey's Test
LSMmethod<-lsmeans(fm00, pairwise~Method, adjust="tukey")
LSMmethod
quartz()
plot(LSMmethod)
LSSignal<-lsmeans(fm00, pairwise~Signal, adjust="tukey")
LSSignal
quartz()
plot(LSSignal)
LSInteraction<-lsmeans(fm00, pairwise~Method*Signal, adjust="tukey")
LSInteraction
quartz()
plot(LSInteraction)






###########################################################################
### SCENARIO 4
### (4) Two studies for training and testing: (SSO, TT1)
### Data_for_R_two_studies_SSO_TT1_axis_28_cases_Feb_2023.csv
###########################################################################
SClabel<-"Two studies (axis):"
### Here we will load the DATA
tmpD<-read.csv("Data_for_R_two_studies_SSO_TT1_axis_28_cases_Feb_2023.csv",header=T,sep=",")
dim(tmpD)

### Here is the response variable:
AUC0<-tmpD$value
summary(AUC0)

### Here are the categorical variables acting as EXPLANATORY (FIXED EFFECTS) variables
### Method
Method0<-factor(tmpD$Model,levels = c("rf","lstm"))
Method0
table(Method0)
### Arousal Signal
Signal0<-factor(tmpD$Arousal_Signal,
                levels=c("BR","HR","PP","HR_BR","PP_BR","PP_HR","PP_HR_BR"))
Signal0
table(Signal0)

### Cross Validation Indicator ("CV" if the same data set used for Training  
### and Testing or "non_CV" otherwise)
CVind<-factor(tmpD$CV_ind)
CVind
table(CVind)

### Here are the Cases which indicate the pair of (Training, Testing) data sets that
### is used with every different Arousal_Signal and Method. Thus for these cases we 
### have multiple measurements and so they will play the role of RANDOM EFFECTS
C0<-factor(tmpD$Case,levels=c("C1","C2","C3","C4"))
table(C0)

### These are the non-CV cases, which we will study in this paper,
### i.e. we focus only on the non_CV cases where the training 
### and testing data sets are different
AUC<-AUC0[CVind=="non_CV"]
summary(AUC)
Method<-Method0[CVind=="non_CV"]
table(Method)
Signal<-Signal0[CVind=="non_CV"]
table(Signal)
C<-C0[CVind=="non_CV"]
table(C)

quartz()
par(mfrow=c(1,2),mar=c(4.5,3.2,2,1.5),mgp=c(3.2,0.6,0),oma=c(0.5,1.5,1.5,0.5))
boxplot(AUC~Method,col=c("blue","red"),las=2,xlab = "",ylab = "")
title(xlab = "Method", line = 3.5,cex.lab=1.3) 
title(ylab = "non-CV AUC", line = 2.2,cex.lab=1.3) 
boxplot(AUC~Signal,las=2,xlab = "",ylab = "")
title(xlab = "Signal", line = 3.5,cex.lab=1.3) 
title(ylab = "non-CV AUC", line = 2.2,cex.lab=1.3) 
title(paste(SClabel,"non-CV AUC vs Method and Signal"),outer=TRUE, cex.main=1.5,col.main="blue", line=-1)
quartz()
par(mfrow=c(1,1),mar=c(6.5,3.2,2,1.5),mgp=c(3.2,0.6,0),oma=c(0.5,1.5,1.5,0.5))
boxplot(AUC~Method*Signal,col=rep(c("blue","red"),7),las=2,xlab = "",ylab = "")
title(xlab = "Method:Signal", line = 5.5,cex.lab=1.3) 
title(ylab = "non-CV AUC", line = 2.2,cex.lab=1.3) 
title(paste(SClabel,"non-CV AUC vs Method*Signal"),outer=TRUE, cex.main=1.5,col.main="blue", line=-1)


### Here we will run the MIXED EFFECTS MODEL (MEM) on Method, Signal
### and Method*Signal interaction(fixed effects) while the C (i.e. Cases) 
### will be treated as random effect
fm00<-lmer(AUC ~ 1 + Method*Signal + (1|C))


### Here is the model's output
print("____________________________________________________")
print("Mixed Effects Model (MEM)")
print(summary(fm00))
print(anova(fm00))


quartz()
plot_model(fm00,"est",title = "non-CV AUC", show.values = TRUE, value.offset = .3,xlab="")#+font_size(labels.x=8,labels.y=8,title = 8,axis_title.x =8)
quartz()
plot_model(fm00,"re", show.values = TRUE, value.offset = .3,xlab="")+font_size(labels.x=8,labels.y=8,title = 8,axis_title.x =8)
quartz()
plot_model(fm00,"pred",title = "non-CV AUC")$Method#+font_size(labels.x=18,labels.y=18,title = 18,axis_title.x =18)
quartz()
plot_model(fm00,"pred",title = "non-CV AUC")$Signal#+font_size(labels.x=18,labels.y=18,title = 18,axis_title.x =18)
quartz()
plot_model(fm00,"int",title = "non-CV AUC")#+font_size(labels.x=18,labels.y=18,title = 18,axis_title.x =18)

### Tukey's Test
LSMmethod<-lsmeans(fm00, pairwise~Method, adjust="tukey")
LSMmethod
quartz()
plot(LSMmethod)
LSSignal<-lsmeans(fm00, pairwise~Signal, adjust="tukey")
LSSignal
quartz()
plot(LSSignal)
LSInteraction<-lsmeans(fm00, pairwise~Method*Signal, adjust="tukey")
LSInteraction
quartz()
plot(LSInteraction)








