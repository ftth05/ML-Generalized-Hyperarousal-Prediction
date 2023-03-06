### Code Updated on 08 Feb 2023
### In this code we run the data that provided by Shaila  where the 
### goal is to examine the ground truth data and decide whether for each 
### signal and study we have a satisfactory fit to a normal curve or not

### This is the path where the data are
#setwd("/.../")
### Here we will load all the DATA

library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)
#install.packages("extrafont")
library(extrafont) 

setwd("/Users/f3tth/Documents/GitHub/ML-Generalized-Hyperarousal-Prediction/taffc/Statistical_Reports/Fkiran")
tmpD<-read.csv("ground_truth_data.csv",header=T,sep=",")
dim(tmpD)



Study<-factor(tmpD$Study,levels=c("sim1","sim2","tt1","OT","DS","SS","SST","SSO","SSTO"))

table(Study)



PPall<-tmpD$PP_Normalized	
HRall<-tmpD$HR_Normalized
BRall<-tmpD$BR_Normalized


D<-cbind(PPall,HRall,BRall)
D<- as.data.frame(D)
dim(D)

rm(tmpD)

StudyLabels<-c("sim1","sim2","tt1","OT")#,"DS","SS","SST","SSO","SSTO")


pdf(file = "FK_PP_HR_BR_ground_truthing_conformance_to_normality_in_4_V3.pdf",
    width = 12, # The width of the plot in inches
    height = 12) # The height of the plot in inches

# S1
  tmpPP.s1<-tibble(value =PPall[Study=="sim1"])
  tmpHR.s1<-tibble(value =HRall[Study=="sim1"])
  tmpBR.s1<-tibble(value =BRall[Study=="sim1"])
  
# https://tex.stackexchange.com/questions/283472/using-latex-font-in-r-computer-modern
  
  #quartz()
   s1p1<- ggplot(tmpPP.s1,aes(x=value, y = ..density..))+
    geom_histogram(col=I("green"),fill = "green") + xlim(-1.5,1.5)+
    geom_density(alpha = .5)+    ylab("S1") + xlab("")+  theme_bw()+
     theme(plot.title = element_text(hjust = 0.5), # center the plot title
           axis.title.x=element_blank(),
           axis.text.x=element_blank(),
           axis.ticks.x=element_blank(),
           axis.title.y = element_text(size=28, 
                                       angle=0,
                                       vjust=0.5, 
                                       face='bold.italic'),
                                       #family = 'Comic Sans MS'),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank())  
   
   s1p2<-ggplot(tmpPP.s1, aes(sample=value))+ 
     ylab("") + xlab("")+  theme_bw()+
     stat_qq() +stat_qq_line(colour = "red") +
     theme(plot.title = element_text(hjust = 0.5), # center the plot title
           axis.title.x=element_blank(),
           #axis.text.x=element_blank(),
           #axis.ticks.x=element_blank(),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank())
   
   
   s1p3<- ggplot(tmpHR.s1,aes(x=value, y = ..density..))+
     geom_histogram(col=I("green"),fill = "green") + xlim(-40,40)+
     geom_density(alpha = .5)+    ylab("") + xlab("")+  theme_bw()+
     theme(plot.title = element_text(hjust = 0.5), # center the plot title
           axis.title.x=element_blank(),
           axis.text.x=element_blank(),
           axis.ticks.x=element_blank(),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank()) 
   
   s1p4<-ggplot(tmpHR.s1, aes(sample=value))+
     ylab("") + xlab("")+  theme_bw()+
     stat_qq() +stat_qq_line(colour = "red") +
     theme(plot.title = element_text(hjust = 0.5), # center the plot title
           axis.title.x=element_blank(),
           #axis.text.x=element_blank(),
           #axis.ticks.x=element_blank(),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank())
   
   s1p5<- ggplot(tmpBR.s1,aes(x=value, y = ..density..))+
     geom_histogram(col=I("green"),fill = "green") +  xlim(-20,20)+
     geom_density(alpha = .5)+    ylab("") + xlab("")+  theme_bw()+
     theme(plot.title = element_text(hjust = 0.5), # center the plot title
           axis.title.x=element_blank(),
           axis.text.x=element_blank(),
           axis.ticks.x=element_blank(),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank())  
   
   s1p6<-ggplot(tmpBR.s1, aes(sample=value))+   
     ylab("") + xlab("")+  theme_bw()+
     stat_qq() +stat_qq_line(colour = "red") +
     theme(plot.title = element_text(hjust = 0.5), # center the plot title
            axis.title.x=element_blank(),
           axis.text.x=element_blank(),
           axis.ticks.x=element_blank(),
            plot.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
   
   
  # S2
   
   tmpPP.s2<-tibble(value =PPall[Study=="sim2"])
   tmpHR.s2<-tibble(value =HRall[Study=="sim2"])
   tmpBR.s2<-tibble(value =BRall[Study=="sim2"])
   

   s2p1<- ggplot(tmpPP.s2,aes(x=value, y = ..density..))+
     geom_histogram(col=I("green"),fill = "green") + xlim(-1.5,1.5)+
     geom_density(alpha = .5)+    ylab("S2") + xlab("")+  theme_bw()+
     theme(plot.title = element_text(hjust = 0.5), # center the plot title
           axis.title.x=element_blank(),
           axis.text.x=element_blank(),
           axis.ticks.x=element_blank(),
           #text=element_text(family="Garamond"),
           axis.title.y = element_text(size=28,
                                       angle=0,
                                       vjust=0.5,
                                       face='bold.italic'),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank())  
   
   s2p2<-ggplot(tmpPP.s2, aes(sample=value))+ 
     ylab("") + xlab("")+  theme_bw()+
     stat_qq() +stat_qq_line(colour = "red") +
     theme(plot.title = element_text(hjust = 0.5), # center the plot title
           axis.title.x=element_blank(),
           #axis.text.x=element_blank(),
           #axis.ticks.x=element_blank(),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank())
   
   
   s2p3<- ggplot(tmpHR.s2,aes(x=value, y = ..density..))+
     geom_histogram(col=I("green"),fill = "green") + xlim(-40,40)+
     geom_density(alpha = .5)+    ylab("") + xlab("")+  theme_bw()+
     theme(plot.title = element_text(hjust = 0.5), # center the plot title
           axis.title.x=element_blank(),
           axis.text.x=element_blank(),
           axis.ticks.x=element_blank(),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank()) 
   
   s2p4<-ggplot(tmpHR.s2, aes(sample=value))+
     ylab("") + xlab("")+  theme_bw()+
     stat_qq() +stat_qq_line(colour = "red") +
     theme(plot.title = element_text(hjust = 0.5), # center the plot title
           axis.title.x=element_blank(),
          # axis.text.x=element_blank(),
          # axis.ticks.x=element_blank(),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank())
   
   s2p5<- ggplot(tmpBR.s2,aes(x=value, y = ..density..))+
     geom_histogram(col=I("green"),fill = "green") +  xlim(-20,20)+
     geom_density(alpha = .5)+    ylab("") + xlab("")+  theme_bw()+
     theme(plot.title = element_text(hjust = 0.5), # center the plot title
           axis.title.x=element_blank(),
           axis.text.x=element_blank(),
           axis.ticks.x=element_blank(),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank())  
   
   s2p6<-ggplot(tmpBR.s2, aes(sample=value))+   
     ylab("") + xlab("")+  theme_bw()+
     stat_qq() +stat_qq_line(colour = "red") +
     theme(plot.title = element_text(hjust = 0.5), # center the plot title
           axis.title.x=element_blank(),
           #axis.text.x=element_blank(),
           #axis.ticks.x=element_blank(),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank())
   
   
   
   #   R
   
   tmpPP.r<-tibble(value =PPall[Study=="tt1"])
   tmpHR.r<-tibble(value =HRall[Study=="tt1"])
   tmpBR.r<-tibble(value =BRall[Study=="tt1"])
   

   
   rp1<- ggplot(tmpPP.r,aes(x=value, y = ..density..))+
     geom_histogram(col=I("green"),fill = "green") + xlim(-1.5,1.5)+
     geom_density(alpha = .5)+    ylab("R ") + xlab("")+  theme_bw()+
     theme(plot.title = element_text(hjust = 0.5), # center the plot title
           axis.title.x=element_blank(),
           axis.text.x=element_blank(),
           axis.ticks.x=element_blank(),
           axis.title.y = element_text(size=28, 
                                       angle=0,
                                       vjust=0.5, 
                                       face='bold.italic'),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank())  
   
   rp2<-ggplot(tmpPP.r, aes(sample=value))+ 
     ylab("") + xlab("")+  theme_bw()+
     stat_qq() +stat_qq_line(colour = "red") +
     theme(plot.title = element_text(hjust = 0.5), # center the plot title
           axis.title.x=element_blank(),
           #axis.text.x=element_blank(),
           #axis.ticks.x=element_blank(),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank())
   
   
   rp3<- ggplot(tmpHR.r,aes(x=value, y = ..density..))+
     geom_histogram(col=I("green"),fill = "green") + xlim(-40,40)+
     geom_density(alpha = .5)+    ylab("") + xlab("")+  theme_bw()+
     theme(plot.title = element_text(hjust = 0.5), # center the plot title
           axis.title.x=element_blank(),
           axis.text.x=element_blank(),
           axis.ticks.x=element_blank(),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank()) 
   
   rp4<-ggplot(tmpHR.r, aes(sample=value))+
     ylab("") + xlab("")+  theme_bw()+
     stat_qq() +stat_qq_line(colour = "red") +
     theme(plot.title = element_text(hjust = 0.5), # center the plot title
           axis.title.x=element_blank(),
           #axis.text.x=element_blank(),
           #axis.ticks.x=element_blank(),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank())
   
   rp5<- ggplot(tmpBR.r,aes(x=value, y = ..density..))+
     geom_histogram(col=I("green"),fill = "green") +  xlim(-20,20)+
     geom_density(alpha = .5)+    ylab("") + xlab("")+  theme_bw()+
     theme(plot.title = element_text(hjust = 0.5), # center the plot title
           axis.title.x=element_blank(),
           axis.text.x=element_blank(),
           axis.ticks.x=element_blank(),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank())  
   
   rp6<-ggplot(tmpBR.r, aes(sample=value))+   
     ylab("") + xlab("")+  theme_bw()+
     stat_qq() +stat_qq_line(colour = "red") +
     theme(plot.title = element_text(hjust = 0.5), # center the plot title
           axis.title.x=element_blank(),
           #axis.text.x=element_blank(),
           #axis.ticks.x=element_blank(),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank())
   
   
   
   
   
   
   
   
   
   
   
   
 
# O     
   
   

   tmpPP.o<-tibble(value =PPall[Study=="OT"])
   tmpHR.o<-tibble(value =HRall[Study=="OT"])
   tmpBR.o<-tibble(value =BRall[Study=="OT"])
   
   
   
   op1<- ggplot(tmpPP.o,aes(x=value, y = ..density..))+
     geom_histogram(col=I("green"),fill = "green") + xlim(-1.5,1.5)+
     geom_density(alpha = .5)+    ylab("O ") + xlab("")+  theme_bw()+
     theme(plot.title = element_text(hjust = 0.5), # center the plot title
           axis.title.x=element_blank(),
          # axis.text.x=element_blank(),
           #axis.ticks.x=element_blank(),
          axis.title.y = element_text(size=28, 
                                            angle=0,
                                            vjust=0.5, 
                                            face='bold.italic'),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank())  
   
   op2<-ggplot(tmpPP.o, aes(sample=value))+ 
     ylab("") + xlab("")+  theme_bw()+
     stat_qq() +stat_qq_line(colour = "red") +
     theme(plot.title = element_text(hjust = 0.5), # center the plot title
           axis.title.x=element_blank(),
           #axis.text.x=element_blank(),
           #axis.ticks.x=element_blank(),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank())
   
   
   op3<- ggplot(tmpHR.o,aes(x=value, y = ..density..))+
     geom_histogram(col=I("green"),fill = "green") + xlim(-40,40)+
     geom_density(alpha = .5)+    ylab("") + xlab("")+  theme_bw()+
     theme(plot.title = element_text(hjust = 0.5), # center the plot title
           axis.title.x=element_blank(),
           #axis.text.x=element_blank(),
           #axis.ticks.x=element_blank(),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank()) 
   
   op4<-ggplot(tmpHR.o, aes(sample=value))+
     ylab("") + xlab("")+  theme_bw()+
     stat_qq() +stat_qq_line(colour = "red") +
     theme(plot.title = element_text(hjust = 0.5), # center the plot title
           axis.title.x=element_blank(),
           #axis.text.x=element_blank(),
           #axis.ticks.x=element_blank(),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank())
   
   op5<- ggplot(tmpBR.o,aes(x=value, y = ..density..))+
     geom_histogram(col=I("green"),fill = "green") +  xlim(-20,20)+
     geom_density(alpha = .5)+    ylab("") + xlab("")+  theme_bw()+
     theme(plot.title = element_text(hjust = 0.5), # center the plot title
           axis.title.x=element_blank(),
           #axis.text.x=element_blank(),
           #axis.ticks.x=element_blank(),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank())  
   
   op6<-ggplot(tmpBR.o, aes(sample=value))+   
     ylab("") + xlab("")+  theme_bw()+
     stat_qq() +stat_qq_line(colour = "red") +
     theme(plot.title = element_text(hjust = 0.5), # center the plot title
           axis.title.x=element_blank(),
           #axis.text.x=element_blank(),
           #axis.ticks.x=element_blank(),
           plot.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank())
   
   
   
   
  # pdf("Figure1_v3.pdf",
  #     width = 24, # The width of the plot in inches
  #     height = 12) # The height of the plot in inches) 
  # panel.Sall<-grid.arrange(s1p1, s1p2,s1p3,s1p4,s1p5,s1p6, 
  #                        s2p1, s2p2,s2p3,s2p4,s2p5,s2p6,
  #                        rp1, rp2,rp3,rp4,rp5,rp6,
  #                        op1, op2,op3,op4,op5,op6,
  #                        nrow=4, ncol=6)
  # dev.off()
  
  
  # panel.S1<-grid.arrange(s1p1, s1p2,s1p3,s1p4,s1p5,s1p6, nrow=1)
  # panel.S2<-grid.arrange(s2p1, s2p2,s2p3,s2p4,s2p5,s2p6, nrow=1)
  # panel.R<-grid.arrange(rp1, rp2,rp3,rp4,rp5,rp6, nrow=1)
  # panel.O<-grid.arrange(op1, op2,op3,op4,op5,op6, nrow=1)
  
  #quartz()
  panel.PP<-grid.arrange(s1p1, s1p2, s2p1, s2p2,rp1,rp2, op1, op2, nrow =4, ncol=2)
  PP.ann<- annotate_figure( panel.PP ,
                            top = text_grob("ΔPP",face = "bold.italic",  size = 28)
                            ) 
  
  panel.HR<-grid.arrange(s1p5,s1p6, s2p5,s2p6, rp5,rp6, op5,op6,  nrow=4, ncol=2)
  HR.ann<- annotate_figure( panel.HR ,top = text_grob("ΔHR",face = "bold.italic",  size = 28)
                            )        
  
  panel.BR<-grid.arrange( s1p3, s1p4,s2p3,s2p4,rp3,rp4,op3,op4, nrow=4, ncol=2)
  BR.ann<- annotate_figure( panel.BR ,top = text_grob("ΔBR",face = "bold.italic",  size = 28)
                            )                     
                         

  


  #quartz()
  all<-ggarrange( PP.ann,HR.ann, BR.ann + rremove("x.text"), 
                ncol =3#, nrow = 1#,
               # labels = c("","A","B","C")
                #widths = c(3, 2, 1,1),
                
                # widths = c(1, 0.05, 1 ,0.05,0.05,0.05,1 ,0.05,1) ,
                # heights = c(1, 0.05, 1 ,0.05,0.05,0.05,1 ,0.05,1)
                ) #+    theme(plot.margin = margin(5,0.5,5,0.5, "cm"))
  
  

  ggsave(paste0("Figure1_v9.pdf"),
         all,
         device=cairo_pdf,
         
         #--------- For ncol=2
         # width=20,
         # height=14,
         
         #--------- For ncol=3
         # width=21,
         # height=6,
         
         #--------- For ncol=1
         width=24,
         height=12,
  )







