#####Cleaning the wokring space before starting#################################
rm(list=ls())

####Setting a working directory#################################################

setwd("~/Desktop/Masters 2021-2022/Mini-Project")
##########Reading in the original file##########################################

turtles<-read.csv("hermanni.csv",header = T)

########Removing the first row from the original data as it is empty############
turtle<-turtles[-1,]

##########Subseting the observation of turtles that do not have a habitat######

tur<-subset(turtle,turtle$Habitat!="NA")

##########Subseting those observations that do not have sex assigned###########

s<-subset(tur,tur$Sex!="NA")
###Creating a new data set to make it more easily to visualize and work with####

require(dplyr)
require(tidyr)

hermanni_turtle<-s%>%select(Ind..No.,C.R,Sex,Habitat,Month,Year,BM,SH,SCL,MCW,MaxCW,Status)

##############Renaming the individual columns###################################

colnames(hermanni_turtle) <- c("Individual","CR","Sex","Habitat","Month","Year","BM","SH","SCL","MCW","MaxCW","Status")

#########Sub-setting Na's########################################################


h_t_1<-subset(hermanni_turtle,hermanni_turtle$BM != "NA")

h_t<-subset(h_t_1,h_t_1$SH!="NA")

h_t_2<-subset(h_t,h_t$SCL!="NA")
#############################Subseting Recaptures and NA's######################

h_t_3<-subset(h_t_2,h_t_2$CR!="R")

ht_4<-subset(h_t_3,h_t_3$Habitat!=" NA")
################################################################################

####Converting the SH from character to numeric#################################

h_t_2$SH<-as.numeric(h_t_2$SH)
#########Removing two habitats Urban Opening and Road###########################

ht_5<-subset(ht_4,ht_4$Habitat!="Opening+Urban")
str(ht_4)

h_t_3<-subset(h_t_2,h_t_2$Habitat!="Road")

##############Separate the habitats into those necessary for analysis##########

Meadow<-subset(ht_4,ht_4$Habitat=="Meadow")
Degraded_Forest<-subset(ht_4,ht_4$Habitat=="Degraded forest")
Pine_forest<-subset(ht_4,ht_4$Habitat=="Pine forest")
D_P<-subset(ht_4,ht_4$Habitat=="Degraded forest+Pine forest")
Opening<-subset(ht_4,ht_4$Habitat=="Opening")
###########Visualize how many turtles were captured in each habitat###########

#########Make a data set with the number of turtles in each habitat#############
abundance2<-data.frame(Habitat=c("Meadow","Pine forest","Opening","Degraded forest","Degraded Pine forest"),
                       Abundance=c(94,20,8,209,38))

################################################################################

require(ggplot2)

################Calculating the Body Condition Index from the linear model######

BCI<-lm(log(BM)~log(SCL),data=Degraded_Forest)
b<-data.frame(resid(BCI))
mean(b$resid.BCI.)

BCI2<-lm(log(BM)~log(SCL),data=Meadow)
m<-data.frame(resid(BCI2))
mean(m$resid.BCI2.)

BCI3<-lm(log(BM)~log(SCL),data=Pine_forest)
p<-data.frame(resid(BCI3))
mean(p$resid.BCI3.)

BCI5<-lm(log(BM)~log(SCL),data=D_P)
d_p<-data.frame(resid(BCI5))
mean(d_p$resid.BCI5.)

BCI6<-lm(log(BM)~log(SCL),data=Opening)
o<-data.frame(resid(BCI6))
mean(o$resid.BCI6.)
##################Test if the residuals are normal distributed##################
hist(BCI$BCI)

###################Concatenate the residual data set ###########################

d_p<-d_p%>%mutate(habitat="Degraded forest+Pine forest")%>%rename(BCI=resid.BCI5.)
m<-m%>%mutate(habitat="Meadow")%>%rename(BCI=resid.BCI2.)
o<-o%>%mutate(habitat="Opening")%>%rename(BCI=resid.BCI6.)
p<-p%>%mutate(habitat="Pine forest")%>%rename(BCI=resid.BCI3.)
b<-b%>%mutate(habitat="Degraded forest")%>%rename(BCI=resid.BCI.)
##############Adding the sex column in each of the data set######################
m$Sex<-Meadow$Sex
o$Sex<-Opening$Sex
p$Sex<-Pine_forest$Sex
b$Sex<-Degraded_Forest$Sex
d_p$Sex<-D_P$Sex
#################Adding the Status column#######################################
m$Status<-Meadow$Status
o$Status<-Opening$Status
p$Status<-Pine_forest$Status
b$Status<-Degraded_Forest$Status
d_p$Status<-D_P$Status

##############Adding the years column###########################################
m$Year<-Meadow$Year
o$Year<-Opening$Year
p$Year<-Pine_forest$Year
b$Year<-Degraded_Forest$Year
d_p$Year<-D_P$Year

################Combining the several data frames into one and identifying whether 
##a habitat has trees or does not have trees####################################
BCI<-rbind(d_p,m,o,p,b)%>% 
  mutate(Trees=ifelse(habitat=="Meadow"|
                        habitat=="Opening",0,1),
         Trees=as.character(Trees))

##############Plotting the BCI per habitat#####################################
bar3<-ggplot(BCI,aes(y=BCI,x=habitat,fill=Trees))+geom_boxplot()+theme_classic()+
  theme(legend.text=element_text(size=6),
        axis.text = element_text(family = "Helvetica", size = (10), colour = "grey1"),
        axis.title=element_text(family = "Helvetica", size = (10), colour = "grey1"))+
  scale_x_discrete(labels=c("Degraded forest"="DF","Degraded forest+Pine forest"="DPF","Meadow"=
                              "M","Opening"="O","Pine forest"="PF"))
                                                                        

plot(bar3)

################################################################################


################Fixing a mistake in the data set where a space was distorting my
#variables


BCI <- mutate(BCI, Sex2 = ifelse(Sex == "F " | Sex == "F", "F", "M")) 
BCI <- BCI %>%  mutate(Sex = Sex2) %>% select(-Sex2)
unique(BCI$Sex)

######################Carrying  out a GLM########################################

M3<-glm(BCI~as.factor(habitat)+as.factor(Sex)+as.factor(Status),data=BCI,family="gaussian")
summary(M3)     
###################Diagnostics of the GLM#######################################
plot(M3)

dev.off()

###############Plotting the relationship of body mass and SCL ##################

scatter<-ggplot(ht_4,aes(y=BM,x=SCL))+geom_point()+theme_classic()
plot(scatter)
scatter2<-scatter+xlab("Straight Carapace Length(mm)")+ylab("Body Mass(g)")+
  theme(axis.title =element_text(family = "Helvetica", size = (10), colour = "grey1"),
        axis.text = element_text(family = "Helvetica", size = (10), colour = "grey1"))
plot(scatter2)
#################Putting the two graphs into one##############################
install.packages("cowplot")
require(cowplot)

plot_grid(scatter2,bar3,labels='AUTO')

