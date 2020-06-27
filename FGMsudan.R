setwd("C:/Users/exl15/Downloads/Rwork/sudan")
getwd()
library(foreign)
library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(survey)
library(car)
library(RcmdrMisc)
library(dummies)
library(corrplot)

wm<-read.spss(file="wm.sav",to.data.frame = TRUE)
#hh=read.spss(file="hh.sav",to.data.frame = TRUE)
#hl=read.spss(file="hl.sav",to.data.frame = TRUE)
#fg=read.spss(file="fg.sav",to.data.frame = TRUE)

View(wm)
#View(hh)
#View(hl)
#View(fg)

write.csv(wm,file="wm.csv", row.names = FALSE)
#write.csv(hh,file="hh.csv", row.names = FALSE)
#write.csv(hl,file="hl.csv", row.names = FALSE)
#write.csv(fg,file="fg.csv", row.names = FALSE)

#########Indicator 5.3.2: Proportion of girls and women aged 15-49 years who have undergone female genital
#########mutilation/cutting, by age###################################################

wmcompleted<-subset(wm, WM7=='Completed')
View(wmcompleted)
sum(wmcompleted$wmweight)
wmyes<-subset(wmcompleted, FG3=='Yes')
wmno<-subset(wmcompleted, FG3=='No')
sum(wmyes$wmweight)
sum(wmno$wmweight)
sum(wmyes$wmweight)/sum(wmcompleted$wmweight) #FGM prevalence ratio
##########Urban/Rural FGM prevalence###################################################
urbanity<-subset(wmcompleted, HH6=='Urban') #denominator for urban
rurality<-subset(wmcompleted, HH6=='Rural') #denominator for rural
sum(urbanity$wmweight)
sum(rurality$wmweight)
urbanityyes<-subset(urbanity, FG3=='Yes') #numerator for urban
ruralityyes<-subset(rurality, FG3=='Yes') #numerator for rural
sum(urbanityyes$wmweight)/sum(urbanity$wmweight) #FGM prevalence ratio urban
sum(ruralityyes$wmweight)/sum(rurality$wmweight) #FGM prevalence ratio rural
############FGM prevalence by ag groups##################################################
age1519<-subset(wmcompleted, WB2>=15 & WB2<20) #denominator
age2024<-subset(wmcompleted, WB2>=20 & WB2<25) #denominator
age2529<-subset(wmcompleted, WB2>=25 & WB2<30) #denominator
age3034<-subset(wmcompleted, WB2>=30 & WB2<35) #denominator
age3539<-subset(wmcompleted, WB2>=35 & WB2<40) #denominator
age4044<-subset(wmcompleted, WB2>=40 & WB2<45) #denominator
age4549<-subset(wmcompleted, WB2>=45 & WB2<50) #denominator

age1519yes<-subset(age1519, FG3=='Yes') #numerator
age2024yes<-subset(age2024, FG3=='Yes') #numerator
age2529yes<-subset(age2529, FG3=='Yes') #numerator
age3034yes<-subset(age3034, FG3=='Yes') #numerator
age3539yes<-subset(age3539, FG3=='Yes') #numerator
age4044yes<-subset(age4044, FG3=='Yes') #numerator
age4549yes<-subset(age4549, FG3=='Yes') #numerator

sum(age1519yes$wmweight)/sum(age1519$wmweight) #FGM prevalence ratio 1519
sum(age2024yes$wmweight)/sum(age2024$wmweight) #FGM prevalence ratio 2024
sum(age2529yes$wmweight)/sum(age2529$wmweight) #FGM prevalence ratio 2529
sum(age3034yes$wmweight)/sum(age3034$wmweight) #FGM prevalence ratio 3034
sum(age3539yes$wmweight)/sum(age3539$wmweight) #FGM prevalence ratio 3539
sum(age4044yes$wmweight)/sum(age4044$wmweight) #FGM prevalence ratio 4044
sum(age4549yes$wmweight)/sum(age4549$wmweight) #FGM prevalence ratio 4549
################FGM prevalence by education##################################################
edunone<-subset(wmcompleted, welevel=='None') #denominator
eduprimary<-subset(wmcompleted, welevel=='Primary') #denominator
edusecondary<-subset(wmcompleted, welevel=='Secondary') #denominator
eduhigher<-subset(wmcompleted, welevel=='Higher') #denominator

edunoneyes<-subset(edunone, FG3=='Yes') #numerator
eduprimaryyes<-subset(eduprimary, FG3=='Yes') #numerator
edusecondaryyes<-subset(edusecondary, FG3=='Yes') #numerator
eduhigheryes<-subset(eduhigher, FG3=='Yes') #numerator

sum(edunoneyes$wmweight)/sum(edunone$wmweight) #FGM prevalence ratio none
sum(eduprimaryyes$wmweight)/sum(eduprimary$wmweight) #FGM prevalence ratio primary
sum(edusecondaryyes$wmweight)/sum(edusecondary$wmweight) #FGM prevalence ratio secondary
sum(eduhigheryes$wmweight)/sum(eduhigher$wmweight) #FGM prevalence ratio hihger
#######################FGM prevalence by Wealth index quintile###############################
poorest<-subset(wmcompleted, windex5=='Poorest') #denominator
second<-subset(wmcompleted, windex5=='Second') #denominator
middle<-subset(wmcompleted, windex5=='Middle') #denominator
fourth<-subset(wmcompleted, windex5=='Fourth') #denominator
richest<-subset(wmcompleted, windex5=='Richest') #denominator

poorestyes<-subset(poorest, FG3=='Yes') #numerator
secondyes<-subset(second, FG3=='Yes') #numerator
middleyes<-subset(middle, FG3=='Yes') #numerator
fourthyes<-subset(fourth, FG3=='Yes') #numerator
richestyes<-subset(richest, FG3=='Yes') #numerator

sum(poorestyes$wmweight)/sum(poorest$wmweight) #FGM prevalence ratio none
sum(secondyes$wmweight)/sum(second$wmweight) #FGM prevalence ratio primary
sum(middleyes$wmweight)/sum(middle$wmweight) #FGM prevalence ratio secondary
sum(fourthyes$wmweight)/sum(fourth$wmweight) #FGM prevalence ratio hihger
sum(richestyes$wmweight)/sum(richest$wmweight) #FGM prevalence ratio hihger
#######################Attitudes toward FGM###################################################
attcontinued<-subset(wmcompleted, FG22=='Continued' & FG1=="Yes") #continued
attdiscontinued<-subset(wmcompleted, FG22=='Discontinued' & FG1=="Yes") #discontinued
attdepends<-subset(wmcompleted, FG22=='Depends' & FG1=="Yes") #depends
attdk<-subset(wmcompleted, FG22=='DK' & FG1=="Yes") #don't know
atttotal<-subset(wmcompleted, FG1=="Yes")

sum(attcontinued$wmweight)/sum(atttotal$wmweight)
sum(attdiscontinued$wmweight)/sum(atttotal$wmweight)
sum(attdepends$wmweight)/sum(atttotal$wmweight)
sum(attdk$wmweight)/sum(atttotal$wmweight)

######################################Child marriage age1549####################################
wmcompleted<-subset(wm, (WM7=='Completed') & WB2>=15 & WB2<50) 
sum(wmcompleted$wmweight)
age15<-subset(wmcompleted, WAGEM<15)
sum(age15$wmweight)/sum(wmcompleted$wmweight)
######################################Child marriage SDG 5.3.1##################################
wmcompleted<-subset(wm, (WM7=='Completed') & WB2>=20 & WB2<25) 
sum(wmcompleted$wmweight)
age18<-subset(wmcompleted, WAGEM<18)
sum(age18$wmweight)/sum(wmcompleted$wmweight)

######################################Currently married 15-19####################################
wmcompleted<-subset(wm, (WM7=='Completed') & WB2>=15 & WB2<20)
cmarried<-subset(wmcompleted, (MSTATUS=='Currently married')|(MSTATUS=='Formerly married')) #numerator
sum(cmarried$wmweight)/sum(wmcompleted$wmweight)

######################################currently married by urban and rural#######################
wmcompleted<-subset(wm, (WM7=='Completed') & WB2>=15 & WB2<20)
wmcompletedu<-subset(wm, (WM7=='Completed') & (WB2>=15 & WB2<20)&(HH6=='Urban'))
wmcompletedr<-subset(wm, (WM7=='Completed') & (WB2>=15 & WB2<20)&(HH6=='Rural'))
Cmarriedu<-subset(wmcompletedu, (MSTATUS=='Currently married')|(MSTATUS=='Formerly married'))
cmarriedr<-subset(wmcompletedr, (MSTATUS=='Currently married')|(MSTATUS=='Formerly married'))
Cmarriedu<-subset(cmarried, HH6=='Urban')
Cmarriedr<-subset(cmarried, HH6=='Rural')
sum(Cmarriedu$wmweight)/sum(wmcompletedu$wmweight)
sum(Cmarriedr$wmweight)/sum(wmcompletedr$wmweight)

######################################currently married by education#############################
wmcompleted<-subset(wm, (WM7=='Completed') & WB2>=15 & WB2<20) #women aged 15-19 and completed survey
edunone<-subset(wmcompleted, welevel=='None') #denominator none
eduprimary<-subset(wmcompleted, welevel=='Primary') #denominator primary
edusecondary<-subset(wmcompleted, welevel=='Secondary') #denominator secondary
eduhigher<-subset(wmcompleted, welevel=='Higher') #denominator higher

cmedunone<-subset(edunone, (MSTATUS=='Currently married')|(MSTATUS=='Formerly married')) #denominator none
cmeduprimary<-subset(eduprimary, (MSTATUS=='Currently married')|(MSTATUS=='Formerly married'))
cmedusecondary<-subset(edusecondary, (MSTATUS=='Currently married')|(MSTATUS=='Formerly married'))
cmeduhigher<-subset(eduhigher, (MSTATUS=='Currently married')|(MSTATUS=='Formerly married'))

#cmedunone<-subset(cmedunone, welevel=='None')
#cmeduprimary<-subset(cmeduprimary, welevel=='Primary')
#cmedusecondary<-subset(cmedusecondary, welevel=='Secondary')
#cmeduhigher<-subset(cmeduhigher, welevel=='Higher')


sum(cmedunone$wmweight)/sum(edunone$wmweight) #FGM prevalence ratio none
sum(cmeduprimary$wmweight)/sum(eduprimary$wmweight) #FGM prevalence ratio primary
sum(cmedusecondary$wmweight)/sum(edusecondary$wmweight) #FGM prevalence ratio secondary
sum(cmeduhigher$wmweight)/sum(eduhigher$wmweight) #FGM prevalence ratio hihger

