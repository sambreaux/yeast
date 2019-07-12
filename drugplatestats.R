getwd()
library(tidyverse)
library(plater)
library(formattable)
library(dplyr)
library(zoo)
library(htmlTable)
library(reshape2)

##load data proccessed

GD<-read.csv("2019-07-02_summarised-data (1).csv")

## merge PD with Plate Map

data <- add_plate(
  data = GD, 
  file = "PLATE MAP.csv",
  well_ids_column = "well")

##split dataframe based on plate 

x<-split(data, data$plate, drop = F)

BY4743<-x$S1L1
U5.05<-x$S1L2
S6.14<-x$S2L1
S6.45<-x$S2L2
S6.113<-x$S3L1
W.34.70<-x$S3L2
WLP644<-x$S4L1


#load in raw data

MD<-read.csv("2019-07-02_measures-data (2).csv")

##split dataframe based on plate 
xx<-split(MD, MD$plate, drop = F)


BY4743m<-xx$S1L1
U5.05m<-xx$S1L2
S6.14m<-xx$S2L1
S6.45m<-xx$S2L2
S6.113m<-xx$S3L1
W.34.70m<-xx$S3L2
WLP644m<-xx$S4L1


## function to calculate area undercurve

calc_AUC <- function(n,z){
  x <- n %>%
    filter(well == z)%>%
    select(runtime)  
  
  X<-x$runtime
  
  
  y <- n %>%
    filter(well == z)%>%
    select(measure) 
  Y<-y$measure 
  
  
  sum(diff(X)*rollmean(Y,2))
}

## function to calculate area undercurve of a plate and return it as a dataframe

calc_AUC_plate <- function(BY4743m){

A01<- calc_AUC(BY4743m,"A01") 
B01<- calc_AUC(BY4743m,"B01") 
C01<- calc_AUC(BY4743m,"C01") 
D01<- calc_AUC(BY4743m,"D01") 
E01<- calc_AUC(BY4743m,"E01") 
F01<- calc_AUC(BY4743m,"F01") 
G01<- calc_AUC(BY4743m,"G01")  
H01<- calc_AUC(BY4743m,"H01")  

A02<- calc_AUC(BY4743m,"A02") 
B02<- calc_AUC(BY4743m,"B02") 
C02<- calc_AUC(BY4743m,"C02")  
D02<- calc_AUC(BY4743m,"D02") 
E02<- calc_AUC(BY4743m,"E02") 
F02<- calc_AUC(BY4743m,"F02") 
G02<- calc_AUC(BY4743m,"G02") 
H02<- calc_AUC(BY4743m,"H02") 

A03<- calc_AUC(BY4743m,"A03") 
B03<- calc_AUC(BY4743m,"B03") 
C03<- calc_AUC(BY4743m,"C03") 
D03<- calc_AUC(BY4743m,"D03") 
E03<- calc_AUC(BY4743m,"E03") 
F03<- calc_AUC(BY4743m,"F03") 
G03<- calc_AUC(BY4743m,"G03") 
H03<- calc_AUC(BY4743m,"H03") 

A04<- calc_AUC(BY4743m,"A04") 
B04<- calc_AUC(BY4743m,"B04") 
C04<- calc_AUC(BY4743m,"C04") 
D04<- calc_AUC(BY4743m,"D04") 
E04<- calc_AUC(BY4743m,"E04") 
F04<- calc_AUC(BY4743m,"F04") 
G04<- calc_AUC(BY4743m,"G04") 
H04<- calc_AUC(BY4743m,"H04") 

A05<- calc_AUC(BY4743m,"A05") 
B05<- calc_AUC(BY4743m,"B05") 
C05<- calc_AUC(BY4743m,"C05") 
D05<- calc_AUC(BY4743m,"D05") 
E05<- calc_AUC(BY4743m,"E05") 
F05<- calc_AUC(BY4743m,"F05") 
G05<- calc_AUC(BY4743m,"G05") 
H05<- calc_AUC(BY4743m,"H05") 

A06<- calc_AUC(BY4743m,"A06") 
B06<- calc_AUC(BY4743m,"B06") 
C06<- calc_AUC(BY4743m,"C06") 
D06<- calc_AUC(BY4743m,"D06") 
E06<- calc_AUC(BY4743m,"E06") 
F06<- calc_AUC(BY4743m,"F06") 
G06<- calc_AUC(BY4743m,"G06") 
H06<- calc_AUC(BY4743m,"H06") 

A07<- calc_AUC(BY4743m,"A07") 
B07<- calc_AUC(BY4743m,"B07") 
C07<- calc_AUC(BY4743m,"C07") 
D07<- calc_AUC(BY4743m,"D07") 
E07<- calc_AUC(BY4743m,"E07") 
F07<- calc_AUC(BY4743m,"F07") 
G07<- calc_AUC(BY4743m,"G07") 
H07<- calc_AUC(BY4743m,"H07")

A08<- calc_AUC(BY4743m,"A08") 
B08<- calc_AUC(BY4743m,"B08") 
C08<- calc_AUC(BY4743m,"C08") 
D08<- calc_AUC(BY4743m,"D08") 
E08<- calc_AUC(BY4743m,"E08") 
F08<- calc_AUC(BY4743m,"F08") 
G08<- calc_AUC(BY4743m,"G08") 
H08<- calc_AUC(BY4743m,"H08") 

A09<- calc_AUC(BY4743m,"A09") 
B09<- calc_AUC(BY4743m,"B09") 
C09<- calc_AUC(BY4743m,"C09") 
D09<- calc_AUC(BY4743m,"D09") 
E09<- calc_AUC(BY4743m,"E09")
F09<- calc_AUC(BY4743m,"F09")
G09<- calc_AUC(BY4743m,"G09")
H09<- calc_AUC(BY4743m,"H09")

A10<- calc_AUC(BY4743m,"A10") 
B10<- calc_AUC(BY4743m,"B10") 
C10<- calc_AUC(BY4743m,"C10") 
D10<- calc_AUC(BY4743m,"D10") 
E10<- calc_AUC(BY4743m,"E10")
F10<- calc_AUC(BY4743m,"F10")
G10<- calc_AUC(BY4743m,"G10")
H10<- calc_AUC(BY4743m,"H10")

A11<- calc_AUC(BY4743m,"A11") 
B11<- calc_AUC(BY4743m,"A11") 
B11<- calc_AUC(BY4743m,"B11") 
C11<- calc_AUC(BY4743m,"C11") 
D11<- calc_AUC(BY4743m,"D11") 
E11<- calc_AUC(BY4743m,"E11")
F11<- calc_AUC(BY4743m,"F11")
G11<- calc_AUC(BY4743m,"G11")
H11<- calc_AUC(BY4743m,"H11")

A12<- calc_AUC(BY4743m,"A12") 
B12<- calc_AUC(BY4743m,"B12") 
C12<- calc_AUC(BY4743m,"C12") 
D12<- calc_AUC(BY4743m,"D12") 
E12<- calc_AUC(BY4743m,"E12")
F12<- calc_AUC(BY4743m,"F12")
G12<- calc_AUC(BY4743m,"G12")
H12<- calc_AUC(BY4743m,"H12")


AUC<-tibble(A01,	B01,	C01,	D01,	E01,	F01,	G01,	H01, 
            A02,	B02,	C02,	D02,	E02,	F02,	G02,	H02,
            A03,	B03,	C03,	D03,	E03,	F03,	G03,	H03,
            A04,	B04,	C04,	D04,	E04,	F04,	G04,	H04,
            A05,	B05,	C05,	D05,	E05,	F05,	G05,	H05,
            A06,	B06,	C06,	D06,	E06,	F06,	G06,	H06,
            A07,	B07,	C07,	D07,	E07,	F07,	G07,	H07,
            A08,	B08,	C08,	D08,	E08,	F08,	G08,	H08,
            A09,	B09,	C09,	D09,	E09,	F09,	G09,	H09,
            A10,	B10,	C10,	D10,	E10,	F10,	G10,	H10,
            A11,	B11,	C11,	D11,	E11,	F11,	G11,	H11,
            A12,	B12,	C12,	D12,	E12,	F12,	G12,	H12)

auc<-melt(AUC)
names(auc) <- c("well", "auc")
aucdata <- add_plate(
  data = auc, 
  file = "PLATE MAP.csv",
  well_ids_column = "well")

}

## calc AUC by sample

BYauc<-calc_AUC_plate(BY4743m)
U5auc<-calc_AUC_plate(U5.05m)
S14auc<-calc_AUC_plate(S6.14m)
S45auc<-calc_AUC_plate(S6.45m)
S113auc<-calc_AUC_plate(S6.113m)
W34auc<-calc_AUC_plate(W.34.70m)
WLPauc<-calc_AUC_plate(WLP644m)

## combine with processed data
BYstats<-merge(BYauc, BY4743)%>%
  mutate(DT = log(2)/mu/3600)
U5stats<-merge(U5auc, U5.05)%>%
  mutate(rel_fitness = auc/ BYstats$auc)
S14stats<-merge(S14auc, S6.14)
S45stats<-merge(S45auc, S6.45)
S113stats<-merge(S113auc, S6.113)
W34stats<-merge(W34auc, W.34.70)
WLPstats<-merge(WLPauc, WLP644)

divide<-function(n,z){n/z}

divide(4,10)

yyy <- aggregate(auc~control*solvent*plate, BYstats, FUN = mean)
?aggregate
BYaverage_stats <- merge(aggregate(list(AVE_auc=BYstats$auc, AVE_A=BYstats$A, AVE_mu=BYstats$mu, AVE_lambda=BYstats$lambda, AVE_DT=BYstats$DT, ave_rel=BYstats$), by = list(sample=BYstats$sample), mean), df)

##output
write.csv(BYstats, "BYstats.csv")
write.csv(U5stats, "U5stats.csv")
write.csv(S14stats, "S14stats.csv")
write.csv(S45stats, "S45stats.csv")
write.csv(S113stats, "S113stats.csv")
write.csv(W34stats, "W34stats.csv")
write.csv(WLPstats, "WLPstats.csv")



BYstats
U5stats
S14stats
S45stats
S113stats
W34stats
WLPstats


BYaverage_stats <- get_ave_stats(BYstats)
U5average_stats <- get_ave_stats(U5stats)
S14average_stats <- get_ave_stats(S14stats)
S45average_stats <- get_ave_stats(S45stats)
S113average_stats <- get_ave_stats(BYstats)
W34average_stats <- get_ave_stats(W34stats)
WLPaverage_stats <- get_ave_stats(WLPstats)
  
write.csv(BYaverage_stats, "BYaverage_stats.csv")
write.csv(U5average_stats, "U5average_stats.csv")
write.csv(S14average_stats, "S14average_stats.csv")
write.csv(S45stats, "S45average_stats")
write.csv(S113stats, "S113stats.csv")
write.csv(W34stats, "W34average_stats")
write.csv(WLPstats, "WLPaverage_stats")


get_ave_stats <- function(BYstats){
df <- subset(BYstats, select = -c(auc,A,lambda, mu, well))
BYaverage_stats <- merge(aggregate(list(AVE_auc=BYstats$auc, AVE_A=BYstats$A, AVE_mu=BYstats$mu, AVE_lambda=BYstats$lambda, AVE_DT=BYstats$DT, ave_rel=BYstats$), by = list(sample=BYstats$sample), mean), df)%>%
  distinct()}


 
 
?aggregate.data.frame
strains<-c("S6.14","S6.45","S6.113","U5.05","W.34.70","WLP644","BY4743")
ave_mu<-signif(c(mean(S6.14$mu),mean(S6.45$mu),mean(S6.113$mu),mean(U5.05$mu),mean(W.34.70$mu),mean(WLP644$mu),mean(BY4743$mu),mean(chlamy$mu),mean(GLU$mu)), digits = 2)
ave_A<-c(mean(S6.14$A),mean(S6.45$A),mean(S6.113$A),mean(U5.05$A),mean(W.34.70$A),mean(WLP644$A),mean(BY4743$A),mean(chlamy$A),mean(GLU$A))
ave_lambda<-c(mean(S6.14$lambda),mean(S6.45$lambda),mean(S6.113$lambda),mean(U5.05$lambda),mean(W.34.70$lambda),mean(WLP644$lambda),mean(BY4743$lambda),mean(chlamy$lambda),mean(GLU$lambda))
ave_AUC<-c(mean(S6.14$auc),mean(S6.45$auc),mean(S6.113$auc),mean(U5.05$auc),mean(W.34.70$auc),mean(WLP644$auc),mean(BY4743$auc),mean(chlamy$auc),mean(GLU$auc))
summ<-data.frame(strains,ave_A,ave_mu,ave_lambda, ave_AUC)

cccc <- ifelse("H2O" = (BYstats$sample.2) & 
                        (BYstats$sample.2) = "H2O C")



summ<-summ%>%
  mutate(ave_DT = log(2)/summ$ave_mu/3600, rel_fitness = ave_AUC/102021.65)
summp<-summ
summp[,c(2,4:7)]<-rs<-round(summp [,c(2,4:7)], 2)
colnames(summp)<-c("Strains", "Average A", "Average mu", "Average Lambda", "Average AUC", "Average Doubling Time", "Relavtive Fitness")
widget_formattable = formattable(summp) 

widget_formattable

?merge

