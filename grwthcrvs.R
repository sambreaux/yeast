getwd()
library(tidyverse)
library(plater)
library(formattable)
install.packages("formattable") 
GD<-read.csv("C:/Users/Sam/Downloads/2019-05-25_summarised-data.csv")

read_plate("C:/Users/Sam/documents/design.csv")

data <- add_plate(
  data = GD, 
  file = "C:/Users/Sam/documents/design.csv",
  well_ids_column = "well")

data2<-subset(data, plate!="S1L2")
data2<- merge(aucdata,data2)
BY4743<-data2%>%
  filter(sample == "BY4743")

GLU<-data2%>%
  filter(sample == "GLU")

S6.14<-data2%>%
  filter(sample == "S6.14")

S6.45<-data2%>%
  filter(sample == "S6.45")
S6.113<-data2%>%
  filter(sample == "S6.113")
chlamy<-data2%>%
  filter(sample == "Chlamy")
U5.05<-data2%>%
  filter(sample == "U5.05")
W.34.70<-data2%>%
  filter(sample == "W.34.70")
WLP644<-data2%>%
  filter(sample == "WLP644")

strains<-c("S6.14","S6.45","S6.113","U5.05","W.34.70","WLP644","BY4743","chlamy","GLU")
ave_mu<-signif(c(mean(S6.14$mu),mean(S6.45$mu),mean(S6.113$mu),mean(U5.05$mu),mean(W.34.70$mu),mean(WLP644$mu),mean(BY4743$mu),mean(chlamy$mu),mean(GLU$mu)), digits = 2)
ave_A<-c(mean(S6.14$A),mean(S6.45$A),mean(S6.113$A),mean(U5.05$A),mean(W.34.70$A),mean(WLP644$A),mean(BY4743$A),mean(chlamy$A),mean(GLU$A))
ave_lambda<-c(mean(S6.14$lambda),mean(S6.45$lambda),mean(S6.113$lambda),mean(U5.05$lambda),mean(W.34.70$lambda),mean(WLP644$lambda),mean(BY4743$lambda),mean(chlamy$lambda),mean(GLU$lambda))
ave_AUC<-c(mean(S6.14$auc),mean(S6.45$auc),mean(S6.113$auc),mean(U5.05$auc),mean(W.34.70$auc),mean(WLP644$auc),mean(BY4743$auc),mean(chlamy$auc),mean(GLU$auc))
summ<-data.frame(strains,ave_A,ave_mu,ave_lambda, ave_AUC)

summ<-summ%>%
  mutate(ave_DT = log(2)/summ$ave_mu/3600, rel_fitness = ave_AUC/102021.65)
summp<-summ
summp[,c(2,4:7)]<-rs<-round(summp [,c(2,4:7)], 2)
colnames(summp)<-c("Strains", "Average A", "Average mu", "Average Lambda", "Average AUC", "Average Doubling Time", "Relavtive Fitness")
widget_formattable = formattable(summp) 

widget_formattable








MD<-read.csv("C:/Users/Sam/Downloads/2019-05-25_measures-data (5).csv")
MD<-subset(MD, plate!="S1L2")
library(zoo)
library(htmlTable)
library(reshape2)


GLU2<-MD%>%
  filter(sample == "GLU")
BY47432<-MD%>%
  filter(sample == "BY4743")
S6.142<-MD%>%
  filter(sample == "S6.14")
S6.452<-MD%>%
  filter(sample == "S6.45")
S6.1132<-MD%>%
  filter(sample == "S6.113")
chlamy2<-MD%>%
  filter(sample == "Chlamy")
U5.052<-MD%>%
  filter(sample == "U5.05")
W.34.702<-MD%>%
  filter(sample == "W.34.70")
WLP6442<-MD%>%
  filter(sample == "WLP644")

i<-calc_AUC(chlamy2,"B08")


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

A07<- calc_AUC(BY47432,"A07")
B07<- calc_AUC(BY47432,"B07")
C07<- calc_AUC(BY47432,"C07")
D07<- calc_AUC(BY47432,"D07")
E07<- calc_AUC(BY47432,"E07")
F07<- calc_AUC(BY47432,"F07")
G07<- calc_AUC(BY47432,"G07")
H07<- calc_AUC(BY47432,"H07")

A01<- calc_AUC(S6.142,"A01")
B01<- calc_AUC(S6.142,"B01")
C01<- calc_AUC(S6.142,"C01")
D01<- calc_AUC(S6.142,"D01")
E01<- calc_AUC(S6.142,"E01")
F01<- calc_AUC(S6.142,"F01")
G01<- calc_AUC(S6.142,"G01")
H01<- calc_AUC(S6.142,"H01")

A02<- calc_AUC(S6.452,"A02")
B02<- calc_AUC(S6.452,"B02")
C02<- calc_AUC(S6.452,"C02")
D02<- calc_AUC(S6.452,"D02")
E02<- calc_AUC(S6.452,"E02")
F02<- calc_AUC(S6.452,"F02")
G02<- calc_AUC(S6.452,"G02")
H02<- calc_AUC(S6.452,"H02")

A03<- calc_AUC(S6.1132,"A03")
B03<- calc_AUC(S6.1132,"B03")
C03<- calc_AUC(S6.1132,"C03")
D03<- calc_AUC(S6.1132,"D03")
E03<- calc_AUC(S6.1132,"E03")
F03<- calc_AUC(S6.1132,"F03")
G03<- calc_AUC(S6.1132,"G03")
H03<- calc_AUC(S6.1132,"H03")

A04<- calc_AUC(U5.052,"A04")
B04<- calc_AUC(U5.052,"B04")
C04<- calc_AUC(U5.052,"C04")
D04<- calc_AUC(U5.052,"D04")
E04<- calc_AUC(U5.052,"E04")
F04<- calc_AUC(U5.052,"F04")
G04<- calc_AUC(U5.052,"G04")
H04<- calc_AUC(U5.052,"H04")

A05<- calc_AUC(W.34.702,"A05")
B05<- calc_AUC(W.34.702,"B05")
C05<- calc_AUC(W.34.702,"C05")
D05<- calc_AUC(W.34.702,"D05")
E05<- calc_AUC(W.34.702,"E05")
F05<- calc_AUC(W.34.702,"F05")
G05<- calc_AUC(W.34.702,"G05")
H05<- calc_AUC(W.34.702,"H05")

A06<- calc_AUC(WLP6442,"A06")
B06<- calc_AUC(WLP6442,"B06")
C06<- calc_AUC(WLP6442,"C06")
D06<- calc_AUC(WLP6442,"D06")
E06<- calc_AUC(WLP6442,"E06")
F06<- calc_AUC(WLP6442,"F06")
G06<- calc_AUC(WLP6442,"G06")
H06<- calc_AUC(WLP6442,"H06")

A08<- calc_AUC(chlamy2,"A08")
B08<- calc_AUC(chlamy2,"B08")
C08<- calc_AUC(chlamy2,"C08")
D08<- calc_AUC(chlamy2,"D08")
E08<- calc_AUC(chlamy2,"E08")
F08<- calc_AUC(chlamy2,"F08")
G08<- calc_AUC(chlamy2,"G08")
H08<- calc_AUC(chlamy2,"H08")

A09<- calc_AUC(GLU2,"A09")
B09<- calc_AUC(GLU2,"B09")
C09<- calc_AUC(GLU2,"C09")
D09<- calc_AUC(GLU2,"D09")
E09<- calc_AUC(GLU2,"E09")
F09<- calc_AUC(GLU2,"F09")
G09<- calc_AUC(GLU2,"G09")
H09<- calc_AUC(GLU2,"H09")

AUC<-tibble(A01,	B01,	C01,	D01,	E01,	F01,	G01,	H01,
            A02,	B02,	C02,	D02,	E02,	F02,	G02,	H02,
            A03,	B03,	C03,	D03,	E03,	F03,	G03,	H03,
            A04,	B04,	C04,	D04,	E04,	F04,	G04,	H04,
            A05,	B05,	C05,	D05,	E05,	F05,	G05,	H05,
            A06,	B06,	C06,	D06,	E06,	F06,	G06,	H06,
            A07,	B07,	C07,	D07,	E07,	F07,	G07,	H07,
            A08,	B08,	C08,	D08,	E08,	F08,	G08,	H08,
            A09,	B09,	C09,	D09,	E09,	F09,	G09,	H09
            
)

auc<-melt(AUC)
names(auc) <- c("well", "auc")
aucdata <- add_plate(
  data = auc, 
  file = "C:/Users/Sam/documents/design.csv",
  well_ids_column = "well")


