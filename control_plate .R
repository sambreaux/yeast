library(tidyverse)
library(plater)
library(formattable)
library(dplyr)
library(zoo)
library(htmlTable)
library(reshape2)
library(mtpview1)
library(stringr)
library(scales)

##load data proccessed

C_data<-read.csv("m_control_summarised-data.csv")
m_data<-read.csv("m_contol_measure_d.csv", stringsAsFactors=FALSE)

m_data2<-m_data[!grepl("S1L2", m_data$plate),]
C_data2<-C_data[!grepl("S1L2", C_data$plate),]

## WANT:average mu, lambda, A, relative fitness to By4743, average doubling time, area under curve 

## merge PD with Plate Map

dataC <- add_plate(
  data = C_data2, 
  file = "m_contol_platemap.csv",
  well_ids_column = "well")%>%
  mutate(DT = log(2)/mu/3600)

auc<-calc_AUC_plate2 (m_data2)
dataC<-merge(dataC, auc, by = c('well', 'compound', 'strain'))


average_stats <- aggregate(list(AVE_A=dataC$A, AVE_mu=dataC$mu, AVE_lambda=dataC$lambda, AVE_DT=dataC$DT, AVE_auc= dataC$auc), 
                                   by = list(sample=dataC$strain, compound = dataC$compound), mean)%>%
  distinct()

lll<-average_stats%>% 
  group_by(compound) %>% 
  bind_rows(., .) %>%
  mutate(rel_fit = ifelse(test = (compound == 'ETOH'), 
                       yes = AVE_auc/105368.11,
                       no = ifelse(test = (compound == 'DMSO'), 
                                   yes = AVE_auc/94703.53,
                                   no = AVE_auc/104331.16))) %>%
  unique()

write.csv(lll, "C:/Users/Sam/documents/yeast/RF2/control_plate_av.csv")
write.csv(dataC, "C:/Users/Sam/documents/yeast/RF2/control_plate_sum_mets.csv")
## function to calculate area undercurve

calc_AUC2 <- function(n,z){
  x <- n %>%
    filter(well == z)%>%
    select(runtime)  
  
  X<-x$runtime
  
  
  y <- n %>%
    filter(well == z)%>%
    select(measure_pp) 
  Y<-y$measure_pp 
  
  
  sum(diff(X)*rollmean(Y,2))
}

## function to calculate area undercurve of a plate and return it as a dataframe

calc_AUC_plate2 <- function(BY4743m){
  
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
    file = "m_contol_platemap.csv",
    well_ids_column = "well")
  
}
