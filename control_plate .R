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
dataC<-merg



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

