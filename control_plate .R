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

## merge PD with Plate Map

dataC <- add_plate(
  data = C_data, 
  file = "m_contol_platemap.csv",
  well_ids_column = "well")

controls<- split(dataC, dataC$compound, drop = F)
