sumD<-read.csv("con2_sum_data.csv")

MeasD<-read.csv("con2_meas-data.csv")

std <- function(x){ sd(x)/sqrt(length(x))}

sum_data_map <- add_plate(
  data = sumD, 
  file = "PLATE MAP2.csv",
  well_ids_column = "well")%>%
  select(-control, -sample)

meas_data_map <- add_plate(
  data = MeasD, 
  file = "PLATE MAP2.csv",
  well_ids_column = "well")%>%
  select(-control, -sample)

plate<-c("S1L1", "S1L2", "S2L1","S2L2","S3L1","S3L2")
strain<-c("S6.14","S6.45", "S6.113", "U5.05","WLP.644","W.34.70")

pscon<-as.data.frame(cbind(plate,strain))
sum_D<-merge(sum_data_map,pscon)

con_mu<-sum_D%>%
  select (strain, solvent, mu, plate, well)

strains_list<-split(con_mu, con_mu$plate)

s14<-strains_list$S1L1%>%
  rename_col_by_position(3, 'mu_s14')%>%
  select(-strain,-plate)
s45<-strains_list$S1L2%>%
  rename_col_by_position(3, 'mu_s45')%>%
  select(-strain,-plate)
s113<-strains_list$S2L2%>%
  rename_col_by_position(3, 'mu_s113')%>%
  select(-strain,-plate)
u5<-strains_list$S2L2%>%
  rename_col_by_position(3, 'mu_u5')%>%
  select(-strain,-plate)
wlp<-strains_list$S3L1%>%
  rename_col_by_position(3, 'mu_wlp')%>%
  select(-strain,-plate)
w34<-strains_list$S3L2%>%
  rename_col_by_position(3, 'mu_w34')%>%
  select(-strain,-plate)

strain_mu<-merge(s14,s45)%>%
  merge(s113)%>%
  merge(u5)%>%
  merge(wlp)%>%
  merge(w34)%>%
  select(-well)%>%
  column_to_rownames("solvent")



write.csv(sum_D, "control_sum_meas.csv")

list_samples<-split(sum_D, sum_D$plate)



s14<-list_samples$S1L1

list_sol14<-split(s14, s14$solvent)

s14h2o<-rbind(list_sol14$H2O, list_sol14$`H2O-2`)

sem_mu<-sd(s14h2o$mu)/sqrt(62)
sem_A<-sd(s14h2o$A)
sem_lambda<-sd(s14h2o$lambda) 

StD_mu<-std(s14h2o$mu)
StD_A<-std(s14h2o$A)
StD_lambda<-std(s14h2o$lambda) 

stra<-as.data.frame(s14$strain)%>%
  rename_col_by_position(1, "strain")

stand_devs<-as.data.frame(cbind(sem_A,sem_mu,sem_lambda))%>%
  merge(stra)%>%
  unique

###################################


s14<-list_samples$S1L1

list_sol14<-split(s14, s14$solvent)

s14h2o<-rbind(list_sol14$H2O, list_sol14$`H2O-2`)

sem_h2o_mu<-std(s14h2o$mu)
sem_h2o_A<-std(s14h2o$A)
sem_h2o_lambda<-std(s14h2o$lambda)

s14h2o<-rbind(list_sol14$DMSO, list_sol14$`DMSO-2`)

sem_dmso_mu<-std(s14h2o$mu)
sem_dmso_A<-std(s14h2o$A)
sem_dmso_lambda<-std(s14h2o$lambda) 

s14h2o<-rbind(list_sol14$ETOH, list_sol14$`ETOH-2`)

sem_etoh_mu<-std(s14h2o$mu)
sem_etoh_A<-std(s14h2o$A)
sem_etoh_lambda<-std(s14h2o$lambda) 

stra<-as.data.frame(s14$strain)%>%
  rename_col_by_position(1, "strain")

stand_devs<-as.data.frame(cbind(sem_h2o_A,sem_h2o_mu,sem_h2o_lambda, sem_dmso_A,sem_dmso_mu,sem_dmso_lambda,sem_etoh_A,sem_etoh_mu,sem_etoh_lambda))%>%
  merge(stra)%>%
  unique

#######################################

s14<-list_samples$S1L1

list_sol14<-split(s14, s14$solvent)

s14h2o<-rbind(list_sol14$H2O, list_sol14$`H2O-2`)

sem_h2o_mu<-std(s14h2o$mu)
sem_h2o_A<-std(s14h2o$A)
sem_h2o_lambda<-std(s14h2o$lambda)

s14h2o<-rbind(list_sol14$DMSO, list_sol14$`DMSO-2`)

sem_dmso_mu<-std(s14h2o$mu)
sem_dmso_A<-std(s14h2o$A)
sem_dmso_lambda<-std(s14h2o$lambda) 

s14h2o<-rbind(list_sol14$ETOH, list_sol14$`ETOH-2`)

sem_etoh_mu<-std(s14h2o$mu)
sem_etoh_A<-std(s14h2o$A)
sem_etoh_lambda<-std(s14h2o$lambda) 

stra<-as.data.frame(s14$strain)%>%
  rename_col_by_position(1, "strain")

stand_devs_14<-as.data.frame(cbind(sem_h2o_A,sem_h2o_mu,sem_h2o_lambda, sem_dmso_A,sem_dmso_mu,sem_dmso_lambda,sem_etoh_A,sem_etoh_mu,sem_etoh_lambda))%>%
  merge(stra)%>%
  unique


s14<-list_samples$S1L2

list_sol14<-split(s14, s14$solvent)

s14h2o<-rbind(list_sol14$H2O, list_sol14$`H2O-2`)

sem_h2o_mu<-std(s14h2o$mu)
sem_h2o_A<-std(s14h2o$A)
sem_h2o_lambda<-std(s14h2o$lambda)

s14h2o<-rbind(list_sol14$DMSO, list_sol14$`DMSO-2`)

sem_dmso_mu<-std(s14h2o$mu)
sem_dmso_A<-std(s14h2o$A)
sem_dmso_lambda<-std(s14h2o$lambda) 

s14h2o<-rbind(list_sol14$ETOH, list_sol14$`ETOH-2`)

sem_etoh_mu<-std(s14h2o$mu)
sem_etoh_A<-std(s14h2o$A)
sem_etoh_lambda<-std(s14h2o$lambda) 

stra<-as.data.frame(s14$strain)%>%
  rename_col_by_position(1, "strain")

stand_devs_46<-as.data.frame(cbind(sem_h2o_A,sem_h2o_mu,sem_h2o_lambda, sem_dmso_A,sem_dmso_mu,sem_dmso_lambda,sem_etoh_A,sem_etoh_mu,sem_etoh_lambda))%>%
  merge(stra)%>%
  unique

s14<-list_samples$S2L1

list_sol14<-split(s14, s14$solvent)

s14h2o<-rbind(list_sol14$H2O, list_sol14$`H2O-2`)

sem_h2o_mu<-std(s14h2o$mu)
sem_h2o_A<-std(s14h2o$A)
sem_h2o_lambda<-std(s14h2o$lambda)

s14h2o<-rbind(list_sol14$DMSO, list_sol14$`DMSO-2`)

sem_dmso_mu<-std(s14h2o$mu)
sem_dmso_A<-std(s14h2o$A)
sem_dmso_lambda<-std(s14h2o$lambda) 

s14h2o<-rbind(list_sol14$ETOH, list_sol14$`ETOH-2`)

sem_etoh_mu<-std(s14h2o$mu)
sem_etoh_A<-std(s14h2o$A)
sem_etoh_lambda<-std(s14h2o$lambda) 

stra<-as.data.frame(s14$strain)%>%
  rename_col_by_position(1, "strain")

stand_devs_113<-as.data.frame(cbind(sem_h2o_A,sem_h2o_mu,sem_h2o_lambda, sem_dmso_A,sem_dmso_mu,sem_dmso_lambda,sem_etoh_A,sem_etoh_mu,sem_etoh_lambda))%>%
  merge(stra)%>%
  unique

s14<-list_samples$S2L2

list_sol14<-split(s14, s14$solvent)

s14h2o<-rbind(list_sol14$H2O, list_sol14$`H2O-2`)

sem_h2o_mu<-std(s14h2o$mu)
sem_h2o_A<-std(s14h2o$A)
sem_h2o_lambda<-std(s14h2o$lambda)

s14h2o<-rbind(list_sol14$DMSO, list_sol14$`DMSO-2`)

sem_dmso_mu<-std(s14h2o$mu)
sem_dmso_A<-std(s14h2o$A)
sem_dmso_lambda<-std(s14h2o$lambda) 

s14h2o<-rbind(list_sol14$ETOH, list_sol14$`ETOH-2`)

sem_etoh_mu<-std(s14h2o$mu)
sem_etoh_A<-std(s14h2o$A)
sem_etoh_lambda<-std(s14h2o$lambda) 

stra<-as.data.frame(s14$strain)%>%
  rename_col_by_position(1, "strain")

stand_devs_u5<-as.data.frame(cbind(sem_h2o_A,sem_h2o_mu,sem_h2o_lambda, sem_dmso_A,sem_dmso_mu,sem_dmso_lambda,sem_etoh_A,sem_etoh_mu,sem_etoh_lambda))%>%
  merge(stra)%>%
  unique

s14<-list_samples$S3L1

list_sol14<-split(s14, s14$solvent)

s14h2o<-rbind(list_sol14$H2O, list_sol14$`H2O-2`)

sem_h2o_mu<-std(s14h2o$mu)
sem_h2o_A<-std(s14h2o$A)
sem_h2o_lambda<-std(s14h2o$lambda)

s14h2o<-rbind(list_sol14$DMSO, list_sol14$`DMSO-2`)

sem_dmso_mu<-std(s14h2o$mu)
sem_dmso_A<-std(s14h2o$A)
sem_dmso_lambda<-std(s14h2o$lambda) 

s14h2o<-rbind(list_sol14$ETOH, list_sol14$`ETOH-2`)

sem_etoh_mu<-std(s14h2o$mu)
sem_etoh_A<-std(s14h2o$A)
sem_etoh_lambda<-std(s14h2o$lambda) 

stra<-as.data.frame(s14$strain)%>%
  rename_col_by_position(1, "strain")

stand_devs_wlp<-as.data.frame(cbind(sem_h2o_A,sem_h2o_mu,sem_h2o_lambda, sem_dmso_A,sem_dmso_mu,sem_dmso_lambda,sem_etoh_A,sem_etoh_mu,sem_etoh_lambda))%>%
  merge(stra)%>%
  unique

s14<-list_samples$S3L2

list_sol14<-split(s14, s14$solvent)

s14h2o<-rbind(list_sol14$H2O, list_sol14$`H2O-2`)

sem_h2o_mu<-std(s14h2o$mu)
sem_h2o_A<-std(s14h2o$A)
sem_h2o_lambda<-std(s14h2o$lambda)

s14h2o<-rbind(list_sol14$DMSO, list_sol14$`DMSO-2`)

sem_dmso_mu<-std(s14h2o$mu)
sem_dmso_A<-std(s14h2o$A)
sem_dmso_lambda<-std(s14h2o$lambda) 

s14h2o<-rbind(list_sol14$ETOH, list_sol14$`ETOH-2`)

sem_etoh_mu<-std(s14h2o$mu)
sem_etoh_A<-std(s14h2o$A)
sem_etoh_lambda<-std(s14h2o$lambda) 

stra<-as.data.frame(s14$strain)%>%
  rename_col_by_position(1, "strain")

stand_devs_w34<-as.data.frame(cbind(sem_h2o_A,sem_h2o_mu,sem_h2o_lambda, sem_dmso_A,sem_dmso_mu,sem_dmso_lambda,sem_etoh_A,sem_etoh_mu,sem_etoh_lambda))%>%
  merge(stra)%>%
  unique

stand_devs<-rbind(stand_devs_14,stand_devs_46, stand_devs_113, stand_devs_u5,stand_devs_w34,stand_devs_wlp)

write.csv(stand_devs, "strain_sem.csv")

###################################


s14<-list_samples$S1L1

list_sol14<-split(s14, s14$solvent)

s14h2o<-rbind(list_sol14$H2O, list_sol14$`H2O-2`)

sem_h2o_mu<-std(s14h2o$mu)
sem_h2o_A<-std(s14h2o$A)
sem_h2o_lambda<-std(s14h2o$lambda)

s14h2o<-rbind(list_sol14$DMSO, list_sol14$`DMSO-2`)

sem_dmso_mu<-std(s14h2o$mu)
sem_dmso_A<-std(s14h2o$A)
sem_dmso_lambda<-std(s14h2o$lambda) 

s14h2o<-rbind(list_sol14$ETOH, list_sol14$`ETOH-2`)

sem_etoh_mu<-std(s14h2o$mu)
sem_etoh_A<-std(s14h2o$A)
sem_etoh_lambda<-std(s14h2o$lambda) 

stra<-as.data.frame(s14$strain)%>%
  rename_col_by_position(1, "strain")

stand_devs<-as.data.frame(cbind(sem_h2o_A,sem_h2o_mu,sem_h2o_lambda, sem_dmso_A,sem_dmso_mu,sem_dmso_lambda,sem_etoh_A,sem_etoh_mu,sem_etoh_lambda))%>%
  merge(stra)%>%
  unique

####################################### coeficent of variability
co_var <- function(x){ sd(x)/mean(x)}


s14<-list_samples$S1L1

list_sol14<-split(s14, s14$solvent)

s14h2o<-rbind(list_sol14$H2O, list_sol14$`H2O-2`)

co_var_h2o_mu<-co_var(s14h2o$mu)
co_var_h2o_A<-co_var(s14h2o$A)
co_var_h2o_lambda<-co_var(s14h2o$lambda)

s14h2o<-rbind(list_sol14$DMSO, list_sol14$`DMSO-2`)

co_var_dmso_mu<-co_var(s14h2o$mu)
co_var_dmso_A<-co_var(s14h2o$A)
co_var_dmso_lambda<-co_var(s14h2o$lambda) 

s14h2o<-rbind(list_sol14$ETOH, list_sol14$`ETOH-2`)

co_var_etoh_mu<-co_var(s14h2o$mu)
co_var_etoh_A<-co_var(s14h2o$A)
co_var_etoh_lambda<-co_var(s14h2o$lambda) 

stra<-as.data.frame(s14$strain)%>%
  rename_col_by_position(1, "strain")

stand_devs_14<-as.data.frame(cbind(co_var_h2o_A,co_var_h2o_mu,co_var_h2o_lambda, co_var_dmso_A,co_var_dmso_mu,co_var_dmso_lambda,co_var_etoh_A,co_var_etoh_mu,co_var_etoh_lambda))%>%
  merge(stra)%>%
  unique


s14<-list_samples$S1L2

list_sol14<-split(s14, s14$solvent)

s14h2o<-rbind(list_sol14$H2O, list_sol14$`H2O-2`)

co_var_h2o_mu<-co_var(s14h2o$mu)
co_var_h2o_A<-co_var(s14h2o$A)
co_var_h2o_lambda<-co_var(s14h2o$lambda)

s14h2o<-rbind(list_sol14$DMSO, list_sol14$`DMSO-2`)

co_var_dmso_mu<-co_var(s14h2o$mu)
co_var_dmso_A<-co_var(s14h2o$A)
co_var_dmso_lambda<-co_var(s14h2o$lambda) 

s14h2o<-rbind(list_sol14$ETOH, list_sol14$`ETOH-2`)

co_var_etoh_mu<-co_var(s14h2o$mu)
co_var_etoh_A<-co_var(s14h2o$A)
co_var_etoh_lambda<-co_var(s14h2o$lambda) 

stra<-as.data.frame(s14$strain)%>%
  rename_col_by_position(1, "strain")

stand_devs_46<-as.data.frame(cbind(co_var_h2o_A,co_var_h2o_mu,co_var_h2o_lambda, co_var_dmso_A,co_var_dmso_mu,co_var_dmso_lambda,co_var_etoh_A,co_var_etoh_mu,co_var_etoh_lambda))%>%
  merge(stra)%>%
  unique

s14<-list_samples$S2L1

list_sol14<-split(s14, s14$solvent)

s14h2o<-rbind(list_sol14$H2O, list_sol14$`H2O-2`)

co_var_h2o_mu<-co_var(s14h2o$mu)
co_var_h2o_A<-co_var(s14h2o$A)
co_var_h2o_lambda<-co_var(s14h2o$lambda)

s14h2o<-rbind(list_sol14$DMSO, list_sol14$`DMSO-2`)

co_var_dmso_mu<-co_var(s14h2o$mu)
co_var_dmso_A<-co_var(s14h2o$A)
co_var_dmso_lambda<-co_var(s14h2o$lambda) 

s14h2o<-rbind(list_sol14$ETOH, list_sol14$`ETOH-2`)

co_var_etoh_mu<-co_var(s14h2o$mu)
co_var_etoh_A<-co_var(s14h2o$A)
co_var_etoh_lambda<-co_var(s14h2o$lambda) 

stra<-as.data.frame(s14$strain)%>%
  rename_col_by_position(1, "strain")

stand_devs_113<-as.data.frame(cbind(co_var_h2o_A,co_var_h2o_mu,co_var_h2o_lambda, co_var_dmso_A,co_var_dmso_mu,co_var_dmso_lambda,co_var_etoh_A,co_var_etoh_mu,co_var_etoh_lambda))%>%
  merge(stra)%>%
  unique

s14<-list_samples$S2L2

list_sol14<-split(s14, s14$solvent)

s14h2o<-rbind(list_sol14$H2O, list_sol14$`H2O-2`)

co_var_h2o_mu<-co_var(s14h2o$mu)
co_var_h2o_A<-co_var(s14h2o$A)
co_var_h2o_lambda<-co_var(s14h2o$lambda)

s14h2o<-rbind(list_sol14$DMSO, list_sol14$`DMSO-2`)

co_var_dmso_mu<-co_var(s14h2o$mu)
co_var_dmso_A<-co_var(s14h2o$A)
co_var_dmso_lambda<-co_var(s14h2o$lambda) 

s14h2o<-rbind(list_sol14$ETOH, list_sol14$`ETOH-2`)

co_var_etoh_mu<-co_var(s14h2o$mu)
co_var_etoh_A<-co_var(s14h2o$A)
co_var_etoh_lambda<-co_var(s14h2o$lambda) 

stra<-as.data.frame(s14$strain)%>%
  rename_col_by_position(1, "strain")

stand_devs_u5<-as.data.frame(cbind(co_var_h2o_A,co_var_h2o_mu,co_var_h2o_lambda, co_var_dmso_A,co_var_dmso_mu,co_var_dmso_lambda,co_var_etoh_A,co_var_etoh_mu,co_var_etoh_lambda))%>%
  merge(stra)%>%
  unique

s14<-list_samples$S3L1

list_sol14<-split(s14, s14$solvent)

s14h2o<-rbind(list_sol14$H2O, list_sol14$`H2O-2`)

co_var_h2o_mu<-co_var(s14h2o$mu)
co_var_h2o_A<-co_var(s14h2o$A)
co_var_h2o_lambda<-co_var(s14h2o$lambda)

s14h2o<-rbind(list_sol14$DMSO, list_sol14$`DMSO-2`)

co_var_dmso_mu<-co_var(s14h2o$mu)
co_var_dmso_A<-co_var(s14h2o$A)
co_var_dmso_lambda<-co_var(s14h2o$lambda) 

s14h2o<-rbind(list_sol14$ETOH, list_sol14$`ETOH-2`)

co_var_etoh_mu<-co_var(s14h2o$mu)
co_var_etoh_A<-co_var(s14h2o$A)
co_var_etoh_lambda<-co_var(s14h2o$lambda) 

stra<-as.data.frame(s14$strain)%>%
  rename_col_by_position(1, "strain")

stand_devs_wlp<-as.data.frame(cbind(co_var_h2o_A,co_var_h2o_mu,co_var_h2o_lambda, co_var_dmso_A,co_var_dmso_mu,co_var_dmso_lambda,co_var_etoh_A,co_var_etoh_mu,co_var_etoh_lambda))%>%
  merge(stra)%>%
  unique

s14<-list_samples$S3L2

list_sol14<-split(s14, s14$solvent)

s14h2o<-rbind(list_sol14$H2O, list_sol14$`H2O-2`)

co_var_h2o_mu<-co_var(s14h2o$mu)
co_var_h2o_A<-co_var(s14h2o$A)
co_var_h2o_lambda<-co_var(s14h2o$lambda)

s14h2o<-rbind(list_sol14$DMSO, list_sol14$`DMSO-2`)

co_var_dmso_mu<-co_var(s14h2o$mu)
co_var_dmso_A<-co_var(s14h2o$A)
co_var_dmso_lambda<-co_var(s14h2o$lambda) 

s14h2o<-rbind(list_sol14$ETOH, list_sol14$`ETOH-2`)

co_var_etoh_mu<-co_var(s14h2o$mu)
co_var_etoh_A<-co_var(s14h2o$A)
co_var_etoh_lambda<-co_var(s14h2o$lambda) 

stra<-as.data.frame(s14$strain)%>%
  rename_col_by_position(1, "strain")

stand_devs_w34<-as.data.frame(cbind(co_var_h2o_A,co_var_h2o_mu,co_var_h2o_lambda, co_var_dmso_A,co_var_dmso_mu,co_var_dmso_lambda,co_var_etoh_A,co_var_etoh_mu,co_var_etoh_lambda))%>%
  merge(stra)%>%
  unique

stand_co_dev<-rbind(stand_devs_14,stand_devs_46, stand_devs_113, stand_devs_u5,stand_devs_w34,stand_devs_wlp)

write.csv(stand_co_dev, "strain_co-var.csv")

