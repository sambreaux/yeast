getwd()
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
library(cowplot)
??mtpview1
##load data proccessed

sum_dat<-read.csv("drug_run3_sum_dat.csv")

Meas_dat<-read.csv("drug_run3_meas-dat.csv")

##split dataframe based on plate 
xx<-split(Meas_dat, MD$plate, drop = F)

## merge PD with Plate Map

data_sum <- add_plate(
  data = sum_dat, 
  file = "PLATE MAP.csv",
  well_ids_column = "well")

data_meas <- add_plate(
  data = Meas_dat, 
  file = "PLATE MAP.csv",
  well_ids_column = "well")


###compare to off plate controls, on plate controls, by4743 (control strain)

##split dataframe based on plate 

x<-split(data_sum, data_sum$plate, drop = F)

S6.14<-x$S1L1
S6.45<-x$S1L2
s6.113<-x$S2L1
u5.05<-x$S2L2
wlp.644<-x$S3L1
W.34.70<-x$S3L2


#load in raw data

MD<-read.csv("drug_run3_meas-dat.csv")

##split dataframe based on plate 
xx<-split(data_meas, data_meas$plate, drop = F)



S6.14m<-xx$S1L1%>%
  arrange(runtime)
S6.45m<-xx$S1L2%>%
  arrange(runtime)
s6.113m<-xx$S2L1%>%
  arrange(runtime)
u5.05m<-xx$S2L2%>%
  arrange(runtime)
wlp.644m<-xx$S3L1%>%
  arrange(runtime)
W.34.70m<-xx$S3L2%>%
  arrange(runtime)



U5auc<-calc_AUC_plate(u5.05m)
S14auc<-calc_AUC_plate(S6.14m)
S45auc<-calc_AUC_plate(S6.45m)
S113auc<-calc_AUC_plate(s6.113m)
W34auc<-calc_AUC_plate(W.34.70m)
w34_rels<-off_plate_rel_fit(lll, W34auc, "W-34/70")%>%
  on_plate_rel_fit()

WLPauc<-calc_AUC_plate(wlp.644m)

sample_aucs<-rbind(U5auc,S14auc,S45auc,S113auc,W34auc,WLPauc)

W34auc$solvent<-(gsub("DMSO-2", "DMSO", W34auc$solvent))
W.34.70$solvent<-(gsub("DMSO-2", "DMSO", W.34.70$solvent))
is.data.frame(W34auc)
is.data.frame(W.34.70)
is.data.frame(W34stats)


xtx<-as.data.frame(W34stats)
iii<-relfit_DT(xtx)
## combine with processed data

U5stats<-merge(U5auc, u5.05)%>%
  relfit_DT()
S14stats<-merge(S14auc, S6.14)%>%
  relfit_DT()
S45stats<-merge(S45auc, S6.45)%>%
  relfit_DT()
S113stats<-merge(S113auc, s6.113)%>%
  relfit_DT()
W34stats<-merge(W34auc, W.34.70)%>%
  relfit_DT_no_DMSO2()
WLPstats<-merge(WLPauc, wlp.644)%>%
  relfit_DT()


U5_plat_rel<-U5stats%>% 
  group_by(solvent) %>% 
  bind_rows(., .) %>%
  mutate(rel_fit_off_plate = ifelse(test = (solvent == 'ETOH'), 
                          yes = auc/42626.6644,
                          no = ifelse(test = (solvent == 'ETOH-2'), 
                                      yes = auc/42626.6644,
                                      no = ifelse(test = (solvent == 'DMSO'),
                                                  yes = auc/39340.45,
                                                  no = ifelse(test = (solvent =='DMSO-2'),
                                                             yes = auc/39340.45,
                                                             no = ifelse(test = (solvent =='H2O'),
                                                                         yes= auc/48859.36,
                                                                         no =  auc/48859.36))))))%>%
        
  unique()


s14_plat_rel<-S14stats%>% 
  group_by(solvent) %>% 
  bind_rows(., .) %>%
  mutate(rel_fit_off_plate = ifelse(test = (solvent == 'ETOH'), 
                                    yes = auc/69464.01,
                                    no = ifelse(test = (solvent == 'ETOH-2'), 
                                                yes = auc/69464.01,
                                                no = ifelse(test = (solvent == 'DMSO'),
                                                            yes = auc/69716.13,
                                                            no = ifelse(test = (solvent =='DMSO-2'),
                                                                        yes = auc/69716.13,
                                                                        no = ifelse(test = (solvent =='H2O'),
                                                                                    yes= auc/73568.27,
                                                                                    no =  auc/73568.27))))))%>%
  
  unique()


s45_plat_rel<-S45stats%>% 
  group_by(solvent) %>% 
  bind_rows(., .) %>%
  mutate(rel_fit_off_plate = ifelse(test = (solvent == 'ETOH'), 
                                    yes = auc/664722.48,
                                    no = ifelse(test = (solvent == 'ETOH-2'), 
                                                yes = auc/64722.48,
                                                no = ifelse(test = (solvent == 'DMSO'),
                                                            yes = auc/74970.44,
                                                            no = ifelse(test = (solvent =='DMSO-2'),
                                                                        yes = auc/74970.44,
                                                                        no = ifelse(test = (solvent =='H2O'),
                                                                                    yes= auc/78111.87,
                                                                                    no =  auc/78111.87))))))%>%
  
  unique()

s113_plat_rel<-S113stats%>% 
  group_by(solvent) %>% 
  bind_rows(., .) %>%
  mutate(rel_fit_off_plate = ifelse(test = (solvent == 'ETOH'), 
                                    yes = auc/77574.89,
                                    no = ifelse(test = (solvent == 'ETOH-2'), 
                                                yes = auc/77574.89,
                                                no = ifelse(test = (solvent == 'DMSO'),
                                                            yes = auc/73206.75,
                                                            no = ifelse(test = (solvent =='DMSO-2'),
                                                                        yes = auc/73206.75,
                                                                        no = ifelse(test = (solvent =='H2O'),
                                                                                    yes= auc/80648.28,
                                                                                    no =  auc/80648.28))))))%>%
  
  unique()

wlp_plat_rel<-WLPstats%>% 
  group_by(solvent) %>% 
  bind_rows(., .) %>%
  mutate(rel_fit_off_plate = ifelse(test = (solvent == 'ETOH'), 
                                    yes = auc/51464.56,
                                    no = ifelse(test = (solvent == 'ETOH-2'), 
                                                yes = auc/51464.56,
                                                no = ifelse(test = (solvent == 'DMSO'),
                                                            yes = auc/51579.41,
                                                            no = ifelse(test = (solvent =='DMSO-2'),
                                                                        yes = auc/51579.41,
                                                                        no = ifelse(test = (solvent =='H2O'),
                                                                                    yes= auc/58266.59,
                                                                                    no =  auc/558266.59))))))%>%
  
  unique()


w34_plat_rel<-W34stats%>% 
  group_by(solvent) %>% 
  bind_rows(., .) %>%
  mutate(rel_fit_off_plate = ifelse(test = (solvent == 'ETOH'), 
                                    yes = auc/89838.12,
                                    no = ifelse(test = (solvent == 'ETOH-2'), 
                                                yes = auc/89838.12,
                                                no = ifelse(test = (solvent == 'DMSO'),
                                                            yes = auc/79310.60,
                                                            no = ifelse(test = (solvent =='DMSO-2'),
                                                                        yes = auc/79310.60,
                                                                        no = ifelse(test = (solvent =='H2O'),
                                                                                    yes= auc/94964.75,
                                                                                    no =  auc/94964.75))))))%>%
  
  unique()

sample_stats<- rbind(U5_plat_rel,w34_plat_rel,wlp_plat_rel,s14_plat_rel,s45_plat_rel,s113_plat_rel)%>%
  merge(ps, by = "plate")%>%
  merge(byauc, by = c("sample", "well"))%>%
  mutate(rel_fit_to_by = auc/BYauc)%>%
  select(-BYauc)


average_sample_stats <- aggregate(list(AVE_A=sample_stats$A, AVE_mu=sample_stats$mu, AVE_lambda=sample_stats$lambda,
                                       AVE_DT=sample_stats$DT, AVE_auc= sample_stats$auc,ave_rel_GR = sample_stats$rel_max_GR,ave_rel_fit = sample_stats$rel_fitness, 
                                       ave_rel_fit_off_plate=sample_stats$rel_fit_off_plate, ave_rel_fit_to_by = sample_stats$rel_fit_to_by), 
                           by = list(compound=sample_stats$sample, strain= sample_stats$strain), mean)%>%
  distinct()





plate<- c("S1L1", "S1L2", "S2L1","S2L2","S3L1","S3L2")
strain<-c("S6.14",
           "S6.45",
           "s6.113",
           "u5.05",
           "wlp.644",
           "W.34.70")

ps<-as.data.frame(cbind(plate,strain))

byauc<-BYstats%>%
  select(sample,auc, solvent, well)%>%
  rename(c("auc"= "BYauc"))

dat_meas<-data_meas%>%
  rename(c("sample" = "compound"))%>%
  merge(ps)
avergage_measures<- merge(dat_meas, average_sample_stats, by = c("compound", "strain"))

plot2 <- sum_measures %>% 
  mtp_ggplot(aes(plate = plate, well = well)) + 
  mtp_spec_96well() + 
  geom_footprint(aes(fill = strain), alpha = .25) +
  geom_notched_border() +
  geom_col_label(size = 2) +
  geom_row_label(size = 2) +
  geom_well_rect(aes(fill = as.factor(control) , alpha = as.factor(ave_rel_fit))) +
  geom_well_line(aes(x = runtime, y = measure), size = 0.2) + 
  facet_wrap(~plate)

xc<-sample_stats%>%
  rename(c("sample"= "compound"))%>%
  select(-solvent, -control, -control2, -run )

sum_measures<- merge(dat_meas, xc, by = c("plate", "well", "compound", "strain"))

uuu<-split(sum_measures, sum_measures$plate, drop = F)

write.csv(sample_stats, "C:/Users/SamBr/documents/yeast/result files/RBSC3_all_mets.csv")

write.csv(average_sample_stats, "C:/Users/SamBr/documents/yeast/result files/RBSC3_average_mets.csv")




plot_rel_fit <- function(X){ 
  X%>% 
    mutate(plate = 1) %>% 
    unnest() %>% 
    mtp_ggplot(aes(plate = plate, well = well)) + 
    mtp_spec_96well() + 
    geom_footprint() + 
    geom_notched_border() +
    geom_col_label() + 
    geom_row_label() + 
    geom_well_rect(aes(fill = control, alpha = rel_fitness)) + 
    geom_well_line(aes(x = runtime, y = measure)) + 
    geom_well_text(aes(label = compound))
  
}

plots<-lapply(uuu, plot_rel_fit)

cowplot::plot_grid(plotlist = plots, nrow = 4)
plot2
names(avergage_measures)

a<-uuu$S1L1
b<-uuu$S1L2
c<-uuu$S2L1
d<-uuu$S2L2
e<-uuu$S3L1
f<-uuu$S3L2


uuu$S3L2%>%
  mutate(plate = 1) %>% 
  unnest() %>% 
  mtp_ggplot(aes(plate = plate, well = well)) + 
  mtp_spec_96well() + 
  geom_footprint() + 
  geom_notched_border() +
  geom_col_label() + 
  geom_row_label() + 
  geom_well_rect(aes(fill = control, alpha = rel_fitness)) + 
  geom_well_line(aes(x = runtime, y = measure)) + 
  geom_well_text(aes(label = compound))

a02<-a[grepl("A02", a$well.x),]
a08<-a[grepl("A08", a$well.x),]
n<-which(is.na(sum_measures))
## function to calculate area undercurve

calc_AUC <- function(n,z){
  x <- n %>%
    filter(well == z)%>%
    select(runtime)  
  
  X<-x$runtime
  
  
  y <- n %>%
    filter(well == z)%>%
    select(measures_pp) 
  Y<-y$measures_pp 
  
  
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
  plate_id<-BY4743m%>%
    select(well, plate)
  
  auc<-melt(AUC)
  names(auc) <- c("well", "auc")
  aucdata <- add_plate(
    data = auc, 
    file = "PLATE MAP.csv",
    well_ids_column = "well")%>%
    merge(plate_id, by = "well")
  
}



## calculate relative fitness and doubling time
relfit_DT<- function(BYstats){
  xtt<-split(BYstats, BYstats$solvent) 
  
  dmso<-xtt$DMSO %>%
    remove_rownames() %>%
    column_to_rownames("sample")
  
  dmsocon <-dmso["DMSO control", "auc"]
  dmsoconmu <-dmso["DMSO control", "mu"]
  
  dmso<-dmso%>%
    rownames_to_column("sample") %>%
    mutate(rel_fitness = auc/dmsocon, rel_max_GR = mu/dmsoconmu)
  
  H2O<-xtt$H2O %>%
    remove_rownames() %>%
    column_to_rownames("sample")
  
  H2Ocon <-H2O["H2O control", "auc"]
  H2Oconmu <-H2O["H2O control", "mu"]
  
  H2O<-H2O%>%
    rownames_to_column("sample") %>%
    mutate(rel_fitness = auc/H2Ocon,  rel_max_GR = mu/H2Oconmu)
  
  ETOH<-xtt$ETOH %>%
    remove_rownames() %>%
    column_to_rownames("sample")
  
  etohcon <-ETOH["ETOH control", "auc"]
  etohconmu <-ETOH["ETOH control", "mu"]
  
  ETOH<-ETOH%>%
    rownames_to_column("sample") %>%
    mutate(rel_fitness = auc/etohcon,  rel_max_GR = mu/etohconmu)
  
  dmso2<-xtt$`DMSO-2` %>%
    remove_rownames() %>%
    column_to_rownames("sample")
  
  dmsocon2 <-dmso2["DMSO control", "auc"]
  dmsoconmu2 <-dmso2["DMSO control", "mu"]
  
  dmso2<-dmso2%>%
    rownames_to_column("sample") %>%
    mutate(rel_fitness = auc/dmsocon2, rel_max_GR = mu/dmsoconmu2)
  
  H2O2<-xtt$`H2O-2` %>%
    remove_rownames() %>%
    column_to_rownames("sample")
  
  H2Ocon2 <-H2O2["H2O control", "auc"]
  H2Oconmu2 <-H2O2["H2O control", "mu"]
  
  H2O2<-H2O2%>%
    rownames_to_column("sample") %>%
    mutate(rel_fitness = auc/H2Ocon2,  rel_max_GR = mu/H2Oconmu2)
  
  
  ETOH2<-xtt$`ETOH-2` %>%
    remove_rownames() %>%
    column_to_rownames("sample")
  
  etohcon2 <-ETOH2["ETOH control", "auc"]
  etohconmu2 <-ETOH2["ETOH control", "mu"]
  
  ETOH2<-ETOH2%>%
    rownames_to_column("sample") %>%
    mutate(rel_fitness = auc/etohcon2,  rel_max_GR = mu/etohconmu2)
  
  BYstats<-rbind(dmso,H2O,ETOH,dmso2,H2O2,ETOH2)%>%
    mutate(DT = log(2)/mu/3600)
}



relfit_DT_no_DMSO2<- function(BYstats){
  xtt<-split(BYstats, BYstats$solvent) 
  
  dmso<-xtt$DMSO %>%
    remove_rownames()
 dmso<-dmso%>%
   as.character(dmso$sample)%>%
   make.unique(dmso$sample)%>%
    column_to_rownames("sample")
  
  dmsocon <-dmso["DMSO control", "auc"]
  dmsoconmu <-dmso["DMSO control", "mu"]
  
  dmso<-dmso%>%
    rownames_to_column("sample") %>%
    mutate(rel_fitness = auc/dmsocon, rel_max_GR = mu/dmsoconmu)
  
  H2O<-xtt$H2O %>%
    remove_rownames() %>%
    column_to_rownames("sample")
  
  H2Ocon <-H2O["H2O control", "auc"]
  H2Oconmu <-H2O["H2O control", "mu"]
  
  H2O<-H2O%>%
    rownames_to_column("sample") %>%
    mutate(rel_fitness = auc/H2Ocon,  rel_max_GR = mu/H2Oconmu)
  
  ETOH<-xtt$ETOH %>%
    remove_rownames() %>%
    column_to_rownames("sample")
  
  etohcon <-ETOH["ETOH control", "auc"]
  etohconmu <-ETOH["ETOH control", "mu"]
  
  ETOH<-ETOH%>%
    rownames_to_column("sample") %>%
    mutate(rel_fitness = auc/etohcon,  rel_max_GR = mu/etohconmu)
  
  
  H2O2<-xtt$`H2O-2` %>%
    remove_rownames() %>%
    column_to_rownames("sample")
  
  H2Ocon2 <-H2O2["H2O control", "auc"]
  H2Oconmu2 <-H2O2["H2O control", "mu"]
  
  H2O2<-H2O2%>%
    rownames_to_column("sample") %>%
    mutate(rel_fitness = auc/H2Ocon2,  rel_max_GR = mu/H2Oconmu2)
  
  
  ETOH2<-xtt$`ETOH-2` %>%
    remove_rownames() %>%
    column_to_rownames("sample")
  
  etohcon2 <-ETOH2["ETOH control", "auc"]
  etohconmu2 <-ETOH2["ETOH control", "mu"]
  
  ETOH2<-ETOH2%>%
    rownames_to_column("sample") %>%
    mutate(rel_fitness = auc/etohcon2,  rel_max_GR = mu/etohconmu2)
  
  BYstats<-rbind(dmso,H2O,ETOH,H2O2,ETOH2)%>%
    mutate(DT = log(2)/mu/3600)
}




## average stat per compound
get_ave_stats <- function(BYstats) {
  df <- subset(BYstats, select = -c(auc,A,lambda, mu, well,control, DT, rel_fitness,solvent, rel_max_GR, con_rel_auc, con_rel_mu ,BYmu, BYauc, BYrel_mu, BYrel_auc))
  BYaverage_stats <- merge(aggregate(list(AVE_auc=BYstats$auc, AVE_A=BYstats$A, AVE_mu=BYstats$mu, AVE_lambda=BYstats$lambda, AVE_DT=BYstats$DT, AVE_rel_fit = BYstats$rel_fitness, AVE_rel_max_GR = BYstats$rel_max_GR, AVE_con_rel_auc = BYstats$con_rel_auc, AVE_con_rel_mu= BYstats$con_rel_mu ,AVE_BYmu = BYstats$BYmu, AVE_BYauc = BYstats$BYauc, AVE_BYrel_mu= BYstats$BYrel_mu, AVE_BYrel_auc= BYstats$BYrel_auc), 
                                     by = list(sample=BYstats$sample), mean), df)%>%
    distinct()
}

names(BYstats)

##plot plate

plot_plate_fitness<-function(BY4743m, BYstats){
  VVV<-BY4743m%>%
    
    merge(BYstats)
  
  
  VVV %>% 
    mutate(plate = 1) %>% 
    mtp_ggplot(aes(plate = plate, well = well)) + 
    mtp_spec_96well(w=127.76*5, h=85.48*5) + 
    geom_footprint() + 
    geom_col_label() + 
    geom_row_label() + 
    geom_well_rect(aes ( fill = control, alpha = rel_fitness)) +
    geom_well_line(aes(x = runtime, y = measure)) + 
    geom_well_text(aes(label =str_wrap( sample,w = 10)),  colour="black") + 
    guides(fill = FALSE)
}




#plot sample average 

plot_ave_fitness<-function(BYaverage_stats,BY4743m){
  well_data(BYaverage_stats,BY4743m) %>% 
    mutate(plate = 1) %>% 
    mtp_ggplot(aes(plate = plate, well = well)) + 
    mtp_spec_96well(w=127.76*5, h=85.48*5) + 
    geom_footprint() + 
    geom_col_label() + 
    geom_row_label() + 
    geom_well_rect(aes ( fill =plate,alpha = AVE_rel_fit)) +
    geom_well_line(aes(x = runtime, y = measure)) + 
    geom_well_text(aes(label =str_wrap( sample,w = 10)),  colour="black") + 
    guides(fill = FALSE)
}



well_data<-function(BYaverage_stats,BY4743m){b<-merge(BYaverage_stats,hq)%>%
  merge(BY4743m)
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
  relfit_DT()
U5stats<-merge(U5auc, U5.05)%>%
  relfit_DT()
S14stats<-merge(S14auc, S6.14)%>%
  relfit_DT()
S45stats<-merge(S45auc, S6.45)%>%
  relfit_DT()
S113stats<-merge(S113auc, S6.113)%>%
  relfit_DT()
W34stats<-merge(W34auc, W.34.70)%>%
  relfit_DT()
WLPstats<-merge(WLPauc, WLP644)%>%
  relfit_DT()

BY4743stats<-BYstats%>%
  select(c(well,auc,mu))
colnames(BY4743stats) <- c("well", "BYauc","BYmu")

BYstats[,"BYauc"] <- NA
BYstats[,"BYmu"] <- NA
BYstats[,"BYrel_auc"] <- NA
BYstats[,"BYrel_mu"] <- NA

BYstats <-BYstats%>%
  mutate(con_rel_auc = auc/102021.65, con_rel_mu = mu/6.0e-05)
U5stats <-merge(BY4743stats, U5stats)%>%
  mutate(BYrel_auc= auc/BYauc, BYrel_mu= mu/BYmu, con_rel_auc = auc/71112.73, con_rel_mu = mu/3.2e-05)
S14stats<-merge(BY4743stats, S14stats)%>%
  mutate(BYrel_auc= auc/BYauc, BYrel_mu= mu/BYmu, con_rel_auc = auc/104062.91, con_rel_mu = mu/3.8e-05)
S45stats<-merge(BY4743stats, U5stats)%>%
  mutate(BYrel_auc= auc/BYauc, BYrel_mu= mu/BYmu, con_rel_auc = auc/104530.84, con_rel_mu = mu/4.5e-05)
S113stats<-merge(BY4743stats, U5stats)%>%
  mutate(BYrel_auc= auc/BYauc, BYrel_mu= mu/BYmu, con_rel_auc = auc/107479.96, con_rel_mu = mu/3.1e-05)
W34stats<-merge(BY4743stats, U5stats)%>%
  mutate(BYrel_auc= auc/BYauc, BYrel_mu= mu/BYmu, con_rel_auc = auc/76260.33, con_rel_mu = mu/1.9e-05)
WLPstats<-merge(BY4743stats, U5stats)%>%
  mutate(BYrel_auc= auc/BYauc, BYrel_mu= mu/BYmu, con_rel_auc = auc/108883.04, con_rel_mu = mu/6.6e-05)


ctt<-split(BYstats, BYstats$control)
hq<-ctt$`1`%>%
  select(sample, well)

BYaverage_stats <- get_ave_stats(BYstats)
U5average_stats <- get_ave_stats(U5stats)
S14average_stats <- get_ave_stats(S14stats)
S45average_stats <- get_ave_stats(S45stats)
S113average_stats <- get_ave_stats(BYstats)
W34average_stats <- get_ave_stats(W34stats)
WLPaverage_stats <- get_ave_stats(WLPstats)


##output
write.csv(BYstats, "BYstats.csv")
write.csv(U5stats, "U5stats.csv")
write.csv(S14stats, "S14stats.csv")
write.csv(S45stats, "S45stats.csv")
write.csv(S113stats, "S113stats.csv")
write.csv(W34stats, "W34stats.csv")
write.csv(WLPstats, "WLPstats.csv")


g<-rbind(BYstats,
         U5stats,
         S14stats,
         S45stats,
         S113stats,
         W34stats,
         WLPstats)


write.csv(BYaverage_stats, "BYaverage_stats.csv")
write.csv(U5average_stats, "U5average_stats.csv")
write.csv(S14average_stats, "S14average_stats.csv")
write.csv(S45stats, "S45average_stats")
write.csv(S113stats, "S113stats.csv")
write.csv(W34stats, "W34average_stats")
write.csv(WLPstats, "WLPaverage_stats")





plot_plate_mu<-function(BY4743m, BYstats){
  VVV<-BY4743m%>%
    
    merge(BYstats)
  
  
  VVV %>% 
    mutate(plate = 1) %>% 
    mtp_ggplot(aes(plate = plate, well = well)) + 
    mtp_spec_96well(w=127.76*5, h=85.48*5) + 
    geom_footprint() + 
    geom_col_label() + 
    geom_row_label() + 
    geom_well_rect(aes ( fill = control, alpha = rel_max_GR)) +
    geom_well_line(aes(x = runtime, y = measure)) + 
    geom_well_text(aes(label =str_wrap( sample,w = 10)),  colour="black") + 
    guides(fill = FALSE)
}

plot_plate_BYauc<-function(x, z){
  VVV<-x%>%
    
    merge(z)
  
  
  VVV %>% 
    mutate(plate = 1) %>% 
    mtp_ggplot(aes(plate = plate, well = well)) + 
    mtp_spec_96well(w=127.76*5, h=85.48*5) + 
    geom_footprint() + 
    geom_col_label() + 
    geom_row_label() + 
    geom_well_rect(aes ( fill = control, alpha = BYauc)) +
    geom_well_line(aes(x = runtime, y = measure)) + 
    geom_well_text(aes(label =str_wrap( sample,w = 10)),  colour="black") + 
    guides(fill = FALSE)
}

plot_plate_BYmu<-function(x, z){
  VVV<-x%>%
    
    merge(z)
  
  
  VVV %>% 
    mutate(plate = 1) %>% 
    mtp_ggplot(aes(plate = plate, well = well)) + 
    mtp_spec_96well(w=127.76*5, h=85.48*5) + 
    geom_footprint() + 
    geom_col_label() + 
    geom_row_label() + 
    geom_well_rect(aes ( fill = control, alpha = BYmu)) +
    geom_well_line(aes(x = runtime, y = measure)) + 
    geom_well_text(aes(label =str_wrap( sample,w = 10)),  colour="black") + 
    guides(fill = FALSE)
}

plot_plate_conauc<-function(BY4743m, BYstats){
  VVV<-BY4743m%>%
    
    merge(BYstats)
  
  
  VVV %>% 
    mutate(plate = 1) %>% 
    mtp_ggplot(aes(plate = plate, well = well)) + 
    mtp_spec_96well(w=127.76*5, h=85.48*5) + 
    geom_footprint() + 
    geom_col_label() + 
    geom_row_label() + 
    geom_well_rect(aes ( fill = control, alpha = con_rel_auc)) +
    geom_well_line(aes(x = runtime, y = measure)) + 
    geom_well_text(aes(label =str_wrap( sample,w = 10)),  colour="black") + 
    guides(fill = FALSE)
}

plot_plate_conmu<-function(BY4743m, BYstats){
  VVV<-BY4743m%>%
    
    merge(BYstats)
  
  
  VVV %>% 
    mutate(plate = 1) %>% 
    mtp_ggplot(aes(plate = plate, well = well)) + 
    mtp_spec_96well(w=127.76*5, h=85.48*5) + 
    geom_footprint() + 
    geom_col_label() + 
    geom_row_label() + 
    geom_well_rect(aes ( fill = control, alpha = con_rel_mu)) +
    geom_well_line(aes(x = runtime, y = measure)) + 
    geom_well_text(aes(label =str_wrap( sample,w = 10)),  colour="black") + 
    guides(fill = FALSE)
}


AVE_plot_plate_mu<-function(BYaverage_stats,BY4743m){
  well_data(BYaverage_stats,BY4743m) %>% 
    mutate(plate = 1) %>% 
    mtp_ggplot(aes(plate = plate, well = well)) + 
    mtp_spec_96well(w=127.76*5, h=85.48*5) + 
    geom_footprint() + 
    geom_col_label() + 
    geom_row_label() + 
    geom_well_rect(aes ( fill = plate, alpha = AVE_rel_max_GR)) +
    geom_well_line(aes(x = runtime, y = measure)) + 
    geom_well_text(aes(label =str_wrap( sample,w = 10)),  colour="black") + 
    guides(fill = FALSE)
}

AVE_plot_plate_BYauc<-function(BYaverage_stats,BY4743m){
  well_data(BYaverage_stats,BY4743m) %>% 
    mutate(plate = 1) %>% 
    mtp_ggplot(aes(plate = plate, well = well)) + 
    mtp_spec_96well(w=127.76*5, h=85.48*5) + 
    geom_footprint() + 
    geom_col_label() + 
    geom_row_label() + 
    geom_well_rect(aes ( fill = plate, alpha = AVE_BYauc)) +
    geom_well_line(aes(x = runtime, y = measure)) + 
    geom_well_text(aes(label =str_wrap( sample,w = 10)),  colour="black") + 
    guides(fill = FALSE)
}

AVE_plot_plate_BYmu<-function(BYaverage_stats,BY4743m){
  well_data(BYaverage_stats,BY4743m) %>% 
    mutate(plate = 1) %>% 
    mtp_ggplot(aes(plate = plate, well = well)) + 
    mtp_spec_96well(w=127.76*5, h=85.48*5) + 
    geom_footprint() + 
    geom_col_label() + 
    geom_row_label() + 
    geom_well_rect(aes ( fill = plate, alpha = AVE_BYmu)) +
    geom_well_line(aes(x = runtime, y = measure)) + 
    geom_well_text(aes(label =str_wrap( sample,w = 10)),  colour="black") + 
    guides(fill = FALSE)
}

AVE_plot_plate_conauc<-function(BYaverage_stats,BY4743m){
  well_data(BYaverage_stats,BY4743m) %>% 
    mutate(plate = 1) %>% 
    mtp_ggplot(aes(plate = plate, well = well)) + 
    mtp_spec_96well(w=127.76*5, h=85.48*5) + 
    geom_footprint() + 
    geom_col_label() + 
    geom_row_label() + 
    geom_well_rect(aes ( fill = plate, alpha = AVE_con_rel_auc)) +
    geom_well_line(aes(x = runtime, y = measure)) + 
    geom_well_text(aes(label =str_wrap( sample,w = 10)),  colour="black") + 
    guides(fill = FALSE)
}

AVE_plot_plate_conmu<-function(BYaverage_stats,BY4743m){
  well_data(BYaverage_stats,BY4743m) %>% 
    mutate(plate = 1) %>%
    mtp_ggplot(aes(plate = plate, well = well)) + 
    mtp_spec_96well(w=127.76*5, h=85.48*5) + 
    geom_footprint() + 
    geom_col_label() + 
    geom_row_label() + 
    geom_well_rect(aes ( fill = plate, alpha = AVE_con_rel_mu)) +
    geom_well_line(aes(x = runtime, y = measure)) + 
    geom_well_text(aes(label =str_wrap( sample,w = 10)),  colour="black") + 
    guides(fill = FALSE)
}



plot_plate_fitness(BYstats,BY4743m)
plot_plate_mu(BYstats,BY4743m)
plot_plate_BYauc(BYstats,BY4743m)
plot_plate_BYmu(BYstats,BY4743m)
plot_plate_conauc(BYstats,BY4743m)
plot_plate_conmu(BYstats,BY4743m)

plot_ave_fitness(BYaverage_stats,BY4743m)
AVE_plot_plate_mu(BYaverage_stats,BY4743m)
AVE_plot_plate_BYauc(BYaverage_stats,BY4743m)
AVE_plot_plate_BYmu(BYaverage_stats,BY4743m)
AVE_plot_plate_conauc(BYaverage_stats,BY4743m)
AVE_plot_plate_conmu(BYaverage_stats,BY4743m)
