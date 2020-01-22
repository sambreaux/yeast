library(devtools)
library(factoextra)
install_github("vqv/ggbiplot")



drug2_mets<-average_sample_stats%>%
  merge(ps2)


drug3_mets<-d2_sum_metrics3%>%
  merge(ps3)

#remove BY4743
all_mets<-rbind(drug2_mets, drug3_mets) 

all_mets<-all_mets[all_mets$strain != "BY4743", 
]
                   
pcs<-prcomp(all_mets[,c(7:10, 12:17) ], center = T, scale = T)
plot(pcs)
summary(pcs)


ind <- get_pca_ind(pcs)
fviz_pca_ind(pcs)
ix<-pcs$rotation

##########################

drug2_mets_ave<-average_sample_stats%>%
  merge(ps2)


drug3_mets_ave<-average_sample_stats3%>%
  merge(ps3)

#remove BY4743
all_mets<-rbind(drug2_mets_ave, drug3_mets_ave) 

all_mets<-all_mets[all_mets$strain != "BY4743", ]%>%
  select(-plate)

all_plate_average_stats<-aggregate(all_mets[,2:11], by = list(compound = all_mets$compound, strain = all_mets$strain), FUN = mean)
?aggregate

pcs<-prcomp(all_plate_average_stats[,7:12 ], center = T, scale = T)
plot(pcs)
summary(pcs)

cstrain<-dcast(all_plate_average_stats, compound~strain, fun.aggregate = mean, value.var = "AVE_mu")%>%
  column_to_rownames('compound')

ccompound<-dcast(all_plate_average_stats, strain~compound, fun.aggregate = mean, value.var = "AVE_mu")%>%
  column_to_rownames('strain')

pcstrain<-prcomp(cstrain, center = T, scale = T)
plot(pcstrain)
summary(pcstrain)

pccomp<-prcomp(ccompound, center = T, scale = T)
plot(pccomp)
summary(pccomp)


ind <- get_pca_ind(pcs)
fviz_pca_ind(pcs)
ix<-pcs$rotation




