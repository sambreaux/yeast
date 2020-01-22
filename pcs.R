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
  select(-plate, -ave_rel_fit_off_plate, -ave_rel_inhib_off_Plate)

all_plate_average_stats<-aggregate(all_mets[,2:9], by = list(compound = all_mets$compound, strain = all_mets$strain), FUN = mean)
?aggregate

pca<-prcomp(all_plate_average_stats[,3:10 ], center = T, scale = T)
plot(pca)
summary(pca)


fviz_pca_var(pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

var <- get_pca_var(pca)
library("corrplot")
corrplot(var$contrib, is.corr=FALSE)

head(var$contrib, 8)

######################################################
cstrain<-dcast(all_plate_average_stats, compound~strain, fun.aggregate = mean, value.var = "AVE_mu")%>%
  column_to_rownames('compound')

ccstrain<-dcast(all_plate_average_stats, compound~strain, fun.aggregate = mean, value.var = "AVE_mu")

ccompound<-dcast(all_plate_average_stats, strain~compound, fun.aggregate = mean, value.var = "AVE_mu")%>%
  column_to_rownames('strain')

pca_strain<-prcomp(cstrain, center = T, scale = T)
plot(pca_strain)
summary(pca_strain)

fviz_pca_var(pca_strain, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

vars <- get_pca_var(pca_strain)
corrplot(vars$contrib, is.corr=FALSE)
head(vars$contrib, 6)

library(viridis)
fviz_pca_ind(pca_strain,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = ccstrain$compound, # color by groups
             palette = viridis(48),
             legend.title = "Groups"
)


ind <- get_pca_ind(pca_strain)
fviz_pca_ind(pca_strain, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

fviz_contrib(pca_strain, choice = "ind", axes = 1:6)

pccomp<-prcomp(ccompound, center = T, scale = T)
plot(pccomp)
summary(pccomp)


ind <- get_pca_ind(pcs)
fviz_pca_ind(pcs)
ix<-pcs$rotation




