library(devtools)
install_github("vqv/ggbiplot")



drug2_mets<-d2_sum_metrics%>%
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

plot(pcs[, c])

ix<-pcs$rotation
