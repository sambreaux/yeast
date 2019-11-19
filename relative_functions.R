
#x= table off plate ave summary measures
#y= table of plate measures
#z = current sample name in ""


off_plate_rel_fit<- function(x, y, z){
  
  rel_scores<-reshape2::dcast(x, compound~sample, fun.aggregate = mean, value.var = "AVE_auc")%>%
    column_to_rownames(var="compound")
  
  off_rel<-y%>% 
    group_by(solvent) %>% 
    bind_rows(., .) %>%
    mutate(rel_fit_off_plate = ifelse(test = (solvent == 'ETOH'), 
                                      yes = auc/rel_scores["ETOH",z],
                                      no = ifelse(test = (solvent == 'ETOH-2'), 
                                                  yes = auc/rel_scores["ETOH",z],
                                                  no = ifelse(test = (solvent == 'DMSO'),
                                                              yes = auc/rel_scores["DMSO",z],
                                                              no = ifelse(test = (solvent =='DMSO-2'),
                                                                          yes = auc/rel_scores["DMSO",z],
                                                                          no = ifelse(test = (solvent =='H2O'),
                                                                                      yes= auc/rel_scores["H2O",z],
                                                                                      no =  auc/rel_scores["H2O",z]))))))%>%
    
    unique()
  }

U5_off_fit<-off_plate_rel_fit(lll, U5stats, 'U5-05')
s14_off_fit<-off_plate_rel_fit(lll,S14stats, "S6-14")



on_plate_rel_fit<- function(x){
  
  rel_scores<-reshape2::dcast(x, well~plate, fun.aggregate = mean, value.var = "auc")%>%
    column_to_rownames(var="well")
  
  on_rel<-x%>% 
    group_by(solvent) %>% 
    bind_rows(., .) %>%
    mutate(rel_fit_on_plate = ifelse(test = (solvent == 'ETOH'), 
                                      yes = auc/rel_scores["C01", 1],
                                      no = ifelse(test = (solvent == 'ETOH-2'), 
                                                  yes = auc/rel_scores["C07", 1],
                                                  no = ifelse(test = (solvent == 'DMSO'),
                                                              yes = auc/rel_scores["B01", 1],
                                                              no = ifelse(test = (solvent =='DMSO-2'),
                                                                          yes = auc/rel_scores["B07", 1],
                                                                          no = ifelse(test = (solvent =='H2O'),
                                                                                      yes= auc/rel_scores["A01", 1],
                                                                                      no =  auc/rel_scores["A07", 1]))))))%>%
    
    unique()
}


s14_on<-on_plate_rel_fit(S14auc)
