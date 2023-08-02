## DA MERGE

load(file="/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/rawdata/workspace.RData")
source("/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/code/canproj-2022_HJ.R")

inci_cname_M <- inci_cname[inci_cname %in% c("C15_I","C16_I","C18_C20_I","C22_I","C23_C24_I","C25_I","C33_C34_I","C61_I","C64_I","C67_I","C82_C86_C96_I","C91_C95_I")]
mort_cname_M <- mort_cname[mort_cname %in% c("C15_D","C16_D","C18_C21_D","C22_D","C23_C24_D" ,"C25_D","C33_C34_D","C61_D","C64_D","C67_D","C82_C86_D","C91_C95_D")]

inci_cname_F <- inci_cname[inci_cname %in% c("C16_I","C18_C20_I","C22_I","C23_C24_I","C25_I","C33_C34_I","C53_I","C82_C86_C96_I","C91_C95_I")]
mort_cname_F <- mort_cname[mort_cname %in% c("C16_D","C18_C21_D","C22_D","C23_C24_D","C25_D","C33_C34_D","C53_D","C82_C86_D","C91_C95_D")]

title_nameM<-c("Esophagus (C15)","Stomach (C16)","Colorectal (C18-C20) | Colorectal & Anus (C18-C21)","Liver (C22)","Gallbladder (C23-C24)","Pancreas (C25)","Lung (C33-C34)","Prostate (C61)","Kidney (C64)","Bladder (C67)","Non-Hodgkin lymphoma (C82-C86,C96) | (C82-C86)","Leukemia (C91-C95)")
title_nameF<-c("Stomach (C16)","Colorectal (C18-C20) | Colorectal & Anus (C18-C21)","Liver (C22)","Gallbladder (C23-C24)", "Pancreas (C25)","Lung (C33-C34)","Cervix uteri (C53)","Non-Hodgkin lymphoma (C82-C86,C96) | (C82-C86)","Leukemia (C91-C95)")

####################################################################################
# Current DA check
####################################################################################
DA <- function (count, population, reference, comparison){
  ## Reference
  N1     <- sum(population[,colnames(population) %like% as.character(reference)])
  e1     <- count[,colnames(population) %like% as.character(reference)]/population[,colnames(population) %like% as.character(reference)]
  s1     <- (population[,colnames(population) %like% as.character(reference)]/colSums(population)[colnames(population) %like% as.character(reference)])
  
  ## Comparison
  N2     <- sum(population[,colnames(population) %like% as.character(comparison)])
  e2     <- count[,colnames(population) %like% as.character(comparison)]/population[,colnames(population) %like% as.character(comparison)]
  s2     <- population[,colnames(population) %like% as.character(comparison)]/colSums(population)[colnames(population) %like% as.character(comparison)]
  
  M_p  <-c()
  M_a  <-c()
  M_e  <-c()
  I_pa <-c()
  I_pe <-c()
  I_ae <-c()
  I_pae<-c()
  
  for ( i in 1:18){
    M_p[i]   <- sum((N2-N1)*s1[i]*e1[i])
    M_a[i]   <- sum(N1*(s2[i]-s1[i])*e1[i])
    M_e[i]   <- sum(N1*s1[i]*(e2[i]-e1[i]))
    I_pa[i]  <- sum((N2-N1)*(s2[i]-s1[i])*e1[i])
    I_pe[i]  <- sum((N2-N1)*s1[i]*(e2[i]-e1[i]))
    I_ae[i]  <- sum(N1*(s2[i]-s1[i])*(e2[i]-e1[i]))
    I_pae[i] <- sum((N2-N1)*(s2[i]-s1[i])*(e2[i]-e1[i]))
  }
  
  M_p  <-sum(M_p)
  M_a  <-sum(M_a)
  M_e  <-sum(M_e)
  I_pa <-sum(I_pa)
  I_pe <-sum(I_pe)
  I_ae <-sum(I_ae)
  I_pae<-sum(I_pae)
  
  
  # Effect of changes in population size 
  P <-  (M_p + (1/2 * I_pe) + (1/2 * I_pa) + (1/3 * I_pae))
  
  # Effect of changes in  age structure
  A <-  (M_a + (1/2 * I_ae) + (1/2 * I_pa) + (1/3 * I_pae))
  
  # Effect of changes in mortality  or Incidence
  E <-  (M_e + (1/2 * I_pe) + (1/2 * I_ae) + (1/3 * I_pae))
  
  method3        <- list(P,A,E)
  names(method3) <- c("P","A","E")
  return(method3)
}


ageing_effect_check <- function(Sex,Extract_M) {
  if (Sex == 1) {
    changes_history <-list()
    result <-list()
    for (j in 1:12){
      
      ### Incidence
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 20)
      
      for (i in 1:20){
        case <- (as.numeric(as.vector(unlist(DA(subset(get(inci_cname_M[j]),Sex=="害切")[,-c(1:3,ncol(get(inci_cname_M[j])))],current_population_male[,-1],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum((subset(get(inci_cname_M[j]),Sex=="害切")[,-c(1:3,ncol(get(inci_cname_M[j])))])[,i])-sum((subset(get(inci_cname_M[j]),Sex=="害切")[,-c(1:3,ncol(get(inci_cname_M[j])))])[,1])
      }
      
      years = c(2000:2019)
      change_inc <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                           Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 20))
      change_inc$Group <- factor(change_inc$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change_inc$Type <- c("Incidence")
      
      ### Mortality
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 20)
      
      for (i in 1:20){
        case <- (as.numeric(as.vector(unlist(DA(subset(get(mort_cname_M[j]),Sex=="害切")[,-c(1:3,ncol(get(mort_cname_M[j]))-1,ncol(get(mort_cname_M[j])))],current_population_male[,-1],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum((subset(get(mort_cname_M[j]),Sex=="害切")[,-c(1:3,ncol(get(mort_cname_M[j]))-1,ncol(get(mort_cname_M[j])))])[,i])-sum((subset(get(mort_cname_M[j]),Sex=="害切")[,-c(1:3,ncol(get(mort_cname_M[j]))-1,ncol(get(mort_cname_M[j])))])[,6])
      }
      
      years = c(2000:2019)
      change_mort <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                           Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 20))
      change_mort$Group <- factor(change_mort$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change_mort$Type <- c("Mortality")

      change <- rbind(change_inc,change_mort)
      
      result[[j]] <- ggplot(data = change, aes(x = Year, y = Change,group = Group,colour = Group,linetype = Group)) + geom_line(size=2.0) + 
               scale_x_continuous(breaks = seq(2000, 2019, 5)) + geom_hline(yintercept = 0, color = 'black', linetype = 2,size=1.6) +
               facet_wrap(~ Type, strip.position = "top", nrow = 1) +
               theme_stata() + scale_color_stata() + 
               theme(legend.position = "none",
                                   text=element_text(size=30),
                                   axis.text.x=element_text(angle=90, hjust=1),
                                   plot.title = element_text(size = 30, face = "bold")) + 
               labs(title=title_nameM[j], x = 'Calender Year',y = "Difference Total Incidence Case")
      
      changes_history[[j]] <- change
    } 
  } 
  else if (Sex == 2) {
    changes_history <-list()
    result <-list()
    for (j in 1:9){
      ### Incidence
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 20)
      
      for (i in 1:20){
        case <- (as.numeric(as.vector(unlist(DA(subset(get(inci_cname_F[j]),Sex=="食切")[,-c(1:3,ncol(get(inci_cname_F[j])))],current_population_female[,-1],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum((subset(get(inci_cname_F[j]),Sex=="食切")[,-c(1:3,ncol(get(inci_cname_F[j])))])[,i])-sum((subset(get(inci_cname_F[j]),Sex=="食切")[,-c(1:3,ncol(get(inci_cname_F[j])))])[,1])
      }
      
      years = c(2000:2019)
      change_inc <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                               Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 20))
      change_inc$Group <- factor(change_inc$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change_inc$Type <- c("Incidence")
      
      
      ### Mortality
      P_year <- A_year <- E_year <- net_change <- numeric(length = 20)
      
      for (i in 1:20){
        case <- (as.numeric(as.vector(unlist(DA(subset(get(mort_cname_F[j]),Sex=="食切")[,-c(1:3,ncol(get(mort_cname_F[j]))-1,ncol(get(mort_cname_F[j])))],current_population_female[,-1],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum((subset(get(mort_cname_F[j]),Sex=="食切")[,-c(1:3,ncol(get(mort_cname_F[j]))-1,ncol(get(mort_cname_F[j])))])[,i])-sum((subset(get(mort_cname_F[j]),Sex=="食切")[,-c(1:3,ncol(get(mort_cname_F[j]))-1,ncol(get(mort_cname_F[j])))])[,6])
      }
      
      years = c(2000:2019)
      change_mort <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                                Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 20))
      change_mort$Group <- factor(change_mort$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change_mort$Type <- c("Mortality")
      
      change <- rbind(change_inc,change_mort)
      
      result[[j]] <- ggplot(data = change, aes(x = Year, y = Change,group = Group,colour = Group,linetype = Group)) + geom_line(size=2.0) + 
               scale_x_continuous(breaks = seq(2000, 2019, 5)) + geom_hline(yintercept = 0, color = 'black', linetype = 2,size=1.6) +
               facet_wrap(~ Type, strip.position = "top", nrow = 1) +
               theme_stata() + scale_color_stata() + 
               theme(legend.position = "none",
                     text=element_text(size=30),
                     axis.text.x=element_text(angle=90, hjust=1),
                     plot.title = element_text(size = 30, face = "bold")) + 
               labs(title=title_nameF[j], x = 'Calender Year',y = "Difference Total Death Case")
      changes_history[[j]] <- change
    }
  }
  
  if (Extract_M=="Table") {
    return(changes_history)
  } else if (Extract_M=="Plot"){
    return(result)
  }
}

# Male
ageing_effect_check(1,"Plot")

# FeMale
ageing_effect_check(2,"Plot")



########################################################################################################################
NORDPRED_func_modif <- function (Scenarios1_male,Scenarios1_female){
  
  NORDPRED_INCI_M <-list()
  NORDPRED_INCI_F <-list()
  
  for (i in 1: length(inci_cname_M)){
    male_case <-subset(get(inci_cname_M[i]),Sex=="害切")[,-c(1:3,24)]
    male_pop  <- Scenarios1_male[, c(as.character(2000:2040))]
    NORDPRED_INCI_M[[i]] <-canproj(male_case,male_pop,startp=2020,startestage=7, methods="nordpred")$agsproj*(male_pop)/100000
  }
  for (i in 1: length(inci_cname_F)){
    female_case <-subset(get(inci_cname_F[i]),Sex=="食切")[,-c(1:3,24)]
    female_pop  <- Scenarios1_female[, c(as.character(2000:2040))]
    NORDPRED_INCI_F[[i]] <-canproj(female_case,female_pop,startp=2020,startestage=7, methods="nordpred")$agsproj*(female_pop)/100000
  }
  
  NORDPRED_MORT_M <-list()
  NORDPRED_MORT_F <-list()
  
  for ( i in 1: length(mort_cname_M)){
    male_case <- subset(get(mort_cname_M[i]),Sex=="害切")[,-c(1:3,24,25)]
    male_pop  <- Scenarios1_male[, c(as.character(2000:2040))]
    if(length(which(rowSums(male_case/20) <1)[length(which(rowSums(male_case/20) <1))]) == 0){
      NORDPRED_MORT_M[[i]] <-canproj(male_case,male_pop,startp=2020,startestage=7, methods="nordpred")$agsproj*(male_pop)/100000
    } else if (length(which(rowSums(male_case/20) <1)[length(which(rowSums(male_case/20) <1))]) !=0) {
      if (which(rowSums(male_case/20) <1)[length(which(rowSums(male_case/20) <1))] >7){  
        num = as.numeric(which(rowSums(male_case/20) <1)[length(which(rowSums(male_case /20) <1))])+1
        NORDPRED_MORT_M[[i]] <-canproj(male_case,male_pop,startp=2020,startestage=num, methods="nordpred")$agsproj*(male_pop)/100000
      } else {
        NORDPRED_MORT_M[[i]] <-canproj(male_case,male_pop,startp=2020,startestage=7, methods="nordpred")$agsproj*(male_pop)/100000
      }
    }
  }
  for ( i in 1: length(mort_cname_F)){
    female_case <- subset(get(mort_cname_F[i]),Sex=="食切")[,-c(1:3,24,25)]
    female_pop  <- Scenarios1_female[, c(as.character(2000:2040))]
    if(length(which(rowSums(female_case/20) <1)[length(which(rowSums(female_case/20) <1))]) == 0){
      NORDPRED_MORT_F[[i]] <-canproj(female_case,female_pop,startp=2020,startestage=7, methods="nordpred")$agsproj*(female_pop)/100000
    } else if (length(which(rowSums(female_case/20) <1)[length(which(rowSums(female_case/20) <1))]) !=0) {
      if (which(rowSums(female_case/20) <1)[length(which(rowSums(female_case/20) <1))] >7){  
        num = as.numeric(which(rowSums(female_case/20) <1)[length(which(rowSums(female_case /20) <1))])+1
        NORDPRED_MORT_F[[i]] <-canproj(female_case,female_pop,startp=2020,startestage=num, methods="nordpred")$agsproj*(female_pop)/100000
      } else {
        NORDPRED_MORT_F[[i]] <-canproj(female_case,female_pop,startp=2020,startestage=7, methods="nordpred")$agsproj*(female_pop)/100000
      }
    }
  }
  NORDPRED_LIST <-list(NORDPRED_INCI_M,NORDPRED_INCI_F,NORDPRED_MORT_M,NORDPRED_MORT_F)
  return(NORDPRED_LIST)
}

scene1_nd_byage<-NORDPRED_func_modif(Scenarios1_male,Scenarios1_female)
scene1_nd_byage_male_inc <- scene1_nd_byage[[1]]
scene1_nd_byage_male_mort <- scene1_nd_byage[[3]]
scene1_nd_byage_female_inc <- scene1_nd_byage[[2]]
scene1_nd_byage_female_mort <- scene1_nd_byage[[4]]

BAPC_func_modif <- function (Scenarios1_male,Scenarios1_female,comparison){
  cancer_pred_sample <- matrix(data = NA, nrow = 21, ncol = 18) %>% as.data.frame() 
  rownames(cancer_pred_sample) <- c(seq(2020,2040,1))
  colnames(cancer_pred_sample) <-  c(1:18)
  
  BAPC_INCI_M <-list()
  BAPC_INCI_F <-list()
  
  for ( i in 1:length(inci_cname_M)){
    male_case <- subset(get(inci_cname_M[i]),Sex=="害切")[,-c(1:3,24)]
    male_pop  <- Scenarios1_male[, c(as.character(2000:2040))]
    test_M_pop <- data.frame(t(male_pop));colnames(test_M_pop) <- c(1:18)
    test_M_ca <- data.frame(t(male_case));colnames(test_M_ca)  <- c(1:18)
    test_M_ca<-rbind(test_M_ca, cancer_pred_sample);colnames(test_M_ca)<- c(1:18)
    
    test_M_ca_rate <- agespec.proj(x = BAPC(APCList(test_M_ca, test_M_pop, gf=5), predict = list(npredict = 21, retro = TRUE),
                                            model = list(age = list(model = "rw2", prior="loggamma", param = c(1, 0.005), initial = 4, scale.model=FALSE),
                                                         period = list(include = TRUE, model = "drift", prior="loggamma", param = c(1, 0.005), initial = 4, scale.model=FALSE),
                                                         cohort = list(include = TRUE, model = "drift", prior="loggamma", param = c(1, 0.005), initial = 4, scale.model=FALSE),
                                                         overdis = list(include = FALSE, model = "iid", prior="loggamma", param = c(1, 0.005), initial = 4)),
                                            secondDiff = FALSE, stdweight = NULL, verbose = FALSE)) %>% as.data.frame()
    
    data_sample <- test_M_ca_rate[,colnames(test_M_ca_rate) %like% '%mean']
    colnames(data_sample) <-agegroups
    data_sample<-data.frame(t(data_sample))
    colnames(data_sample) <-c(2000:2040)
    obs_pred<-data.frame(scene1_nd_byage_male_inc[i])
    obs<-obs_pred[,c(1:20)]
    data_sample<-cbind(obs,data_sample[,(21:41)])
    colnames(data_sample) <-c(2000:2040)
    BAPC_INCI_M[[i]] <- data_sample
  }
  
  for ( i in 1:length(inci_cname_F)){
    female_case <- subset(get(inci_cname_F[i]),Sex=="食切")[,-c(1:3,24)]
    female_pop  <- Scenarios1_female[, c(as.character(2000:2040))]
    test_F_pop <- data.frame(t(female_pop));colnames(test_F_pop) <- c(1:18)
    test_F_ca <- data.frame(t(female_case));colnames(test_F_ca)  <- c(1:18)
    test_F_ca<-rbind(test_F_ca, cancer_pred_sample);colnames(test_F_ca)<- c(1:18)
    
    test_F_ca_rate <- agespec.proj(x = BAPC(APCList(test_F_ca, test_F_pop, gf=5), predict = list(npredict = 21, retro = TRUE),
                                            model = list(age = list(model = "rw2", prior="loggamma", param = c(1, 0.005), initial = 4, scale.model=FALSE),
                                                         period = list(include = TRUE, model = "drift", prior="loggamma", param = c(1, 0.005), initial = 4, scale.model=FALSE),
                                                         cohort = list(include = TRUE, model = "drift", prior="loggamma", param = c(1, 0.005), initial = 4, scale.model=FALSE),
                                                         overdis = list(include = FALSE, model = "iid", prior="loggamma", param = c(1, 0.005), initial = 4)),
                                            secondDiff = FALSE, stdweight = NULL, verbose = FALSE)) %>% as.data.frame()
    
    data_sample <- test_F_ca_rate[,colnames(test_F_ca_rate) %like% '%mean']
    colnames(data_sample) <-agegroups
    data_sample<-data.frame(t(data_sample))
    colnames(data_sample) <-c(2000:2040)
    obs_pred<-data.frame(scene1_nd_byage_female_inc[i])
    obs<-obs_pred[,c(1:20)]
    data_sample<-cbind(obs,data_sample[,(21:41)])
    colnames(data_sample) <-c(2000:2040)
    BAPC_INCI_F[[i]] <- data_sample
  }
  
  BAPC_MORT_M <-list()
  BAPC_MORT_F <-list()
  
  for ( i in 1:length(mort_cname_M)){
    male_case <- subset(get(mort_cname_M[i]),Sex=="害切")[,-c(1:3,24,25)]
    male_pop  <- Scenarios1_male[, c(as.character(2000:2040))]
    test_M_pop <- data.frame(t(male_pop));colnames(test_M_pop) <- c(1:18)
    test_M_ca <- data.frame(t(male_case));colnames(test_M_ca)  <- c(1:18)
    test_M_ca<-rbind(test_M_ca, cancer_pred_sample);colnames(test_M_ca)<-  c(1:18)
    
    test_M_ca_rate <- agespec.proj(x = BAPC(APCList(test_M_ca, test_M_pop, gf=5), predict = list(npredict = 21, retro = TRUE),
                                            model = list(age = list(model = "rw2", prior="loggamma", param = c(1, 0.005), initial = 4, scale.model=FALSE),
                                                         period = list(include = TRUE, model = "drift", prior="loggamma", param = c(1, 0.005), initial = 4, scale.model=FALSE),
                                                         cohort = list(include = TRUE, model = "drift", prior="loggamma", param = c(1, 0.005), initial = 4, scale.model=FALSE),
                                                         overdis = list(include = FALSE, model = "iid", prior="loggamma", param = c(1, 0.005), initial = 4)),
                                            secondDiff = FALSE, stdweight = NULL, verbose = FALSE)) %>% as.data.frame()
    
    data_sample <- test_M_ca_rate[,colnames(test_M_ca_rate) %like% '%mean']
    colnames(data_sample) <-agegroups
    data_sample<-data.frame(t(data_sample))
    colnames(data_sample) <-c(2000:2040)
    obs_pred<-data.frame(scene1_nd_byage_male_mort[i])
    obs<-obs_pred[,c(1:20)]
    data_sample<-cbind(obs,data_sample[,(21:41)])
    colnames(data_sample) <-c(2000:2040)
    BAPC_MORT_M[[i]] <- data_sample
  }
  
  for ( i in 1:length(mort_cname_F)){
    female_case <- subset(get(mort_cname_F[i]),Sex=="食切")[,-c(1:3,24,25)]
    female_pop  <- Scenarios1_female[, c(as.character(2000:2040))]
    test_F_pop <- data.frame(t(female_pop));colnames(test_F_pop) <- c(1:18)
    test_F_ca <- data.frame(t(female_case));colnames(test_F_ca)  <- c(1:18)
    test_F_ca<-rbind(test_F_ca, cancer_pred_sample);colnames(test_F_ca)<- c(1:18)
    
    test_F_ca_rate <- agespec.proj(x = BAPC(APCList(test_F_ca, test_F_pop, gf=5), predict = list(npredict = 21, retro = TRUE),
                                            model = list(age = list(model = "rw2", prior="loggamma", param = c(1, 0.005), initial = 4, scale.model=FALSE),
                                                         period = list(include = TRUE, model = "drift", prior="loggamma", param = c(1, 0.005), initial = 4, scale.model=FALSE),
                                                         cohort = list(include = TRUE, model = "drift", prior="loggamma", param = c(1, 0.005), initial = 4, scale.model=FALSE),
                                                         overdis = list(include = FALSE, model = "iid", prior="loggamma", param = c(1, 0.005), initial = 4)),
                                            secondDiff = FALSE, stdweight = NULL, verbose = FALSE)) %>% as.data.frame()
    
    data_sample <- test_F_ca_rate[,colnames(test_F_ca_rate) %like% '%mean']
    colnames(data_sample) <-agegroups
    data_sample<-data.frame(t(data_sample))
    colnames(data_sample) <-c(2000:2040)
    obs_pred<-data.frame(scene1_nd_byage_female_mort[i])
    obs<-obs_pred[,c(1:20)]
    data_sample<-cbind(obs,data_sample[,(21:41)])
    colnames(data_sample) <-c(2000:2040)
    BAPC_MORT_F[[i]] <-data_sample
  }
  BAPC_LIST <-list(BAPC_INCI_M,BAPC_INCI_F,BAPC_MORT_M,BAPC_MORT_F)
  return(BAPC_LIST)
}

scene1_bp_byage<-BAPC_func_modif(Scenarios1_male,Scenarios1_female)

scene1_bp_byage_male_inc <- scene1_bp_byage[[1]]
scene1_bp_byage_male_mort <- scene1_bp_byage[[3]]
scene1_bp_byage_female_inc <- scene1_bp_byage[[2]]
scene1_bp_byage_female_mort <- scene1_bp_byage[[4]]

############################################################################################################################################################

ageing_effect_check_modif <- function(Sex,Method,Extract_M) {
  if (Sex == 1 & Method == 1) {
    changes_history <-list()
    for (j in 1:length(inci_cname_M)){
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene1_nd_byage_male_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios1_male[,-c(1:5)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
      }
      
      years = c(2000:2040)
      change <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                           Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change$Group <- factor(change$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change$Type <-c("Nordpred")
      
      #######
      P_year_d <- A_year_d <- E_year_d <- net_change_d <- numeric(length = 41)
      sample_d<-data.frame(scene1_bp_byage_male_inc[j])
      for (i in 1:41){
        case_d <- (as.numeric(as.vector(unlist(DA(sample_d,Scenarios1_male[,-c(1:5)],2000,1999+i)))))
        P_year_d[i] <- case_d[1]
        A_year_d[i] <- case_d[2]
        E_year_d[i] <- case_d[3]
        net_change_d[i] <- sum(sample_d[,i])-sum(sample_d[,1])
      }
      
      years = c(2000:2040)
      change_d <- data.frame(Year = years, Change = c(P_year_d, A_year_d, E_year_d, net_change_d), 
                           Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change_d$Group <- factor(change_d$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change_d$Type <-c("BAPC")
      
      change_total <- rbind(change,change_d)
      change_total$Type <- factor(change_total$Type,levels=c("Nordpred","BAPC"))
      
      assign(paste0("plot",j),ggplot(data = change_total, aes(x = Year, y = Change,group = Group,colour = Group,linetype = Group)) + geom_line() + 
               scale_x_continuous(breaks = seq(2000, 2040, 5)) + geom_hline(yintercept = 0, color = 'black', linetype = 2) +
               theme_void(base_family = "AppleSDGothicNeo-Regular") +
               facet_wrap(~ Type, strip.position = "top", nrow = 1) +
               theme_few() + theme(legend.position = "top",text=element_text(size=10),axis.text.x=element_text(angle=90, hjust=1),legend.text = element_text(size = 15),legend.title = element_text(size = 15),plot.title = element_text(size = 15, face = "bold")) + 
               labs(title=title_nameM_I[j], x = 'Calender Year',y = "Difference Total Incidence Case (reference 2000)",family = "AppleSDGothicNeo-ExtraBold"))
      
      changes_history[[j]] <- change_total
      
    } 
  }
  
  else if (Sex == 1 & Method == 2) {
    changes_history <-list()
    for (j in 1:length(mort_cname_M)){
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene1_nd_byage_male_mort[j])
      
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios1_male[,-c(1:5)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
      }
      
      years = c(2000:2040)
      change <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                           Group = rep(c("Population Growth risk", "Population Aging risk", "Mortality Rate risk", "Net Change"), each = 41))
      change$Group <- factor(change$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Mortality Rate risk"))
      change$Type <-c("Nordpred")
      ######
      P_year_d <- A_year_d <- E_year_d <- net_change_d <- numeric(length = 41)
      sample_d<-data.frame(scene1_bp_byage_male_mort[j])
      for (i in 1:41){
        case_d <- (as.numeric(as.vector(unlist(DA(sample_d,Scenarios1_male[,-c(1:5)],2005,1999+i)))))
        P_year_d[i] <- case_d[1]
        A_year_d[i] <- case_d[2]
        E_year_d[i] <- case_d[3]
        net_change_d[i] <- sum(sample_d[,i])-sum(sample_d[,6])
      }
      
      years = c(2000:2040)
      change_d <- data.frame(Year = years, Change = c(P_year_d, A_year_d, E_year_d, net_change_d), 
                             Group = rep(c("Population Growth risk", "Population Aging risk", "Mortality Rate risk", "Net Change"), each = 41))
      change_d$Group <- factor(change_d$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Mortality Rate risk"))
      change_d$Type <-c("BAPC")
      
      change_total <- rbind(change,change_d)
      change_total$Type <- factor(change_total$Type,levels=c("Nordpred","BAPC"))
      
      assign(paste0("plot",j),ggplot(data = change_total, aes(x = Year, y = Change,group = Group,colour = Group,linetype = Group)) + geom_line() + 
               scale_x_continuous(breaks = seq(2000, 2040, 5)) + geom_hline(yintercept = 0, color = 'black', linetype = 2)+ 
               facet_wrap(~ Type, strip.position = "top", nrow = 1) +
               theme_few() + theme(legend.position = "top",text=element_text(size=10),axis.text.x=element_text(angle=90, hjust=1),legend.text = element_text(size = 15),legend.title = element_text(size = 15),plot.title = element_text(size = 15, face = "bold")) + 
               labs(title=title_nameM_D[j], x = 'Calender Year',y = "Difference Total Death Case (reference 2005)",family = "AppleSDGothicNeo-ExtraBold"))
      
      changes_history[[j]] <- change_total
      
    }
  }
  else if (Sex == 2 & Method == 1) {
    changes_history <-list()
    for (j in 1:length(inci_cname_F)){
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene1_nd_byage_female_inc[j])
      
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios1_female[,-c(1:5)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
      }
      years = c(2000:2040)
      change <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                           Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change$Group <- factor(change$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change$Type <-c("Nordpred")
      ######
      P_year_d <- A_year_d <- E_year_d <- net_change_d <- numeric(length = 41)
      sample_d<-data.frame(scene1_bp_byage_female_inc[j])
      for (i in 1:41){
        case_d <- (as.numeric(as.vector(unlist(DA(sample_d,Scenarios1_female[,-c(1:5)],2000,1999+i)))))
        P_year_d[i] <- case_d[1]
        A_year_d[i] <- case_d[2]
        E_year_d[i] <- case_d[3]
        net_change_d[i] <- sum(sample_d[,i])-sum(sample_d[,1])
      }
      
      years = c(2000:2040)
      change_d <- data.frame(Year = years, Change = c(P_year_d, A_year_d, E_year_d, net_change_d), 
                             Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change_d$Group <- factor(change_d$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change_d$Type <-c("BAPC")
      
      change_total <- rbind(change,change_d)
      change_total$Type <- factor(change_total$Type,levels=c("Nordpred","BAPC"))
      
      assign(paste0("plot",j),ggplot(data = change_total, aes(x = Year, y = Change,group = Group,colour = Group,linetype = Group)) + geom_line() + 
               scale_x_continuous(breaks = seq(2000, 2040, 5)) + geom_hline(yintercept = 0, color = 'black', linetype = 2) + 
               facet_wrap(~ Type, strip.position = "top", nrow = 1) +
               theme_few() + theme(legend.position = "top",text=element_text(size=10),axis.text.x=element_text(angle=90, hjust=1),legend.text = element_text(size = 15),legend.title = element_text(size = 15),plot.title = element_text(size = 15, face = "bold")) + 
               labs(title=title_nameF_I[j], x = 'Calender Year',y = "Difference Total Incidence Case (reference 2000)",family = "AppleSDGothicNeo-ExtraBold"))
      
      changes_history[[j]] <- change_total
    }
  }
  else if (Sex == 2 & Method == 2) {
    changes_history <-list()
    for (j in 1:length(mort_cname_F)){
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene1_nd_byage_female_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios1_female[,-c(1:5)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
      }
      
      years = c(2000:2040)
      change <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                           Group = rep(c("Population Growth risk", "Population Aging risk", "Mortality Rate risk", "Net Change"), each = 41))
      change$Group <- factor(change$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Mortality Rate risk"))
      change$Type <-c("Nordpred")
      ######
      P_year_d <- A_year_d <- E_year_d <- net_change_d <- numeric(length = 41)
      sample_d<-data.frame(scene1_bp_byage_female_mort[j])
      
      for (i in 1:41){
        case_d <- (as.numeric(as.vector(unlist(DA(sample_d,Scenarios1_female[,-c(1:5)],2005,1999+i)))))
        P_year_d[i] <- case_d[1]
        A_year_d[i] <- case_d[2]
        E_year_d[i] <- case_d[3]
        net_change_d[i] <- sum(sample_d[,i])-sum(sample_d[,6])
      }
      
      years = c(2000:2040)
      change_d <- data.frame(Year = years, Change = c(P_year_d, A_year_d, E_year_d, net_change_d), 
                             Group = rep(c("Population Growth risk", "Population Aging risk", "Mortality Rate risk", "Net Change"), each = 41))
      change_d$Group <- factor(change_d$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Mortality Rate risk"))
      change_d$Type <-c("BAPC")
      
      change_total <- rbind(change,change_d)
      change_total$Type <- factor(change_total$Type,levels=c("Nordpred","BAPC"))
      
      assign(paste0("plot",j),ggplot(data = change_total, aes(x = Year, y = Change,group = Group,colour = Group,linetype = Group)) + geom_line() + 
               scale_x_continuous(breaks = seq(2000, 2040,5)) + geom_hline(yintercept = 0, color = 'black', linetype = 2)+ 
               facet_wrap(~ Type, strip.position = "top", nrow = 1) +
               theme_few() + theme(legend.position = "top",text=element_text(size=10),axis.text.x=element_text(angle=90, hjust=1),legend.text = element_text(size = 15),legend.title = element_text(size = 15),plot.title = element_text(size = 15, face = "bold")) + 
               labs(title=title_nameF_D[j], x = 'Calender Year',y = "Difference Total Death Case (reference 2005) ",family = "AppleSDGothicNeo-ExtraBold"))
      
      changes_history[[j]] <- change_total
      
    }
  }
  
  if (Extract_M=="Table") {
    
    return(changes_history)
    
  } else if (Extract_M=="Plot"){
    if (Sex == 1 & Method == 1 ) {
      plt1<-ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
      plt2<-ggarrange(plot5,plot6,plot7,plot8,labels = c("E","F","G","H"),ncol = 2, nrow = 2, common.legend = TRUE)
      plt3<-ggarrange(plot9,labels = c("I"),ncol = 2, nrow = 2, common.legend = TRUE)
      result <- list(plt1,plt2,plt3)
      plt <-ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,labels = c("A","B","C","D","E","F","G","H","I"),ncol = 2, nrow = 5, common.legend = TRUE)
      return(plt)
    } else if(Sex == 1 & Method == 2){
      plt1<-ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
      plt2<-ggarrange(plot5,plot6,plot7,plot8,labels = c("E","F","G","H"),ncol = 2, nrow = 2, common.legend = TRUE)
      plt3<-ggarrange(plot9,labels = c("I"),ncol = 2, nrow = 2, common.legend = TRUE)
      result <- list(plt1,plt2,plt3)
      plt <-ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,labels = c("A","B","C","D","E","F","G","H","I"),ncol = 2, nrow = 5, common.legend = TRUE)
      
      return(plt)
    }
    else if (Sex == 2 & Method == 1){
      plt1<-ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
      plt2<-ggarrange(plot5,plot6,labels = c("E","F"),ncol = 2, nrow = 2, common.legend = TRUE)
      result <- list(plt1,plt2)
      plt <-ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,labels = c("A","B","C","D","E","F"),ncol = 2, nrow = 5, common.legend = TRUE)
      return(plt)
      
    } else if (Sex == 2 & Method == 2) {
      plt1<-ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
      plt2<-ggarrange(plot5,plot6,plot7,plot8,labels = c("E","F","G","H"),ncol = 2, nrow = 2, common.legend = TRUE)
      result <- list(plt1,plt2)
      plt <-ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,labels = c("A","B","C","D","E","F","G","H"),ncol = 2, nrow = 5, common.legend = TRUE)
      
      return(plt)
    }
  }
}
# Male, Incidence
ageing_effect_check_modif(1,1,"Table")
ageing_effect_check_modif(1,1,"Plot")
# Male, Death
ageing_effect_check_modif(1,2,"Table")
ageing_effect_check_modif(1,2,"Plot")
# Female, Incidence
ageing_effect_check_modif(2,1,"Table")
ageing_effect_check_modif(2,1,"Plot")
# Female, Death
ageing_effect_check_modif(2,2,"Table")
ageing_effect_check_modif(2,2,"Plot")

sample1<-ageing_effect_check(1,"Table")
sample2<-ageing_effect_check(2,"Table")

sample_by1 <-list()
sample_by2 <-list()
sample_by3 <-list()
sample_by4 <-list()

for (i in 1:length(inci_cname_M)){
  sample_by1[[i]] <- subset(sample1[[i]],Type == "Incidence" & Group %in% c("Net Change","Population Aging risk") & Year %in% c(2000,2005,2010,2015,2019))
}

for (i in 1:length(mort_cname_M)){
  sample_by2[[i]] <- subset(sample1[[i]],Type == "Mortality" & Group %in% c("Net Change","Population Aging risk")& Year %in% c(2000,2005,2010,2015,2019))
}

for (i in 1:length(inci_cname_F)){
  sample_by3[[i]] <- subset(sample2[[i]],Type == "Incidence" & Group %in% c("Net Change","Population Aging risk")& Year %in% c(2000,2005,2010,2015,2019))
}

for (i in 1:length(mort_cname_F)){
  sample_by4[[i]] <- subset(sample2[[i]],Type == "Mortality" & Group %in% c("Net Change","Population Aging risk")& Year %in% c(2000,2005,2010,2015,2019))
}

rbind(t(reshape(subset(subset(sample_by1[[1]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by1[[2]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by1[[3]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by1[[4]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by1[[5]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by1[[6]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by1[[7]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by1[[8]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by1[[9]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by1[[10]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by1[[11]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by1[[12]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]))

rbind(t(reshape(subset(subset(sample_by2[[1]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by2[[2]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by2[[3]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by2[[4]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by2[[5]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by2[[6]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019,))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by2[[7]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019,))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by2[[8]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by2[[9]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by2[[10]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by2[[11]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by2[[12]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]))

####

rbind(t(reshape(subset(subset(sample_by3[[1]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by3[[2]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by3[[3]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by3[[4]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by3[[5]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by3[[6]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by3[[7]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by3[[8]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by3[[9]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]))

rbind(t(reshape(subset(subset(sample_by4[[1]],Group=="Percent"),Year %in% c(2005,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by4[[2]],Group=="Percent"),Year %in% c(2005,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by4[[3]],Group=="Percent"),Year %in% c(2005,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by4[[4]],Group=="Percent"),Year %in% c(2005,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by4[[5]],Group=="Percent"),Year %in% c(2005,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by4[[6]],Group=="Percent"),Year %in% c(2005,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by4[[7]],Group=="Percent"),Year %in% c(2005,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by4[[8]],Group=="Percent"),Year %in% c(2005,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample_by3[[9]],Group=="Percent"),Year %in% c(2000,2005,2010,2005,2019,))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]))

