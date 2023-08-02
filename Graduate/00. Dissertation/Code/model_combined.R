library(openxlsx)
library(dplyr)
library(reshape2)
library(tidyr)
library(DescTools)
library(extrafont)
library(ggthemes)
library(ggplot2)
library(viridis)
library(ggpubr)
library(hrbrthemes)
library(INLA)
library(BAPC)
library(graphics)
library(magrittr)
library(incAnalysis)
library(splines)
library(tidyverse)
library(reshape)
library(colorspace)

load(file="/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/rawdata/workspace.RData")
source("/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/code/canproj-2022_HJ.R")
agegroups <- c("00-04","05-09","10-14","15-19","20-24","25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84","85+" )

inci_cname_M <- inci_cname[inci_cname %in% c("C15_I","C16_I","C18_C20_I","C22_I","C23_C24_I","C25_I","C33_C34_I","C61_I","C64_I","C67_I","C82_C86_C96_I","C91_C95_I")]
mort_cname_M <- mort_cname[mort_cname %in% c("C15_D","C16_D","C18_C21_D","C22_D","C23_C24_I" ,"C25_D","C33_C34_D","C61_D","C64_D","C67_I","C82_C86_D","C91_C95_D")]

inci_cname_F <- inci_cname[inci_cname %in% c("C16_I","C18_C20_I","C22_I","C23_C24_I","C25_I","C33_C34_I","C53_I","C82_C86_C96_I","C91_C95_I")]
mort_cname_F <- mort_cname[mort_cname %in% c("C16_D","C18_C21_D","C22_D","C23_C24_D","C25_D","C33_C34_D","C53_D","C82_C86_D","C91_C95_D")]

title_nameM_I<-c("Esophagus (C15)","Stomach (C16)","Colorectal (C18-C20)","Liver (C22)","Gallbladder (C23-C24)","Pancreas (C25)","Lung (C33-C34)","Prostate (C61)","Kidney (C64)","Bladder (C67)","Non-Hodgkin lymphoma (C82-C86)","Leukemia (C91-C95)")
title_nameM_D<-c("Esophagus (C15)","Stomach (C16)","Colorectal & Anus (C18-C21)","Liver (C22)","Gallbladder (C23-C24)","Pancreas (C25)","Lung (C33-C34)","Prostate (C61)","Kidney (C64)","Bladder (C67)","Non-Hodgkin lymphoma (C82-C86)","Leukemia (C91-C95)")

title_nameF_I<-c("Stomach (C16)","Colorectal (C18-C20)","Liver (C22)","Gallbladder (C23-C24)", "Pancreas (C25)","Lung (C33-C34)","Kidney (C64)","Cervix uteri (C53)","Non-Hodgkin lymphoma (C82-C86)","Leukemia (C91-C95)")
title_nameF_D<-c("Stomach (C16)","Colorectal & Anus (C18-C21)","Liver (C22)", "Pancreas (C25)","Lung (C33-C34)","Cervix uteri (C53)","Non-Hodgkin lymphoma (C82-C86)","Leukemia (C91-C95)")




NORDPRED_func <- function (Scenarios1_male,Scenarios1_female,comparison){
  
  if (comparison=="first"){
    
    NORDPRED_INCI_M <-c()
    NORDPRED_INCI_F <-c()
    
    for (i in 1: length(inci_cname_M)){
      male_case <-subset(get(inci_cname_M[i]),Sex=="남자")[,-c(1:3,24)]
      male_pop  <- Scenarios1_male[, c(as.character(2000:2040))]
      assign(paste0(inci_cname_M[i],"_M"),colSums(canproj(male_case,male_pop,startp=2020,startestage=7, methods="nordpred")$agsproj*(male_pop)/100000))
      NORDPRED_INCI_M <- cbind(NORDPRED_INCI_M,get(paste0(inci_cname_M[i],"_M")))
    }
    colnames(NORDPRED_INCI_M) <- inci_cname_M
    
    for (i in 1: length(inci_cname_F)){
      female_case <-subset(get(inci_cname_F[i]),Sex=="여자")[,-c(1:3,24)]
      female_pop  <- Scenarios1_female[, c(as.character(2000:2040))]
      assign(paste0(inci_cname_F[i],"_F"),colSums(canproj(female_case,female_pop,startp=2020,startestage=7, methods="nordpred")$agsproj*(female_pop)/100000))
      NORDPRED_INCI_F <- cbind(NORDPRED_INCI_F,get(paste0(inci_cname_F[i],"_F")))
    }
    colnames(NORDPRED_INCI_F) <- inci_cname_F
    
    NORDPRED_MORT_M <-c()
    NORDPRED_MORT_F <-c()
    
    for ( i in 1: length(mort_cname_M)){
      male_case <- subset(get(mort_cname_M[i]),Sex=="남자")[,-c(1:3,24,25)]
      male_pop  <- Scenarios1_male[, c(as.character(2000:2040))]
      if(length(which(rowSums(male_case/20) <1)[length(which(rowSums(male_case/20) <1))]) == 0){
        assign(paste0(mort_cname_M[i],"_M"),colSums(canproj(male_case,male_pop,startp=2020,startestage=7, methods="nordpred")$agsproj*(male_pop)/100000))
      } else if (length(which(rowSums(male_case/20) <1)[length(which(rowSums(male_case/20) <1))]) !=0) {
        if (which(rowSums(male_case/20) <1)[length(which(rowSums(male_case/20) <1))] >7){  
          num = as.numeric(which(rowSums(male_case/20) <1)[length(which(rowSums(male_case /20) <1))])+1
          assign(paste0(mort_cname_M[i],"_M"),colSums(canproj(male_case,male_pop,startp=2020,startestage=num, methods="nordpred")$agsproj*(male_pop)/100000))
        } else {
          assign(paste0(mort_cname_M[i],"_M"),colSums(canproj(male_case,male_pop,startp=2020,startestage=7, methods="nordpred")$agsproj*(male_pop)/100000))
        }
      }
      NORDPRED_MORT_M <- cbind(NORDPRED_MORT_M,get(paste0(mort_cname_M[i],"_M")))
    }
    colnames(NORDPRED_MORT_M) <- mort_cname_M
    
    for ( i in 1: length(mort_cname_F)){
      female_case <- subset(get(mort_cname_F[i]),Sex=="여자")[,-c(1:3,24,25)]
      female_pop  <- Scenarios1_female[, c(as.character(2000:2040))]
      if(length(which(rowSums(female_case/20) <1)[length(which(rowSums(female_case/20) <1))]) == 0){
        assign(paste0(mort_cname_F[i],"_F"),colSums(canproj(female_case,female_pop,startp=2020,startestage=7, methods="nordpred")$agsproj*(female_pop)/100000))
      } else if (length(which(rowSums(female_case/20) <1)[length(which(rowSums(female_case/20) <1))]) !=0) {
        if (which(rowSums(female_case/20) <1)[length(which(rowSums(female_case/20) <1))] >7){  
          num = as.numeric(which(rowSums(female_case/20) <1)[length(which(rowSums(female_case /20) <1))])+1
          assign(paste0(mort_cname_F[i],"_F"),colSums(canproj(female_case,female_pop,startp=2020,startestage=num, methods="nordpred")$agsproj*(female_pop)/100000))
        } else {
          assign(paste0(mort_cname_F[i],"_F"),colSums(canproj(female_case,female_pop,startp=2020,startestage=7, methods="nordpred")$agsproj*(female_pop)/100000))
        }
      }
      NORDPRED_MORT_F <- cbind(NORDPRED_MORT_F,get(paste0(mort_cname_F[i],"_F")))
      
    }
    colnames(NORDPRED_MORT_F) <- mort_cname_F
    NORDPRED_LIST <-list(NORDPRED_INCI_M,NORDPRED_INCI_F,NORDPRED_MORT_M,NORDPRED_MORT_F)
    return(NORDPRED_LIST)
  } else if (comparison=="second"){
    NORDPRED_INCI_M <-c()
    NORDPRED_INCI_F <-c()
    
    for (i in 1: length(inci_cname_M)){
      male_case <-subset(get(inci_cname_M[i]),Sex=="남자")[,-c(1:4)]
      male_pop  <- Scenarios1_male[, c(as.character(2001:2040))]
      assign(paste0(inci_cname_M[i],"_M"),colSums(canproj(male_case,male_pop,startp=2021,startestage=7, methods="nordpred")$agsproj*(male_pop)/100000))
      NORDPRED_INCI_M <- cbind(NORDPRED_INCI_M,get(paste0(inci_cname_M[i],"_M")))
    }
    colnames(NORDPRED_INCI_M) <- inci_cname_M
    
    for (i in 1: length(inci_cname_F)){
      female_case <-subset(get(inci_cname_F[i]),Sex=="여자")[,-c(1:4)]
      female_pop  <- Scenarios1_female[, c(as.character(2001:2040))]
      assign(paste0(inci_cname_F[i],"_F"),colSums(canproj(female_case,female_pop,startp=2021,startestage=7, methods="nordpred")$agsproj*(female_pop)/100000))
      NORDPRED_INCI_F <- cbind(NORDPRED_INCI_F,get(paste0(inci_cname_F[i],"_F")))
    }
    colnames(NORDPRED_INCI_F) <- inci_cname_F
    
    NORDPRED_MORT_M <-c()
    NORDPRED_MORT_F <-c()
    
    for ( i in 1: length(mort_cname_M)){
      male_case <- subset(get(mort_cname_M[i]),Sex=="남자")[,-c(1:4,25)]
      male_pop  <- Scenarios1_male[, c(as.character(2001:2040))]
      if(length(which(rowSums(male_case/20) <1)[length(which(rowSums(male_case/20) <1))]) == 0){
        assign(paste0(mort_cname_M[i],"_M"),colSums(canproj(male_case,male_pop,startp=2021,startestage=7, methods="nordpred")$agsproj*(male_pop)/100000))
      } else if (length(which(rowSums(male_case/20) <1)[length(which(rowSums(male_case/20) <1))]) !=0) {
        if (which(rowSums(male_case/20) <1)[length(which(rowSums(male_case/20) <1))] >7){  
          num = as.numeric(which(rowSums(male_case/20) <1)[length(which(rowSums(male_case /20) <1))])+1
          assign(paste0(mort_cname_M[i],"_M"),colSums(canproj(male_case,male_pop,startp=2021,startestage=num, methods="nordpred")$agsproj*(male_pop)/100000))
        } else {
          assign(paste0(mort_cname_M[i],"_M"),colSums(canproj(male_case,male_pop,startp=2021,startestage=7, methods="nordpred")$agsproj*(male_pop)/100000))
        }
      }
      NORDPRED_MORT_M <- cbind(NORDPRED_MORT_M,get(paste0(mort_cname_M[i],"_M")))
    }
    colnames(NORDPRED_MORT_M) <- mort_cname_M
    
    for ( i in 1: length(mort_cname_F)){
      female_case <- subset(get(mort_cname_F[i]),Sex=="여자")[,-c(1:4,25)]
      female_pop  <- Scenarios1_female[, c(as.character(2001:2040))]
      if(length(which(rowSums(female_case/20) <1)[length(which(rowSums(female_case/20) <1))]) == 0){
        assign(paste0(mort_cname_F[i],"_F"),colSums(canproj(female_case,female_pop,startp=2021,startestage=7, methods="nordpred")$agsproj*(female_pop)/100000))
      } else if (length(which(rowSums(female_case/20) <1)[length(which(rowSums(female_case/20) <1))]) !=0) {
        if (which(rowSums(female_case/20) <1)[length(which(rowSums(female_case/20) <1))] >7){  
          num = as.numeric(which(rowSums(female_case/20) <1)[length(which(rowSums(female_case /20) <1))])+1
          assign(paste0(mort_cname_F[i],"_F"),colSums(canproj(female_case,female_pop,startp=2021,startestage=num, methods="nordpred")$agsproj*(female_pop)/100000))
        } else {
          assign(paste0(mort_cname_F[i],"_F"),colSums(canproj(female_case,female_pop,startp=2021,startestage=7, methods="nordpred")$agsproj*(female_pop)/100000))
        }
      }
      NORDPRED_MORT_F <- cbind(NORDPRED_MORT_F,get(paste0(mort_cname_F[i],"_F")))
    }
    colnames(NORDPRED_MORT_F) <- mort_cname_F
    NORDPRED_LIST <-list(NORDPRED_INCI_M,NORDPRED_INCI_F,NORDPRED_MORT_M,NORDPRED_MORT_F)
    return(NORDPRED_LIST)
  }else if (comparison=="third"){
    
    NORDPRED_MORT_M <-c()
    NORDPRED_MORT_F <-c()
    
    for ( i in 1: length(mort_cname_M)){
      male_case <- subset(get(mort_cname_M[i]),Sex=="남자")[,-c(1:5)]
      male_pop  <- Scenarios1_male[, c(as.character(2002:2040))]
      if(length(which(rowSums(male_case/20) <1)[length(which(rowSums(male_case/20) <1))]) == 0){
        assign(paste0(mort_cname_M[i],"_M"),colSums(canproj(male_case,male_pop,startp=2022,startestage=7, methods="nordpred")$agsproj*(male_pop)/100000))
      } else if (length(which(rowSums(male_case/20) <1)[length(which(rowSums(male_case/20) <1))]) !=0) {
        if (which(rowSums(male_case/20) <1)[length(which(rowSums(male_case/20) <1))] >7){  
          num = as.numeric(which(rowSums(male_case/20) <1)[length(which(rowSums(male_case /20) <1))])+1
          assign(paste0(mort_cname_M[i],"_M"),colSums(canproj(male_case,male_pop,startp=2022,startestage=num, methods="nordpred")$agsproj*(male_pop)/100000))
        } else {
          assign(paste0(mort_cname_M[i],"_M"),colSums(canproj(male_case,male_pop,startp=2022,startestage=7, methods="nordpred")$agsproj*(male_pop)/100000))
        }
      }
      NORDPRED_MORT_M <- cbind(NORDPRED_MORT_M,get(paste0(mort_cname_M[i],"_M")))
    }
    colnames(NORDPRED_MORT_M) <- mort_cname_M
    
    for ( i in 1: length(mort_cname_F)){
      female_case <- subset(get(mort_cname_F[i]),Sex=="여자")[,-c(1:5)]
      female_pop  <- Scenarios1_female[, c(as.character(2002:2040))]
      if(length(which(rowSums(female_case/20) <1)[length(which(rowSums(female_case/20) <1))]) == 0){
        assign(paste0(mort_cname_F[i],"_F"),colSums(canproj(female_case,female_pop,startp=2022,startestage=7, methods="nordpred")$agsproj*(female_pop)/100000))
      } else if (length(which(rowSums(female_case/20) <1)[length(which(rowSums(female_case/20) <1))]) !=0) {
        if (which(rowSums(female_case/20) <1)[length(which(rowSums(female_case/20) <1))] >7){  
          num = as.numeric(which(rowSums(female_case/20) <1)[length(which(rowSums(female_case /20) <1))])+1
          assign(paste0(mort_cname_F[i],"_F"),colSums(canproj(female_case,female_pop,startp=2022,startestage=num, methods="nordpred")$agsproj*(female_pop)/100000))
        } else {
          assign(paste0(mort_cname_F[i],"_F"),colSums(canproj(female_case,female_pop,startp=2022,startestage=7, methods="nordpred")$agsproj*(female_pop)/100000))
        }
      }
      NORDPRED_MORT_F <- cbind(NORDPRED_MORT_F,get(paste0(mort_cname_F[i],"_F")))
    }
    colnames(NORDPRED_MORT_F) <- mort_cname_F
    NORDPRED_LIST <-list(NORDPRED_MORT_M,NORDPRED_MORT_F)
    return(NORDPRED_LIST)
  }
}
scene1_nd_first  <- NORDPRED_func(Scenarios1_male,Scenarios1_female,"first") # 중위 (fit:2000-2019)

scene1_nd_first_M_inci_Rate<-data.frame(scene1_nd_first[[1]])/colSums(Scenarios1_male[,as.character(2000:2040)])*100000
scene1_nd_first_F_inci_Rate<-data.frame(scene1_nd_first[[2]])/colSums(Scenarios1_female[,as.character(2000:2040)])*100000
scene1_nd_first_M_mort_Rate<-data.frame(scene1_nd_first[[3]])/colSums(Scenarios1_male[,as.character(2000:2040)])*100000
scene1_nd_first_F_mort_Rate<-data.frame(scene1_nd_first[[4]])/colSums(Scenarios1_female[,as.character(2000:2040)])*100000

scene1_nd_first_M_inci_Rate<-cbind("Year"=c(2000:2040),scene1_nd_first_M_inci_Rate)
scene1_nd_first_F_inci_Rate<-cbind("Year"=c(2000:2040),scene1_nd_first_F_inci_Rate)
scene1_nd_first_M_mort_Rate<-cbind("Year"=c(2000:2040),scene1_nd_first_M_mort_Rate)
scene1_nd_first_F_mort_Rate<-cbind("Year"=c(2000:2040),scene1_nd_first_F_mort_Rate)

first_nd_Rate<-rbind(
  cbind(melt(scene1_nd_first_M_inci_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Incidence"),"Comparison"=c("Prediction")),
  cbind(melt(scene1_nd_first_F_inci_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Incidence"),"Comparison"=c("Prediction")),
  cbind(melt(scene1_nd_first_M_mort_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Mortality"),"Comparison"=c("Prediction")),
  cbind(melt(scene1_nd_first_F_mort_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Mortality"),"Comparison"=c("Prediction")))

nd_comp_Rate <-first_nd_Rate
colnames(nd_comp_Rate) <-c("Year","Cancer","Rate","Sex","Type","Comparison")
nd_comp_Rate$Sex <- factor(nd_comp_Rate$Sex,levels=c("Male","Female"))

nd_comp_Rate<-nd_comp_Rate %>% mutate(
  Comparison = case_when(
    Comparison = 0019 & Type=="Incidence" & Year <=2019 ~ "Observation",
    Comparison = 0019 & Type=="Mortality" & Year <=2019 ~ "Observation",
    TRUE ~ Comparison
  ))
BAPC_func <- function (Scenarios1_male,Scenarios1_female,comparison){
  cancer_pred_sample <- matrix(data = NA, nrow = 21, ncol = 18) %>% as.data.frame() 
  rownames(cancer_pred_sample) <- c(seq(2020,2040,1))
  colnames(cancer_pred_sample) <-  c(1:18)
  
  BAPC_INCI_M <-c()
  BAPC_INCI_F <-c()
  
  for ( i in 1:length(inci_cname_M)){
    male_case <- subset(get(inci_cname_M[i]),Sex=="남자")[,-c(1:3,24)]
    male_pop  <- Scenarios1_male[, c(as.character(2000:2040))]
    test_M_pop <- data.frame(t(male_pop));colnames(test_M_pop) <- c(1:18)
    test_M_ca  <- data.frame(t(male_case));colnames(test_M_ca)  <- c(1:18)
    test_M_ca<-rbind(test_M_ca, cancer_pred_sample);colnames(test_M_ca)<- c(1:18)
    
    test_M_ca_rate <- agespec.proj(x = BAPC(APCList(test_M_ca, test_M_pop, gf=5), predict = list(npredict = 21, retro = TRUE),
                                            model = list(age = list(model = "rw2", prior="loggamma", param = c(1, 0.005), initial = 4, scale.model=FALSE),
                                                         period = list(include = TRUE, model = "drift", prior="loggamma", param = c(1, 0.005), initial = 4, scale.model=FALSE),
                                                         cohort = list(include = TRUE, model = "drift", prior="loggamma", param = c(1, 0.005), initial = 4, scale.model=FALSE),
                                                         overdis = list(include = FALSE, model = "iid", prior="loggamma", param = c(1, 0.005), initial = 4)),
                                            secondDiff = FALSE, stdweight = NULL, verbose = FALSE)) %>% as.data.frame()
    assign(paste0(inci_cname_M[i],"_M"),rowSums(test_M_ca_rate[,colnames(test_M_ca_rate) %like% '%mean']))
    BAPC_INCI_M <- cbind(BAPC_INCI_M,get(paste0(inci_cname_M[i],"_M")))
  }
  colnames(BAPC_INCI_M) <- inci_cname_M
  
  for ( i in 1:length(inci_cname_F)){
    female_case <- subset(get(inci_cname_F[i]),Sex=="여자")[,-c(1:3,24)]
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
    assign(paste0(inci_cname_F[i],"_F"),rowSums(test_F_ca_rate[,colnames(test_F_ca_rate) %like% '%mean']))
    BAPC_INCI_F <- cbind(BAPC_INCI_F,get(paste0(inci_cname_F[i],"_F")))
    
  }
  colnames(BAPC_INCI_F) <- inci_cname_F
  
  BAPC_MORT_M <- c()
  BAPC_MORT_F <- c()
  
  for ( i in 1:length(mort_cname_M)){
    male_case <- subset(get(mort_cname_M[i]),Sex=="남자")[,-c(1:3,24,25)]
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
    assign(paste0(mort_cname_M[i],"_M"),rowSums(test_M_ca_rate[,colnames(test_M_ca_rate) %like% '%mean']))
    BAPC_MORT_M <- cbind(BAPC_MORT_M,get(paste0(mort_cname_M[i],"_M")))
  }
  colnames(BAPC_MORT_M) <- mort_cname_M
  
  for ( i in 1:length(mort_cname_F)){
    female_case <- subset(get(mort_cname_F[i]),Sex=="여자")[,-c(1:3,24,25)]
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
    assign(paste0(mort_cname_F[i],"_F"),rowSums(test_F_ca_rate[,colnames(test_F_ca_rate) %like% '%mean']))
    BAPC_MORT_F <- cbind(BAPC_MORT_F,get(paste0(mort_cname_F[i],"_F")))
  }
  colnames(BAPC_MORT_F) <- mort_cname_F
  BAPC_LIST <-list(BAPC_INCI_M,BAPC_INCI_F,BAPC_MORT_M,BAPC_MORT_F)
  return(BAPC_LIST)
}

scene1_bp_first  <- BAPC_func(Scenarios1_male,Scenarios1_female,"first") # 중위 (fit:2000-2019)

scene1_bp_first_M_inci_Rate<-data.frame(scene1_bp_first[[1]])/colSums(Scenarios1_male[,as.character(2000:2040)])*100000
scene1_bp_first_F_inci_Rate<-data.frame(scene1_bp_first[[2]])/colSums(Scenarios1_female[,as.character(2000:2040)])*100000
scene1_bp_first_M_mort_Rate<-data.frame(scene1_bp_first[[3]])/colSums(Scenarios1_male[,as.character(2000:2040)])*100000
scene1_bp_first_F_mort_Rate<-data.frame(scene1_bp_first[[4]])/colSums(Scenarios1_female[,as.character(2000:2040)])*100000

scene1_bp_first_M_inci_Rate<-cbind("Year"=c(2000:2040),scene1_bp_first_M_inci_Rate)
scene1_bp_first_F_inci_Rate<-cbind("Year"=c(2000:2040),scene1_bp_first_F_inci_Rate)
scene1_bp_first_M_mort_Rate<-cbind("Year"=c(2000:2040),scene1_bp_first_M_mort_Rate)
scene1_bp_first_F_mort_Rate<-cbind("Year"=c(2000:2040),scene1_bp_first_F_mort_Rate)

first_bp_Rate<-rbind(
  cbind(melt(scene1_bp_first_M_inci_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Incidence"),"Comparison"=c("Prediction")),
  cbind(melt(scene1_bp_first_F_inci_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Incidence"),"Comparison"=c("Prediction")),
  cbind(melt(scene1_bp_first_M_mort_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Mortality"),"Comparison"=c("Prediction")),
  cbind(melt(scene1_bp_first_F_mort_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Mortality"),"Comparison"=c("Prediction")))

bp_comp_Rate <-first_bp_Rate
colnames(bp_comp_Rate) <-c("Year","Cancer","Rate","Sex","Type","Comparison")
bp_comp_Rate$Sex <- factor(bp_comp_Rate$Sex,levels=c("Male","Female"))

bp_comp_Rate<-bp_comp_Rate %>% mutate(
  Comparison = case_when(
    Comparison = 0019 & Type=="Incidence" & Year <=2019 ~ "Observation",
    Comparison = 0019 & Type=="Mortality" & Year <=2019 ~ "Observation",
    TRUE ~ Comparison
  ))

# observation 값 바꿔줌 (BAPC는 모형의 값임)
bp_comp_Rate<-rbind(subset(nd_comp_Rate,Year<=2019),subset(bp_comp_Rate,Year>=2020))

library(dlm)
library(forecast)

time_series_ag <- function (){
  
  ARIMA_INCI_M <-c()
  SSML_INCI_M <-c()
  
  for (i in 1:length(inci_cname_M)){
    inf<-ts(as.numeric(colSums( subset(get(inci_cname_M[i]),Sex=="남자")[,-c(1:3,24)])),start = c(2000))
    
    ## Autoregressive  model
    ARIMA <- round(as.numeric(forecast(auto.arima(inf), h = 21)$mean))
    
    ## linear state???space model
    fn <- function(parm) { dlmModPoly(order = 2, dV = parm[1], dW = parm[2:3])}
    fit <- dlmMLE(inf, rep(0, 3), build = fn, hessian = TRUE)
    mod <- fn(fit$par)
    filtered <- dlmFilter(inf, mod = mod)
    SSML_kalman <-dlmForecast(filtered,nAhead=21)
    SSML_kalman<-c(SSML_kalman$a[,1])
    
    ARIMA_INCI_M <- cbind(ARIMA_INCI_M,ARIMA)
    SSML_INCI_M <- cbind(SSML_INCI_M,SSML_kalman)
  }
  ARIMA_INCI_M <-data.frame(ARIMA_INCI_M)
  SSML_INCI_M <-data.frame(SSML_INCI_M)
  
  colnames(ARIMA_INCI_M) <- inci_cname_M
  colnames(SSML_INCI_M) <- inci_cname_M
  
  ARIMA_INCI_F <-c()
  SSML_INCI_F <-c()
  
  for (i in 1:length(inci_cname_F)){
    inf<-ts(as.numeric(colSums(subset(get(inci_cname_F[i]),Sex=="여자")[,-c(1:3,24)])),start = c(2000))
    
    ## Autoregressive  model
    ARIMA <- round(as.numeric(forecast(auto.arima(inf), h = 21)$mean))
    
    ## linear state???space model
    fn <- function(parm) { dlmModPoly(order = 2, dV = parm[1], dW = parm[2:3])}
    fit <- dlmMLE(inf, rep(0, 3), build = fn, hessian = TRUE)
    mod <- fn(fit$par)
    filtered <- dlmFilter(inf, mod = mod)
    SSML_kalman <-dlmForecast(filtered,nAhead=21)
    SSML_kalman<-c(SSML_kalman$a[,1])
    
    
    ARIMA_INCI_F <- cbind(ARIMA_INCI_F,ARIMA)
    SSML_INCI_F <- cbind(SSML_INCI_F,SSML_kalman)
  }
  
  ARIMA_INCI_F<-data.frame(ARIMA_INCI_F)
  SSML_INCI_F<-data.frame(SSML_INCI_F)
  
  colnames(ARIMA_INCI_F) <- inci_cname_F
  colnames(SSML_INCI_F) <- inci_cname_F
  
  ARIMA_MORT_M <-c()
  SSML_MORT_M <-c()
  
  for (i in 1:length(mort_cname_M)){
    inf<-ts(as.numeric(colSums( subset(get(mort_cname_M[i]),Sex=="남자")[,-c(1:3,24,25)])),start = c(2000))
    
    ## Autoregressive  model
    ARIMA <- round(as.numeric(forecast(auto.arima(inf), h = 21)$mean))
    
    ## linear state???space model
    fn <- function(parm) { dlmModPoly(order = 2, dV = parm[1], dW = parm[2:3])}
    fit <- dlmMLE(inf, rep(0, 3), build = fn, hessian = TRUE)
    mod <- fn(fit$par)
    filtered <- dlmFilter(inf, mod = mod)
    SSML_kalman <-dlmForecast(filtered,nAhead=21)
    SSML_kalman<-c(SSML_kalman$a[,1])
    
    
    ARIMA_MORT_M <- cbind(ARIMA_MORT_M,ARIMA)
    SSML_MORT_M <- cbind(ARIMA_MORT_M,SSML_kalman)
  }
  
  ARIMA_MORT_M<-data.frame(ARIMA_MORT_M)
  SSML_MORT_M<-data.frame(SSML_MORT_M)
  
  colnames(ARIMA_MORT_M) <- mort_cname_M
  colnames(SSML_MORT_M) <- mort_cname_M
  
  ARIMA_MORT_F <-c()
  SSML_MORT_F <-c()
  
  for (i in 1:length(mort_cname_F)){
    inf<-ts(as.numeric(colSums(subset(get(mort_cname_F[i]),Sex=="여자")[,-c(1:3,24,25)])),start = c(2000))
    
    ## Autoregressive  model
    ARIMA <- round(as.numeric(forecast(auto.arima(inf), h = 21)$mean))
    
    ## linear state???space model
    fn <- function(parm) { dlmModPoly(order = 2, dV = parm[1], dW = parm[2:3])}
    fit <- dlmMLE(inf, rep(0, 3), build = fn, hessian = TRUE)
    mod <- fn(fit$par)
    filtered <- dlmFilter(inf, mod = mod)
    SSML_kalman <-dlmForecast(filtered,nAhead=21)
    SSML_kalman<-c(SSML_kalman$a[,1])
    
    
    ARIMA_MORT_F <- cbind(ARIMA_MORT_F,ARIMA)
    SSML_MORT_F <- cbind(ARIMA_MORT_F,SSML_kalman)
  }
  
  ARIMA_MORT_F<-data.frame(ARIMA_MORT_F)
  SSML_MORT_F<-data.frame(SSML_MORT_F)
  
  colnames(ARIMA_MORT_F) <- mort_cname_F
  colnames(SSML_MORT_F) <- mort_cname_F
  
  ARIMA_LIST <-list(ARIMA_INCI_M,ARIMA_INCI_F,ARIMA_MORT_M,ARIMA_MORT_F)
  SSML_LIST <-list(SSML_INCI_M,SSML_INCI_F,SSML_MORT_M,SSML_MORT_F)
  
  time_series_list<-list(ARIMA_LIST,SSML_LIST)
  return(time_series_list)
}

scene1_ts <-time_series_ag()

scene1_ts_ARIMA<-scene1_ts[[1]]
scene1_ts_SSML<-scene1_ts[[2]]

scene1_ts_ARIMA_first_M_inci_Rate<-data.frame(scene1_ts_ARIMA[[1]])/colSums(Scenarios1_male[,as.character(2020:2040)])*100000
scene1_ts_ARIMA_first_F_inci_Rate<-data.frame(scene1_ts_ARIMA[[2]])/colSums(Scenarios1_female[,as.character(2020:2040)])*100000
scene1_ts_ARIMA_first_M_mort_Rate<-data.frame(scene1_ts_ARIMA[[3]])/colSums(Scenarios1_male[,as.character(2020:2040)])*100000
scene1_ts_ARIMA_first_F_mort_Rate<-data.frame(scene1_ts_ARIMA[[4]])/colSums(Scenarios1_female[,as.character(2020:2040)])*100000

scene1_ts_SSML_first_M_inci_Rate<-data.frame(scene1_ts_SSML[[1]])/colSums(Scenarios1_male[,as.character(2020:2040)])*100000
scene1_ts_SSML_first_F_inci_Rate<-data.frame(scene1_ts_SSML[[2]])/colSums(Scenarios1_female[,as.character(2020:2040)])*100000
scene1_ts_SSML_first_M_mort_Rate<-data.frame(scene1_ts_SSML[[3]])/colSums(Scenarios1_male[,as.character(2020:2040)])*100000
scene1_ts_SSML_first_F_mort_Rate<-data.frame(scene1_ts_SSML[[4]])/colSums(Scenarios1_female[,as.character(2020:2040)])*100000

scene1_ts_ARIMA_first_M_inci_Rate<-cbind("Year"=c(2020:2040),scene1_ts_ARIMA_first_M_inci_Rate)
scene1_ts_ARIMA_first_F_inci_Rate<-cbind("Year"=c(2020:2040),scene1_ts_ARIMA_first_F_inci_Rate)
scene1_ts_ARIMA_first_M_mort_Rate<-cbind("Year"=c(2020:2040),scene1_ts_ARIMA_first_M_mort_Rate)
scene1_ts_ARIMA_first_F_mort_Rate<-cbind("Year"=c(2020:2040),scene1_ts_ARIMA_first_F_mort_Rate)

scene1_ts_SSML_first_M_inci_Rate<-cbind("Year"=c(2020:2040),scene1_ts_SSML_first_M_inci_Rate)
scene1_ts_SSML_first_F_inci_Rate<-cbind("Year"=c(2020:2040),scene1_ts_SSML_first_F_inci_Rate)
scene1_ts_SSML_first_M_mort_Rate<-cbind("Year"=c(2020:2040),scene1_ts_SSML_first_M_mort_Rate)
scene1_ts_SSML_first_F_mort_Rate<-cbind("Year"=c(2020:2040),scene1_ts_SSML_first_F_mort_Rate)

first_ts_ARIMA_Rate<-rbind(
  cbind(melt(scene1_ts_ARIMA_first_M_inci_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Incidence"),"Comparison"=c("Prediction")),
  cbind(melt(scene1_ts_ARIMA_first_F_inci_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Incidence"),"Comparison"=c("Prediction")),
  cbind(melt(scene1_ts_ARIMA_first_M_mort_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Mortality"),"Comparison"=c("Prediction")),
  cbind(melt(scene1_ts_ARIMA_first_F_mort_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Mortality"),"Comparison"=c("Prediction")))

first_ts_SSML_Rate<-rbind(
  cbind(melt(scene1_ts_SSML_first_M_inci_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Incidence"),"Comparison"=c("Prediction")),
  cbind(melt(scene1_ts_SSML_first_F_inci_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Incidence"),"Comparison"=c("Prediction")),
  cbind(melt(scene1_ts_SSML_first_M_mort_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Mortality"),"Comparison"=c("Prediction")),
  cbind(melt(scene1_ts_SSML_first_F_mort_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Mortality"),"Comparison"=c("Prediction")))

colnames(first_ts_ARIMA_Rate) <-c("Year","Cancer","Rate","Sex","Type","Comparison")
colnames(first_ts_SSML_Rate) <-c("Year","Cancer","Rate","Sex","Type","Comparison")

first_ts_ARIMA_Rate$Sex <- factor(first_ts_ARIMA_Rate$Sex,levels=c("Male","Female"))
first_ts_SSML_Rate$Sex <- factor(first_ts_SSML_Rate$Sex,levels=c("Male","Female"))

## after applicate norpred func

first_ts_ARIMA_Rate<-rbind(subset(nd_comp_Rate,Year<=2019),first_ts_ARIMA_Rate)
first_ts_SSML_Rate<-rbind(subset(nd_comp_Rate,Year<=2019),first_ts_SSML_Rate)

############################################################################################################################################
library("reshape")
library("sqldf")
library("ggplot2")
library("dplyr")
library("ggpubr")
library("glue")

load(file="/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/rawdata/jp_model_M_inci.RData")
load(file="/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/rawdata/jp_model_M_mort.RData")
load(file="/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/rawdata/jp_model_F_inci.RData")
load(file="/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/rawdata/jp_model_F_mort.RData")

jp_model_M_inci_apc<-data.frame(jp_model_M_inci$data_export)
jp_model_M_mort_apc<-data.frame(jp_model_M_mort$data_export)
jp_model_F_inci_apc<-data.frame(jp_model_F_inci$data_export)
jp_model_F_mort_apc<-data.frame(jp_model_F_mort$data_export)

jp_model_M_inci_apc_sql<-sqldf("SELECT *, MAX(year) as last_value FROM jp_model_M_inci_apc GROUP BY comparison, cancer")[,-c(7,8,9)]
jp_model_M_mort_apc_sql<-sqldf("SELECT *, MAX(year) as last_value FROM jp_model_M_mort_apc GROUP BY comparison, cancer")[,-c(7,8,9)]
jp_model_F_inci_apc_sql<-sqldf("SELECT *, MAX(year) as last_value FROM jp_model_F_inci_apc GROUP BY comparison, cancer")[,-c(7,8,9)]
jp_model_F_mort_apc_sql<-sqldf("SELECT *, MAX(year) as last_value FROM jp_model_F_mort_apc GROUP BY comparison, cancer")[,-c(7,8,9)]

jp_model_M_inci_apc_sql$apc <- as.numeric(gsub("\\*", "", jp_model_M_inci_apc_sql$apc))
jp_model_M_mort_apc_sql$apc <- as.numeric(gsub("\\*", "", jp_model_M_mort_apc_sql$apc))
jp_model_F_inci_apc_sql$apc <- as.numeric(gsub("\\*", "", jp_model_F_inci_apc_sql$apc))
jp_model_F_mort_apc_sql$apc <- as.numeric(gsub("\\*", "", jp_model_F_mort_apc_sql$apc))

jp_model_M_inci_apc_sql<-subset(jp_model_M_inci_apc_sql,comparison=="first")[,-1]
jp_model_M_mort_apc_sql<-subset(jp_model_M_mort_apc_sql,comparison=="first")[,-1]
jp_model_F_inci_apc_sql<-subset(jp_model_F_inci_apc_sql,comparison=="first")[,-1]
jp_model_F_mort_apc_sql<-subset(jp_model_F_mort_apc_sql,comparison=="first")[,-1]


check<-function(jp_model_M_inci_apc_sql,inci_cname_M){
  model_rslt_all<-data.frame('cancer'=c(),'year'=c(),'rate'=c(),'model'=c())
  for (i in 1:length(inci_cname_M)) {
    data <- subset(jp_model_M_inci_apc_sql,cancer==inci_cname_M[i])
    year=2019
    k=1
    next_year  <-c()
    next_rate  <-c()
    next_model <-c()
    
    dat_model <-data$model
    dat_apc <-data$apc
    
    while(year < 2040) {
      # 다음 연도의 값을 계산
      next_year[k] <-  year +1
      next_rate[k] <-  dat_model+(dat_model*dat_apc*0.01)
      next_model[k] <- dat_model+(dat_model*dat_apc*0.01)
      dat_model = dat_model+(dat_model*dat_apc*0.01)
      year=year+1
      k=k+1
    }
    sample_data  <- data.frame('cancer'=data$cancer,'year'=next_year,'rate'=next_rate,'model'=next_model)
    assign(paste0("model_rslt_all"),rbind(model_rslt_all,sample_data))
  }
  return(model_rslt_all)
}

jp_model_inci_M_dat<-check(jp_model_M_inci_apc_sql,inci_cname_M);jp_model_inci_M_dat$Sex<-c("Male");jp_model_inci_M_dat$Comparison<-c("JP");jp_model_inci_M_dat$Type <-c("Incidence")
jp_model_mort_M_dat<-check(jp_model_M_mort_apc_sql,mort_cname_M);jp_model_mort_M_dat$Sex<-c("Male");jp_model_mort_M_dat$Comparison<-c("JP");jp_model_mort_M_dat$Type <-c("Mortality")
jp_model_inci_F_dat<-check(jp_model_F_inci_apc_sql,inci_cname_F);jp_model_inci_F_dat$Sex<-c("Female");jp_model_inci_F_dat$Comparison<-c("JP");jp_model_inci_F_dat$Type <-c("Incidence")
jp_model_mort_F_dat<-check(jp_model_F_mort_apc_sql,mort_cname_F);jp_model_mort_F_dat$Sex<-c("Female");jp_model_mort_F_dat$Comparison<-c("JP");jp_model_mort_F_dat$Type<-c("Mortality")

model_jp_pred<-rbind(jp_model_inci_M_dat,jp_model_mort_M_dat,jp_model_inci_F_dat,jp_model_mort_F_dat)[,-3]
model_jp_pred<-model_jp_pred[,c(2,1,3,4,6,5)]



############################################################################################################################################



model_obs<-subset(nd_comp_Rate,Comparison=="Observation")
model_obs_pred1<-subset(model_obs,Year==2019);model_obs_pred1$Comparison <-c("Nordpred")
model_obs_pred2<-subset(model_obs,Year==2019);model_obs_pred2$Comparison <-c("BAPC")
model_obs_pred3<-subset(model_obs,Year==2019);model_obs_pred3$Comparison <-c("ARIMA")
model_obs_pred4<-subset(model_obs,Year==2019);model_obs_pred4$Comparison <-c("SSML")
model_obs_pred5<-subset(model_obs,Year==2019);model_obs_pred5$Comparison <-c("JP")

model_np_pred<-subset(nd_comp_Rate,Comparison=="Prediction");model_np_pred$Comparison <-c("Nordpred")
model_bp_pred<-subset(bp_comp_Rate,Comparison=="Prediction");model_bp_pred$Comparison <-c("BAPC")
model_AR_pred<-subset(first_ts_ARIMA_Rate,Comparison=="Prediction");model_AR_pred$Comparison <-c("ARIMA")
model_SSML_pred<-subset(first_ts_SSML_Rate,Comparison=="Prediction");model_SSML_pred$Comparison <-c("SSML")
colnames(model_jp_pred) <-colnames(model_obs)

model_np_pred <-rbind(model_obs_pred1,model_np_pred)
model_bp_pred <-rbind(model_obs_pred2,model_bp_pred)
model_AR_pred <-rbind(model_obs_pred3,model_AR_pred)
model_SSML_pred <-rbind(model_obs_pred4,model_SSML_pred)
model_jp_pred <-rbind(model_obs_pred5,model_jp_pred)

mdata<-rbind(model_obs,model_np_pred,model_bp_pred,model_AR_pred,model_SSML_pred,model_jp_pred)
mdata$Comparison <-factor(mdata$Comparison)
mdata$Comparison <- factor(mdata$Comparison,levels=c("Observation",'Nordpred',"BAPC","JP", "ARIMA", "SSML"))

for ( i in 1:length(inci_cname_M)){
  assign(paste0("plot",i),ggplot(subset(mdata,Type==c("Incidence") & Sex=="Male" &  Cancer==inci_cname_M[i]), aes(x = Year, y = Rate, shape=Comparison, color = Comparison)) +
           geom_line(size = 1.2) +
           geom_point(size = 1.5,fill = "white") +
           scale_fill_viridis(discrete = T, option = "E") +
           scale_color_manual(values = c("black", "chartreuse3", "brown3",'cadetblue3',"cornsilk3","blueviolet")) + 
           scale_shape_manual(values = c(15,16,17,18,19,20)) +
           theme_classic() +
           facet_wrap(~ Sex, strip.position = "top", nrow = 1) +
           labs(x = "Year", y = "Incidence Rate (per 100,000)",title=title_nameM_I[i]) +
           theme_ipsum() +
           theme(
             strip.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5,size=15), # 패널 레이블 위치 조정
             axis.title.x = element_text(hjust = 0.5, vjust = 0.5,size=15), # X축 이름 중앙 정렬
             axis.title.y = element_text(hjust = 0.5, vjust = 0.5,size=15), # Y축 이름 중앙 정렬
             plot.margin = margin(1, 1, 1, 1, "cm"), # 그래프 안에 레이블 넣기 위해 여백 조정
             legend.text = element_text(size = 15),legend.title = element_text(size = 15),plot.title = element_text(size = 15, face = "bold")           ))
}

ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot5,plot6,plot7,plot8,labels = c("E","F","G","H"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot9,labels = c("I"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,labels = c("A","B","C","D","E","F","G","H","I"),ncol = 2, nrow = 5, common.legend = TRUE)


for ( i in 1:length(mort_cname_M)){
  assign(paste0("plot",i),ggplot(subset(mdata,Type==c("Mortality") & Sex=="Male" &  Cancer==mort_cname_M[i]), aes(x = Year, y = Rate, shape=Comparison, color = Comparison)) +
           geom_line(size = 1.2) +
           geom_point(size = 1.5,fill = "white") +
           scale_fill_viridis(discrete = T, option = "E") +
           scale_color_manual(values = c("black", "chartreuse3", "brown3",'cadetblue3',"cornsilk3","blueviolet")) + 
           scale_shape_manual(values = c(15,16,17,18,19,20)) +
           theme_classic() +
           facet_wrap(~ Sex, strip.position = "top", nrow = 1) +
           labs(x = "Year", y = "Mortality Rate (per 100,000)",title=title_nameM_D[i]) +
           theme_ipsum() +
           theme(
             strip.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5,size=15), # 패널 레이블 위치 조정
             axis.title.x = element_text(hjust = 0.5, vjust = 0.5,size=15), # X축 이름 중앙 정렬
             axis.title.y = element_text(hjust = 0.5, vjust = 0.5,size=15), # Y축 이름 중앙 정렬
             plot.margin = margin(1, 1, 1, 1, "cm"), # 그래프 안에 레이블 넣기 위해 여백 조정
             legend.text = element_text(size = 15),legend.title = element_text(size = 15),plot.title = element_text(size = 15, face = "bold")           ))
}

ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot5,plot6,plot7,plot8,labels = c("E","F","G","H"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot9,labels = c("I"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,labels = c("A","B","C","D","E","F","G","H","I"),ncol = 2, nrow = 5, common.legend = TRUE)


for ( i in 1:length(inci_cname_F)){
  assign(paste0("plot",i),ggplot(subset(mdata,Type==c("Incidence") & Sex=="Female" &  Cancer==inci_cname_F[i]), aes(x = Year, y = Rate, shape=Comparison, color = Comparison)) +
           geom_line(size = 1.2) +
           geom_point(size = 1.5,fill = "white") +
           scale_fill_viridis(discrete = T, option = "E") +
           scale_color_manual(values = c("black", "chartreuse3", "brown3",'cadetblue3',"cornsilk3","blueviolet")) + 
           scale_shape_manual(values = c(15,16,17,18,19,20)) +
           theme_classic() +
           facet_wrap(~ Sex, strip.position = "top", nrow = 1) +
           labs(x = "Year", y = "Incidence Rate (per 100,000)",title=title_nameF_I[i]) +
           theme_ipsum() +
           theme(
             strip.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5,size=15), # 패널 레이블 위치 조정
             axis.title.x = element_text(hjust = 0.5, vjust = 0.5,size=15), # X축 이름 중앙 정렬
             axis.title.y = element_text(hjust = 0.5, vjust = 0.5,size=15), # Y축 이름 중앙 정렬
             plot.margin = margin(1, 1, 1, 1, "cm"), # 그래프 안에 레이블 넣기 위해 여백 조정
             legend.text = element_text(size = 15),legend.title = element_text(size = 15),plot.title = element_text(size = 15, face = "bold")           ))
}

ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot5,plot6,labels = c("E","F"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,labels = c("A","B","C","D","E","F"),ncol = 2, nrow = 5, common.legend = TRUE)


for ( i in 1:length(mort_cname_F)){
  assign(paste0("plot",i),ggplot(subset(mdata,Type==c("Mortality") & Sex=="Female" &  Cancer==mort_cname_F[i]), aes(x = Year, y = Rate, shape=Comparison, color = Comparison)) +
           geom_line(size = 1.2) +
           geom_point(size = 1.5,fill = "white") +
           scale_fill_viridis(discrete = T, option = "E") +
           scale_color_manual(values = c("black", "chartreuse3", "brown3",'cadetblue3',"cornsilk3","blueviolet")) + 
           scale_shape_manual(values = c(15,16,17,18,19,20)) +
           theme_classic() +
           facet_wrap(~ Sex, strip.position = "top", nrow = 1) +
           labs(x = "Year", y = "Mortality Rate (per 100,000)",title=title_nameF_D[i]) +
           theme_ipsum() +
           theme(
             strip.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5,size=15), # 패널 레이블 위치 조정
             axis.title.x = element_text(hjust = 0.5, vjust = 0.5,size=15), # X축 이름 중앙 정렬
             axis.title.y = element_text(hjust = 0.5, vjust = 0.5,size=15), # Y축 이름 중앙 정렬
             plot.margin = margin(1, 1, 1, 1, "cm"), # 그래프 안에 레이블 넣기 위해 여백 조정
             legend.text = element_text(size = 15),legend.title = element_text(size = 15),plot.title = element_text(size = 15, face = "bold")
           ))
}

ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot5,plot6,plot7,plot8,labels = c("E","F","G","H"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,labels = c("A","B","C","D","E","F","G","H"),ncol = 2, nrow = 5, common.legend = TRUE)

############################################################################################################################################
