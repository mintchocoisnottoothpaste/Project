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
library(splines)
library(tidyverse)
library(colorspace)
library(dlm)
library(forecast)
library(sqldf)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(glue)

load(file="/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/rawdata/workspace.RData")
source("/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/code/canproj-2022_HJ.R")
agegroups <- c("00-04","05-09","10-14","15-19","20-24","25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84","85+" )

inci_cname_M <- inci_cname[inci_cname %in% c("C15_I","C16_I","C18_C20_I","C22_I","C23_C24_I","C25_I","C33_C34_I","C61_I","C64_I","C67_I","C82_C86_C96_I","C91_C95_I")]
mort_cname_M <- mort_cname[mort_cname %in% c("C15_D","C16_D","C18_C21_D","C22_D","C23_C24_D" ,"C25_D","C33_C34_D","C61_D","C64_D","C67_D","C82_C86_D","C91_C95_D")]

inci_cname_F <- inci_cname[inci_cname %in% c("C16_I","C18_C20_I","C22_I","C23_C24_I","C25_I","C33_C34_I","C53_I","C82_C86_C96_I","C91_C95_I")]
mort_cname_F <- mort_cname[mort_cname %in% c("C16_D","C18_C21_D","C22_D","C23_C24_D","C25_D","C33_C34_D","C53_D","C82_C86_D","C91_C95_D")]

title_nameM<-c("Esophagus (C15)","Stomach (C16)","Colorectal (C18-C20) | Colorectal & Anus (C18-C21)","Liver (C22)","Gallbladder (C23-C24)","Pancreas (C25)","Lung (C33-C34)","Prostate (C61)","Kidney (C64)","Bladder (C67)","Non-Hodgkin lymphoma (C82-C86,C96) | (C82-C86)","Leukemia (C91-C95)")
title_nameF<-c("Stomach (C16)","Colorectal (C18-C20) | Colorectal & Anus (C18-C21)","Liver (C22)","Gallbladder (C23-C24)", "Pancreas (C25)","Lung (C33-C34)","Cervix uteri (C53)","Non-Hodgkin lymphoma (C82-C86,C96) | (C82-C86)","Leukemia (C91-C95)")


################################################################################
################################################################################

NORDPRED_func <- function (Scenarios1_male,Scenarios1_female){
  
  NORDPRED_INCI_M <-c()
  NORDPRED_INCI_F <-c()
  
  for (i in 1: length(inci_cname_M)){
    male_case <-subset(get(inci_cname_M[i]),Sex=="남자")[,-c(1:3,ncol(get(inci_cname_M[i])))]
    male_pop  <- Scenarios1_male[, c(as.character(2000:2040))]
    assign(paste0(inci_cname_M[i],"_M"),colSums(canproj(male_case,male_pop,startp=2020,startestage=7, methods="nordpred")$agsproj*(male_pop)/100000))
    NORDPRED_INCI_M <- cbind(NORDPRED_INCI_M,get(paste0(inci_cname_M[i],"_M")))
  }
  colnames(NORDPRED_INCI_M) <- inci_cname_M
  
  for (i in 1: length(inci_cname_F)){
    female_case <-subset(get(inci_cname_F[i]),Sex=="여자")[,-c(1:3,ncol(get(inci_cname_F[i])))]
    female_pop  <- Scenarios1_female[, c(as.character(2000:2040))]
    assign(paste0(inci_cname_F[i],"_F"),colSums(canproj(female_case,female_pop,startp=2020,startestage=7, methods="nordpred")$agsproj*(female_pop)/100000))
    NORDPRED_INCI_F <- cbind(NORDPRED_INCI_F,get(paste0(inci_cname_F[i],"_F")))
  }
  colnames(NORDPRED_INCI_F) <- inci_cname_F
  
  NORDPRED_MORT_M <-c()
  NORDPRED_MORT_F <-c()
  
  for ( i in 1: length(mort_cname_M)){
    male_case <- subset(get(mort_cname_M[i]),Sex=="남자")[,-c(1:3,ncol(get(mort_cname_M[i]))-1,ncol(get(mort_cname_M[i])))]
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
    female_case <- subset(get(mort_cname_F[i]),Sex=="여자")[,-c(1:3,ncol(get(mort_cname_M[i]))-1,ncol(get(mort_cname_F[i])))]
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
}

scene1_nd_first  <- NORDPRED_func(Scenarios1_male,Scenarios1_female) # 중위 (fit:2000-2019)

scene1_nd_first_M_inci_Rate<-data.frame(scene1_nd_first[[1]])/colSums(Scenarios1_male[,as.character(2000:2040)])*100000
scene1_nd_first_F_inci_Rate<-data.frame(scene1_nd_first[[2]])/colSums(Scenarios1_female[,as.character(2000:2040)])*100000
scene1_nd_first_M_mort_Rate<-data.frame(scene1_nd_first[[3]])/colSums(Scenarios1_male[,as.character(2000:2040)])*100000
scene1_nd_first_F_mort_Rate<-data.frame(scene1_nd_first[[4]])/colSums(Scenarios1_female[,as.character(2000:2040)])*100000

scene1_nd_first_M_inci_Rate<-cbind("Year"=c(2000:2040),scene1_nd_first_M_inci_Rate)
scene1_nd_first_F_inci_Rate<-cbind("Year"=c(2000:2040),scene1_nd_first_F_inci_Rate)
scene1_nd_first_M_mort_Rate<-cbind("Year"=c(2000:2040),scene1_nd_first_M_mort_Rate)
scene1_nd_first_F_mort_Rate<-cbind("Year"=c(2000:2040),scene1_nd_first_F_mort_Rate)

colnames(scene1_nd_first_M_inci_Rate) <-c("Year",title_nameM)
colnames(scene1_nd_first_M_mort_Rate) <-c("Year",title_nameM)
colnames(scene1_nd_first_F_inci_Rate) <-c("Year",title_nameF)
colnames(scene1_nd_first_F_mort_Rate) <-c("Year",title_nameF)

first_nd_Rate<-rbind(
  cbind(melt(scene1_nd_first_M_inci_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Incidence"),"Comparison"=c("NP")),
  cbind(melt(scene1_nd_first_F_inci_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Incidence"),"Comparison"=c("NP")),
  cbind(melt(scene1_nd_first_M_mort_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Mortality"),"Comparison"=c("NP")),
  cbind(melt(scene1_nd_first_F_mort_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Mortality"),"Comparison"=c("NP")))

nd_comp_Rate <-first_nd_Rate
colnames(nd_comp_Rate) <-c("Year","Cancer","Rate","Sex","Type","Comparison")
nd_comp_Rate$Sex <- factor(nd_comp_Rate$Sex,levels=c("Male","Female"))

nd_comp_Rate<-nd_comp_Rate %>% mutate(
  Comparison = case_when(
    Comparison = 0019 & Type=="Incidence" & Year <=2019 ~ "OBS",
    Comparison = 0019 & Type=="Mortality" & Year <=2019 ~ "OBS",
    TRUE ~ Comparison
  ))

model_obs<-subset(nd_comp_Rate,Comparison=="OBS" & Year==2019);model_obs$Comparison <-c("NP")
model_np_pred <-rbind(model_obs,nd_comp_Rate)

BAPC_func <- function (Scenarios1_male,Scenarios1_female){
  cancer_pred_sample <- matrix(data = NA, nrow = 21, ncol = 18) %>% as.data.frame() 
  rownames(cancer_pred_sample) <- c(seq(2020,2040,1))
  colnames(cancer_pred_sample) <-  c(1:18)
  
  BAPC_INCI_M <-c()
  BAPC_INCI_F <-c()
  
  for ( i in 1:12){
    male_case <- subset(get(inci_cname_M[i]),Sex=="남자")[,-c(1:3,ncol(get(inci_cname_M[i])))]
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
  
  for ( i in 1:9){
    female_case <- subset(get(inci_cname_F[i]),Sex=="여자")[,-c(1:3,ncol(get(inci_cname_M[i])))]
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
  
  for ( i in 1:12){
    male_case <- subset(get(mort_cname_M[i]),Sex=="남자")[,-c(1:3,ncol(get(inci_cname_M[i]))-1,ncol(get(inci_cname_M[i])))]
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
  
  for ( i in 1:9){
    female_case <- subset(get(mort_cname_F[i]),Sex=="여자")[,-c(1:3,ncol(get(inci_cname_M[i]))-1,ncol(get(inci_cname_M[i])))]
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

scene1_bp_first  <- BAPC_func(Scenarios1_male,Scenarios1_female) # 중위 (fit:2000-2019)

scene1_bp_first_M_inci_Rate<-data.frame(scene1_bp_first[[1]])/colSums(Scenarios1_male[,as.character(2000:2040)])*100000
scene1_bp_first_F_inci_Rate<-data.frame(scene1_bp_first[[2]])/colSums(Scenarios1_female[,as.character(2000:2040)])*100000
scene1_bp_first_M_mort_Rate<-data.frame(scene1_bp_first[[3]])/colSums(Scenarios1_male[,as.character(2000:2040)])*100000
scene1_bp_first_F_mort_Rate<-data.frame(scene1_bp_first[[4]])/colSums(Scenarios1_female[,as.character(2000:2040)])*100000

scene1_bp_first_M_inci_Rate<-cbind("Year"=c(2000:2040),scene1_bp_first_M_inci_Rate)
scene1_bp_first_F_inci_Rate<-cbind("Year"=c(2000:2040),scene1_bp_first_F_inci_Rate)
scene1_bp_first_M_mort_Rate<-cbind("Year"=c(2000:2040),scene1_bp_first_M_mort_Rate)
scene1_bp_first_F_mort_Rate<-cbind("Year"=c(2000:2040),scene1_bp_first_F_mort_Rate)

colnames(scene1_bp_first_M_inci_Rate) <-c("Year",title_nameM)
colnames(scene1_bp_first_M_mort_Rate) <-c("Year",title_nameM)
colnames(scene1_bp_first_F_inci_Rate) <-c("Year",title_nameF)
colnames(scene1_bp_first_F_mort_Rate) <-c("Year",title_nameF)

first_bp_Rate<-rbind(
  cbind(melt(scene1_bp_first_M_inci_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Incidence"),"Comparison"=c("BAPC")),
  cbind(melt(scene1_bp_first_F_inci_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Incidence"),"Comparison"=c("BAPC")),
  cbind(melt(scene1_bp_first_M_mort_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Mortality"),"Comparison"=c("BAPC")),
  cbind(melt(scene1_bp_first_F_mort_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Mortality"),"Comparison"=c("BAPC")))

bp_comp_Rate <-first_bp_Rate
colnames(bp_comp_Rate) <-c("Year","Cancer","Rate","Sex","Type","Comparison")
bp_comp_Rate$Sex <- factor(bp_comp_Rate$Sex,levels=c("Male","Female"))

bp_comp_Rate<-bp_comp_Rate %>% mutate(
  Comparison = case_when(
    Comparison = 0019 & Type=="Incidence" & Year <=2019 ~ "OBS",
    Comparison = 0019 & Type=="Mortality" & Year <=2019 ~ "OBS",
    TRUE ~ Comparison
  ))

model_obs<-subset(nd_comp_Rate,Comparison=="OBS" & Year==2019);model_obs$Comparison <-c("BAPC")
model_bp_pred <-rbind(model_obs,bp_comp_Rate)

time_series_ag <- function (){
  
  ARIMA_INCI_M <-c()
  SSML_INCI_M <-c()
  
  for (i in 1:length(inci_cname_M)){
    inf<-ts(as.numeric(colSums( subset(get(inci_cname_M[i]),Sex=="남자")[,-c(1:3,ncol(get(inci_cname_M[i])))])),start = c(2000))
    
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
    inf<-ts(as.numeric(colSums(subset(get(inci_cname_F[i]),Sex=="여자")[,-c(1:3,ncol(get(inci_cname_F[i])))])),start = c(2000))
    
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
    inf<-ts(as.numeric(colSums( subset(get(mort_cname_M[i]),Sex=="남자")[,-c(1:3,ncol(get(mort_cname_M[i]))-1,ncol(get(mort_cname_M[i])))])),start = c(2000))
    
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
    inf<-ts(as.numeric(colSums(subset(get(mort_cname_F[i]),Sex=="여자")[,-c(1:3,ncol(get(mort_cname_F[i]))-1,ncol(get(mort_cname_F[i])))])),start = c(2000))
    
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

colnames(scene1_ts_ARIMA_first_M_inci_Rate) <-c("Year",title_nameM)
colnames(scene1_ts_ARIMA_first_M_mort_Rate) <-c("Year",title_nameM)
colnames(scene1_ts_ARIMA_first_F_inci_Rate) <-c("Year",title_nameF)
colnames(scene1_ts_ARIMA_first_F_mort_Rate) <-c("Year",title_nameF)

colnames(scene1_ts_SSML_first_M_inci_Rate) <-c("Year",title_nameM)
colnames(scene1_ts_SSML_first_M_mort_Rate) <-c("Year",title_nameM)
colnames(scene1_ts_SSML_first_F_inci_Rate) <-c("Year",title_nameF)
colnames(scene1_ts_SSML_first_F_mort_Rate) <-c("Year",title_nameF)

ARIMA_comp_Rate<-rbind(
  cbind(melt(scene1_ts_ARIMA_first_M_inci_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Incidence"),"Comparison"=c("ARIMA")),
  cbind(melt(scene1_ts_ARIMA_first_F_inci_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Incidence"),"Comparison"=c("ARIMA")),
  cbind(melt(scene1_ts_ARIMA_first_M_mort_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Mortality"),"Comparison"=c("ARIMA")),
  cbind(melt(scene1_ts_ARIMA_first_F_mort_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Mortality"),"Comparison"=c("ARIMA")))

SSML_comp_Rate<-rbind(
  cbind(melt(scene1_ts_SSML_first_M_inci_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Incidence"),"Comparison"=c("SSML")),
  cbind(melt(scene1_ts_SSML_first_F_inci_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Incidence"),"Comparison"=c("SSML")),
  cbind(melt(scene1_ts_SSML_first_M_mort_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Mortality"),"Comparison"=c("SSML")),
  cbind(melt(scene1_ts_SSML_first_F_mort_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Mortality"),"Comparison"=c("SSML")))

colnames(ARIMA_comp_Rate) <-c("Year","Cancer","Rate","Sex","Type","Comparison")
colnames(SSML_comp_Rate) <-c("Year","Cancer","Rate","Sex","Type","Comparison")

ARIMA_comp_Rate$Sex <- factor(ARIMA_comp_Rate$Sex,levels=c("Male","Female"))
SSML_comp_Rate$Sex <- factor(SSML_comp_Rate$Sex,levels=c("Male","Female"))

model_obs<-subset(nd_comp_Rate,Comparison=="OBS" & Year==2019);model_obs$Comparison <-c("ARIMA")
model_arima_pred <-rbind(model_obs,ARIMA_comp_Rate)

model_obs<-subset(nd_comp_Rate,Comparison=="OBS" & Year==2019);model_obs$Comparison <-c("SSML")
model_ssml_pred <-rbind(model_obs,SSML_comp_Rate)

##### JOINPOINT #######

load(file="/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/rawdata/jp_model_M_inci.RData")
load(file="/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/rawdata/jp_model_M_mort.RData")
load(file="/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/rawdata/jp_model_F_inci.RData")
load(file="/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/rawdata/jp_model_F_mort.RData")

jp_model_M_inci_apc<-data.frame(jp_model_M_inci$data_export)
jp_model_M_mort_apc<-data.frame(jp_model_M_mort$data_export)
jp_model_F_inci_apc<-data.frame(jp_model_F_inci$data_export)
jp_model_F_mort_apc<-data.frame(jp_model_F_mort$data_export)

jp_model_M_inci_apc_sql<-sqldf("SELECT *, MAX(year) as last_value FROM jp_model_M_inci_apc GROUP BY cancer")[,-c(6,7,8)]
jp_model_M_mort_apc_sql<-sqldf("SELECT *, MAX(year) as last_value FROM jp_model_M_mort_apc GROUP BY cancer")[,-c(6,7,8)]
jp_model_F_inci_apc_sql<-sqldf("SELECT *, MAX(year) as last_value FROM jp_model_F_inci_apc GROUP BY cancer")[,-c(6,7,8)]
jp_model_F_mort_apc_sql<-sqldf("SELECT *, MAX(year) as last_value FROM jp_model_F_mort_apc GROUP BY cancer")[,-c(6,7,8)]

jp_model_M_inci_apc_sql$apc <- as.numeric(gsub("\\*", "", jp_model_M_inci_apc_sql$apc))
jp_model_M_mort_apc_sql$apc <- as.numeric(gsub("\\*", "", jp_model_M_mort_apc_sql$apc))
jp_model_F_inci_apc_sql$apc <- as.numeric(gsub("\\*", "", jp_model_F_inci_apc_sql$apc))
jp_model_F_mort_apc_sql$apc <- as.numeric(gsub("\\*", "", jp_model_F_mort_apc_sql$apc))

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

jp_model_inci_M_dat$cancer <- rep(title_nameM, c(21,21,21,21,21,21,21,21,21,21,21,21))
jp_model_mort_M_dat$cancer <- rep(title_nameM, c(21,21,21,21,21,21,21,21,21,21,21,21))
jp_model_inci_F_dat$cancer <- rep(title_nameF, c(21,21,21,21,21,21,21,21,21))
jp_model_mort_F_dat$cancer <- rep(title_nameF, c(21,21,21,21,21,21,21,21,21))

model_jp_pred<-rbind(jp_model_inci_M_dat,jp_model_mort_M_dat,jp_model_inci_F_dat,jp_model_mort_F_dat)[,-3]
model_jp_pred<-model_jp_pred[,c(2,1,3,4,6,5)]

model_obs<-subset(nd_comp_Rate,Comparison=="OBS" & Year==2019);model_obs$Comparison <-c("JP")
colnames(model_jp_pred) <-colnames(model_obs)
model_jp_pred <-rbind(model_obs,model_jp_pred)

##### Combination #####

mdata<-rbind(model_obs,model_np_pred,model_bp_pred,model_arima_pred,model_ssml_pred,model_jp_pred)
mdata$Comparison <-factor(mdata$Comparison)
mdata$Comparison <- factor(mdata$Comparison,levels=c("OBS",'NP',"BAPC","JP", "ARIMA", "SSML"))


ggplot(subset(mdata, Sex=="Female" & Type=="Mortality" & Cancer==title_nameF[8]), aes(x = Year, y = Rate, shape=Comparison, color = Comparison, linetype = Comparison)) +
  geom_line(size = 2.0) +
  geom_point(size = 1.5,fill = "white") +
  scale_fill_viridis(discrete = T, option = "E") +
  #scale_color_manual(values = c("black", "chartreuse3", "brown3",'cadetblue3',"cornsilk3","blueviolet")) +
  scale_shape_manual(values = c(15,16,17,18,19,20)) +
  #facet_wrap(~ Type, strip.position = "top", nrow = 1) +
  theme_stata() + scale_color_stata() +
  labs(x = "Year", y = "Crude Rate (per 100,000)",title=title_nameF[8]) +
  theme(legend.position = "none",
        text=element_text(size=30),
        axis.text.x=element_text(angle=90, hjust=1),
        plot.title = element_text(size = 30, face = "bold"))



result <-list()
for ( i in 1:12){
  result[[i]] <- ggplot(subset(mdata, Sex=="Male"& Cancer==title_nameM[i]), aes(x = Year, y = Rate, shape=Comparison, color = Comparison, linetype = Comparison)) +
           geom_line(size = 2.0) +
           geom_point(size = 1.5,fill = "white") +
           scale_fill_viridis(discrete = T, option = "E") +
           #scale_color_manual(values = c("black", "chartreuse3", "brown3",'cadetblue3',"cornsilk3","blueviolet")) + 
           scale_shape_manual(values = c(15,16,17,18,19,20)) +
           #facet_wrap(~ Type, strip.position = "top", nrow = 1) +
           theme_stata() + scale_color_stata() + 
           labs(x = "Year", y = "Crude Rate (per 100,000)",title=title_nameM[i]) +
    theme(legend.position = "none",
          text=element_text(size=30),
          axis.text.x=element_text(angle=90, hjust=1),
          plot.title = element_text(size = 30, face = "bold")) 
}
result

result <-list()
for ( i in 1:9){
  result[[i]] <- ggplot(subset(mdata, Sex=="Female" &  Cancer==title_nameF[i]), aes(x = Year, y = Rate, shape=Comparison, color = Comparison, linetype = Comparison)) +
    geom_line(size = 2.0) +
    geom_point(size = 1.5,fill = "white") +
    scale_fill_viridis(discrete = T, option = "E") +
    #scale_color_manual(values = c("black", "chartreuse3", "brown3",'cadetblue3',"cornsilk3","blueviolet")) + 
    scale_shape_manual(values = c(15,16,17,18,19,20)) +
    facet_wrap(~ Type, strip.position = "top", nrow = 1) +
    theme_stata() + scale_color_stata() + 
    labs(x = "Year", y = "Crude Rate (per 100,000)",title=title_nameF[i]) +
    theme(legend.position = "none",
          text=element_text(size=30),
          axis.text.x=element_text(angle=90, hjust=1),
          plot.title = element_text(size = 30, face = "bold")) 
}
result

######################################################################
# boxplot
# Confidene Interncal check


# NORDPRED_func_performance <- function (Scenarios1_male,Scenarios1_female,type){
#   
#   NORDPRED_INCI_M_gof <-c()
#   NORDPRED_INCI_M_aic <-c()
#   
#   male_case <-subset(get(inci_cname_M[1]),Sex=="남자")[,c(as.character(2000:2019))]
#   male_pop  <- Scenarios1_male[, c(as.character(2000:2040))]
#   canproj_rslt <-canproj(male_case,male_pop,startp=2020,startestage=7, methods="nordpred")
#   
#   canproj_rslt$out
#   
#   sample <- summary(canproj_rslt)
#   sample$out$glm$fit
#   sample$out$glm$weights
#   
#   for (i in 1:12){
#     male_case <-subset(get(inci_cname_M[i]),Sex=="남자")[,c(as.character(2000:2019))]
#     male_pop  <- Scenarios1_male[, c(as.character(2000:2040))]
#     canproj_rslt <-canproj(male_case,male_pop,startp=2020,startestage=7, methods="nordpred")
#     assign(paste0(inci_cname_M[i],"_M_gof"),canproj_rslt$out$gofpvalue)
#     NORDPRED_INCI_M_gof <- cbind(NORDPRED_INCI_M_gof,get(paste0(inci_cname_M[i],"_M_gof")))
#     assign(paste0(inci_cname_M[i],"_M_aic"),canproj_rslt$out$glm$aic)
#     NORDPRED_INCI_M_aic <- cbind(NORDPRED_INCI_M_aic,get(paste0(inci_cname_M[i],"_M_aic")))
#   }
#   
#   NORDPRED_INCI_M_gof <- data.frame(NORDPRED_INCI_M_gof)
#   NORDPRED_INCI_M_aic <- data.frame(NORDPRED_INCI_M_aic)
#   
#   colnames(NORDPRED_INCI_M_gof) <- inci_cname_M
#   colnames(NORDPRED_INCI_M_aic) <- inci_cname_M
#   
#   
#   NORDPRED_INCI_F_gof <-c()
#   NORDPRED_INCI_F_aic <-c()
#   
#   for (i in 1:9){
#     female_case <-subset(get(inci_cname_F[i]),Sex=="여자")[,c(as.character(2000:2019))]
#     female_pop  <- Scenarios1_female[, c(as.character(2000:2040))]
#     canproj_rslt <-canproj(female_case,female_pop,startp=2020,startestage=7, methods="nordpred")
#     assign(paste0(inci_cname_F[i],"_F_gof"),canproj_rslt$out$gofpvalue)
#     NORDPRED_INCI_F_gof <- cbind(NORDPRED_INCI_F_gof,get(paste0(inci_cname_F[i],"_F_gof")))
#     assign(paste0(inci_cname_F[i],"_F_aic"),canproj_rslt$out$glm$aic)
#     NORDPRED_INCI_F_aic <- cbind(NORDPRED_INCI_F_aic,get(paste0(inci_cname_F[i],"_F_aic")))
#   }
#   
#   NORDPRED_INCI_F_gof <- data.frame(NORDPRED_INCI_F_gof)
#   NORDPRED_INCI_F_aic <- data.frame(NORDPRED_INCI_F_aic)
#   
#   colnames(NORDPRED_INCI_M_gof) <- inci_cname_F
#   colnames(NORDPRED_INCI_M_aic) <- inci_cname_F
#   
#   NORDPRED_MORT_M_gof <-c()
#   NORDPRED_MORT_M_aic <-c()
# 
#   for ( i in 1:12){
#     male_case <- subset(get(mort_cname_M[i]),Sex=="남자")[,c(as.character(2000:2019))]
#     male_pop  <- Scenarios1_male[, c(as.character(2000:2040))]
#     if(length(which(rowSums(male_case/20) <1)[length(which(rowSums(male_case/20) <1))]) == 0){
#       canproj_rslt <-canproj(male_case,male_pop,startp=2020,startestage=7, methods="nordpred")
#       assign(paste0(mort_cname_M[i],"_M_gof"),canproj_rslt$out$gofpvalue)
#       NORDPRED_MORT_M_gof <- cbind(NORDPRED_MORT_M_gof,get(paste0(mort_cname_M[i],"_M_gof")))
#       assign(paste0(mort_cname_M[i],"_M_aic"),canproj_rslt$out$glm$aic)
#       NORDPRED_MORT_M_aic <- cbind(NORDPRED_MORT_M_aic,get(paste0(mort_cname_M[i],"_M_aic")))
# 
#     } else if (length(which(rowSums(male_case/20) <1)[length(which(rowSums(male_case/20) <1))]) !=0) {
#       if (which(rowSums(male_case/20) <1)[length(which(rowSums(male_case/20) <1))] >7){  
#         num = as.numeric(which(rowSums(male_case/20) <1)[length(which(rowSums(male_case /20) <1))])+1
#         canproj_rslt <-canproj(male_case,female_pop,startp=2020,startestage=num, methods="nordpred")
#         assign(paste0(mort_cname_M[i],"_M_gof"),canproj_rslt$out$gofpvalue)
#         NORDPRED_MORT_M_gof <- cbind(NORDPRED_MORT_M_gof,get(paste0(mort_cname_M[i],"_M_gof")))
#         assign(paste0(mort_cname_M[i],"_M_aic"),canproj_rslt$out$glm$aic)
#         NORDPRED_MORT_M_aic <- cbind(NORDPRED_MORT_M_aic,get(paste0(mort_cname_M[i],"_M_aic")))
#       } else {
#         canproj_rslt <-canproj(male_case,female_pop,startp=2020,startestage=7, methods="nordpred")
#         assign(paste0(mort_cname_M[i],"_M_gof"),canproj_rslt$out$gofpvalue)
#         NORDPRED_MORT_M_gof <- cbind(NORDPRED_MORT_M_gof,get(paste0(mort_cname_M[i],"_M_gof")))
#         assign(paste0(mort_cname_M[i],"_M_aic"),canproj_rslt$out$glm$aic)
#         NORDPRED_MORT_M_aic <- cbind(NORDPRED_MORT_M_aic,get(paste0(mort_cname_M[i],"_M_aic")))
#       }
#     }
#   }
#   
#   NORDPRED_MORT_M_gof <- data.frame(NORDPRED_MORT_M_gof)
#   NORDPRED_MORT_M_aic <- data.frame(NORDPRED_MORT_M_aic)
#   
#   colnames(NORDPRED_MORT_M_gof) <- mort_cname_M
#   colnames(NORDPRED_MORT_M_aic) <- mort_cname_M
#   
#   
#   NORDPRED_MORT_F_gof <-c()
#   NORDPRED_MORT_F_aic <-c()
#   
#   for ( i in 1:9){
#     female_case <- subset(get(mort_cname_F[i]),Sex=="여자")[,c(as.character(2000:2019))]
#     female_pop  <- Scenarios1_female[, c(as.character(2000:2040))]
#     if(length(which(rowSums(female_case/20) <1)[length(which(rowSums(female_case/20) <1))]) == 0){
#       canproj_rslt <-canproj(female_case,female_pop,startp=2020,startestage=7, methods="nordpred")
#       assign(paste0(mort_cname_F[i],"_F_gof"),canproj_rslt$out$gofpvalue)
#       NORDPRED_MORT_F_gof <- cbind(NORDPRED_MORT_F_gof,get(paste0(mort_cname_F[i],"_F_gof")))
#       assign(paste0(mort_cname_F[i],"_F_aic"),canproj_rslt$out$glm$aic)
#       NORDPRED_MORT_F_aic <- cbind(NORDPRED_MORT_F_aic,get(paste0(mort_cname_F[i],"_F_aic")))
#       
#     } else if (length(which(rowSums(female_case/20) <1)[length(which(rowSums(female_case/20) <1))]) !=0) {
#       if (which(rowSums(female_case/20) <1)[length(which(rowSums(female_case/20) <1))] >7){  
#         num = as.numeric(which(rowSums(female_case/20) <1)[length(which(rowSums(female_case /20) <1))])+1
#         canproj_rslt <-canproj(female_case,female_pop,startp=2020,startestage=num, methods="nordpred")
#         assign(paste0(mort_cname_F[i],"_F_gof"),canproj_rslt$out$gofpvalue)
#         NORDPRED_MORT_F_gof <- cbind(NORDPRED_MORT_F_gof,get(paste0(mort_cname_F[i],"_F_gof")))
#         assign(paste0(mort_cname_F[i],"_F_aic"),canproj_rslt$out$glm$aic)
#         NORDPRED_MORT_F_aic <- cbind(NORDPRED_MORT_F_aic,get(paste0(mort_cname_F[i],"_F_aic")))
#       } else {
#         canproj_rslt <-canproj(female_case,female_pop,startp=2020,startestage=7, methods="nordpred")
#         assign(paste0(mort_cname_F[i],"_F_gof"),canproj_rslt$out$gofpvalue)
#         NORDPRED_MORT_F_gof <- cbind(NORDPRED_MORT_F_gof,get(paste0(mort_cname_F[i],"_F_gof")))
#         assign(paste0(mort_cname_F[i],"_F_aic"),canproj_rslt$out$glm$aic)
#         NORDPRED_MORT_F_aic <- cbind(NORDPRED_MORT_F_aic,get(paste0(mort_cname_F[i],"_F_aic")))
#       }
#     }
#   }
#   
#   NORDPRED_MORT_F_gof <- data.frame(NORDPRED_MORT_F_gof)
#   NORDPRED_MORT_F_aic <- data.frame(NORDPRED_MORT_F_aic)
#   
#   colnames(NORDPRED_MORT_F_gof) <- mort_cname_F
#   colnames(NORDPRED_MORT_F_aic) <- mort_cname_F
#     
#   NORDPRED_LIST_gof <-list(NORDPRED_INCI_M_gof,NORDPRED_INCI_F_gof,NORDPRED_MORT_M_gof,NORDPRED_MORT_F_gof)
#   NORDPRED_LIST_aic <-list(NORDPRED_INCI_M_aic,NORDPRED_INCI_F_aic,NORDPRED_MORT_M_aic,NORDPRED_MORT_F_aic)
#   
#   if (type =="gof"){
#     return(NORDPRED_LIST_gof)
#   } else if (type =="aic"){
#     return(NORDPRED_LIST_aic)
#   }
# }

scene1_nd_gof  <- NORDPRED_func_performance(Scenarios1_male,Scenarios1_female,"gof") 
scene1_nd_aic  <- NORDPRED_func_performance(Scenarios1_male,Scenarios1_female,"aic")


BAPC_func <- function (Scenarios1_male,Scenarios1_female){
  cancer_pred_sample <- matrix(data = NA, nrow = 21, ncol = 18) %>% as.data.frame() 
  rownames(cancer_pred_sample) <- c(seq(2020,2040,1))
  colnames(cancer_pred_sample) <-  c(1:18)
  
  BAPC_INCI_M <-c()
  BAPC_INCI_F <-c()
  
  for ( i in 1:12){
    male_case <- subset(get(inci_cname_M[1]),Sex=="남자")[,c(as.character(2000:2019))]
    male_pop  <- Scenarios1_male[, c(as.character(2000:2040))]
    test_M_pop <- data.frame(t(male_pop));colnames(test_M_pop) <- c(1:18)
    test_M_ca  <- data.frame(t(male_case));colnames(test_M_ca)  <- c(1:18)
    test_M_ca<-rbind(test_M_ca, cancer_pred_sample);colnames(test_M_ca)<- c(1:18)
    
    sample<-BAPC(APCList(test_M_ca, test_M_pop, gf=5), predict = list(npredict = 21, retro = TRUE),
         model = list(age = list(model = "rw2", prior="loggamma", param = c(1, 0.005), initial = 4, scale.model=FALSE),
                      period = list(include = TRUE, model = "drift", prior="loggamma", param = c(1, 0.005), initial = 4, scale.model=FALSE),
                      cohort = list(include = TRUE, model = "drift", prior="loggamma", param = c(1, 0.005), initial = 4, scale.model=FALSE),
                      overdis = list(include = FALSE, model = "iid", prior="loggamma", param = c(1, 0.005), initial = 4)),
         secondDiff = FALSE, stdweight = NULL, verbose = FALSE)
    
  
    
    
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
  
  for ( i in 1:9){
    female_case <- subset(get(inci_cname_F[i]),Sex=="여자")[,c(as.character(2000:2019))]
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
  
  for ( i in 1:12){
    male_case <- subset(get(mort_cname_M[i]),Sex=="남자")[,c(as.character(2000:2019))]
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
  
  for ( i in 1:9){
    female_case <- subset(get(mort_cname_F[i]),Sex=="여자")[,c(as.character(2000:2019))]
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

scene1_bp_first  <- BAPC_func(Scenarios1_male,Scenarios1_female) # 중위 (fit:2000-2019)
