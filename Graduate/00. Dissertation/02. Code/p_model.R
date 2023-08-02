################################################################################
# Chater3 
################################################################################

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
      female_case <- subset(get(mort_cname_F[i]),Sex=="여자")[,-c(1:3,ncol(get(mort_cname_M[i]))-1,ncol(get(mort_cname_M[i])))]
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


model_obs<-subset(nd_comp_Rate,Comparison=="Observation" & Year==2019);model_obs$Comparison <-c("Prediction")
model_np_pred <-rbind(model_obs,nd_comp_Rate)




#################################

result <- list()
for ( i in 1:12){
  result[[i]] <- ggplot(subset(model_np_pred, Sex=="Male" &  Cancer==title_nameM[i]), aes(x = Year, y = Rate, shape=Comparison, color = Comparison,linetype=Comparison)) +
           geom_line(size = 2.0) +
           geom_point(size = 0.7,fill = "white") +
           scale_fill_viridis(discrete = T, option = "E") +
           scale_shape_manual(values = c(15,16,17)) +
           theme_stata() + scale_color_stata() + 
           facet_wrap(~ Type, strip.position = "top", nrow = 1) +
           labs(x = "Year", y = "Crude Rate (per 100,000)",title=title_nameM[i]) +
           theme(
             legend.position = "none",
             text=element_text(size=30),
             axis.text.x=element_text(angle=90, hjust=1),
             plot.title = element_text(size = 30, face = "bold")
           )
}
result


result <- list()
for ( i in 1:9){
  ggplot(subset(nd_comp_Rate,Sex=="Female" &  Cancer==inci_cname_F[i]), aes(x = Year, y = Rate, shape=Comparison, color = Comparison,linetype=Comparison)) +
           geom_line(size = 2.0) +
           geom_point(size = 0.7,fill = "white") +
           scale_fill_viridis(discrete = T, option = "E") +
           scale_shape_manual(values = c(15,16,17)) +
           theme_stata() + scale_color_stata() + 
           facet_wrap(~ Type, strip.position = "top", nrow = 1) +
           labs(x = "Year", y = "Crude Rate (per 100,000)",title=title_nameF[i]) +
           theme(
             legend.position = "none",
             text=element_text(size=30),
             axis.text.x=element_text(angle=90, hjust=1),
             plot.title = element_text(size = 30, face = "bold")
           )
}
result



################################################################################
################################################################################

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

for ( i in 1:length(inci_cname_M)){
  assign(paste0("plot",i),ggplot(subset(bp_comp_Rate,Type==c("Incidence") & Sex=="Male" &  Cancer==inci_cname_M[i]), aes(x = Year, y = Rate, shape=Comparison, color = Comparison)) +
           geom_line(size = 1.2) +
           geom_point(size = 1.5,fill = "white") +
           scale_fill_viridis(discrete = T, option = "E") +
           #scale_linetype_manual(values = c("solid", "dashed")) +
           scale_shape_manual(values = c(15,16,17)) +
           theme_classic() +
           facet_wrap(~ Sex, strip.position = "top", nrow = 1) +
           labs(x = "Year", y = "Incidence Rate (per 100,000)",title=title_nameM_I[i]) +
           theme_ipsum() +
           theme(
             strip.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5,size=15), # 패널 레이블 위치 조정
             axis.title.x = element_text(hjust = 0.5, vjust = 0.5,size=15), # X축 이름 중앙 정렬
             axis.title.y = element_text(hjust = 0.5, vjust = 0.5,size=15), # Y축 이름 중앙 정렬
             plot.margin = margin(1, 1, 1, 1, "cm") # 그래프 안에 레이블 넣기 위해 여백 조정
           ))
}

ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot5,plot6,plot7,plot8,labels = c("E","F","G","H"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot9,labels = c("I"),ncol = 2, nrow = 2, common.legend = TRUE)



for ( i in 1:length(mort_cname_M)){
  assign(paste0("plot",i),ggplot(subset(bp_comp_Rate,Type==c("Mortality") & Sex=="Male" &  Cancer==mort_cname_M[i]), aes(x = Year, y = Rate, shape=Comparison, color = Comparison)) +
           geom_line(size = 1.2) +
           geom_point(size = 1.5,fill = "white") +
           scale_fill_viridis(discrete = T, option = "E") +
           #scale_linetype_manual(values = c("solid", "dashed")) +
           scale_shape_manual(values = c(15,16,17)) +
           theme_classic() +
           facet_wrap(~ Sex, strip.position = "top", nrow = 1) +
           labs(x = "Year", y = "Mortality Rate (per 100,000)",title=title_nameM_D[i]) +
           theme_ipsum() +
           theme(
             strip.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5,size=15), # 패널 레이블 위치 조정
             axis.title.x = element_text(hjust = 0.5, vjust = 0.5,size=15), # X축 이름 중앙 정렬
             axis.title.y = element_text(hjust = 0.5, vjust = 0.5,size=15), # Y축 이름 중앙 정렬
             plot.margin = margin(1, 1, 1, 1, "cm") # 그래프 안에 레이블 넣기 위해 여백 조정
           ))
}

ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot5,plot6,plot7,plot8,labels = c("E","F","G","H"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot9,plot10,labels = c("I","J"),ncol = 2, nrow = 2, common.legend = TRUE)


for ( i in 1:length(inci_cname_F)){
  assign(paste0("plot",i),ggplot(subset(bp_comp_Rate,Type==c("Incidence") & Sex=="Female" &  Cancer==inci_cname_F[i]), aes(x = Year, y = Rate, shape=Comparison, color = Comparison)) +
           geom_line(size = 1.2) +
           geom_point(size = 1.5,fill = "white") +
           scale_fill_viridis(discrete = T, option = "E") +
           #scale_linetype_manual(values = c("solid", "dashed")) +
           scale_shape_manual(values = c(15,16,17)) +
           theme_classic() +
           facet_wrap(~ Sex, strip.position = "top", nrow = 1) +
           labs(x = "Year", y = "Incidence Rate (per 100,000)",title=title_nameF_I[i]) +
           theme_ipsum() +
           theme(
             strip.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5,size=15), # 패널 레이블 위치 조정
             axis.title.x = element_text(hjust = 0.5, vjust = 0.5,size=15), # X축 이름 중앙 정렬
             axis.title.y = element_text(hjust = 0.5, vjust = 0.5,size=15), # Y축 이름 중앙 정렬
             plot.margin = margin(1, 1, 1, 1, "cm") # 그래프 안에 레이블 넣기 위해 여백 조정
           ))
}

ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot5,plot6,plot7,labels = c("E","F","G"),ncol = 2, nrow = 2, common.legend = TRUE)


for ( i in 1:length(mort_cname_F)){
  assign(paste0("plot",i),ggplot(subset(bp_comp_Rate,Type==c("Mortality") & Sex=="Female" &  Cancer==mort_cname_F[i]), aes(x = Year, y = Rate, shape=Comparison, color = Comparison)) +
           geom_line(size = 1.2) +
           geom_point(size = 1.5,fill = "white") +
           scale_fill_viridis(discrete = T, option = "E") +
           #scale_linetype_manual(values = c("solid", "dashed")) +
           scale_shape_manual(values = c(15,16,17)) +
           theme_classic() +
           facet_wrap(~ Sex, strip.position = "top", nrow = 1) +
           labs(x = "Year", y = "Mortality Rate (per 100,000)",title=title_nameM_D[i]) +
           theme_ipsum() +
           theme(
             strip.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5,size=15), # 패널 레이블 위치 조정
             axis.title.x = element_text(hjust = 0.5, vjust = 0.5,size=15), # X축 이름 중앙 정렬
             axis.title.y = element_text(hjust = 0.5, vjust = 0.5,size=15), # Y축 이름 중앙 정렬
             plot.margin = margin(1, 1, 1, 1, "cm") # 그래프 안에 레이블 넣기 위해 여백 조정
           ))
}

ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot5,plot6,plot7,plot8,labels = c("E","F","G","H"),ncol = 2, nrow = 2, common.legend = TRUE)




#########################################################################################################

load(file="Y:/Dissertation/newone/rawdata/workspace.RData")

library("nih.joinpoint")
library("tidyverse")
library("reshape")
library("sqldf")
library("ggplot2")
library("dplyr")
library("ggpubr")
library("glue")
################################################################################

# Joinpoint (need to Decomposting --> each age proejction needed)

# 1 all age aggregated (ver1.2000-2019 ver2.2001-2020, ver3.2002-2021(only mortality))
# 
# fit_ver <- function(comp){
#   if (comp=="first"){
#     inci_cname_M <- inci_cname[!inci_cname %in% c("C50_I","C53_I","C54_I","C56_I")]
#     inci_cname_F <- inci_cname[!inci_cname %in% c("C61_I","C62_I")]
# 
#     for ( i in 1:length(inci_cname_M)) {
#       assign(paste0(inci_cname_M[i],"_jp_dat_M_0019"),data.frame('Type'=c('Male'),'Cancer'=inci_cname_M[i],'Year' = c(2000:2019),cbind(melt(colSums(subset(get(inci_cname_M[i]),Sex=="남자")[,-c(1:3,24)])),"value2"=melt(colSums(Scenarios1_male[, c(as.character(2000:2019))])))))
#     }
#     for ( i in 1:length(inci_cname_F)) {
#       assign(paste0(inci_cname_F[i],"_jp_dat_F_0019"),data.frame('Type'=c('Female'),'Cancer'=inci_cname_F[i],'Year' = c(2000:2019),cbind(melt(colSums(subset(get(inci_cname_F[i]),Sex=="여자")[,-c(1:3,24)])),"value2"=melt(colSums(Scenarios1_female[, c(as.character(2000:2019))])))))
#     }
# 
#     mort_cname_M <- mort_cname[!mort_cname %in% c("C50_D","C53_D","C54_C55_D","C56_D")]
#     mort_cname_F <- mort_cname[!mort_cname %in% c("C61_D")]
# 
#     for ( i in 1:length(mort_cname_M)) {
#       assign(paste0(mort_cname_M[i],"_jp_dat_M_0019"),data.frame('Type'=c('Male'),'Cancer'=mort_cname_M[i],'Year' = c(2000:2019),cbind(melt(colSums(subset(get(mort_cname_M[i]),Sex=="남자")[,-c(1:3,24,25)])),"value2"=melt(colSums(Scenarios1_male[, c(as.character(2000:2019))])))))
#     }
#     for ( i in 1:length(mort_cname_F)) {
#       assign(paste0(mort_cname_F[i],"_jp_dat_F_0019"),data.frame('Type'=c('Female'),'Cancer'=mort_cname_F[i],'Year' = c(2000:2019),cbind(melt(colSums(subset(get(mort_cname_F[i]),Sex=="여자")[,-c(1:3,24,25)])),"value2"=melt(colSums(Scenarios1_female[, c(as.character(2000:2019))])))))
#     }
# 
#     male_ijp_dat_0019<-data.frame("Type"=c(),"Cancer"=c(),"Year"=c(),"value"=c(),"value.1"=c())
#     female_ijp_dat_0019<-data.frame("Type"=c(),"Cancer"=c(),"Year"=c(),"value"=c(),"value.1"=c())
# 
#     male_mjp_dat_0019<-data.frame("Type"=c(),"Cancer"=c(),"Year"=c(),"value"=c(),"value.1"=c())
#     female_mjp_dat_0019<-data.frame("Type"=c(),"Cancer"=c(),"Year"=c(),"value"=c(),"value.1"=c())
# 
# 
#     for (i in 1:length(inci_cname_M)){
#       assign(paste0("male_ijp_dat_0019"),rbind(get(paste0("male_ijp_dat_0019")),get(paste0(inci_cname_M[i],"_jp_dat_M_0019"))))
#     }
#     for (i in 1:length(inci_cname_F)){
#       assign(paste0("female_ijp_dat_0019"),rbind(get(paste0("female_ijp_dat_0019")),get(paste0(inci_cname_F[i],"_jp_dat_F_0019"))))
#     }
#     for (i in 1:length(mort_cname_M)){
#       assign(paste0("male_mjp_dat_0019"),rbind(get(paste0("male_mjp_dat_0019")),get(paste0(mort_cname_M[i],"_jp_dat_M_0019"))))
#     }
#     for (i in 1:length(mort_cname_F)){
#       assign(paste0("female_mjp_dat_0019"),rbind(get(paste0("female_mjp_dat_0019")),get(paste0(mort_cname_F[i],"_jp_dat_F_0019"))))
#     }
#     inci_jp_dat_0019 <- rbind(male_ijp_dat_0019,female_ijp_dat_0019)
#     mort_jp_dat_0019 <- rbind(male_mjp_dat_0019,female_mjp_dat_0019)
# 
#     colnames(inci_jp_dat_0019) <- c("sex","cancer","year","case","population")
#     colnames(mort_jp_dat_0019) <- c("sex","cancer","year","case","population")
# 
#     inci_jp_dat_0019$Crude_Rate <- inci_jp_dat_0019$case/inci_jp_dat_0019$population*100000
#     mort_jp_dat_0019$Crude_Rate <- mort_jp_dat_0019$case/mort_jp_dat_0019$population*100000
# 
#     inci_jp_dat_0019$Type <-c('Incidence')
#     mort_jp_dat_0019$Type <-c('Mortality')
# 
#     jp_dat <- rbind(inci_jp_dat_0019,mort_jp_dat_0019)
#     jp_dat$Comp <-c("first")
# 
#   }else if (comp=="second"){
#     inci_cname_M <- inci_cname[!inci_cname %in% c("C50_I","C53_I","C54_I","C56_I")]
#     inci_cname_F <- inci_cname[!inci_cname %in% c("C61_I","C62_I")]
# 
#     for ( i in 1:length(inci_cname_M)) {
#       assign(paste0(inci_cname_M[i],"_jp_dat_M_0120"),data.frame('Type'=c('Male'),'Cancer'=inci_cname_M[i],'Year' = c(2001:2020),cbind(melt(colSums(subset(get(inci_cname_M[i]),Sex=="남자")[,-c(1:4)])),"value2"=melt(colSums(Scenarios1_male[, c(as.character(2001:2020))])))))
#     }
#     for ( i in 1:length(inci_cname_F)) {
#       assign(paste0(inci_cname_F[i],"_jp_dat_F_0120"),data.frame('Type'=c('Female'),'Cancer'=inci_cname_F[i],'Year' = c(2001:2020),cbind(melt(colSums(subset(get(inci_cname_F[i]),Sex=="여자")[,-c(1:4)])),"value2"=melt(colSums(Scenarios1_female[, c(as.character(2001:2020))])))))
#     }
# 
#     mort_cname_M <- mort_cname[!mort_cname %in% c("C50_D","C53_D","C54_C55_D","C56_D")]
#     mort_cname_F <- mort_cname[!mort_cname %in% c("C61_D")]
# 
#     for ( i in 1:length(mort_cname_M)) {
#       assign(paste0(mort_cname_M[i],"_jp_dat_M_0120"),data.frame('Type'=c('Male'),'Cancer'=mort_cname_M[i],'Year' = c(2001:2020),cbind(melt(colSums(subset(get(mort_cname_M[i]),Sex=="남자")[,-c(1:4,25)])),"value2"=melt(colSums(Scenarios1_male[, c(as.character(2001:2020))])))))
#     }
#     for ( i in 1:length(mort_cname_F)) {
#       assign(paste0(mort_cname_F[i],"_jp_dat_F_0120"),data.frame('Type'=c('Female'),'Cancer'=mort_cname_F[i],'Year' = c(2001:2020),cbind(melt(colSums(subset(get(mort_cname_F[i]),Sex=="여자")[,-c(1:4,25)])),"value2"=melt(colSums(Scenarios1_female[, c(as.character(2001:2020))])))))
#     }
# 
#     male_ijp_dat_0120<-data.frame("Type"=c(),"Cancer"=c(),"Year"=c(),"value"=c(),"value.1"=c())
#     female_ijp_dat_0120<-data.frame("Type"=c(),"Cancer"=c(),"Year"=c(),"value"=c(),"value.1"=c())
# 
#     male_mjp_dat_0120<-data.frame("Type"=c(),"Cancer"=c(),"Year"=c(),"value"=c(),"value.1"=c())
#     female_mjp_dat_0120<-data.frame("Type"=c(),"Cancer"=c(),"Year"=c(),"value"=c(),"value.1"=c())
# 
#     for (i in 1:length(inci_cname_M)){
#       assign(paste0("male_ijp_dat_0120"),rbind(get(paste0("male_ijp_dat_0120")),get(paste0(inci_cname_M[i],"_jp_dat_M_0120"))))
#     }
#     for (i in 1:length(inci_cname_F)){
#       assign(paste0("female_ijp_dat_0120"),rbind(get(paste0("female_ijp_dat_0120")),get(paste0(inci_cname_F[i],"_jp_dat_F_0120"))))
#     }
#     for (i in 1:length(mort_cname_M)){
#       assign(paste0("male_mjp_dat_0120"),rbind(get(paste0("male_mjp_dat_0120")),get(paste0(mort_cname_M[i],"_jp_dat_M_0120"))))
#     }
#     for (i in 1:length(mort_cname_F)){
#       assign(paste0("female_mjp_dat_0120"),rbind(get(paste0("female_mjp_dat_0120")),get(paste0(mort_cname_F[i],"_jp_dat_F_0120"))))
#     }
# 
#     inci_jp_dat_0120 <- rbind(male_ijp_dat_0120,female_ijp_dat_0120)
#     mort_jp_dat_0120 <- rbind(male_mjp_dat_0120,female_mjp_dat_0120)
# 
#     colnames(inci_jp_dat_0120) <- c("sex","cancer","year","case","population")
#     colnames(mort_jp_dat_0120) <- c("sex","cancer","year","case","population")
# 
#     inci_jp_dat_0120$Crude_Rate <- inci_jp_dat_0120$case/inci_jp_dat_0120$population*100000
#     mort_jp_dat_0120$Crude_Rate <- mort_jp_dat_0120$case/mort_jp_dat_0120$population*100000
# 
#     inci_jp_dat_0120$Type <-c('Incidence')
#     mort_jp_dat_0120$Type <-c('Mortality')
# 
#     jp_dat <- rbind(inci_jp_dat_0120,mort_jp_dat_0120)
#     jp_dat$Comp <-c("second")
#   }else if (comp=='third'){
# 
#     mort_cname_M <- mort_cname[!mort_cname %in% c("C50_D","C53_D","C54_C55_D","C56_D")]
#     mort_cname_F <- mort_cname[!mort_cname %in% c("C61_D")]
# 
#     for ( i in 1:length(mort_cname_M)) {
#       assign(paste0(mort_cname_M[i],"_jp_dat_M_0221"),data.frame('Type'=c('Male'),'Cancer'=mort_cname_M[i],'Year' = c(2002:2021),cbind(melt(colSums(subset(get(mort_cname_M[i]),Sex=="남자")[,-c(1:5)])),"value2"=melt(colSums(Scenarios1_male[, c(as.character(2002:2021))])))))
#     }
#     for ( i in 1:length(mort_cname_F)) {
#       assign(paste0(mort_cname_F[i],"_jp_dat_F_0221"),data.frame('Type'=c('Female'),'Cancer'=mort_cname_F[i],'Year' = c(2002:2021),cbind(melt(colSums(subset(get(mort_cname_F[i]),Sex=="여자")[,-c(1:5)])),"value2"=melt(colSums(Scenarios1_female[, c(as.character(2002:2021))])))))
#     }
# 
#     male_mjp_dat_0221<-data.frame("Type"=c(),"Cancer"=c(),"Year"=c(),"value"=c(),"value.1"=c())
#     female_mjp_dat_0221<-data.frame("Type"=c(),"Cancer"=c(),"Year"=c(),"value"=c(),"value.1"=c())
# 
#     for (i in 1:length(mort_cname_M)){
#       assign(paste0("male_mjp_dat_0221"),rbind(get(paste0("male_mjp_dat_0221")),get(paste0(mort_cname_M[i],"_jp_dat_M_0221"))))
#     }
#     for (i in 1:length(mort_cname_F)){
#       assign(paste0("female_mjp_dat_0221"),rbind(get(paste0("female_mjp_dat_0221")),get(paste0(mort_cname_F[i],"_jp_dat_F_0221"))))
#     }
#     mort_jp_dat_0221 <- rbind(male_mjp_dat_0221,female_mjp_dat_0221)
#     colnames(mort_jp_dat_0221) <- c("sex","cancer","year","case","population")
#     mort_jp_dat_0221$Crude_Rate <- mort_jp_dat_0221$case/mort_jp_dat_0221$population*100000
# 
#     mort_jp_dat_0221$Type <-c('Mortality')
# 
#     jp_dat<- mort_jp_dat_0221
#     jp_dat$Comp <-c("third")
#   }
#   return(jp_dat)
# }
# 
# first_comp<-fit_ver("first")
# second_comp<-fit_ver("second")
# third_comp<-fit_ver("third")
# 
# comp<-data.frame(rbind(first_comp,second_comp,third_comp))
# comp<-cbind(comp[,c(7,8)],comp[,c(1,2,3,4,5,6)])
# rownames(comp)<-NULL
# 
# 
# # ###############################################################################################
# ## running only windows os setting
# 
inci_cname_M <- inci_cname[inci_cname %in% c("C16_I","C18_C20_I","C22_I","C23_C24_I","C25_I","C33_C34_I","C61_I","C62_I","C73_I","C82_C86_C96_I")]
inci_cname_F <- inci_cname[inci_cname %in% c("C16_I","C18_C20_I","C22_I","C23_C24_I","C25_I","C33_C34_I","C50_I","C53_I","C54_I","C56_I","C73_I","C82_C86_C96_I")]
mort_cname_M <- mort_cname[mort_cname %in% c("C15_D","C16_D","C18_C21_D","C22_D","C25_D","C33_C34_D","C61_D","C67_D","C82_C86_D","C91_C95_D")]
mort_cname_F <- mort_cname[mort_cname %in% c("C15_D","C16_D","C18_C21_D","C22_D","C25_D","C33_C34_D","C50_D","C53_D","C56_D","C67_D","C82_C86_D","C91_C95_D")]

# run_opt = run_options(model="ln", max_joinpoints=3, n_cores=3, model_selection_method = c("permutation test"))
# export_opt = export_options()
# 
# comp_M_inci <- subset(comp,sex=="Male" & Type == 'Incidence')[,-c(1,3)]
# comp_M_mort <- subset(comp,sex=="Male" & Type == 'Mortality')[,-c(1,3)]
# comp_F_inci <- subset(comp,sex=="Female" & Type == 'Incidence')[,-c(1,3)]
# comp_F_mort <- subset(comp,sex=="Female" & Type == 'Mortality')[,-c(1,3)]
# 
# comp_M_inci <-subset(comp_M_inci, cancer %in% inci_cname_M)
# comp_M_mort <-subset(comp_M_mort, cancer %in% mort_cname_M)
# comp_F_inci <-subset(comp_F_inci, cancer %in% inci_cname_F)
# comp_F_mort <-subset(comp_F_mort, cancer %in% mort_cname_F)
# 
# colnames(comp_M_inci) <-c("comparison","cancer","year",'case','population','rate')
# colnames(comp_M_mort) <-c("comparison","cancer","year",'case','population','rate')
# colnames(comp_F_inci) <-c("comparison","cancer","year",'case','population','rate')
# colnames(comp_F_mort) <-c("comparison","cancer","year",'case','population','rate')
# 
# jp_model_M_inci<-joinpoint(comp_M_inci, x=year, y=rate, by=c("comparison","cancer"),run_opts=run_opt, export_opts=export_opt)
# jp_model_M_mort<-joinpoint(comp_M_mort, x=year, y=rate, by=c("comparison","cancer"),run_opts=run_opt, export_opts=export_opt)
# jp_model_F_inci<-joinpoint(comp_F_inci, x=year, y=rate, by=c("comparison","cancer"),run_opts=run_opt, export_opts=export_opt)
# jp_model_F_mort<-joinpoint(comp_F_mort, x=year, y=rate, by=c("comparison","cancer"),run_opts=run_opt, export_opts=export_opt)
# 
# 
# save(jp_model_M_inci,file="Y:/Dissertation/newone/rawdata/jp_model_M_inci.RData")
# save(jp_model_M_mort,file="Y:/Dissertation/newone/rawdata/jp_model_M_mort.RData")
# save(jp_model_F_inci,file="Y:/Dissertation/newone/rawdata/jp_model_F_inci.RData")
# save(jp_model_F_mort,file="Y:/Dissertation/newone/rawdata/jp_model_F_mort.RData")

#############################################################################################################################

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

check<-function(jp_model_M_inci_apc_sql,inci_cname_M,comp){
  model_rslt_all<-data.frame('comparison'=c(),'cancer'=c(),'year'=c(),'rate'=c(),'model'=c())
  for (i in 1:length(inci_cname_M)) {
    
    if (comp=="first") {
      data <- subset(jp_model_M_inci_apc_sql,cancer==inci_cname_M[i] & comparison =="first")
      year=2019
    } else if(comp=='second'){
      data <- subset(jp_model_M_inci_apc_sql,cancer==inci_cname_M[i] & comparison =='second')
      year=2020
    } else if(comp=="third"){
      data <- subset(jp_model_M_inci_apc_sql,cancer==inci_cname_M[i] & comparison =='third')
      year=2021
    }
    
    cancer <- data$cancer
    k=1
    
    next_year  <-c()
    next_rate  <-c()
    next_model <-c()
    next_apc <-c()
    
    dat_model <-data$model
    dat_apc <-data$apc
    
    while(year < 2040) {
      # 다음 연도의 값을 계산
      next_year[k] <-  year +1
      next_rate[k] <-  dat_model+(dat_model*dat_apc*0.01)
      next_model[k] <- dat_model+(dat_model*dat_apc*0.01)
      next_apc[k] <- dat_apc
      dat_model = dat_model+(dat_model*dat_apc*0.01)
      year=year+1
      k=k+1
    }
    sample_data  <- rbind(data[,-6],data.frame('comparison'=comp,'cancer'=cancer,'year'=next_year,'rate'=next_rate,'model'=next_model))[-1,]
    model_rslt_all<-rbind(model_rslt_all,sample_data)
  }
  return(model_rslt_all)
}


jp_model_inci_M_dat<-rbind(check(jp_model_M_inci_apc_sql,inci_cname_M,'first'),check(jp_model_M_inci_apc_sql,inci_cname_M,'second'))
jp_model_mort_M_dat<-rbind(check(jp_model_M_mort_apc_sql,mort_cname_M,'first'),check(jp_model_M_mort_apc_sql,mort_cname_M,'second'),check(jp_model_M_mort_apc_sql,mort_cname_M,'third'))
jp_model_inci_F_dat<-rbind(check(jp_model_F_inci_apc_sql,inci_cname_F,'first'),check(jp_model_F_inci_apc_sql,inci_cname_F,'second'))
jp_model_mort_F_dat<-rbind(check(jp_model_F_mort_apc_sql,mort_cname_F,'first'),check(jp_model_F_mort_apc_sql,mort_cname_F,'second'),check(jp_model_F_mort_apc_sql,mort_cname_F,'third'))

####################################################################################################################################################################################
title_nameM_I<-c("Stomach (C16)","Colorectal (C18-C20)","Liver (C22)","Gallbladder and other biliary tract (C23-C24)",
                 "Pancreas (C25)","Lung (C33-C34)","Prostate (C61)","Testis (C62)","Thyroid (C73)","Non-Hodgkin lymphoma (C82-C86, C96)")

title_nameF_I<-c("Stomach (C16)","Colorectal (C18-C20)","Liver (C22)","Gallbladder and other biliary tract (C23-C24)",
                 "Pancreas (C25)","Lung (C33-C34)","Breast (C50)","Cervix uteri (C53)","Corpus uteri (C54)","Ovary (C56)","Thyroid (C73)","Non-Hodgkin lymphoma (C82-C86, C96)")

title_nameM_D<-c("Esophagus (C15)","Stomach (C16)","Colorectal & Anus (C18-C21)","Liver (C22)",
                 "Pancreas (C25)","Lung (C33-C34)","Prostate (C61)","Bladder (C67)","Non-Hodgkin lymphoma (C82-C86)","Leukemia (C91-C95)")

title_nameF_D<-c("Esophagus (C15)","Stomach (C16)","Colorectal & Anus (C18-C21)","Liver (C22)",
                 "Pancreas (C25)","Lung (C33-C34)","Breast (C50)","Cervix uteri (C53)","Ovary (C56)","Bladder (C67)","Non-Hodgkin lymphoma (C82-C86)","Leukemia (C91-C95)")


Cancer_Name = inci_cname_M
Case = "Incidence"
for (i in 1: length(Cancer_Name)){
  data = subset(jp_model_M_inci$data_export[,-c(7,8)],cancer==Cancer_Name[i])[,-2]
  # 변수 지정
  x <- sym(attr(data, "variables")$x)
  y <- sym(attr(data, "variables")$y)
  by <- attr(data, "variables")$by[i]
  v <- intersect(c("apc", "slope"), names(data))
  
  # 라벨 지정
  v_label <- ifelse(v == "apc", "Annual Percent Change", "Slope")
  v <- sym(v)
  
  legend_pattern = getOption("jp_plot_legend_pattern", "{comp} | {xmin}-{xmax}: {slope}")
  title_pattern = getOption("jp_plot_title_pattern", "{key}={val}")
  
  # 그래프 데이터 생성
  data <- data %>% 
    mutate(slope0 = na_if(!!v, ".") %>% zoo::na.locf(fromLast = TRUE) %>% as_factor()) %>% 
    group_by(.data$slope0) %>% 
    mutate(slope = .data$slope0[1], comp=comparison,xmin = min({{x}}), xmax = max({{x}}), `:=`(!!v, glue(legend_pattern))) %>%
    as.data.frame()
  
  data<-data[,c(1:5)]
  
  # 투영 모형 데이터 결합
  
  if (Case=="Incidence"){
    proj<-rbind(cbind(subset(jp_model_inci_M_dat,cancer==Cancer_Name[i])[,-2] %>% filter(comparison=="first"),'apc'=c("first | 2020-2040")),
                cbind(subset(jp_model_inci_M_dat,cancer==Cancer_Name[i])[,-2] %>% filter(comparison=="second"),'apc'=c("second | 2021-2040")))
  } else if (Case=="Mortality"){
    proj<-rbind(cbind(subset(jp_model_mort_M_dat,cancer==Cancer_Name[i])[,-2] %>% filter(comparison=="first"),'apc'=c("first | 2020-2040")),
                cbind(subset(jp_model_mort_M_dat,cancer==Cancer_Name[i])[,-2] %>% filter(comparison=="second"),'apc'=c("second | 2021-2040")),
                cbind(subset(jp_model_mort_M_dat,cancer==Cancer_Name[i])[,-2] %>% filter(comparison=="third"),'apc'=c("third | 2022-2040")))
  }
  
  proj$model <- round(as.numeric(proj$model),2)
  proj$rate <-proj$model
  
  data <-rbind(data,proj)
  
  assign(paste0("plot",i),data %>% 
           ggplot(aes(x = {{x}}, y = {{y}})) + 
           geom_point() + 
           labs(color = v_label) + 
           geom_line(aes(y = .data$model, color = !!v, group = .data$apc), size = 1) + 
           labs(x = "Year", y = "Incidence Rate (per 100,000)",title=title_nameM_I[i]) +
           ylim(0, NA))
}


ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2)
ggarrange(plot5,plot6,plot7,plot8,labels = c("E","F","G","H"),ncol = 2, nrow = 2)
ggarrange(plot9,plot10,abels = c("I","J"),ncol = 2, nrow = 2)

############################################################################################

Cancer_Name = mort_cname_M
Case = "Mortality"
for (i in 1: length(Cancer_Name)){
  data = subset(jp_model_M_mort$data_export[,-c(7,8)],cancer==Cancer_Name[i])[,-2]
  # 변수 지정
  x <- sym(attr(data, "variables")$x)
  y <- sym(attr(data, "variables")$y)
  by <- attr(data, "variables")$by[i]
  v <- intersect(c("apc", "slope"), names(data))
  
  # 라벨 지정
  v_label <- ifelse(v == "apc", "Annual Percent Change", "Slope")
  v <- sym(v)
  
  legend_pattern = getOption("jp_plot_legend_pattern", "{comp} | {xmin}-{xmax}: {slope}")
  title_pattern = getOption("jp_plot_title_pattern", "{key}={val}")
  
  # 그래프 데이터 생성
  data <- data %>% 
    mutate(slope0 = na_if(!!v, ".") %>% zoo::na.locf(fromLast = TRUE) %>% as_factor()) %>% 
    group_by(.data$slope0) %>% 
    mutate(slope = .data$slope0[1], comp=comparison,xmin = min({{x}}), xmax = max({{x}}), `:=`(!!v, glue(legend_pattern))) %>%
    as.data.frame()
  
  data<-data[,c(1:5)]
  
  # 투영 모형 데이터 결합
  
  if (Case=="Incidence"){
    proj<-rbind(cbind(subset(jp_model_inci_M_dat,cancer==Cancer_Name[i])[,-2] %>% filter(comparison=="first"),'apc'=c("first | 2020-2040")),
                cbind(subset(jp_model_inci_M_dat,cancer==Cancer_Name[i])[,-2] %>% filter(comparison=="second"),'apc'=c("second | 2021-2040")))
  } else if (Case=="Mortality"){
    proj<-rbind(cbind(subset(jp_model_mort_M_dat,cancer==Cancer_Name[i])[,-2] %>% filter(comparison=="first"),'apc'=c("first | 2020-2040")),
                cbind(subset(jp_model_mort_M_dat,cancer==Cancer_Name[i])[,-2] %>% filter(comparison=="second"),'apc'=c("second | 2021-2040")),
                cbind(subset(jp_model_mort_M_dat,cancer==Cancer_Name[i])[,-2] %>% filter(comparison=="third"),'apc'=c("third | 2022-2040")))
  }
  
  proj$model <- round(as.numeric(proj$model),2)
  proj$rate <-proj$model
  
  data <-rbind(data,proj)
  
  assign(paste0("plot",i),data %>% 
           ggplot(aes(x = {{x}}, y = {{y}})) + 
           geom_point() + 
           labs(color = v_label) + 
           geom_line(aes(y = .data$model, color = !!v, group = .data$apc), size = 1) + 
           labs(x = "Year", y = "Mortality Rate (per 100,000)",title=title_nameM_D[i]) +
           ylim(0, NA))
}



ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2)
ggarrange(plot5,plot6,plot7,plot8,labels = c("E","F","G","H"),ncol = 2, nrow = 2)
ggarrange(plot9,plot10,abels = c("I","J"),ncol = 2, nrow = 2)


############################################################################################

Cancer_Name = inci_cname_F
Case = "Incidence"

for (i in 1: length(Cancer_Name)){
  data = subset(jp_model_F_inci$data_export[,-c(7,8)],cancer==Cancer_Name[i])[,-2]
  # 변수 지정
  x <- sym(attr(data, "variables")$x)
  y <- sym(attr(data, "variables")$y)
  by <- attr(data, "variables")$by[i]
  v <- intersect(c("apc", "slope"), names(data))
  
  # 라벨 지정
  v_label <- ifelse(v == "apc", "Annual Percent Change", "Slope")
  v <- sym(v)
  
  legend_pattern = getOption("jp_plot_legend_pattern", "{comp} | {xmin}-{xmax}: {slope}")
  title_pattern = getOption("jp_plot_title_pattern", "{key}={val}")
  
  # 그래프 데이터 생성
  data <- data %>% 
    mutate(slope0 = na_if(!!v, ".") %>% zoo::na.locf(fromLast = TRUE) %>% as_factor()) %>% 
    group_by(.data$slope0) %>% 
    mutate(slope = .data$slope0[1], comp=comparison,xmin = min({{x}}), xmax = max({{x}}), `:=`(!!v, glue(legend_pattern))) %>%
    as.data.frame()
  
  data<-data[,c(1:5)]
  
  # 투영 모형 데이터 결합
  
  if (Case=="Incidence"){
    proj<-rbind(cbind(subset(jp_model_inci_F_dat,cancer==Cancer_Name[i])[,-2] %>% filter(comparison=="first"),'apc'=c("first | 2020-2040")),
                cbind(subset(jp_model_inci_F_dat,cancer==Cancer_Name[i])[,-2] %>% filter(comparison=="second"),'apc'=c("second | 2021-2040")))
  } else if (Case=="Mortality"){
    proj<-rbind(cbind(subset(jp_model_mort_F_dat,cancer==Cancer_Name[i])[,-2] %>% filter(comparison=="first"),'apc'=c("first | 2020-2040")),
                cbind(subset(jp_model_mort_F_dat,cancer==Cancer_Name[i])[,-2] %>% filter(comparison=="second"),'apc'=c("second | 2021-2040")),
                cbind(subset(jp_model_mort_F_dat,cancer==Cancer_Name[i])[,-2] %>% filter(comparison=="third"),'apc'=c("third | 2022-2040")))
  }
  
  proj$model <- round(as.numeric(proj$model),2)
  proj$rate <-proj$model
  
  data <-rbind(data,proj)
  
  assign(paste0("plot",i),data %>% 
           ggplot(aes(x = {{x}}, y = {{y}})) + 
           geom_point() + 
           labs(color = v_label) + 
           geom_line(aes(y = .data$model, color = !!v, group = .data$apc), size = 1) + 
           labs(x = "Year", y = "Incidence Rate (per 100,000)",title=title_nameF_I[i]) +
           ylim(0, NA))
}


ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2)
ggarrange(plot5,plot6,plot7,plot8,labels = c("E","F","G","H"),ncol = 2, nrow = 2)
ggarrange(plot9,plot10,plot11,plot12,labels = c("I","J","K","L"),ncol = 2, nrow = 2)



############################################################################################

Cancer_Name = mort_cname_F
Case = "Mortality"
for (i in 1: length(Cancer_Name)){
  data = subset(jp_model_F_mort$data_export[,-c(7,8)],cancer==Cancer_Name[i])[,-2]
  # 변수 지정
  x <- sym(attr(data, "variables")$x)
  y <- sym(attr(data, "variables")$y)
  by <- attr(data, "variables")$by[i]
  v <- intersect(c("apc", "slope"), names(data))
  
  # 라벨 지정
  v_label <- ifelse(v == "apc", "Annual Percent Change", "Slope")
  v <- sym(v)
  
  legend_pattern = getOption("jp_plot_legend_pattern", "{comp} | {xmin}-{xmax}: {slope}")
  title_pattern = getOption("jp_plot_title_pattern", "{key}={val}")
  
  # 그래프 데이터 생성
  data <- data %>% 
    mutate(slope0 = na_if(!!v, ".") %>% zoo::na.locf(fromLast = TRUE) %>% as_factor()) %>% 
    group_by(.data$slope0) %>% 
    mutate(slope = .data$slope0[1], comp=comparison,xmin = min({{x}}), xmax = max({{x}}), `:=`(!!v, glue(legend_pattern))) %>%
    as.data.frame()
  
  data<-data[,c(1:5)]
  
  # 투영 모형 데이터 결합
  
  if (Case=="Incidence"){
    proj<-rbind(cbind(subset(jp_model_inci_F_dat,cancer==Cancer_Name[i])[,-2] %>% filter(comparison=="first"),'apc'=c("first | 2020-2040")),
                cbind(subset(jp_model_inci_F_dat,cancer==Cancer_Name[i])[,-2] %>% filter(comparison=="second"),'apc'=c("second | 2021-2040")))
  } else if (Case=="Mortality"){
    proj<-rbind(cbind(subset(jp_model_mort_F_dat,cancer==Cancer_Name[i])[,-2] %>% filter(comparison=="first"),'apc'=c("first | 2020-2040")),
                cbind(subset(jp_model_mort_F_dat,cancer==Cancer_Name[i])[,-2] %>% filter(comparison=="second"),'apc'=c("second | 2021-2040")),
                cbind(subset(jp_model_mort_F_dat,cancer==Cancer_Name[i])[,-2] %>% filter(comparison=="third"),'apc'=c("third | 2022-2040")))
  }
  
  proj$model <- round(as.numeric(proj$model),2)
  proj$rate <-proj$model
  
  data <-rbind(data,proj)
  
  assign(paste0("plot",i),data %>% 
           ggplot(aes(x = {{x}}, y = {{y}})) + 
           geom_point() + 
           labs(color = v_label) + 
           geom_line(aes(y = .data$model, color = !!v, group = .data$apc), size = 1) + 
           labs(x = "Year", y = "Mortality Rate (per 100,000)",title=title_nameF_D[i]) +
           ylim(0, NA))
}


ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2)
ggarrange(plot5,plot6,plot7,plot8,labels = c("E","F","G","H"),ncol = 2, nrow = 2)
ggarrange(plot9,plot10,plot11,plot12,labels = c("I","J","K","L"),ncol = 2, nrow = 2)

#####################################################################################################

# Population structure model

model_obs<-subset(nd_comp_Rate,Comparison=="Observation")

model_np_pred<-subset(nd_comp_Rate,Comparison=="Prediction");model_np_pred$Comparison <-c("Nordpred")
model_bp_pred<-subset(bp_comp_Rate,Comparison=="Prediction");model_bp_pred$Comparison <-c("BAPC")

model_obs_pred1<-subset(model_obs,Year==2019);model_obs_pred1$Comparison <-c("Nordpred")
model_obs_pred2<-subset(model_obs,Year==2019);model_obs_pred2$Comparison <-c("BAPC")

model_np_pred <-rbind(model_obs_pred1,model_np_pred)
model_bp_pred <-rbind(model_obs_pred2,model_bp_pred)

mdata<-rbind(model_obs,model_np_pred,model_bp_pred)

mdata$Comparison <-factor(mdata$Comparison)
mdata$Comparison <-factor(mdata$Comparison)
mdata$Comparison <- factor(mdata$Comparison,levels=c("Observation","Nordpred", "BAPC"))

for ( i in 1:length(inci_cname_M)){
  assign(paste0("plot",i),ggplot(subset(mdata,Type==c("Incidence") & Sex=="Male" &  Cancer==inci_cname_M[i]), aes(x = Year, y = Rate, shape=Comparison, color = Comparison)) +
           geom_line(size = 1.2) +
           geom_point(size = 1.5,fill = "white") +
           scale_fill_viridis(discrete = T, option = "E") +
           scale_color_manual(values = c("black", "chartreuse3", "brown3")) + 
           scale_shape_manual(values = c(15,16,17)) +
           theme_classic() +
           facet_wrap(~ Sex, strip.position = "top", nrow = 1) +
           labs(x = "Year", y = "Incidence Rate (per 100,000)",title=title_nameM_I[i]) +
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
ggarrange(plot9,labels = c("I"),ncol = 2, nrow = 2, common.legend = TRUE)

ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,labels = c("A","B","C","D","E","F","G","H","I"),ncol = 2, nrow = 5, common.legend = TRUE)


for ( i in 1:length(mort_cname_M)){
  assign(paste0("plot",i),ggplot(subset(mdata,Type==c("Mortality") & Sex=="Male" &  Cancer==mort_cname_M[i]), aes(x = Year, y = Rate, shape=Comparison, color = Comparison)) +
           geom_line(size = 1.2) +
           geom_point(size = 1.5,fill = "white") +
           scale_fill_viridis(discrete = T, option = "E") +
           scale_color_manual(values = c("black", "chartreuse3", "brown3")) + 
           scale_shape_manual(values = c(15,16,17)) +
           theme_classic() +
           facet_wrap(~ Sex, strip.position = "top", nrow = 1) +
           labs(x = "Year", y = "Mortality Rate (per 100,000)",title=title_nameM_D[i]) +
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
ggarrange(plot9,labels = c("I"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,labels = c("A","B","C","D","E","F","G","H","I"),ncol = 2, nrow = 5, common.legend = TRUE)


for ( i in 1:length(inci_cname_F)){
  assign(paste0("plot",i),ggplot(subset(mdata,Type==c("Incidence") & Sex=="Female" &  Cancer==inci_cname_F[i]), aes(x = Year, y = Rate, shape=Comparison, color = Comparison)) +
           geom_line(size = 1.2) +
           geom_point(size = 1.5,fill = "white") +
           scale_fill_viridis(discrete = T, option = "E") +
           scale_color_manual(values = c("black", "chartreuse3", "brown3")) + 
           scale_shape_manual(values = c(15,16,17)) +
           theme_classic() +
           facet_wrap(~ Sex, strip.position = "top", nrow = 1) +
           labs(x = "Year", y = "Incidence Rate (per 100,000)",title=title_nameF_I[i]) +
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
ggarrange(plot5,plot6,labels = c("E","F"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,labels = c("A","B","C","D","E","F"),ncol = 2, nrow = 5, common.legend = TRUE)


for ( i in 1:length(mort_cname_F)){
  assign(paste0("plot",i),ggplot(subset(mdata,Type==c("Mortality") & Sex=="Female" &  Cancer==mort_cname_F[i]), aes(x = Year, y = Rate, shape=Comparison, color = Comparison)) +
           geom_line(size = 1.2) +
           geom_point(size = 1.5,fill = "white") +
           scale_fill_viridis(discrete = T, option = "E") +
           scale_color_manual(values = c("black", "chartreuse3", "brown3")) + 
           scale_shape_manual(values = c(15,16,17)) +
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
