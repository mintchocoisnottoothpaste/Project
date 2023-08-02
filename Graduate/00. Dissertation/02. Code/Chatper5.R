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
scene2_nd_first  <- NORDPRED_func(Scenarios2_male,Scenarios2_female) # 고위 (fit:2000-2019)
scene3_nd_first  <- NORDPRED_func(Scenarios3_male,Scenarios3_female) # 저위 (fit:2000-2019)
scene4_nd_first  <- NORDPRED_func(Scenarios4_male,Scenarios4_female) # 빠른 고령화 (fit:2000-2019)
scene5_nd_first  <- NORDPRED_func(Scenarios5_male,Scenarios5_female) # 느린 고령화  (fit:2000-2019)

scene1_nd_first_M_inci_Rate<-data.frame(scene1_nd_first[[1]])/colSums(Scenarios1_male[,as.character(2000:2040)])*100000
scene1_nd_first_F_inci_Rate<-data.frame(scene1_nd_first[[2]])/colSums(Scenarios1_female[,as.character(2000:2040)])*100000
scene1_nd_first_M_mort_Rate<-data.frame(scene1_nd_first[[3]])/colSums(Scenarios1_male[,as.character(2000:2040)])*100000
scene1_nd_first_F_mort_Rate<-data.frame(scene1_nd_first[[4]])/colSums(Scenarios1_female[,as.character(2000:2040)])*100000

scene2_nd_first_M_inci_Rate<-data.frame(scene2_nd_first[[1]])/colSums(Scenarios2_male[,as.character(2000:2040)])*100000
scene2_nd_first_F_inci_Rate<-data.frame(scene2_nd_first[[2]])/colSums(Scenarios2_female[,as.character(2000:2040)])*100000
scene2_nd_first_M_mort_Rate<-data.frame(scene2_nd_first[[3]])/colSums(Scenarios2_male[,as.character(2000:2040)])*100000
scene2_nd_first_F_mort_Rate<-data.frame(scene2_nd_first[[4]])/colSums(Scenarios2_female[,as.character(2000:2040)])*100000

scene3_nd_first_M_inci_Rate<-data.frame(scene3_nd_first[[1]])/colSums(Scenarios3_male[,as.character(2000:2040)])*100000
scene3_nd_first_F_inci_Rate<-data.frame(scene3_nd_first[[2]])/colSums(Scenarios3_female[,as.character(2000:2040)])*100000
scene3_nd_first_M_mort_Rate<-data.frame(scene3_nd_first[[3]])/colSums(Scenarios3_male[,as.character(2000:2040)])*100000
scene3_nd_first_F_mort_Rate<-data.frame(scene3_nd_first[[4]])/colSums(Scenarios3_female[,as.character(2000:2040)])*100000

scene4_nd_first_M_inci_Rate<-data.frame(scene4_nd_first[[1]])/colSums(Scenarios4_male[,as.character(2000:2040)])*100000
scene4_nd_first_F_inci_Rate<-data.frame(scene4_nd_first[[2]])/colSums(Scenarios4_female[,as.character(2000:2040)])*100000
scene4_nd_first_M_mort_Rate<-data.frame(scene4_nd_first[[3]])/colSums(Scenarios4_male[,as.character(2000:2040)])*100000
scene4_nd_first_F_mort_Rate<-data.frame(scene4_nd_first[[4]])/colSums(Scenarios4_female[,as.character(2000:2040)])*100000

scene5_nd_first_M_inci_Rate<-data.frame(scene5_nd_first[[1]])/colSums(Scenarios5_male[,as.character(2000:2040)])*100000
scene5_nd_first_F_inci_Rate<-data.frame(scene5_nd_first[[2]])/colSums(Scenarios5_female[,as.character(2000:2040)])*100000
scene5_nd_first_M_mort_Rate<-data.frame(scene5_nd_first[[3]])/colSums(Scenarios5_male[,as.character(2000:2040)])*100000
scene5_nd_first_F_mort_Rate<-data.frame(scene5_nd_first[[4]])/colSums(Scenarios5_female[,as.character(2000:2040)])*100000

scene1_nd_first_M_inci_Rate<-cbind("Year"=c(2000:2040),scene1_nd_first_M_inci_Rate)
scene1_nd_first_F_inci_Rate<-cbind("Year"=c(2000:2040),scene1_nd_first_F_inci_Rate)
scene1_nd_first_M_mort_Rate<-cbind("Year"=c(2000:2040),scene1_nd_first_M_mort_Rate)
scene1_nd_first_F_mort_Rate<-cbind("Year"=c(2000:2040),scene1_nd_first_F_mort_Rate)

colnames(scene1_nd_first_M_inci_Rate) <-c("Year",title_nameM)
colnames(scene1_nd_first_M_mort_Rate) <-c("Year",title_nameM)
colnames(scene1_nd_first_F_inci_Rate) <-c("Year",title_nameF)
colnames(scene1_nd_first_F_mort_Rate) <-c("Year",title_nameF)

scene2_nd_first_M_inci_Rate<-cbind("Year"=c(2000:2040),scene2_nd_first_M_inci_Rate)
scene2_nd_first_F_inci_Rate<-cbind("Year"=c(2000:2040),scene2_nd_first_F_inci_Rate)
scene2_nd_first_M_mort_Rate<-cbind("Year"=c(2000:2040),scene2_nd_first_M_mort_Rate)
scene2_nd_first_F_mort_Rate<-cbind("Year"=c(2000:2040),scene2_nd_first_F_mort_Rate)

colnames(scene2_nd_first_M_inci_Rate) <-c("Year",title_nameM)
colnames(scene2_nd_first_M_mort_Rate) <-c("Year",title_nameM)
colnames(scene2_nd_first_F_inci_Rate) <-c("Year",title_nameF)
colnames(scene2_nd_first_F_mort_Rate) <-c("Year",title_nameF)

scene3_nd_first_M_inci_Rate<-cbind("Year"=c(2000:2040),scene3_nd_first_M_inci_Rate)
scene3_nd_first_F_inci_Rate<-cbind("Year"=c(2000:2040),scene3_nd_first_F_inci_Rate)
scene3_nd_first_M_mort_Rate<-cbind("Year"=c(2000:2040),scene3_nd_first_M_mort_Rate)
scene3_nd_first_F_mort_Rate<-cbind("Year"=c(2000:2040),scene3_nd_first_F_mort_Rate)

colnames(scene3_nd_first_M_inci_Rate) <-c("Year",title_nameM)
colnames(scene3_nd_first_M_mort_Rate) <-c("Year",title_nameM)
colnames(scene3_nd_first_F_inci_Rate) <-c("Year",title_nameF)
colnames(scene3_nd_first_F_mort_Rate) <-c("Year",title_nameF)

scene4_nd_first_M_inci_Rate<-cbind("Year"=c(2000:2040),scene4_nd_first_M_inci_Rate)
scene4_nd_first_F_inci_Rate<-cbind("Year"=c(2000:2040),scene4_nd_first_F_inci_Rate)
scene4_nd_first_M_mort_Rate<-cbind("Year"=c(2000:2040),scene4_nd_first_M_mort_Rate)
scene4_nd_first_F_mort_Rate<-cbind("Year"=c(2000:2040),scene4_nd_first_F_mort_Rate)

colnames(scene4_nd_first_M_inci_Rate) <-c("Year",title_nameM)
colnames(scene4_nd_first_M_mort_Rate) <-c("Year",title_nameM)
colnames(scene4_nd_first_F_inci_Rate) <-c("Year",title_nameF)
colnames(scene4_nd_first_F_mort_Rate) <-c("Year",title_nameF)

scene5_nd_first_M_inci_Rate<-cbind("Year"=c(2000:2040),scene5_nd_first_M_inci_Rate)
scene5_nd_first_F_inci_Rate<-cbind("Year"=c(2000:2040),scene5_nd_first_F_inci_Rate)
scene5_nd_first_M_mort_Rate<-cbind("Year"=c(2000:2040),scene5_nd_first_M_mort_Rate)
scene5_nd_first_F_mort_Rate<-cbind("Year"=c(2000:2040),scene5_nd_first_F_mort_Rate)

colnames(scene5_nd_first_M_inci_Rate) <-c("Year",title_nameM)
colnames(scene5_nd_first_M_mort_Rate) <-c("Year",title_nameM)
colnames(scene5_nd_first_F_inci_Rate) <-c("Year",title_nameF)
colnames(scene5_nd_first_F_mort_Rate) <-c("Year",title_nameF)

first_nd_Rate1<-rbind(
  cbind(melt(scene1_nd_first_M_inci_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Incidence"),"Comparison"=c("Median")),
  cbind(melt(scene1_nd_first_F_inci_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Incidence"),"Comparison"=c("Median")),
  cbind(melt(scene1_nd_first_M_mort_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Mortality"),"Comparison"=c("Median")),
  cbind(melt(scene1_nd_first_F_mort_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Mortality"),"Comparison"=c("Median")))
first_nd_Rate2<-rbind(
  cbind(melt(scene2_nd_first_M_inci_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Incidence"),"Comparison"=c("High")),
  cbind(melt(scene2_nd_first_F_inci_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Incidence"),"Comparison"=c("High")),
  cbind(melt(scene2_nd_first_M_mort_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Mortality"),"Comparison"=c("High")),
  cbind(melt(scene2_nd_first_F_mort_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Mortality"),"Comparison"=c("High")))

first_nd_Rate3<-rbind(
  cbind(melt(scene3_nd_first_M_inci_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Incidence"),"Comparison"=c("Low")),
  cbind(melt(scene3_nd_first_F_inci_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Incidence"),"Comparison"=c("Low")),
  cbind(melt(scene3_nd_first_M_mort_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Mortality"),"Comparison"=c("Low")),
  cbind(melt(scene3_nd_first_F_mort_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Mortality"),"Comparison"=c("Low")))

first_nd_Rate4<-rbind(
  cbind(melt(scene4_nd_first_M_inci_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Incidence"),"Comparison"=c("Fast Ageing")),
  cbind(melt(scene4_nd_first_F_inci_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Incidence"),"Comparison"=c("Fast Ageing")),
  cbind(melt(scene4_nd_first_M_mort_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Mortality"),"Comparison"=c("Fast Ageing")),
  cbind(melt(scene4_nd_first_F_mort_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Mortality"),"Comparison"=c("Fast Ageing")))

first_nd_Rate5<-rbind(
  cbind(melt(scene5_nd_first_M_inci_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Incidence"),"Comparison"=c("Slow Ageing")),
  cbind(melt(scene5_nd_first_F_inci_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Incidence"),"Comparison"=c("Slow Ageing")),
  cbind(melt(scene5_nd_first_M_mort_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Mortality"),"Comparison"=c("Slow Ageing")),
  cbind(melt(scene5_nd_first_F_mort_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Mortality"),"Comparison"=c("Slow Ageing")))


nd_comp_Rate1 <-first_nd_Rate1
colnames(nd_comp_Rate1) <-c("Year","Cancer","Rate","Sex","Type","Comparison")
nd_comp_Rate1$Sex <- factor(nd_comp_Rate1$Sex,levels=c("Male","Female"))

nd_comp_Rate1<-nd_comp_Rate1 %>% mutate(
  Comparison = case_when(
    Comparison = 0019 & Type=="Incidence" & Year <=2019 ~ "Observation",
    Comparison = 0019 & Type=="Mortality" & Year <=2019 ~ "Observation",
    TRUE ~ Comparison
  ))

nd_comp_Rate2 <-first_nd_Rate2
colnames(nd_comp_Rate2) <-c("Year","Cancer","Rate","Sex","Type","Comparison")
nd_comp_Rate2$Sex <- factor(nd_comp_Rate2$Sex,levels=c("Male","Female"))

nd_comp_Rate2<-nd_comp_Rate2 %>% mutate(
  Comparison = case_when(
    Comparison = 0019 & Type=="Incidence" & Year <=2019 ~ "Observation",
    Comparison = 0019 & Type=="Mortality" & Year <=2019 ~ "Observation",
    TRUE ~ Comparison
  ))

nd_comp_Rate3 <-first_nd_Rate3
colnames(nd_comp_Rate3) <-c("Year","Cancer","Rate","Sex","Type","Comparison")
nd_comp_Rate3$Sex <- factor(nd_comp_Rate3$Sex,levels=c("Male","Female"))

nd_comp_Rate3<-nd_comp_Rate3 %>% mutate(
  Comparison = case_when(
    Comparison = 0019 & Type=="Incidence" & Year <=2019 ~ "Observation",
    Comparison = 0019 & Type=="Mortality" & Year <=2019 ~ "Observation",
    TRUE ~ Comparison
  ))

nd_comp_Rate4 <-first_nd_Rate4
colnames(nd_comp_Rate4) <-c("Year","Cancer","Rate","Sex","Type","Comparison")
nd_comp_Rate4$Sex <- factor(nd_comp_Rate4$Sex,levels=c("Male","Female"))

nd_comp_Rate4<-nd_comp_Rate4 %>% mutate(
  Comparison = case_when(
    Comparison = 0019 & Type=="Incidence" & Year <=2019 ~ "Observation",
    Comparison = 0019 & Type=="Mortality" & Year <=2019 ~ "Observation",
    TRUE ~ Comparison
  ))

nd_comp_Rate5 <-first_nd_Rate5
colnames(nd_comp_Rate5) <-c("Year","Cancer","Rate","Sex","Type","Comparison")
nd_comp_Rate5$Sex <- factor(nd_comp_Rate5$Sex,levels=c("Male","Female"))

nd_comp_Rate5<-nd_comp_Rate5 %>% mutate(
  Comparison = case_when(
    Comparison = 0019 & Type=="Incidence" & Year <=2019 ~ "Observation",
    Comparison = 0019 & Type=="Mortality" & Year <=2019 ~ "Observation",
    TRUE ~ Comparison
  ))


model_obs<-subset(nd_comp_Rate1,Comparison=="Observation")

model_obs_pred1<-subset(model_obs,Year==2019);model_obs_pred1$Comparison <-c("Median")
model_obs_pred2<-subset(model_obs,Year==2019);model_obs_pred2$Comparison <-c("High")
model_obs_pred3<-subset(model_obs,Year==2019);model_obs_pred3$Comparison <-c("Low")
model_obs_pred4<-subset(model_obs,Year==2019);model_obs_pred4$Comparison <-c("Fast Ageing")
model_obs_pred5<-subset(model_obs,Year==2019);model_obs_pred5$Comparison <-c("Slow Ageing")

model_np_pred1<-subset(nd_comp_Rate1,Comparison=="Median");model_np_pred1 <-rbind(model_obs_pred1,model_np_pred1)
model_np_pred2<-subset(nd_comp_Rate2,Comparison=="High");model_np_pred2 <-rbind(model_obs_pred2,model_np_pred2)
model_np_pred3<-subset(nd_comp_Rate3,Comparison=="Low");model_np_pred3 <-rbind(model_obs_pred3,model_np_pred3)
model_np_pred4<-subset(nd_comp_Rate4,Comparison=="Fast Ageing");model_np_pred4 <-rbind(model_obs_pred4,model_np_pred4)
model_np_pred5<-subset(nd_comp_Rate5,Comparison=="Slow Ageing");model_np_pred5 <-rbind(model_obs_pred5,model_np_pred5)
mdata<-rbind(model_obs,model_np_pred1,model_np_pred2,model_np_pred3,model_np_pred4,model_np_pred5)
mdata$Comparison <-factor(mdata$Comparison)
mdata$Comparison <- factor(mdata$Comparison,levels=c("Observation","Median","High", "Low", "Fast Ageing","Slow Ageing"))


## Scenaro %

subset(mdata, Year %in% c(2019,2030,2040) & Sex=="Male")
subset(mdata, Year %in% c(2019,2030,2040) & Sex=="Female")

ggplot(subset(mdata, Sex=="Male" &  Type =="Mortality" & Cancer==title_nameM[11]), aes(x = Year, y = Rate, color = Comparison, linetype = Comparison)) +
  geom_line(size = 2.0) +
  #geom_point(size = 1.5,fill = "white") +
  scale_fill_viridis(discrete = T, option = "E") +
  theme_stata() + scale_color_stata() +
  #scale_color_manual(values = c("black", "chartreuse3", "brown3",'cadetblue3',"cornsilk3","blueviolet")) + 
  scale_shape_manual(c("solid", "longdash", "dashed", "dotted")) +
  scale_linetype() + 
  #facet_wrap(~ Type, strip.position = "top", nrow = 1) +
  
  labs(x = "Year", y = "Crude Rate (per 100,000)",title=title_nameM[11]) +
  theme(legend.position = "none",
        text=element_text(size=30),
        axis.text.x=element_text(angle=90, hjust=1),
        plot.title = element_text(size = 30, face = "bold")) 


dev.off()
result <-list()
for ( i in 1:12){
  result[[i]] <- ggplot(subset(mdata, Sex=="Male" &  Cancer==title_nameM[i]), aes(x = Year, y = Rate, color = Comparison, linetype = Comparison)) +
    geom_line(size = 2.0) +
    #geom_point(size = 1.5,fill = "white") +
    scale_fill_viridis(discrete = T, option = "E") +
    theme_stata() + scale_color_stata() +
    #scale_color_manual(values = c("black", "chartreuse3", "brown3",'cadetblue3',"cornsilk3","blueviolet")) + 
    scale_shape_manual(c("solid", "longdash", "dashed", "dotted")) +
    scale_linetype() + 
    facet_wrap(~ Type, strip.position = "top", nrow = 1) +
 
    labs(x = "Year", y = "Crude Rate (per 100,000)",title=title_nameM[i]) +
    theme(legend.position = "none",
          text=element_text(size=30),
          axis.text.x=element_text(angle=90, hjust=1),
          plot.title = element_text(size = 30, face = "bold")) 
}
result

result <-list()
for ( i in 1:9){
  result[[i]] <- ggplot(subset(mdata, Sex=="Female" &  Cancer==title_nameF[i]), aes(x = Year, y = Rate, color = Comparison, linetype = Comparison)) +
    geom_line(size = 2.0) +
    #geom_point(size = 1.5,fill = "white") +
    scale_fill_viridis(discrete = T, option = "E") +
    theme_stata() + scale_color_stata() +
    #scale_color_manual(values = c("black", "chartreuse3", "brown3",'cadetblue3',"cornsilk3","blueviolet")) + 
    scale_shape_manual(c("solid", "longdash", "dashed", "dotted")) +
    scale_linetype() + 
    facet_wrap(~ Type, strip.position = "top", nrow = 1) +
    
    labs(x = "Year", y = "Crude Rate (per 100,000)",title=title_nameF[i]) +
    theme(legend.position = "none",
          text=element_text(size=30),
          axis.text.x=element_text(angle=90, hjust=1),
          plot.title = element_text(size = 30, face = "bold")) 
}
result


#######################################################################################################################################

NORDPRED_func_modif <- function (Scenarios1_male,Scenarios1_female){
  
  NORDPRED_INCI_M <-list()
  NORDPRED_INCI_F <-list()
  
  for (i in 1: length(inci_cname_M)){
    male_case <-subset(get(inci_cname_M[i]),Sex=="남자")[,-c(1:3,24)]
    male_pop  <- Scenarios1_male[, c(as.character(2000:2040))]
    NORDPRED_INCI_M[[i]] <-canproj(male_case,male_pop,startp=2020,startestage=7, methods="nordpred")$agsproj*(male_pop)/100000
  }
  for (i in 1: length(inci_cname_F)){
    female_case <-subset(get(inci_cname_F[i]),Sex=="여자")[,-c(1:3,24)]
    female_pop  <- Scenarios1_female[, c(as.character(2000:2040))]
    NORDPRED_INCI_F[[i]] <-canproj(female_case,female_pop,startp=2020,startestage=7, methods="nordpred")$agsproj*(female_pop)/100000
  }
  
  NORDPRED_MORT_M <-list()
  NORDPRED_MORT_F <-list()
  
  for ( i in 1: length(mort_cname_M)){
    male_case <- subset(get(mort_cname_M[i]),Sex=="남자")[,-c(1:3,24,25)]
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
    female_case <- subset(get(mort_cname_F[i]),Sex=="여자")[,-c(1:3,24,25)]
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
scene2_nd_byage<-NORDPRED_func_modif(Scenarios2_male,Scenarios2_female)
scene3_nd_byage<-NORDPRED_func_modif(Scenarios3_male,Scenarios3_female)
scene4_nd_byage<-NORDPRED_func_modif(Scenarios4_male,Scenarios4_female)
scene5_nd_byage<-NORDPRED_func_modif(Scenarios5_male,Scenarios5_female)

scene1_nd_byage_male_inc <- scene1_nd_byage[[1]];scene1_nd_byage_male_mort <- scene1_nd_byage[[3]];scene1_nd_byage_female_inc <- scene1_nd_byage[[2]];scene1_nd_byage_female_mort <- scene1_nd_byage[[4]]
scene2_nd_byage_male_inc <- scene2_nd_byage[[1]];scene2_nd_byage_male_mort <- scene2_nd_byage[[3]];scene2_nd_byage_female_inc <- scene2_nd_byage[[2]];scene2_nd_byage_female_mort <- scene2_nd_byage[[4]]
scene3_nd_byage_male_inc <- scene3_nd_byage[[1]];scene3_nd_byage_male_mort <- scene3_nd_byage[[3]];scene3_nd_byage_female_inc <- scene3_nd_byage[[2]];scene3_nd_byage_female_mort <- scene3_nd_byage[[4]]
scene4_nd_byage_male_inc <- scene4_nd_byage[[1]];scene4_nd_byage_male_mort <- scene4_nd_byage[[3]];scene4_nd_byage_female_inc <- scene4_nd_byage[[2]];scene4_nd_byage_female_mort <- scene4_nd_byage[[4]]
scene5_nd_byage_male_inc <- scene5_nd_byage[[1]];scene5_nd_byage_male_mort <- scene5_nd_byage[[3]];scene5_nd_byage_female_inc <- scene5_nd_byage[[2]];scene5_nd_byage_female_mort <- scene5_nd_byage[[4]]



################################################################################

title_nameM_inc<-c("Esophagus (C15)","Stomach (C16)","Colorectal (C18-C20)","Liver (C22)","Gallbladder (C23-C24)","Pancreas (C25)","Lung (C33-C34)","Prostate (C61)","Kidney (C64)","Bladder (C67)","Non-Hodgkin lymphoma (C82-C86,C96)","Leukemia (C91-C95)")
title_nameM_mort<-c("Esophagus (C15)","Stomach (C16)","Colorectal & Anus (C18-C21)","Liver (C22)","Gallbladder (C23-C24)","Pancreas (C25)","Lung (C33-C34)","Prostate (C61)","Kidney (C64)","Bladder (C67)","Non-Hodgkin lymphoma (C82-C86)","Leukemia (C91-C95)")

title_nameF_inc<-c("Stomach (C16)","Colorectal (C18-C20)","Liver (C22)","Gallbladder (C23-C24)", "Pancreas (C25)","Lung (C33-C34)","Cervix uteri (C53)","Non-Hodgkin lymphoma (C82-C86,C96)","Leukemia (C91-C95)")
title_nameF_mort<-c("Stomach (C16)","Colorectal & Anus (C18-C21)","Liver (C22)","Gallbladder (C23-C24)", "Pancreas (C25)","Lung (C33-C34)","Cervix uteri (C53)","Non-Hodgkin lymphoma (C82-C86)","Leukemia (C91-C95)")
result<-list()
ageing_effect_check_modif <- function(Sex,Method,Contents,Extract_M){
  if (Sex == 1) {
    changes_history <-list()
    for (j in 1:12){
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene1_nd_byage_male_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios1_male[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
      }
      
      years = c(2000:2040)
      change1 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change1$Group <- factor(change1$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change1$Scenarios <-c("Median")
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene2_nd_byage_male_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios2_male[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
      }
      
      
      years = c(2000:2040)
      change2 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change),
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change2$Group <- factor(change2$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change2$Scenarios <-c("High")
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene3_nd_byage_male_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios3_male[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
      }
      
      
      years = c(2000:2040)
      change3 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change3$Group <- factor(change3$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change3$Scenarios <-c("Low")
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene4_nd_byage_male_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios4_male[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
      }
      
      years = c(2000:2040)
      change4 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change4$Group <- factor(change4$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change4$Scenarios <-c("Fast Ageing")
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene5_nd_byage_male_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios5_male[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
      }
      
      
      years = c(2000:2040)
      change5 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change5$Group <- factor(change5$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change5$Scenarios <-c("Slow Ageing")
      
      
      change_total_inc <- rbind(change1,change2,change3,change4,change5)
      change_total_inc$Scenarios <- factor(change_total_inc$Scenarios,levels=c("Median","High","Low","Fast Ageing","Slow Ageing"))
      change_total_inc$Type <- c("Incidence")
      ##############################################################################################################################
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene1_nd_byage_male_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios1_male[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
      }
      
      years = c(2000:2040)
      change1 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change1$Group <- factor(change1$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change1$Scenarios <-c("Median")
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene2_nd_byage_male_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios2_male[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
        
      }
      
      years = c(2000:2040)
      change2 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change2$Group <- factor(change2$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change2$Scenarios <-c("High")
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene3_nd_byage_male_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios3_male[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
      }
      
      years = c(2000:2040)
      change3 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change3$Group <- factor(change3$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change3$Scenarios <-c("Low")
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene4_nd_byage_male_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios4_male[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
        
      }
      
      years = c(2000:2040)
      change4 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change),  
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change4$Group <- factor(change4$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change4$Scenarios <-c("Fast Ageing")
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene5_nd_byage_male_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios5_male[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
        
      }
      
      years = c(2000:2040)
      change5 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change5$Group <- factor(change5$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change5$Scenarios <-c("Slow Ageing")
      
      
      change_total_mort <- rbind(change1,change2,change3,change4,change5)
      change_total_mort$Scenarios <- factor(change_total_mort$Scenarios,levels=c("Median","High","Low","Fast Ageing","Slow Ageing"))
      change_total_mort$Type <- c("Mortality")
      ##############################################################################################################################
      
      change_total <- rbind(change_total_inc,change_total_mort)
      
      ##############################################################################################################################
      changes_history[[j]] <- change_total
      
      if (Method=="Incidence" & Contents =="Main") {
        result[[j]] <- ggplot(data = subset(change_total,Type=='Incidence' & Scenarios %in% c("Median","Fast Ageing","Slow Ageing")), aes(x = Year, y = Change,group = Group,colour = Group,linetype = Group)) + geom_line(size=2.0) + 
          scale_x_continuous(breaks = seq(2000, 2040, 10)) + geom_hline(yintercept = 0, color = 'black', linetype = 2,size=1.6) +
          facet_wrap(~ Scenarios, strip.position = "top", nrow = 1) +
          theme_stata() + scale_color_stata() + 
          theme(legend.position = "none",
                text=element_text(size=25),
                axis.text.x=element_text(angle=90, hjust=1),
                plot.title = element_text(size = 30, face = "bold")) + 
          labs(title=title_nameM_inc[j], x = 'Calender Year',y = "Difference Attributable Incidence Case")
      } else if (Method=="Incidence" & Contents =="Sup"){
        result[[j]] <- ggplot(data = subset(change_total,Type=='Incidence'), aes(x = Year, y = Change,group = Group,colour = Group,linetype = Group)) + geom_line(size=2.0) + 
          scale_x_continuous(breaks = seq(2000, 2040, 10)) + geom_hline(yintercept = 0, color = 'black', linetype = 2,size=1.6) +
          facet_wrap(~ Scenarios, strip.position = "top", nrow = 1) +
          theme_stata() + scale_color_stata() + 
          theme(legend.position = "none",
                text=element_text(size=25),
                axis.text.x=element_text(angle=90, hjust=1),
                plot.title = element_text(size = 30, face = "bold")) + 
          labs(title=title_nameM_inc[j], x = 'Calender Year',y = "Difference Attributable Incidence Case")
      } else if (Method=="Mortality" & Contents =="Main"){
        result[[j]] <- ggplot(data = subset(change_total,Type=='Mortality' & Scenarios %in% c("Median","Fast Ageing","Slow Ageing")), aes(x = Year, y = Change,group = Group,colour = Group,linetype = Group)) + geom_line(size=2.0) + 
          scale_x_continuous(breaks = seq(2000, 2040, 10)) + geom_hline(yintercept = 0, color = 'black', linetype = 2,size=1.6) +
          facet_wrap(~ Scenarios, strip.position = "top", nrow = 1) +
          theme_stata() + scale_color_stata() + 
          theme(legend.position = "none",
                text=element_text(size=25),
                axis.text.x=element_text(angle=90, hjust=1),
                plot.title = element_text(size = 30, face = "bold")) + 
          labs(title=title_nameM_mort[j], x = 'Calender Year',y = "Difference Attributable Mortality Case")
      } else if (Method=="Mortality" & Contents =="Sup"){
        result[[j]] <- ggplot(data = subset(change_total,Type=='Mortality'), aes(x = Year, y = Change,group = Group,colour = Group,linetype = Group)) + geom_line(size=2.0) + 
          scale_x_continuous(breaks = seq(2000, 2040, 10)) + geom_hline(yintercept = 0, color = 'black', linetype = 2,size=1.6) +
          facet_wrap(~ Scenarios, strip.position = "top", nrow = 1) +
          theme_stata() + scale_color_stata() + 
          theme(legend.position = "none",
                text=element_text(size=25),
                axis.text.x=element_text(angle=90, hjust=1),
                plot.title = element_text(size = 30, face = "bold")) + 
          labs(title=title_nameM_mort[j], x = 'Calender Year',y = "Difference Attributable Mortality Case")
      }
      

    } 
  }
  else if (Sex == 2) {
    changes_history <-list()
    for (j in 1:9){
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene1_nd_byage_female_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios1_female[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
      }
      
      years = c(2000:2040)
      change1 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change1$Group <- factor(change1$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change1$Scenarios <-c("Median")
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene2_nd_byage_female_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios2_female[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
      }
      
      
      years = c(2000:2040)
      change2 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change),
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change2$Group <- factor(change2$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change2$Scenarios <-c("High")
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene3_nd_byage_female_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios3_female[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
      }
      
      
      years = c(2000:2040)
      change3 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change3$Group <- factor(change3$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change3$Scenarios <-c("Low")
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene4_nd_byage_female_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios4_female[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
      }
      
      years = c(2000:2040)
      change4 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change4$Group <- factor(change4$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change4$Scenarios <-c("Fast Ageing")
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene5_nd_byage_female_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios5_female[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
      }
      
      
      years = c(2000:2040)
      change5 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change5$Group <- factor(change5$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change5$Scenarios <-c("Slow Ageing")
      
      
      change_total_inc <- rbind(change1,change2,change3,change4,change5)
      change_total_inc$Scenarios <- factor(change_total_inc$Scenarios,levels=c("Median","High","Low","Fast Ageing","Slow Ageing"))
      change_total_inc$Type <- c("Incidence")
      ##############################################################################################################################
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene1_nd_byage_female_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios1_female[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
      }
      
      years = c(2000:2040)
      change1 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change1$Group <- factor(change1$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change1$Scenarios <-c("Median")
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene2_nd_byage_female_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios2_female[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
        
      }
      
      years = c(2000:2040)
      change2 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change2$Group <- factor(change2$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change2$Scenarios <-c("High")
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene3_nd_byage_female_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios3_female[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
      }
      
      years = c(2000:2040)
      change3 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change3$Group <- factor(change3$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change3$Scenarios <-c("Low")
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene4_nd_byage_female_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios4_female[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
        
      }
      
      years = c(2000:2040)
      change4 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change),  
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change4$Group <- factor(change4$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change4$Scenarios <-c("Fast Ageing")
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene5_nd_byage_female_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios5_female[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
        
      }
      
      years = c(2000:2040)
      change5 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change5$Group <- factor(change5$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change5$Scenarios <-c("Slow Ageing")
      
      
      change_total_mort <- rbind(change1,change2,change3,change4,change5)
      change_total_mort$Scenarios <- factor(change_total_mort$Scenarios,levels=c("Median","High","Low","Fast Ageing","Slow Ageing"))
      change_total_mort$Type <- c("Mortality")
      ##############################################################################################################################
      
      change_total <- rbind(change_total_inc,change_total_mort)
      
      ##############################################################################################################################
      changes_history[[j]] <- change_total
      
      if (Method=="Incidence" & Contents =="Main") {
        result[[j]] <- ggplot(data = subset(change_total,Type=='Incidence' & Scenarios %in% c("Median","Fast Ageing","Slow Ageing")), aes(x = Year, y = Change,group = Group,colour = Group,linetype = Group)) + geom_line(size=2.0) + 
          scale_x_continuous(breaks = seq(2000, 2040, 10)) + geom_hline(yintercept = 0, color = 'black', linetype = 2,size=1.6) +
          facet_wrap(~ Scenarios, strip.position = "top", nrow = 1) +
          theme_stata() + scale_color_stata() + 
          theme(legend.position = "none",
                text=element_text(size=25),
                axis.text.x=element_text(angle=90, hjust=1),
                plot.title = element_text(size = 30, face = "bold")) + 
          labs(title=title_nameF_inc[j], x = 'Calender Year',y = "Difference Attributable Incidence Case")
      } else if (Method=="Incidence" & Contents =="Sup"){
        result[[j]] <- ggplot(data = subset(change_total,Type=='Incidence'), aes(x = Year, y = Change,group = Group,colour = Group,linetype = Group)) + geom_line(size=2.0) + 
          scale_x_continuous(breaks = seq(2000, 2040, 10)) + geom_hline(yintercept = 0, color = 'black', linetype = 2,size=1.6) +
          facet_wrap(~ Scenarios, strip.position = "top", nrow = 1) +
          theme_stata() + scale_color_stata() + 
          theme(legend.position = "none",
                text=element_text(size=25),
                axis.text.x=element_text(angle=90, hjust=1),
                plot.title = element_text(size = 30, face = "bold")) + 
          labs(title=title_nameF_inc[j], x = 'Calender Year',y = "Difference Attributable Incidence Case")
      } else if (Method=="Mortality" & Contents =="Main"){
        result[[j]] <- ggplot(data = subset(change_total,Type=='Mortality' & Scenarios %in% c("Median","Fast Ageing","Slow Ageing")), aes(x = Year, y = Change,group = Group,colour = Group,linetype = Group)) + geom_line(size=2.0) + 
          scale_x_continuous(breaks = seq(2000, 2040, 10)) + geom_hline(yintercept = 0, color = 'black', linetype = 2,size=1.6) +
          facet_wrap(~ Scenarios, strip.position = "top", nrow = 1) +
          theme_stata() + scale_color_stata() + 
          theme(legend.position = "none",
                text=element_text(size=25),
                axis.text.x=element_text(angle=90, hjust=1),
                plot.title = element_text(size = 30, face = "bold")) + 
          labs(title=title_nameF_mort[j], x = 'Calender Year',y = "Difference Attributable Mortality Case")
      } else if (Method=="Mortality" & Contents =="Sup"){
        result[[j]] <- ggplot(data = subset(change_total,Type=='Mortality'), aes(x = Year, y = Change,group = Group,colour = Group,linetype = Group)) + geom_line(size=2.0) + 
          scale_x_continuous(breaks = seq(2000, 2040, 10)) + geom_hline(yintercept = 0, color = 'black', linetype = 2,size=1.6) +
          facet_wrap(~ Scenarios, strip.position = "top", nrow = 1) +
          theme_stata() + scale_color_stata() + 
          theme(legend.position = "none",
                text=element_text(size=25),
                axis.text.x=element_text(angle=90, hjust=1),
                plot.title = element_text(size = 30, face = "bold")) + 
          labs(title=title_nameF_mort[j], x = 'Calender Year',y = "Difference Attributable Mortality Case")
      }
      
    } 
  }
  if (Extract_M=="Table") {
    return(changes_history)
  } else if (Extract_M=="Plot"){
    return(result)
  }
}

# Male, Incidence
ageing_effect_check_modif(1,"Incidence","Main","Plot")
ageing_effect_check_modif(1,"Incidence","Sup","Plot")
# Male, Death
ageing_effect_check_modif(1,"Mortality","Main","Plot")
ageing_effect_check_modif(1,"Mortality","Sup","Plot")
# Male, Incidence
ageing_effect_check_modif(2,"Incidence","Main","Plot")
ageing_effect_check_modif(2,"Incidence","Sup","Plot")
# Male, Death
ageing_effect_check_modif(2,"Mortality","Main","Plot")
ageing_effect_check_modif(2,"Mortality","Sup","Plot")
########

################################################################################


title_nameM_inc<-c("Esophagus (C15)","Stomach (C16)","Colorectal (C18-C20)","Liver (C22)","Gallbladder (C23-C24)","Pancreas (C25)","Lung (C33-C34)","Prostate (C61)","Kidney (C64)","Bladder (C67)","Non-Hodgkin lymphoma (C82-C86,C96)","Leukemia (C91-C95)")
title_nameM_mort<-c("Esophagus (C15)","Stomach (C16)","Colorectal & Anus (C18-C21)","Liver (C22)","Gallbladder (C23-C24)","Pancreas (C25)","Lung (C33-C34)","Prostate (C61)","Kidney (C64)","Bladder (C67)","Non-Hodgkin lymphoma (C82-C86)","Leukemia (C91-C95)")

title_nameF_inc<-c("Stomach (C16)","Colorectal (C18-C20)","Liver (C22)","Gallbladder (C23-C24)", "Pancreas (C25)","Lung (C33-C34)","Cervix uteri (C53)","Non-Hodgkin lymphoma (C82-C86,C96)","Leukemia (C91-C95)")
title_nameF_mort<-c("Stomach (C16)","Colorectal & Anus (C18-C21)","Liver (C22)","Gallbladder (C23-C24)", "Pancreas (C25)","Lung (C33-C34)","Cervix uteri (C53)","Non-Hodgkin lymphoma (C82-C86)","Leukemia (C91-C95)")

ageing_effect_check_modif <- function(Sex){
  if (Sex == 1) {
    changes_history <-list()
    for (j in 1:12){
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene1_nd_byage_male_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios1_male[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
      }
      
      years = c(2000:2040)
      change1 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Age-standarized Rate risk", "Net Change"), each = 41))
      change1$Group <- factor(change1$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Age-standarized Rate risk"))
      change1$Scenarios <-c("Median")
      change1$Change <- change1$Change + as.numeric(colSums(data.frame(scene1_nd_byage_male_inc[j])))[1]
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene4_nd_byage_male_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios4_male[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
      }
      
      years = c(2000:2040)
      change4 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Age-standarized Rate risk", "Net Change"), each = 41))
      change4$Group <- factor(change4$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Age-standarized Rate risk"))
      change4$Scenarios <-c("Fast Ageing")
      change4$Change <- change4$Change + as.numeric(colSums(data.frame(scene4_nd_byage_male_inc[j])))[1]
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene5_nd_byage_male_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios5_male[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
      }
      
      
      years = c(2000:2040)
      change5 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Age-standarized Rate risk", "Net Change"), each = 41))
      change5$Group <- factor(change5$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Age-standarized Rate risk"))
      change5$Scenarios <-c("Slow Ageing")
      change5$Change <- change5$Change + as.numeric(colSums(data.frame(scene5_nd_byage_male_inc[j])))[1]
      
      change_total_inc <- rbind(change1,change4,change5)
      change_total_inc$Scenarios <- factor(change_total_inc$Scenarios,levels=c("Median","Fast Ageing","Slow Ageing"))
      change_total_inc$Type <- c("Incidence")
      ##############################################################################################################################
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene1_nd_byage_male_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios1_male[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
      }
      
      years = c(2000:2040)
      change1 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Age-standarized Rate risk", "Net Change"), each = 41))
      change1$Group <- factor(change1$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Age-standarized Rate risk"))
      change1$Scenarios <-c("Median")
      change1$Change <- change1$Change + as.numeric(colSums(data.frame(scene1_nd_byage_male_mort[j])))[6]
      
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene4_nd_byage_male_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios4_male[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
        
      }
      
      years = c(2000:2040)
      change4 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change),  
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Age-standarized Rate risk", "Net Change"), each = 41))
      change4$Group <- factor(change4$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Age-standarized Rate risk"))
      change4$Scenarios <-c("Fast Ageing")
      change4$Change <- change4$Change + as.numeric(colSums(data.frame(scene4_nd_byage_male_mort[j])))[6]
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene5_nd_byage_male_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios5_male[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
        
      }
      
      years = c(2000:2040)
      change5 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Age-standarized Rate risk", "Net Change"), each = 41))
      change5$Group <- factor(change5$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Age-standarized Rate risk"))
      change5$Scenarios <-c("Slow Ageing")
      change5$Change <- change5$Change + as.numeric(colSums(data.frame(scene5_nd_byage_male_mort[j])))[6]
      
      change_total_mort <- rbind(change1,change4,change5)
      change_total_mort$Scenarios <- factor(change_total_mort$Scenarios,levels=c("Median","Fast Ageing","Slow Ageing"))
      change_total_mort$Type <- c("Mortality")
      
      
      ##############################################################################################################################
      
      change_total <- rbind(change_total_inc,change_total_mort)
      
      ##############################################################################################################################
      changes_history[[j]] <- change_total
      
    } 
  }
  else if (Sex == 2) {
    changes_history <-list()
    for (j in 1:9){
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene1_nd_byage_female_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios1_female[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
      }
      
      years = c(2000:2040)
      change1 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Age-standarized Rate risk", "Net Change"), each = 41))
      change1$Group <- factor(change1$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Age-standarized Rate risk"))
      change1$Scenarios <-c("Median")
      change1$Change <- change1$Change + as.numeric(colSums(data.frame(scene1_nd_byage_female_inc[j])))[1]
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene4_nd_byage_female_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios4_female[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
      }
      
      years = c(2000:2040)
      change4 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Age-standarized Rate risk", "Net Change"), each = 41))
      change4$Group <- factor(change4$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Age-standarized Rate risk"))
      change4$Scenarios <-c("Fast Ageing")
      change4$Change <- change4$Change + as.numeric(colSums(data.frame(scene4_nd_byage_female_inc[j])))[1]
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene5_nd_byage_female_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios5_female[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
      }
      
      
      years = c(2000:2040)
      change5 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Age-standarized Rate risk", "Net Change"), each = 41))
      change5$Group <- factor(change5$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Age-standarized Rate risk"))
      change5$Scenarios <-c("Slow Ageing")
      change5$Change <- change5$Change + as.numeric(colSums(data.frame(scene5_nd_byage_female_inc[j])))[1]
      
      change_total_inc <- rbind(change1,change4,change5)
      change_total_inc$Scenarios <- factor(change_total_inc$Scenarios,levels=c("Median","Fast Ageing","Slow Ageing"))
      change_total_inc$Type <- c("Incidence")
      ##############################################################################################################################
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene1_nd_byage_female_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios1_female[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
      }
      
      years = c(2000:2040)
      change1 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Age-standarized Rate risk", "Net Change"), each = 41))
      change1$Group <- factor(change1$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Age-standarized Rate risk"))
      change1$Scenarios <-c("Median")
      change1$Change <- change1$Change + as.numeric(colSums(data.frame(scene1_nd_byage_female_mort[j])))[6]
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene4_nd_byage_female_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios4_female[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
        
      }
      
      years = c(2000:2040)
      change4 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change),  
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Age-standarized Rate risk", "Net Change"), each = 41))
      change4$Group <- factor(change4$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Age-standarized Rate risk"))
      change4$Scenarios <-c("Fast Ageing")
      
      
      change4$Change <- change4$Change + as.numeric(colSums(data.frame(scene4_nd_byage_female_mort[j])))[6]
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene5_nd_byage_female_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios5_female[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
        
      }
      
      years = c(2000:2040)
      change5 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Age-standarized Rate risk", "Net Change"), each = 41))
      change5$Group <- factor(change5$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Age-standarized Rate risk"))
      change5$Scenarios <-c("Slow Ageing")
      change5$Change <- change5$Change + as.numeric(colSums(data.frame(scene5_nd_byage_female_mort[j])))[6]
      
      change_total_mort <- rbind(change1,change4,change5)
      change_total_mort$Scenarios <- factor(change_total_mort$Scenarios,levels=c("Median","Fast Ageing","Slow Ageing"))
      change_total_mort$Type <- c("Mortality")
      ##############################################################################################################################
      
      change_total <- rbind(change_total_inc,change_total_mort)
      
      ##############################################################################################################################
      changes_history[[j]] <- change_total
    } 
  }
  return(changes_history)
}

tbl1 <- ageing_effect_check_modif(1)
tbl2 <- ageing_effect_check_modif(2)


sample1<-list()
sample2<-list()
sample3<-list()
sample4<-list()

for ( i in 1:12){
  sample1[[i]] <- subset(tbl1[[i]],Year %in% c(2000,2019,2030,2040) & Group %in% c("Population Aging risk") & Type == c("Incidence"))
  sample2[[i]] <- subset(tbl1[[i]],Year %in% c(2005,2019,2030,2040) & Group %in% c("Population Aging risk") & Type == c("Mortality") & Scenarios ==c("Slow Ageing"))
} 
for ( i in 1:9){
  sample3[[i]] <- subset(tbl2[[i]],Year %in% c(2000,2019,2030,2040) & Group %in% c("Population Aging risk") & Type == c("Incidence"))
  sample4[[i]] <- subset(tbl2[[i]],Year %in% c(2005,2019,2030,2040) & Group %in% c("Population Aging risk") & Type == c("Mortality"))
} 


sample1
sample2
sample3
sample4


sample1<-list()
sample2<-list()
sample3<-list()
sample4<-list()

for ( i in 1:12){
  sample1[[i]] <- subset(tbl1[[i]],Year %in% c(2000,2019,2030,2040) & Group %in% c("Net Change") & Type == c("Incidence"))[,c(1,2)]
  sample2[[i]] <- subset(tbl1[[i]],Year %in% c(2005,2019,2030,2040) & Group %in% c("Net Change") & Type == c("Mortality"))[,c(1,2)]
} 
for ( i in 1:9){
  sample3[[i]] <- subset(tbl2[[i]],Year %in% c(2000,2019,2030,2040) & Group %in% c("Net Change") & Type == c("Incidence"))[,c(1,2)]
  sample4[[i]] <- subset(tbl2[[i]],Year %in% c(2005,2019,2030,2040) & Group %in% c("Net Change") & Type == c("Mortality"))[,c(1,2)]
} 

sample1
sample2
sample3
sample4
