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
#font_import()

# TODO: data import
# current_population<-read.xlsx("/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/rawdata/registration.xlsx")
# projection_population_ver1<-read.xlsx("/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/rawdata/projection.xlsx")
# projection_population_ver2<-read.xlsx("/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/rawdata/Scenario_projection.xlsx")
# 
# cancer_incidence_data<-read.xlsx("/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/rawdata/incidence.xlsx")
# cancer_mortality_data<-read.xlsx("/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/rawdata/mortality.xlsx")
# 
# agegroups <- c("00-04","05-09","10-14","15-19","20-24","25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84","85+")
# 
# # TODO: data 정제
# ## 2000-2019 인구
# current_population<-current_population[-c(1:6),-c(2)]
# colnames(current_population) <- gsub("\\.", "", colnames(current_population))
# 
# c1 <- current_population[c(1:8),c(1:21)]
# c1[,c(3:ncol(c1))] <-sapply(c1[,c(3:ncol(c1))],as.numeric)
# 
# c2 <- current_population[c(9:nrow(current_population)),c(1:25)]
# c2[,c(3:ncol(c2))] <-sapply(c2[,c(3:ncol(c2))],as.numeric)
# c2<-cbind(c2[,c(1:20)],'85세이상'=rowSums(c2[,c(21:25)]))
# 
# current_population <-rbind(c1,c2)
# current_population<-melt(current_population,id.vars =c('시점','성별'))
# current_population<-reshape(current_population, idvar=c("성별", "variable"), timevar="시점", direction="wide")
# colnames(current_population) <- c("Sex",'Age',c(1996:2022))
# current_population_male <- current_population %>% filter (Sex=='남자');  current_population_male  <- current_population_male[-17,-1]
# current_population_female <- current_population %>% filter (Sex=='여자');current_population_female<- current_population_female[-17,-1]
# 
# current_population_male  <- current_population_male[,-c(2,3,4,5,26,27,28)]
# current_population_female<- current_population_female[,-c(2,3,4,5,26,27,28)]
# 
# rownames(current_population_male)=NULL
# rownames(current_population_female)=NULL
# 
# ## 2020-2040 인구 추계 시나리오
# 
# colnames(projection_population_ver1) <- c("Scenarios","Sex","Age",c(2020:2050))
# colnames(projection_population_ver2) <- c("Scenarios","Sex","Age",c(2020:2040))
# 
# projection_population_ver1$Age<- gsub("\\s+", "", projection_population_ver1$Age)
# projection_population_ver2$Age<- gsub("\\s+", "", projection_population_ver2$Age)
# 
# projection_population<-rbind(projection_population_ver1[,c("Scenarios","Sex","Age",c(2020:2040))],projection_population_ver2)
# 
# projection_population_male <- projection_population %>% filter (Sex=='남자');  projection_population_male  <- projection_population_male[,-2]
# projection_population_female <- projection_population %>% filter (Sex=='여자');projection_population_female<- projection_population_female[,-2]
# 
# # Scenario 1 중위 추계(기본 추계: 출산율-중위 / 기대수명-중위 / 국제순이동-중위)
# Scenarios1_male  <- projection_population_male %>% filter (Scenarios=='중위 추계(기본 추계: 출산율-중위 / 기대수명-중위 / 국제순이동-중위)'); rownames(Scenarios1_male)=NULL;Scenarios1_male<-  cbind("Age"=agegroups,current_population_male[,-1],Scenarios1_male[,-c(1,2)])      
# Scenarios1_female<- projection_population_female %>% filter (Scenarios=='중위 추계(기본 추계: 출산율-중위 / 기대수명-중위 / 국제순이동-중위)'); rownames(Scenarios1_female)=NULL;Scenarios1_female<-  cbind("Age"=agegroups,current_population_female[,-1],Scenarios1_female[,-c(1,2)])
# 
# # Scenario 2 고위 추계(최대인구 추계: 출산율-고위 / 기대수명-고위 / 국제순이동-고위)')
# Scenarios2_male  <- projection_population_male %>% filter (Scenarios=='고위 추계(최대인구 추계: 출산율-고위 / 기대수명-고위 / 국제순이동-고위)');rownames(Scenarios2_male)=NULL ;Scenarios2_male<-  cbind("Age"=agegroups,current_population_male[,-1],Scenarios2_male[,-c(1,2)])   
# Scenarios2_female<- projection_population_female %>% filter (Scenarios=='고위 추계(최대인구 추계: 출산율-고위 / 기대수명-고위 / 국제순이동-고위)');rownames(Scenarios2_female)=NULL;Scenarios2_female<-  cbind("Age"=agegroups,current_population_female[,-1],Scenarios2_female[,-c(1,2)])   
# 
# # Scenario 3 저위 추계(최대인구 추계: 출산율-저위 / 기대수명-저위 / 국제순이동-저위)
# Scenarios3_male  <- projection_population_male %>% filter (Scenarios=='저위 추계(최대인구 추계: 출산율-저위 / 기대수명-저위 / 국제순이동-저위)');rownames(Scenarios3_male)=NULL ;Scenarios3_male<-  cbind("Age"=agegroups,current_population_male[,-1],Scenarios3_male[,-c(1,2)])   
# Scenarios3_female<- projection_population_female %>% filter (Scenarios=='저위 추계(최대인구 추계: 출산율-저위 / 기대수명-저위 / 국제순이동-저위)');rownames(Scenarios3_female)=NULL;Scenarios3_female<-  cbind("Age"=agegroups,current_population_female[,-1],Scenarios3_female[,-c(1,2)])   
# 
# # Scenario 4 빠른 고령화 추계(출산율-저위 / 기대수명-고위 / 국제순이동-저위)
# Scenarios4_male  <- projection_population_male %>% filter (Scenarios=='빠른 고령화 추계(출산율-저위 / 기대수명-고위 / 국제순이동-저위)');rownames(Scenarios4_male)=NULL;Scenarios4_male<-  cbind("Age"=agegroups,current_population_male[,-1],Scenarios4_male[,-c(1,2)])   
# Scenarios4_female<- projection_population_female %>% filter (Scenarios=='빠른 고령화 추계(출산율-저위 / 기대수명-고위 / 국제순이동-저위)');rownames(Scenarios4_female)=NULL;Scenarios4_female<-  cbind("Age"=agegroups,current_population_female[,-1],Scenarios4_female[,-c(1,2)])   
# 
# # Scenario 5 느린 고령화 추계(출산율-고위 / 기대수명-저위 / 국제순이동-고위)
# Scenarios5_male  <- projection_population_male %>% filter (Scenarios=='느린 고령화 추계(출산율-고위 / 기대수명-저위 / 국제순이동-고위)');rownames(Scenarios5_male)=NULL;Scenarios5_male<-  cbind("Age"=agegroups,current_population_male[,-1],Scenarios5_male[,-c(1,2)])   
# Scenarios5_female<- projection_population_female %>% filter (Scenarios=='느린 고령화 추계(출산율-고위 / 기대수명-저위 / 국제순이동-고위)');rownames(Scenarios5_female)=NULL;Scenarios5_female<-  cbind("Age"=agegroups,current_population_female[,-1],Scenarios5_female[,-c(1,2)])   
# 
# 
# ## Cancer Incidence
# 
# colnames(cancer_incidence_data) <- c("Cancer","Sex","Age",c(1999:2020))
# cancer_incidence_data <- cancer_incidence_data[,-4]
# cancer_incidence_data$Cancer<- gsub("\\s+", "", cancer_incidence_data$Cancer)
# cancer_incidence_data$Age <- as.factor(cancer_incidence_data$Age)
# levels(cancer_incidence_data$Age) <-agegroups
# 
# cancer_incidence_data[,c(4:ncol(cancer_incidence_data))] <- sapply(cancer_incidence_data[,c(4:ncol(cancer_incidence_data))] ,as.numeric)
# 
# ci_name <-unique(cancer_incidence_data$Cancer)
# ci_vname <- gsub("[-,]", "_", ci_name)
# inci_cname <-c()
# for (i in 1:length(ci_name)){
#   assign(paste0(gsub(".*\\((.*)\\).*", "\\1", ci_vname[i]),"_I"),subset(cancer_incidence_data,Cancer==ci_name[i]))
#   assign(paste0(gsub(".*\\((.*)\\).*", "\\1", ci_vname[i]),"_I"),arrange(get(paste0(gsub(".*\\((.*)\\).*", "\\1", ci_vname[i]),"_I")),Age))
#   inci_cname[i] <- paste0(gsub(".*\\((.*)\\).*", "\\1", ci_vname[i]),"_I")
# }
# 
# 
# ## Cancer Mortality
# colnames(cancer_mortality_data) <- c("Cancer","Sex","Age",c(2000:2021))
# cancer_mortality_data$Age<- gsub("\\s+", "", cancer_mortality_data$Age)
# 
# cancer_mortality_data<-melt(cancer_mortality_data,id.vars=c("Cancer","Sex","Age"))
# cancer_mortality_data[,5] <-sapply(cancer_mortality_data[,5],as.numeric)
# 
# oth <-cancer_mortality_data %>%
#   filter(!Age %in% c('0세', '1-4세','85-89세', '90세이상'))
# 
# oth$Age <- c(2:17)
# 
# yd <- cancer_mortality_data %>%
#   filter(Age %in% c('0세', '1-4세')) %>%
#   group_by(Cancer,Sex,variable) %>%
#   summarize(value = sum(value))
# 
# yd$Age <-1
# yd <- yd[,c(1,2,5,3,4)]
# 
# od <- cancer_mortality_data %>%
#   filter(Age %in% c('85-89세', '90세이상')) %>%
#   group_by(Cancer,Sex,variable) %>%
#   summarize(value = sum(value))
# 
# od$Age <-18
# od <- od[,c(1,2,5,3,4)]
# cancer_mortality_data <- rbind(yd,oth,od)
# cancer_mortality_data<-data.frame(cancer_mortality_data)
# 
# cancer_mortality_data<-cancer_mortality_data %>% 
#   arrange(Cancer, Sex,variable)
# 
# 
# cancer_mortality_data<-reshape(cancer_mortality_data, idvar=c("Cancer", "Sex","Age"), timevar="variable", direction="wide")
# colnames(cancer_mortality_data) <- c(c("Cancer", "Sex","Age"),2000:2021)
# cancer_mortality_data$Cancer<- gsub("\\s+", "", cancer_mortality_data$Cancer)
# cm_name <-unique(cancer_mortality_data$Cancer)
# cm_vname <- gsub("[-,]", "_", cm_name)
# mort_cname<-c()
# 
# for (i in 1:length(cm_name)){
#   assign(paste0(gsub(".*\\((.*)\\).*", "\\1", cm_vname[i]),"_D"),subset(cancer_mortality_data,Cancer==cm_name[i]))
#   assign(paste0(gsub(".*\\((.*)\\).*", "\\1", cm_vname[i]),"_D"),arrange(get(paste0(gsub(".*\\((.*)\\).*", "\\1", cm_vname[i]),"_D")),Cancer,Age))
#   mort_cname[i] <- paste0(gsub(".*\\((.*)\\).*", "\\1", cm_vname[i]),"_D")
# }
# 
# 
# mort_cname <- sort(mort_cname)
# mort_cname[c(1,2)] <- rev(mort_cname[c(1, 2)])
# mort_cname[c(21,22)] <- rev(mort_cname[c(21, 22)])
# 
# 
# save.image(file="/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/rawdata/workspace.RData")

################################################################################

load(file="/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/rawdata/workspace.RData")

male_ir_agespecific<-c()
female_ir_agespecific<-c()
male_mr_agespecific<-c()
female_mr_agespecific<-c()

## 1. age-specific
for (i in 1:length(inci_cname)){
  assign(paste0(inci_cname[i],"_Male_ir"),round((subset(get(inci_cname[i]),Sex=="남자")[,-c(1:3,ncol(get(inci_cname[i])))] / current_population_male [,-1])*100000,3))
  assign(paste0(inci_cname[i],"_Female_ir"),round((subset(get(inci_cname[i]),Sex=="여자")[,-c(1:3,ncol(get(inci_cname[i])))]  / current_population_female[,-1])*100000,3))
  male_ir_agespecific[i] <- paste0(inci_cname[i],"_Male_ir")
  female_ir_agespecific[i] <- paste0(inci_cname[i],"_Female_ir")
}

for (i in 1:length(mort_cname)){
  assign(paste0(mort_cname[i],"_Male_mr"),round((subset(get(mort_cname[i]),Sex=="남자")[,-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))] / current_population_male [,-1])*100000,3))
  assign(paste0(mort_cname[i],"_Female_mr"),round((subset(get(mort_cname[i]),Sex=="여자")[,-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))] / current_population_female [,-1])*100000,3))
  male_mr_agespecific[i] <- paste0(mort_cname[i],"_Male_mr")
  female_mr_agespecific[i] <- paste0(mort_cname[i],"_Female_mr")
}

##################################################################

## 2. all age
male_ir_allage<-c()
female_ir_allage<-c()
male_mr_allage<-c()
female_mr_allage<-c()

for (i in 1:length(inci_cname)){
  assign(paste0(inci_cname[i],"_Male_ir_all"),round((colSums(subset(get(inci_cname[i]),Sex=="남자")[,-c(1:3,ncol(get(inci_cname[i])))]) / colSums(current_population_male[,-1]))*100000,3))
  assign(paste0(inci_cname[i],"_Female_ir_all"),round((colSums(subset(get(inci_cname[i]),Sex=="여자")[,-c(1:3,ncol(get(inci_cname[i])))]) / colSums(current_population_female[,-1]))*100000,3))
  male_ir_allage[i] <- paste0(inci_cname[i],"_Male_ir_all")
  female_ir_allage[i] <- paste0(inci_cname[i],"_Female_ir_all")
}

for (i in 1:length(mort_cname)){
  assign(paste0(mort_cname[i],"_Male_mr_all"),round((colSums(subset(get(mort_cname[i]),Sex=="남자")[,-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))]) / colSums(current_population_male[,-1]))*100000,3))
  assign(paste0(mort_cname[i],"_Female_mr_all"),round((colSums(subset(get(mort_cname[i]),Sex=="여자")[,-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))]) / colSums(current_population_female[,-1]))*100000,3))
  male_mr_allage[i] <- paste0(mort_cname[i],"_Male_mr_all")
  female_mr_allage[i] <- paste0(mort_cname[i],"_Female_mr_all")
}

## 2-1. 65+
male_ir_age65<-c()
female_ir_age65<-c()
male_mr_age65<-c()
female_mr_age65<-c()

for (i in 1:length(inci_cname)){
  assign(paste0(inci_cname[i],"_Male_ir65"),round((colSums(subset(get(inci_cname[i]),Sex=="남자")[c(14:18),-c(1:3,ncol(get(inci_cname[i])))]) / colSums(current_population_male[c(14:18),][,-1]))*100000,3))
  assign(paste0(inci_cname[i],"_Female_ir65"),round((colSums(subset(get(inci_cname[i]),Sex=="여자")[c(14:18),-c(1:3,ncol(get(inci_cname[i])))]) / colSums(current_population_female[c(14:18),][,-1]))*100000,3))
  male_ir_age65[i] <- paste0(inci_cname[i],"_Male_ir65")
  female_ir_age65[i] <- paste0(inci_cname[i],"_Female_ir65")
}

for (i in 1:length(mort_cname)){
  assign(paste0(mort_cname[i],"_Male_mr65"),round((colSums(subset(get(mort_cname[i]),Sex=="남자")[c(14:18),-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))]) / colSums(current_population_male[c(14:18),][,-1]))*100000,3))
  assign(paste0(mort_cname[i],"_Female_mr65"),round((colSums(subset(get(mort_cname[i]),Sex=="여자")[c(14:18),-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))]) / colSums(current_population_female[c(14:18),][,-1]))*100000,3))
  male_mr_age65[i] <- paste0(mort_cname[i],"_Male_mr65")
  female_mr_age65[i] <- paste0(mort_cname[i],"_Female_mr65")
}

## 3. ASR (incidence ref: 2000, mortality ref: 2005)

ref_2000 <- (current_population_male[,-1][,1] + current_population_female[,-1][,1]) / (sum(current_population_male[,-c(2,3,4,5,ncol(current_population_male))][,-1][,1]) + sum(current_population_female[,-1][,1]))
ref_2005 <- (current_population_male[,-1][,6] + current_population_female[,-1][,6]) / (sum(current_population_male[,-c(2,3,4,5,ncol(current_population_male))][,-1][,6]) + sum(current_population_female[,-1][,6]))

male_ir_asr<-c()
female_ir_asr<-c()
male_mr_asr<-c()
female_mr_asr<-c()

for (i in 1:length(inci_cname)){
  assign(paste0(inci_cname[i],"_Male_ir_s"),round(colSums(((subset(get(inci_cname[i]),Sex=="남자")[,-c(1:3,ncol(get(inci_cname[i])))] / current_population_male[,-1])*100000) * ref_2000),3))
  assign(paste0(inci_cname[i],"_Female_ir_s"),round(colSums(((subset(get(inci_cname[i]),Sex=="여자")[,-c(1:3,ncol(get(inci_cname[i])))] / current_population_female[,-1])*100000) * ref_2000),3))
  male_ir_asr[i] <- paste0(inci_cname[i],"_Male_ir_s")
  female_ir_asr[i] <- paste0(inci_cname[i],"_Female_ir_s")
}

for (i in 1:length(mort_cname)){
  assign(paste0(mort_cname[i],"_Male_mr_s"),round(colSums(((subset(get(mort_cname[i]),Sex=="남자")[,-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))] / current_population_male[,-1])*100000) * ref_2005),3))
  assign(paste0(mort_cname[i],"_Female_mr_s"),round(colSums(((subset(get(mort_cname[i]),Sex=="여자")[,-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))] / current_population_female[,-1])*100000) * ref_2005),3))
  male_mr_asr[i] <- paste0(mort_cname[i],"_Male_mr_s")
  female_mr_asr[i] <- paste0(mort_cname[i],"_Female_mr_s")
}

## 4. Population attributable fraction (ref 65+)

male_ir_paf<-c()
female_ir_paf<-c()
male_mr_paf<-c()
female_mr_paf<-c()

for (i in 1:length(inci_cname)){
  I_Tm <- colSums(subset(get(inci_cname[i]),Sex=="남자")[,-c(1:3,ncol(get(inci_cname[i])))]) / colSums(current_population_male[,-1])
  I_NEm <-colSums(subset(get(inci_cname[i]),Sex=="남자")[c(1:13),-c(1:3,ncol(get(inci_cname[i])))]) / colSums(current_population_male[c(1:13),][,-1])
  I_Tw <- colSums(subset(get(inci_cname[i]),Sex=="여자")[,-c(1:3,ncol(get(inci_cname[i])))]) / colSums(current_population_female[,-1])
  I_NEw <-colSums(subset(get(inci_cname[i]),Sex=="여자")[c(1:13),-c(1:3,ncol(get(inci_cname[i])))]) / colSums(current_population_female[c(1:13),][,-1])
  
  assign(paste0(inci_cname[i],"_Male_ir_paf"),round((I_Tm-I_NEm)/I_Tm*100,1))
  assign(paste0(inci_cname[i],"_Female_ir_paf"),round((I_Tw-I_NEw)/I_Tw*100,1))
  male_ir_paf[i] <- paste0(inci_cname[i],"_Male_ir_paf")
  female_ir_paf[i] <- paste0(inci_cname[i],"_Female_ir_paf")
}


for (i in 1:length(mort_cname)){
  I_Tm <- colSums(subset(get(inci_cname[i]),Sex=="남자")[,-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))]) / colSums(current_population_male[,-1])
  I_NEm <-colSums(subset(get(inci_cname[i]),Sex=="남자")[c(1:13),-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))]) / colSums(current_population_male[c(1:13),][,-1])
  I_Tw <- colSums(subset(get(inci_cname[i]),Sex=="여자")[,-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))]) / colSums(current_population_female[,-1])
  I_NEw <-colSums(subset(get(inci_cname[i]),Sex=="여자")[c(1:13),-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))]) / colSums(current_population_female[c(1:13),][,-1])
  
  assign(paste0(mort_cname[i],"_Male_mr_paf"),round((I_Tm-I_NEm)/I_Tm*100,1))
  assign(paste0(mort_cname[i],"_Female_mr_paf"),round((I_Tw-I_NEw)/I_Tw*100,1))
  male_mr_paf[i] <- paste0(mort_cname[i],"_Male_mr_paf")
  female_mr_paf[i] <- paste0(mort_cname[i],"_Female_mr_paf")
}


########## ratio 00-34, 35-49, 50-64, 65+######## 

male_ir_0034<-c()
female_ir_0034<-c()
male_mr_0034<-c()
female_mr_0034<-c()

male_ir_3549<-c()
female_ir_3549<-c()
male_mr_3549<-c()
female_mr_3549<-c()

male_ir_5064<-c()
female_ir_5064<-c()
male_mr_5064<-c()
female_mr_5064<-c()


for (i in 1:length(inci_cname)){
  # 0034
  assign(paste0(inci_cname[i],"_Male_ir_0034_age"),round((colSums(subset(get(inci_cname[i]),Sex=="남자")[c(1:7),-c(1:3,ncol(get(inci_cname[i])))]) / colSums(current_population_male[c(1:7),-1]))*100000,3))
  assign(paste0(inci_cname[i],"_Female_ir_0034_age"),round((colSums(subset(get(inci_cname[i]),Sex=="여자")[c(1:7),-c(1:3,ncol(get(inci_cname[i])))]) / colSums(current_population_female[c(1:7),-1]))*100000,3))
  male_ir_0034[i] <- paste0(inci_cname[i],"_Male_ir_0034_age")
  female_ir_0034[i] <- paste0(inci_cname[i],"_Female_ir_0034_age")
  
  # 3549
  assign(paste0(inci_cname[i],"_Male_ir_3549_age"),round((colSums(subset(get(inci_cname[i]),Sex=="남자")[c(8:10),-c(1:3,ncol(get(inci_cname[i])))]) / colSums(current_population_male[c(8:10),-1]))*100000,3))
  assign(paste0(inci_cname[i],"_Female_ir_3549_age"),round((colSums(subset(get(inci_cname[i]),Sex=="여자")[c(8:10),-c(1:3,ncol(get(inci_cname[i])))]) / colSums(current_population_female[c(8:10),-1]))*100000,3))
  male_ir_3549[i] <- paste0(inci_cname[i],"_Male_ir_3549_age")
  female_ir_3549[i] <- paste0(inci_cname[i],"_Female_ir_3549_age")
  
  # 5064
  assign(paste0(inci_cname[i],"_Male_ir_5064_age"),round((colSums(subset(get(inci_cname[i]),Sex=="남자")[c(11:13),-c(1:3,ncol(get(inci_cname[i])))]) / colSums(current_population_male[c(11:13),-1]))*100000,3))
  assign(paste0(inci_cname[i],"_Female_ir_5064_age"),round((colSums(subset(get(inci_cname[i]),Sex=="여자")[c(11:13),-c(1:3,ncol(get(inci_cname[i])))]) / colSums(current_population_female[c(11:13),-1]))*100000,3))
  male_ir_5064[i] <- paste0(inci_cname[i],"_Male_ir_5064_age")
  female_ir_5064[i] <- paste0(inci_cname[i],"_Female_ir_5064_age")
}

for (i in 1:length(mort_cname)){
  # 0034
  assign(paste0(mort_cname[i],"_Male_mr_0034_age"),round((colSums(subset(get(mort_cname[i]),Sex=="남자")[c(1:7),-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))]) / colSums(current_population_male[c(1:7),-1]))*100000,3))
  assign(paste0(mort_cname[i],"_Female_mr_0034_age"),round((colSums(subset(get(mort_cname[i]),Sex=="여자")[c(1:7),-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))]) / colSums(current_population_female[c(1:7),-1]))*100000,3))
  male_mr_0034[i] <- paste0(mort_cname[i],"_Male_mr_0034_age")
  female_mr_0034[i] <- paste0(mort_cname[i],"_Female_mr_0034_age")
  # 3549
  assign(paste0(mort_cname[i],"_Male_mr_3549_age"),round((colSums(subset(get(mort_cname[i]),Sex=="남자")[c(8:10),-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))]) / colSums(current_population_male[c(8:10),-1]))*100000,1))
  assign(paste0(mort_cname[i],"_Female_mr_3549_age"),round((colSums(subset(get(mort_cname[i]),Sex=="여자")[c(8:10),-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))]) / colSums(current_population_female[c(8:10),-1]))*100000,3))
  male_mr_3549[i] <- paste0(mort_cname[i],"_Male_mr_3549_age")
  female_mr_3549[i] <- paste0(mort_cname[i],"_Female_mr_3549_age")
  
  # 5064
  assign(paste0(mort_cname[i],"_Male_mr_5064_age"),round((colSums(subset(get(mort_cname[i]),Sex=="남자")[c(11:13),-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))]) / colSums(current_population_male[c(11:13),-1]))*100000,3))
  assign(paste0(mort_cname[i],"_Female_mr_5064_age"),round((colSums(subset(get(mort_cname[i]),Sex=="여자")[c(11:13),-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))]) / colSums(current_population_female[c(11:13),-1]))*100000,3))
  male_mr_5064[i] <- paste0(mort_cname[i],"_Male_mr_5064_age")
  female_mr_5064[i] <- paste0(mort_cname[i],"_Female_mr_5064_age")
  
}
########## Making Table - extract csv######## 

mak_Tble<-function (ref1,ref2,ref3,ref4){
  combined_data1 <- data.frame(matrix(ncol = 19, nrow = 0))
  combined_data2 <- data.frame(matrix(ncol = 19, nrow = 0))
  combined_data3 <- data.frame(matrix(ncol = 19, nrow = 0))
  combined_data4 <- data.frame(matrix(ncol = 19, nrow = 0))
  
  for (i in 1:length(inci_cname)) {
    data1 <- get(ref1[i])
    combined_data1 <- rbind(combined_data1, data1)
    data2 <- get(ref2[i])
    combined_data2 <- rbind(combined_data2, data2)
  }
  
  for (i in 1:length(mort_cname)) {
    data3 <- get(ref3[i])
    combined_data3 <- rbind(combined_data3, data3)
    data4 <- get(ref4[i])
    combined_data4 <- rbind(combined_data4, data4)
  }
  
  colnames(combined_data1) <- c(2000:2019)
  colnames(combined_data2) <- c(2000:2019)
  colnames(combined_data3) <- c(2000:2019)
  colnames(combined_data4) <- c(2000:2019)
  
  rownames(combined_data1) <- inci_cname
  rownames(combined_data2) <- inci_cname
  rownames(combined_data3) <- mort_cname
  rownames(combined_data4) <- mort_cname
  
  combined_data<-list(male_incidence = combined_data1, 
                      female_incidence = combined_data2,
                      male_mortality = combined_data3,
                      female_mortality = combined_data4)
  
  return(combined_data)
}

allage_rate <-mak_Tble(male_ir_allage,female_ir_allage,male_mr_allage,female_mr_allage)

age0034_rate <-mak_Tble(male_ir_0034,female_ir_0034,male_mr_0034,female_mr_0034)
age3549_rate <-mak_Tble(male_ir_3549,female_ir_3549,male_mr_3549,female_mr_3549)
age5064_rate <-mak_Tble(male_ir_5064,female_ir_5064,male_mr_5064,female_mr_5064)
age65_rate <-mak_Tble(male_ir_age65,female_ir_age65,male_mr_age65,female_mr_age65)

asr <-mak_Tble(male_ir_asr,female_ir_asr,male_mr_asr,female_mr_asr)
paf <-mak_Tble(male_ir_paf,female_ir_paf,male_mr_paf,female_mr_paf)

capture.output(allage_rate, file = "/users/jun/Desktop/allage_rate.csv")

capture.output(age0034_rate, file = "/users/jun/Desktop/age0034_rate.csv")
capture.output(age3549_rate, file = "/users/jun/Desktop/age3549_rate.csv")
capture.output(age5064_rate, file = "/users/jun/Desktop/age5064_rate.csv")
capture.output(age65_rate, file = "/users/jun/Desktop/age65_rate.csv")

capture.output(asr, file = "/users/jun/Desktop/asr.csv")
capture.output(paf, file = "/users/jun/Desktop/paf.csv")


################################################################################

# check apc effect -> apc web tools

pop_data_M<-cbind('2000-2004'=rowSums(Scenarios1_male[,as.character(2000:2004)]),
                  '2005-2009'=rowSums(Scenarios1_male[,as.character(2005:2009)]),
                  '2010-2014'=rowSums(Scenarios1_male[,as.character(2010:2014)]),
                  '2015-2019'=rowSums(Scenarios1_male[,as.character(2015:2019)]))

pop_data_F<-cbind('2000-2004'=rowSums(Scenarios1_female[,as.character(2000:2004)]),
                  '2005-2009'=rowSums(Scenarios1_female[,as.character(2005:2009)]),
                  '2010-2014'=rowSums(Scenarios1_female[,as.character(2010:2014)]),
                  '2015-2019'=rowSums(Scenarios1_female[,as.character(2015:2019)]))


for ( i in 1:length(inci_cname_M)){
  case_data_M<-cbind('2000-2004'=rowSums(subset(get(inci_cname_M[i]),Sex=="남자")[,as.character(2000:2004)]),
                     '2005-2009'=rowSums(subset(get(inci_cname_M[i]),Sex=="남자")[,as.character(2005:2009)]),
                     '2010-2014'=rowSums(subset(get(inci_cname_M[i]),Sex=="남자")[,as.character(2010:2014)]),
                     '2015-2019'=rowSums(subset(get(inci_cname_M[i]),Sex=="남자")[,as.character(2015:2019)]))
  
  print(cbind(case_data_M[,1],pop_data_M[,1],case_data_M[,2],pop_data_M[,2],case_data_M[,3],pop_data_M[,3],case_data_M[,4],pop_data_M[,4])
  )
}

for ( i in 1:length(mort_cname_M)){
  case_data_M<-cbind('2000-2004'=rowSums(subset(get(mort_cname_M[i]),Sex=="남자")[,as.character(2000:2004)]),
                     '2005-2009'=rowSums(subset(get(mort_cname_M[i]),Sex=="남자")[,as.character(2005:2009)]),
                     '2010-2014'=rowSums(subset(get(mort_cname_M[i]),Sex=="남자")[,as.character(2010:2014)]),
                     '2015-2019'=rowSums(subset(get(mort_cname_M[i]),Sex=="남자")[,as.character(2015:2019)]))
  
  print(cbind(case_data_M[,1],pop_data_M[,1],case_data_M[,2],pop_data_M[,2],case_data_M[,3],pop_data_M[,3],case_data_M[,4],pop_data_M[,4])
  )
}


for ( i in 1:length(inci_cname_F)){
  case_data_M<-cbind('2000-2004'=rowSums(subset(get(inci_cname_F[i]),Sex=="여자")[,as.character(2000:2004)]),
                     '2005-2009'=rowSums(subset(get(inci_cname_F[i]),Sex=="여자")[,as.character(2005:2009)]),
                     '2010-2014'=rowSums(subset(get(inci_cname_F[i]),Sex=="여자")[,as.character(2010:2014)]),
                     '2015-2019'=rowSums(subset(get(inci_cname_F[i]),Sex=="여자")[,as.character(2015:2019)]))
  
  print(cbind(case_data_M[,1],pop_data_F[,1],case_data_M[,2],pop_data_F[,2],case_data_M[,3],pop_data_F[,3],case_data_M[,4],pop_data_F[,4])
  )
}

for ( i in 1:length(mort_cname_F)){
  case_data_M<-cbind('2000-2004'=rowSums(subset(get(mort_cname_F[i]),Sex=="여자")[,as.character(2000:2004)]),
                     '2005-2009'=rowSums(subset(get(mort_cname_F[i]),Sex=="여자")[,as.character(2005:2009)]),
                     '2010-2014'=rowSums(subset(get(mort_cname_F[i]),Sex=="여자")[,as.character(2010:2014)]),
                     '2015-2019'=rowSums(subset(get(mort_cname_F[i]),Sex=="여자")[,as.character(2015:2019)]))
  
  print(cbind(case_data_M[,1],pop_data_F[,1],case_data_M[,2],pop_data_F[,2],case_data_M[,3],pop_data_F[,3],case_data_M[,4],pop_data_F[,4])
  )
}



####################################################################################
####################################################################################


pop_data_M_proj<-cbind('2000-2004'=rowSums(Scenarios1_male[,as.character(2000:2004)]),
                       '2005-2009'=rowSums(Scenarios1_male[,as.character(2005:2009)]),
                       '2010-2014'=rowSums(Scenarios1_male[,as.character(2010:2014)]),
                       '2015-2019'=rowSums(Scenarios1_male[,as.character(2015:2019)]),
                       '2020-2024'=rowSums(Scenarios1_male[,as.character(2020:2024)]),
                       '2025-2029'=rowSums(Scenarios1_male[,as.character(2025:2029)]),
                       '2030-2034'=rowSums(Scenarios1_male[,as.character(2030:2034)]),
                       '2035-2039'=rowSums(Scenarios1_male[,as.character(2035:2039)]))

pop_data_F_proj<-cbind('2000-2004'=rowSums(Scenarios1_female[,as.character(2000:2004)]),
                       '2005-2009'=rowSums(Scenarios1_female[,as.character(2005:2009)]),
                       '2010-2014'=rowSums(Scenarios1_female[,as.character(2010:2014)]),
                       '2015-2019'=rowSums(Scenarios1_female[,as.character(2015:2019)]),
                       '2020-2024'=rowSums(Scenarios1_female[,as.character(2020:2024)]),
                       '2025-2029'=rowSums(Scenarios1_female[,as.character(2025:2029)]),
                       '2030-2034'=rowSums(Scenarios1_female[,as.character(2030:2034)]),
                       '2035-2039'=rowSums(Scenarios1_female[,as.character(2035:2039)]))


scene1_nd_byage_male_inc <- scene1_nd_byage[[1]]
scene1_nd_byage_female_inc <- scene1_nd_byage[[2]]
scene1_nd_byage_male_mort <- scene1_nd_byage[[3]]
scene1_nd_byage_female_mort <- scene1_nd_byage[[4]]

for ( i in 1:length(inci_cname_M)){
  case_data_M<-cbind('2000-2004'=rowSums(scene1_nd_byage_male_inc[[i]][,as.character(2000:2004)]),
                     '2005-2009'=rowSums(scene1_nd_byage_male_inc[[i]][,as.character(2005:2009)]),
                     '2010-2014'=rowSums(scene1_nd_byage_male_inc[[i]][,as.character(2010:2014)]),
                     '2015-2019'=rowSums(scene1_nd_byage_male_inc[[i]][,as.character(2015:2019)]),
                     '2020-2024'=rowSums(scene1_nd_byage_male_inc[[i]][,as.character(2020:2024)]),
                     '2025-2029'=rowSums(scene1_nd_byage_male_inc[[i]][,as.character(2025:2029)]),
                     '2030-2034'=rowSums(scene1_nd_byage_male_inc[[i]][,as.character(2030:2034)]),
                     '2035-2039'=rowSums(scene1_nd_byage_male_inc[[i]][,as.character(2035:2039)]))
  
  print(cbind(case_data_M[,1],pop_data_M_proj[,1],case_data_M[,2],pop_data_M_proj[,2],case_data_M[,3],pop_data_M_proj[,3],case_data_M[,4],pop_data_M_proj[,4],
              case_data_M[,5],pop_data_M_proj[,5],case_data_M[,6],pop_data_M_proj[,6],case_data_M[,7],pop_data_M_proj[,7],case_data_M[,8],pop_data_M_proj[,8])
  )
}

for ( i in 1:length(mort_cname_M)){
  case_data_M<-cbind('2000-2004'=rowSums(scene1_nd_byage_male_mort[[i]][,as.character(2000:2004)]),
                     '2005-2009'=rowSums(scene1_nd_byage_male_mort[[i]][,as.character(2005:2009)]),
                     '2010-2014'=rowSums(scene1_nd_byage_male_mort[[i]][,as.character(2010:2014)]),
                     '2015-2019'=rowSums(scene1_nd_byage_male_mort[[i]][,as.character(2015:2019)]),
                     '2020-2024'=rowSums(scene1_nd_byage_male_mort[[i]][,as.character(2020:2024)]),
                     '2025-2029'=rowSums(scene1_nd_byage_male_mort[[i]][,as.character(2025:2029)]),
                     '2030-2034'=rowSums(scene1_nd_byage_male_mort[[i]][,as.character(2030:2034)]),
                     '2035-2039'=rowSums(scene1_nd_byage_male_mort[[i]][,as.character(2035:2039)]))
  
  print(cbind(case_data_M[,1],pop_data_M_proj[,1],case_data_M[,2],pop_data_M_proj[,2],case_data_M[,3],pop_data_M_proj[,3],case_data_M[,4],pop_data_M_proj[,4],
              case_data_M[,5],pop_data_M_proj[,5],case_data_M[,6],pop_data_M_proj[,6],case_data_M[,7],pop_data_M_proj[,7],case_data_M[,8],pop_data_M_proj[,8])
  )
}


for ( i in 1:length(inci_cname_F)){
  case_data_F<-cbind('2000-2004'=rowSums(scene1_nd_byage_female_inc[[i]][,as.character(2000:2004)]),
                     '2005-2009'=rowSums(scene1_nd_byage_female_inc[[i]][,as.character(2005:2009)]),
                     '2010-2014'=rowSums(scene1_nd_byage_female_inc[[i]][,as.character(2010:2014)]),
                     '2015-2019'=rowSums(scene1_nd_byage_female_inc[[i]][,as.character(2015:2019)]),
                     '2020-2024'=rowSums(scene1_nd_byage_female_inc[[i]][,as.character(2020:2024)]),
                     '2025-2029'=rowSums(scene1_nd_byage_female_inc[[i]][,as.character(2025:2029)]),
                     '2030-2034'=rowSums(scene1_nd_byage_female_inc[[i]][,as.character(2030:2034)]),
                     '2035-2039'=rowSums(scene1_nd_byage_female_inc[[i]][,as.character(2035:2039)]))
  
  print(cbind(case_data_F[,1],pop_data_F_proj[,1],case_data_F[,2],pop_data_F_proj[,2],case_data_F[,3],pop_data_F_proj[,3],case_data_F[,4],pop_data_F_proj[,4],
              case_data_F[,5],pop_data_F_proj[,5],case_data_F[,6],pop_data_F_proj[,6],case_data_F[,7],pop_data_F_proj[,7],case_data_F[,8],pop_data_F_proj[,8])
  )
}

for ( i in 1:length(mort_cname_F)){
  case_data_F<-cbind('2000-2004'=rowSums(scene1_nd_byage_female_mort[[i]][,as.character(2000:2004)]),
                     '2005-2009'=rowSums(scene1_nd_byage_female_mort[[i]][,as.character(2005:2009)]),
                     '2010-2014'=rowSums(scene1_nd_byage_female_mort[[i]][,as.character(2010:2014)]),
                     '2015-2019'=rowSums(scene1_nd_byage_female_mort[[i]][,as.character(2015:2019)]),
                     '2020-2024'=rowSums(scene1_nd_byage_female_mort[[i]][,as.character(2020:2024)]),
                     '2025-2029'=rowSums(scene1_nd_byage_female_mort[[i]][,as.character(2025:2029)]),
                     '2030-2034'=rowSums(scene1_nd_byage_female_mort[[i]][,as.character(2030:2034)]),
                     '2035-2039'=rowSums(scene1_nd_byage_female_mort[[i]][,as.character(2035:2039)]))
  
  print(cbind(case_data_F[,1],pop_data_F_proj[,1],case_data_F[,2],pop_data_F_proj[,2],case_data_F[,3],pop_data_F_proj[,3],case_data_F[,4],pop_data_F_proj[,4],
              case_data_F[,5],pop_data_F_proj[,5],case_data_F[,6],pop_data_F_proj[,6],case_data_F[,7],pop_data_F_proj[,7],case_data_F[,8],pop_data_F_proj[,8])
  )
}

################################################################################

A <- function (count, population, reference, comparison){
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
        case <- (as.numeric(as.vector(unlist(DA(subset(get(inci_cname_M[j]),Sex=="남자")[,-c(1:3,ncol(get(inci_cname_M[j])))],current_population_male[,-1],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum((subset(get(inci_cname_M[j]),Sex=="남자")[,-c(1:3,ncol(get(inci_cname_M[j])))])[,i])-sum((subset(get(inci_cname_M[j]),Sex=="남자")[,-c(1:3,ncol(get(inci_cname_M[j])))])[,1])
      }
      
      years = c(2000:2019)
      change_inc <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                               Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 20))
      change_inc$Group <- factor(change_inc$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change_inc$Type <- c("Incidence")
      
      ### Mortality
      
      P_year <- A_year <- E_year <- net_change <- numeric(length = 20)
      
      for (i in 1:20){
        case <- (as.numeric(as.vector(unlist(DA(subset(get(mort_cname_M[j]),Sex=="남자")[,-c(1:3,ncol(get(mort_cname_M[j]))-1,ncol(get(mort_cname_M[j])))],current_population_male[,-1],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum((subset(get(mort_cname_M[j]),Sex=="남자")[,-c(1:3,ncol(get(mort_cname_M[j]))-1,ncol(get(mort_cname_M[j])))])[,i])-sum((subset(get(mort_cname_M[j]),Sex=="남자")[,-c(1:3,ncol(get(mort_cname_M[j]))-1,ncol(get(mort_cname_M[j])))])[,6])
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
        case <- (as.numeric(as.vector(unlist(DA(subset(get(inci_cname_F[j]),Sex=="여자")[,-c(1:3,ncol(get(inci_cname_F[j])))],current_population_female[,-1],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum((subset(get(inci_cname_F[j]),Sex=="여자")[,-c(1:3,ncol(get(inci_cname_F[j])))])[,i])-sum((subset(get(inci_cname_F[j]),Sex=="여자")[,-c(1:3,ncol(get(inci_cname_F[j])))])[,1])
      }
      
      years = c(2000:2019)
      change_inc <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                               Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 20))
      change_inc$Group <- factor(change_inc$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change_inc$Type <- c("Incidence")
      
      
      ### Mortality
      P_year <- A_year <- E_year <- net_change <- numeric(length = 20)
      
      for (i in 1:20){
        case <- (as.numeric(as.vector(unlist(DA(subset(get(mort_cname_F[j]),Sex=="여자")[,-c(1:3,ncol(get(mort_cname_F[j]))-1,ncol(get(mort_cname_F[j])))],current_population_female[,-1],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum((subset(get(mort_cname_F[j]),Sex=="여자")[,-c(1:3,ncol(get(mort_cname_F[j]))-1,ncol(get(mort_cname_F[j])))])[,i])-sum((subset(get(mort_cname_F[j]),Sex=="여자")[,-c(1:3,ncol(get(mort_cname_F[j]))-1,ncol(get(mort_cname_F[j])))])[,6])
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



