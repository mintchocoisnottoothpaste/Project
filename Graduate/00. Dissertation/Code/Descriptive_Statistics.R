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

load(file="/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/rawdata/workspace.RData")

##########Incidence Rates / Mortality Rates ######## 

male_ir_agespecific<-c()
female_ir_agespecific<-c()
male_mr_agespecific<-c()
female_mr_agespecific<-c()

## 1. age-specific
for (i in 1:length(inci_cname)){
  assign(paste0(inci_cname[i],"_Male_ir"),round((subset(get(inci_cname[i]),Sex=="害切")[,-c(1:3,ncol(get(inci_cname[i])))] / current_population_male [,-1])*100000,3))
  assign(paste0(inci_cname[i],"_Female_ir"),round((subset(get(inci_cname[i]),Sex=="食切")[,-c(1:3,ncol(get(inci_cname[i])))]  / current_population_female[,-1])*100000,3))
  male_ir_agespecific[i] <- paste0(inci_cname[i],"_Male_ir")
  female_ir_agespecific[i] <- paste0(inci_cname[i],"_Female_ir")
}

for (i in 1:length(mort_cname)){
  assign(paste0(mort_cname[i],"_Male_mr"),round((subset(get(mort_cname[i]),Sex=="害切")[,-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))] / current_population_male [,-1])*100000,3))
  assign(paste0(mort_cname[i],"_Female_mr"),round((subset(get(mort_cname[i]),Sex=="食切")[,-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))] / current_population_female [,-1])*100000,3))
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
  assign(paste0(inci_cname[i],"_Male_ir_all"),round((colSums(subset(get(inci_cname[i]),Sex=="害切")[,-c(1:3,ncol(get(inci_cname[i])))]) / colSums(current_population_male[,-1]))*100000,3))
  assign(paste0(inci_cname[i],"_Female_ir_all"),round((colSums(subset(get(inci_cname[i]),Sex=="食切")[,-c(1:3,ncol(get(inci_cname[i])))]) / colSums(current_population_female[,-1]))*100000,3))
  male_ir_allage[i] <- paste0(inci_cname[i],"_Male_ir_all")
  female_ir_allage[i] <- paste0(inci_cname[i],"_Female_ir_all")
}

for (i in 1:length(mort_cname)){
  assign(paste0(mort_cname[i],"_Male_mr_all"),round((colSums(subset(get(mort_cname[i]),Sex=="害切")[,-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))]) / colSums(current_population_male[,-1]))*100000,3))
  assign(paste0(mort_cname[i],"_Female_mr_all"),round((colSums(subset(get(mort_cname[i]),Sex=="食切")[,-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))]) / colSums(current_population_female[,-1]))*100000,3))
  male_mr_allage[i] <- paste0(mort_cname[i],"_Male_mr_all")
  female_mr_allage[i] <- paste0(mort_cname[i],"_Female_mr_all")
}

## 2-1. 65+
male_ir_age65<-c()
female_ir_age65<-c()
male_mr_age65<-c()
female_mr_age65<-c()

for (i in 1:length(inci_cname)){
  assign(paste0(inci_cname[i],"_Male_ir65"),round((colSums(subset(get(inci_cname[i]),Sex=="害切")[c(14:18),-c(1:3,ncol(get(inci_cname[i])))]) / colSums(current_population_male[c(14:18),][,-1]))*100000,3))
  assign(paste0(inci_cname[i],"_Female_ir65"),round((colSums(subset(get(inci_cname[i]),Sex=="食切")[c(14:18),-c(1:3,ncol(get(inci_cname[i])))]) / colSums(current_population_female[c(14:18),][,-1]))*100000,3))
  male_ir_age65[i] <- paste0(inci_cname[i],"_Male_ir65")
  female_ir_age65[i] <- paste0(inci_cname[i],"_Female_ir65")
}

for (i in 1:length(mort_cname)){
  assign(paste0(mort_cname[i],"_Male_mr65"),round((colSums(subset(get(mort_cname[i]),Sex=="害切")[c(14:18),-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))]) / colSums(current_population_male[c(14:18),][,-1]))*100000,3))
  assign(paste0(mort_cname[i],"_Female_mr65"),round((colSums(subset(get(mort_cname[i]),Sex=="食切")[c(14:18),-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))]) / colSums(current_population_female[c(14:18),][,-1]))*100000,3))
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
  assign(paste0(inci_cname[i],"_Male_ir_s"),round(colSums(((subset(get(inci_cname[i]),Sex=="害切")[,-c(1:3,ncol(get(inci_cname[i])))] / current_population_male[,-1])*100000) * ref_2000),3))
  assign(paste0(inci_cname[i],"_Female_ir_s"),round(colSums(((subset(get(inci_cname[i]),Sex=="食切")[,-c(1:3,ncol(get(inci_cname[i])))] / current_population_female[,-1])*100000) * ref_2000),3))
  male_ir_asr[i] <- paste0(inci_cname[i],"_Male_ir_s")
  female_ir_asr[i] <- paste0(inci_cname[i],"_Female_ir_s")
}

for (i in 1:length(mort_cname)){
  assign(paste0(mort_cname[i],"_Male_mr_s"),round(colSums(((subset(get(mort_cname[i]),Sex=="害切")[,-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))] / current_population_male[,-1])*100000) * ref_2005),3))
  assign(paste0(mort_cname[i],"_Female_mr_s"),round(colSums(((subset(get(mort_cname[i]),Sex=="食切")[,-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))] / current_population_female[,-1])*100000) * ref_2005),3))
  male_mr_asr[i] <- paste0(mort_cname[i],"_Male_mr_s")
  female_mr_asr[i] <- paste0(mort_cname[i],"_Female_mr_s")
}

## 4. Population attributable fraction (ref 65+)

male_ir_paf<-c()
female_ir_paf<-c()
male_mr_paf<-c()
female_mr_paf<-c()

for (i in 1:length(inci_cname)){
  I_Tm <- colSums(subset(get(inci_cname[i]),Sex=="害切")[,-c(1:3,ncol(get(inci_cname[i])))]) / colSums(current_population_male[,-1])
  I_NEm <-colSums(subset(get(inci_cname[i]),Sex=="害切")[c(1:13),-c(1:3,ncol(get(inci_cname[i])))]) / colSums(current_population_male[c(1:13),][,-1])
  I_Tw <- colSums(subset(get(inci_cname[i]),Sex=="食切")[,-c(1:3,ncol(get(inci_cname[i])))]) / colSums(current_population_female[,-1])
  I_NEw <-colSums(subset(get(inci_cname[i]),Sex=="食切")[c(1:13),-c(1:3,ncol(get(inci_cname[i])))]) / colSums(current_population_female[c(1:13),][,-1])
  
  assign(paste0(inci_cname[i],"_Male_ir_paf"),round((I_Tm-I_NEm)/I_Tm*100,1))
  assign(paste0(inci_cname[i],"_Female_ir_paf"),round((I_Tw-I_NEw)/I_Tw*100,1))
  male_ir_paf[i] <- paste0(inci_cname[i],"_Male_ir_paf")
  female_ir_paf[i] <- paste0(inci_cname[i],"_Female_ir_paf")
}


for (i in 1:length(mort_cname)){
  I_Tm <- colSums(subset(get(inci_cname[i]),Sex=="害切")[,-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))]) / colSums(current_population_male[,-1])
  I_NEm <-colSums(subset(get(inci_cname[i]),Sex=="害切")[c(1:13),-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))]) / colSums(current_population_male[c(1:13),][,-1])
  I_Tw <- colSums(subset(get(inci_cname[i]),Sex=="食切")[,-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))]) / colSums(current_population_female[,-1])
  I_NEw <-colSums(subset(get(inci_cname[i]),Sex=="食切")[c(1:13),-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))]) / colSums(current_population_female[c(1:13),][,-1])
  
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
  assign(paste0(inci_cname[i],"_Male_ir_0034_age"),round((colSums(subset(get(inci_cname[i]),Sex=="害切")[c(1:7),-c(1:3,ncol(get(inci_cname[i])))]) / colSums(current_population_male[c(1:7),-1]))*100000,3))
  assign(paste0(inci_cname[i],"_Female_ir_0034_age"),round((colSums(subset(get(inci_cname[i]),Sex=="食切")[c(1:7),-c(1:3,ncol(get(inci_cname[i])))]) / colSums(current_population_female[c(1:7),-1]))*100000,3))
  male_ir_0034[i] <- paste0(inci_cname[i],"_Male_ir_0034_age")
  female_ir_0034[i] <- paste0(inci_cname[i],"_Female_ir_0034_age")
  
  # 3549
  assign(paste0(inci_cname[i],"_Male_ir_3549_age"),round((colSums(subset(get(inci_cname[i]),Sex=="害切")[c(8:10),-c(1:3,ncol(get(inci_cname[i])))]) / colSums(current_population_male[c(8:10),-1]))*100000,3))
  assign(paste0(inci_cname[i],"_Female_ir_3549_age"),round((colSums(subset(get(inci_cname[i]),Sex=="食切")[c(8:10),-c(1:3,ncol(get(inci_cname[i])))]) / colSums(current_population_female[c(8:10),-1]))*100000,3))
  male_ir_3549[i] <- paste0(inci_cname[i],"_Male_ir_3549_age")
  female_ir_3549[i] <- paste0(inci_cname[i],"_Female_ir_3549_age")
  
  # 5064
  assign(paste0(inci_cname[i],"_Male_ir_5064_age"),round((colSums(subset(get(inci_cname[i]),Sex=="害切")[c(11:13),-c(1:3,ncol(get(inci_cname[i])))]) / colSums(current_population_male[c(11:13),-1]))*100000,3))
  assign(paste0(inci_cname[i],"_Female_ir_5064_age"),round((colSums(subset(get(inci_cname[i]),Sex=="食切")[c(11:13),-c(1:3,ncol(get(inci_cname[i])))]) / colSums(current_population_female[c(11:13),-1]))*100000,3))
  male_ir_5064[i] <- paste0(inci_cname[i],"_Male_ir_5064_age")
  female_ir_5064[i] <- paste0(inci_cname[i],"_Female_ir_5064_age")
}

for (i in 1:length(mort_cname)){
  # 0034
  assign(paste0(mort_cname[i],"_Male_mr_0034_age"),round((colSums(subset(get(mort_cname[i]),Sex=="害切")[c(1:7),-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))]) / colSums(current_population_male[c(1:7),-1]))*100000,3))
  assign(paste0(mort_cname[i],"_Female_mr_0034_age"),round((colSums(subset(get(mort_cname[i]),Sex=="食切")[c(1:7),-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))]) / colSums(current_population_female[c(1:7),-1]))*100000,3))
  male_mr_0034[i] <- paste0(mort_cname[i],"_Male_mr_0034_age")
  female_mr_0034[i] <- paste0(mort_cname[i],"_Female_mr_0034_age")
  # 3549
  assign(paste0(mort_cname[i],"_Male_mr_3549_age"),round((colSums(subset(get(mort_cname[i]),Sex=="害切")[c(8:10),-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))]) / colSums(current_population_male[c(8:10),-1]))*100000,1))
  assign(paste0(mort_cname[i],"_Female_mr_3549_age"),round((colSums(subset(get(mort_cname[i]),Sex=="食切")[c(8:10),-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))]) / colSums(current_population_female[c(8:10),-1]))*100000,3))
  male_mr_3549[i] <- paste0(mort_cname[i],"_Male_mr_3549_age")
  female_mr_3549[i] <- paste0(mort_cname[i],"_Female_mr_3549_age")
  
  # 5064
  assign(paste0(mort_cname[i],"_Male_mr_5064_age"),round((colSums(subset(get(mort_cname[i]),Sex=="害切")[c(11:13),-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))]) / colSums(current_population_male[c(11:13),-1]))*100000,3))
  assign(paste0(mort_cname[i],"_Female_mr_5064_age"),round((colSums(subset(get(mort_cname[i]),Sex=="食切")[c(11:13),-c(1:3,ncol(get(mort_cname[i]))-1,ncol(get(mort_cname[i])))]) / colSums(current_population_female[c(11:13),-1]))*100000,3))
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



