library(openxlsx)
library(INLA)
library(BAPC)
library(graphics)
library(magrittr)
library(stringr)
library(DescTools)
library(reshape)
library(ggplot2)
library(ggpubr)

male_cancer_name <- c("Whole Cancer", "Gastric Cancer", "Colorectal Cancer", "Liver Cancer", "Biliary Cancer",
                      "Pancreas Cancer","Lung Cancer","Prostate Cancer")

female_cancer_name <- c("Whole Cancer", "Gastric Cancer", "Colorectal Cancer", "Liver Cancer", "Biliary Cancer",
                        "Pancreas Cancer","Lung Cancer","Breast Cancer")

agegroups <- c("00-04","05-09","10-14","15-19","20-24","25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84","85+" )

age <- seq(2,87,by=5)
ly <- rep(2000,8)
uy <- rep(2019,8)
col <- c("grey20", "grey35", "grey50", "grey65", "grey75", "grey85")
lty <- c(rep(c(1,2), 3), rep(c(2,1),3))
path <- "/Users/jun/Library/CloudStorage/OneDrive-개인/APC trend data/total_death_BAPC.xlsx"

########

pop_men <- read.xlsx(xlsxFile="/Users/jun/Library/CloudStorage/OneDrive-개인/0902_meeting/2. Nordpred/canproj/pop_00_19.xlsx", sheet = 'Male', colNames = TRUE)
pop_women <- read.xlsx(xlsxFile="/Users/jun/Library/CloudStorage/OneDrive-개인/0902_meeting/2. Nordpred/canproj/pop_00_19.xlsx", sheet = 'Female', colNames = TRUE)

pop_predmen <- read.xlsx(xlsxFile="/Users/jun/Library/CloudStorage/OneDrive-개인/0902_meeting/2. Nordpred/canproj/predict_population.xlsx", sheet = 'Male', colNames = TRUE)
pop_predwomen <- read.xlsx(xlsxFile="/Users/jun/Library/CloudStorage/OneDrive-개인/0902_meeting/2. Nordpred/canproj/predict_population.xlsx", sheet = 'Female', colNames = TRUE)

pop_predmen<-as.data.frame(pop_predmen[c(-1,-22)])
pop_predwomen<-as.data.frame(pop_predwomen[c(-1,-22)])

## trans population data

trandat_pop <- function(dat, starty) { 
  n <- dim(dat)[1]/18
  outdate <- as.data.frame(matrix(dat$Population, ncol=n)) 
  colnames(outdate) <- starty : (starty + n -1) 
  return(outdate)
}

pop_men       <- as.data.frame(t(trandat_pop(pop_men, 2000)))
pop_women     <- as.data.frame(t(trandat_pop(pop_women, 2000)))

colnames(pop_men)   <-c(1:18)
colnames(pop_women) <-c(1:18)

pop_predmen   <- as.data.frame(t((pop_predmen)))
pop_predwomen <- as.data.frame(t((pop_predwomen)))

pop_men  <-rbind(pop_men,pop_predmen[c(1:10),])
pop_women<-rbind(pop_women,pop_predwomen[c(1:10),])



########

path <- "/Users/jun/Library/CloudStorage/OneDrive-개인/APC trend data/total_incidence_BAPC.xlsx"

whole.ca.m <-subset(read.xlsx(path, sheet = "Whole_Cancer"),Sex =='Male');  whole.ca.m <- whole.ca.m[,-c(1:4)]; whole.ca.m[is.na(whole.ca.m)] <- 0;  whole.ca.m <-as.data.frame(t(whole.ca.m))
whole.ca.w <-subset(read.xlsx(path, sheet = "Whole_Cancer"),Sex =='Female');whole.ca.w <- whole.ca.w[,-c(1:4)]; whole.ca.w[is.na(whole.ca.w)] <- 0;  whole.ca.w <-as.data.frame(t(whole.ca.w))

colnames(whole.ca.m)=c(1:18)
colnames(whole.ca.w)=c(1:18)

gastric.ca.m <-subset(read.xlsx(path, sheet = "Gastric_Cancer"),Sex =='Male');  gastric.ca.m <- gastric.ca.m[,-c(1:4)];  gastric.ca.m[is.na(gastric.ca.m)] <- 0; gastric.ca.m <-as.data.frame(t(gastric.ca.m))
gastric.ca.w <-subset(read.xlsx(path, sheet = "Gastric_Cancer"),Sex =='Female');gastric.ca.w <- gastric.ca.w[,-c(1:4)];  gastric.ca.w[is.na(gastric.ca.w)] <- 0; gastric.ca.w <-as.data.frame(t(gastric.ca.w))

colnames(gastric.ca.m)=c(1:18)
colnames(gastric.ca.w)=c(1:18)

colorectal.ca.m <-subset(read.xlsx(path, sheet = "Colorectal_Cancer"),Sex =='Male');  colorectal.ca.m <- colorectal.ca.m[,-c(1:4)]; colorectal.ca.m[is.na(colorectal.ca.m)] <- 0; colorectal.ca.m <-as.data.frame(t(colorectal.ca.m))
colorectal.ca.w <-subset(read.xlsx(path, sheet = "Colorectal_Cancer"),Sex =='Female');colorectal.ca.w <- colorectal.ca.w[,-c(1:4)]; colorectal.ca.w[is.na(colorectal.ca.w)] <- 0; colorectal.ca.w <-as.data.frame(t(colorectal.ca.w))

colnames(colorectal.ca.m)=c(1:18)
colnames(colorectal.ca.w)=c(1:18)

liver.ca.m <-subset(read.xlsx(path, sheet = "Liver_Cancer"),Sex =='Male');  liver.ca.m <- liver.ca.m[,-c(1:4)]; liver.ca.m[is.na(liver.ca.m)] <- 0; liver.ca.m <-as.data.frame(t(liver.ca.m))
liver.ca.w <-subset(read.xlsx(path, sheet = "Liver_Cancer"),Sex =='Female');liver.ca.w <- liver.ca.w[,-c(1:4)]; liver.ca.w[is.na(liver.ca.w)] <- 0; liver.ca.w <-as.data.frame(t(liver.ca.w))

colnames(liver.ca.m)=c(1:18)
colnames(liver.ca.w)=c(1:18)

biliary.ca.m <-subset(read.xlsx(path, sheet = "Biliary_Cancer"),Sex =='Male');  biliary.ca.m <- biliary.ca.m[,-c(1:4)];  biliary.ca.m[is.na(biliary.ca.m)] <- 0; biliary.ca.m <-as.data.frame(t(biliary.ca.m))
biliary.ca.w <-subset(read.xlsx(path, sheet = "Biliary_Cancer"),Sex =='Female');biliary.ca.w <- biliary.ca.w[,-c(1:4)];  biliary.ca.w[is.na(biliary.ca.w)] <- 0; biliary.ca.w <-as.data.frame(t(biliary.ca.w))

colnames(biliary.ca.m)=c(1:18)
colnames(biliary.ca.w)=c(1:18)

pancreas.ca.m <-subset(read.xlsx(path, sheet = "Pancreas_Cancer"),Sex =='Male');  pancreas.ca.m <- pancreas.ca.m[,-c(1:4)]; pancreas.ca.m[is.na(pancreas.ca.m)] <- 0; pancreas.ca.m <-as.data.frame(t(pancreas.ca.m))
pancreas.ca.w <-subset(read.xlsx(path, sheet = "Pancreas_Cancer"),Sex =='Female');pancreas.ca.w <- pancreas.ca.w[,-c(1:4)]; pancreas.ca.w[is.na(pancreas.ca.w)] <- 0; pancreas.ca.w <-as.data.frame(t(pancreas.ca.w))

colnames(pancreas.ca.m)=c(1:18)
colnames(pancreas.ca.w)=c(1:18)

lung.ca.m <-subset(read.xlsx(path, sheet = "Lung_Cancer"),Sex =='Male');  lung.ca.m <- lung.ca.m[,-c(1:4)];  lung.ca.m[is.na(lung.ca.m)] <- 0; lung.ca.m <-as.data.frame(t(lung.ca.m))
lung.ca.w <-subset(read.xlsx(path, sheet = "Lung_Cancer"),Sex =='Female');lung.ca.w <- lung.ca.w[,-c(1:4)];  lung.ca.w[is.na(lung.ca.w)] <- 0; lung.ca.w <-as.data.frame(t(lung.ca.w))

colnames(lung.ca.m)=c(1:18)
colnames(lung.ca.w)=c(1:18)

prostate.ca.m <-subset(read.xlsx(path, sheet = "SEX"),Sex =='Male');  prostate.ca.m <- prostate.ca.m[,-c(1:4)];  prostate.ca.m[is.na(prostate.ca.m)] <- 0; prostate.ca.m <-as.data.frame(t(prostate.ca.m))
breast.ca.w   <-subset(read.xlsx(path, sheet = "SEX"),Sex =='Female');breast.ca.w <- breast.ca.w[,-c(1:4)];      breast.ca.w[is.na(breast.ca.w)] <- 0;     breast.ca.w <-as.data.frame(t(breast.ca.w))

colnames(prostate.ca.m)=c(1:18)
colnames(breast.ca.w)=c(1:18)

######## 
# Death

########
path <- "/Users/jun/Library/CloudStorage/OneDrive-개인/APC trend data/total_death_BAPC.xlsx"

whole.ca.m_death <-subset(read.xlsx(path, sheet = "Whole_Cancer"),Sex =='Male');  whole.ca.m_death <- whole.ca.m_death[,-c(1:20)]; whole.ca.m_death[is.na(whole.ca.m_death)] <- 0; whole.ca.m_death <-whole.ca.m_death[,-21];  whole.ca.m_death <-as.data.frame(t(whole.ca.m_death))
whole.ca.w_death <-subset(read.xlsx(path, sheet = "Whole_Cancer"),Sex =='Female');whole.ca.w_death <- whole.ca.w_death[,-c(1:20)]; whole.ca.w_death[is.na(whole.ca.w_death)] <- 0; whole.ca.w_death  <-whole.ca.w_death[,-21]; whole.ca.w_death <-as.data.frame(t(whole.ca.w_death))

colnames(whole.ca.m_death)=c(1:18)
colnames(whole.ca.w_death)=c(1:18)

gastric.ca.m_death <-subset(read.xlsx(path, sheet = "Gastric_Cancer"),Sex =='Male');  gastric.ca.m_death <- gastric.ca.m_death[,-c(1:20)];  gastric.ca.m_death[is.na(gastric.ca.m_death)] <- 0; gastric.ca.m_death <-gastric.ca.m_death[,-21];gastric.ca.m_death <-as.data.frame(t(gastric.ca.m_death))
gastric.ca.w_death <-subset(read.xlsx(path, sheet = "Gastric_Cancer"),Sex =='Female');gastric.ca.w_death <- gastric.ca.w_death[,-c(1:20)];  gastric.ca.w_death[is.na(gastric.ca.w_death)] <- 0; gastric.ca.w_death <-gastric.ca.w_death[,-21];gastric.ca.w_death <-as.data.frame(t(gastric.ca.w_death))

colnames(gastric.ca.m_death)=c(1:18)
colnames(gastric.ca.w_death)=c(1:18)

colorectal.ca.m_death <-subset(read.xlsx(path, sheet = "Colorectal_Cancer"),Sex =='Male');  colorectal.ca.m_death <- colorectal.ca.m_death[,-c(1:20)]; colorectal.ca.m_death[is.na(colorectal.ca.m_death)] <- 0; colorectal.ca.m_death <-colorectal.ca.m_death[,-21]; colorectal.ca.m_death <-as.data.frame(t(colorectal.ca.m_death))                                            
colorectal.ca.w_death <-subset(read.xlsx(path, sheet = "Colorectal_Cancer"),Sex =='Female');colorectal.ca.w_death <- colorectal.ca.w_death[,-c(1:20)]; colorectal.ca.w_death[is.na(colorectal.ca.w_death)] <- 0; colorectal.ca.w_death <-colorectal.ca.w_death[,-21]; colorectal.ca.w_death <-as.data.frame(t(colorectal.ca.w_death))

colnames(colorectal.ca.m_death)=c(1:18)
colnames(colorectal.ca.w_death)=c(1:18)

liver.ca.m_death <-subset(read.xlsx(path, sheet = "Liver_Cancer"),Sex =='Male');  liver.ca.m_death <- liver.ca.m_death[,-c(1:20)]; liver.ca.m_death[is.na(liver.ca.m_death)] <- 0; liver.ca.m_death <-liver.ca.m_death[,-21];liver.ca.m_death <-as.data.frame(t(liver.ca.m_death))
liver.ca.w_death <-subset(read.xlsx(path, sheet = "Liver_Cancer"),Sex =='Female');liver.ca.w_death <- liver.ca.w_death[,-c(1:20)]; liver.ca.w_death[is.na(liver.ca.w_death)] <- 0; liver.ca.w_death <-liver.ca.w_death[,-21];liver.ca.w_death <-as.data.frame(t(liver.ca.w_death))

colnames(liver.ca.m_death)=c(1:18)
colnames(liver.ca.w_death)=c(1:18)

biliary.ca.m_death <-subset(read.xlsx(path, sheet = "Biliary_Cancer"),Sex =='Male');  biliary.ca.m_death <- biliary.ca.m_death[,-c(1:20)];  biliary.ca.m_death[is.na(biliary.ca.m_death)] <- 0; biliary.ca.m_death <-biliary.ca.m_death[,-21];biliary.ca.m_death <-as.data.frame(t(biliary.ca.m_death))
biliary.ca.w_death <-subset(read.xlsx(path, sheet = "Biliary_Cancer"),Sex =='Female');biliary.ca.w_death <- biliary.ca.w_death[,-c(1:20)];  biliary.ca.w_death[is.na(biliary.ca.w_death)] <- 0; biliary.ca.w_death <-biliary.ca.w_death[,-21];biliary.ca.w_death <-as.data.frame(t(biliary.ca.w_death))

colnames(biliary.ca.m_death)=c(1:18)
colnames(biliary.ca.w_death)=c(1:18)

pancreas.ca.m_death <-subset(read.xlsx(path, sheet = "Pancreas_Cancer"),Sex =='Male');  pancreas.ca.m_death <- pancreas.ca.m_death[,-c(1:20)]; pancreas.ca.m_death[is.na(pancreas.ca.m_death)] <- 0; pancreas.ca.m_death <-pancreas.ca.m_death[,-21];pancreas.ca.m_death <-as.data.frame(t(pancreas.ca.m_death))
pancreas.ca.w_death <-subset(read.xlsx(path, sheet = "Pancreas_Cancer"),Sex =='Female');pancreas.ca.w_death <- pancreas.ca.w_death[,-c(1:20)]; pancreas.ca.w_death[is.na(pancreas.ca.w_death)] <- 0; pancreas.ca.w_death <-pancreas.ca.w_death[,-21];pancreas.ca.w_death <-as.data.frame(t(pancreas.ca.w_death))

colnames(pancreas.ca.m_death)=c(1:18)
colnames(pancreas.ca.w_death)=c(1:18)

lung.ca.m_death <-subset(read.xlsx(path, sheet = "Lung_Cancer"),Sex =='Male');  lung.ca.m_death <- lung.ca.m_death[,-c(1:20)];  lung.ca.m_death[is.na(lung.ca.m_death)] <- 0; lung.ca.m_death <-lung.ca.m_death[,-21];lung.ca.m_death <-as.data.frame(t(lung.ca.m_death))
lung.ca.w_death <-subset(read.xlsx(path, sheet = "Lung_Cancer"),Sex =='Female');lung.ca.w_death <- lung.ca.w_death[,-c(1:20)];  lung.ca.w_death[is.na(lung.ca.w_death)] <- 0; lung.ca.w_death <-lung.ca.w_death[,-21];lung.ca.w_death <-as.data.frame(t(lung.ca.w_death))

colnames(lung.ca.m_death)=c(1:18)
colnames(lung.ca.w_death)=c(1:18)

prostate.ca.m_death <-subset(read.xlsx(path, sheet = "SEX"),Sex =='Male');  prostate.ca.m_death <- prostate.ca.m_death[,-c(1:20)];  prostate.ca.m_death[is.na(prostate.ca.m_death)] <- 0; prostate.ca.m_death <-prostate.ca.m_death[,-21];prostate.ca.m_death <-as.data.frame(t(prostate.ca.m_death))
breast.ca.w_death   <-subset(read.xlsx(path, sheet = "SEX"),Sex =='Female');breast.ca.w_death <- breast.ca.w_death[,-c(1:20)];      breast.ca.w_death[is.na(breast.ca.w_death)] <- 0; breast.ca.w_death <-breast.ca.w_death[,-21];        breast.ca.w_death <-as.data.frame(t(breast.ca.w_death))

colnames(prostate.ca.m_death)=c(1:18)
colnames(breast.ca.w_death)=c(1:18)

########################################################################

decomp_analysis_PAM <- function (count, population,reference,comparison){
  ## Reference
  N1     <- sum(population[,colnames(population) %like% as.character(reference)])
  m1     <- count[,colnames(population) %like% as.character(reference)]/population[,colnames(population) %like% as.character(reference)]
  s1     <- (population[,colnames(population) %like% as.character(reference)]/colSums(population)[colnames(population) %like% as.character(reference)])
  
  ## Comparison
  N2     <- sum(population[,colnames(population) %like% as.character(comparison)])
  m2     <- count[,colnames(population) %like% as.character(comparison)]/population[,colnames(population) %like% as.character(comparison)]
  s2     <- population[,colnames(population) %like% as.character(comparison)]/colSums(population)[colnames(population) %like% as.character(comparison)]
  
  
  M_p  <-c()
  M_a  <-c()
  M_m  <-c()
  I_pa <-c()
  I_pm <-c()
  I_am <-c()
  I_pam<-c()
  
  for ( i in 1:18){
    M_p[i]   <- sum((N2-N1)*s1[i]*m1[i])
    M_a[i]   <- sum(N1*(s2[i]-s1[i])*m1[i])
    M_m[i]   <- sum(N1*s1[i]*(m2[i]-m1[i]))
    I_pa[i]  <- sum((N2-N1)*(s2[i]-s1[i])*m1[i])
    I_pm[i]  <- sum((N2-N1)*s1[i]*(m2[i]-m1[i]))
    I_am[i]  <- sum(N1*(s2[i]-s1[i])*(m2[i]-m1[i]))
    I_pam[i] <- sum((N2-N1)*(s2[i]-s1[i])*(m2[i]-m1[i]))
  }
  
  M_p  <-sum(M_p)
  M_a  <-sum(M_a)
  M_m  <-sum(M_m)
  I_pa <-sum(I_pa)
  I_pm <-sum(I_pm)
  I_am <-sum(I_am)
  I_pam<-sum(I_pam)
  
  
  # Effect of changes in population size 
  P <-  (M_p + (1/2 * I_pm) + (1/2 * I_pa) + (1/3 * I_pam))
  
  # Effect of changes in  age structure
  A <-  (M_a + (1/2 * I_am) + (1/2 * I_pa) + (1/3 * I_pam))
  
  # Effect of changes in mortality 
  M <-  (M_m + (1/2 * I_pm) + (1/2 * I_am) + (1/3 * I_pam))
  
  method3        <- list(P,A,M)
  names(method3) <- c("P","A","M")
  return(method3)
}

########################################################################

cancer_pred_sample <- matrix(data = NA, nrow = 10, ncol = 18) %>% as.data.frame() 
rownames(cancer_pred_sample) <- seq(2020,2029,1)
colnames(cancer_pred_sample) <-  c(1:18)

#######################################

whole.ca.m      <- rbind(whole.ca.m, cancer_pred_sample)
gastric.ca.m    <- rbind(gastric.ca.m, cancer_pred_sample)
colorectal.ca.m <- rbind(colorectal.ca.m, cancer_pred_sample)
liver.ca.m      <- rbind(liver.ca.m, cancer_pred_sample)
biliary.ca.m    <- rbind(biliary.ca.m, cancer_pred_sample)
pancreas.ca.m   <- rbind(pancreas.ca.m, cancer_pred_sample)
lung.ca.m       <- rbind(lung.ca.m, cancer_pred_sample)
prostate.ca.m   <- rbind(prostate.ca.m, cancer_pred_sample)

whole.ca.w      <- rbind(whole.ca.w, cancer_pred_sample)
gastric.ca.w    <- rbind(gastric.ca.w, cancer_pred_sample)
colorectal.ca.w <- rbind(colorectal.ca.w, cancer_pred_sample)
liver.ca.w      <- rbind(liver.ca.w, cancer_pred_sample)
biliary.ca.w    <- rbind(biliary.ca.w, cancer_pred_sample)
pancreas.ca.w   <- rbind(pancreas.ca.w, cancer_pred_sample)
lung.ca.w       <- rbind(lung.ca.w, cancer_pred_sample)
breast.ca.w     <- rbind(breast.ca.w, cancer_pred_sample)

#######################################

whole.ca.m_death      <- rbind(whole.ca.m_death, cancer_pred_sample)
gastric.ca.m_death    <- rbind(gastric.ca.m_death, cancer_pred_sample)
colorectal.ca.m_death <- rbind(colorectal.ca.m_death, cancer_pred_sample)
liver.ca.m_death      <- rbind(liver.ca.m_death, cancer_pred_sample)
biliary.ca.m_death    <- rbind(biliary.ca.m_death, cancer_pred_sample)
pancreas.ca.m_death   <- rbind(pancreas.ca.m_death, cancer_pred_sample)
lung.ca.m_death       <- rbind(lung.ca.m_death, cancer_pred_sample)
prostate.ca.m_death   <- rbind(prostate.ca.m_death, cancer_pred_sample)

whole.ca.w_death      <- rbind(whole.ca.w_death, cancer_pred_sample)
gastric.ca.w_death    <- rbind(gastric.ca.w_death, cancer_pred_sample)
colorectal.ca.w_death <- rbind(colorectal.ca.w_death, cancer_pred_sample)
liver.ca.w_death      <- rbind(liver.ca.w_death, cancer_pred_sample)
biliary.ca.w_death    <- rbind(biliary.ca.w_death, cancer_pred_sample)
lung.ca.w_death       <- rbind(lung.ca.w_death, cancer_pred_sample)
pancreas.ca.w_death   <- rbind(pancreas.ca.w_death, cancer_pred_sample)
breast.ca.w_death     <- rbind(breast.ca.w_death, cancer_pred_sample)

########################################################################

whole.ca.m_data        <- APCList(whole.ca.m, pop_men, gf=5,agelab=agegroups)
gastric.ca.m_data      <- APCList(gastric.ca.m, pop_men, gf=5,agelab=agegroups)
colorectal.ca.m_data   <- APCList(colorectal.ca.m, pop_men, gf=5,agelab=agegroups)
liver.ca.m_data        <- APCList(liver.ca.m, pop_men, gf=5,agelab=agegroups)
biliary.ca.m_data      <- APCList(biliary.ca.m, pop_men, gf=5,agelab=agegroups)
pancreas.ca.m_data     <- APCList(pancreas.ca.m, pop_men, gf=5,agelab=agegroups)
lung.ca.m_data         <- APCList(lung.ca.m, pop_men, gf=5,agelab=agegroups)
prostate.ca.m_data     <- APCList(prostate.ca.m, pop_men, gf=5,agelab=agegroups)

whole.ca.w_data       <- APCList(whole.ca.w, pop_women, gf=5,agelab=agegroups)
gastric.ca.w_data     <- APCList(gastric.ca.w, pop_women, gf=5,agelab=agegroups)
colorectal.ca.w_data  <- APCList(colorectal.ca.w, pop_women, gf=5,agelab=agegroups)
liver.ca.w_data       <- APCList(liver.ca.w, pop_women, gf=5,agelab=agegroups)
biliary.ca.w_data     <- APCList(biliary.ca.w, pop_women, gf=5,agelab=agegroups)
pancreas.ca.w_data    <- APCList(pancreas.ca.w, pop_women, gf=5,agelab=agegroups)
lung.ca.w_data        <- APCList(lung.ca.w, pop_women, gf=5,agelab=agegroups)
breast.ca.w_data      <- APCList(breast.ca.w, pop_women, gf=5,agelab=agegroups)

########################################################################

whole.ca.m_death_data        <- APCList(whole.ca.m_death, pop_men, gf=5,agelab=agegroups)
gastric.ca.m_death_data      <- APCList(gastric.ca.m_death, pop_men, gf=5,agelab=agegroups)
colorectal.ca.m_death_data   <- APCList(colorectal.ca.m_death, pop_men, gf=5,agelab=agegroups)
liver.ca.m_death_data        <- APCList(liver.ca.m_death, pop_men, gf=5,agelab=agegroups)
biliary.ca.m_death_data      <- APCList(biliary.ca.m_death, pop_men, gf=5,agelab=agegroups)
pancreas.ca.m_death_data     <- APCList(pancreas.ca.m_death, pop_men, gf=5,agelab=agegroups)
lung.ca.m_death_data         <- APCList(lung.ca.m_death, pop_men, gf=5,agelab=agegroups)
prostate.ca.m_death_data     <- APCList(prostate.ca.m_death, pop_men, gf=5,agelab=agegroups)

whole.ca.w_death_data       <- APCList(whole.ca.w_death, pop_women, gf=5,agelab=agegroups)
gastric.ca.w_death_data     <- APCList(gastric.ca.w_death, pop_women, gf=5,agelab=agegroups)
colorectal.ca.w_death_data  <- APCList(colorectal.ca.w_death, pop_women, gf=5,agelab=agegroups)
liver.ca.w_death_data       <- APCList(liver.ca.w_death, pop_women, gf=5,agelab=agegroups)
biliary.ca.w_death_data     <- APCList(biliary.ca.w_death, pop_women, gf=5,agelab=agegroups)
pancreas.ca.w_death_data    <- APCList(pancreas.ca.w_death, pop_women, gf=5,agelab=agegroups)
lung.ca.w_death_data        <- APCList(lung.ca.w_death, pop_women, gf=5,agelab=agegroups)
breast.ca.w_death_data      <- APCList(breast.ca.w_death, pop_women, gf=5,agelab=agegroups)

########################################################################

whole.ca.m.res = BAPC(whole.ca.m_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)
gastric.ca.m.res = BAPC(gastric.ca.m_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)
colorectal.ca.m.res = BAPC(colorectal.ca.m_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)
liver.ca.m.res = BAPC(liver.ca.m_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)
biliary.ca.m.res = BAPC(biliary.ca.m_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)
pancreas.ca.m.res = BAPC(pancreas.ca.m_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)
lung.ca.m.res = BAPC(lung.ca.m_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)
prostate.ca.m.res = BAPC(prostate.ca.m_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)


whole.ca.w.res = BAPC(whole.ca.w_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)
gastric.ca.w.res = BAPC(gastric.ca.w_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)
colorectal.ca.w.res = BAPC(colorectal.ca.w_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)
liver.ca.w.res = BAPC(liver.ca.w_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)
biliary.ca.w.res = BAPC(biliary.ca.w_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)
pancreas.ca.w.res = BAPC(pancreas.ca.w_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)
lung.ca.w.res = BAPC(lung.ca.w_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)
breast.ca.w.res = BAPC(breast.ca.w_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)


# male
whole.ca.m.case <- agespec.proj(x = whole.ca.m.res) %>% as.data.frame(); whole.ca.m.case <- round(whole.ca.m.case[,colnames(whole.ca.m.case) %like% '%mean'],0);
colnames(whole.ca.m.case) <- agegroups; whole.ca.m.case <- t(whole.ca.m.case) %>% as.data.frame();

gastric.ca.m.case <- agespec.proj(x = gastric.ca.m.res) %>% as.data.frame(); gastric.ca.m.case <- round(gastric.ca.m.case[,colnames(gastric.ca.m.case) %like% '%mean'],0);
colnames(gastric.ca.m.case) <- agegroups; gastric.ca.m.case <- t(gastric.ca.m.case) %>% as.data.frame();

colorectal.ca.m.case <- agespec.proj(x = colorectal.ca.m.res) %>% as.data.frame(); colorectal.ca.m.case <- round(colorectal.ca.m.case[,colnames(colorectal.ca.m.case) %like% '%mean'],0);
colnames(colorectal.ca.m.case) <- agegroups; colorectal.ca.m.case <- t(colorectal.ca.m.case) %>% as.data.frame();

liver.ca.m.case <- agespec.proj(x = liver.ca.m.res) %>% as.data.frame(); liver.ca.m.case <- round(liver.ca.m.case[,colnames(liver.ca.m.case) %like% '%mean'],0);
colnames(liver.ca.m.case) <- agegroups; liver.ca.m.case <- t(liver.ca.m.case) %>% as.data.frame();

biliary.ca.m.case <- agespec.proj(x = biliary.ca.m.res) %>% as.data.frame(); biliary.ca.m.case <- round(biliary.ca.m.case[,colnames(biliary.ca.m.case) %like% '%mean'],0);
colnames(biliary.ca.m.case) <- agegroups; biliary.ca.m.case <- t(biliary.ca.m.case) %>% as.data.frame();

pancreas.ca.m.case <- agespec.proj(x = pancreas.ca.m.res) %>% as.data.frame(); pancreas.ca.m.case <- round(pancreas.ca.m.case[,colnames(pancreas.ca.m.case) %like% '%mean'],0);
colnames(pancreas.ca.m.case) <- agegroups; pancreas.ca.m.case <- t(pancreas.ca.m.case) %>% as.data.frame();

lung.ca.m.case <- agespec.proj(x = lung.ca.m.res) %>% as.data.frame(); lung.ca.m.case <- round(lung.ca.m.case[,colnames(lung.ca.m.case) %like% '%mean'],0);
colnames(lung.ca.m.case) <- agegroups; lung.ca.m.case <- t(lung.ca.m.case) %>% as.data.frame();

prostate.ca.m.case <- agespec.proj(x = prostate.ca.m.res) %>% as.data.frame(); prostate.ca.m.case <- round(prostate.ca.m.case[,colnames(prostate.ca.m.case) %like% '%mean'],0);
colnames(prostate.ca.m.case) <- agegroups; prostate.ca.m.case <- t(prostate.ca.m.case) %>% as.data.frame();

# female

whole.ca.w.case <- agespec.proj(x = whole.ca.w.res) %>% as.data.frame(); whole.ca.w.case <- round(whole.ca.w.case[,colnames(whole.ca.w.case) %like% '%mean'],0);
colnames(whole.ca.w.case) <- agegroups; whole.ca.w.case <- t(whole.ca.w.case) %>% as.data.frame();

gastric.ca.w.case <- agespec.proj(x = gastric.ca.w.res) %>% as.data.frame(); gastric.ca.w.case <- round(gastric.ca.w.case[,colnames(gastric.ca.w.case) %like% '%mean'],0);
colnames(gastric.ca.w.case) <- agegroups; gastric.ca.w.case <- t(gastric.ca.w.case) %>% as.data.frame();

colorectal.ca.w.case <- agespec.proj(x = colorectal.ca.w.res) %>% as.data.frame(); colorectal.ca.w.case <- round(colorectal.ca.w.case[,colnames(colorectal.ca.w.case) %like% '%mean'],0);
colnames(colorectal.ca.w.case) <- agegroups; colorectal.ca.w.case <- t(colorectal.ca.w.case) %>% as.data.frame();

liver.ca.w.case <- agespec.proj(x = liver.ca.w.res) %>% as.data.frame(); liver.ca.w.case <- round(liver.ca.w.case[,colnames(liver.ca.w.case) %like% '%mean'],0);
colnames(liver.ca.w.case) <- agegroups; liver.ca.w.case <- t(liver.ca.w.case) %>% as.data.frame();

biliary.ca.w.case <- agespec.proj(x = biliary.ca.w.res) %>% as.data.frame(); biliary.ca.w.case <- round(biliary.ca.w.case[,colnames(biliary.ca.w.case) %like% '%mean'],0);
colnames(biliary.ca.w.case) <- agegroups; biliary.ca.w.case <- t(biliary.ca.w.case) %>% as.data.frame();

pancreas.ca.w.case <- agespec.proj(x = pancreas.ca.w.res) %>% as.data.frame(); pancreas.ca.w.case <- round(pancreas.ca.w.case[,colnames(pancreas.ca.w.case) %like% '%mean'],0);
colnames(pancreas.ca.w.case) <- agegroups; pancreas.ca.w.case <- t(pancreas.ca.w.case) %>% as.data.frame();

lung.ca.w.case <- agespec.proj(x = lung.ca.w.res) %>% as.data.frame(); lung.ca.w.case <- round(lung.ca.w.case[,colnames(lung.ca.w.case) %like% '%mean'],0);
colnames(lung.ca.w.case) <- agegroups; lung.ca.w.case <- t(lung.ca.w.case) %>% as.data.frame();

breast.ca.w.case <- agespec.proj(x = breast.ca.w.res) %>% as.data.frame(); breast.ca.w.case <- round(breast.ca.w.case[,colnames(breast.ca.w.case) %like% '%mean'],0);
colnames(breast.ca.w.case) <- agegroups; breast.ca.w.case <- t(breast.ca.w.case) %>% as.data.frame();


##################################################################################################################################

whole.ca.m.res_death = BAPC(whole.ca.m_death_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)
gastric.ca.m.res_death = BAPC(gastric.ca.m_death_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)
colorectal.ca.m.res_death = BAPC(colorectal.ca.m_death_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)
liver.ca.m.res_death = BAPC(liver.ca.m_death_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)
biliary.ca.m.res_death = BAPC(biliary.ca.m_death_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)
pancreas.ca.m.res_death = BAPC(pancreas.ca.m_death_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)
lung.ca.m.res_death = BAPC(lung.ca.m_death_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)
prostate.ca.m.res_death = BAPC(prostate.ca.m_death_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)


whole.ca.w.res_death = BAPC(whole.ca.w_death_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)
gastric.ca.w.res_death = BAPC(gastric.ca.w_death_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)
colorectal.ca.w.res_death = BAPC(colorectal.ca.w_death_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)
liver.ca.w.res_death = BAPC(liver.ca.w_death_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)
biliary.ca.w.res_death = BAPC(biliary.ca.w_death_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)
pancreas.ca.w.res_death = BAPC(pancreas.ca.w_death_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)
lung.ca.w.res_death = BAPC(lung.ca.w_death_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)
breast.ca.w.res_death = BAPC(breast.ca.w_death_data, predict=list(npredict=10, retro=TRUE),stdweight=NULL)

##################################################################################################################################

whole.ca.m.death_case <- agespec.proj(x = whole.ca.m.res_death) %>% as.data.frame(); whole.ca.m.death_case <- round(whole.ca.m.death_case[,colnames(whole.ca.m.death_case) %like% '%mean'],0);
colnames(whole.ca.m.death_case) <- agegroups; whole.ca.m.death_case <- t(whole.ca.m.death_case) %>% as.data.frame();

gastric.ca.m.death_case <- agespec.proj(x = gastric.ca.m.res_death) %>% as.data.frame(); gastric.ca.m.death_case <- round(gastric.ca.m.death_case[,colnames(gastric.ca.m.death_case) %like% '%mean'],0);
colnames(gastric.ca.m.death_case) <- agegroups; gastric.ca.m.death_case <- t(gastric.ca.m.death_case) %>% as.data.frame();

colorectal.ca.m.death_case <- agespec.proj(x = colorectal.ca.m.res_death) %>% as.data.frame(); colorectal.ca.m.death_case <- round(colorectal.ca.m.death_case[,colnames(colorectal.ca.m.death_case) %like% '%mean'],0);
colnames(colorectal.ca.m.death_case) <- agegroups; colorectal.ca.m.death_case <- t(colorectal.ca.m.death_case) %>% as.data.frame();

liver.ca.m.death_case <- agespec.proj(x = liver.ca.m.res_death) %>% as.data.frame(); liver.ca.m.death_case <- round(liver.ca.m.death_case[,colnames(liver.ca.m.death_case) %like% '%mean'],0);
colnames(liver.ca.m.death_case) <- agegroups; liver.ca.m.death_case <- t(liver.ca.m.death_case) %>% as.data.frame();

biliary.ca.m.death_case <- agespec.proj(x = biliary.ca.m.res_death) %>% as.data.frame(); biliary.ca.m.death_case <- round(biliary.ca.m.death_case[,colnames(biliary.ca.m.death_case) %like% '%mean'],0);
colnames(biliary.ca.m.death_case) <- agegroups; biliary.ca.m.death_case <- t(biliary.ca.m.death_case) %>% as.data.frame();

pancreas.ca.m.death_case <- agespec.proj(x = pancreas.ca.m.res_death) %>% as.data.frame(); pancreas.ca.m.death_case <- round(pancreas.ca.m.death_case[,colnames(pancreas.ca.m.death_case) %like% '%mean'],0);
colnames(pancreas.ca.m.death_case) <- agegroups; pancreas.ca.m.death_case <- t(pancreas.ca.m.death_case) %>% as.data.frame();

lung.ca.m.death_case <- agespec.proj(x = lung.ca.m.res_death) %>% as.data.frame(); lung.ca.m.death_case <- round(lung.ca.m.death_case[,colnames(lung.ca.m.death_case) %like% '%mean'],0);
colnames(lung.ca.m.death_case) <- agegroups; lung.ca.m.death_case <- t(lung.ca.m.death_case) %>% as.data.frame();

prostate.ca.m.death_case <- agespec.proj(x = prostate.ca.m.res_death) %>% as.data.frame(); prostate.ca.m.death_case <- round(prostate.ca.m.death_case[,colnames(prostate.ca.m.death_case) %like% '%mean'],0);
colnames(prostate.ca.m.death_case) <- agegroups; prostate.ca.m.death_case <- t(prostate.ca.m.death_case) %>% as.data.frame();

# female
whole.ca.w.death_case <- agespec.proj(x = whole.ca.w.res_death) %>% as.data.frame(); whole.ca.w.death_case <- round(whole.ca.w.death_case[,colnames(whole.ca.w.death_case) %like% '%mean'],0);
colnames(whole.ca.w.death_case) <- agegroups; whole.ca.w.death_case <- t(whole.ca.w.death_case) %>% as.data.frame();

gastric.ca.w.death_case <- agespec.proj(x = gastric.ca.w.res_death) %>% as.data.frame(); gastric.ca.w.death_case <- round(gastric.ca.w.death_case[,colnames(gastric.ca.w.death_case) %like% '%mean'],0);
colnames(gastric.ca.w.death_case) <- agegroups; gastric.ca.w.death_case <- t(gastric.ca.w.death_case) %>% as.data.frame();

colorectal.ca.w.death_case <- agespec.proj(x = colorectal.ca.w.res_death) %>% as.data.frame(); colorectal.ca.w.death_case <- round(colorectal.ca.w.death_case[,colnames(colorectal.ca.w.death_case) %like% '%mean'],0);
colnames(colorectal.ca.w.death_case) <- agegroups; colorectal.ca.w.death_case <- t(colorectal.ca.w.death_case) %>% as.data.frame();

liver.ca.w.death_case <- agespec.proj(x = liver.ca.w.res_death) %>% as.data.frame(); liver.ca.w.death_case <- round(liver.ca.w.death_case[,colnames(liver.ca.w.death_case) %like% '%mean'],0);
colnames(liver.ca.w.death_case) <- agegroups; liver.ca.w.death_case <- t(liver.ca.w.death_case) %>% as.data.frame();

biliary.ca.w.death_case <- agespec.proj(x = biliary.ca.w.res_death) %>% as.data.frame(); biliary.ca.w.death_case <- round(biliary.ca.w.death_case[,colnames(biliary.ca.w.death_case) %like% '%mean'],0);
colnames(biliary.ca.w.death_case) <- agegroups; biliary.ca.w.death_case <- t(biliary.ca.w.death_case) %>% as.data.frame();

pancreas.ca.w.death_case <- agespec.proj(x = pancreas.ca.w.res_death) %>% as.data.frame(); pancreas.ca.w.death_case <- round(pancreas.ca.w.death_case[,colnames(pancreas.ca.w.death_case) %like% '%mean'],0);
colnames(pancreas.ca.w.death_case) <- agegroups; pancreas.ca.w.death_case <- t(pancreas.ca.w.death_case) %>% as.data.frame();

lung.ca.w.death_case <- agespec.proj(x = lung.ca.w.res_death) %>% as.data.frame(); lung.ca.w.death_case <- round(lung.ca.w.death_case[,colnames(lung.ca.w.death_case) %like% '%mean'],0);
colnames(lung.ca.w.death_case) <- agegroups; lung.ca.w.death_case <- t(lung.ca.w.death_case) %>% as.data.frame();

breast.ca.w.death_case <- agespec.proj(x = breast.ca.w.res_death) %>% as.data.frame(); breast.ca.w.death_case <- round(breast.ca.w.death_case[,colnames(breast.ca.w.death_case) %like% '%mean'],0);
colnames(breast.ca.w.death_case) <- agegroups; breast.ca.w.death_case <- t(breast.ca.w.death_case) %>% as.data.frame()
##################################################################################################################################

pop_men   <- t(pop_men)
pop_women <- t(pop_women)

##################################################################################################################################

# PAF, 65+ incidence/mortality, ASR

PAF_M  <-rep(0,16)
PAF_W  <-rep(0,16)
abm    <-rep(0,16)
abw    <-rep(0,16)
crm    <-rep(0,16)
crw    <-rep(0,16)
asr_m  <-rep(0,16)
asr_w  <-rep(0,16)

automata <-function(method,estimated){
  male_cancer_name   <- c("whole", "gastric", "colorectal", "liver", "biliary","pancreas","lung","prostate")
  female_cancer_name <- c("whole", "gastric", "colorectal", "liver", "biliary","pancreas","lung","breast")
  k<-1
  if (method == 1){
    for (j in 1:8){
      I_tm     <- as.vector(colSums(get(paste0(male_cancer_name[j],".ca.m.case"))[,c(6,estimated)])/colSums(pop_men[,c(6,estimated)])*100000)
      I_um     <- as.vector(colSums(get(paste0(male_cancer_name[j],".ca.m.case"))[c(1:13),c(6,estimated)])/colSums(pop_men[c(1:13),c(6,estimated)])*100000)
      abm[k]   <- as.vector(colSums(get(paste0(male_cancer_name[j],".ca.m.case"))[c(14:18),c(6,estimated)])/colSums(pop_men[c(14:18),c(6,estimated)])*100000)[1]
      abm[k+1] <- as.vector(colSums(get(paste0(male_cancer_name[j],".ca.m.case"))[c(14:18),c(6,estimated)])/colSums(pop_men[c(14:18),c(6,estimated)])*100000)[2]
      
      I_tw     <- as.vector(colSums(get(paste0(female_cancer_name[j],".ca.w.case"))[,c(6,estimated)])/colSums(pop_women[,c(6,estimated)])*100000)
      I_uw     <- as.vector(colSums(get(paste0(female_cancer_name[j],".ca.w.case"))[c(1:13),c(6,estimated)])/colSums(pop_women[c(1:13),c(6,estimated)])*100000)
      abw[k]   <- as.vector(colSums(get(paste0(female_cancer_name[j],".ca.w.case"))[c(14:18),c(6,estimated)])/colSums(pop_women[c(14:18),c(6,estimated)])*100000)[1]
      abw[k+1] <- as.vector(colSums(get(paste0(female_cancer_name[j],".ca.w.case"))[c(14:18),c(6,estimated)])/colSums(pop_women[c(14:18),c(6,estimated)])*100000)[2]
      
      crm[k]   <- I_tm[1]
      crm[k+1] <- I_tm[2]
      
      crw[k]   <- I_tw[1]
      crw[k+1] <- I_tw[2]
      
      asr_m[k]    <- (as.vector(colSums(get(paste0(male_cancer_name[j],".ca.m.case"))[,c(6,estimated)]/pop_men[,c(6,estimated)]* pop_men[,6]))/sum(pop_men[,6])*100000)[1]
      asr_m[k+1]  <- (as.vector(colSums(get(paste0(male_cancer_name[j],".ca.m.case"))[,c(6,estimated)]/pop_men[,c(6,estimated)]* pop_men[,6]))/sum(pop_men[,6])*100000)[2]
      
      asr_w[k]    <- (as.vector(colSums(get(paste0(female_cancer_name[j],".ca.w.case"))[,c(6,estimated)]/pop_women[,c(6,estimated)]* pop_women[,6]))/sum(pop_women[,6])*100000)[1]
      asr_w[k+1]  <- (as.vector(colSums(get(paste0(female_cancer_name[j],".ca.w.case"))[,c(6,estimated)]/pop_women[,c(6,estimated)]* pop_women[,6]))/sum(pop_women[,6])*100000)[2]
      
      
      PAF_M[k]   <-round((I_tm-I_um)/I_tm*100,2)[1]
      PAF_M[k+1] <-round((I_tm-I_um)/I_tm*100,2)[2]
      
      PAF_W[k] <-round((I_tw-I_uw)/I_tw*100,2)[1]
      PAF_W[k+1] <-round((I_tw-I_uw)/I_tw*100,2)[2]
      
      k<-k+2
    }
  }
  else if(method == 2){
    for (j in 1:8){
      I_tm     <- as.vector(colSums(get(paste0(male_cancer_name[j],".ca.m.death_case"))[,c(6,estimated)])/colSums(pop_men[,c(6,estimated)])*100000)
      I_um     <- as.vector(colSums(get(paste0(male_cancer_name[j],".ca.m.death_case"))[c(1:13),c(6,estimated)])/colSums(pop_men[c(1:13),c(6,estimated)])*100000)
      abm[k]   <- as.vector(colSums(get(paste0(male_cancer_name[j],".ca.m.death_case"))[c(14:18),c(6,estimated)])/colSums(pop_men[c(14:18),c(6,estimated)])*100000)[1]
      abm[k+1] <- as.vector(colSums(get(paste0(male_cancer_name[j],".ca.m.death_case"))[c(14:18),c(6,estimated)])/colSums(pop_men[c(14:18),c(6,estimated)])*100000)[2]
      
      I_tw     <- as.vector(colSums(get(paste0(female_cancer_name[j],".ca.w.death_case"))[,c(6,estimated)])/colSums(pop_women[,c(6,estimated)])*100000)
      I_uw     <- as.vector(colSums(get(paste0(female_cancer_name[j],".ca.w.death_case"))[c(1:13),c(6,estimated)])/colSums(pop_women[c(1:13),c(6,estimated)])*100000)
      abw[k]   <- as.vector(colSums(get(paste0(female_cancer_name[j],".ca.w.death_case"))[c(14:18),c(6,estimated)])/colSums(pop_women[c(14:18),c(6,estimated)])*100000)[1]
      abw[k+1] <- as.vector(colSums(get(paste0(female_cancer_name[j],".ca.w.death_case"))[c(14:18),c(6,estimated)])/colSums(pop_women[c(14:18),c(6,estimated)])*100000)[2]
      
      crm[k]   <- I_tm[1]
      crm[k+1] <- I_tm[2]
      
      crw[k]   <- I_tw[1]
      crw[k+1] <- I_tw[2]
      
      asr_m[k]    <- (as.vector(colSums(get(paste0(male_cancer_name[j],".ca.m.death_case"))[,c(6,estimated)]/pop_men[,c(6,estimated)]* pop_men[,6]))/sum(pop_men[,6])*100000)[1]
      asr_m[k+1]  <- (as.vector(colSums(get(paste0(male_cancer_name[j],".ca.m.death_case"))[,c(6,estimated)]/pop_men[,c(6,estimated)]* pop_men[,6]))/sum(pop_men[,6])*100000)[2]
      
      asr_w[k]    <- (as.vector(colSums(get(paste0(female_cancer_name[j],".ca.w.death_case"))[,c(6,estimated)]/pop_women[,c(6,estimated)]* pop_women[,6]))/sum(pop_women[,6])*100000)[1]
      asr_w[k+1]  <- (as.vector(colSums(get(paste0(female_cancer_name[j],".ca.w.death_case"))[,c(6,estimated)]/pop_women[,c(6,estimated)]* pop_women[,6]))/sum(pop_women[,6])*100000)[2]
      
      
      PAF_M[k]   <-round((I_tm-I_um)/I_tm*100,2)[1]
      PAF_M[k+1] <-round((I_tm-I_um)/I_tm*100,2)[2]
      
      PAF_W[k] <-round((I_tw-I_uw)/I_tw*100,2)[1]
      PAF_W[k+1] <-round((I_tw-I_uw)/I_tw*100,2)[2]
      
      k<-k+2
    }
    
  }
  
  save <-list(PAF_M,PAF_W,abm,abw,crm,crw,asr_m,asr_w)
  names(save) <- c("PAF_M","PAF_W","abm","abw","crm","crw","asr_m","asr_w")
  return(save)
}

automata(method=1,estimated=25) # incidence, comparison = 2024
automata(method=1,estimated=30) # incidence, comparison = 2029

automata(method=2,estimated=20) # mortality, comparison = 2024
automata(method=2,estimated=25) # mortality, comparison = 2024
automata(method=2,estimated=30) # mortality, comparison = 2029






##################################################################################################################################
attributable_plt <- function(Sex,Method) {
  male_cancer_name   <- c("whole", "gastric", "colorectal", "liver", "biliary","pancreas","lung","prostate")
  female_cancer_name <- c("whole", "gastric", "colorectal", "liver", "biliary","pancreas","lung","breast")
  
  if (Sex == 1 & Method == 1) {
    for (j in 1:length(male_cancer_name)){
      P_year <-c()
      A_year <-c()
      M_year <-c()
      net_change<-c()
      
      for (i in 1:30){
        case               <- (as.numeric(as.vector(unlist(decomp_analysis_PAM(get(paste0(male_cancer_name[j],".ca.m.case")),pop_men,2005,1999+i)))))
        P_year[i]          <- case[1]
        A_year[i]          <- case[2]
        M_year[i]          <- case[3]
        
        net_change[i]         <-sum(get(paste0(male_cancer_name[j],".ca.m.case"))[,i])-sum(get(paste0(male_cancer_name[j],".ca.m.case"))[,6])
      }
      
      calender_year <-c(2000:2029)
      change <- as.data.frame(rbind(cbind(calender_year,P_year,('Population Growth')),cbind(calender_year,A_year,c('Population Aging')),cbind(calender_year,M_year,c('ASIR')),cbind(calender_year,net_change,c('Net Change'))))
      colnames(change) <-  c("Year","Change","Group") 
      change[,c(2)] <- sapply(change[,c(2)],as.numeric)
      assign(paste0("plot",j),ggplot(data = change, aes(x = Year, y = Change,group = Group,colour = Group)) + geom_line() + 
               scale_x_discrete(breaks = seq(2000, 2029, 1)) + geom_hline(yintercept = 0, color = 'black', linetype = 2)+ 
               theme_bw() + theme(legend.position = "top",text=element_text(size=10),axis.text.x=element_text(angle=90, hjust=1)) + 
               labs(title=paste(male_cancer_name[j]," cancer"),x = 'Calender Year',y = "Total Incidence (Case)'"))
    } 
  }
  else if (Sex == 1 & Method == 2) {
    for (j in 1:length(male_cancer_name)){
      P_year <-c()
      A_year <-c()
      M_year <-c()
      net_change<-c()
      
      for (i in 1:30){
        case <- (as.numeric(as.vector(unlist(decomp_analysis_PAM(get(paste0(male_cancer_name[j],".ca.m.death_case")),pop_men,2005,1999+i)))))
        P_year[i]          <- case[1]
        A_year[i]          <- case[2]
        M_year[i]          <- case[3]
        
        net_change[i]         <-sum(get(paste0(male_cancer_name[j],".ca.m.death_case"))[,i])-sum(get(paste0(male_cancer_name[j],".ca.m.death_case"))[,6])
      }
      
      calender_year <-c(2000:2029)
      change <- as.data.frame(rbind(cbind(calender_year,P_year,('Population Growth')),cbind(calender_year,A_year,c('Population Aging')),cbind(calender_year,M_year,c('ASMR')),cbind(calender_year,net_change,c('Net Change'))))      
      colnames(change) <-  c("Year","Change","Group") 
      change[,c(2)] <- sapply(change[,c(2)],as.numeric)
      assign(paste0("plot",j),ggplot(data = change, aes(x = Year, y = Change,group = Group,colour = Group)) + geom_line() + 
               scale_x_discrete(breaks = seq(2000, 2029, 1)) + geom_hline(yintercept = 0, color = 'black', linetype = 2)+ 
               theme_bw() + theme(legend.position = "top",text=element_text(size=10),axis.text.x=element_text(angle=90, hjust=1)) + 
               labs(title=paste(male_cancer_name[j]," cancer"),x = 'Calender Year',y = 'Total Death (Case)'))
    }
  }
  else if (Sex == 2 & Method == 1) {
    for (j in 1:length(female_cancer_name)){
      P_year <-c()
      A_year <-c()
      M_year <-c()
      net_change<-c()
      
      for (i in 1:30){
        case <- (as.numeric(as.vector(unlist(decomp_analysis_PAM(get(paste0(female_cancer_name[j],".ca.w.case")),pop_women,2005,1999+i)))))
        P_year[i]          <- case[1]
        A_year[i]          <- case[2]
        M_year[i]          <- case[3]
        
        net_change[i]         <-sum(get(paste0(female_cancer_name[j],".ca.w.case"))[,i])-sum(get(paste0(female_cancer_name[j],".ca.w.case"))[,6])
      }
      calender_year <-c(2000:2029)
      change <- as.data.frame(rbind(cbind(calender_year,P_year,('Population Growth')),cbind(calender_year,A_year,c('Population Aging')),cbind(calender_year,M_year,c('ASIR')),cbind(calender_year,net_change,c('Net Change'))))
      colnames(change) <-  c("Year","Change","Group") 
      change[,c(2)] <- sapply(change[,c(2)],as.numeric)
      assign(paste0("plot",j),ggplot(data = change, aes(x = Year, y = Change,group = Group,colour = Group)) + geom_line() + 
               scale_x_discrete(breaks = seq(2000, 2029, 1)) + geom_hline(yintercept = 0, color = 'black', linetype = 2)+ 
               theme_bw() + theme(legend.position = "top",text=element_text(size=10),axis.text.x=element_text(angle=90, hjust=1)) + 
               labs(title=paste(female_cancer_name[j]," cancer"),x = 'Calender Year',y = 'Total Incidence (Case)'))
    }
  }
  else if (Sex == 2 & Method == 2) {
    for (j in 1:length(female_cancer_name)){
      P_year <-c()
      A_year <-c()
      M_year <-c()
      net_change<-c()
      
      for (i in 1:30){
        case <- (as.numeric(as.vector(unlist(decomp_analysis_PAM(get(paste0(female_cancer_name[j],".ca.w.death_case")),pop_women,2005,1999+i)))))
        P_year[i]          <- case[1]
        A_year[i]          <- case[2]
        M_year[i]          <- case[3]
        
        net_change[i]         <-sum(get(paste0(female_cancer_name[j],".ca.w.death_case"))[,i])-sum(get(paste0(female_cancer_name[j],".ca.w.death_case"))[,6])
      }
      
      calender_year <-c(2000:2029)
      change <- as.data.frame(rbind(cbind(calender_year,P_year,('Population Growth')),cbind(calender_year,A_year,c('Population Aging')),cbind(calender_year,M_year,c('ASMR')),cbind(calender_year,net_change,c('Net Change'))))      
      colnames(change) <-  c("Year","Change","Group") 
      change[,c(2)] <- sapply(change[,c(2)],as.numeric)
      assign(paste0("plot",j),ggplot(data = change, aes(x = Year, y = Change,group = Group,colour = Group)) + geom_line() + 
               scale_x_discrete(breaks = seq(2000, 2029, 1)) + geom_hline(yintercept = 0, color = 'black', linetype = 2)+ 
               theme_bw() + theme(legend.position = "top",text=element_text(size=10),axis.text.x=element_text(angle=90, hjust=1)) + 
               labs(title=paste(female_cancer_name[j]," cancer"),x = 'Calender Year',y = 'Total Death (Case)'))
      
    }
  }
  plt<-ggarrange(plot1, plot2,plot3,plot4,plot5,plot6,plot7,plot8,labels = c("A","B","C","D","E","F","G","H"),ncol = 3, nrow = 3)
  return(plt)
}

# Male, Incidence
attributable_plt(1,1)
# Male, Death
attributable_plt(1,2)
# Female, Incidence
attributable_plt(2,1)
# Female, Death
attributable_plt(2,2)


##################################################################################################################################

### Total attr risk
# Sex 1: Male, 2: Female, Mehtod 1: Incidene, 2: Death

attributable_plt <- function(Sex,Method) {
  male_cancer_name   <- c("whole", "gastric", "colorectal", "liver", "biliary","pancreas","lung","prostate")
  female_cancer_name <- c("whole", "gastric", "colorectal", "liver", "biliary","pancreas","lung","breast")
  
  if (Sex == 1 & Method == 1) {
    for (j in 1:length(male_cancer_name)){
      P_year <-c()
      A_year <-c()
      M_year <-c()
      net_change<-c()
      
      for (i in 1:30){
        case               <- as.numeric(as.vector(unlist(decomp_analysis_PAM(get(paste0(male_cancer_name[j],".ca.m.case")),pop_men,2005,1999+i)))) / sum(pop_men[,6])*100000
        P_year[i]          <- case[1]
        A_year[i]          <- case[2]
        M_year[i]          <- case[3]
        net_change[i]      <-(sum(get(paste0(male_cancer_name[j],".ca.m.case"))[,i])/sum(pop_men[,6])*100000) - (sum(get(paste0(male_cancer_name[j],".ca.m.case"))[,6])/sum(pop_men[,6])*100000)
      }
      
      calender_year <-c(2000:2029)
      change <- as.data.frame(rbind(cbind(calender_year,P_year,('Population Growth')),cbind(calender_year,A_year,c('Population Aging')),cbind(calender_year,M_year,c('ASIR')),cbind(calender_year,net_change,c('Net Change'))))
      colnames(change) <-  c("Year","Change","Group") 
      change[,c(2)] <- sapply(change[,c(2)],as.numeric)
      assign(paste0("plot",j),ggplot(data = change, aes(x = Year, y = Change,group = Group,colour = Group)) + geom_line() + 
               scale_x_discrete(breaks = seq(2000, 2029, 1)) + geom_hline(yintercept = 0, color = 'black', linetype = 2)+ 
               theme_bw() + theme(legend.position = "top",text=element_text(size=10),axis.text.x=element_text(angle=90, hjust=1)) + 
               labs(title=paste(male_cancer_name[j]," cancer"),x = 'Calender Year',y = "Attributed Proportion Incidence (%)"))
    } 
  }
  else if (Sex == 1 & Method == 2) {
    for (j in 1:length(male_cancer_name)){
      P_year <-c()
      A_year <-c()
      M_year <-c()
      net_change<-c()
      
      for (i in 1:30){
        case               <- as.numeric(as.vector(unlist(decomp_analysis_PAM(get(paste0(male_cancer_name[j],".ca.m.death_case")),pop_men,2005,1999+i)))) / sum(pop_men[,6])*100000
        P_year[i]          <- case[1]
        A_year[i]          <- case[2]
        M_year[i]          <- case[3]
        net_change[i]      <-(sum(get(paste0(male_cancer_name[j],".ca.m.death_case"))[,i])/sum(pop_men[,6])*100000) - (sum(get(paste0(male_cancer_name[j],".ca.m.death_case"))[,6])/sum(pop_men[,6])*100000)
      }
      
      calender_year <-c(2000:2029)
      change <- as.data.frame(rbind(cbind(calender_year,P_year,('Population Growth')),cbind(calender_year,A_year,c('Population Aging')),cbind(calender_year,M_year,c('ASMR')),cbind(calender_year,net_change,c('Net Change'))))      
      colnames(change) <-  c("Year","Change","Group") 
      change[,c(2)] <- sapply(change[,c(2)],as.numeric)
      assign(paste0("plot",j),ggplot(data = change, aes(x = Year, y = Change,group = Group,colour = Group)) + geom_line() + 
               scale_x_discrete(breaks = seq(2000, 2029, 1)) + geom_hline(yintercept = 0, color = 'black', linetype = 2)+ 
               theme_bw() + theme(legend.position = "top",text=element_text(size=10),axis.text.x=element_text(angle=90, hjust=1)) + 
               labs(title=paste(male_cancer_name[j]," cancer"),x = 'Calender Year',y = 'Attributed Proportion Death (%)'))
    }
  }
  else if (Sex == 2 & Method == 1) {
    for (j in 1:length(female_cancer_name)){
      P_year <-c()
      A_year <-c()
      M_year <-c()
      net_change<-c()
      
      for (i in 1:30){
        case  <- as.numeric(as.vector(unlist(decomp_analysis_PAM(get(paste0(female_cancer_name[j],".ca.w.case")),pop_women,2005,1999+i)))) / sum(pop_women[,6])*100000
        P_year[i]          <- case[1]
        A_year[i]          <- case[2]
        M_year[i]          <- case[3]
        net_change[i]      <-(sum(get(paste0(female_cancer_name[j],".ca.w.case"))[,i])/sum(pop_women[,6])*100000) - (sum(get(paste0(female_cancer_name[j],".ca.w.case"))[,6])/sum(pop_women[,6])*100000)
      }
      calender_year <-c(2000:2029)
      change <- as.data.frame(rbind(cbind(calender_year,P_year,('Population Growth')),cbind(calender_year,A_year,c('Population Aging')),cbind(calender_year,M_year,c('ASIR')),cbind(calender_year,net_change,c('Net Change'))))
      colnames(change) <-  c("Year","Change","Group") 
      change[,c(2)] <- sapply(change[,c(2)],as.numeric)
      assign(paste0("plot",j),ggplot(data = change, aes(x = Year, y = Change,group = Group,colour = Group)) + geom_line() + 
               scale_x_discrete(breaks = seq(2000, 2029, 1)) + geom_hline(yintercept = 0, color = 'black', linetype = 2)+ 
               theme_bw() + theme(legend.position = "top",text=element_text(size=10),axis.text.x=element_text(angle=90, hjust=1)) + 
               labs(title=paste(female_cancer_name[j]," cancer"),x = 'Calender Year',y = 'Attributed Proportion Incidence (%)'))
    }
  }
  else if (Sex == 2 & Method == 2) {
    for (j in 1:length(female_cancer_name)){
      P_year <-c()
      A_year <-c()
      M_year <-c()
      net_change<-c()
      
      for (i in 1:30){
        case               <- as.numeric(as.vector(unlist(decomp_analysis_PAM(get(paste0(female_cancer_name[j],".ca.w.death_case")),pop_women,2005,1999+i)))) / sum(pop_women[,6])*100000
        P_year[i]          <- case[1]
        A_year[i]          <- case[2]
        M_year[i]          <- case[3]
        net_change[i]      <-(sum(get(paste0(female_cancer_name[j],".ca.w.death_case"))[,i])/sum(pop_women[,6])*100000) - (sum(get(paste0(female_cancer_name[j],".ca.w.death_case"))[,6])/sum(pop_women[,6])*100000)
      }
      
      calender_year <-c(2000:2029)
      change <- as.data.frame(rbind(cbind(calender_year,P_year,('Population Growth')),cbind(calender_year,A_year,c('Population Aging')),cbind(calender_year,M_year,c('ASMR')),cbind(calender_year,net_change,c('Net Change'))))      
      colnames(change) <-  c("Year","Change","Group") 
      change[,c(2)] <- sapply(change[,c(2)],as.numeric)
      assign(paste0("plot",j),ggplot(data = change, aes(x = Year, y = Change,group = Group,colour = Group)) + geom_line() + 
               scale_x_discrete(breaks = seq(2000, 2029, 1)) + geom_hline(yintercept = 0, color = 'black', linetype = 2)+ 
               theme_bw() + theme(legend.position = "top",text=element_text(size=10),axis.text.x=element_text(angle=90, hjust=1)) + 
               labs(title=paste(female_cancer_name[j]," cancer"),x = 'Calender Year',y = 'Attributed Proportion Death (%)'))
      
    }
  }
  plt<-ggarrange(plot1, plot2,plot3,plot4,plot5,plot6,plot7,plot8,labels = c("A","B","C","D","E","F","G","H"),ncol = 3, nrow = 3)
  return(plt)
}

# Male, Incidence
attributable_plt(1,1)
# Male, Death
attributable_plt(1,2)
# Female, Incidence
attributable_plt(2,1)
# Female, Death
attributable_plt(2,2)








