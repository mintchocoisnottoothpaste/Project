# load data

load(file="/Users/jun/Library/Mobile Documents/com~apple~CloudDocs/output/data/lung_data_original.RData")

library("openxlsx") 
library("MatchIt") 
library("ggplot2")
library("survival")
library("survminer")
library("gridExtra")

lung_data2 <- lung_data[,c("No","Age2", "Sex", "ECOG3", "PDL1_IHC", "Smoking2", "Histology", "Previous_Treatment3", "Regimen_4", "Season2","pd_60", "pfs_60","os_60","death_60")]
colnames(lung_data2) <-c("No","Age","Sex","ECOG","PDL1_expression","Smoking","Histology","Previous_chemotherapy","Anti_PDL1_inhibitors","Season","pd_60", "pfs_60","os_60","death_60")

levels(lung_data2$Age)[levels(lung_data2$Age)=="1"] = "0. <65"
levels(lung_data2$Age)[levels(lung_data2$Age)=="2"] = "1. ≥65"

levels(lung_data2$Sex)[levels(lung_data2$Sex)=="1"] = "0. Male"
levels(lung_data2$Sex)[levels(lung_data2$Sex)=="2"] = "1. Female"

levels(lung_data2$ECOG)[levels(lung_data2$ECOG)=="1"] = "1. Status 0"
levels(lung_data2$ECOG)[levels(lung_data2$ECOG)=="2"] = "2. Status 1"
levels(lung_data2$ECOG)[levels(lung_data2$ECOG)=="3"] = "3. Status 2≥"

levels(lung_data2$PDL1_expression)[levels(lung_data2$PDL1_expression)=="1"] = "1. <5%"
levels(lung_data2$PDL1_expression)[levels(lung_data2$PDL1_expression)=="2"] = "2. ≥5%"

levels(lung_data2$Smoking)[levels(lung_data2$Smoking)=="1"] = "0. Never"
levels(lung_data2$Smoking)[levels(lung_data2$Smoking)=="2"] = "1. Smoker"

levels(lung_data2$Histology)[levels(lung_data2$Histology)=="1"] = "1. Adenocarcinoma"
levels(lung_data2$Histology)[levels(lung_data2$Histology)=="2"] = "2. Squamous cell carcinoma"
levels(lung_data2$Histology)[levels(lung_data2$Histology)=="3"] = "3. Other NSCLC"
levels(lung_data2$Histology)[levels(lung_data2$Histology)=="4"] = "4. Small cell lung cancer"

levels(lung_data2$Previous_chemotherapy)[levels(lung_data2$Previous_chemotherapy)=="1"] = "1. 0"
levels(lung_data2$Previous_chemotherapy)[levels(lung_data2$Previous_chemotherapy)=="2"] = "2. 1-2"
levels(lung_data2$Previous_chemotherapy)[levels(lung_data2$Previous_chemotherapy)=="3"] = "3. ≥3"


levels(lung_data2$Anti_PDL1_inhibitors)[levels(lung_data2$Anti_PDL1_inhibitors)=="1"] = "1. Pembrolizumab"
levels(lung_data2$Anti_PDL1_inhibitors)[levels(lung_data2$Anti_PDL1_inhibitors)=="2"] = "2. Nivolumab"
levels(lung_data2$Anti_PDL1_inhibitors)[levels(lung_data2$Anti_PDL1_inhibitors)=="3"] = "3. Atezolizumab"
levels(lung_data2$Anti_PDL1_inhibitors)[levels(lung_data2$Anti_PDL1_inhibitors)=="4"] = "4. Others"

levels(lung_data2$Season)[levels(lung_data2$Season)=="1"] = "1. Winter Season"
levels(lung_data2$Season)[levels(lung_data2$Season)=="0"] = "0. Other Season"


##################################################################################

res.cox1 <-coxph(Surv(pfs_60,pd_60==1) ~ Age + Sex + ECOG + PDL1_expression + Smoking + Histology + Previous_chemotherapy + Anti_PDL1_inhibitors + Season,data=lung_data2)
res.cox2 <-coxph(Surv(os_60,death_60==1) ~ Age + Sex + ECOG + PDL1_expression + Smoking + Histology + Previous_chemotherapy + Anti_PDL1_inhibitors + Season,data=lung_data2)

test.ph1 <- cox.zph(res.cox1) # ECOG, Regimen4, Global
test.ph2 <- cox.zph(res.cox2) # ECOG, Regimen4, Global


# 유의하지 않으면 가정 성립

ggcoxzph(test.ph1)
ggcoxzph(test.ph2)


ggcoxdiagnostics(res.cox1,type="schoenfeld",linear.prediction = TRUE)
ggcoxdiagnostics(res.cox2,type="schoenfeld",linear.prediction = TRUE)

##################################################################################






A1<-coxph(Surv(pfs_60,pd_60==1) ~ Age ,data=lung_data2)
A2<-coxph(Surv(pfs_60,pd_60==1) ~ Sex ,data=lung_data2)
A3<-coxph(Surv(pfs_60,pd_60==1) ~ ECOG ,data=lung_data2)
A4<-coxph(Surv(pfs_60,pd_60==1) ~ PDL1_expression ,data=lung_data2)
A5<-coxph(Surv(pfs_60,pd_60==1) ~ Smoking ,data=lung_data2)
A6<-coxph(Surv(pfs_60,pd_60==1) ~ Histology ,data=lung_data2)
A7<-coxph(Surv(pfs_60,pd_60==1) ~ Previous_chemotherapy ,data=lung_data2)
A8<-coxph(Surv(pfs_60,pd_60==1) ~ Anti_PDL1_inhibitors ,data=lung_data2)
A9<-coxph(Surv(pfs_60,pd_60==1) ~ Season ,data=lung_data2)


AA1<-cox.zph(A1,transform = 'log')
AA2<-cox.zph(A2,transform = 'log')
AA3<-cox.zph(A3,transform = 'log')
AA4<-cox.zph(A4,transform = 'log')
AA5<-cox.zph(A5,transform = 'log')
AA6<-cox.zph(A6,transform = 'log')
AA7<-cox.zph(A7,transform = 'log')
AA8<-cox.zph(A8,transform = 'log')
AA9<-cox.zph(A9,transform = 'log')

AAA1<-ggcoxzph(AA1)
AAA2<-ggcoxzph(AA2)
AAA3<-ggcoxzph(AA3)
AAA4<-ggcoxzph(AA4)
AAA5<-ggcoxzph(AA5)
AAA6<-ggcoxzph(AA6)
AAA7<-ggcoxzph(AA7)
AAA8<-ggcoxzph(AA8)
AAA9<-ggcoxzph(AA9)

ggcoxdiagnostics(A1,type="schoenfeld",linear.prediction = TRUE)
ggcoxdiagnostics(A2,type="schoenfeld",linear.prediction = TRUE)
ggcoxdiagnostics(A3,type="schoenfeld",linear.prediction = TRUE)
ggcoxdiagnostics(A4,type="schoenfeld",linear.prediction = TRUE)
ggcoxdiagnostics(A5,type="schoenfeld",linear.prediction = TRUE)
ggcoxdiagnostics(A6,type="schoenfeld",linear.prediction = TRUE)
ggcoxdiagnostics(A7,type="schoenfeld",linear.prediction = TRUE)
ggcoxdiagnostics(A8,type="schoenfeld",linear.prediction = TRUE)
ggcoxdiagnostics(A9,type="schoenfeld",linear.prediction = TRUE)

##################################################################################

B1<-coxph(Surv(os_60,death_60==1) ~ Age ,data=lung_data2)
B2<-coxph(Surv(os_60,death_60==1) ~ Sex ,data=lung_data2)
B3<-coxph(Surv(os_60,death_60==1) ~ ECOG ,data=lung_data2)
B4<-coxph(Surv(os_60,death_60==1) ~ PDL1_expression ,data=lung_data2)
B5<-coxph(Surv(os_60,death_60==1) ~ Smoking ,data=lung_data2)
B6<-coxph(Surv(os_60,death_60==1) ~ Histology ,data=lung_data2)
B7<-coxph(Surv(os_60,death_60==1) ~ Previous_chemotherapy ,data=lung_data2)
B8<-coxph(Surv(os_60,death_60==1) ~ Anti_PDL1_inhibitors ,data=lung_data2)
B9<-coxph(Surv(os_60,death_60==1) ~ Season ,data=lung_data2)

AB1<-cox.zph(B1)
AB2<-cox.zph(B2)
AB3<-cox.zph(B3)
AB4<-cox.zph(B4)
AB5<-cox.zph(B5)
AB6<-cox.zph(B6)
AB7<-cox.zph(B7)
AB8<-cox.zph(B8)
AB9<-cox.zph(B9)

ggcoxzph(AB1)
ggcoxzph(AB2)
ggcoxzph(AB3)
ggcoxzph(AB4)
ggcoxzph(AB5)
ggcoxzph(AB6)
ggcoxzph(AB7)
ggcoxzph(AB8)
ggcoxzph(AB9)

ggcoxdiagnostics(B1,type="schoenfeld",linear.prediction = TRUE)
ggcoxdiagnostics(B2,type="schoenfeld",linear.prediction = TRUE)
ggcoxdiagnostics(B3,type="schoenfeld",linear.prediction = TRUE)
ggcoxdiagnostics(B4,type="schoenfeld",linear.prediction = TRUE)
ggcoxdiagnostics(B5,type="schoenfeld",linear.prediction = TRUE)
ggcoxdiagnostics(B6,type="schoenfeld",linear.prediction = TRUE)
ggcoxdiagnostics(B7,type="schoenfeld",linear.prediction = TRUE)
ggcoxdiagnostics(B8,type="schoenfeld",linear.prediction = TRUE)
ggcoxdiagnostics(B9,type="schoenfeld",linear.prediction = TRUE)



#################################################################################
library('fastDummies')

lung_data3 <- dummy_cols(lung_data2, select_columns = c("Age","Sex","ECOG","PDL1_expression","Smoking","Histology","Previous_chemotherapy","Anti_PDL1_inhibitors","Season"),remove_selected_columns = TRUE)

cox.zph(coxph(Surv(pfs_60,pd_60==1) ~ lung_data3$"ECOG_1. Status 0" ,data=lung_data3))
cox.zph(coxph(Surv(pfs_60,pd_60==1) ~ lung_data3$"ECOG_2. Status 1" ,data=lung_data3))
cox.zph(coxph(Surv(pfs_60,pd_60==1) ~ lung_data3$"ECOG_3. Status 2≥" ,data=lung_data3))

cox.zph(coxph(Surv(pfs_60,pd_60==1) ~ lung_data3$"Histology_1. Adenocarcinoma" ,data=lung_data3))
cox.zph(coxph(Surv(pfs_60,pd_60==1) ~ lung_data3$"Histology_2. Squamous cell carcinoma" ,data=lung_data3))
cox.zph(coxph(Surv(pfs_60,pd_60==1) ~ lung_data3$"Histology_3. Other NSCLC" ,data=lung_data3))
cox.zph(coxph(Surv(pfs_60,pd_60==1) ~ lung_data3$"Histology_4. Small cell lung cancer" ,data=lung_data3))

cox.zph(coxph(Surv(pfs_60,pd_60==1) ~ lung_data3$"Previous_chemotherapy_1. 0" ,data=lung_data3))
cox.zph(coxph(Surv(pfs_60,pd_60==1) ~ lung_data3$"Previous_chemotherapy_2. 1-2" ,data=lung_data3))
cox.zph(coxph(Surv(pfs_60,pd_60==1) ~ lung_data3$"Previous_chemotherapy_3. ≥3" ,data=lung_data3))

cox.zph(coxph(Surv(pfs_60,pd_60==1) ~ lung_data3$"Anti_PDL1_inhibitors_1. Pembrolizumab" ,data=lung_data3))
cox.zph(coxph(Surv(pfs_60,pd_60==1) ~ lung_data3$"Anti_PDL1_inhibitors_2. Nivolumab" ,data=lung_data3))
cox.zph(coxph(Surv(pfs_60,pd_60==1) ~ lung_data3$"Anti_PDL1_inhibitors_3. Atezolizumab" ,data=lung_data3))
cox.zph(coxph(Surv(pfs_60,pd_60==1) ~ lung_data3$"Anti_PDL1_inhibitors_4. Others" ,data=lung_data3))








