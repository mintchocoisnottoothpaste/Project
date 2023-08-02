library("openxlsx") # open xlsx
library("MatchIt") # propensity mathcing method

lung_data <- read.xlsx(xlsxFile="/Users/jun/Library/CloudStorage/OneDrive-개인/lung/data/study_data_20220421.xlsx", sheet = 1, colNames = TRUE)
lung_data$Season2 <- ifelse(lung_data$Season2==2,0,1) # Season2 = 1 : Cold Season , Season2 = 0 : Else Season
vars <- c("Age2","Sex","ECOG3","PDL1_IHC","Previous.Treatment3","Smoking2","Histology","Regimen.4","Response","PD","Season2")
lung_data[,vars] <- lapply(lung_data[,vars] , factor)

lung_data$Start.date <- as.Date(lung_data$Start.date,origin = "1899-12-30")
lung_data$Progression.date.or.FU.data[append(seq(1,168),seq(246,503))] <- as.Date(as.numeric(lung_data$Progression.date.or.FU.data[append(seq(1:168),seq(246,503))]),origin = "1899-12-30")
lung_data$Progression.date.or.FU.data[seq(169,245)] <- as.Date(gsub("-","/",lung_data$Progression.date.or.FU.data[seq(169,245)]), format='%Y/%m/%d')
lung_data$Death.date.or.FU.date[append(seq(1,168),seq(246,503))] <- as.Date(as.numeric(lung_data$Death.date.or.FU.date[append(seq(1:168),seq(246,503))]),origin = "1899-12-30")
lung_data$Death.date.or.FU.date[seq(169,245)] <- as.Date(gsub("-","/",lung_data$Death.date.or.FU.date[seq(169,245)]), format='%Y/%m/%d')

save(lung_data,file="/Users/jun/Downloads/lung_data.RData")

########## ########## ########## ##########
# Unmatched 

load(file="/Users/jun/Downloads/lung_data.RData")

# Matched 2cov 

Matched_2cov <- matchit(factor(Season2) ~ ECOG3 + PDL1_IHC, data = lung_data, method = "nearest", ratio = 1,caliper = 0.1)
Matched_2cov <- match.data(Matched_2cov)

# Matched 4cov

Matched_4cov <- matchit(factor(Season2) ~ ECOG3 + PDL1_IHC + Previous.Treatment3 + Smoking2, data = lung_data, method = "nearest", ratio = 1,caliper = 0.1)
Matched_4cov = match.data(Matched_4cov)


save(Matched_2cov,file="/Users/jun/Downloads/Matched_2cov.RData")
save(Matched_4cov,file="/Users/jun/Downloads/Matched_4cov.RData")



