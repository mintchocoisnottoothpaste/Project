###################
# Date 19.05.2018 #
###################

library("openxlsx") 
library("tidyverse")
set.seed(2022)

# Korea 2005 Population : standard population
Korea05 <-c(
  0.0519927,
  0.0677420,
  0.0725639,
  0.0644455,
  0.0781165,
  0.0799971,
  0.0912217,
  0.0907564,
  0.0891165,
  0.0818885,
  0.0581230,
  0.0462167,
  0.0410390,
  0.0344449,
  0.0238813,
  0.0149370,
  0.0085521,
  0.0049653)


# Population

#pop_men <- read.xlsx(xlsxFile="/Users/jun/Library/CloudStorage/OneDrive-媛쒖씤/?끂?씤?븫?넻怨?/canproj/pop_00_19.xlsx", sheet = 'Male', colNames = TRUE)
#pop_women <- read.xlsx(xlsxFile="/Users/jun/Library/CloudStorage/OneDrive-媛쒖씤/?끂?씤?븫?넻怨?/canproj/pop_00_19.xlsx", sheet = 'Female', colNames = TRUE)

#pop_predmen <- read.xlsx(xlsxFile="/Users/jun/Library/CloudStorage/OneDrive-媛쒖씤/?끂?씤?븫?넻怨?/canproj/predict_population.xlsx", sheet = 'Male', colNames = TRUE)
#pop_predwomen <- read.xlsx(xlsxFile="/Users/jun/Library/CloudStorage/OneDrive-媛쒖씤/?끂?씤?븫?넻怨?/canproj/predict_population.xlsx", sheet = 'Female', colNames = TRUE)


pop_men <- read.xlsx(xlsxFile="C:/Users/jun/OneDrive/노인암통계/canproj/pop_00_19.xlsx", sheet = 'Male', colNames = TRUE)
pop_women <- read.xlsx(xlsxFile="C:/Users/jun/OneDrive/노인암통계/canproj/pop_00_19.xlsx", sheet = 'Female', colNames = TRUE)

pop_predmen <- read.xlsx(xlsxFile="C:/Users/jun/OneDrive/노인암통계/canproj/predict_population.xlsx", sheet = 'Male', colNames = TRUE)
pop_predwomen <- read.xlsx(xlsxFile="C:/Users/jun/OneDrive/노인암통계/canproj/canproj/predict_population.xlsx", sheet = 'Female', colNames = TRUE)


pop_predmen<-as.data.frame(pop_predmen[c(-1,-22)])
pop_predwomen<-as.data.frame(pop_predwomen[c(-1,-22)])


# Incidence Case

#cancer_men <- read.xlsx(xlsxFile="/Users/jun/Library/CloudStorage/OneDrive-媛쒖씤/?끂?씤?븫?넻怨?/canproj/cancer_case.xlsx", sheet = 'Male', colNames = TRUE)
#cancer_women <- read.xlsx(xlsxFile="/Users/jun/Library/CloudStorage/OneDrive-媛쒖씤/?끂?씤?븫?넻怨?/canproj/cancer_case.xlsx", sheet = 'Female', colNames = TRUE)

cancer_men <- read.xlsx(xlsxFile="C:/Users/jun/OneDrive/노인암통계/canproj/cancer_case.xlsx", sheet = 'Male', colNames = TRUE)
cancer_women <- read.xlsx(xlsxFile="C:/Users/jun/OneDrive/노인암통계/canproj/cancer_case.xlsx", sheet = 'Female', colNames = TRUE)

cancer_men<-cancer_men[c(1,3,2,4)]
cancer_women<-cancer_women[c(1,3,2,4)]

# Death

#Death_men <- read.xlsx(xlsxFile="C:/Users/jun/OneDrive/노인암통계/nordpred/canproj/death_longitudinal.xlsx", sheet = 'Male', colNames = TRUE)
#Death_women <- read.xlsx(xlsxFile="C:/Users/jun/OneDrive/노인암통계/nordpred/canproj/death_longitudinal.xlsx", sheet = 'Female', colNames = TRUE)

Death_men <- read.xlsx(xlsxFile="C:/Users/jun/OneDrive/노인암통계/canproj/death_longitudinal.xlsx", sheet = 'Male', colNames = TRUE)
Death_women <- read.xlsx(xlsxFile="C:/Users/jun/OneDrive/노인암통계/canproj/death_longitudinal.xlsx", sheet = 'Female', colNames = TRUE)


## trans population data

trandat_pop <- function(dat, starty) { 
  n <- dim(dat)[1]/18
  outdate <- as.data.frame(matrix(dat$Population, ncol=n)) 
  colnames(outdate) <- starty : (starty + n -1) 
  return(outdate)
}

pop_men <- trandat_pop(pop_men, 2000)
pop_women <- trandat_pop(pop_women, 2000)

pop_men  <-cbind(pop_men,pop_predmen)
pop_women<-cbind(pop_women,pop_predwomen)

# trans cancer data
trandat <- function(dat, starty) { 
  n <- dim(dat)[1]/18
  outdate <- as.data.frame(matrix(dat$Cases, ncol=n)) 
  colnames(outdate) <- starty : (starty + n -1) 
  return(outdate)
}

# Incidence

# Male
C00_C96_Men <- trandat(subset(cancer_men,Cancer=='C00-C96')[-1], 2000)
C16_Men <- trandat(subset(cancer_men,Cancer=='C16')[-1], 2000)
C18_C20_Men <- trandat(subset(cancer_men,Cancer=='C18-C20')[-1], 2000)
C22_Men <- trandat(subset(cancer_men,Cancer=='C22')[-1], 2000)
C23_C24_Men <- trandat(subset(cancer_men,Cancer=='C23-C24')[-1], 2000)
C25_Men <- trandat(subset(cancer_men,Cancer=='C25')[-1], 2000)
C33_C34_Men <- trandat(subset(cancer_men,Cancer=='C33-C34')[-1], 2000)
C61_Men <- trandat(subset(cancer_men,Cancer=='C61')[-1], 2000)

# Female
C00_C96_Women <- trandat(subset(cancer_women,Cancer=='C00-C96')[-1], 2000)
C16_Women <- trandat(subset(cancer_women,Cancer=='C16')[-1], 2000)
C18_C20_Women <- trandat(subset(cancer_women,Cancer=='C18-C20')[-1], 2000)
C22_Women <- trandat(subset(cancer_women,Cancer=='C22')[-1], 2000)
C23_C24_Women <- trandat(subset(cancer_women,Cancer=='C23-C24')[-1], 2000)
C25_Women <- trandat(subset(cancer_women,Cancer=='C25')[-1], 2000)
C33_C34_Women <- trandat(subset(cancer_women,Cancer=='C33-C34')[-1], 2000)
C50_Women <- trandat(subset(cancer_women,Cancer=='C50')[-1], 2000)

# Death

cancer_death <- function(data,chr) {
  rbind(colSums(subset(data,Cancer==chr)[3:23][c(1,2),]),subset(Death_men,Cancer==chr)[3:23][c(3:18),],colSums(subset(Death_men,Cancer==chr)[3:23][c(19,20),]))[,-21]
}
# Male

C00_C96_DMen <- cancer_death(Death_men,unique(Death_men$Cancer)[1])
C16_DMen     <- cancer_death(Death_men,unique(Death_men$Cancer)[2])
C18_C21_DMen <- cancer_death(Death_men,unique(Death_men$Cancer)[3])
C22_DMen     <- cancer_death(Death_men,unique(Death_men$Cancer)[4])
C25_DMen     <- cancer_death(Death_men,unique(Death_men$Cancer)[5])
C33_C34_DMen <- cancer_death(Death_men,unique(Death_men$Cancer)[6])
C61_DMen     <- cancer_death(Death_men,unique(Death_men$Cancer)[7])

# Female

C00_C96_DWomen <- cancer_death(Death_women,unique(Death_women$Cancer)[1])
C16_DWomen     <- cancer_death(Death_women,unique(Death_women$Cancer)[2])
C18_C21_DWomen <- cancer_death(Death_women,unique(Death_women$Cancer)[3])
C22_DWomen     <- cancer_death(Death_women,unique(Death_women$Cancer)[4])
C25_DWomen     <- cancer_death(Death_women,unique(Death_women$Cancer)[5])
C33_C34_DWomen <- cancer_death(Death_women,unique(Death_women$Cancer)[6])
C50_DWomen     <- cancer_death(Death_women,unique(Death_women$Cancer)[7])


# library canproj2019 pakcage
source("/Users/jun/Library/CloudStorage/OneDrive-媛쒖씤/?끂?씤?븫?넻怨?/canproj/canproj-v2019.R")

# Example 1. 2000-2014 / 2015-201

C16_Men.obj0<-canproj(C16_Men[,c(1:15)], pop_men[,c(1:20)], 2015,startestage=5)
C18_C20_Men.obj0<-canproj(C18_C20_Men[,c(1:15)], pop_men[,c(1:20)], 2015,startestage=5)
C22_Men.obj0<-canproj(C22_Men[,c(1:15)], pop_men[,c(1:20)], 2015,startestage=5)
C23_C24_Men.obj0<-canproj(C23_C24_Men[,c(1:15)], pop_men[,c(1:20)], 2015,startestage=5)
C25_Men.obj0<-canproj(C25_Men[,c(1:15)], pop_men[,c(1:20)], 2015,startestage=5)
C33_C34_Men.obj0<-canproj(C33_C34_Men[,c(1:15)], pop_men[,c(1:20)], 2015,startestage=5)
C61_Men.obj0<-canproj(C61_Men[,c(1:15)], pop_men[,c(1:20)], 2015,startestage=5) #


C16_Women.obj0<-canproj(C16_Men[,c(1:15)], pop_women[,c(1:20)], 2015,startestage=5)
C18_C20_Women.obj0<-canproj(C18_C20_Women[,c(1:15)], pop_women[,c(1:20)], 2015,startestage=5)
C22_Women.obj0<-canproj(C22_Women[,c(1:15)], pop_women[,c(1:20)], 2015,startestage=5) # 
C23_C24_Women.obj0<-canproj(C23_C24_Women[,c(1:15)], pop_women[,c(1:20)], 2015,startestage=5)
C25_Women.obj0<-canproj(C25_Women[,c(1:15)], pop_women[,c(1:20)], 2015,startestage=5)
C33_C34_Women.obj0<-canproj(C33_C34_Women[,c(1:15)], pop_women[,c(1:20)], 2015,startestage=5) #
C50_Women.obj0<-canproj(C50_Women[,c(1:15)], pop_women[,c(1:20)], 2015,startestage=5) #

##############################################################################


summary.canproj(C16_Men.obj0)
summary.canproj(C18_C20_Men.obj0)
summary.canproj(C22_Men.obj0)
summary.canproj(C23_C24_Men.obj0)
summary.canproj(C25_Men.obj0)
summary.canproj(C33_C34_Men.obj0)
summary.canproj(C61_Men.obj0)

summary.canproj(C16_Women.obj0)
summary.canproj(C18_C20_Women.obj0)
summary.canproj(C22_Women.obj0)
summary.canproj(C23_C24_Women.obj0)
summary.canproj(C25_Women.obj0)
summary.canproj(C33_C34_Women.obj0)
summary.canproj(C50_Women.obj0)















##############################################################################
####################                         ################################
##############################################################################

# Example 2. 2000-2019 / 2029

C00_C96_Men.obj0<-canproj(C00_C96_Men, pop_men[,c(1:30)], 2020,startestage=5)
C16_Men.obj0<-canproj(C16_Men, pop_men[,c(1:20)], 2020,startestage=5)
C18_C20_Men.obj0<-canproj(C18_C20_Men, pop_men[,c(1:30)], 2015,startestage=5)
C22_Men.obj0<-canproj(C22_Men, pop_men[,c(1:30)], 2020,startestage=5)
C23_C24_Men.obj0<-canproj(C23_C24_Men, pop_men[,c(1:20)], 2015,startestage=5)
C25_Men.obj0<-canproj(C25_Men, pop_men[,c(1:30)], 2020,startestage=5)
C33_C34_Men.obj0<-canproj(C33_C34_Men, pop_men[,c(1:20)], 2015,startestage=5)
C61_Men.obj0<-canproj(C61_Men, pop_men[,c(1:30)], 2020,startestage=5)

C00_C96_Women.obj0<-canproj(C00_C96_Women, pop_women[,c(1:20)], 2020,startestage=5)
C16_Women.obj0<-canproj(C16_Men, pop_women[,c(1:20)], 2020,startestage=5)
C18_C20_Women.obj0<-canproj(C18_C20_Women, pop_women[,c(1:20)], 2020,startestage=5)
C22_Women.obj0<-canproj(C22_Women, pop_women[,c(1:20)], 2020,startestage=5)
C23_C24_Women.obj0<-canproj(C23_C24_Women, pop_women[,c(1:20)], 2020,startestage=5)
C25_Women.obj0<-canproj(C25_Women, pop_women[,c(1:20)], 2020,startestage=5)
C33_C34_Women.obj0<-canproj(C33_C34_Women, pop_women[,c(1:20)], 2020,startestage=5)
C50_Women.obj0<-canproj(C50_Women, pop_women[,c(1:20)], 2020,startestage=5)

##############################################################################

summary.canproj(C00_C96_Men.obj0)
summary.canproj(C16_Men.obj0)
summary.canproj(C18_C20_Men.obj0)
summary.canproj(C22_Men.obj0)
summary.canproj(C23_C24_Men.obj0)
summary.canproj(C25_Men.obj0)
summary.canproj(C33_C34_Men.obj0)
summary.canproj(C61_Men.obj0)

summary.canproj(C00_C96_Women.obj0)
summary.canproj(C16_Women.obj0)
summary.canproj(C18_C20_Women.obj0)
summary.canproj(C22_Women.obj0)
summary.canproj(C23_C24_Women.obj0)
summary.canproj(C25_Women.obj0)
summary.canproj(C33_C34_Women.obj0)
summary.canproj(C50_Women.obj0)




