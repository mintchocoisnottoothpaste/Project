library("openxlsx") 
library("tidyverse")
library("reshape")
set.seed(2022)


# Population
pop_women <- read.xlsx(xlsxFile="/Users/jun/Desktop/elderlystatistics/canproj/pop_00_19.xlsx", sheet = 'Female', colNames = TRUE)
pop_predwomen <- read.xlsx(xlsxFile="/Users/jun/Desktop/elderlystatistics/canproj/predict_population.xlsx", sheet = 'Female', colNames = TRUE)
pop_predwomen<-as.data.frame(pop_predwomen[c(-1,-22)])

# Incidence Case
cancer_women <- read.xlsx(xlsxFile="/Users/jun/Desktop/elderlystatistics/canproj/cancer_case.xlsx", sheet = 'Female', colNames = TRUE)
cancer_women<-cancer_women[c(1,3,2,4)]

trandat_pop <- function(dat, starty) { 
  n <- dim(dat)[1]/18
  outdate <- as.data.frame(matrix(dat$Population, ncol=n)) 
  colnames(outdate) <- starty : (starty + n -1) 
  return(outdate)
}

pop_women <- trandat_pop(pop_women, 2000)
pop_women<-cbind(pop_women,pop_predwomen)

trandat_ca <- function(dat, starty) { 
  n <- dim(dat)[1]/18
  outdate <- as.data.frame(matrix(as.numeric(dat$Cases), ncol=n)) 
  colnames(outdate) <- starty : (starty + n -1) 
  return(outdate)
}

# Female
C00_C96_Women <- trandat_ca(subset(cancer_women,Cancer=='C00-C96')[-1], 2000)
C16_Women <- trandat_ca(subset(cancer_women,Cancer=='C16')[-1], 2000)
C18_C20_Women <- trandat_ca(subset(cancer_women,Cancer=='C18-C20')[-1], 2000)
C22_Women <- trandat_ca(subset(cancer_women,Cancer=='C22')[-1], 2000)
C23_C24_Women <- trandat_ca(subset(cancer_women,Cancer=='C23-C24')[-1], 2000)
C25_Women <- trandat_ca(subset(cancer_women,Cancer=='C25')[-1], 2000)
C33_C34_Women <- trandat_ca(subset(cancer_women,Cancer=='C33-C34')[-1], 2000)
C50_Women <- trandat_ca(subset(cancer_women,Cancer=='C50')[-1], 2000)

##################################################################

source("/Users/jun/Desktop/elderlystatistics/canproj/canproj-v2019.R")

##################################################################

# observed 200-2019, prediction 2020-2029
C00_C96_Women.obj0<-canproj(C00_C96_Women, pop_women[,seq(1,30)], 2020,startestage=7)
C16_Women.obj0<-canproj(C16_Women, pop_women[,seq(1,30)], 2020,startestage=7)
C18_C20_Women.obj0<-canproj(C18_C20_Women, pop_women[,seq(1,30)], 2020,startestage=7)
C22_Women.obj0<-canproj(C22_Women, pop_women[,seq(1,30)], 2020,startestage=7)
C23_C24_Women.obj0<-canproj(C23_C24_Women, pop_women[,seq(1,30)], 2020,startestage=9)
C25_Women.obj0<-canproj(C25_Women, pop_women[,seq(1,30)], 2020,startestage=7)
C33_C34_Women.obj0<-canproj(C33_C34_Women, pop_women[,seq(1,30)], 2020,startestage=7)
C50_Women.obj0<-canproj(C50_Women, pop_women[,seq(1,30)], 2020,startestage=7)

summary(C00_C96_Women.obj0)
summary(C16_Women.obj0)
summary(C18_C20_Women.obj0)
summary(C22_Women.obj0)
summary(C23_C24_Women.obj0)
summary(C25_Women.obj0)
summary(C33_C34_Women.obj0)
summary(C50_Women.obj0)


C00_96<-round(canproj.getproj(C00_C96_Women.obj0),0)
C16<-round(canproj.getproj(C16_Women.obj0),0)
C18_C20<-round(canproj.getproj(C18_C20_Women.obj0),0)
C22<-round(canproj.getproj(C22_Women.obj0),0)
C23_C24<-round(canproj.getproj(C23_C24_Women.obj0),0)
C25<-round(canproj.getproj(C25_Women.obj0),0)
C33_C34<-round(canproj.getproj(C33_C34_Women.obj0),0)
C50<-round(canproj.getproj(C50_Women.obj0),0)


proj_list <- c("C00_96","C16","C18_C20","C22","C23_C24","C25","C33_C34","C50")
Age <- as.data.frame(as.matrix(seq(1,18),ncol = 1))
colnames(Age) <-c("Age")

pop0029 <- pop_women[,seq(1,30)];pop0029 <- bind_cols(Age,pop0029);pop0029 <- melt(pop0029,id = c("Age"));
pop0029 <- pop0029 %>% rename(c("variable" = "Year","value" = "Pop"));pop0029<-as.data.frame(as.matrix(pop0029[,-c(1,2)],ncol=1))
colnames(pop0029) <-c("Pop")

for (i in proj_list){
  assign(paste(i), bind_cols(Age,get(i)))
  assign(paste(i),melt(get(i),id = c("Age")))
  assign(paste(i),get(i) %>% rename(c("variable" = "Year","value" = "Count")))
  assign(paste(i), bind_cols(get(i),pop0029))
  assign(paste("ASIR"),round(get(i)$Count/get(i)$Pop*100000,2))
  assign(paste(i),bind_cols(get(i),get(paste("ASIR"))))
  assign(paste(i),get(i) %>% rename(c("...5" = "ASIR")))
  write.xlsx(get(i),paste0("/Users/jun/Desktop/elderlystatistics/projection_data/female/",i,".xlsx"))
}


##################################################################

# observed 2000-2014, prediction 2015-2019

C00_C96_Women.obj0<-canproj(C00_C96_Women[,seq(1,15)], pop_women[,seq(1,20)], 2015,startestage=7)
C16_Women.obj0<-canproj(C16_Women[,seq(1,15)], pop_women[,seq(1,20)], 2015,startestage=7)
C18_C20_Women.obj0<-canproj(C18_C20_Women[,seq(1,15)], pop_women[,seq(1,20)], 2015,startestage=7)
C22_Women.obj0<-canproj(C22_Women[,seq(1,15)], pop_women[,seq(1,20)], 2015,startestage=7)
C23_C24_Women.obj0<-canproj(C23_C24_Women[,seq(1,15)], pop_women[,seq(1,20)], 2015,startestage=7)
C25_Women.obj0<-canproj(C25_Women[,seq(1,15)], pop_women[,seq(20)], 2015,startestage=7)
C33_C34_Women.obj0<-canproj(C33_C34_Women[,seq(1,15)], pop_women[,seq(1,20)], 2015,startestage=7)
C50_Women.obj0<-canproj(C50_Women[,seq(1,15)], pop_women[,seq(1,20)], 2015,startestage=7)

summary(C00_C96_Women.obj0)
summary(C16_Women.obj0)
summary(C18_C20_Women.obj0)
summary(C22_Women.obj0)
summary(C23_C24_Women.obj0)
summary(C25_Women.obj0)
summary(C33_C34_Women.obj0)
summary(C50_Women.obj0)


C00_96<-round(canproj.getproj(C00_C96_Women.obj0),0)
C16<-round(canproj.getproj(C16_Women.obj0),0)
C18_C20<-round(canproj.getproj(C18_C20_Women.obj0),0)
C22<-round(canproj.getproj(C22_Women.obj0),0)
C23_C24<-round(canproj.getproj(C23_C24_Women.obj0),0)
C25<-round(canproj.getproj(C25_Women.obj0),0)
C33_C34<-round(canproj.getproj(C33_C34_Women.obj0),0)
C50<-round(canproj.getproj(C50_Women.obj0),0)

proj_list <- c("C00_96","C16","C18_C20","C22","C23_C24","C25","C33_C34","C50")
Age <- as.data.frame(as.matrix(seq(1,18),ncol = 1))
colnames(Age) <-c("Age")

pop0019 <- pop_women[,seq(1,20)];pop0019 <- bind_cols(Age,pop0019);pop0019 <- melt(pop0019,id = c("Age"));
pop0019 <- pop0019 %>% rename(c("variable" = "Year","value" = "Pop"));pop0019<-as.data.frame(as.matrix(pop0019[,-c(1,2)],ncol=1))
colnames(pop0019) <-c("Pop")

for (i in proj_list){
  assign(paste(i), bind_cols(Age,get(i)))
  assign(paste(i),melt(get(i),id = c("Age")))
  assign(paste(i),get(i) %>% rename(c("variable" = "Year","value" = "Count")))
  assign(paste(i), bind_cols(get(i),pop0019))
  assign(paste("ASIR"),round(get(i)$Count/get(i)$Pop*100000,2))
  assign(paste(i),bind_cols(get(i),get(paste("ASIR"))))
  assign(paste(i),get(i) %>% rename(c("...5" = "ASIR")))
  write.xlsx(get(i),paste0("/Users/jun/Library/Mobile Documents/com~apple~CloudDocs/elderlystatistics/projection_data/female/",i,".xlsx"))
}

##################################################################

