# library import

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

#library(extrafont)
#font_import()


# library import
cancer_inci_name <- c("C00_C96","C00_C14","C15","C16","C18_C21","C22","C23_C24","C25","C32","C33_C34","C50","C53","C54_C55","C56",
                       "C61","C60_C63","C64","C67","C70_C72","C73","C81","C82_C86_C96","C90","C91_C95","etc(C00_C96)")

cancer_death_name <- c("C00_C97","C00_C14","C15","C16","C18_C21","C22","C23_C24","C25","C32","C33_C34","C50","C53","C54_C55","C56",
                       "C61","C60_C63","C64","C67","C70_C72","C73","C81","C82_C86","C90","C91_C95","etc(C00_C97)")

agegroups <- c("00-04","05-09","10-14","15-19","20-24","25-29", "30-34", "35-39", "40-44", "45-49", 
               "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84","85+" )

age <- seq(2,87,by=5)
ly <- rep(2000,8)
uy <- rep(2019,8)
col <- c("grey20", "grey35", "grey50", "grey65", "grey75", "grey85")
lty <- c(rep(c(1,2), 3), rep(c(2,1),3))

################## Population

pop_men <- read.xlsx(xlsxFile="/Users/jun/Library/CloudStorage/OneDrive-개인/2022/노인암정리/population83_21.xlsx", sheet = 'Male', colNames = TRUE)[,-1]
pop_women <- read.xlsx(xlsxFile="/Users/jun/Library/CloudStorage/OneDrive-개인/2022/노인암정리/population83_21.xlsx", sheet = 'Female', colNames = TRUE)[,-1]

pop_predmen <- read.xlsx(xlsxFile="/Users/jun/Library/CloudStorage/OneDrive-개인/2022/노인암정리/predict_population20_40.xlsx", sheet = 'Male', colNames = TRUE)
pop_predwomen <- read.xlsx(xlsxFile="/Users/jun/Library/CloudStorage/OneDrive-개인/2022/노인암정리/predict_population20_40.xlsx", sheet = 'Female', colNames = TRUE)

pop_predmen<-as.data.frame(pop_predmen[c(-c(1,2,3),-22)])
pop_predwomen<-as.data.frame(pop_predwomen[c(-c(1,2,3),-22)])

#  Present population data and expected future population data (ref KOSIS)
## trans population data

# trandat_pop <- function(dat, starty) { 
#   n <- dim(dat)[1]/18
#   outdate <- as.data.frame(matrix(dat, ncol=n)) 
#   colnames(outdate) <- starty : (starty + n -1) 
#   return(outdate)
# }

pop_men       <- as.data.frame(t(pop_men))
pop_women     <- as.data.frame(t(pop_women))

colnames(pop_men)   <-c(1:18)
colnames(pop_women) <-c(1:18)

pop_predmen   <- as.data.frame(t(pop_predmen))
pop_predwomen <- as.data.frame(t(pop_predwomen))

wh_pop_men   <- rbind(pop_men,pop_predmen)
wh_pop_women <- rbind(pop_women,pop_predwomen)

########################################################################
# Cancer incidence/death data import
########################################################################

path_incidence <- "/Users/jun/Library/CloudStorage/OneDrive-개인/2022/노인암정리/24_cancers_incidence.xlsx"

C00_C96.ca.m <-subset(read.xlsx(path_incidence, sheet = "C00_C96"),Sex =='Male');  C00_C96.ca.m <- C00_C96.ca.m[,-c(1:3)]; C00_C96.ca.m[is.na(C00_C96.ca.m)] <- 0;  C00_C96.ca.m <-as.data.frame(t(C00_C96.ca.m));colnames(C00_C96.ca.m)=c(1:18)
C00_C96.ca.w <-subset(read.xlsx(path_incidence, sheet = "C00_C96"),Sex =='Female');C00_C96.ca.w <- C00_C96.ca.w[,-c(1:3)]; C00_C96.ca.w[is.na(C00_C96.ca.w)] <- 0;  C00_C96.ca.w <-as.data.frame(t(C00_C96.ca.w));colnames(C00_C96.ca.w)=c(1:18)

C00_C14.ca.m <-subset(read.xlsx(path_incidence, sheet = "C00_C14"),Sex =='Male');  C00_C14.ca.m <- C00_C14.ca.m[,-c(1:3)]; C00_C14.ca.m[is.na(C00_C14.ca.m)] <- 0;  C00_C14.ca.m <-as.data.frame(t(C00_C14.ca.m));colnames(C00_C14.ca.m)=c(1:18)
C00_C14.ca.w <-subset(read.xlsx(path_incidence, sheet = "C00_C14"),Sex =='Female');C00_C14.ca.w <- C00_C14.ca.w[,-c(1:3)]; C00_C14.ca.w[is.na(C00_C14.ca.w)] <- 0;  C00_C14.ca.w <-as.data.frame(t(C00_C14.ca.w));colnames(C00_C14.ca.w)=c(1:18)

C15.ca.m <-subset(read.xlsx(path_incidence, sheet = "C15"),Sex =='Male');  C15.ca.m <- C15.ca.m[,-c(1:3)]; C15.ca.m[is.na(C15.ca.m)] <- 0;  C15.ca.m <-as.data.frame(t(C15.ca.m));colnames(C15.ca.m)=c(1:18)
C15.ca.w <-subset(read.xlsx(path_incidence, sheet = "C15"),Sex =='Female');C15.ca.w <- C15.ca.w[,-c(1:3)]; C15.ca.w[is.na(C15.ca.w)] <- 0;  C15.ca.w <-as.data.frame(t(C15.ca.w));colnames(C15.ca.w)=c(1:18)

C16.ca.m <-subset(read.xlsx(path_incidence, sheet = "C16"),Sex =='Male');  C16.ca.m <- C16.ca.m[,-c(1:3)]; C16.ca.m[is.na(C16.ca.m)] <- 0;  C16.ca.m <-as.data.frame(t(C16.ca.m));colnames(C16.ca.m)=c(1:18)
C16.ca.w <-subset(read.xlsx(path_incidence, sheet = "C16"),Sex =='Female');C16.ca.w <- C16.ca.w[,-c(1:3)]; C16.ca.w[is.na(C16.ca.w)] <- 0;  C16.ca.w <-as.data.frame(t(C16.ca.w));colnames(C16.ca.w)=c(1:18)

C18_C21.ca.m <-subset(read.xlsx(path_incidence, sheet = "C18_C21"),Sex =='Male');  C18_C21.ca.m <- C18_C21.ca.m[,-c(1:3)]; C18_C21.ca.m[is.na(C18_C21.ca.m)] <- 0;  C18_C21.ca.m <-as.data.frame(t(C18_C21.ca.m));colnames(C18_C21.ca.m)=c(1:18)
C18_C21.ca.w <-subset(read.xlsx(path_incidence, sheet = "C18_C21"),Sex =='Female');C18_C21.ca.w <- C18_C21.ca.w[,-c(1:3)]; C18_C21.ca.w[is.na(C18_C21.ca.w)] <- 0;  C18_C21.ca.w <-as.data.frame(t(C18_C21.ca.w));colnames(C18_C21.ca.w)=c(1:18)

C22.ca.m <-subset(read.xlsx(path_incidence, sheet = "C22"),Sex =='Male');  C22.ca.m <- C22.ca.m[,-c(1:3)]; C22.ca.m[is.na(C22.ca.m)] <- 0;  C22.ca.m <-as.data.frame(t(C22.ca.m));colnames(C22.ca.m)=c(1:18)
C22.ca.w <-subset(read.xlsx(path_incidence, sheet = "C22"),Sex =='Female');C22.ca.w <- C22.ca.w[,-c(1:3)]; C22.ca.w[is.na(C22.ca.w)] <- 0;  C22.ca.w <-as.data.frame(t(C22.ca.w));colnames(C22.ca.w)=c(1:18)

C23_C24.ca.m <-subset(read.xlsx(path_incidence, sheet = "C23_C24"),Sex =='Male');  C23_C24.ca.m <- C23_C24.ca.m[,-c(1:3)]; C23_C24.ca.m[is.na(C23_C24.ca.m)] <- 0;  C23_C24.ca.m <-as.data.frame(t(C23_C24.ca.m));colnames(C23_C24.ca.m)=c(1:18)
C23_C24.ca.w <-subset(read.xlsx(path_incidence, sheet = "C23_C24"),Sex =='Female');C23_C24.ca.w <- C23_C24.ca.w[,-c(1:3)]; C23_C24.ca.w[is.na(C23_C24.ca.w)] <- 0;  C23_C24.ca.w <-as.data.frame(t(C23_C24.ca.w));colnames(C23_C24.ca.w)=c(1:18)

C25.ca.m <-subset(read.xlsx(path_incidence, sheet = "C25"),Sex =='Male');  C25.ca.m <- C25.ca.m[,-c(1:3)]; C25.ca.m[is.na(C25.ca.m)] <- 0;  C25.ca.m <-as.data.frame(t(C25.ca.m));colnames(C25.ca.m)=c(1:18)
C25.ca.w <-subset(read.xlsx(path_incidence, sheet = "C25"),Sex =='Female');C25.ca.w <- C25.ca.w[,-c(1:3)]; C25.ca.w[is.na(C25.ca.w)] <- 0;  C25.ca.w <-as.data.frame(t(C25.ca.w));colnames(C25.ca.w)=c(1:18)

C32.ca.m <-subset(read.xlsx(path_incidence, sheet = "C32"),Sex =='Male');  C32.ca.m <- C32.ca.m[,-c(1:3)]; C32.ca.m[is.na(C32.ca.m)] <- 0;  C32.ca.m <-as.data.frame(t(C32.ca.m));colnames(C32.ca.m)=c(1:18)
C32.ca.w <-subset(read.xlsx(path_incidence, sheet = "C32"),Sex =='Female');C32.ca.w <- C32.ca.w[,-c(1:3)]; C32.ca.w[is.na(C32.ca.w)] <- 0;  C32.ca.w <-as.data.frame(t(C32.ca.w));colnames(C32.ca.w)=c(1:18)

C33_C34.ca.m <-subset(read.xlsx(path_incidence, sheet = "C33_C34"),Sex =='Male');  C33_C34.ca.m <- C33_C34.ca.m[,-c(1:3)]; C33_C34.ca.m[is.na(C33_C34.ca.m)] <- 0;  C33_C34.ca.m <-as.data.frame(t(C33_C34.ca.m));colnames(C33_C34.ca.m)=c(1:18)
C33_C34.ca.w <-subset(read.xlsx(path_incidence, sheet = "C33_C34"),Sex =='Female');C33_C34.ca.w <- C33_C34.ca.w[,-c(1:3)]; C33_C34.ca.w[is.na(C33_C34.ca.w)] <- 0;  C33_C34.ca.w <-as.data.frame(t(C33_C34.ca.w));colnames(C33_C34.ca.w)=c(1:18)

# Sex-specific Cancer (Female)

C50.ca.m <-subset(read.xlsx(path_incidence, sheet = "C50"),Sex =='Male');  C50.ca.m <- C50.ca.m[,-c(1:3)]; C50.ca.m[is.na(C50.ca.m)] <- 0;  C50.ca.m <-as.data.frame(t(C50.ca.m));colnames(C50.ca.m)=c(1:18)
C50.ca.w <-subset(read.xlsx(path_incidence, sheet = "C50"),Sex =='Female');C50.ca.w <- C50.ca.w[,-c(1:3)]; C50.ca.w[is.na(C50.ca.w)] <- 0;  C50.ca.w <-as.data.frame(t(C50.ca.w));colnames(C50.ca.w)=c(1:18)

C53.ca.m <-subset(read.xlsx(path_incidence, sheet = "C53"),Sex =='Male');  C53.ca.m <- C53.ca.m[,-c(1:3)]; C53.ca.m[is.na(C53.ca.m)] <- 0;  C53.ca.m <-as.data.frame(t(C53.ca.m));colnames(C53.ca.m)=c(1:18)
C53.ca.w <-subset(read.xlsx(path_incidence, sheet = "C53"),Sex =='Female');C53.ca.w <- C53.ca.w[,-c(1:3)]; C53.ca.w[is.na(C53.ca.w)] <- 0;  C53.ca.w <-as.data.frame(t(C53.ca.w));colnames(C53.ca.w)=c(1:18)
#

C54_C55.ca.m <-subset(read.xlsx(path_incidence, sheet = "C54_C55"),Sex =='Male');  C54_C55.ca.m <- C54_C55.ca.m[,-c(1:3)]; C54_C55.ca.m[is.na(C54_C55.ca.m)] <- 0;  C54_C55.ca.m <-as.data.frame(t(C54_C55.ca.m));colnames(C54_C55.ca.m)=c(1:18)
C54_C55.ca.w <-subset(read.xlsx(path_incidence, sheet = "C54_C55"),Sex =='Female');C54_C55.ca.w <- C54_C55.ca.w[,-c(1:3)]; C54_C55.ca.w[is.na(C54_C55.ca.w)] <- 0;  C54_C55.ca.w <-as.data.frame(t(C54_C55.ca.w));colnames(C54_C55.ca.w)=c(1:18)

C56.ca.m <-subset(read.xlsx(path_incidence, sheet = "C56"),Sex =='Male');  C56.ca.m <- C56.ca.m[,-c(1:3)]; C56.ca.m[is.na(C56.ca.m)] <- 0;  C56.ca.m <-as.data.frame(t(C56.ca.m));colnames(C56.ca.m)=c(1:18)
C56.ca.w <-subset(read.xlsx(path_incidence, sheet = "C56"),Sex =='Female');C56.ca.w <- C56.ca.w[,-c(1:3)]; C56.ca.w[is.na(C56.ca.w)] <- 0;  C56.ca.w <-as.data.frame(t(C56.ca.w));colnames(C56.ca.w)=c(1:18)
#

# Sex-specific Cancer (Male)
C61.ca.m <-subset(read.xlsx(path_incidence, sheet = "C61"),Sex =='Male');  C61.ca.m <- C61.ca.m[,-c(1:3)]; C61.ca.m[is.na(C61.ca.m)] <- 0;  C61.ca.m <-as.data.frame(t(C61.ca.m));colnames(C61.ca.m)=c(1:18)
C61.ca.w <-subset(read.xlsx(path_incidence, sheet = "C61"),Sex =='Female');C61.ca.w <- C61.ca.w[,-c(1:3)]; C61.ca.w[is.na(C61.ca.w)] <- 0;  C61.ca.w <-as.data.frame(t(C61.ca.w));colnames(C61.ca.w)=c(1:18)

C60_C63.ca.m <-subset(read.xlsx(path_incidence, sheet = "C60_C63"),Sex =='Male');  C60_C63.ca.m <- C60_C63.ca.m[,-c(1:3)]; C60_C63.ca.m[is.na(C60_C63.ca.m)] <- 0;  C60_C63.ca.m <-as.data.frame(t(C60_C63.ca.m));colnames(C60_C63.ca.m)=c(1:18)
C60_C63.ca.w <-subset(read.xlsx(path_incidence, sheet = "C60_C63"),Sex =='Female');C60_C63.ca.w <- C60_C63.ca.w[,-c(1:3)]; C60_C63.ca.w[is.na(C60_C63.ca.w)] <- 0;  C60_C63.ca.w <-as.data.frame(t(C60_C63.ca.w));colnames(C60_C63.ca.w)=c(1:18)

C64.ca.m <-subset(read.xlsx(path_incidence, sheet = "C64"),Sex =='Male');  C64.ca.m <- C64.ca.m[,-c(1:3)]; C64.ca.m[is.na(C64.ca.m)] <- 0;  C64.ca.m <-as.data.frame(t(C64.ca.m));colnames(C64.ca.m)=c(1:18)
C64.ca.w <-subset(read.xlsx(path_incidence, sheet = "C64"),Sex =='Female');C64.ca.w <- C64.ca.w[,-c(1:3)]; C64.ca.w[is.na(C64.ca.w)] <- 0;  C64.ca.w <-as.data.frame(t(C64.ca.w));colnames(C64.ca.w)=c(1:18)
#

C67.ca.m <-subset(read.xlsx(path_incidence, sheet = "C67"),Sex =='Male');  C67.ca.m <- C67.ca.m[,-c(1:3)]; C67.ca.m[is.na(C67.ca.m)] <- 0;  C67.ca.m <-as.data.frame(t(C67.ca.m));colnames(C67.ca.m)=c(1:18)
C67.ca.w <-subset(read.xlsx(path_incidence, sheet = "C67"),Sex =='Female');C67.ca.w <- C67.ca.w[,-c(1:3)]; C67.ca.w[is.na(C67.ca.w)] <- 0;  C67.ca.w <-as.data.frame(t(C67.ca.w));colnames(C67.ca.w)=c(1:18)

C70_C72.ca.m <-subset(read.xlsx(path_incidence, sheet = "C70_C72"),Sex =='Male');  C70_C72.ca.m <- C70_C72.ca.m[,-c(1:3)]; C70_C72.ca.m[is.na(C70_C72.ca.m)] <- 0;  C70_C72.ca.m <-as.data.frame(t(C70_C72.ca.m));colnames(C70_C72.ca.m)=c(1:18)
C70_C72.ca.w <-subset(read.xlsx(path_incidence, sheet = "C70_C72"),Sex =='Female');C70_C72.ca.w <- C70_C72.ca.w[,-c(1:3)]; C70_C72.ca.w[is.na(C70_C72.ca.w)] <- 0;  C70_C72.ca.w <-as.data.frame(t(C70_C72.ca.w));colnames(C70_C72.ca.w)=c(1:18)

C73.ca.m <-subset(read.xlsx(path_incidence, sheet = "C73"),Sex =='Male');  C73.ca.m <- C73.ca.m[,-c(1:3)]; C73.ca.m[is.na(C73.ca.m)] <- 0;  C73.ca.m <-as.data.frame(t(C73.ca.m));colnames(C73.ca.m)=c(1:18)
C73.ca.w <-subset(read.xlsx(path_incidence, sheet = "C73"),Sex =='Female');C73.ca.w <- C73.ca.w[,-c(1:3)]; C73.ca.w[is.na(C73.ca.w)] <- 0;  C73.ca.w <-as.data.frame(t(C73.ca.w));colnames(C73.ca.w)=c(1:18)

C81.ca.m <-subset(read.xlsx(path_incidence, sheet = "C81"),Sex =='Male');  C81.ca.m <- C81.ca.m[,-c(1:3)]; C81.ca.m[is.na(C81.ca.m)] <- 0;  C81.ca.m <-as.data.frame(t(C81.ca.m));colnames(C81.ca.m)=c(1:18)
C81.ca.w <-subset(read.xlsx(path_incidence, sheet = "C81"),Sex =='Female');C81.ca.w <- C81.ca.w[,-c(1:3)]; C81.ca.w[is.na(C81.ca.w)] <- 0;  C81.ca.w <-as.data.frame(t(C81.ca.w));colnames(C81.ca.w)=c(1:18)

C82_C86_C96.ca.m <-subset(read.xlsx(path_incidence, sheet = "C82_C86_C96"),Sex =='Male');  C82_C86_C96.ca.m <- C82_C86_C96.ca.m[,-c(1:3)]; C82_C86_C96.ca.m[is.na(C82_C86_C96.ca.m)] <- 0;  C82_C86_C96.ca.m <-as.data.frame(t(C82_C86_C96.ca.m));colnames(C82_C86_C96.ca.m)=c(1:18)
C82_C86_C96.ca.w <-subset(read.xlsx(path_incidence, sheet = "C82_C86_C96"),Sex =='Female');C82_C86_C96.ca.w <- C82_C86_C96.ca.w[,-c(1:3)]; C82_C86_C96.ca.w[is.na(C82_C86_C96.ca.w)] <- 0;  C82_C86_C96.ca.w <-as.data.frame(t(C82_C86_C96.ca.w));colnames(C82_C86_C96.ca.w)=c(1:18)

C90.ca.m <-subset(read.xlsx(path_incidence, sheet = "C90"),Sex =='Male');  C90.ca.m <- C90.ca.m[,-c(1:3)]; C90.ca.m[is.na(C90.ca.m)] <- 0;  C90.ca.m <-as.data.frame(t(C90.ca.m));colnames(C90.ca.m)=c(1:18)
C90.ca.w <-subset(read.xlsx(path_incidence, sheet = "C90"),Sex =='Female');C90.ca.w <- C90.ca.w[,-c(1:3)]; C90.ca.w[is.na(C90.ca.w)] <- 0;  C90.ca.w <-as.data.frame(t(C90.ca.w));colnames(C90.ca.w)=c(1:18)

C91_C95.ca.m <-subset(read.xlsx(path_incidence, sheet = "C91_C95"),Sex =='Male');  C91_C95.ca.m <- C91_C95.ca.m[,-c(1:3)]; C91_C95.ca.m[is.na(C91_C95.ca.m)] <- 0;  C91_C95.ca.m <-as.data.frame(t(C91_C95.ca.m));colnames(C91_C95.ca.m)=c(1:18)
C91_C95.ca.w <-subset(read.xlsx(path_incidence, sheet = "C91_C95"),Sex =='Female');C91_C95.ca.w <- C91_C95.ca.w[,-c(1:3)]; C91_C95.ca.w[is.na(C91_C95.ca.w)] <- 0;  C91_C95.ca.w <-as.data.frame(t(C91_C95.ca.w));colnames(C91_C95.ca.w)=c(1:18)

etc_C00_C96_.ca.m <-subset(read.xlsx(path_incidence, sheet = "etc(C00_C96)"),Sex =='Male');  etc_C00_C96_.ca.m <- etc_C00_C96_.ca.m[,-c(1:3)]; etc_C00_C96_.ca.m[is.na(etc_C00_C96_.ca.m)] <- 0;  etc_C00_C96_.ca.m <-as.data.frame(t(etc_C00_C96_.ca.m));colnames(etc_C00_C96_.ca.m)=c(1:18)
etc_C00_C96_.ca.w <-subset(read.xlsx(path_incidence, sheet = "etc(C00_C96)"),Sex =='Female');etc_C00_C96_.ca.w <- etc_C00_C96_.ca.w[,-c(1:3)]; etc_C00_C96_.ca.w[is.na(etc_C00_C96_.ca.w)] <- 0;  etc_C00_C96_.ca.w <-as.data.frame(t(etc_C00_C96_.ca.w));colnames(etc_C00_C96_.ca.w)=c(1:18)



######## 
# Death
########

path_death <- "/Users/jun/Library/CloudStorage/OneDrive-개인/2022/노인암정리/24_cancers_death.xlsx"

C00_C97.ca.m_death <-subset(read.xlsx(path_death, sheet = "C00_C97"),Sex =='Male');  C00_C97.ca.m_death <- C00_C97.ca.m_death[,-c(1:3)]; C00_C97.ca.m_death[is.na(C00_C97.ca.m_death)] <- 0;  C00_C97.ca.m_death <-as.data.frame(t(C00_C97.ca.m_death));colnames(C00_C97.ca.m_death)=c(1:18)
C00_C97.ca.w_death <-subset(read.xlsx(path_death, sheet = "C00_C97"),Sex =='Female');C00_C97.ca.w_death <- C00_C97.ca.w_death[,-c(1:3)]; C00_C97.ca.w_death[is.na(C00_C97.ca.w_death)] <- 0;  C00_C97.ca.w_death <-as.data.frame(t(C00_C97.ca.w_death));colnames(C00_C97.ca.w_death)=c(1:18)

C00_C14.ca.m_death <-subset(read.xlsx(path_death, sheet = "C00_C14"),Sex =='Male');  C00_C14.ca.m_death <- C00_C14.ca.m_death[,-c(1:3)]; C00_C14.ca.m_death[is.na(C00_C14.ca.m_death)] <- 0;  C00_C14.ca.m_death <-as.data.frame(t(C00_C14.ca.m_death));colnames(C00_C14.ca.m_death)=c(1:18)
C00_C14.ca.w_death <-subset(read.xlsx(path_death, sheet = "C00_C14"),Sex =='Female');C00_C14.ca.w_death <- C00_C14.ca.w_death[,-c(1:3)]; C00_C14.ca.w_death[is.na(C00_C14.ca.w_death)] <- 0;  C00_C14.ca.w_death <-as.data.frame(t(C00_C14.ca.w_death));colnames(C00_C14.ca.w_death)=c(1:18)

C15.ca.m_death <-subset(read.xlsx(path_death, sheet = "C15"),Sex =='Male');  C15.ca.m_death <- C15.ca.m_death[,-c(1:3)]; C15.ca.m_death[is.na(C15.ca.m_death)] <- 0;  C15.ca.m_death <-as.data.frame(t(C15.ca.m_death));colnames(C15.ca.m_death)=c(1:18)
C15.ca.w_death <-subset(read.xlsx(path_death, sheet = "C15"),Sex =='Female');C15.ca.w_death <- C15.ca.w_death[,-c(1:3)]; C15.ca.w_death[is.na(C15.ca.w_death)] <- 0;  C15.ca.w_death <-as.data.frame(t(C15.ca.w_death));colnames(C15.ca.w_death)=c(1:18)

C16.ca.m_death <-subset(read.xlsx(path_death, sheet = "C16"),Sex =='Male');  C16.ca.m_death <- C16.ca.m_death[,-c(1:3)]; C16.ca.m_death[is.na(C16.ca.m_death)] <- 0;  C16.ca.m_death <-as.data.frame(t(C16.ca.m_death));colnames(C16.ca.m_death)=c(1:18)
C16.ca.w_death <-subset(read.xlsx(path_death, sheet = "C16"),Sex =='Female');C16.ca.w_death <- C16.ca.w_death[,-c(1:3)]; C16.ca.w_death[is.na(C16.ca.w_death)] <- 0;  C16.ca.w_death <-as.data.frame(t(C16.ca.w_death));colnames(C16.ca.w_death)=c(1:18)

C18_C21.ca.m_death <-subset(read.xlsx(path_death, sheet = "C18_C21"),Sex =='Male');  C18_C21.ca.m_death <- C18_C21.ca.m_death[,-c(1:3)]; C18_C21.ca.m_death[is.na(C18_C21.ca.m_death)] <- 0;  C18_C21.ca.m_death <-as.data.frame(t(C18_C21.ca.m_death));colnames(C18_C21.ca.m_death)=c(1:18)
C18_C21.ca.w_death <-subset(read.xlsx(path_death, sheet = "C18_C21"),Sex =='Female');C18_C21.ca.w_death <- C18_C21.ca.w_death[,-c(1:3)]; C18_C21.ca.w_death[is.na(C18_C21.ca.w_death)] <- 0;  C18_C21.ca.w_death <-as.data.frame(t(C18_C21.ca.w_death));colnames(C18_C21.ca.w_death)=c(1:18)

C22.ca.m_death <-subset(read.xlsx(path_death, sheet = "C22"),Sex =='Male');  C22.ca.m_death <- C22.ca.m_death[,-c(1:3)]; C22.ca.m_death[is.na(C22.ca.m_death)] <- 0;  C22.ca.m_death <-as.data.frame(t(C22.ca.m_death));colnames(C22.ca.m_death)=c(1:18)
C22.ca.w_death <-subset(read.xlsx(path_death, sheet = "C22"),Sex =='Female');C22.ca.w_death <- C22.ca.w_death[,-c(1:3)]; C22.ca.w_death[is.na(C22.ca.w_death)] <- 0;  C22.ca.w_death <-as.data.frame(t(C22.ca.w_death));colnames(C22.ca.w_death)=c(1:18)

C23_C24.ca.m_death <-subset(read.xlsx(path_death, sheet = "C23_C24"),Sex =='Male');  C23_C24.ca.m_death <- C23_C24.ca.m_death[,-c(1:3)]; C23_C24.ca.m_death[is.na(C23_C24.ca.m_death)] <- 0;  C23_C24.ca.m_death <-as.data.frame(t(C23_C24.ca.m_death));colnames(C23_C24.ca.m_death)=c(1:18)
C23_C24.ca.w_death <-subset(read.xlsx(path_death, sheet = "C23_C24"),Sex =='Female');C23_C24.ca.w_death <- C23_C24.ca.w_death[,-c(1:3)]; C23_C24.ca.w_death[is.na(C23_C24.ca.w_death)] <- 0;  C23_C24.ca.w_death <-as.data.frame(t(C23_C24.ca.w_death));colnames(C23_C24.ca.w_death)=c(1:18)

C25.ca.m_death <-subset(read.xlsx(path_death, sheet = "C25"),Sex =='Male');  C25.ca.m_death <- C25.ca.m_death[,-c(1:3)]; C25.ca.m_death[is.na(C25.ca.m_death)] <- 0;  C25.ca.m_death <-as.data.frame(t(C25.ca.m_death));colnames(C25.ca.m_death)=c(1:18)
C25.ca.w_death <-subset(read.xlsx(path_death, sheet = "C25"),Sex =='Female');C25.ca.w_death <- C25.ca.w_death[,-c(1:3)]; C25.ca.w_death[is.na(C25.ca.w_death)] <- 0;  C25.ca.w_death <-as.data.frame(t(C25.ca.w_death));colnames(C25.ca.w_death)=c(1:18)

C32.ca.m_death <-subset(read.xlsx(path_death, sheet = "C32"),Sex =='Male');  C32.ca.m_death <- C32.ca.m_death[,-c(1:3)]; C32.ca.m_death[is.na(C32.ca.m_death)] <- 0;  C32.ca.m_death <-as.data.frame(t(C32.ca.m_death));colnames(C32.ca.m_death)=c(1:18)
C32.ca.w_death <-subset(read.xlsx(path_death, sheet = "C32"),Sex =='Female');C32.ca.w_death <- C32.ca.w_death[,-c(1:3)]; C32.ca.w_death[is.na(C32.ca.w_death)] <- 0;  C32.ca.w_death <-as.data.frame(t(C32.ca.w_death));colnames(C32.ca.w_death)=c(1:18)

C33_C34.ca.m_death <-subset(read.xlsx(path_death, sheet = "C33_C34"),Sex =='Male');  C33_C34.ca.m_death <- C33_C34.ca.m_death[,-c(1:3)]; C33_C34.ca.m_death[is.na(C33_C34.ca.m_death)] <- 0;  C33_C34.ca.m_death <-as.data.frame(t(C33_C34.ca.m_death));colnames(C33_C34.ca.m_death)=c(1:18)
C33_C34.ca.w_death <-subset(read.xlsx(path_death, sheet = "C33_C34"),Sex =='Female');C33_C34.ca.w_death <- C33_C34.ca.w_death[,-c(1:3)]; C33_C34.ca.w_death[is.na(C33_C34.ca.w_death)] <- 0;  C33_C34.ca.w_death <-as.data.frame(t(C33_C34.ca.w_death));colnames(C33_C34.ca.w_death)=c(1:18)

# Sex-specific Cancer (Female)

C50.ca.m_death <-subset(read.xlsx(path_death, sheet = "C50"),Sex =='Male');  C50.ca.m_death <- C50.ca.m_death[,-c(1:3)]; C50.ca.m_death[is.na(C50.ca.m_death)] <- 0;  C50.ca.m_death <-as.data.frame(t(C50.ca.m_death));colnames(C50.ca.m_death)=c(1:18)
C50.ca.w_death <-subset(read.xlsx(path_death, sheet = "C50"),Sex =='Female');C50.ca.w_death <- C50.ca.w_death[,-c(1:3)]; C50.ca.w_death[is.na(C50.ca.w_death)] <- 0;  C50.ca.w_death <-as.data.frame(t(C50.ca.w_death));colnames(C50.ca.w_death)=c(1:18)

C53.ca.m_death <-subset(read.xlsx(path_death, sheet = "C53"),Sex =='Male');  C53.ca.m_death <- C53.ca.m_death[,-c(1:3)]; C53.ca.m_death[is.na(C53.ca.m_death)] <- 0;  C53.ca.m_death <-as.data.frame(t(C53.ca.m_death));colnames(C53.ca.m_death)=c(1:18)
C53.ca.w_death <-subset(read.xlsx(path_death, sheet = "C53"),Sex =='Female');C53.ca.w_death <- C53.ca.w_death[,-c(1:3)]; C53.ca.w_death[is.na(C53.ca.w_death)] <- 0;  C53.ca.w_death <-as.data.frame(t(C53.ca.w_death));colnames(C53.ca.w_death)=c(1:18)
#

C54_C55.ca.m_death <-subset(read.xlsx(path_death, sheet = "C54_C55"),Sex =='Male');  C54_C55.ca.m_death <- C54_C55.ca.m_death[,-c(1:3)]; C54_C55.ca.m_death[is.na(C54_C55.ca.m_death)] <- 0;  C54_C55.ca.m_death <-as.data.frame(t(C54_C55.ca.m_death));colnames(C54_C55.ca.m_death)=c(1:18)
C54_C55.ca.w_death <-subset(read.xlsx(path_death, sheet = "C54_C55"),Sex =='Female');C54_C55.ca.w_death <- C54_C55.ca.w_death[,-c(1:3)]; C54_C55.ca.w_death[is.na(C54_C55.ca.w_death)] <- 0;  C54_C55.ca.w_death <-as.data.frame(t(C54_C55.ca.w_death));colnames(C54_C55.ca.w_death)=c(1:18)

C56.ca.m_death <-subset(read.xlsx(path_death, sheet = "C56"),Sex =='Male');  C56.ca.m_death <- C56.ca.m_death[,-c(1:3)]; C56.ca.m_death[is.na(C56.ca.m_death)] <- 0;  C56.ca.m_death <-as.data.frame(t(C56.ca.m_death));colnames(C56.ca.m_death)=c(1:18)
C56.ca.w_death <-subset(read.xlsx(path_death, sheet = "C56"),Sex =='Female');C56.ca.w_death <- C56.ca.w_death[,-c(1:3)]; C56.ca.w_death[is.na(C56.ca.w_death)] <- 0;  C56.ca.w_death <-as.data.frame(t(C56.ca.w_death));colnames(C56.ca.w_death)=c(1:18)
#

# Sex-specific Cancer (Male)
C61.ca.m_death <-subset(read.xlsx(path_death, sheet = "C61"),Sex =='Male');  C61.ca.m_death <- C61.ca.m_death[,-c(1:3)]; C61.ca.m_death[is.na(C61.ca.m_death)] <- 0;  C61.ca.m_death <-as.data.frame(t(C61.ca.m_death));colnames(C61.ca.m_death)=c(1:18)
C61.ca.w_death <-subset(read.xlsx(path_death, sheet = "C61"),Sex =='Female');C61.ca.w_death <- C61.ca.w_death[,-c(1:3)]; C61.ca.w_death[is.na(C61.ca.w_death)] <- 0;  C61.ca.w_death <-as.data.frame(t(C61.ca.w_death));colnames(C61.ca.w_death)=c(1:18)

C60_C63.ca.m_death <-subset(read.xlsx(path_death, sheet = "C60_C63"),Sex =='Male');  C60_C63.ca.m_death <- C60_C63.ca.m_death[,-c(1:3)]; C60_C63.ca.m_death[is.na(C60_C63.ca.m_death)] <- 0;  C60_C63.ca.m_death <-as.data.frame(t(C60_C63.ca.m_death));colnames(C60_C63.ca.m_death)=c(1:18)
C60_C63.ca.w_death <-subset(read.xlsx(path_death, sheet = "C60_C63"),Sex =='Female');C60_C63.ca.w_death <- C60_C63.ca.w_death[,-c(1:3)]; C60_C63.ca.w_death[is.na(C60_C63.ca.w_death)] <- 0;  C60_C63.ca.w_death <-as.data.frame(t(C60_C63.ca.w_death));colnames(C60_C63.ca.w_death)=c(1:18)

C64.ca.m_death <-subset(read.xlsx(path_death, sheet = "C64"),Sex =='Male');  C64.ca.m_death <- C64.ca.m_death[,-c(1:3)]; C64.ca.m_death[is.na(C64.ca.m_death)] <- 0;  C64.ca.m_death <-as.data.frame(t(C64.ca.m_death));colnames(C64.ca.m_death)=c(1:18)
C64.ca.w_death <-subset(read.xlsx(path_death, sheet = "C64"),Sex =='Female');C64.ca.w_death <- C64.ca.w_death[,-c(1:3)]; C64.ca.w_death[is.na(C64.ca.w_death)] <- 0;  C64.ca.w_death <-as.data.frame(t(C64.ca.w_death));colnames(C64.ca.w_death)=c(1:18)
#

C67.ca.m_death <-subset(read.xlsx(path_death, sheet = "C67"),Sex =='Male');  C67.ca.m_death <- C67.ca.m_death[,-c(1:3)]; C67.ca.m_death[is.na(C67.ca.m_death)] <- 0;  C67.ca.m_death <-as.data.frame(t(C67.ca.m_death));colnames(C67.ca.m_death)=c(1:18)
C67.ca.w_death <-subset(read.xlsx(path_death, sheet = "C67"),Sex =='Female');C67.ca.w_death <- C67.ca.w_death[,-c(1:3)]; C67.ca.w_death[is.na(C67.ca.w_death)] <- 0;  C67.ca.w_death <-as.data.frame(t(C67.ca.w_death));colnames(C67.ca.w_death)=c(1:18)

C70_C72.ca.m_death <-subset(read.xlsx(path_death, sheet = "C70_C72"),Sex =='Male');  C70_C72.ca.m_death <- C70_C72.ca.m_death[,-c(1:3)]; C70_C72.ca.m_death[is.na(C70_C72.ca.m_death)] <- 0;  C70_C72.ca.m_death <-as.data.frame(t(C70_C72.ca.m_death));colnames(C70_C72.ca.m_death)=c(1:18)
C70_C72.ca.w_death <-subset(read.xlsx(path_death, sheet = "C70_C72"),Sex =='Female');C70_C72.ca.w_death <- C70_C72.ca.w_death[,-c(1:3)]; C70_C72.ca.w_death[is.na(C70_C72.ca.w_death)] <- 0;  C70_C72.ca.w_death <-as.data.frame(t(C70_C72.ca.w_death));colnames(C70_C72.ca.w_death)=c(1:18)

C73.ca.m_death <-subset(read.xlsx(path_death, sheet = "C73"),Sex =='Male');  C73.ca.m_death <- C73.ca.m_death[,-c(1:3)]; C73.ca.m_death[is.na(C73.ca.m_death)] <- 0;  C73.ca.m_death <-as.data.frame(t(C73.ca.m_death));colnames(C73.ca.m_death)=c(1:18)
C73.ca.w_death <-subset(read.xlsx(path_death, sheet = "C73"),Sex =='Female');C73.ca.w_death <- C73.ca.w_death[,-c(1:3)]; C73.ca.w_death[is.na(C73.ca.w_death)] <- 0;  C73.ca.w_death <-as.data.frame(t(C73.ca.w_death));colnames(C73.ca.w_death)=c(1:18)

C82_C86.ca.m_death <-subset(read.xlsx(path_death, sheet = "C82_C86"),Sex =='Male');  C82_C86.ca.m_death <- C82_C86.ca.m_death[,-c(1:3)]; C82_C86.ca.m_death[is.na(C82_C86.ca.m_death)] <- 0;  C82_C86.ca.m_death <-as.data.frame(t(C82_C86.ca.m_death));colnames(C82_C86.ca.m_death)=c(1:18)
C82_C86.ca.w_death <-subset(read.xlsx(path_death, sheet = "C82_C86"),Sex =='Female');C82_C86.ca.w_death <- C82_C86.ca.w_death[,-c(1:3)]; C82_C86.ca.w_death[is.na(C82_C86.ca.w_death)] <- 0;  C82_C86.ca.w_death <-as.data.frame(t(C82_C86.ca.w_death));colnames(C82_C86.ca.w_death)=c(1:18)

C90.ca.m_death <-subset(read.xlsx(path_death, sheet = "C90"),Sex =='Male');  C90.ca.m_death <- C90.ca.m_death[,-c(1:3)]; C90.ca.m_death[is.na(C90.ca.m_death)] <- 0;  C90.ca.m_death <-as.data.frame(t(C90.ca.m_death));colnames(C90.ca.m_death)=c(1:18)
C90.ca.w_death <-subset(read.xlsx(path_death, sheet = "C90"),Sex =='Female');C90.ca.w_death <- C90.ca.w_death[,-c(1:3)]; C90.ca.w_death[is.na(C90.ca.w_death)] <- 0;  C90.ca.w_death <-as.data.frame(t(C90.ca.w_death));colnames(C90.ca.w_death)=c(1:18)

C91_C95.ca.m_death <-subset(read.xlsx(path_death, sheet = "C91_C95"),Sex =='Male');  C91_C95.ca.m_death <- C91_C95.ca.m_death[,-c(1:3)]; C91_C95.ca.m_death[is.na(C91_C95.ca.m_death)] <- 0;  C91_C95.ca.m_death <-as.data.frame(t(C91_C95.ca.m_death));colnames(C91_C95.ca.m_death)=c(1:18)
C91_C95.ca.w_death <-subset(read.xlsx(path_death, sheet = "C91_C95"),Sex =='Female');C91_C95.ca.w_death <- C91_C95.ca.w_death[,-c(1:3)]; C91_C95.ca.w_death[is.na(C91_C95.ca.w_death)] <- 0;  C91_C95.ca.w_death <-as.data.frame(t(C91_C95.ca.w_death));colnames(C91_C95.ca.w_death)=c(1:18)

etc_C00_C97_.ca.m_death <-subset(read.xlsx(path_death, sheet = "etc(C00_C97)"),Sex =='Male');  etc_C00_C97_.ca.m_death <- etc_C00_C97_.ca.m_death[,-c(1:3)]; etc_C00_C97_.ca.m_death[is.na(etc_C00_C97_.ca.m_death)] <- 0;  etc_C00_C97_.ca.m_death <-as.data.frame(t(etc_C00_C97_.ca.m_death));colnames(etc_C00_C97_.ca.m_death)=c(1:18)
etc_C00_C97_.ca.w_death <-subset(read.xlsx(path_death, sheet = "etc(C00_C97)"),Sex =='Female');etc_C00_C97_.ca.w_death <- etc_C00_C97_.ca.w_death[,-c(1:3)]; etc_C00_C97_.ca.w_death[is.na(etc_C00_C97_.ca.w_death)] <- 0;  etc_C00_C97_.ca.w_death <-as.data.frame(t(etc_C00_C97_.ca.w_death));colnames(etc_C00_C97_.ca.w_death)=c(1:18)

########################################################################
# Aging adjusted Rate (65+)
########################################################################

AAF_total_65<-as.numeric(round(rowSums(wh_pop_men[,14:18]+wh_pop_women[,14:18])/rowSums(wh_pop_men+wh_pop_women)*100,4))  # aging-adjusted factor, inci ref 2000 / death ref 2005
AAF_total_75<-as.numeric(round(rowSums(wh_pop_men[,16:18]+wh_pop_women[,16:18])/rowSums(wh_pop_men+wh_pop_women)*100,4))  # aging-adjusted factor, inci ref 2000 / death ref 2005

AAF_Inci_65 <- as.numeric(round(AAF_total_65/AAF_total_65[18],4)) # Incidence reference (2000)
AAF_Mort_65 <- as.numeric(round(AAF_total_65/AAF_total_65[23],4)) # Death reference (2005)

AAF_Inci_75 <- as.numeric(round(AAF_total_75/AAF_total_75[18],4)) # Incidence reference (2000)
AAF_Mort_75 <- as.numeric(round(AAF_total_75/AAF_total_75[23],4)) # Death reference (2005)

YEAR <-seq(1983,2039)


AAF65<-data.frame(YEAR,Proportion = AAF_total_65, Incidence_AAF=AAF_Inci_65, Mortality_AAF = AAF_Mort_65)
AAF75<-data.frame(YEAR,Proportion = AAF_total_75, Incidence_AAF=AAF_Inci_75, Mortality_AAF = AAF_Mort_75)


library(formattable)
formattable(AAF65)
formattable(AAF75)


########################################################################################################

M_Ca_inci_Name <-c("C00_C96.ca.m","C00_C14.ca.m",'C15.ca.m','C16.ca.m','C18_C21.ca.m','C23_C24.ca.m',
                   'C25.ca.m','C32.ca.m','C33_C34.ca.m','C61.ca.m','C60_C63.ca.m',
                   'C64.ca.m','C67.ca.m','C70_C72.ca.m','C73.ca.m','C81.ca.m','C82_C86_C96.ca.m','C90.ca.m','C91_C95.ca.m','etc_C00_C96_.ca.m')

W_Ca_inci_Name <-c("C00_C96.ca.w","C00_C14.ca.w",'C15.ca.w','C16.ca.w','C18_C21.ca.w','C23_C24.ca.w',
                   'C25.ca.w','C32.ca.w','C33_C34.ca.w','C50.ca.w','C53.ca.w','C54_C55.ca.w','C56.ca.w',
                   'C64.ca.w','C67.ca.w','C70_C72.ca.w','C73.ca.w','C81.ca.w','C82_C86_C96.ca.w','C90.ca.w','C91_C95.ca.w','etc_C00_C96_.ca.w')

M_Ca_D_Name    <-c("C00_C97.ca.m_death","C00_C14.ca.m_death",'C15.ca.m_death','C16.ca.m_death','C18_C21.ca.m_death','C23_C24.ca.m_death',
                   'C25.ca.m_death','C32.ca.m_death','C33_C34.ca.m_death','C61.ca.m_death','C60_C63.ca.m_death',
                   'C64.ca.m_death','C67.ca.m_death','C70_C72.ca.m_death','C73.ca.m_death','C82_C86.ca.m_death','C90.ca.m_death','C91_C95.ca.m_death','etc_C00_C97_.ca.m_death')

W_Ca_D_Name    <-c("C00_C97.ca.w_death","C00_C14.ca.w_death",'C15.ca.w_death','C16.ca.w_death','C18_C21.ca.w_death','C23_C24.ca.w_death',
                   'C25.ca.w_death','C32.ca.w_death','C33_C34.ca.w_death','C50.ca.w_death','C53.ca.w_death','C54_C55.ca.w_death','C56.ca.w_death',
                   'C64.ca.w_death','C67.ca.w_death','C70_C72.ca.w_death','C73.ca.w_death','C82_C86.ca.w_death','C90.ca.w_death','C91_C95.ca.w_death','etc_C00_C97_.ca.w_death')


###################### PLOT #####################################

Ini_Year <-c(1999:2019)
De_Year  <-c(1983:2021)


for (i in 1:20) {
  A<-get(M_Ca_inci_Name[i])/wh_pop_men[17:37,]*100000 # 연령별 발생률
  B<-colSums(wh_pop_men[18,] + wh_pop_women[18,]) # 표준인구의 연령별 인구
  C <-sum(wh_pop_men[18,] + wh_pop_women[18,]) # 표준인구
  ASIR <- rowSums(A*B) / C # 연령표준화 발생률
  
  IR65<-rowSums(get(M_Ca_inci_Name[i])[,14:18])/rowSums(wh_pop_men[17:37,14:18])*100000 # 65세 이상 발생률
  
  AAIR<- IR65 * AAF_Inci_65[seq(17,37,1)]
  
  assign(paste0(M_Ca_inci_Name[i],"_comp_m_ini"),as.data.frame(setNames(as.data.frame(cbind(Ini_Year,ASIR,AAIR,IR65)),c("YEAR","ASIR","AAIR","IR65"))))
  
  }

for (i in 1:19) {
  A<-get(M_Ca_D_Name[i])/wh_pop_men[1:39,]*100000 # 연령별 사망률
  A[is.na(A)] = 0
  B<-colSums(wh_pop_men[23,] + wh_pop_women[23,]) # 표준인구의 연령별 인구
  C <-sum(wh_pop_men[23,] + wh_pop_women[23,]) # 표준인구
  ASMR <- rowSums(A*B) / C # 연령표준화 발생률
  
  MR65<-rowSums(get(M_Ca_D_Name[i])[,14:18])/rowSums(wh_pop_men[1:39,14:18])*100000 # 65세 이상 발생률
  
  AAMR<- MR65 * AAF_Mort_65[seq(1,39,1)]
  
  assign(paste0(M_Ca_D_Name[i],"_comp_m_de"),as.data.frame(setNames(as.data.frame(cbind(De_Year,ASMR,AAMR,MR65)),c("YEAR","ASMR","AAMR","MR65"))))
  }


for (i in 1:22) {
  A<-get(W_Ca_inci_Name[i])/wh_pop_women[17:37,]*100000 # 연령별 발생률
  B<-colSums(wh_pop_men[18,] + wh_pop_women[18,]) # 표준인구의 연령별 인구
  C <-sum(wh_pop_men[18,] + wh_pop_women[18,]) # 표준인구
  ASIR <- rowSums(A*B) / C # 연령표준화 발생률
  
  IR65<-rowSums(get(W_Ca_inci_Name[i])[,14:18])/rowSums(wh_pop_women[17:37,14:18])*100000 # 65세 이상 발생률
  
  AAIR<- IR65 * AAF_Inci_65[seq(17,37,1)]
  
  assign(paste0(W_Ca_inci_Name[i],"_comp_w_ini"),as.data.frame(setNames(as.data.frame(cbind(Ini_Year,ASIR,AAIR,IR65)),c("YEAR","ASIR","AAIR","IR65"))))
  } 

for (i in 1:21) {
  A<-get(W_Ca_D_Name[i])/wh_pop_women[1:39,]*100000 # 연령별 사망률
  A[is.na(A)] = 0
  B<-colSums(wh_pop_men[23,] + wh_pop_women[23,]) # 표준인구의 연령별 인구
  C <-sum(wh_pop_men[23,] + wh_pop_women[23,]) # 표준인구
  ASMR <- rowSums(A*B) / C # 연령표준화 발생률

  MR65<-rowSums(get(W_Ca_D_Name[i])[,14:18])/rowSums(wh_pop_women[1:39,14:18])*100000 # 65세 이상 발생률

  AAMR<- MR65 * AAF_Mort_65[seq(1,39,1)]
  
  assign(paste0(W_Ca_D_Name[i],"_comp_w_de"),as.data.frame(setNames(as.data.frame(cbind(De_Year,ASMR,AAMR,MR65)),c("YEAR","ASMR","AAMR","MR65"))))
  }


################################################################################################################################################
################################################################################################################################################

# Male Incidence
for(i in 1:20){
  assign(paste0(M_Ca_inci_Name[i],"_plot"),ggplot(data=get(paste0(M_Ca_inci_Name[i],"_comp_m_ini")))+
           geom_line(aes(x=YEAR, y=ASIR))+
           geom_line(data=get(paste0(M_Ca_inci_Name[i],"_comp_m_ini")),aes(x=YEAR, y=AAIR),color = '#3399FF') +
           geom_line(data=get(paste0(M_Ca_inci_Name[i],"_comp_m_ini")),aes(x=YEAR, y=IR65),color = '#3399FF',linetype = 2) +
           scale_x_continuous(breaks = seq(1999, 2019, by = 2)) + 
           ylab('Incidence Rate/100,000') + 
           theme(panel.background = element_blank()))
} 

ggarrange(get(paste0(M_Ca_inci_Name[1],"_plot")), get(paste0(M_Ca_inci_Name[2],"_plot")),
          get(paste0(M_Ca_inci_Name[3],"_plot")), get(paste0(M_Ca_inci_Name[4],"_plot")),
          get(paste0(M_Ca_inci_Name[5],"_plot")), get(paste0(M_Ca_inci_Name[6],"_plot")),
          labels = c("C00_C96","C00_C14","C15","C16","C18_C21","C22"),ncol = 2, nrow = 3)

ggarrange(get(paste0(M_Ca_inci_Name[7],"_plot")), get(paste0(M_Ca_inci_Name[8],"_plot")),
          get(paste0(M_Ca_inci_Name[9],"_plot")), get(paste0(M_Ca_inci_Name[10],"_plot")),
          get(paste0(M_Ca_inci_Name[11],"_plot")),get(paste0(M_Ca_inci_Name[12],"_plot")),
          labels = c("C23_C24","C25","C32","C33_C34","C61","C60_C63"),ncol = 2, nrow = 3)

ggarrange(get(paste0(M_Ca_inci_Name[13],"_plot")), get(paste0(M_Ca_inci_Name[14],"_plot")),
          get(paste0(M_Ca_inci_Name[15],"_plot")), get(paste0(M_Ca_inci_Name[16],"_plot")),
          get(paste0(M_Ca_inci_Name[16],"_plot")), get(paste0(M_Ca_inci_Name[17],"_plot")),
          labels = c("C64","C67","C70_C72","C73","C81","C82_C86_C96"),ncol = 2, nrow = 3)

ggarrange(get(paste0(M_Ca_inci_Name[18],"_plot")), get(paste0(M_Ca_inci_Name[19],"_plot")),
          get(paste0(M_Ca_inci_Name[20],"_plot")),
          labels = c("C90","C91_C95","etc(C00_C96)"),ncol = 2, nrow = 3)


## Male Mortality
for (i in 1: 19){
  assign(paste0(M_Ca_D_Name[i],"_plot"),ggplot(data=get(paste0(M_Ca_D_Name[i],"_comp_m_de")))+
           geom_line(aes(x=YEAR, y=ASMR))+
           geom_line(data=get(paste0(M_Ca_D_Name[i],"_comp_m_de")),aes(x=YEAR, y=AAMR),color = '#3399FF') +
           geom_line(data=get(paste0(M_Ca_D_Name[i],"_comp_m_de")),aes(x=YEAR, y=MR65),color = '#3399FF',linetype = 2) +
           scale_x_continuous(breaks = seq(1983, 2021, by = 2)) + 
           ylab('Mortality Rate/100,000') + 
           theme(panel.background = element_blank()))
} 

ggarrange(get(paste0(M_Ca_D_Name[1],"_plot")), get(paste0(M_Ca_D_Name[2],"_plot")),
          get(paste0(M_Ca_D_Name[3],"_plot")), get(paste0(M_Ca_D_Name[4],"_plot")),
          get(paste0(M_Ca_D_Name[5],"_plot")), get(paste0(M_Ca_D_Name[6],"_plot")),
          labels = c("C00_C97","C00_C14","C15","C16","C18_C21","C22"),ncol = 2, nrow = 3)

ggarrange(get(paste0(M_Ca_D_Name[7],"_plot")), get(paste0(M_Ca_D_Name[8],"_plot")),
          get(paste0(M_Ca_D_Name[9],"_plot")), get(paste0(M_Ca_D_Name[10],"_plot")),
          get(paste0(M_Ca_D_Name[11],"_plot")),get(paste0(M_Ca_D_Name[12],"_plot")),
          labels = c("C23_C24","C25","C32","C33_C34","C61","C60_C63"),ncol = 2, nrow = 3)

ggarrange(get(paste0(M_Ca_D_Name[13],"_plot")), get(paste0(M_Ca_D_Name[14],"_plot")),
          get(paste0(M_Ca_D_Name[15],"_plot")), get(paste0(M_Ca_D_Name[16],"_plot")),
          get(paste0(M_Ca_D_Name[16],"_plot")), get(paste0(M_Ca_D_Name[17],"_plot")),
          labels = c("C64","C67","C70_C72","C73","C82_C86","C90"),ncol = 2, nrow = 3)

ggarrange(get(paste0(M_Ca_D_Name[18],"_plot")), get(paste0(M_Ca_inci_Name[19],"_plot")),
          labels = c("C91_C95","etc(C00_C97)"),ncol = 2, nrow = 3)



######################################################

# Female Incidence
for(i in 1:22){
  assign(paste0(W_Ca_inci_Name[i],"_plot"),ggplot(data=get(paste0(W_Ca_inci_Name[i],"_comp_w_ini")))+
           geom_line(aes(x=YEAR, y=ASIR))+
           geom_line(data=get(paste0(W_Ca_inci_Name[i],"_comp_w_ini")),aes(x=YEAR, y=AAIR),color = '#3399FF') +
           geom_line(data=get(paste0(W_Ca_inci_Name[i],"_comp_w_ini")),aes(x=YEAR, y=IR65),color = '#3399FF',linetype = 2) +
           scale_x_continuous(breaks = seq(1999, 2019, by = 2)) + 
           ylab('Incidence Rate/100,000') + 
           theme(panel.background = element_blank()))
} 

ggarrange(get(paste0(W_Ca_inci_Name[1],"_plot")), get(paste0(W_Ca_inci_Name[2],"_plot")),
          get(paste0(W_Ca_inci_Name[3],"_plot")), get(paste0(W_Ca_inci_Name[4],"_plot")),
          get(paste0(W_Ca_inci_Name[5],"_plot")), get(paste0(W_Ca_inci_Name[6],"_plot")),
          labels = c("C00_C96","C00_C14","C15","C16","C18_C21","C22"),ncol = 2, nrow = 3)

ggarrange(get(paste0(W_Ca_inci_Name[7],"_plot")), get(paste0(W_Ca_inci_Name[8],"_plot")),
          get(paste0(W_Ca_inci_Name[9],"_plot")), get(paste0(W_Ca_inci_Name[10],"_plot")),
          get(paste0(W_Ca_inci_Name[11],"_plot")),get(paste0(W_Ca_inci_Name[12],"_plot")),
          labels = c("C23_C24","C25","C32","C33_C34","C50","C53"),ncol = 2, nrow = 3)

ggarrange(get(paste0(W_Ca_inci_Name[13],"_plot")), get(paste0(W_Ca_inci_Name[14],"_plot")),
          get(paste0(W_Ca_inci_Name[15],"_plot")), get(paste0(W_Ca_inci_Name[16],"_plot")),
          get(paste0(W_Ca_inci_Name[16],"_plot")), get(paste0(W_Ca_inci_Name[17],"_plot")),
          labels = c("C54_C55","C56","C64","C67","C70_C72","C73"),ncol = 2, nrow = 3)

ggarrange(get(paste0(W_Ca_inci_Name[18],"_plot")), get(paste0(W_Ca_inci_Name[19],"_plot")),
          get(paste0(W_Ca_inci_Name[20],"_plot")), get(paste0(W_Ca_inci_Name[21],"_plot")),
          get(paste0(W_Ca_inci_Name[22],"_plot")),
          labels = c("C81","C82_C86_C96","C90","C91_C95","etc(C00_C96)"),ncol = 2, nrow = 3)

## Female Mortality
for (i in 1: 21){
  assign(paste0(W_Ca_D_Name[i],"_plot"),ggplot(data=get(paste0(W_Ca_D_Name[i],"_comp_w_de")))+
           geom_line(aes(x=YEAR, y=ASMR))+
           geom_line(data=get(paste0(W_Ca_D_Name[i],"_comp_w_de")),aes(x=YEAR, y=AAMR),color = '#3399FF') +
           geom_line(data=get(paste0(W_Ca_D_Name[i],"_comp_w_de")),aes(x=YEAR, y=MR65),color = '#3399FF',linetype = 2) +
           scale_x_continuous(breaks = seq(1983, 2021, by = 2)) + 
           ylab('Mortality Rate/100,000') + 
           theme(panel.background = element_blank()))
} 

ggarrange(get(paste0(W_Ca_D_Name[1],"_plot")), get(paste0(W_Ca_D_Name[2],"_plot")),
          get(paste0(W_Ca_D_Name[3],"_plot")), get(paste0(W_Ca_D_Name[4],"_plot")),
          get(paste0(W_Ca_D_Name[5],"_plot")), get(paste0(W_Ca_D_Name[6],"_plot")),
          labels = c("C00_C97","C00_C14","C15","C16","C18_C21","C22"),ncol = 2, nrow = 3)

ggarrange(get(paste0(W_Ca_D_Name[7],"_plot")), get(paste0(W_Ca_D_Name[8],"_plot")),
          get(paste0(W_Ca_D_Name[9],"_plot")), get(paste0(W_Ca_D_Name[10],"_plot")),
          get(paste0(W_Ca_D_Name[11],"_plot")),get(paste0(W_Ca_D_Name[12],"_plot")),
          labels = c("C23_C24","C25","C32","C33_C34","C50","C53"),ncol = 2, nrow = 3)

ggarrange(get(paste0(W_Ca_D_Name[13],"_plot")), get(paste0(W_Ca_D_Name[14],"_plot")),
          get(paste0(W_Ca_D_Name[15],"_plot")), get(paste0(W_Ca_D_Name[16],"_plot")),
          get(paste0(W_Ca_D_Name[16],"_plot")), get(paste0(W_Ca_D_Name[17],"_plot")),
          labels = c("C54_C55","C56","C64","C67","C70_C72","C73"),ncol = 2, nrow = 3)

ggarrange(get(paste0(W_Ca_D_Name[18],"_plot")), get(paste0(M_Ca_inci_Name[19],"_plot")),
          get(paste0(W_Ca_D_Name[20],"_plot")), get(paste0(W_Ca_D_Name[21],"_plot")),
          labels = c("C82_C86","C90","C91_C95","etc(C00_C97)"),ncol = 2, nrow = 3)



########################################################################

# Whether Dominated or not?

for (i in 1:20){
  assign(paste0(M_Ca_inci_Name[i],"_M_DR"),((rowSums(get(M_Ca_inci_Name[i])[,14:18]) /rowSums(wh_pop_men[17:37,14:18])*100000)/(rowSums(get(M_Ca_inci_Name[i])[,1:13])/rowSums(wh_pop_men[17:37,1:13])*100000)))
}
for (i in 1:19){
  assign(paste0(M_Ca_D_Name[i],"_M_Death_DR"),((rowSums(get(M_Ca_D_Name[i])[,14:18]) /rowSums(wh_pop_men[1:39,14:18])*100000)/(rowSums(get(M_Ca_D_Name[i])[,1:13])/rowSums(wh_pop_men[1:39,1:13])*100000)))
}
for (i in 1:22){
  assign(paste0(W_Ca_inci_Name[i],"_W_DR"),((rowSums(get(W_Ca_inci_Name[i])[,14:18]) /rowSums(wh_pop_women[17:37,14:18])*100000)/(rowSums(get(W_Ca_inci_Name[i])[,1:13])/rowSums(wh_pop_women[17:37,1:13])*100000)))
}
for (i in 1:21){
  assign(paste0(W_Ca_D_Name[i],"_W_Death_DR"),((rowSums(get(W_Ca_D_Name[i])[,14:18]) /rowSums(wh_pop_women[1:39,14:18])*100000)/(rowSums(get(W_Ca_D_Name[i])[,1:13])/rowSums(wh_pop_women[1:39,1:13])*100000)))
}

M_inci_Name <-c("C00_C96","C00_C14",'C15','C16','C18_C21','C23_C24',
                   'C25','C32','C33_C34','C61','C60_C63',
                   'C64','C67','C70_C72','C73','C81','C82_C86_C96','C90','C91_C95','etc(C00_C96)')

W_inci_Name <-c("C00_C96","C00_C14",'C15','C16','C18_C21','C23_C24','
                C25','C32','C33_C34','C50','C53','C54_C55','C56','C64',
                'C67','C70_C72','C73','C81','C82_C86_C96','C90','C91_C95','etc(C00_C96)')

M_D_Name    <-c("C00_C97","C00_C14",'C15','C16','C18_C21','C23_C24',
                   'C25','C32','C33_C34','C61','C60_C63',
                   'C64','C67','C70_C72','C73','C82_C86','C90','C91_C95','etc(C00_C97)')

W_D_Name    <-c("C00_C97","C00_C14",'C15','C16','C18_C21','C23_C24',
                   'C25','C32','C33_C34','C50','C53','C54_C55','C56',
                   'C64','C67','C70_C72','C73','C82_C86','C90','C91_C95','etc(C00_C97)')


MaleIncDR<-as.data.frame(cbind(get(paste0(M_Ca_inci_Name[1],"_M_DR")),get(paste0(M_Ca_inci_Name[2],"_M_DR")),get(paste0(M_Ca_inci_Name[3],"_M_DR")),get(paste0(M_Ca_inci_Name[4],"_M_DR")),
      get(paste0(M_Ca_inci_Name[5],"_M_DR")),get(paste0(M_Ca_inci_Name[5],"_M_DR")),get(paste0(M_Ca_inci_Name[7],"_M_DR")),get(paste0(M_Ca_inci_Name[8],"_M_DR")),
      get(paste0(M_Ca_inci_Name[9],"_M_DR")),get(paste0(M_Ca_inci_Name[10],"_M_DR")),get(paste0(M_Ca_inci_Name[11],"_M_DR")),get(paste0(M_Ca_inci_Name[12],"_M_DR")),
      get(paste0(M_Ca_inci_Name[13],"_M_DR")),get(paste0(M_Ca_inci_Name[14],"_M_DR")),get(paste0(M_Ca_inci_Name[15],"_M_DR")),get(paste0(M_Ca_inci_Name[16],"_M_DR")),
      get(paste0(M_Ca_inci_Name[17],"_M_DR")),get(paste0(M_Ca_inci_Name[18],"_M_DR")),get(paste0(M_Ca_inci_Name[19],"_M_DR")),get(paste0(M_Ca_inci_Name[20],"_M_DR"))))

MaleDeDR<-as.data.frame(cbind(get(paste0(M_Ca_D_Name[1],"_M_Death_DR")),get(paste0(M_Ca_D_Name[2],"_M_Death_DR")),get(paste0(M_Ca_D_Name[3],"_M_Death_DR")),get(paste0(M_Ca_D_Name[4],"_M_Death_DR")),
                 get(paste0(M_Ca_D_Name[5],"_M_Death_DR")),get(paste0(M_Ca_D_Name[5],"_M_Death_DR")),get(paste0(M_Ca_D_Name[7],"_M_Death_DR")),get(paste0(M_Ca_D_Name[8],"_M_Death_DR")),
                 get(paste0(M_Ca_D_Name[9],"_M_Death_DR")),get(paste0(M_Ca_D_Name[10],"_M_Death_DR")),get(paste0(M_Ca_D_Name[11],"_M_Death_DR")),get(paste0(M_Ca_D_Name[12],"_M_Death_DR")),
                 get(paste0(M_Ca_D_Name[13],"_M_Death_DR")),get(paste0(M_Ca_D_Name[14],"_M_Death_DR")),get(paste0(M_Ca_D_Name[15],"_M_Death_DR")),get(paste0(M_Ca_D_Name[16],"_M_Death_DR")),
                 get(paste0(M_Ca_D_Name[17],"_M_Death_DR")),get(paste0(M_Ca_D_Name[18],"_M_Death_DR")),get(paste0(M_Ca_D_Name[19],"_M_Death_DR"))))

colnames(MaleIncDR) <- M_inci_Name
colnames(MaleDeDR)  <- M_D_Name

MaleIncDR<-round(MaleIncDR,1)
MaleDeDR<-round(MaleDeDR,1)

MaleIncDR<-cbind(Ini_Year,MaleIncDR)
MaleDeDR<-cbind(De_Year,MaleDeDR)

MaleIncDR<-melt(data = MaleIncDR, id.vars = c("Ini_Year"))
MaleDeDR <-melt(data = MaleDeDR, id.vars = c("De_Year"))

colnames(MaleIncDR) <-c("Year","Cancer","DR")
colnames(MaleDeDR)  <-c("Year","Cancer","DR")


FemaleIncDR<-as.data.frame(cbind(get(paste0(W_Ca_inci_Name[1],"_W_DR")),get(paste0(W_Ca_inci_Name[2],"_W_DR")),get(paste0(W_Ca_inci_Name[3],"_W_DR")),get(paste0(W_Ca_inci_Name[4],"_W_DR")),
                 get(paste0(W_Ca_inci_Name[5],"_W_DR")),get(paste0(W_Ca_inci_Name[5],"_W_DR")),get(paste0(W_Ca_inci_Name[7],"_W_DR")),get(paste0(W_Ca_inci_Name[8],"_W_DR")),
                 get(paste0(W_Ca_inci_Name[9],"_W_DR")),get(paste0(W_Ca_inci_Name[10],"_W_DR")),get(paste0(W_Ca_inci_Name[11],"_W_DR")),get(paste0(W_Ca_inci_Name[12],"_W_DR")),
                 get(paste0(W_Ca_inci_Name[13],"_W_DR")),get(paste0(W_Ca_inci_Name[14],"_W_DR")),get(paste0(W_Ca_inci_Name[15],"_W_DR")),get(paste0(W_Ca_inci_Name[16],"_W_DR")),
                 get(paste0(W_Ca_inci_Name[17],"_W_DR")),get(paste0(W_Ca_inci_Name[18],"_W_DR")),get(paste0(W_Ca_inci_Name[19],"_W_DR")),get(paste0(W_Ca_inci_Name[20],"_W_DR")),
                 get(paste0(W_Ca_inci_Name[21],"_W_DR")),get(paste0(W_Ca_inci_Name[22],"_W_DR"))))

FemaleDeDR<-as.data.frame(cbind(get(paste0(W_Ca_D_Name[1],"_W_Death_DR")),get(paste0(W_Ca_D_Name[2],"_W_Death_DR")),get(paste0(W_Ca_D_Name[3],"_W_Death_DR")),get(paste0(W_Ca_D_Name[4],"_W_Death_DR")),
                get(paste0(W_Ca_D_Name[5],"_W_Death_DR")),get(paste0(W_Ca_D_Name[5],"_W_Death_DR")),get(paste0(W_Ca_D_Name[7],"_W_Death_DR")),get(paste0(W_Ca_D_Name[8],"_W_Death_DR")),
                get(paste0(W_Ca_D_Name[9],"_W_Death_DR")),get(paste0(W_Ca_D_Name[10],"_W_Death_DR")),get(paste0(W_Ca_D_Name[11],"_W_Death_DR")),get(paste0(W_Ca_D_Name[12],"_W_Death_DR")),
                get(paste0(W_Ca_D_Name[13],"_W_Death_DR")),get(paste0(W_Ca_D_Name[14],"_W_Death_DR")),get(paste0(W_Ca_D_Name[15],"_W_Death_DR")),get(paste0(W_Ca_D_Name[16],"_W_Death_DR")),
                get(paste0(W_Ca_D_Name[17],"_W_Death_DR")),get(paste0(W_Ca_D_Name[18],"_W_Death_DR")),get(paste0(W_Ca_D_Name[19],"_W_Death_DR")),get(paste0(W_Ca_D_Name[20],"_W_Death_DR")),
                get(paste0(W_Ca_D_Name[21],"_W_Death_DR"))))

colnames(FemaleIncDR) <- W_inci_Name
colnames(FemaleDeDR)  <- W_D_Name

FemaleIncDR<-round(FemaleIncDR,1)
FemaleDeDR<-round(FemaleDeDR,1)

FemaleIncDR<-cbind(Ini_Year,FemaleIncDR)
FemaleDeDR<-cbind(De_Year,FemaleDeDR)

FemaleIncDR<-melt(data = FemaleIncDR, id.vars = c("Ini_Year"))
FemaleDeDR <-melt(data = FemaleDeDR, id.vars = c("De_Year"))

colnames(FemaleIncDR) <-c("Year","Cancer","DR")
colnames(FemaleDeDR)  <-c("Year","Cancer","DR")

## Heatmap DR
ggplot(MaleIncDR, aes(x=Cancer, y=Year, fill=DR)) +
  geom_tile(colour="black", linetype="dashed") +
  scale_fill_gradient2(low="darkred",high="darkgreen",breaks=c(0,1,2,10,15,20,25,30,40,50,60,65),
                      limits=c(0,65)) +
  geom_text(aes(label=DR),color='black')

ggplot(FemaleIncDR, aes(x=Cancer, y=Year, fill=DR)) +
  geom_tile(colour="black", linetype="dashed") +
  scale_fill_gradient2(low="darkred",high="darkgreen",breaks=c(0,1,2,10,15,20,25,30,40,50,60,65),
                       limits=c(0,65)) +
  geom_text(aes(label=DR),color='black')

ggplot(MaleDeDR, aes(x=Cancer, y=Year, fill=DR)) +
  geom_tile(colour="black", linetype="dashed") +
  scale_fill_gradient2(low="darkred",high="darkgreen",breaks=c(0,1,2,10,15,20,25,30,40,50,60,65,80,90,100,120,150),
                       limits=c(0,150)) +
  geom_text(aes(label=DR),color='black')

ggplot(FemaleDeDR, aes(x=Cancer, y=Year, fill=DR)) +
  geom_tile(colour="black", linetype="dashed") +
  scale_fill_gradient2(low="darkred",high="darkgreen",breaks=c(0,1,2,10,15,20,25,30,40,50,60,65,80,90,100,120,150),
                       limits=c(0,150)) +
  geom_text(aes(label=DR),color='black')

########################################################################

# Whether Shift or not? (trend analysis)

for (i in 1:20) {
  AR <-rowSums(get(M_Ca_inci_Name[i])[,14:18])/rowSums(wh_pop_men[17:37,14:18])*100000 # 65에 이상 발생률
  YR <-rowSums(get(M_Ca_inci_Name[i])[,1:13]) /rowSums(wh_pop_men[17:37,1:13]) *100000 # 65세 미만 사망률
  assign(paste0(M_Ca_inci_Name[i],"_male_in_shift"),as.data.frame(setNames(as.data.frame(cbind(Ini_Year,AR,YR)),c("YEAR","Aging","Young"))))
}

for (i in 1:19) {
  AR <-rowSums(get(M_Ca_D_Name[i])[,14:18])/rowSums(wh_pop_men[1:39,14:18])*100000 # 65에 이상 발생률
  YR <-rowSums(get(M_Ca_D_Name[i])[,1:13]) /rowSums(wh_pop_men[1:39,1:13]) *100000 # 65세 미만 사망률
  assign(paste0(M_Ca_D_Name[i],"_male_de_shift"),as.data.frame(setNames(as.data.frame(cbind(De_Year,AR,YR)),c("YEAR","Aging","Young"))))
}

for (i in 1:22) {
  AR <-rowSums(get(W_Ca_inci_Name[i])[,14:18])/rowSums(wh_pop_women[17:37,14:18])*100000 # 65에 이상 발생률
  YR <-rowSums(get(W_Ca_inci_Name[i])[,1:13]) /rowSums(wh_pop_women[17:37,1:13]) *100000 # 65세 미만 사망률
  assign(paste0(W_Ca_inci_Name[i],"_female_in_shift"),as.data.frame(setNames(as.data.frame(cbind(Ini_Year,AR,YR)),c("YEAR","Aging","Young"))))
}

for (i in 1:21) {
  AR <-rowSums(get(W_Ca_D_Name[i])[,14:18])/rowSums(wh_pop_women[1:39,14:18])*100000 # 65에 이상 발생률
  YR <-rowSums(get(W_Ca_D_Name[i])[,1:13]) /rowSums(wh_pop_women[1:39,1:13]) *100000 # 65세 미만 사망률
  assign(paste0(W_Ca_D_Name[i],"_female_de_shift"),as.data.frame(setNames(as.data.frame(cbind(De_Year,AR,YR)),c("YEAR","Aging","Young"))))
}

############

for (i in 1: 20){
  assign(paste0(M_Ca_inci_Name[i],"_shiftplot"),ggplot(data=get(paste0(M_Ca_inci_Name[i],"_male_in_shift")))+
           geom_line(data=get(paste0(M_Ca_inci_Name[i],"_male_in_shift")),aes(x=YEAR, y=Aging),color = '#3399FF') +
           geom_line(data=get(paste0(M_Ca_inci_Name[i],"_male_in_shift")),aes(x=YEAR, y=Young),color = '#3399FF',linetype = 2) +
           scale_x_continuous(breaks = seq(1999, 2019, by = 2)) + 
           ylab('Incidence Rate/100,000') + 
           theme(panel.background = element_blank()))
} 

ggarrange(get(paste0(M_Ca_inci_Name[1],"_shiftplot")), get(paste0(M_Ca_inci_Name[2],"_shiftplot")),
          get(paste0(M_Ca_inci_Name[3],"_shiftplot")), get(paste0(M_Ca_inci_Name[4],"_shiftplot")),
          get(paste0(M_Ca_inci_Name[5],"_shiftplot")), get(paste0(M_Ca_inci_Name[6],"_shiftplot")),
          labels = c("C00_C96","C00_C14","C15","C16","C18_C21","C22"),ncol = 2, nrow = 3)

ggarrange(get(paste0(M_Ca_inci_Name[7],"_shiftplot")), get(paste0(M_Ca_inci_Name[8],"_shiftplot")),
          get(paste0(M_Ca_inci_Name[9],"_shiftplot")), get(paste0(M_Ca_inci_Name[10],"_shiftplot")),
          get(paste0(M_Ca_inci_Name[11],"_shiftplot")),get(paste0(M_Ca_inci_Name[12],"_shiftplot")),
          labels = c("C23_C24","C25","C32","C33_C34","C61","C60_C63"),ncol = 2, nrow = 3)

ggarrange(get(paste0(M_Ca_inci_Name[13],"_shiftplot")), get(paste0(M_Ca_inci_Name[14],"_shiftplot")),
          get(paste0(M_Ca_inci_Name[15],"_shiftplot")), get(paste0(M_Ca_inci_Name[16],"_shiftplot")),
          get(paste0(M_Ca_inci_Name[16],"_shiftplot")), get(paste0(M_Ca_inci_Name[17],"_shiftplot")),
          labels = c("C64","C67","C70_C72","C73","C81","C82_C86_C96"),ncol = 2, nrow = 3)

ggarrange(get(paste0(M_Ca_inci_Name[18],"_shiftplot")), get(paste0(M_Ca_inci_Name[19],"_shiftplot")),
          get(paste0(M_Ca_inci_Name[20],"_shiftplot")),
          labels = c("C90","C91_C95","etc(C00_C96)"),ncol = 2, nrow = 3)




for (i in 1: 19){
  assign(paste0(M_Ca_D_Name[i],"_shiftplot"),ggplot(data=get(paste0(M_Ca_D_Name[i],"_male_de_shift")))+
           geom_line(data=get(paste0(M_Ca_D_Name[i],"_male_de_shift")),aes(x=YEAR, y=Aging),color = '#3399FF') +
           geom_line(data=get(paste0(M_Ca_D_Name[i],"_male_de_shift")),aes(x=YEAR, y=Young),color = '#3399FF',linetype = 2) +
           scale_x_continuous(breaks = seq(1983, 2021, by = 2)) + 
           ylab('Mortality Rate/100,000') + 
           theme(panel.background = element_blank()))
} 

ggarrange(get(paste0(M_Ca_D_Name[1],"_shiftplot")), get(paste0(M_Ca_D_Name[2],"_shiftplot")),
          get(paste0(M_Ca_D_Name[3],"_shiftplot")), get(paste0(M_Ca_D_Name[4],"_shiftplot")),
          get(paste0(M_Ca_D_Name[5],"_shiftplot")), get(paste0(M_Ca_D_Name[6],"_shiftplot")),
          labels = c("C00_C97","C00_C14","C15","C16","C18_C21","C22"),ncol = 2, nrow = 3)

ggarrange(get(paste0(M_Ca_D_Name[7],"_shiftplot")), get(paste0(M_Ca_D_Name[8],"_shiftplot")),
          get(paste0(M_Ca_D_Name[9],"_shiftplot")), get(paste0(M_Ca_D_Name[10],"_shiftplot")),
          get(paste0(M_Ca_D_Name[11],"_shiftplot")),get(paste0(M_Ca_D_Name[12],"_shiftplot")),
          labels = c("C23_C24","C25","C32","C33_C34","C61","C60_C63"),ncol = 2, nrow = 3)

ggarrange(get(paste0(M_Ca_D_Name[13],"_shiftplot")), get(paste0(M_Ca_D_Name[14],"_shiftplot")),
          get(paste0(M_Ca_D_Name[15],"_shiftplot")), get(paste0(M_Ca_D_Name[16],"_shiftplot")),
          get(paste0(M_Ca_D_Name[16],"_shiftplot")), get(paste0(M_Ca_D_Name[17],"_shiftplot")),
          labels = c("C64","C67","C70_C72","C73","C82_C86","C90"),ncol = 2, nrow = 3)

ggarrange(get(paste0(M_Ca_D_Name[18],"_shiftplot")), get(paste0(M_Ca_inci_Name[19],"_shiftplot")),
          labels = c("C91_C95","etc(C00_C97)"),ncol = 2, nrow = 3)


for (i in 1: 22){
  assign(paste0(W_Ca_inci_Name[i],"_shiftplot"),ggplot(data=get(paste0(W_Ca_inci_Name[i],"_female_in_shift")))+
           geom_line(data=get(paste0(W_Ca_inci_Name[i],"_female_in_shift")),aes(x=YEAR, y=Aging),color = '#3399FF') +
           geom_line(data=get(paste0(W_Ca_inci_Name[i],"_female_in_shift")),aes(x=YEAR, y=Young),color = '#3399FF',linetype = 2) +
           scale_x_continuous(breaks = seq(1999, 2019, by = 2)) + 
           ylab('Incidence Rate/100,000') + 
           theme(panel.background = element_blank()))
} 

ggarrange(get(paste0(W_Ca_inci_Name[1],"_shiftplot")), get(paste0(W_Ca_inci_Name[2],"_shiftplot")),
          get(paste0(W_Ca_inci_Name[3],"_shiftplot")), get(paste0(W_Ca_inci_Name[4],"_shiftplot")),
          get(paste0(W_Ca_inci_Name[5],"_shiftplot")), get(paste0(W_Ca_inci_Name[6],"_shiftplot")),
          labels = c("C00_C96","C00_C14","C15","C16","C18_C21","C22"),ncol = 2, nrow = 3)

ggarrange(get(paste0(W_Ca_inci_Name[7],"_shiftplot")), get(paste0(W_Ca_inci_Name[8],"_shiftplot")),
          get(paste0(W_Ca_inci_Name[9],"_shiftplot")), get(paste0(W_Ca_inci_Name[10],"_shiftplot")),
          get(paste0(W_Ca_inci_Name[11],"_shiftplot")),get(paste0(W_Ca_inci_Name[12],"_shiftplot")),
          labels = c("C23_C24","C25","C32","C33_C34","C50","C53"),ncol = 2, nrow = 3)

ggarrange(get(paste0(W_Ca_inci_Name[13],"_shiftplot")), get(paste0(W_Ca_inci_Name[14],"_shiftplot")),
          get(paste0(W_Ca_inci_Name[15],"_shiftplot")), get(paste0(W_Ca_inci_Name[16],"_shiftplot")),
          get(paste0(W_Ca_inci_Name[16],"_shiftplot")), get(paste0(W_Ca_inci_Name[17],"_shiftplot")),
          labels = c("C54_C55","C56","C64","C67","C70_C72","C73"),ncol = 2, nrow = 3)

ggarrange(get(paste0(W_Ca_inci_Name[18],"_shiftplot")), get(paste0(W_Ca_inci_Name[19],"_shiftplot")),
          get(paste0(W_Ca_inci_Name[20],"_shiftplot")), get(paste0(W_Ca_inci_Name[21],"_shiftplot")),
          get(paste0(W_Ca_inci_Name[22],"_shiftplot")),
          labels = c("C81","C82_C86_C96","C90","C91_C95","etc(C00_C96)"),ncol = 2, nrow = 3)




for (i in 1: 21){
  assign(paste0(W_Ca_D_Name[i],"_shiftplot"),ggplot(data=get(paste0(W_Ca_D_Name[i],"_female_de_shift")))+
           geom_line(data=get(paste0(W_Ca_D_Name[i],"_female_de_shift")),aes(x=YEAR, y=Aging),color = '#3399FF') +
           geom_line(data=get(paste0(W_Ca_D_Name[i],"_female_de_shift")),aes(x=YEAR, y=Young),color = '#3399FF',linetype = 2) +
           scale_x_continuous(breaks = seq(1983, 2021, by = 2)) + 
           ylab('Mortality Rate/100,000') + 
           theme(panel.background = element_blank()))
} 

ggarrange(get(paste0(W_Ca_D_Name[1],"_shiftplot")), get(paste0(W_Ca_D_Name[2],"_shiftplot")),
          get(paste0(W_Ca_D_Name[3],"_shiftplot")), get(paste0(W_Ca_D_Name[4],"_shiftplot")),
          get(paste0(W_Ca_D_Name[5],"_shiftplot")), get(paste0(W_Ca_D_Name[6],"_shiftplot")),
          labels = c("C00_C97","C00_C14","C15","C16","C18_C21","C22"),ncol = 2, nrow = 3)

ggarrange(get(paste0(W_Ca_D_Name[7],"_shiftplot")), get(paste0(W_Ca_D_Name[8],"_shiftplot")),
          get(paste0(W_Ca_D_Name[9],"_shiftplot")), get(paste0(W_Ca_D_Name[10],"_shiftplot")),
          get(paste0(W_Ca_D_Name[11],"_shiftplot")),get(paste0(W_Ca_D_Name[12],"_shiftplot")),
          labels = c("C23_C24","C25","C32","C33_C34","C50","C53"),ncol = 2, nrow = 3)

ggarrange(get(paste0(W_Ca_D_Name[13],"_shiftplot")), get(paste0(W_Ca_D_Name[14],"_shiftplot")),
          get(paste0(W_Ca_D_Name[15],"_shiftplot")), get(paste0(W_Ca_D_Name[16],"_shiftplot")),
          get(paste0(W_Ca_D_Name[16],"_shiftplot")), get(paste0(W_Ca_D_Name[17],"_shiftplot")),
          labels = c("C54_C55","C56","C64","C67","C70_C72","C73"),ncol = 2, nrow = 3)

ggarrange(get(paste0(W_Ca_D_Name[18],"_shiftplot")), get(paste0(M_Ca_inci_Name[19],"_shiftplot")),
          get(paste0(W_Ca_D_Name[20],"_shiftplot")), get(paste0(W_Ca_D_Name[21],"_shiftplot")),
          labels = c("C82_C86","C90","C91_C95","etc(C00_C97)"),ncol = 2, nrow = 3)

################################################################################################################
##############################75+##################################
################################################################################################################

for (i in 1:20) {
  A<-get(M_Ca_inci_Name[i])/wh_pop_men[17:37,]*100000 # 연령별 발생률
  B<-colSums(wh_pop_men[18,] + wh_pop_women[18,]) # 표준인구의 연령별 인구
  C <-sum(wh_pop_men[18,] + wh_pop_women[18,]) # 표준인구
  ASIR <- rowSums(A*B) / C # 연령표준화 발생률
  
  IR75<-rowSums(get(M_Ca_inci_Name[i])[,16:18])/rowSums(wh_pop_men[17:37,16:18])*100000 # 75세 이상 발생률
  
  AAIR<- IR75 * AAF_Inci_75[seq(17,37,1)]
  
  assign(paste0(M_Ca_inci_Name[i],"_comp_m_ini"),as.data.frame(setNames(as.data.frame(cbind(Ini_Year,ASIR,AAIR,IR75)),c("YEAR","ASIR","AAIR","IR75"))))
  
}


# Male Incidence
for(i in 1:20){
  assign(paste0(M_Ca_inci_Name[i],"_plot"),ggplot(data=get(paste0(M_Ca_inci_Name[i],"_comp_m_ini")))+
           geom_line(aes(x=YEAR, y=ASIR))+
           geom_line(data=get(paste0(M_Ca_inci_Name[i],"_comp_m_ini")),aes(x=YEAR, y=AAIR),color = '#3399FF') +
           geom_line(data=get(paste0(M_Ca_inci_Name[i],"_comp_m_ini")),aes(x=YEAR, y=IR75),color = '#3399FF',linetype = 2) +
           scale_x_continuous(breaks = seq(1999, 2019, by = 2)) + 
           ylab('Incidence Rate/100,000') + 
           theme(panel.background = element_blank()))
} 

ggarrange(get(paste0(M_Ca_inci_Name[1],"_plot")), get(paste0(M_Ca_inci_Name[2],"_plot")),
          get(paste0(M_Ca_inci_Name[3],"_plot")), get(paste0(M_Ca_inci_Name[4],"_plot")),
          get(paste0(M_Ca_inci_Name[5],"_plot")), get(paste0(M_Ca_inci_Name[6],"_plot")),
          labels = c("C00_C96","C00_C14","C15","C16","C18_C21","C22"),ncol = 2, nrow = 3)

ggarrange(get(paste0(M_Ca_inci_Name[7],"_plot")), get(paste0(M_Ca_inci_Name[8],"_plot")),
          get(paste0(M_Ca_inci_Name[9],"_plot")), get(paste0(M_Ca_inci_Name[10],"_plot")),
          get(paste0(M_Ca_inci_Name[11],"_plot")),get(paste0(M_Ca_inci_Name[12],"_plot")),
          labels = c("C23_C24","C25","C32","C33_C34","C61","C60_C63"),ncol = 2, nrow = 3)

ggarrange(get(paste0(M_Ca_inci_Name[13],"_plot")), get(paste0(M_Ca_inci_Name[14],"_plot")),
          get(paste0(M_Ca_inci_Name[15],"_plot")), get(paste0(M_Ca_inci_Name[16],"_plot")),
          get(paste0(M_Ca_inci_Name[16],"_plot")), get(paste0(M_Ca_inci_Name[17],"_plot")),
          labels = c("C64","C67","C70_C72","C73","C81","C82_C86_C96"),ncol = 2, nrow = 3)

ggarrange(get(paste0(M_Ca_inci_Name[18],"_plot")), get(paste0(M_Ca_inci_Name[19],"_plot")),
          get(paste0(M_Ca_inci_Name[20],"_plot")),
          labels = c("C90","C91_C95","etc(C00_C96)"),ncol = 2, nrow = 3)


for (i in 1:19) {
  A<-get(M_Ca_D_Name[i])/wh_pop_men[1:39,]*100000 # 연령별 사망률
  A[is.na(A)] = 0
  B<-colSums(wh_pop_men[23,] + wh_pop_women[23,]) # 표준인구의 연령별 인구
  C <-sum(wh_pop_men[23,] + wh_pop_women[23,]) # 표준인구
  ASMR <- rowSums(A*B) / C # 연령표준화 발생률

  MR75<-rowSums(get(M_Ca_D_Name[i])[,16:18])/rowSums(wh_pop_men[1:39,16:18])*100000 # 75세 이상 발생률
  
  AAMR<- MR75 * AAF_Mort_75[seq(1,39,1)]
  
  assign(paste0(M_Ca_D_Name[i],"_comp_m_de"),as.data.frame(setNames(as.data.frame(cbind(De_Year,ASMR,AAMR,MR75)),c("YEAR","ASMR","AAMR","MR75"))))
}


## Male Mortality
for (i in 1: 19){
  assign(paste0(M_Ca_D_Name[i],"_plot"),ggplot(data=get(paste0(M_Ca_D_Name[i],"_comp_m_de")))+
           geom_line(aes(x=YEAR, y=ASMR))+
           geom_line(data=get(paste0(M_Ca_D_Name[i],"_comp_m_de")),aes(x=YEAR, y=AAMR),color = '#3399FF') +
           geom_line(data=get(paste0(M_Ca_D_Name[i],"_comp_m_de")),aes(x=YEAR, y=MR75),color = '#3399FF',linetype = 2) +
           scale_x_continuous(breaks = seq(1983, 2021, by = 2)) + 
           ylab('Mortality Rate/100,000') + 
           theme(panel.background = element_blank()))
} 

ggarrange(get(paste0(M_Ca_D_Name[1],"_plot")), get(paste0(M_Ca_D_Name[2],"_plot")),
          get(paste0(M_Ca_D_Name[3],"_plot")), get(paste0(M_Ca_D_Name[4],"_plot")),
          get(paste0(M_Ca_D_Name[5],"_plot")), get(paste0(M_Ca_D_Name[6],"_plot")),
          labels = c("C00_C97","C00_C14","C15","C16","C18_C21","C22"),ncol = 2, nrow = 3)

ggarrange(get(paste0(M_Ca_D_Name[7],"_plot")), get(paste0(M_Ca_D_Name[8],"_plot")),
          get(paste0(M_Ca_D_Name[9],"_plot")), get(paste0(M_Ca_D_Name[10],"_plot")),
          get(paste0(M_Ca_D_Name[11],"_plot")),get(paste0(M_Ca_D_Name[12],"_plot")),
          labels = c("C23_C24","C25","C32","C33_C34","C61","C60_C63"),ncol = 2, nrow = 3)

ggarrange(get(paste0(M_Ca_D_Name[13],"_plot")), get(paste0(M_Ca_D_Name[14],"_plot")),
          get(paste0(M_Ca_D_Name[15],"_plot")), get(paste0(M_Ca_D_Name[16],"_plot")),
          get(paste0(M_Ca_D_Name[16],"_plot")), get(paste0(M_Ca_D_Name[17],"_plot")),
          labels = c("C64","C67","C70_C72","C73","C82_C86","C90"),ncol = 2, nrow = 3)

ggarrange(get(paste0(M_Ca_D_Name[18],"_plot")), get(paste0(M_Ca_inci_Name[19],"_plot")),
          labels = c("C91_C95","etc(C00_C97)"),ncol = 2, nrow = 3)



for (i in 1:22) {
  A<-get(W_Ca_inci_Name[i])/wh_pop_women[17:37,]*100000 # 연령별 발생률
  B<-colSums(wh_pop_men[18,] + wh_pop_women[18,]) # 표준인구의 연령별 인구
  C <-sum(wh_pop_men[18,] + wh_pop_women[18,]) # 표준인구
  ASIR <- rowSums(A*B) / C # 연령표준화 발생률
  
  IR75<-rowSums(get(W_Ca_inci_Name[i])[,16:18])/rowSums(wh_pop_women[17:37,16:18])*100000 # 75세 이상 발생률
  
  AAIR<- IR75 * AAF_Inci_75[seq(17,37,1)]

  assign(paste0(W_Ca_inci_Name[i],"_comp_w_ini"),as.data.frame(setNames(as.data.frame(cbind(Ini_Year,ASIR,AAIR,IR75)),c("YEAR","ASIR","AAIR","IR75"))))
} 

# Female Incidence
for(i in 1:22){
  assign(paste0(W_Ca_inci_Name[i],"_plot"),ggplot(data=get(paste0(W_Ca_inci_Name[i],"_comp_w_ini")))+
           geom_line(aes(x=YEAR, y=ASIR))+
           geom_line(data=get(paste0(W_Ca_inci_Name[i],"_comp_w_ini")),aes(x=YEAR, y=AAIR),color = '#3399FF') +
           geom_line(data=get(paste0(W_Ca_inci_Name[i],"_comp_w_ini")),aes(x=YEAR, y=IR75),color = '#3399FF',linetype = 2) +
           scale_x_continuous(breaks = seq(1999, 2019, by = 2)) + 
           ylab('Incidence Rate/100,000') + 
           theme(panel.background = element_blank()))
} 

ggarrange(get(paste0(W_Ca_inci_Name[1],"_plot")), get(paste0(W_Ca_inci_Name[2],"_plot")),
          get(paste0(W_Ca_inci_Name[3],"_plot")), get(paste0(W_Ca_inci_Name[4],"_plot")),
          get(paste0(W_Ca_inci_Name[5],"_plot")), get(paste0(W_Ca_inci_Name[6],"_plot")),
          labels = c("C00_C96","C00_C14","C15","C16","C18_C21","C22"),ncol = 2, nrow = 3)

ggarrange(get(paste0(W_Ca_inci_Name[7],"_plot")), get(paste0(W_Ca_inci_Name[8],"_plot")),
          get(paste0(W_Ca_inci_Name[9],"_plot")), get(paste0(W_Ca_inci_Name[10],"_plot")),
          get(paste0(W_Ca_inci_Name[11],"_plot")),get(paste0(W_Ca_inci_Name[12],"_plot")),
          labels = c("C23_C24","C25","C32","C33_C34","C50","C53"),ncol = 2, nrow = 3)

ggarrange(get(paste0(W_Ca_inci_Name[13],"_plot")), get(paste0(W_Ca_inci_Name[14],"_plot")),
          get(paste0(W_Ca_inci_Name[15],"_plot")), get(paste0(W_Ca_inci_Name[16],"_plot")),
          get(paste0(W_Ca_inci_Name[16],"_plot")), get(paste0(W_Ca_inci_Name[17],"_plot")),
          labels = c("C54_C55","C56","C64","C67","C70_C72","C73"),ncol = 2, nrow = 3)

ggarrange(get(paste0(W_Ca_inci_Name[18],"_plot")), get(paste0(W_Ca_inci_Name[19],"_plot")),
          get(paste0(W_Ca_inci_Name[20],"_plot")), get(paste0(W_Ca_inci_Name[21],"_plot")),
          get(paste0(W_Ca_inci_Name[22],"_plot")),
          labels = c("C81","C82_C86_C96","C90","C91_C95","etc(C00_C96)"),ncol = 2, nrow = 3)

for (i in 1:21) {
  A<-get(W_Ca_D_Name[i])/wh_pop_women[1:39,]*100000 # 연령별 사망률
  A[is.na(A)] = 0
  B<-colSums(wh_pop_men[23,] + wh_pop_women[23,]) # 표준인구의 연령별 인구
  C <-sum(wh_pop_men[23,] + wh_pop_women[23,]) # 표준인구
  ASMR <- rowSums(A*B) / C # 연령표준화 발생률
  
  MR75<-rowSums(get(W_Ca_D_Name[i])[,16:18])/rowSums(wh_pop_women[1:39,16:18])*100000 # 75세 이상 발생률
  
  AAMR<- MR75 * AAF_Mort_75[seq(1,39,1)]
  
  assign(paste0(W_Ca_D_Name[i],"_comp_w_de"),as.data.frame(setNames(as.data.frame(cbind(De_Year,ASMR,AAMR,MR75)),c("YEAR","ASMR","AAMR","MR75"))))
}

## Female Mortality
for (i in 1: 21){
  assign(paste0(W_Ca_D_Name[i],"_plot"),ggplot(data=get(paste0(W_Ca_D_Name[i],"_comp_w_de")))+
           geom_line(aes(x=YEAR, y=ASMR))+
           geom_line(data=get(paste0(W_Ca_D_Name[i],"_comp_w_de")),aes(x=YEAR, y=AAMR),color = '#3399FF') +
           geom_line(data=get(paste0(W_Ca_D_Name[i],"_comp_w_de")),aes(x=YEAR, y=MR75),color = '#3399FF',linetype = 2) +
           scale_x_continuous(breaks = seq(1983, 2021, by = 2)) + 
           ylab('Mortality Rate/100,000') + 
           theme(panel.background = element_blank()))
} 

ggarrange(get(paste0(W_Ca_D_Name[1],"_plot")), get(paste0(W_Ca_D_Name[2],"_plot")),
          get(paste0(W_Ca_D_Name[3],"_plot")), get(paste0(W_Ca_D_Name[4],"_plot")),
          get(paste0(W_Ca_D_Name[5],"_plot")), get(paste0(W_Ca_D_Name[6],"_plot")),
          labels = c("C00_C97","C00_C14","C15","C16","C18_C21","C22"),ncol = 2, nrow = 3)

ggarrange(get(paste0(W_Ca_D_Name[7],"_plot")), get(paste0(W_Ca_D_Name[8],"_plot")),
          get(paste0(W_Ca_D_Name[9],"_plot")), get(paste0(W_Ca_D_Name[10],"_plot")),
          get(paste0(W_Ca_D_Name[11],"_plot")),get(paste0(W_Ca_D_Name[12],"_plot")),
          labels = c("C23_C24","C25","C32","C33_C34","C50","C53"),ncol = 2, nrow = 3)

ggarrange(get(paste0(W_Ca_D_Name[13],"_plot")), get(paste0(W_Ca_D_Name[14],"_plot")),
          get(paste0(W_Ca_D_Name[15],"_plot")), get(paste0(W_Ca_D_Name[16],"_plot")),
          get(paste0(W_Ca_D_Name[16],"_plot")), get(paste0(W_Ca_D_Name[17],"_plot")),
          labels = c("C54_C55","C56","C64","C67","C70_C72","C73"),ncol = 2, nrow = 3)

ggarrange(get(paste0(W_Ca_D_Name[18],"_plot")), get(paste0(M_Ca_inci_Name[19],"_plot")),
          get(paste0(W_Ca_D_Name[20],"_plot")), get(paste0(W_Ca_D_Name[21],"_plot")),
          labels = c("C82_C86","C90","C91_C95","etc(C00_C97)"),ncol = 2, nrow = 3)


######################################################

# Whether Dominated or not?

for (i in 1:20){
  assign(paste0(M_Ca_inci_Name[i],"_M_DR"),((rowSums(get(M_Ca_inci_Name[i])[,16:18]) /rowSums(wh_pop_men[17:37,16:18])*100000)/(rowSums(get(M_Ca_inci_Name[i])[,1:13])/rowSums(wh_pop_men[17:37,1:15])*100000)))
}
for (i in 1:19){
  assign(paste0(M_Ca_D_Name[i],"_M_Death_DR"),((rowSums(get(M_Ca_D_Name[i])[,16:18]) /rowSums(wh_pop_men[1:39,16:18])*100000)/(rowSums(get(M_Ca_D_Name[i])[,1:13])/rowSums(wh_pop_men[1:39,1:15])*100000)))
}
for (i in 1:22){
  assign(paste0(W_Ca_inci_Name[i],"_W_DR"),((rowSums(get(W_Ca_inci_Name[i])[,16:18]) /rowSums(wh_pop_women[17:37,16:18])*100000)/(rowSums(get(W_Ca_inci_Name[i])[,1:13])/rowSums(wh_pop_women[17:37,1:15])*100000)))
}
for (i in 1:21){
  assign(paste0(W_Ca_D_Name[i],"_W_Death_DR"),((rowSums(get(W_Ca_D_Name[i])[,16:18]) /rowSums(wh_pop_women[1:39,16:18])*100000)/(rowSums(get(W_Ca_D_Name[i])[,1:13])/rowSums(wh_pop_women[1:39,1:15])*100000)))
}

M_inci_Name <-c("C00_C96","C00_C14",'C15','C16','C18_C21','C23_C24',
                'C25','C32','C33_C34','C61','C60_C63',
                'C64','C67','C70_C72','C73','C81','C82_C86_C96','C90','C91_C95','etc(C00_C96)')

W_inci_Name <-c("C00_C96","C00_C14",'C15','C16','C18_C21','C23_C24','
                C25','C32','C33_C34','C50','C53','C54_C55','C56','C64',
                'C67','C70_C72','C73','C81','C82_C86_C96','C90','C91_C95','etc(C00_C96)')

M_D_Name    <-c("C00_C97","C00_C14",'C15','C16','C18_C21','C23_C24',
                'C25','C32','C33_C34','C61','C60_C63',
                'C64','C67','C70_C72','C73','C82_C86','C90','C91_C95','etc(C00_C97)')

W_D_Name    <-c("C00_C97","C00_C14",'C15','C16','C18_C21','C23_C24',
                'C25','C32','C33_C34','C50','C53','C54_C55','C56',
                'C64','C67','C70_C72','C73','C82_C86','C90','C91_C95','etc(C00_C97)')



MaleIncDR<-as.data.frame(cbind(get(paste0(M_Ca_inci_Name[1],"_M_DR")),get(paste0(M_Ca_inci_Name[2],"_M_DR")),get(paste0(M_Ca_inci_Name[3],"_M_DR")),get(paste0(M_Ca_inci_Name[4],"_M_DR")),
                               get(paste0(M_Ca_inci_Name[5],"_M_DR")),get(paste0(M_Ca_inci_Name[5],"_M_DR")),get(paste0(M_Ca_inci_Name[7],"_M_DR")),get(paste0(M_Ca_inci_Name[8],"_M_DR")),
                               get(paste0(M_Ca_inci_Name[9],"_M_DR")),get(paste0(M_Ca_inci_Name[10],"_M_DR")),get(paste0(M_Ca_inci_Name[11],"_M_DR")),get(paste0(M_Ca_inci_Name[12],"_M_DR")),
                               get(paste0(M_Ca_inci_Name[13],"_M_DR")),get(paste0(M_Ca_inci_Name[14],"_M_DR")),get(paste0(M_Ca_inci_Name[15],"_M_DR")),get(paste0(M_Ca_inci_Name[16],"_M_DR")),
                               get(paste0(M_Ca_inci_Name[17],"_M_DR")),get(paste0(M_Ca_inci_Name[18],"_M_DR")),get(paste0(M_Ca_inci_Name[19],"_M_DR")),get(paste0(M_Ca_inci_Name[20],"_M_DR"))))

MaleDeDR<-as.data.frame(cbind(get(paste0(M_Ca_D_Name[1],"_M_Death_DR")),get(paste0(M_Ca_D_Name[2],"_M_Death_DR")),get(paste0(M_Ca_D_Name[3],"_M_Death_DR")),get(paste0(M_Ca_D_Name[4],"_M_Death_DR")),
                              get(paste0(M_Ca_D_Name[5],"_M_Death_DR")),get(paste0(M_Ca_D_Name[5],"_M_Death_DR")),get(paste0(M_Ca_D_Name[7],"_M_Death_DR")),get(paste0(M_Ca_D_Name[8],"_M_Death_DR")),
                              get(paste0(M_Ca_D_Name[9],"_M_Death_DR")),get(paste0(M_Ca_D_Name[10],"_M_Death_DR")),get(paste0(M_Ca_D_Name[11],"_M_Death_DR")),get(paste0(M_Ca_D_Name[12],"_M_Death_DR")),
                              get(paste0(M_Ca_D_Name[13],"_M_Death_DR")),get(paste0(M_Ca_D_Name[14],"_M_Death_DR")),get(paste0(M_Ca_D_Name[15],"_M_Death_DR")),get(paste0(M_Ca_D_Name[16],"_M_Death_DR")),
                              get(paste0(M_Ca_D_Name[17],"_M_Death_DR")),get(paste0(M_Ca_D_Name[18],"_M_Death_DR")),get(paste0(M_Ca_D_Name[19],"_M_Death_DR"))))

colnames(MaleIncDR) <- M_inci_Name
colnames(MaleDeDR)  <- M_D_Name

MaleIncDR<-round(MaleIncDR,1)
MaleDeDR<-round(MaleDeDR,1)

MaleIncDR<-cbind(Ini_Year,MaleIncDR)
MaleDeDR<-cbind(De_Year,MaleDeDR)

MaleIncDR<-melt(data = MaleIncDR, id.vars = c("Ini_Year"))
MaleDeDR <-melt(data = MaleDeDR, id.vars = c("De_Year"))

colnames(MaleIncDR) <-c("Year","Cancer","DR")
colnames(MaleDeDR)  <-c("Year","Cancer","DR")


FemaleIncDR<-as.data.frame(cbind(get(paste0(W_Ca_inci_Name[1],"_W_DR")),get(paste0(W_Ca_inci_Name[2],"_W_DR")),get(paste0(W_Ca_inci_Name[3],"_W_DR")),get(paste0(W_Ca_inci_Name[4],"_W_DR")),
                                 get(paste0(W_Ca_inci_Name[5],"_W_DR")),get(paste0(W_Ca_inci_Name[5],"_W_DR")),get(paste0(W_Ca_inci_Name[7],"_W_DR")),get(paste0(W_Ca_inci_Name[8],"_W_DR")),
                                 get(paste0(W_Ca_inci_Name[9],"_W_DR")),get(paste0(W_Ca_inci_Name[10],"_W_DR")),get(paste0(W_Ca_inci_Name[11],"_W_DR")),get(paste0(W_Ca_inci_Name[12],"_W_DR")),
                                 get(paste0(W_Ca_inci_Name[13],"_W_DR")),get(paste0(W_Ca_inci_Name[14],"_W_DR")),get(paste0(W_Ca_inci_Name[15],"_W_DR")),get(paste0(W_Ca_inci_Name[16],"_W_DR")),
                                 get(paste0(W_Ca_inci_Name[17],"_W_DR")),get(paste0(W_Ca_inci_Name[18],"_W_DR")),get(paste0(W_Ca_inci_Name[19],"_W_DR")),get(paste0(W_Ca_inci_Name[20],"_W_DR")),
                                 get(paste0(W_Ca_inci_Name[21],"_W_DR")),get(paste0(W_Ca_inci_Name[22],"_W_DR"))))

FemaleDeDR<-as.data.frame(cbind(get(paste0(W_Ca_D_Name[1],"_W_Death_DR")),get(paste0(W_Ca_D_Name[2],"_W_Death_DR")),get(paste0(W_Ca_D_Name[3],"_W_Death_DR")),get(paste0(W_Ca_D_Name[4],"_W_Death_DR")),
                                get(paste0(W_Ca_D_Name[5],"_W_Death_DR")),get(paste0(W_Ca_D_Name[5],"_W_Death_DR")),get(paste0(W_Ca_D_Name[7],"_W_Death_DR")),get(paste0(W_Ca_D_Name[8],"_W_Death_DR")),
                                get(paste0(W_Ca_D_Name[9],"_W_Death_DR")),get(paste0(W_Ca_D_Name[10],"_W_Death_DR")),get(paste0(W_Ca_D_Name[11],"_W_Death_DR")),get(paste0(W_Ca_D_Name[12],"_W_Death_DR")),
                                get(paste0(W_Ca_D_Name[13],"_W_Death_DR")),get(paste0(W_Ca_D_Name[14],"_W_Death_DR")),get(paste0(W_Ca_D_Name[15],"_W_Death_DR")),get(paste0(W_Ca_D_Name[16],"_W_Death_DR")),
                                get(paste0(W_Ca_D_Name[17],"_W_Death_DR")),get(paste0(W_Ca_D_Name[18],"_W_Death_DR")),get(paste0(W_Ca_D_Name[19],"_W_Death_DR")),get(paste0(W_Ca_D_Name[20],"_W_Death_DR")),
                                get(paste0(W_Ca_D_Name[21],"_W_Death_DR"))))

colnames(FemaleIncDR) <- W_inci_Name
colnames(FemaleDeDR)  <- W_D_Name

FemaleIncDR<-round(FemaleIncDR,1)
FemaleDeDR<-round(FemaleDeDR,1)

FemaleIncDR<-cbind(Ini_Year,FemaleIncDR)
FemaleDeDR<-cbind(De_Year,FemaleDeDR)

FemaleIncDR<-melt(data = FemaleIncDR, id.vars = c("Ini_Year"))
FemaleDeDR <-melt(data = FemaleDeDR, id.vars = c("De_Year"))

colnames(FemaleIncDR) <-c("Year","Cancer","DR")
colnames(FemaleDeDR)  <-c("Year","Cancer","DR")

## Heatmap DR
ggplot(MaleIncDR, aes(x=Cancer, y=Year, fill=DR)) +
  geom_tile(colour="black", linetype="dashed") +
  scale_fill_gradient2(low="darkred",high="darkgreen",breaks=c(0,1,2,10,15,20,25,30,40,50,60,65,120),
                       limits=c(0,120)) +
  geom_text(aes(label=DR),color='black')

ggplot(FemaleIncDR, aes(x=Cancer, y=Year, fill=DR)) +
  geom_tile(colour="black", linetype="dashed") +
  scale_fill_gradient2(low="darkred",high="darkgreen",breaks=c(0,1,2,10,15,20,25,30,40,50,60,65),
                       limits=c(0,65)) +
  geom_text(aes(label=DR),color='black')

ggplot(MaleDeDR, aes(x=Cancer, y=Year, fill=DR)) +
  geom_tile(colour="black", linetype="dashed") +
  scale_fill_gradient2(low="darkred",high="darkgreen",breaks=c(0,1,2,10,15,20,25,30,40,50,60,65,80,90,100,120,150,400),
                       limits=c(0,400)) +
  geom_text(aes(label=DR),color='black')

ggplot(FemaleDeDR, aes(x=Cancer, y=Year, fill=DR)) +
  geom_tile(colour="black", linetype="dashed") +
  scale_fill_gradient2(low="darkred",high="darkgreen",breaks=c(0,1,2,10,15,20,25,30,40,50,60,65,80,90,100,120,150,300),
                       limits=c(0,300)) +
  geom_text(aes(label=DR),color='black')

######################################################################
######################################################################



for (i in 1:20){
  assign(paste0(M_Ca_inci_Name[i],"_M_DR_percent"),((rowSums(get(M_Ca_inci_Name[i])[,16:18]) /rowSums(wh_pop_men[17:37,16:18])*100000)/(rowSums(get(M_Ca_inci_Name[i])[,1:13])/rowSums(wh_pop_men[17:37,1:15])*100000)))
}
for (i in 1:19){
  assign(paste0(M_Ca_D_Name[i],"_M_Death_DR_percent"),((rowSums(get(M_Ca_D_Name[i])[,16:18]) /rowSums(wh_pop_men[1:39,16:18])*100000)/(rowSums(get(M_Ca_D_Name[i])[,1:13])/rowSums(wh_pop_men[1:39,1:15])*100000)))
}
for (i in 1:22){
  assign(paste0(W_Ca_inci_Name[i],"_W_DR_percent"),((rowSums(get(W_Ca_inci_Name[i])[,16:18]) /rowSums(wh_pop_women[17:37,16:18])*100000)/(rowSums(get(W_Ca_inci_Name[i])[,1:13])/rowSums(wh_pop_women[17:37,1:15])*100000)))
}
for (i in 1:21){
  assign(paste0(W_Ca_D_Name[i],"_W_Death_DR_percent"),((rowSums(get(W_Ca_D_Name[i])[,16:18]) /rowSums(wh_pop_women[1:39,16:18])*100000)/(rowSums(get(W_Ca_D_Name[i])[,1:13])/rowSums(wh_pop_women[1:39,1:15])*100000)))
}




rowSums(get(M_Ca_inci_Name[2])[,1:13])/rowSums(get(M_Ca_inci_Name[1])[,1:13])  # 00-04 ~ 60-64
rowSums(get(M_Ca_inci_Name[2])[,1:13])/rowSums(get(M_Ca_inci_Name[1])[,14:18]) # 65-69 ~ 85+










