## Data import 20221113
library(openxlsx)

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


Korea00 <- (wh_pop_men + wh_pop_women)[18,]/sum((wh_pop_men + wh_pop_women)[18,])
Korea05 <- (wh_pop_men + wh_pop_women)[23,]/sum((wh_pop_men + wh_pop_women)[23,])

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

Ini_Year <-seq(1999,2019,1)
De_Year  <-seq(1983,2021,1)

# save R.data
save.image(file = "/Users/jun/Desktop/modeling methods/import_data.RData")




