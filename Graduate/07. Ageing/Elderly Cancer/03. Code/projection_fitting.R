library("openxlsx") 
library("tidyverse")
library("reshape")
set.seed(2022)

# Population
pop_women <- read.xlsx(xlsxFile="/Users/jun/Library/Mobile Documents/com~apple~CloudDocs/elderlystatistics/canproj/pop_00_19.xlsx", sheet = 'Female', colNames = TRUE)
pop_predwomen <- read.xlsx(xlsxFile="/Users/jun/Library/Mobile Documents/com~apple~CloudDocs/elderlystatistics/canproj/predict_population.xlsx", sheet = 'Female', colNames = TRUE)
pop_predwomen<-as.data.frame(pop_predwomen[c(-1,-22)])

# Incidence Case
cancer_women <- read.xlsx(xlsxFile="/Users/jun/Library/Mobile Documents/com~apple~CloudDocs/elderlystatistics/canproj/cancer_case.xlsx", sheet = 'Female', colNames = TRUE)
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

source("/Users/jun/Library/Mobile Documents/com~apple~CloudDocs/elderlystatistics/canproj/canproj-v2019.R")


# Female
C00_C96_Women <- trandat_ca(subset(cancer_women,Cancer=='C00-C96')[-1], 2000)

# observed 2000-2014, prediction 2015-2019

# startestage_list <-seq(5,16)
# cuttrend_list <-c(0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
# shortp_list <-c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1)
# link_func_list<-c("power5","log","sqrt")
# 
# result_list<-list()
# z=1
# for (i in startestage_list){
#   for (j in cuttrend_list) {
#     for (k in shortp_list){
#       for (t in link_func_list){
#         result_list[[z]]<-round(canproj.getproj(canproj(C00_C96_Women[,seq(1,15)], pop_women[,seq(1,20)], startp=2015,startestage=i, cuttrd = j,shortp = k,linkfunc = t)),0)[,seq(16,20)]
#         z=z+1
#       }
#     }
#   }
# }
origin<-round(C00_C96_Women[,seq(16,20)]/pop_women[,seq(16,20)]*100000,2)
sample1<-round(canproj.getproj(canproj(C00_C96_Women[,seq(1,15)], pop_women[,seq(1,20)], startp=2015, startestage=5))[,seq(16,20)],1)
sample2<-round(canproj.getproj(canproj(C00_C96_Women[,seq(1,15)], pop_women[,seq(1,20)], startp=2015, startestage=6))[,seq(16,20)],1)
sample3<-round(canproj.getproj(canproj(C00_C96_Women[,seq(1,15)], pop_women[,seq(1,20)], startp=2015, startestage=7))[,seq(16,20)],1)
sample4<-round(canproj.getproj(canproj(C00_C96_Women[,seq(1,15)], pop_women[,seq(1,20)], startp=2015, startestage=8))[,seq(16,20)],1)
sample5<-round(canproj.getproj(canproj(C00_C96_Women[,seq(1,15)], pop_women[,seq(1,20)], startp=2015, startestage=9))[,seq(16,20)],1)
sample6<-round(canproj.getproj(canproj(C00_C96_Women[,seq(1,15)], pop_women[,seq(1,20)], startp=2015, startestage=10))[,seq(16,20)],1)
sample7<-round(canproj.getproj(canproj(C00_C96_Women[,seq(1,15)], pop_women[,seq(1,20)], startp=2015, startestage=11))[,seq(16,20)],1)
sample8<-round(canproj.getproj(canproj(C00_C96_Women[,seq(1,15)], pop_women[,seq(1,20)], startp=2015, startestage=12))[,seq(16,20)],1)
sample9<-round(canproj.getproj(canproj(C00_C96_Women[,seq(1,15)], pop_women[,seq(1,20)], startp=2015, startestage=13))[,seq(16,20)],1)
sample10<-round(canproj.getproj(canproj(C00_C96_Women[,seq(1,15)], pop_women[,seq(1,20)], startp=2015, startestage=14))[,seq(16,20)],1)
sample11<-round(canproj.getproj(canproj(C00_C96_Women[,seq(1,15)], pop_women[,seq(1,20)], startp=2015, startestage=15))[,seq(16,20)],1)
sample12<-round(canproj.getproj(canproj(C00_C96_Women[,seq(1,15)], pop_women[,seq(1,20)], startp=2015, startestage=16))[,seq(16,20)],1)



sum((origin[6,]-sample1[6,])^2)/18
sum((origin[6,]-sample2[6,])^2)/18
sum((origin[6,]-sample3[6,])^2)/18
sum((origin[6,]-sample4[6,])^2)/18
sum((origin[6,]-sample5[6,])^2)/18
sum((origin[6,]-sample6[6,])^2)/18
sum((origin[6,]-sample7[6,])^2)/18
sum((origin[6,]-sample8[6,])^2)/18
sum((origin[6,]-sample9[6,])^2)/18
sum((origin[6,]-sample10[6,])^2)/18
sum((origin[6,]-sample11[6,])^2)/18
sum((origin[6,]-sample12[6,])^2)/18




