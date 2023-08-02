library("openxlsx") # open xlsx
library("moonBook") # easy to make table

load(file="/Users/jun/Downloads/lung_data.RData")
load(file="/Users/jun/Downloads/opt.data2.RData")

set.seed(2022)

# Unmatched
out<-mytable(Season2~Age2+Sex+ECOG3+PDL1_IHC+Previous.Treatment3+Smoking2+Histology+Regimen.4+Response+new.PD.60+Censoring.PFS.60+Censoring.OS.60+Death,data=lung_data)
mycsv(out,file="Unmatched.csv")

# Matched
out<-mytable(Season2~Age2+Sex+ECOG3+PDL1_IHC+Previous.Treatment3+Smoking2+Histology+Regimen.4+Response+new.PD.60+Censoring.PFS.60+Censoring.OS.60+Death,data=opt.data2)
mycsv(out,file="Matched.csv")


## Median and quartile calcualte

season <-c('1','0')

pfs_median<-c()
pfs_1q<-c()
pfs_3q<-c()
j=1

for (i in season) {
  sample <- subset(lung_data,Season2==i)
  pfs_median[j]<-round(median(sample$Censoring.PFS.60),2)
  pfs_1q[j]    <- round(quantile(sample$Censoring.PFS.60)[2],2)
  pfs_3q[j]    <- round(quantile(sample$Censoring.PFS.60)[4],2)
  j=j+1
} 
os_median<-c()
os_1q<-c()
os_3q<-c()
j=1

for (i in season) {
  sample <- subset(lung_data,Season2==i)
  os_median[j]<-round(median(sample$Censoring.OS.60),2)
  os_1q[j]    <- round(quantile(sample$Censoring.OS.60)[2],2)
  os_3q[j]    <- round(quantile(sample$Censoring.OS.60)[4],2)
  j=j+1
} 

season <-c('Winter',"Other")
season <-as.data.frame(season)

unmatched_pfs_median<-as.data.frame(pfs_median)
unmatched_pfs_quantile_1_3 <-cbind(as.data.frame(pfs_1q),as.data.frame(pfs_3q))

unmatched_os_median<-as.data.frame(os_median)
unmatched_os_quantile_1_3 <-cbind(as.data.frame(os_1q),as.data.frame(os_3q))

unmatched_pfs <-cbind(season,unmatched_pfs_median,unmatched_pfs_quantile_1_3)
unmatched_os <-cbind(season,unmatched_os_median,unmatched_os_quantile_1_3)

################################################################################################################
season <-c('1','0')

pfs_median<-c()
pfs_1q<-c()
pfs_3q<-c()
j=1

for (i in season) {
  sample <- subset(opt.data2,Season2==i)
  pfs_median[j]<-round(median(sample$Censoring.PFS.60),2)
  pfs_1q[j]    <- round(quantile(sample$Censoring.PFS.60)[2],2)
  pfs_3q[j]    <- round(quantile(sample$Censoring.PFS.60)[4],2)
  j=j+1
} 
os_median<-c()
os_1q<-c()
os_3q<-c()
j=1

for (i in season) {
  sample <- subset(opt.data2,Season2==i)
  os_median[j]<-round(median(sample$Censoring.OS.60),2)
  os_1q[j]    <- round(quantile(sample$Censoring.OS.60)[2],2)
  os_3q[j]    <- round(quantile(sample$Censoring.OS.60)[4],2)
  j=j+1
} 
season <-c('Winter',"Other")
season <-as.data.frame(season)

matched_pfs_median<-as.data.frame(pfs_median)
matched_pfs_quantile_1_3 <-cbind(as.data.frame(pfs_1q),as.data.frame(pfs_3q))

matched_os_median<-as.data.frame(os_median)
matched_os_quantile_1_3 <-cbind(as.data.frame(os_1q),as.data.frame(os_3q))

matched_pfs <-cbind(season,matched_pfs_median,matched_pfs_quantile_1_3)
matched_os <-cbind(season,matched_os_median,matched_os_quantile_1_3)

######################################################
# p-value

library("coin")

median_test(Censoring.PFS.60 ~ Season2,data = lung_data) # 0.3831
median_test(Censoring.OS.60 ~ Season2,data = lung_data)   # 0.00149

median_test(Censoring.PFS.60 ~ Season2,data = opt.data2)   # 0.008925
median_test(Censoring.OS.60 ~ Season2,data = opt.data2)     # 0.00003474



unmatched_pfs$pvalue <-c(round(0.3831,3),'')
unmatched_os$pvalue <-c(round(0.00149,3),'')
matched_pfs$pvalue <-c(round(0.008925,3),'')
matched_os$pvalue <-c(0.00003474,'')

write.csv(unmatched_pfs,"unmatched_pfs.csv")
write.csv(unmatched_os,"unmatched_os.csv")
write.csv(matched_pfs,"matched_pfs.csv")
write.csv(matched_os,"matched_os.csv")

