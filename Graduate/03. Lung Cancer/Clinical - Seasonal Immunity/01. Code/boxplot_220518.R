library("lubridate")
library("ggplot2")
library("ggpubr")
library("stringi")
library("plotly")
library("dplyr")
library("rcompanion")

load(file="/Users/jun/OneDrive/lung/lung_data.RData")
#load(file="/Users/jun/OneDrive/lung/opt.data2.RData")

lung_data$Month <- month(as.Date(lung_data$Start.date,origin = "1899-12-30"))
lung_data$Month <- stri_replace_all_regex(lung_data$Month,
                                          pattern=c(10,11,12),
                                          replacement=c("Oct","Nov","Dec"),
                                          vectorize=FALSE)
lung_data$Month <- stri_replace_all_regex(lung_data$Month,
                                          pattern=c(1,2,3,4,5,6,7,8,9),
                                          replacement=c('Jan', 'Feb', 'Mar','Apr','May',"June","July","Aug","Sep"),
                                          vectorize=FALSE)

lung_data$Month  <-as.factor(lung_data$Month)
levels(lung_data$Month)
lung_data$Month <- factor(lung_data$Month, levels=c('Mar','Apr','May',"June","July","Aug","Sep","Oct","Nov","Dec",'Jan', 'Feb'))


str(lung_data)

lung_data <- subset(lung_data,response!='NA')

lung_data_pdl1 <-subset(lung_data,PDL1_IHC ==1)
lung_data_pdl2 <-subset(lung_data,PDL1_IHC ==2)

table(lung_data_pdl1$Month) # number of case
table(lung_data_pdl2$Month) # number of case

round(table(lung_data_pdl1$Month)/sum(table(lung_data_pdl1$Month))*100,0) # Percent
round(table(lung_data_pdl2$Month)/sum(table(lung_data_pdl2$Month))*100,0) # Percent

month <-c('Mar','Apr','May',"June","July","Aug","Sep","Oct","Nov","Dec",'Jan','Feb')

# PDL1 < 5%
Response_rate1<-c()
Response_rate1_freq <- c()
j=1
for (i in month) {
  sample <- subset(lung_data_pdl1,Month==i)
  Response_rate1[j] = round(length(which(sample$Response =='1' | sample$Response =='2'))/ length(sample$Response =='1' | sample$Response =='2'|sample$Response =='3' | sample$Response =='4')*100,0)
  Response_rate1_freq[j] = length(which(sample$Response =='1' | sample$Response =='2'))
  j=j+1
}


AResponse_rate1 <- as.data.frame(cbind(month,Response_rate1,Response_rate1_freq))
AResponse_rate1$Month <- as.factor(AResponse_rate1$month)
AResponse_rate1$Month <- factor(AResponse_rate1$Month, levels=c('Mar','Apr','May',"June","July","Aug","Sep","Oct","Nov","Dec",'Jan', 'Feb'))
AResponse_rate1$Response_rate1 <- as.numeric(AResponse_rate1$Response_rate1)
AResponse_rate1$Response_rate1_freq <- as.numeric(AResponse_rate1$Response_rate1_freq)

# PDL1 <=5%

Response_rate2<-c()
Response_rate2_freq <- c()
j=1
for (i in month) {
  sample <- subset(lung_data_pdl2,Month==i)
  Response_rate2[j] = round(length(which(sample$Response =='1' | sample$Response =='2'))/ length(sample$Response =='1' | sample$Response =='2'|sample$Response =='3' | sample$Response =='4')*100,0)
  Response_rate2_freq[j] = length(which(sample$Response =='1' | sample$Response =='2'))
  j=j+1
}

AResponse_rate2 <- as.data.frame(cbind(month,Response_rate2,Response_rate2_freq))
AResponse_rate2$Month  <-as.factor(AResponse_rate2$month)
AResponse_rate2$Month <- factor(AResponse_rate2$Month, levels=c('Mar','Apr','May',"June","July","Aug","Sep","Oct","Nov","Dec",'Jan','Feb'))
AResponse_rate2$Response_rate2 <- as.numeric(AResponse_rate2$Response_rate2)
AResponse_rate2$Response_rate2_freq <- as.numeric(AResponse_rate2$Response_rate2_freq)


AResponse_rate2$Month

A20 <- ggplot(AResponse_rate1, aes(x=Month, y=Response_rate1))+ ylim(0,50) + 
  geom_bar(stat="identity", fill="#a9a9a9", position=position_dodge())+ 
  labs(title = "PDL1 expression <5%", x = "Month", y = "Response Rate (%)") + 
  theme(panel.background = element_blank(),
        panel.border = element_rect(size = 1.5, fill = NA),
        plot.title = element_text(hjust = 0.5,size = 25,face='bold'),
        axis.title = element_text(size = 20,face='bold'),
        axis.title.x = element_text(vjust=-0.5),
        axis.text = element_text(size = 16,color="black",face='bold'),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16))

B20 <- ggplot(AResponse_rate2, aes(x=Month, y=Response_rate2))+ ylim(0,50) +  
  geom_bar(stat="identity", fill="#a9a9a9", position=position_dodge())+ 
  labs(title = "PDL1 expression ?‰¥5%", x = "Month", y = "Response Rate (%)") + 
  theme(panel.background = element_blank(),
        panel.border = element_rect(size = 1.5, fill = NA),
        plot.title = element_text(hjust = 0.5,size = 25,face='bold'),
        axis.title = element_text(size = 20,face='bold'),
        axis.title.x = element_text(vjust=-0.5),
        axis.text = element_text(size = 16,color="black",face='bold'),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16))

ggarrange(A20, B20,labels = c("(A)", "(B)"),ncol = 2, nrow = 1,font.label = list(size = 25))    


#######################################################################################################################################
#######################################################################################################################################

library(survival)
library(survminer)
subet(lung_data_pdl1,Month)
month <-c('Mar','Apr','May',"June","July","Aug","Sep","Oct","Nov","Dec",'Jan','Feb')

for (i in month) {
  sample <- subset(lung_data_pdl1,Month==i)
  os_mean[j]  <-round(mean(sample$Censoring.OS.60),2)
  os_upper[j] <-round(mean(sample$Censoring.OS.60) + 1.96 * sd(sample$Censoring.OS.60/sqrt(nrow(sample))),2)
  os_lower[j] <-round(mean(sample$Censoring.OS.60) - 1.96 * sd(sample$Censoring.OS.60/sqrt(nrow(sample))),2)
  os_median[j]<-round(median(sample$Censoring.OS.60),2)
  os_medain_upper[i] <- sample$Censoring.OS.60[ceiling(length(sample$Censoring.OS.60) * 0.5 + 1.96 * sqrt(length(sample$Censoring.OS.60) * 0.5 * (1-0.5)))]
  os_medain_lower[i] <- sample$Censoring.OS.60[ceiling(length(sample$Censoring.OS.60) * 0.5 - 1.96 * sqrt(length(sample$Censoring.OS.60) * 0.5 * (1-0.5)))]
  os_1q[j]    <- round(quantile(sample$Censoring.OS.60)[2],2)
  os_3q[j]    <- round(quantile(sample$Censoring.OS.60)[4],2)
  j=j+1
}  




# 5 Year mPFS
A4<- ggplot(data = lung_data_pdl1, aes(x = Month, y = Censoring.PFS.60,fill = Month)) + ylim(0,65) +  
  geom_boxplot(linetype = "dashed", outlier.shape = 15) +
  stat_boxplot(aes(ymin = ..lower.., ymax = ..upper..), outlier.shape = 15,fill="steelblue") +
  stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..)) +
  stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..)) +
  labs(title = "PDL1 expression <5%",
       x = "Month",
       y = "PFS (Months)") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(size = 1.5, fill = NA),
        plot.title = element_text(hjust = 0.5,size = 25,face='bold'),
        axis.title = element_text(size = 20,face='bold'),
        axis.title.x = element_text(vjust=-0.5),
        axis.text = element_text(size = 16,color="black",face='bold'),
        legend.position="none")+ stat_summary(fun.y=mean, geom="point", shape=17, size=2.5, color="red", fill="red")

B4<- ggplot(data = lung_data_pdl2, aes(x = Month, y = Censoring.PFS.60,fill = Month)) + ylim(0,65) +  
  geom_boxplot(linetype = "dashed", outlier.shape = 15) +
  stat_boxplot(aes(ymin = ..lower.., ymax = ..upper..), outlier.shape = 15,fill="steelblue") +
  stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..)) +
  stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..)) +
  labs(title = "PDL1 expression ?‰¥5%",
       x = "Month",
       y = "PFS (Months)") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(size = 1.5, fill = NA),
        plot.title = element_text(hjust = 0.5,size = 25,face='bold'),
        axis.title = element_text(size = 20,face='bold'),
        axis.title.x = element_text(vjust=-0.5),
        axis.text = element_text(size = 16,color="black",face='bold'),
        legend.position="none")+ stat_summary(fun.y=mean, geom="point", shape=17, size=2.5, color="red", fill="red")


ggarrange(A4, B4,labels = c("A", "B"),ncol = 2, nrow = 1,font.label = list(size = 25)) 

########################################################################

# 5 Year mOS

A4_sub<- ggplot(data = lung_data_pdl1, aes(x = Month, y = Censoring.OS.60,fill = Month)) + ylim(0,65) +    
  geom_boxplot(linetype = "dashed", outlier.shape = 15) +
  stat_boxplot(aes(ymin = ..lower.., ymax = ..upper..), outlier.shape = 15,fill="orange") +
  stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..)) +
  stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..)) +
  labs(title = "PDL1 expression <5%",
       x = "Month",
       y = "OS (Months)") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(size = 1.5, fill = NA),
        plot.title = element_text(hjust = 0.5,size = 25,face='bold'),
        axis.title = element_text(size = 20,face='bold'),
        axis.title.x = element_text(vjust=-0.5),
        axis.text = element_text(size = 16,color="black",face='bold'),
        legend.position="none") + stat_summary(fun.y=mean, geom="point", shape=17, size=2.5, color="red", fill="red")

B4_sub<- ggplot(data = lung_data_pdl2, aes(x = Month, y = Censoring.OS.60,fill = Month)) + ylim(0,65) +  
  geom_boxplot(linetype = "dashed", outlier.shape = 15) +
  stat_boxplot(aes(ymin = ..lower.., ymax = ..upper..), outlier.shape = 15,fill="orange") +
  stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..)) +
  stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..)) +
  labs(title = "PDL1 expression ?‰¥5%",
       x = "Month",
       y = "OS (Months)") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(size = 1.5, fill = NA),
        plot.title = element_text(hjust = 0.5,size = 25,face='bold'),
        axis.title = element_text(size = 20,face='bold'),
        axis.title.x = element_text(vjust=-0.5),
        axis.text = element_text(size = 16,color="black",face='bold'),
        legend.position="none")+ stat_summary(fun.y=mean, geom="point", shape=17, size=2.5, color="red", fill="red")


ggarrange(A4_sub, B4_sub,labels = c("A", "B"),ncol = 2, nrow = 1,font.label = list(size = 25)) 



##################################


# 5Y follow up
month <-c('Mar','Apr','May',"June","July","Aug","Sep","Oct","Nov","Dec",'Jan','Feb')

pfs_mean<-c()
pfs_upper <- c()
pfs_lower <- c()
pfs_median<-c()
pfs_medain_upper <-c()
pfs_medain_lower <-c()
pfs_1q<-c()
pfs_3q<-c()
j=1

for (i in month) {
  sample <- subset(lung_data_pdl1,Month==i)
  pfs_mean[j]  <-round(mean(sample$Censoring.PFS.60),2)
  pfs_upper[j] <-round(mean(sample$Censoring.PFS.60) + 1.96 * sd(sample$Censoring.PFS.60/sqrt(nrow(sample))),2)
  pfs_lower[j] <-round(mean(sample$Censoring.PFS.60) - 1.96 * sd(sample$Censoring.PFS.60/sqrt(nrow(sample))),2)
  pfs_median[j]<-round(median(sample$Censoring.PFS.60),2)
  pfs_medain_upper[i] <- sample$Censoring.PFS.60[ceiling(length(sample$Censoring.PFS.60) * 0.5 + 1.96 * sqrt(length(sample$Censoring.PFS.60) * 0.5 * (1-0.5)))]
  pfs_medain_lower[i] <- sample$Censoring.PFS.60[ceiling(length(sample$Censoring.PFS.60) * 0.5 - 1.96 * sqrt(length(sample$Censoring.PFS.60) * 0.5 * (1-0.5)))]
  pfs_1q[j]    <- round(quantile(sample$Censoring.PFS.60)[2],2)
  pfs_3q[j]    <- round(quantile(sample$Censoring.PFS.60)[4],2)
  j=j+1
}  
  
PDL1_PFS_5Y <- as.data.frame(cbind(month,pfs_mean,pfs_upper,pfs_lower,pfs_median,pfs_1q,pfs_3q))
PDL1_PFS_5Y$pfs_mean <- as.numeric(PDL1_PFS_5Y$pfs_mean)
PDL1_PFS_5Y$pfs_upper<- as.numeric(PDL1_PFS_5Y$pfs_upper)
PDL1_PFS_5Y$pfs_lower<- as.numeric(PDL1_PFS_5Y$pfs_lower)
PDL1_PFS_5Y$pfs_median<- as.numeric(PDL1_PFS_5Y$pfs_median)

PDL1_PFS_5Y$pfs_medain_upper<- as.numeric(PDL1_PFS_5Y$pfs_medain_upper)
PDL1_PFS_5Y$pfs_medain_lower<- as.numeric(PDL1_PFS_5Y$pfs_medain_lower)

PDL1_PFS_5Y$pfs_1q<- as.numeric(PDL1_PFS_5Y$pfs_1q)
PDL1_PFS_5Y$pfs_3q<- as.numeric(PDL1_PFS_5Y$pfs_3q)

PDL1_PFS_5Y$month <- factor(PDL1_PFS_5Y$month, levels=c('Mar','Apr','May',"June","July","Aug","Sep","Oct","Nov","Dec",'Jan','Feb'))

pfs_mean<-c()
pfs_upper <- c()
pfs_lower <- c()
pfs_median<-c()
pfs_medain_upper <-c()
pfs_medain_lower <-c()
pfs_1q<-c()
pfs_3q<-c()
j=1
for (i in month) {
  sample <- subset(lung_data_pdl2,Month==i)
  pfs_mean[j]  <-round(mean(sample$Censoring.PFS.60),2)
  pfs_upper[j] <-round(mean(sample$Censoring.PFS.60) + 1.96 * sd(sample$Censoring.PFS.60/sqrt(nrow(sample))),2)
  pfs_lower[j] <-round(mean(sample$Censoring.PFS.60) - 1.96 * sd(sample$Censoring.PFS.60/sqrt(nrow(sample))),2)
  pfs_median[j]<-round(median(sample$Censoring.PFS.60),2)
  pfs_medain_upper[i] <- sample$Censoring.PFS.60[ceiling(length(sample$Censoring.PFS.60) * 0.5 + 1.96 * sqrt(length(sample$Censoring.PFS.60) * 0.5 * (1-0.5)))]
  pfs_medain_lower[i] <- sample$Censoring.PFS.60[ceiling(length(sample$Censoring.PFS.60) * 0.5 - 1.96 * sqrt(length(sample$Censoring.PFS.60) * 0.5 * (1-0.5)))]
  pfs_1q[j]    <- round(quantile(sample$Censoring.PFS.60)[2],2)
  pfs_3q[j]    <- round(quantile(sample$Censoring.PFS.60)[4],2)
  j=j+1
}    

PDL2_PFS_5Y <- as.data.frame(cbind(month,pfs_mean,pfs_upper,pfs_lower,pfs_median,pfs_1q,pfs_3q))
PDL2_PFS_5Y$pfs_mean <- as.numeric(PDL2_PFS_5Y$pfs_mean)
PDL2_PFS_5Y$pfs_upper<- as.numeric(PDL2_PFS_5Y$pfs_upper)
PDL2_PFS_5Y$pfs_lower<- as.numeric(PDL2_PFS_5Y$pfs_lower)
PDL2_PFS_5Y$pfs_median<- as.numeric(PDL2_PFS_5Y$pfs_median)

PDL2_PFS_5Y$pfs_medain_upper<- as.numeric(PDL2_PFS_5Y$pfs_medain_upper)
PDL2_PFS_5Y$pfs_medain_lower<- as.numeric(PDL2_PFS_5Y$pfs_medain_lower)

PDL2_PFS_5Y$pfs_1q<- as.numeric(PDL2_PFS_5Y$pfs_1q)
PDL2_PFS_5Y$pfs_3q<- as.numeric(PDL2_PFS_5Y$pfs_3q)

PDL2_PFS_5Y

PDL2_PFS_5Y$month <- factor(PDL2_PFS_5Y$month, levels=c('Mar','Apr','May',"June","July","Aug","Sep","Oct","Nov","Dec",'Jan','Feb'))

##################################

C31 <- ggplot(PDL1_PFS_5Y, aes(x=month, y=pfs_median, fill=month))+ ylim(0,25)+
  geom_bar(stat="identity", color="black", position=position_dodge(),fill="steelblue")+ 
  labs(title = "PDL1 expression <5%", x = "Month", y = "Median PFS (Months)") + 
  theme(panel.background = element_blank(),
        panel.border = element_rect(size = 1.5, fill = NA),
        plot.title = element_text(hjust = 0.5,size = 25,face='bold'),
        axis.title = element_text(size = 20,face='bold'),
        axis.title.x = element_text(vjust=-0.5),
        axis.text = element_text(size = 16,color="black",face='bold'),
        legend.position="none") + 
  geom_errorbar(aes(ymin = pfs_1q, ymax = pfs_3q), width = 0.3, linetype=1)



C32 <- ggplot(PDL2_PFS_5Y, aes(x=month, y=pfs_median, fill=month))+ ylim(0,25)+
  geom_bar(stat="identity", color="black", position=position_dodge(),fill="steelblue")+ 
  labs(title = "PDL1 expression ?‰¥5%", x = "Month", y = "Median PFS (Months)") + 
  theme(panel.background = element_blank(),
        panel.border = element_rect(size = 1.5, fill = NA),
        plot.title = element_text(hjust = 0.5,size = 25,face='bold'),
        axis.title = element_text(size = 20,face='bold'),
        axis.title.x = element_text(vjust=-0.5),
        axis.text = element_text(size = 16,color="black",face='bold'),
        legend.position="none") + 
  geom_errorbar(aes(ymin = pfs_1q, ymax = pfs_3q), width = 0.3, linetype=1)


ggarrange(C31, C32,labels = c("A", "B"),ncol = 2, nrow = 1,font.label = list(size = 25))   


######################################################################################################

C33 <- ggplot(PDL1_PFS_5Y, aes(x=month, y=pfs_median, fill=month))+ ylim(0,25)+
  labs(title = "PDL1 expression <5%", x = "Month", y = "Mean PFS (Months)") + 
  theme(panel.background = element_blank(),
        panel.border = element_rect(size = 1.5, fill = NA),
        plot.title = element_text(hjust = 0.5,size = 25,face='bold'),
        axis.title = element_text(size = 20,face='bold'),
        axis.title.x = element_text(vjust=-0.5),
        axis.text = element_text(size = 16,color="black",face='bold'),
        legend.position="none") + 
  geom_errorbar(aes(ymin = pfs_lower, ymax = pfs_upper), width = 0.3,linetype=1) +
  geom_point(data=PDL1_PFS_5Y, mapping=aes(x=month, y=pfs_mean), size=2, shape=23, fill="black")



C34 <- ggplot(PDL2_PFS_5Y, aes(x=month, y=pfs_median, fill=month))+ ylim(0,25)+
  labs(title = "PDL1 expression ?‰¥5%", x = "Month", y = "Mean PFS (Months)") + 
  theme(panel.background = element_blank(),
        panel.border = element_rect(size = 1.5, fill = NA),
        plot.title = element_text(hjust = 0.5,size = 25,face='bold'),
        axis.title = element_text(size = 20,face='bold'),
        axis.title.x = element_text(vjust=-0.5),
        axis.text = element_text(size = 16,color="black",face='bold'),
        legend.position="none") + 
  geom_errorbar(aes(ymin = pfs_lower, ymax = pfs_upper), width = 0.3,linetype=1) +
  geom_point(data=PDL2_PFS_5Y, mapping=aes(x=month, y=pfs_mean), size=2, shape=23, fill="black")


ggarrange(C33, C34,labels = c("A", "B"),ncol = 2, nrow = 1,font.label = list(size = 25))  


###########Median CI###############
tab1<-groupwiseMedian(Censoring.PFS.60 ~ Month, data = lung_data_pdl1,conf = 0.95, R= 5000)
tab2<-groupwiseMedian(Censoring.PFS.60 ~ Month, data = lung_data_pdl2,conf = 0.95, R= 5000)

tab1$Month <- factor(tab1$Month, levels=c('Mar','Apr','May',"June","July","Aug","Sep","Oct","Nov","Dec",'Jan','Feb'))
tab2$Month <- factor(tab2$Month, levels=c('Mar','Apr','May',"June","July","Aug","Sep","Oct","Nov","Dec",'Jan','Feb'))

C31_sub <- ggplot(tab1, aes(x=Month, y=Median, fill=Month))+ ylim(0,25)+
  geom_bar(stat="identity", color="black", position=position_dodge(),fill="steelblue")+ 
  labs(title = "PDL1 expression <5%", x = "Month", y = "Median PFS (Months)") + 
  theme(panel.background = element_blank(),
        panel.border = element_rect(size = 1.5, fill = NA),
        plot.title = element_text(hjust = 0.5,size = 25,face='bold'),
        axis.title = element_text(size = 20,face='bold'),
        axis.title.x = element_text(vjust=-0.5),
        axis.text = element_text(size = 16,color="black",face='bold'),
        legend.position="none") + 
  geom_errorbar(aes(ymin = Bca.lower, ymax = Bca.upper), width = 0.3, linetype=1)



C32_sub <- ggplot(tab2, aes(x=Month, y=Median, fill=Month))+ ylim(0,25)+
  geom_bar(stat="identity", color="black", position=position_dodge(),fill="steelblue")+ 
  labs(title = "PDL1 expression ?‰¥5%", x = "Month", y = "Median PFS (Months)") + 
  theme(panel.background = element_blank(),
        panel.border = element_rect(size = 1.5, fill = NA),
        plot.title = element_text(hjust = 0.5,size = 25,face='bold'),
        axis.title = element_text(size = 20,face='bold'),
        axis.title.x = element_text(vjust=-0.5),
        axis.text = element_text(size = 16,color="black",face='bold'),
        legend.position="none") + 
  geom_errorbar(aes(ymin = Bca.lower, ymax = Bca.upper), width = 0.3, linetype=1)


ggarrange(C31_sub, C32_sub,labels = c("A", "B"),ncol = 2, nrow = 1,font.label = list(size = 25))   




##################################
# 5Y follow up
month <-c('Mar','Apr','May',"June","July","Aug","Sep","Oct","Nov","Dec",'Jan','Feb')

os_mean<-c()
os_upper <- c()
os_lower <- c()
os_median<-c()
os_medain_upper <-c()
os_medain_lower <-c()
os_1q<-c()
os_3q<-c()
j=1

for (i in month) {
  sample <- subset(lung_data_pdl1,Month==i)
  os_mean[j]  <-round(mean(sample$Censoring.OS.60),2)
  os_upper[j] <-round(mean(sample$Censoring.OS.60) + 1.96 * sd(sample$Censoring.OS.60/sqrt(nrow(sample))),2)
  os_lower[j] <-round(mean(sample$Censoring.OS.60) - 1.96 * sd(sample$Censoring.OS.60/sqrt(nrow(sample))),2)
  os_median[j]<-round(median(sample$Censoring.OS.60),2)
  os_medain_upper[i] <- sample$Censoring.OS.60[ceiling(length(sample$Censoring.OS.60) * 0.5 + 1.96 * sqrt(length(sample$Censoring.OS.60) * 0.5 * (1-0.5)))]
  os_medain_lower[i] <- sample$Censoring.OS.60[ceiling(length(sample$Censoring.OS.60) * 0.5 - 1.96 * sqrt(length(sample$Censoring.OS.60) * 0.5 * (1-0.5)))]
  os_1q[j]    <- round(quantile(sample$Censoring.OS.60)[2],2)
  os_3q[j]    <- round(quantile(sample$Censoring.OS.60)[4],2)
  j=j+1
}  

PDL1_OS_5Y <- as.data.frame(cbind(month,os_mean,os_upper,os_lower,os_median,os_1q,os_3q))
PDL1_OS_5Y$os_mean <- as.numeric(PDL1_OS_5Y$os_mean)
PDL1_OS_5Y$os_upper<- as.numeric(PDL1_OS_5Y$os_upper)
PDL1_OS_5Y$os_lower<- as.numeric(PDL1_OS_5Y$os_lower)
PDL1_OS_5Y$os_median<- as.numeric(PDL1_OS_5Y$os_median)

PDL1_OS_5Y$os_medain_upper<- as.numeric(PDL1_OS_5Y$os_medain_upper)
PDL1_OS_5Y$os_medain_lower<- as.numeric(PDL1_OS_5Y$os_medain_lower)

PDL1_OS_5Y$os_1q<- as.numeric(PDL1_OS_5Y$os_1q)
PDL1_OS_5Y$os_3q<- as.numeric(PDL1_OS_5Y$os_3q)

PDL1_OS_5Y$month <- factor(PDL1_OS_5Y$month, levels=c('Mar','Apr','May',"June","July","Aug","Sep","Oct","Nov","Dec",'Jan','Feb'))

os_mean<-c()
os_upper <- c()
os_lower <- c()
os_median<-c()
os_medain_upper <-c()
os_medain_lower <-c()
os_1q<-c()
os_3q<-c()
j=1
for (i in month) {
  sample <- subset(lung_data_pdl2,Month==i)
  os_mean[j]  <-round(mean(sample$Censoring.OS.60),2)
  os_upper[j] <-round(mean(sample$Censoring.OS.60) + 1.96 * sd(sample$Censoring.OS.60/sqrt(nrow(sample))),2)
  os_lower[j] <-round(mean(sample$Censoring.OS.60) - 1.96 * sd(sample$Censoring.OS.60/sqrt(nrow(sample))),2)
  os_median[j]<-round(median(sample$Censoring.OS.60),2)
  os_medain_upper[i] <- sample$Censoring.OS.60[ceiling(length(sample$Censoring.OS.60) * 0.5 + 1.96 * sqrt(length(sample$Censoring.OS.60) * 0.5 * (1-0.5)))]
  os_medain_lower[i] <- sample$Censoring.OS.60[ceiling(length(sample$Censoring.OS.60) * 0.5 - 1.96 * sqrt(length(sample$Censoring.OS.60) * 0.5 * (1-0.5)))]
  os_1q[j]    <- round(quantile(sample$Censoring.OS.60)[2],2)
  os_3q[j]    <- round(quantile(sample$Censoring.OS.60)[4],2)
  j=j+1
}    

PDL2_OS_5Y <- as.data.frame(cbind(month,os_mean,os_upper,os_lower,os_median,os_1q,os_3q))
PDL2_OS_5Y$os_mean <- as.numeric(PDL2_OS_5Y$os_mean)
PDL2_OS_5Y$os_upper<- as.numeric(PDL2_OS_5Y$os_upper)
PDL2_OS_5Y$os_lower<- as.numeric(PDL2_OS_5Y$os_lower)
PDL2_OS_5Y$os_median<- as.numeric(PDL2_OS_5Y$os_median)

PDL2_OS_5Y$os_medain_upper<- as.numeric(PDL2_OS_5Y$os_medain_upper)
PDL2_OS_5Y$os_medain_lower<- as.numeric(PDL2_OS_5Y$os_medain_lower)

PDL2_OS_5Y$os_1q<- as.numeric(PDL2_OS_5Y$os_1q)
PDL2_OS_5Y$os_3q<- as.numeric(PDL2_OS_5Y$os_3q)

PDL2_OS_5Y

PDL2_OS_5Y$month <- factor(PDL2_OS_5Y$month, levels=c('Mar','Apr','May',"June","July","Aug","Sep","Oct","Nov","Dec",'Jan','Feb'))


##################################


C35 <- ggplot(PDL1_OS_5Y, aes(x=month, y=os_median, fill=month))+ ylim(0,50)+
  geom_bar(stat="identity", color="black", position=position_dodge(),fill="orange")+ 
  labs(title = "PDL1 expression <5%", x = "Month", y = "Median OS (Months)") + 
  theme(panel.background = element_blank(),
        panel.border = element_rect(size = 1.5, fill = NA),
        plot.title = element_text(hjust = 0.5,size = 25,face='bold'),
        axis.title = element_text(size = 20,face='bold'),
        axis.title.x = element_text(vjust=-0.5),
        axis.text = element_text(size = 16,color="black",face='bold'),
        legend.position="none") + 
  geom_errorbar(aes(ymin = os_1q, ymax = os_3q), width = 0.3, linetype=1)

C36 <- ggplot(PDL2_OS_5Y, aes(x=month, y=os_median, fill=month))+ ylim(0,50)+
  geom_bar(stat="identity", color="black", position=position_dodge(),fill="orange")+ 
  labs(title = "PDL1 expression ?‰¥5%", x = "Month", y = "Median OS (Months)") + 
  theme(panel.background = element_blank(),
        panel.border = element_rect(size = 1.5, fill = NA),
        plot.title = element_text(hjust = 0.5,size = 25,face='bold'),
        axis.title = element_text(size = 20,face='bold'),
        axis.title.x = element_text(vjust=-0.5),
        axis.text = element_text(size = 16,color="black",face='bold'),
        legend.position="none") + 
  geom_errorbar(aes(ymin = os_1q, ymax = os_3q), width = 0.3, linetype=1)


ggarrange(C35, C36,labels = c("A", "B"),ncol = 2, nrow = 1,font.label = list(size = 25))


######################################################################################################

C37 <- ggplot(PDL1_OS_5Y, aes(x=month, y=os_median, fill=month))+ ylim(0,50)+
  labs(title = "PDL1 expression <5%", x = "Month", y = "Mean OS (Months)") + 
  theme(panel.background = element_blank(),
        panel.border = element_rect(size = 1.5, fill = NA),
        plot.title = element_text(hjust = 0.5,size = 25,face='bold'),
        axis.title = element_text(size = 20,face='bold'),
        axis.title.x = element_text(vjust=-0.5),
        axis.text = element_text(size = 16,color="black",face='bold'),
        legend.position="none") + 
  geom_errorbar(aes(ymin = os_lower, ymax = os_upper), width = 0.3,linetype=1) +
  geom_point(data=PDL1_OS_5Y, mapping=aes(x=month, y=os_mean), size=2, shape=23, fill="black")

C38 <- ggplot(PDL2_OS_5Y, aes(x=month, y=os_median, fill=month))+ ylim(0,50)+
  labs(title = "PDL1 expression ?‰¥5%", x = "Month", y = "Mean OS (Months)") + 
  theme(panel.background = element_blank(),
        panel.border = element_rect(size = 1.5, fill = NA),
        plot.title = element_text(hjust = 0.5,size = 25,face='bold'),
        axis.title = element_text(size = 20,face='bold'),
        axis.title.x = element_text(vjust=-0.5),
        axis.text = element_text(size = 16,color="black",face='bold'),
        legend.position="none") + 
  geom_errorbar(aes(ymin = os_lower, ymax = os_upper), width = 0.3,linetype=1) +
  geom_point(data=PDL2_OS_5Y, mapping=aes(x=month, y=os_mean), size=2, shape=23, fill="black")

ggarrange(C37, C38,labels = c("A", "B"),ncol = 2, nrow = 1,font.label = list(size = 25))



###########Median CI###############
tab3<-groupwiseMedian(Censoring.OS.60 ~ Month, data = lung_data_pdl1,conf = 0.95, R= 5000)
tab4<-groupwiseMedian(Censoring.OS.60 ~ Month, data = lung_data_pdl2,conf = 0.95, R= 5000)

tab3$Month <- factor(tab3$Month, levels=c('Mar','Apr','May',"June","July","Aug","Sep","Oct","Nov","Dec",'Jan','Feb'))
tab4$Month <- factor(tab4$Month, levels=c('Mar','Apr','May',"June","July","Aug","Sep","Oct","Nov","Dec",'Jan','Feb'))

C35_sub <- ggplot(tab3, aes(x=Month, y=Median, fill=Month))+ ylim(0,50)+
  geom_bar(stat="identity", color="black", position=position_dodge(),fill="orange")+ 
  labs(title = "PDL1 expression <5%", x = "Month", y = "Median OS (Months)") + 
  theme(panel.background = element_blank(),
        panel.border = element_rect(size = 1.5, fill = NA),
        plot.title = element_text(hjust = 0.5,size = 25,face='bold'),
        axis.title = element_text(size = 20,face='bold'),
        axis.title.x = element_text(vjust=-0.5),
        axis.text = element_text(size = 16,color="black",face='bold'),
        legend.position="none") + 
  geom_errorbar(aes(ymin = Bca.lower, ymax = Bca.upper), width = 0.3, linetype=1)

C36_sub <- ggplot(tab4, aes(x=Month, y=os_median, fill=Month))+ ylim(0,50)+
  geom_bar(stat="identity", color="black", position=position_dodge(),fill="orange")+ 
  labs(title = "PDL1 expression ?‰¥5%", x = "Month", y = "Median OS (Months)") + 
  theme(panel.background = element_blank(),
        panel.border = element_rect(size = 1.5, fill = NA),
        plot.title = element_text(hjust = 0.5,size = 25,face='bold'),
        axis.title = element_text(size = 20,face='bold'),
        axis.title.x = element_text(vjust=-0.5),
        axis.text = element_text(size = 16,color="black",face='bold'),
        legend.position="none") + 
  geom_errorbar(aes(ymin = Bca.lower, ymax = Bca.upper), width = 0.3, linetype=1)


ggarrange(C35_sub, C36_sub,labels = c("A", "B"),ncol = 2, nrow = 1,font.label = list(size = 25))









