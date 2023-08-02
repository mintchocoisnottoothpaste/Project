library(openxlsx)
library(dplyr)
library(reshape2)
library(tidyr)
library(DescTools)
library(extrafont)
library(ggthemes)
library(ggplot2)
library(viridis)
library(ggpubr)
library(hrbrthemes)
library(INLA)
library(BAPC)
library(graphics)
library(magrittr)
library(incAnalysis)
library(splines)
library(tidyverse)
library(reshape)
library(colorspace)

load(file="/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/rawdata/workspace.RData")
source("/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/code/canproj-2022_HJ.R")
agegroups <- c("00-04","05-09","10-14","15-19","20-24","25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84","85+" )

inci_cname_M <- inci_cname[inci_cname %in% c("C15_I","C16_I","C18_C20_I","C22_I","C23_C24_I","C25_I","C33_C34_I","C61_I","C64_I","C67_I","C82_C86_C96_I","C91_C95_I")]
mort_cname_M <- mort_cname[mort_cname %in% c("C15_D","C16_D","C18_C21_D","C22_D","C23_C24_D" ,"C25_D","C33_C34_D","C61_D","C64_D","C67_D","C82_C86_D","C91_C95_D")]

inci_cname_F <- inci_cname[inci_cname %in% c("C16_I","C18_C20_I","C22_I","C23_C24_I","C25_I","C33_C34_I","C53_I","C82_C86_C96_I","C91_C95_I")]
mort_cname_F <- mort_cname[mort_cname %in% c("C16_D","C18_C21_D","C22_D","C23_C24_D","C25_D","C33_C34_D","C53_D","C82_C86_D","C91_C95_D")]

title_nameM_I<-c("Esophagus (C15)","Stomach (C16)","Colorectal (C18-C20)","Liver (C22)","Gallbladder (C23-C24)","Pancreas (C25)","Lung (C33-C34)","Prostate (C61)","Kidney (C64)","Bladder (C67)","Non-Hodgkin lymphoma (C82-C86)","Leukemia (C91-C95)")
title_nameM_D<-c("Esophagus (C15)","Stomach (C16)","Colorectal & Anus (C18-C21)","Liver (C22)","Gallbladder (C23-C24)","Pancreas (C25)","Lung (C33-C34)","Prostate (C61)","Kidney (C64)","Bladder (C67)","Non-Hodgkin lymphoma (C82-C86)","Leukemia (C91-C95)")

title_nameF_I<-c("Stomach (C16)","Colorectal (C18-C20)","Liver (C22)","Gallbladder (C23-C24)", "Pancreas (C25)","Lung (C33-C34)","Kidney (C64)","Cervix uteri (C53)","Non-Hodgkin lymphoma (C82-C86)","Leukemia (C91-C95)")
title_nameF_D<-c("Stomach (C16)","Colorectal & Anus (C18-C21)","Liver (C22)", "Pancreas (C25)","Lung (C33-C34)","Cervix uteri (C53)","Non-Hodgkin lymphoma (C82-C86)","Leukemia (C91-C95)")


###########################################################################################################################
# Non Effect Population Structure
###########################################################################################################################

library(dlm)
library(forecast)

time_series_ag <- function (){
  
  ARIMA_INCI_M <-c()
  SSML_INCI_M <-c()
  
  for (i in 1:length(inci_cname_M)){
    inf<-ts(as.numeric(colSums( subset(get(inci_cname_M[i]),Sex=="남자")[,-c(1:3,24)])),start = c(2000))
    
    ## Autoregressive  model
    ARIMA <- round(as.numeric(forecast(auto.arima(inf), h = 21)$mean))
    
    ## linear state???space model
    fn <- function(parm) { dlmModPoly(order = 2, dV = parm[1], dW = parm[2:3])}
    fit <- dlmMLE(inf, rep(0, 3), build = fn, hessian = TRUE)
    mod <- fn(fit$par)
    filtered <- dlmFilter(inf, mod = mod)
    SSML_kalman <-dlmForecast(filtered,nAhead=21)
    SSML_kalman<-c(SSML_kalman$a[,1])
    
    ARIMA_INCI_M <- cbind(ARIMA_INCI_M,ARIMA)
    SSML_INCI_M <- cbind(SSML_INCI_M,SSML_kalman)
  }
  ARIMA_INCI_M <-data.frame(ARIMA_INCI_M)
  SSML_INCI_M <-data.frame(SSML_INCI_M)
  
  colnames(ARIMA_INCI_M) <- inci_cname_M
  colnames(SSML_INCI_M) <- inci_cname_M
  
  ARIMA_INCI_F <-c()
  SSML_INCI_F <-c()
  
  for (i in 1:length(inci_cname_F)){
    inf<-ts(as.numeric(colSums(subset(get(inci_cname_F[i]),Sex=="여자")[,-c(1:3,24)])),start = c(2000))
    
    ## Autoregressive  model
    ARIMA <- round(as.numeric(forecast(auto.arima(inf), h = 21)$mean))
    
    ## linear state???space model
    fn <- function(parm) { dlmModPoly(order = 2, dV = parm[1], dW = parm[2:3])}
    fit <- dlmMLE(inf, rep(0, 3), build = fn, hessian = TRUE)
    mod <- fn(fit$par)
    filtered <- dlmFilter(inf, mod = mod)
    SSML_kalman <-dlmForecast(filtered,nAhead=21)
    SSML_kalman<-c(SSML_kalman$a[,1])
    
    
    ARIMA_INCI_F <- cbind(ARIMA_INCI_F,ARIMA)
    SSML_INCI_F <- cbind(SSML_INCI_F,SSML_kalman)
  }
  
  ARIMA_INCI_F<-data.frame(ARIMA_INCI_F)
  SSML_INCI_F<-data.frame(SSML_INCI_F)
  
  colnames(ARIMA_INCI_F) <- inci_cname_F
  colnames(SSML_INCI_F) <- inci_cname_F
  
  ARIMA_MORT_M <-c()
  SSML_MORT_M <-c()
  
  for (i in 1:length(mort_cname_M)){
    inf<-ts(as.numeric(colSums( subset(get(mort_cname_M[i]),Sex=="남자")[,-c(1:3,24,25)])),start = c(2000))
    
    ## Autoregressive  model
    ARIMA <- round(as.numeric(forecast(auto.arima(inf), h = 21)$mean))
    
    ## linear state???space model
    fn <- function(parm) { dlmModPoly(order = 2, dV = parm[1], dW = parm[2:3])}
    fit <- dlmMLE(inf, rep(0, 3), build = fn, hessian = TRUE)
    mod <- fn(fit$par)
    filtered <- dlmFilter(inf, mod = mod)
    SSML_kalman <-dlmForecast(filtered,nAhead=21)
    SSML_kalman<-c(SSML_kalman$a[,1])
    
    
    ARIMA_MORT_M <- cbind(ARIMA_MORT_M,ARIMA)
    SSML_MORT_M <- cbind(ARIMA_MORT_M,SSML_kalman)
  }
  
  ARIMA_MORT_M<-data.frame(ARIMA_MORT_M)
  SSML_MORT_M<-data.frame(SSML_MORT_M)
  
  colnames(ARIMA_MORT_M) <- mort_cname_M
  colnames(SSML_MORT_M) <- mort_cname_M
  
  ARIMA_MORT_F <-c()
  SSML_MORT_F <-c()
  
  for (i in 1:length(mort_cname_F)){
    inf<-ts(as.numeric(colSums(subset(get(mort_cname_F[i]),Sex=="여자")[,-c(1:3,24,25)])),start = c(2000))
    
    ## Autoregressive  model
    ARIMA <- round(as.numeric(forecast(auto.arima(inf), h = 21)$mean))
    
    ## linear state???space model
    fn <- function(parm) { dlmModPoly(order = 2, dV = parm[1], dW = parm[2:3])}
    fit <- dlmMLE(inf, rep(0, 3), build = fn, hessian = TRUE)
    mod <- fn(fit$par)
    filtered <- dlmFilter(inf, mod = mod)
    SSML_kalman <-dlmForecast(filtered,nAhead=21)
    SSML_kalman<-c(SSML_kalman$a[,1])
    
    
    ARIMA_MORT_F <- cbind(ARIMA_MORT_F,ARIMA)
    SSML_MORT_F <- cbind(ARIMA_MORT_F,SSML_kalman)
  }
  
  ARIMA_MORT_F<-data.frame(ARIMA_MORT_F)
  SSML_MORT_F<-data.frame(SSML_MORT_F)
  
  colnames(ARIMA_MORT_F) <- mort_cname_F
  colnames(SSML_MORT_F) <- mort_cname_F
  
  ARIMA_LIST <-list(ARIMA_INCI_M,ARIMA_INCI_F,ARIMA_MORT_M,ARIMA_MORT_F)
  SSML_LIST <-list(SSML_INCI_M,SSML_INCI_F,SSML_MORT_M,SSML_MORT_F)
  
  time_series_list<-list(ARIMA_LIST,SSML_LIST)
  return(time_series_list)
}

scene1_ts <-time_series_ag()

scene1_ts_ARIMA<-scene1_ts[[1]]
scene1_ts_SSML<-scene1_ts[[2]]

scene1_ts_ARIMA_first_M_inci_Rate<-data.frame(scene1_ts_ARIMA[[1]])/colSums(Scenarios1_male[,as.character(2020:2040)])*100000
scene1_ts_ARIMA_first_F_inci_Rate<-data.frame(scene1_ts_ARIMA[[2]])/colSums(Scenarios1_female[,as.character(2020:2040)])*100000
scene1_ts_ARIMA_first_M_mort_Rate<-data.frame(scene1_ts_ARIMA[[3]])/colSums(Scenarios1_male[,as.character(2020:2040)])*100000
scene1_ts_ARIMA_first_F_mort_Rate<-data.frame(scene1_ts_ARIMA[[4]])/colSums(Scenarios1_female[,as.character(2020:2040)])*100000

scene1_ts_SSML_first_M_inci_Rate<-data.frame(scene1_ts_SSML[[1]])/colSums(Scenarios1_male[,as.character(2020:2040)])*100000
scene1_ts_SSML_first_F_inci_Rate<-data.frame(scene1_ts_SSML[[2]])/colSums(Scenarios1_female[,as.character(2020:2040)])*100000
scene1_ts_SSML_first_M_mort_Rate<-data.frame(scene1_ts_SSML[[3]])/colSums(Scenarios1_male[,as.character(2020:2040)])*100000
scene1_ts_SSML_first_F_mort_Rate<-data.frame(scene1_ts_SSML[[4]])/colSums(Scenarios1_female[,as.character(2020:2040)])*100000

scene1_ts_ARIMA_first_M_inci_Rate<-cbind("Year"=c(2020:2040),scene1_ts_ARIMA_first_M_inci_Rate)
scene1_ts_ARIMA_first_F_inci_Rate<-cbind("Year"=c(2020:2040),scene1_ts_ARIMA_first_F_inci_Rate)
scene1_ts_ARIMA_first_M_mort_Rate<-cbind("Year"=c(2020:2040),scene1_ts_ARIMA_first_M_mort_Rate)
scene1_ts_ARIMA_first_F_mort_Rate<-cbind("Year"=c(2020:2040),scene1_ts_ARIMA_first_F_mort_Rate)

scene1_ts_SSML_first_M_inci_Rate<-cbind("Year"=c(2020:2040),scene1_ts_SSML_first_M_inci_Rate)
scene1_ts_SSML_first_F_inci_Rate<-cbind("Year"=c(2020:2040),scene1_ts_SSML_first_F_inci_Rate)
scene1_ts_SSML_first_M_mort_Rate<-cbind("Year"=c(2020:2040),scene1_ts_SSML_first_M_mort_Rate)
scene1_ts_SSML_first_F_mort_Rate<-cbind("Year"=c(2020:2040),scene1_ts_SSML_first_F_mort_Rate)

first_ts_ARIMA_Rate<-rbind(
  cbind(melt(scene1_ts_ARIMA_first_M_inci_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Incidence"),"Comparison"=c("Prediction")),
  cbind(melt(scene1_ts_ARIMA_first_F_inci_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Incidence"),"Comparison"=c("Prediction")),
  cbind(melt(scene1_ts_ARIMA_first_M_mort_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Mortality"),"Comparison"=c("Prediction")),
  cbind(melt(scene1_ts_ARIMA_first_F_mort_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Mortality"),"Comparison"=c("Prediction")))

first_ts_SSML_Rate<-rbind(
  cbind(melt(scene1_ts_SSML_first_M_inci_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Incidence"),"Comparison"=c("Prediction")),
  cbind(melt(scene1_ts_SSML_first_F_inci_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Incidence"),"Comparison"=c("Prediction")),
  cbind(melt(scene1_ts_SSML_first_M_mort_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Mortality"),"Comparison"=c("Prediction")),
  cbind(melt(scene1_ts_SSML_first_F_mort_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Mortality"),"Comparison"=c("Prediction")))

colnames(first_ts_ARIMA_Rate) <-c("Year","Cancer","Rate","Sex","Type","Comparison")
colnames(first_ts_SSML_Rate) <-c("Year","Cancer","Rate","Sex","Type","Comparison")

first_ts_ARIMA_Rate$Sex <- factor(first_ts_ARIMA_Rate$Sex,levels=c("Male","Female"))
first_ts_SSML_Rate$Sex <- factor(first_ts_SSML_Rate$Sex,levels=c("Male","Female"))

## after applicate norpred func

first_ts_ARIMA_Rate<-rbind(subset(nd_comp_Rate,Year<=2019),first_ts_ARIMA_Rate)
first_ts_SSML_Rate<-rbind(subset(nd_comp_Rate,Year<=2019),first_ts_SSML_Rate)

############################################################################################################################################
library("reshape")
library("sqldf")
library("ggplot2")
library("dplyr")
library("ggpubr")
library("glue")

load(file="/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/rawdata/jp_model_M_inci.RData")
load(file="/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/rawdata/jp_model_M_mort.RData")
load(file="/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/rawdata/jp_model_F_inci.RData")
load(file="/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/rawdata/jp_model_F_mort.RData")

jp_model_M_inci_apc<-data.frame(jp_model_M_inci$data_export)
jp_model_M_mort_apc<-data.frame(jp_model_M_mort$data_export)
jp_model_F_inci_apc<-data.frame(jp_model_F_inci$data_export)
jp_model_F_mort_apc<-data.frame(jp_model_F_mort$data_export)

jp_model_M_inci_apc_sql<-sqldf("SELECT *, MAX(year) as last_value FROM jp_model_M_inci_apc GROUP BY comparison, cancer")[,-c(7,8,9)]
jp_model_M_mort_apc_sql<-sqldf("SELECT *, MAX(year) as last_value FROM jp_model_M_mort_apc GROUP BY comparison, cancer")[,-c(7,8,9)]
jp_model_F_inci_apc_sql<-sqldf("SELECT *, MAX(year) as last_value FROM jp_model_F_inci_apc GROUP BY comparison, cancer")[,-c(7,8,9)]
jp_model_F_mort_apc_sql<-sqldf("SELECT *, MAX(year) as last_value FROM jp_model_F_mort_apc GROUP BY comparison, cancer")[,-c(7,8,9)]

jp_model_M_inci_apc_sql$apc <- as.numeric(gsub("\\*", "", jp_model_M_inci_apc_sql$apc))
jp_model_M_mort_apc_sql$apc <- as.numeric(gsub("\\*", "", jp_model_M_mort_apc_sql$apc))
jp_model_F_inci_apc_sql$apc <- as.numeric(gsub("\\*", "", jp_model_F_inci_apc_sql$apc))
jp_model_F_mort_apc_sql$apc <- as.numeric(gsub("\\*", "", jp_model_F_mort_apc_sql$apc))

jp_model_M_inci_apc_sql<-subset(jp_model_M_inci_apc_sql,comparison=="first")[,-1]
jp_model_M_mort_apc_sql<-subset(jp_model_M_mort_apc_sql,comparison=="first")[,-1]
jp_model_F_inci_apc_sql<-subset(jp_model_F_inci_apc_sql,comparison=="first")[,-1]
jp_model_F_mort_apc_sql<-subset(jp_model_F_mort_apc_sql,comparison=="first")[,-1]


check<-function(jp_model_M_inci_apc_sql,inci_cname_M){
  model_rslt_all<-data.frame('cancer'=c(),'year'=c(),'rate'=c(),'model'=c())
  for (i in 1:length(inci_cname_M)) {
    data <- subset(jp_model_M_inci_apc_sql,cancer==inci_cname_M[i])
    year=2019
    k=1
    next_year  <-c()
    next_rate  <-c()
    next_model <-c()

    dat_model <-data$model
    dat_apc <-data$apc
    
    while(year < 2040) {
      # 다음 연도의 값을 계산
      next_year[k] <-  year +1
      next_rate[k] <-  dat_model+(dat_model*dat_apc*0.01)
      next_model[k] <- dat_model+(dat_model*dat_apc*0.01)
      dat_model = dat_model+(dat_model*dat_apc*0.01)
      year=year+1
      k=k+1
    }
    sample_data  <- data.frame('cancer'=data$cancer,'year'=next_year,'rate'=next_rate,'model'=next_model)
    assign(paste0("model_rslt_all"),rbind(model_rslt_all,sample_data))
  }
  return(model_rslt_all)
}

jp_model_inci_M_dat<-check(jp_model_M_inci_apc_sql,inci_cname_M);jp_model_inci_M_dat$Sex<-c("Male");jp_model_inci_M_dat$Comparison<-c("JP");jp_model_inci_M_dat$Type <-c("Incidence")
jp_model_mort_M_dat<-check(jp_model_M_mort_apc_sql,mort_cname_M);jp_model_mort_M_dat$Sex<-c("Male");jp_model_mort_M_dat$Comparison<-c("JP");jp_model_mort_M_dat$Type <-c("Mortality")
jp_model_inci_F_dat<-check(jp_model_F_inci_apc_sql,inci_cname_F);jp_model_inci_F_dat$Sex<-c("Female");jp_model_inci_F_dat$Comparison<-c("JP");jp_model_inci_F_dat$Type <-c("Incidence")
jp_model_mort_F_dat<-check(jp_model_F_mort_apc_sql,mort_cname_F);jp_model_mort_F_dat$Sex<-c("Female");jp_model_mort_F_dat$Comparison<-c("JP");jp_model_mort_F_dat$Type<-c("Mortality")

model_jp_pred<-rbind(jp_model_inci_M_dat,jp_model_mort_M_dat,jp_model_inci_F_dat,jp_model_mort_F_dat)[,-3]
model_jp_pred<-model_jp_pred[,c(2,1,3,4,6,5)]



############################################################################################################################################

model_obs<-subset(nd_comp_Rate,Comparison=="Observation")
model_AR_pred<-subset(first_ts_ARIMA_Rate,Comparison=="Prediction");model_AR_pred$Comparison <-c("ARIMA")
model_SSML_pred<-subset(first_ts_SSML_Rate,Comparison=="Prediction");model_SSML_pred$Comparison <-c("SSML")
colnames(model_jp_pred) <-colnames(model_obs)

model_obs_pred1<-subset(model_obs,Year==2019);model_obs_pred1$Comparison <-c("ARIMA")
model_obs_pred2<-subset(model_obs,Year==2019);model_obs_pred2$Comparison <-c("SSML")
model_obs_pred3<-subset(model_obs,Year==2019);model_obs_pred3$Comparison <-c("JP")

model_AR_pred <-rbind(model_obs_pred1,model_AR_pred)
model_SSML_pred <-rbind(model_obs_pred2,model_SSML_pred)
model_jp_pred <-rbind(model_obs_pred3,model_jp_pred)

mdata<-rbind(model_obs,model_AR_pred,model_SSML_pred,model_jp_pred)

mdata$Comparison <-factor(mdata$Comparison)
mdata$Comparison <- factor(mdata$Comparison,levels=c("Observation","JP", "ARIMA", "SSML"))


for ( i in 1:length(inci_cname_M)){
  assign(paste0("plot",i),ggplot(subset(mdata,Type==c("Incidence") & Sex=="Male" &  Cancer==inci_cname_M[i]), aes(x = Year, y = Rate, shape=Comparison, color = Comparison)) +
           geom_line(size = 1.2) +
           geom_point(size = 1.5,fill = "white") +
           scale_fill_viridis(discrete = T, option = "E") +
           #scale_linetype_manual(values = c("solid", "dashed")) +
           scale_color_manual(values = c("black", "chartreuse3", "brown3",'cadetblue3')) + 
           scale_shape_manual(values = c(15,16,17,18)) +
           theme_classic() +
           facet_wrap(~ Sex, strip.position = "top", nrow = 1) +
           labs(x = "Year", y = "Incidence Rate (per 100,000)",title=title_nameM_I[i]) +
           theme_ipsum() +
           theme(
             strip.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5,size=15), # 패널 레이블 위치 조정
             axis.title.x = element_text(hjust = 0.5, vjust = 0.5,size=15), # X축 이름 중앙 정렬
             axis.title.y = element_text(hjust = 0.5, vjust = 0.5,size=15), # Y축 이름 중앙 정렬
             plot.margin = margin(1, 1, 1, 1, "cm"), # 그래프 안에 레이블 넣기 위해 여백 조정
             legend.text = element_text(size = 15),legend.title = element_text(size = 15),plot.title = element_text(size = 15, face = "bold")
           ))
}

ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot5,plot6,plot7,plot8,labels = c("E","F","G","H"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot9,labels = c("I"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,labels = c("A","B","C","D","E","F","G","H","I"),ncol = 2, nrow = 5, common.legend = TRUE)


for ( i in 1:length(mort_cname_M)){
  assign(paste0("plot",i),ggplot(subset(mdata,Type==c("Mortality") & Sex=="Male" &  Cancer==mort_cname_M[i]), aes(x = Year, y = Rate, shape=Comparison, color = Comparison)) +
           geom_line(size = 1.2) +
           geom_point(size = 1.5,fill = "white") +
           scale_fill_viridis(discrete = T, option = "E") +
           scale_color_manual(values = c("black", "chartreuse3", "brown3",'cadetblue3')) + 
           scale_shape_manual(values = c(15,16,17,18)) +
           theme_classic() +
           facet_wrap(~ Sex, strip.position = "top", nrow = 1) +
           labs(x = "Year", y = "Mortality Rate (per 100,000)",title=title_nameM_D[i]) +
           theme_ipsum() +
           theme(
             strip.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5,size=15), # 패널 레이블 위치 조정
             axis.title.x = element_text(hjust = 0.5, vjust = 0.5,size=15), # X축 이름 중앙 정렬
             axis.title.y = element_text(hjust = 0.5, vjust = 0.5,size=15), # Y축 이름 중앙 정렬
             plot.margin = margin(1, 1, 1, 1, "cm"), # 그래프 안에 레이블 넣기 위해 여백 조정
             legend.text = element_text(size = 15),legend.title = element_text(size = 15),plot.title = element_text(size = 15, face = "bold")
           ))
}

ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot5,plot6,plot7,plot8,labels = c("E","F","G","H"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot9,labels = c("I"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,labels = c("A","B","C","D","E","F","G","H","I"),ncol = 2, nrow = 5, common.legend = TRUE)


for ( i in 1:length(inci_cname_F)){
  assign(paste0("plot",i),ggplot(subset(mdata,Type==c("Incidence") & Sex=="Female" &  Cancer==inci_cname_F[i]), aes(x = Year, y = Rate, shape=Comparison, color = Comparison)) +
           geom_line(size = 1.2) +
           geom_point(size = 1.5,fill = "white") +
           scale_fill_viridis(discrete = T, option = "E") +
           scale_color_manual(values = c("black", "chartreuse3", "brown3",'cadetblue3')) + 
           scale_shape_manual(values = c(15,16,17,18)) +
           theme_classic() +
           facet_wrap(~ Sex, strip.position = "top", nrow = 1) +
           labs(x = "Year", y = "Incidence Rate (per 100,000)",title=title_nameF_I[i]) +
           theme_ipsum() +
           theme(
             strip.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5,size=15), # 패널 레이블 위치 조정
             axis.title.x = element_text(hjust = 0.5, vjust = 0.5,size=15), # X축 이름 중앙 정렬
             axis.title.y = element_text(hjust = 0.5, vjust = 0.5,size=15), # Y축 이름 중앙 정렬
             plot.margin = margin(1, 1, 1, 1, "cm"), # 그래프 안에 레이블 넣기 위해 여백 조정
             legend.text = element_text(size = 15),legend.title = element_text(size = 15),plot.title = element_text(size = 15, face = "bold")
           ))
}

ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot5,plot6,labels = c("E","F"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,labels = c("A","B","C","D","E","F"),ncol = 2, nrow = 5, common.legend = TRUE)


for ( i in 1:length(mort_cname_F)){
  assign(paste0("plot",i),ggplot(subset(mdata,Type==c("Mortality") & Sex=="Female" &  Cancer==mort_cname_F[i]), aes(x = Year, y = Rate, shape=Comparison, color = Comparison)) +
           geom_line(size = 1.2) +
           geom_point(size = 1.5,fill = "white") +
           scale_fill_viridis(discrete = T, option = "E") +
           scale_color_manual(values = c("black", "chartreuse3", "brown3",'cadetblue3')) + 
           scale_shape_manual(values = c(15,16,17,18)) +
           theme_classic() +
           facet_wrap(~ Sex, strip.position = "top", nrow = 1) +
           labs(x = "Year", y = "Mortality Rate (per 100,000)",title=title_nameF_D[i]) +
           theme_ipsum() +
           theme(
             strip.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5,size=15), # 패널 레이블 위치 조정
             axis.title.x = element_text(hjust = 0.5, vjust = 0.5,size=15), # X축 이름 중앙 정렬
             axis.title.y = element_text(hjust = 0.5, vjust = 0.5,size=15), # Y축 이름 중앙 정렬
             plot.margin = margin(1, 1, 1, 1, "cm"), # 그래프 안에 레이블 넣기 위해 여백 조정
             legend.text = element_text(size = 15),legend.title = element_text(size = 15),plot.title = element_text(size = 15, face = "bold")
           ))
}

ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot5,plot6,plot7,plot8,labels = c("E","F","G","H"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,labels = c("A","B","C","D","E","F","G","H"),ncol = 2, nrow = 5, common.legend = TRUE)

############################################################################################################################################

















