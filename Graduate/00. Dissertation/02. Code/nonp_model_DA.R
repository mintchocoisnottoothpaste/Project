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

######################################################################################################################################################

inci_cname_M <- inci_cname[inci_cname %in% c("C16_I","C18_C20_I","C22_I","C23_C24_I","C25_I","C33_C34_I","C61_I","C64_I","C67_I")]
inci_cname_F <- inci_cname[inci_cname %in% c("C16_I","C18_C20_I","C22_I","C23_C24_I","C25_I","C33_C34_I","C50_I")]

mort_cname_M <- mort_cname[mort_cname %in% c("C15_D","C16_D","C18_C21_D","C22_D","C25_D","C33_C34_D","C61_D","C67_D","C82_C86_D","C91_C95_D")]
mort_cname_F <- mort_cname[mort_cname %in% c("C16_D","C18_C21_D","C22_D","C25_D","C33_C34_D","C53_D","C82_C86_D","C91_C95_D")]

DA_modif2 <- function (count, population, reference, comparison){
  ## Reference
  N1     <- sum(population[,colnames(population) %like% as.character(reference)])
  e1     <- count[,colnames(population) %like% as.character(reference)]/population[,colnames(population) %like% as.character(reference)]
  s1     <- (population[,colnames(population) %like% as.character(reference)]/colSums(population)[colnames(population) %like% as.character(reference)])
  
  ## Comparison
  N2     <- sum(population[,colnames(population) %like% as.character(comparison)])
  e2     <- count[,colnames(population) %like% as.character(comparison)]/population[,colnames(population) %like% as.character(comparison)]
  s2     <- population[,colnames(population) %like% as.character(comparison)]/colSums(population)[colnames(population) %like% as.character(comparison)]
  
  
  M_p   <- sum((N2-N1)*s1*e1)
  M_a   <- sum(N1*(s2-s1)*e1)
  M_e  <- sum(N1*s1*(e2-e1))
  I_pa  <- sum((N2-N1)*(s2-s1)*e1)
  I_pe <- sum((N2-N1)*s1*(e2-e1))
  I_ae  <- sum(N1*(s2-s1)*(e2-e1))
  I_pae <- sum((N2-N1)*(s2-s1)*(e2-e1))
  
  
  # Effect of changes in population size
  P <-  (M_p + (1/2 * I_pe) + (1/2 * I_pa) + (1/3 * I_pae))
  
  # Effect of changes in  age structure
  A <-  (M_a + (1/2 * I_ae) + (1/2 * I_pa) + (1/3 * I_pae))
  
  # Effect of changes in mortality  or Incidence
  E <-  (M_e + (1/2 * I_pe) + (1/2 * I_ae) + (1/3 * I_pae))
  
  method3        <- list(P,A,E)
  names(method3) <- c("P","A","E")
  return(method3)
}

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


ARIMA_INCI_M<-data.frame(scene1_ts_ARIMA[1])
ARIMA_INCI_F<-data.frame(scene1_ts_ARIMA[2])
ARIMA_MORT_M<-data.frame(scene1_ts_ARIMA[3])
ARIMA_MORT_F<-data.frame(scene1_ts_ARIMA[4])

SSML_INCI_M<-data.frame(scene1_ts_SSML[1])
SSML_INCI_F<-data.frame(scene1_ts_SSML[2])
SSML_MORT_M<-data.frame(scene1_ts_SSML[3])
SSML_MORT_F<-data.frame(scene1_ts_SSML[4])

jp_model_inci_M_dat$POP <- as.numeric(colSums(Scenarios1_male[,as.character(2020:2040)]))
jp_model_mort_M_dat$POP <- as.numeric(colSums(Scenarios1_male[,as.character(2020:2040)]))
jp_model_inci_F_dat$POP <- as.numeric(colSums(Scenarios1_female[,as.character(2020:2040)]))
jp_model_mort_F_dat$POP <- as.numeric(colSums(Scenarios1_female[,as.character(2020:2040)]))

jp_model_inci_M_dat$CASE <- jp_model_inci_M_dat$POP * jp_model_inci_M_dat$model/100000
jp_model_mort_M_dat$CASE <- jp_model_mort_M_dat$POP * jp_model_mort_M_dat$model/100000
jp_model_inci_F_dat$CASE <- jp_model_inci_F_dat$POP * jp_model_inci_F_dat$model/100000
jp_model_mort_F_dat$CASE <- jp_model_mort_F_dat$POP * jp_model_mort_F_dat$model/100000

jp_model_inci_M_dat<-jp_model_inci_M_dat[,c(1,2,9)] 
jp_model_mort_M_dat<-jp_model_mort_M_dat[,c(1,2,9)]
jp_model_inci_F_dat<-jp_model_inci_F_dat[,c(1,2,9)]
jp_model_mort_F_dat<-jp_model_mort_F_dat[,c(1,2,9)]


JP_INCI_M<-data.frame(t(reshape(jp_model_inci_M_dat,idvar = "cancer",timevar = "year",direction = 'wide')))[-1,];colnames(JP_INCI_M)<-inci_cname_M;rownames(JP_INCI_M)<-NULL
JP_MORT_M<-data.frame(t(reshape(jp_model_mort_M_dat,idvar = "cancer",timevar = "year",direction = 'wide')))[-1,];colnames(JP_MORT_M)<-mort_cname_M;rownames(JP_MORT_M)<-NULL
JP_INCI_F<-data.frame(t(reshape(jp_model_inci_F_dat,idvar = "cancer",timevar = "year",direction = 'wide')))[-1,];colnames(JP_INCI_F)<-inci_cname_F;rownames(JP_INCI_F)<-NULL
JP_MORT_F<-data.frame(t(reshape(jp_model_mort_F_dat,idvar = "cancer",timevar = "year",direction = 'wide')))[-1,];colnames(JP_MORT_F)<-mort_cname_F;rownames(JP_MORT_F)<-NULL


JP_INCI_M <- as.data.frame(lapply(JP_INCI_M, as.numeric))
JP_MORT_M <- as.data.frame(lapply(JP_MORT_M, as.numeric))
JP_INCI_F <- as.data.frame(lapply(JP_INCI_F, as.numeric))
JP_MORT_F <- as.data.frame(lapply(JP_MORT_F, as.numeric))

ageing_effect_check_modif <- function(Sex,Method,Extract_M) {
  if (Sex == 1 & Method == 1) {
    changes_history <-list()
    for (j in 1:length(inci_cname_M)){
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample_ARIMA<-data.frame(cbind("Year" = c(2000:2040),c(as.numeric(colSums(subset(get(inci_cname_M[j]),Sex=="남자")[,as.character(2000:2019)])),ARIMA_INCI_M[,j]),"Age"=1))
      colnames(sample_ARIMA) <-c("Year","Case","Age")
      sample_ARIMA<-reshape(sample_ARIMA,idvar = "Age",timevar = "Year",direction = 'wide')[,-1]
      colnames(sample_ARIMA) <- c(2000:2040)
      
      sample_mpop<-data.frame(cbind("Year" = c(2000:2040),c(as.numeric(colSums(Scenarios1_male[,c(as.character(2000:2040))]))),"Age"=1))
      colnames(sample_mpop) <-c("Year","Case","Age")
      sample_mpop<-reshape(sample_mpop,idvar = "Age",timevar = "Year",direction = 'wide')[,-1]
      colnames(sample_mpop) <- c(2000:2040)
      
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA_modif2(sample_ARIMA,sample_mpop,2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample_ARIMA[,i])-sum(sample_ARIMA[,1])
      }
      
      years = c(2000:2040)
      change_ARIMA <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                           Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change_ARIMA$Group <- factor(change_ARIMA$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change_ARIMA$Type <-c("ARIMA")
      
      #######
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample_SSML<-data.frame(cbind("Year" = c(2000:2040),c(as.numeric(colSums(subset(get(inci_cname_M[j]),Sex=="남자")[,as.character(2000:2019)])),SSML_INCI_M[,j]),"Age"=1))
      colnames(sample_SSML) <-c("Year","Case","Age")
      sample_SSML<-reshape(sample_SSML,idvar = "Age",timevar = "Year",direction = 'wide')[,-1]
      colnames(sample_SSML) <- c(2000:2040)
      
      
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA_modif2(sample_SSML,sample_mpop,2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample_SSML[,i])-sum(sample_SSML[,1])
      }
      
      years = c(2000:2040)
      change_SSML <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                           Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change_SSML$Group <- factor(change_SSML$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change_SSML$Type <-c("SSML")
      
      #######
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample_JP<-data.frame(cbind("Year" = c(2000:2040),c(as.numeric(colSums(subset(get(inci_cname_M[j]),Sex=="남자")[,as.character(2000:2019)])),JP_INCI_M[,j]),"Age"=1))
      colnames(sample_JP) <-c("Year","Case","Age")
      sample_JP<-reshape(sample_JP,idvar = "Age",timevar = "Year",direction = 'wide')[,-1]
      colnames(sample_JP) <- c(2000:2040)
      
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA_modif2(sample_JP,sample_mpop,2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample_JP[,i])-sum(sample_JP[,1])
      }
      
      years = c(2000:2040)
      change_JP <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                                Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change_JP$Group <- factor(change_JP$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change_JP$Type <-c("JP")
      
      change_total <- rbind(change_ARIMA,change_SSML,change_JP)
      change_total$Type <- factor(change_total$Type,levels=c("ARIMA","SSML","JP"))
      
      assign(paste0("plot",j),ggplot(data = change_total, aes(x = Year, y = Change,group = Group,colour = Group,linetype = Group)) + geom_line() + 
               scale_x_continuous(breaks = seq(2000, 2040, 5)) + geom_hline(yintercept = 0, color = 'black', linetype = 2) +
               theme_void(base_family = "AppleSDGothicNeo-Regular") +
               facet_wrap(~ Type, strip.position = "top", nrow = 1) +
               theme_few() + theme(legend.position = "top",text=element_text(size=10),axis.text.x=element_text(angle=90, hjust=1)) + 
               labs(title=title_nameM_I[j], x = 'Calender Year',y = "Difference Total Incidence Case (reference 2000)",family = "AppleSDGothicNeo-ExtraBold"))
      
      changes_history[[j]] <- change_total
      
    } 
  }
  
  else if (Sex == 1 & Method == 2) {
    changes_history <-list()
    for (j in 1:length(mort_cname_M)){
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample_ARIMA<-data.frame(cbind("Year" = c(2000:2040),c(as.numeric(colSums(subset(get(mort_cname_M[j]),Sex=="남자")[,as.character(2000:2019)])),ARIMA_MORT_M[,j]),"Age"=1))
      colnames(sample_ARIMA) <-c("Year","Case","Age")
      sample_ARIMA<-reshape(sample_ARIMA,idvar = "Age",timevar = "Year",direction = 'wide')[,-1]
      colnames(sample_ARIMA) <- c(2000:2040)
      
      sample_mpop<-data.frame(cbind("Year" = c(2000:2040),c(as.numeric(colSums(Scenarios1_male[,c(as.character(2000:2040))]))),"Age"=1))
      colnames(sample_mpop) <-c("Year","Case","Age")
      sample_mpop<-reshape(sample_mpop,idvar = "Age",timevar = "Year",direction = 'wide')[,-1]
      colnames(sample_mpop) <- c(2000:2040)
      
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA_modif2(sample_ARIMA,sample_mpop,2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample_ARIMA[,i])-sum(sample_ARIMA[,6])
      }
      
      years = c(2000:2040)
      change_ARIMA <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                                 Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change_ARIMA$Group <- factor(change_ARIMA$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change_ARIMA$Type <-c("ARIMA")
      
      #######
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample_SSML<-data.frame(cbind("Year" = c(2000:2040),c(as.numeric(colSums(subset(get(mort_cname_M[j]),Sex=="남자")[,as.character(2000:2019)])),SSML_MORT_M[,j]),"Age"=1))
      colnames(sample_SSML) <-c("Year","Case","Age")
      sample_SSML<-reshape(sample_SSML,idvar = "Age",timevar = "Year",direction = 'wide')[,-1]
      colnames(sample_SSML) <- c(2000:2040)
      
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA_modif2(sample_SSML,sample_mpop,2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample_SSML[,i])-sum(sample_SSML[,6])
      }
      
      years = c(2000:2040)
      change_SSML <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                                Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change_SSML$Group <- factor(change_SSML$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change_SSML$Type <-c("SSML")
      
      #######
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample_JP<-data.frame(cbind("Year" = c(2000:2040),c(as.numeric(colSums(subset(get(mort_cname_M[j]),Sex=="남자")[,as.character(2000:2019)])),JP_MORT_M[,j]),"Age"=1))
      colnames(sample_JP) <-c("Year","Case","Age")
      sample_JP<-reshape(sample_JP,idvar = "Age",timevar = "Year",direction = 'wide')[,-1]
      colnames(sample_JP) <- c(2000:2040)
      
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA_modif2(sample_JP,sample_mpop,2005,1999+i)))))
        P_year_d[i] <- case_d[1]
        A_year_d[i] <- case_d[2]
        E_year_d[i] <- case_d[3]
        net_change[i] <- sum(sample_JP[,i])-sum(sample_JP[,6])
      }
      
      years = c(2000:2040)
      change_JP <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                              Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change_JP$Group <- factor(change_JP$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change_JP$Type <-c("JP")
      
      change_total <- rbind(change_ARIMA,change_SSML,change_JP)
      change_total$Type <- factor(change_total$Type,levels=c("ARIMA","SSML","JP"))
      assign(paste0("plot",j),ggplot(data = change_total, aes(x = Year, y = Change,group = Group,colour = Group,linetype = Group)) + geom_line() + 
               scale_x_continuous(breaks = seq(2000, 2040, 5)) + geom_hline(yintercept = 0, color = 'black', linetype = 2)+ 
               facet_wrap(~ Type, strip.position = "top", nrow = 1) +
               theme_few() + theme(legend.position = "top",text=element_text(size=10),axis.text.x=element_text(angle=90, hjust=1)) + 
               labs(title=title_nameM_D[j], x = 'Calender Year',y = "Difference Total Death Case (reference 2005)",family = "AppleSDGothicNeo-ExtraBold"))
      
      changes_history[[j]] <- change_total
      
    }
  }
  else if (Sex == 2 & Method == 1) {
    changes_history <-list()
    for (j in 1:length(inci_cname_F)){
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample_ARIMA<-data.frame(cbind("Year" = c(2000:2040),c(as.numeric(colSums(subset(get(inci_cname_F[j]),Sex=="여자")[,as.character(2000:2019)])),ARIMA_INCI_F[,j]),"Age"=1))
      colnames(sample_ARIMA) <-c("Year","Case","Age")
      sample_ARIMA<-reshape(sample_ARIMA,idvar = "Age",timevar = "Year",direction = 'wide')[,-1]
      colnames(sample_ARIMA) <- c(2000:2040)
      
      sample_fpop<-data.frame(cbind("Year" = c(2000:2040),c(as.numeric(colSums(Scenarios1_female[,c(as.character(2000:2040))]))),"Age"=1))
      colnames(sample_fpop) <-c("Year","Case","Age")
      sample_fpop<-reshape(sample_fpop,idvar = "Age",timevar = "Year",direction = 'wide')[,-1]
      colnames(sample_fpop) <- c(2000:2040)
      
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA_modif2(sample_ARIMA,sample_fpop,2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample_ARIMA[,i])-sum(sample_ARIMA[,1])
      }
      
      years = c(2000:2040)
      change_ARIMA <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                                 Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change_ARIMA$Group <- factor(change_ARIMA$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change_ARIMA$Type <-c("ARIMA")
      
      #######
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample_SSML<-data.frame(cbind("Year" = c(2000:2040),c(as.numeric(colSums(subset(get(inci_cname_F[j]),Sex=="여자")[,as.character(2000:2019)])),SSML_INCI_F[,j]),"Age"=1))
      colnames(sample_SSML) <-c("Year","Case","Age")
      sample_SSML<-reshape(sample_SSML,idvar = "Age",timevar = "Year",direction = 'wide')[,-1]
      colnames(sample_SSML) <- c(2000:2040)
      
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA_modif2(sample_SSML,sample_fpop,2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample_SSML[,i])-sum(sample_SSML[,1])
      }
      
      years = c(2000:2040)
      change_SSML <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                                Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change_SSML$Group <- factor(change_SSML$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change_SSML$Type <-c("SSML")
      
      #######
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample_JP<-data.frame(cbind("Year" = c(2000:2040),c(as.numeric(colSums(subset(get(inci_cname_F[j]),Sex=="여자")[,as.character(2000:2019)])),JP_INCI_F[,j]),"Age"=1))
      colnames(sample_JP) <-c("Year","Case","Age")
      sample_JP<-reshape(sample_JP,idvar = "Age",timevar = "Year",direction = 'wide')[,-1]
      colnames(sample_JP) <- c(2000:2040)
      
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA_modif2(sample_JP,sample_fpop,2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample_JP[,i])-sum(sample_JP[,1])
      }
      
      years = c(2000:2040)
      change_JP <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                              Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change_JP$Group <- factor(change_JP$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change_JP$Type <-c("JP")
      
      change_total <- rbind(change_ARIMA,change_SSML,change_JP)
      change_total$Type <- factor(change_total$Type,levels=c("ARIMA","SSML","JP"))
      
      assign(paste0("plot",j),ggplot(data = change_total, aes(x = Year, y = Change,group = Group,colour = Group,linetype = Group)) + geom_line() + 
               scale_x_continuous(breaks = seq(2000, 2040, 5)) + geom_hline(yintercept = 0, color = 'black', linetype = 2) + 
               facet_wrap(~ Type, strip.position = "top", nrow = 1) +
               theme_few() + theme(legend.position = "top",text=element_text(size=10),axis.text.x=element_text(angle=90, hjust=1)) + 
               labs(title=title_nameF_I[j], x = 'Calender Year',y = "Difference Total Incidence Case (reference 2000)",family = "AppleSDGothicNeo-ExtraBold"))
      
      changes_history[[j]] <- change_total
    }
  }
  else if (Sex == 2 & Method == 2) {
    changes_history <-list()
    for (j in 1:length(mort_cname_F)){
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample_ARIMA<-data.frame(cbind("Year" = c(2000:2040),c(as.numeric(colSums(subset(get(mort_cname_F[j]),Sex=="여자")[,as.character(2000:2019)])),ARIMA_MORT_F[,j]),"Age"=1))
      colnames(sample_ARIMA) <-c("Year","Case","Age")
      sample_ARIMA<-reshape(sample_ARIMA,idvar = "Age",timevar = "Year",direction = 'wide')[,-1]
      colnames(sample_ARIMA) <- c(2000:2040)
      
      sample_fpop<-data.frame(cbind("Year" = c(2000:2040),c(as.numeric(colSums(Scenarios1_female[,c(as.character(2000:2040))]))),"Age"=1))
      colnames(sample_fpop) <-c("Year","Case","Age")
      sample_fpop<-reshape(sample_fpop,idvar = "Age",timevar = "Year",direction = 'wide')[,-1]
      colnames(sample_fpop) <- c(2000:2040)
      
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA_modif2(sample_ARIMA,sample_fpop,2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample_ARIMA[,i])-sum(sample_ARIMA[,6])
      }
      
      years = c(2000:2040)
      change_ARIMA <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                                 Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change_ARIMA$Group <- factor(change_ARIMA$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change_ARIMA$Type <-c("ARIMA")
      
      #######
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample_SSML<-data.frame(cbind("Year" = c(2000:2040),c(as.numeric(colSums(subset(get(mort_cname_F[j]),Sex=="여자")[,as.character(2000:2019)])),SSML_MORT_F[,j]),"Age"=1))
      colnames(sample_SSML) <-c("Year","Case","Age")
      sample_ARIMA<-reshape(sample_SSML,idvar = "Age",timevar = "Year",direction = 'wide')[,-1]
      colnames(sample_SSML) <- c(2000:2040)
      
      
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA_modif2(sample_SSML,sample_fpop,2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample_SSML[,i])-sum(sample_SSML[,6])
      }
      
      years = c(2000:2040)
      change_SSML <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                                Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change_SSML$Group <- factor(change_SSML$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change_SSML$Type <-c("SSML")
      
      #######
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample_JP<-data.frame(cbind("Year" = c(2000:2040),c(as.numeric(colSums(subset(get(mort_cname_F[j]),Sex=="여자")[,as.character(2000:2019)])),JP_MORT_F[,j]),"Age"=1))
      colnames(sample_JP) <-c("Year","Case","Age")
      sample_ARIMA<-reshape(sample_JP,idvar = "Age",timevar = "Year",direction = 'wide')[,-1]
      colnames(sample_JP) <- c(2000:2040)
      
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA_modif2(sample_JP,sample_fpop,2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample_JP[,i])-sum(sample_JP[,6])
      }
      
      years = c(2000:2040)
      change_JP <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                              Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change_JP$Group <- factor(change_JP$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change_JP$Type <-c("JP")
      
      change_total <- rbind(change_ARIMA,change_SSML,change_JP)
      change_total$Type <- factor(change_total$Type,levels=c("ARIMA","SSML","JP"))
      
      assign(paste0("plot",j),ggplot(data = change_total, aes(x = Year, y = Change,group = Group,colour = Group,linetype = Group)) + geom_line() + 
               scale_x_continuous(breaks = seq(2000, 2040,5)) + geom_hline(yintercept = 0, color = 'black', linetype = 2)+ 
               facet_wrap(~ Type, strip.position = "top", nrow = 1) +
               theme_few() + theme(legend.position = "top",text=element_text(size=10),axis.text.x=element_text(angle=90, hjust=1)) + 
               labs(title=title_nameF_D[j], x = 'Calender Year',y = "Difference Total Death Case (reference 2005) ",family = "AppleSDGothicNeo-ExtraBold"))
      
      changes_history[[j]] <- change_total
      
    }
  }
  
  if (Extract_M=="Table") {
    
    return(changes_history)
    
  } else if (Extract_M=="Plot"){
    if (Sex == 1 & Method == 1 ) {
      plt1<-ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
      plt2<-ggarrange(plot5,plot6,plot7,plot8,labels = c("E","F","G","H"),ncol = 2, nrow = 2, common.legend = TRUE)
      plt3<-ggarrange(plot9,labels = c("I"),ncol = 2, nrow = 2, common.legend = TRUE)
      result <- list(plt1,plt2,plt3)
      return(result)
    } else if(Sex == 1 & Method == 2){
      plt1<-ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
      plt2<-ggarrange(plot5,plot6,plot7,plot8,labels = c("E","F","G","H"),ncol = 2, nrow = 2, common.legend = TRUE)
      plt3<-ggarrange(plot9,plot10,labels = c("I","J"),ncol = 2, nrow = 2, common.legend = TRUE)
      result <- list(plt1,plt2,plt3)
      return(result)
    }
    else if (Sex == 2 & Method == 1){
      plt1<-ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
      plt2<-ggarrange(plot5,plot6,plot7,labels = c("E","F"),ncol = 2, nrow = 2, common.legend = TRUE)
      result <- list(plt1,plt2)
      return(result)
    } else if (Sex == 2 & Method == 2) {
      plt1<-ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
      plt2<-ggarrange(plot5,plot6,plot7,plot8,labels = c("E","F","G","H"),ncol = 2, nrow = 2, common.legend = TRUE)
      result <- list(plt1,plt2)
      return(result)
    }
  }
}

# Male, Incidence
ageing_effect_check_modif(1,1,"Table")
ageing_effect_check_modif(1,1,"Plot")
# Male, Death
ageing_effect_check_modif(1,2,"Table")
ageing_effect_check_modif(1,2,"Plot")
# Female, Incidence
ageing_effect_check_modif(2,1,"Table")
ageing_effect_check_modif(2,1,"Plot")
# Female, Death
ageing_effect_check_modif(2,2,"Table")
ageing_effect_check_modif(2,2,"Plot")


#########################################################################################################################
###65+ 65-
#########################################################################################################################





