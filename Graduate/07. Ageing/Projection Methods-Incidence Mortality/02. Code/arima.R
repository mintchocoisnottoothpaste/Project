# ARIMA --> based age-adjusted model 20221113
# Update --> 221117
library(openxlsx)
library(TTR)
library(forecast)

# Data import
load("/Users/jun/Desktop/modeling methods/import_data.RData")


## Nominate Cancer name, except C60-C63,C81,C82_C86_C96, C82_C86,etc

Total_Ca_inci_Name <-c("C00_C14.ca.inc",'C15.ca.inc','C16.ca.inc','C18_C21.ca.inc','C22.ca.inc','C23_C24.ca.inc',
                       'C25.ca.inc','C32.ca.inc','C33_C34.ca.inc','C50.ca.inc','C53.ca.inc','C54_C55.ca.inc','C56.ca.inc','C61.ca.inc',
                       'C64.ca.inc','C67.ca.inc','C70_C72.ca.inc','C73.ca.inc','C90.ca.inc','C91_C95.ca.inc')

M_Ca_inci_Name <-c("C00_C14.ca.m",'C15.ca.m','C16.ca.m','C18_C21.ca.m','C22.ca.m','C23_C24.ca.m',
                   'C25.ca.m','C32.ca.m','C33_C34.ca.m','C61.ca.m',
                   'C64.ca.m','C67.ca.m','C70_C72.ca.m','C73.ca.m','C90.ca.m','C91_C95.ca.m')

W_Ca_inci_Name <-c("C00_C14.ca.w",'C15.ca.w','C16.ca.w','C18_C21.ca.w','C22.ca.w','C23_C24.ca.w',
                   'C25.ca.w','C32.ca.w','C33_C34.ca.w','C50.ca.w','C53.ca.w','C54_C55.ca.w','C56.ca.w',
                   'C64.ca.w','C67.ca.w','C70_C72.ca.w','C73.ca.w','C90.ca.w','C91_C95.ca.w')

Total_Ca_dth_Name <-c("C00_C14.ca.dth",'C15.ca.dth','C16.ca.dth','C18_C21.ca.dth','C22.ca.dth','C23_C24.ca.dth',
                      'C25.ca.dth','C32.ca.dth','C33_C34.ca.dth','C50.ca.dth','C53.ca.dth','C54_C55.ca.dth','C56.ca.dth','C61.ca.dth',
                      'C64.ca.dth','C67.ca.dth','C70_C72.ca.dth','C73.ca.dth','C90.ca.dth','C91_C95.ca.dth')

M_Ca_D_Name    <-c("C00_C14.ca.m_death",'C15.ca.m_death','C16.ca.m_death','C18_C21.ca.m_death','C22.ca.m_death','C23_C24.ca.m_death',
                   'C25.ca.m_death','C32.ca.m_death','C33_C34.ca.m_death','C61.ca.m_death',
                   'C64.ca.m_death','C67.ca.m_death','C70_C72.ca.m_death','C73.ca.m_death','C90.ca.m_death','C91_C95.ca.m_death')

W_Ca_D_Name    <-c("C00_C14.ca.w_death",'C15.ca.w_death','C16.ca.w_death','C18_C21.ca.w_death','C22.ca.m_death','C23_C24.ca.w_death',
                   'C25.ca.w_death','C32.ca.w_death','C33_C34.ca.w_death','C50.ca.w_death','C53.ca.w_death','C54_C55.ca.w_death','C56.ca.w_death',
                   'C64.ca.w_death','C67.ca.w_death','C70_C72.ca.w_death','C73.ca.w_death','C90.ca.w_death','C91_C95.ca.w_death')

##############################################################################################################################
# Name nominatd

Total_Name <- c("C00_C14",'C15','C16','C18_C21','C22','C23_C24','C25','C32','C33_C34','C50','C53','C54_C55','C56','C61','C64','C67','C70_C72','C73','C90','C91_C95')
Total_Name_fix <- c("C00-C14",'C15','C16','C18-C21','C22','C23-C24','C25','C32','C33-C34','C50','C53','C54-C55','C56','C61', 'C64','C67','C70-C72','C73','C90','C91-C95')

M_Name <-c("C00_C14",'C15','C16','C18_C21','C22','C23_C24','C25','C32','C33_C34','C61','C64','C67','C70_C72','C73','C90','C91_C95')
M_Name_fix <-c("C00-C14",'C15','C16','C18-C21','C22','C23-C24','C25','C32','C33-C34','C61','C64','C67','C70-C72','C73','C90','C91-C95')

W_Name <-c("C00_C14",'C15','C16','C18_C21','C22','C23_C24','C25','C32','C33_C34','C50','C53','C54_C55','C56','C64','C67','C70_C72','C73','C90','C91_C95')
W_Name_fix <-c("C00-C14",'C15','C16','C18-C21','C22','C23-C24','C25','C32','C33-C34','C50','C53','C54-C55','C56','C64','C67','C70-C72','C73','C90','C91-C95')

##############################################################################################################################
######## Construct AIRMA model 

case<-function(data) {
  rowSums(data) %>% diff %>% ggtsdisplay(main="")
  checkresiduals(auto.arima(rowSums(data),stepwise=FALSE))
  autoplot(forecast(auto.arima(rowSums(data),stepwise=FALSE)))
  autoplot(auto.arima(rowSums(data),stepwise=FALSE))
  
}

case(get(Total_Ca_inci_Name[11]))


















