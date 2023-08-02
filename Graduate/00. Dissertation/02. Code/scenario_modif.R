ageing_effect_check_modif <- function(Sex,Method,Extract_M){
  if (Sex == 1 & Method == 1) {
    changes_history <-list()
    for (j in 1:length(inci_cname_M)){
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene1_nd_byage_male_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios1_male[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
      }
      
      years = c(2000:2040)
      change1 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change1$Group <- factor(change1$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change1$Type <-c("Median")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene2_nd_byage_male_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios2_male[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
      }
      
      years = c(2000:2040)
      change2 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change),
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change2$Group <- factor(change2$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change2$Type <-c("High")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene3_nd_byage_male_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios3_male[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
      }
      
      years = c(2000:2040)
      change3 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change3$Group <- factor(change3$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change3$Type <-c("Low")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene4_nd_byage_male_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios4_male[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
      }
      
      years = c(2000:2040)
      change4 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change4$Group <- factor(change4$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change4$Type <-c("Fast Ageing")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene5_nd_byage_male_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios5_male[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
      }

      years = c(2000:2040)
      change5 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change5$Group <- factor(change5$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change5$Type <-c("Slow Ageing")
      
      
      change_total <- rbind(change1,change2,change3,change4,change5)
      change_total$Type <- factor(change_total$Type,levels=c("Median","High","Low","Fast Ageing","Slow Ageing"))
      
      assign(paste0("plot",j),ggplot(data = change_total, aes(x = Year, y = Change,group = Group,colour = Group,linetype = Group)) + geom_line() + 
               scale_x_continuous(breaks = seq(2000, 2040, 5)) + geom_hline(yintercept = 0, color = 'black', linetype = 2) +
               theme_void(base_family = "AppleSDGothicNeo-Regular") +
               facet_wrap(~ Type, strip.position = "top", nrow = 1) +
               theme_few() + theme(legend.position = "top",text=element_text(size=10),axis.text.x=element_text(angle=90, hjust=1),legend.text = element_text(size = 15),legend.title = element_text(size = 15),plot.title = element_text(size = 15, face = "bold")) + 
               labs(title=title_nameM_I[j], x = 'Calender Year',y = "Difference Total Incidence Case (reference 2000)",family = "AppleSDGothicNeo-ExtraBold"))
      
      changes_history[[j]] <- change_total
      
    } 
  }
  
  else if (Sex == 1 & Method == 2) {
    changes_history <-list()
    for (j in 1:length(mort_cname_M)){
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene1_nd_byage_male_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios1_male[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
      }
      
      
      years = c(2000:2040)
      change1 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change1$Group <- factor(change1$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change1$Type <-c("Median")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene2_nd_byage_male_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios2_male[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
        
      }
      
      
      years = c(2000:2040)
      change2 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change2$Group <- factor(change2$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change2$Type <-c("High")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene3_nd_byage_male_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios3_male[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
      }
      
      
      years = c(2000:2040)
      change3 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change3$Group <- factor(change3$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change3$Type <-c("Low")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene4_nd_byage_male_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios4_male[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
        
        
      }
      
      
      years = c(2000:2040)
      change4 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change),  
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change4$Group <- factor(change4$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change4$Type <-c("Fast Ageing")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene5_nd_byage_male_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios5_male[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
        
      }
      
      
      years = c(2000:2040)
      change5 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change5$Group <- factor(change5$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change5$Type <-c("Slow Ageing")
      
      
      change_total <- rbind(change1,change2,change3,change4,change5)
      change_total$Type <- factor(change_total$Type,levels=c("Median","High","Low","Fast Ageing","Slow Ageing"))
      
      assign(paste0("plot",j),ggplot(data = change_total, aes(x = Year, y = Change,group = Group,colour = Group,linetype = Group)) + geom_line() + 
               scale_x_continuous(breaks = seq(2000, 2040, 5)) + geom_hline(yintercept = 0, color = 'black', linetype = 2)+ 
               facet_wrap(~ Type, strip.position = "top", nrow = 1) +
               theme_few() + theme(legend.position = "top",text=element_text(size=10),axis.text.x=element_text(angle=90, hjust=1),legend.text = element_text(size = 15),legend.title = element_text(size = 15),plot.title = element_text(size = 15, face = "bold")) + 
               labs(title=title_nameM_D[j], x = 'Calender Year',y = "Difference Total Death Case (reference 2005)",family = "AppleSDGothicNeo-ExtraBold"))
      
      changes_history[[j]] <- change_total
      
    }
  }
  else if (Sex == 2 & Method == 1) {
    changes_history <-list()
    for (j in 1:length(inci_cname_F)){
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene1_nd_byage_female_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios1_female[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
        
        
      }
      
      
      
      years = c(2000:2040)
      change1 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change1$Group <- factor(change1$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change1$Type <-c("Median")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene2_nd_byage_female_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios2_female[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
        
        
      }
      
      
      years = c(2000:2040)
      change2 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change2$Group <- factor(change2$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change2$Type <-c("High")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene3_nd_byage_female_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios3_female[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
        
      }
      
      
      years = c(2000:2040)
      change3 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change3$Group <- factor(change3$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change3$Type <-c("Low")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene4_nd_byage_female_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios4_female[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
        
        
      }
      
      
      years = c(2000:2040)
      change4 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change4$Group <- factor(change4$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change4$Type <-c("Fast Ageing")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene5_nd_byage_female_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios5_female[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
        
      }
      
      
      years = c(2000:2040)
      change5 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change5$Group <- factor(change5$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change5$Type <-c("Slow Ageing")
      
      
      change_total <- rbind(change1,change2,change3,change4,change5)
      change_total$Type <- factor(change_total$Type,levels=c("Median","High","Low","Fast Ageing","Slow Ageing"))
      
      assign(paste0("plot",j),ggplot(data = change_total, aes(x = Year, y = Change,group = Group,colour = Group,linetype = Group)) + geom_line() + 
               scale_x_continuous(breaks = seq(2000, 2040, 5)) + geom_hline(yintercept = 0, color = 'black', linetype = 2) + 
               facet_wrap(~ Type, strip.position = "top", nrow = 1) +
               theme_few() + theme(legend.position = "top",text=element_text(size=10),axis.text.x=element_text(angle=90, hjust=1),legend.text = element_text(size = 15),legend.title = element_text(size = 15),plot.title = element_text(size = 15, face = "bold")) + 
               labs(title=title_nameF_I[j], x = 'Calender Year',y = "Difference Total Incidence Case (reference 2000)",family = "AppleSDGothicNeo-ExtraBold"))
      
      changes_history[[j]] <- change_total
    }
  }
  else if (Sex == 2 & Method == 2) {
    changes_history <-list()
    for (j in 1:length(mort_cname_F)){
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene1_nd_byage_female_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios1_female[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
        
      }
      
      
      years = c(2000:2040)
      change1 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change1$Group <- factor(change1$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change1$Type <-c("Median")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene2_nd_byage_female_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios2_female[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
        
      }
      
      
      years = c(2000:2040)
      change2 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change2$Group <- factor(change2$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change2$Type <-c("High")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene3_nd_byage_female_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios3_female[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
        
        
      }
      
      
      years = c(2000:2040)
      change3 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change3$Group <- factor(change3$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change3$Type <-c("Low")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene4_nd_byage_female_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios4_female[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
        
        
      }
      
      
      years = c(2000:2040)
      change4 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change4$Group <- factor(change4$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change4$Type <-c("Fast Ageing")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <- numeric(length = 41)
      sample<-data.frame(scene5_nd_byage_female_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios5_female[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
        
      }
      
      
      years = c(2000:2040)
      change5 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 41))
      change5$Group <- factor(change5$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      change5$Type <-c("Slow Ageing")
      
      
      change_total <- rbind(change1,change2,change3,change4,change5)
      change_total$Type <- factor(change_total$Type,levels=c("Median","High","Low","Fast Ageing","Slow Ageing"))
      
      assign(paste0("plot",j),ggplot(data = change_total, aes(x = Year, y = Change,group = Group,colour = Group,linetype = Group)) + geom_line() + 
               scale_x_continuous(breaks = seq(2000, 2040,5)) + geom_hline(yintercept = 0, color = 'black', linetype = 2)+ 
               facet_wrap(~ Type, strip.position = "top", nrow = 1) +
               theme_few() + theme(legend.position = "top",text=element_text(size=10),axis.text.x=element_text(angle=90, hjust=1),legend.text = element_text(size = 15),legend.title = element_text(size = 15),plot.title = element_text(size = 15, face = "bold")) + 
               labs(title=title_nameF_D[j], x = 'Calender Year',y = "Difference Total Death Case (reference 2005) ",family = "AppleSDGothicNeo-ExtraBold"))
      
      changes_history[[j]] <- change_total
    }
  }
  
  if (Extract_M=="Table") {
    
    return(changes_history)
    
  } else if (Extract_M=="Plot"){
    if (Sex == 1 & Method == 1 ) {
      plt1<-ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
      plt2<-ggarrange(plot5,plot6,plot7,plot8,plot9,labels = c("E","F","G","H","I"),ncol = 3, nrow = 2, common.legend = TRUE)
      result <- list(plt1,plt2)
      return(result)
    } else if(Sex == 1 & Method == 2){
      plt1<-ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
      plt2<-ggarrange(plot5,plot6,plot7,plot8,labels = c("E","F","G","H"),ncol = 2, nrow = 2, common.legend = TRUE)
      plt2<-ggarrange(plot5,plot6,plot7,plot8,plot9,labels = c("E","F","G","H","I"),ncol = 3, nrow = 2, common.legend = TRUE)
      result <- list(plt1,plt2)
      return(result)
    }
    else if (Sex == 2 & Method == 1){
      plt1<-ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
      plt2<-ggarrange(plot5,plot6,labels = c("E","F"),ncol = 2, nrow = 2, common.legend = TRUE)
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
