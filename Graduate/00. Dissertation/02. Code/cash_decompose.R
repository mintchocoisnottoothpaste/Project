ageing_effect_check <- function(Sex,Method,Extract_M) {
  if (Sex == 1 & Method == 1) {
    changes_history <-list()
    for (j in 1:length(inci_cname_M)){
      P_year <- A_year <- E_year <- net_change <- numeric(length = 20)
      
      for (i in 1:20){
        case <- (as.numeric(as.vector(unlist(DA(subset(get(inci_cname_M[j]),Sex=="害切")[,-c(1:3,24)],current_population_male[,-c(1:5)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum((subset(get(inci_cname_M[j]),Sex=="害切")[,-c(1:3)])[,i])-sum((subset(get(inci_cname_M[j]),Sex=="害切")[,-c(1:3)])[,1])
      }
      
      years = c(2000:2019)
      change <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                           Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 20))
      change$Group <- factor(change$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      
      assign(paste0("plot",j),ggplot(data = change, aes(x = Year, y = Change,group = Group,colour = Group,linetype = Group)) + geom_line() + 
               scale_x_continuous(breaks = seq(2000, 2019, 2)) + geom_hline(yintercept = 0, color = 'black', linetype = 2) +
               theme_void(base_family = "AppleSDGothicNeo-Regular") +
               theme_few() + theme(legend.position = "top",text=element_text(size=10),axis.text.x=element_text(angle=90, hjust=1),legend.text = element_text(size = 15),legend.title = element_text(size = 15),plot.title = element_text(size = 15, face = "bold")) + 
               labs(title=title_nameM_I[j], x = 'Calender Year',y = "Difference Total Incidence Case (reference 2000)",family = "AppleSDGothicNeo-ExtraBold"))
      
      changes_history[[j]] <- change
      
    } 
  }
  
  else if (Sex == 1 & Method == 2) {
    changes_history <-list()
    for (j in 1:length(mort_cname_M)){
      P_year <- A_year <- E_year <- net_change <- numeric(length = 20)
      
      for (i in 1:20){
        case <- (as.numeric(as.vector(unlist(DA(subset(get(mort_cname_M[j]),Sex=="害切")[,-c(1:3,24,25)],current_population_male[,-c(1:5)],2005,1999+i)))))
        P_year[i]          <- case[1]
        A_year[i]          <- case[2]
        E_year[i]          <- case[3]
        
        net_change[i] <- sum((subset(get(mort_cname_M[j]),Sex=="害切")[,-c(1:3)])[,i])-sum((subset(get(mort_cname_M[j]),Sex=="害切")[,-c(1:3)])[,6])
      }
      
      years = c(2000:2019)
      change <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                           Group = rep(c("Population Growth risk", "Population Aging risk", "Mortality Rate risk", "Net Change"), each = 20))
      change$Group <- factor(change$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Mortality Rate risk"))
      
      assign(paste0("plot",j),ggplot(data = change, aes(x = Year, y = Change,group = Group,colour = Group,linetype = Group)) + geom_line() + 
               scale_x_continuous(breaks = seq(2000, 2019, 2)) + geom_hline(yintercept = 0, color = 'black', linetype = 2)+ 
               theme_few() + theme(legend.position = "top",text=element_text(size=10),axis.text.x=element_text(angle=90, hjust=1),legend.text = element_text(size = 15),legend.title = element_text(size = 15),plot.title = element_text(size = 15, face = "bold")) + 
               labs(title=title_nameM_D[j], x = 'Calender Year',y = "Difference Total Death Case (reference 2005)",family = "AppleSDGothicNeo-ExtraBold"))
      
      changes_history[[j]] <- change
      
    }
  }
  else if (Sex == 2 & Method == 1) {
    changes_history <-list()
    for (j in 1:length(inci_cname_F)){
      P_year <- A_year <- E_year <- net_change <- numeric(length = 20)
      
      for (i in 1:20){
        case <- (as.numeric(as.vector(unlist(DA(subset(get(inci_cname_F[j]),Sex=="食切")[,-c(1:3,24)],current_population_female[,-c(1:5)],2000,1999+i)))))
        P_year[i]          <- case[1]
        A_year[i]          <- case[2]
        E_year[i]          <- case[3]
        
        net_change[i] <- sum((subset(get(inci_cname_F[j]),Sex=="食切")[,-c(1:3)])[,i])-sum((subset(get(inci_cname_F[j]),Sex=="食切")[,-c(1:3)])[,1])
      }
      years = c(2000:2019)
      change <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                           Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change"), each = 20))
      change$Group <- factor(change$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk"))
      
      assign(paste0("plot",j),ggplot(data = change, aes(x = Year, y = Change,group = Group,colour = Group,linetype = Group)) + geom_line() + 
               scale_x_continuous(breaks = seq(2000, 2019, 2)) + geom_hline(yintercept = 0, color = 'black', linetype = 2)+ 
               theme_few() + theme(legend.position = "top",text=element_text(size=10),axis.text.x=element_text(angle=90, hjust=1),legend.text = element_text(size = 15),legend.title = element_text(size = 15),plot.title = element_text(size = 15, face = "bold")) + 
               labs(title=title_nameF_I[j], x = 'Calender Year',y = "Difference Total Incidence Case (reference 2000)",family = "AppleSDGothicNeo-ExtraBold"))
      
      changes_history[[j]] <- change
    }
  }
  else if (Sex == 2 & Method == 2) {
    changes_history <-list()
    for (j in 1:length(mort_cname_F)){
      P_year <- A_year <- E_year <- net_change <- numeric(length = 20)
      
      for (i in 1:20){
        case <- (as.numeric(as.vector(unlist(DA(subset(get(mort_cname[j]),Sex=="食切")[,-c(1:3,24,25)],current_population_female[,-c(1:5)],2005,1999+i)))))
        P_year[i]          <- case[1]
        A_year[i]          <- case[2]
        E_year[i]          <- case[3]
        
        net_change[i] <- sum((subset(get(mort_cname[j]),Sex=="食切")[,-c(1:3)])[,i])-sum((subset(get(mort_cname[j]),Sex=="食切")[,-c(1:3)])[,6])
      }
      
      years = c(2000:2019)
      change <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change), 
                           Group = rep(c("Population Growth risk", "Population Aging risk", "Mortality Rate risk", "Net Change"), each = 20))
      change$Group <- factor(change$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Mortality Rate risk"))
      
      assign(paste0("plot",j),ggplot(data = change, aes(x = Year, y = Change,group = Group,colour = Group,linetype = Group)) + geom_line() + 
               scale_x_continuous(breaks = seq(2000, 2019, 2)) + geom_hline(yintercept = 0, color = 'black', linetype = 2)+ 
               theme_few() + theme(legend.position = "top",text=element_text(size=10),axis.text.x=element_text(angle=90, hjust=1),legend.text = element_text(size = 15),legend.title = element_text(size = 15),plot.title = element_text(size = 15, face = "bold")) + 
               labs(title=title_nameF_D[j], x = 'Calender Year',y = "Difference Total Death Case (reference 2005)",family = "AppleSDGothicNeo-ExtraBold"))
      
      changes_history[[j]] <- change
      
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
      plt <-ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,labels = c("A","B","C","D","E","F","G","H","I"),ncol = 2, nrow = 5, common.legend = TRUE)
      return(result)
    } else if(Sex == 1 & Method == 2){
      plt1<-ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
      plt2<-ggarrange(plot5,plot6,plot7,plot8,labels = c("E","F","G","H"),ncol = 2, nrow = 2, common.legend = TRUE)
      plt3<-ggarrange(plot9,labels = c("I"),ncol = 2, nrow = 2, common.legend = TRUE)
      result <- list(plt1,plt2,plt3)
      plt <-ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,labels = c("A","B","C","D","E","F","G","H","I"),ncol = 2, nrow = 5, common.legend = TRUE)
      
      return(result)
    }
    else if (Sex == 2 & Method == 1){
      plt1<-ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
      plt2<-ggarrange(plot5,plot6,labels = c("E","F"),ncol = 2, nrow = 2, common.legend = TRUE)
      result <- list(plt1,plt2)
      plt <-ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,labels = c("A","B","C","D","E","F"),ncol = 2, nrow = 5, common.legend = TRUE)
      return(result)
      
    } else if (Sex == 2 & Method == 2) {
      plt1<-ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
      plt2<-ggarrange(plot5,plot6,plot7,plot8,labels = c("E","F","G","H"),ncol = 2, nrow = 2, common.legend = TRUE)
      result <- list(plt1,plt2)
      plt <-ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,labels = c("A","B","C","D","E","F","G","H"),ncol = 2, nrow = 5, common.legend = TRUE)
      
      return(result)
    }
  }
}
