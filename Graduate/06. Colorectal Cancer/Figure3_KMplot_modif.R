## 20220209 modified by hj

install.packages("palettes")
library(haven)
library(ggplot2)
library(palettes)
library(ggsci)
library(openxlsx)

windowsFonts(A=windowsFont("Times New Roman"))
theme_set(theme_minimal())


km<-read.csv("C:/Users/Home/OneDrive/대장암/20111115_KM.csv") #3213
km_all<-km[km$Age==2,]#133
km_old<-km[km$Age==1,]#133
km_young<-km[km$Age==0,]#133

color<-pal_jco("default")(4)
colors <- c("0"=color[1],"1-2"=color[2],"3-4"=color[3],"5+"=color[4])

tiff('C:/Users/Home/OneDrive/대장암/all_n294262.tiff', units="in", width=16, height=9, res=1000, compression = 'lzw')
ggplot(data = as.data.frame(km_all), aes(x =SURVTIME2_mon))+ 
  
  geom_line(aes(y = SURVIVAL0*100,colour="0"), size=1) + 
  geom_line(aes(y = SURVIVAL1_2*100,colour="1-2"), size=1) + 
  geom_line(aes(y = SURVIVAL3_4*100,colour="3-4"), size=1) + 
  geom_line(aes(y = SURVIVAL5*100,colour="5+"), size=1) +  
  
  geom_ribbon(aes(ymin = LCL0*100, ymax = UCL0*100),alpha=0.1,linetype="solid",color="grey")+
  geom_ribbon(aes(ymin = LCL1_2*100, ymax = UCL1_2*100),alpha=0.1,linetype="solid",color="grey")+
  geom_ribbon(aes(ymin = LCL3_4*100, ymax = UCL3_4*100),alpha=0.1,linetype="solid",color="grey")+
  geom_ribbon(aes(ymin = LCL5*100, ymax = UCL5*100),alpha=0.1,linetype="solid",color="grey")+
  
  ggtitle("\nA. Colorectal cancer patients (all age, n=294,262)") +
  
  xlab("\n Time since diagnosis (months)") +
  ylab("\n Survival probability (%)\n")+
  
  ylim(0,100)+
  scale_x_continuous(limit = c(0,132),breaks = c(0,12,24,36,48,60,72,84,96,108,120,132))+
  
  theme(axis.text=element_text(size=17,face="bold", family="A"),
        axis.title=element_text(size=19,face="bold", family="A"),
        strip.text.x = element_text(size=16,face="bold", family="A"),
        
        plot.title=element_text(size=20,face="bold", family="A",hjust = 0),
        plot.subtitle=element_text(size=18, family="A"),
        
        legend.title=element_text(size=19, family="A"),
        legend.text=element_text(size=19, family="A"),
        legend.position="bottom")+
  
  scale_color_manual(name = "Charlson Comorbiditiy Index",
                     values = c("0"=color[1],"1-2"=color[2],"3-4"=color[3],"5+"=color[4]), 
                     labels=c("0", "1-2","3-4", "5+"),
                     guide=guide_legend(title = "Charlson Comorbiditiy Index", title.position = "left",direction="horizontal"))+
  annotate(geom="text", x=15, y=5, label="Log rank : p <0.0001", color="black",fontface=4, family="A",size=7)
dev.off()


tiff('C:/Users/Home/OneDrive/대장암/young_n32191.tiff', units="in", width=16, height=9, res=1000, compression = 'lzw')
ggplot(data = as.data.frame(km_young), aes(x =SURVTIME2_mon))+ 
  
  geom_line(aes(y = SURVIVAL0*100,colour="0"), size=1) + 
  geom_line(aes(y = SURVIVAL1_2*100,colour="1-2"), size=1) + 
  geom_line(aes(y = SURVIVAL3_4*100,colour="3-4"), size=1) + 
  geom_line(aes(y = SURVIVAL5*100,colour="5+"), size=1) +  
  
  geom_ribbon(aes(ymin = LCL0*100, ymax = UCL0*100),alpha=0.1,linetype="solid",color="grey")+
  geom_ribbon(aes(ymin = LCL1_2*100, ymax = UCL1_2*100),alpha=0.1,linetype="solid",color="grey")+
  geom_ribbon(aes(ymin = LCL3_4*100, ymax = UCL3_4*100),alpha=0.1,linetype="solid",color="grey")+
  geom_ribbon(aes(ymin = LCL5*100, ymax = UCL5*100),alpha=0.1,linetype="solid",color="grey")+
  
  ggtitle("\nB. Colorectal cancer patients (aged 30-49, n=32,191)") +
  
  xlab("\n Time since diagnosis (months)") +
  ylab("\n Survival probability (%)\n")+
  
  ylim(0,100)+
  scale_x_continuous(limit = c(0,132),breaks = c(0,12,24,36,48,60,72,84,96,108,120,132))+
  
  theme(axis.text=element_text(size=17,face="bold", family="A"),
        axis.title=element_text(size=19,face="bold", family="A"),
        strip.text.x = element_text(size=16,face="bold", family="A"),
        
        plot.title=element_text(size=20,face="bold", family="A",hjust = 0),
        plot.subtitle=element_text(size=18, family="A"),
        
        legend.title=element_text(size=19, family="A"),
        legend.text=element_text(size=19, family="A"),
        legend.position="bottom")+
  
  scale_color_manual(name = "Charlson Comorbiditiy Index",
                     values = c("0"=color[1],"1-2"=color[2],"3-4"=color[3],"5+"=color[4]), 
                     labels=c("0", "1-2","3-4", "5+"),
                     guide=guide_legend(title = "Charlson Comorbiditiy Index", title.position = "left",direction="horizontal"))+
  annotate(geom="text", x=15, y=5, label="Log rank : p <0.0001", color="black",fontface=4, family="A",size=7)
dev.off()

tiff('C:/Users/Home/OneDrive/대장암/old_n42174.tiff', units="in", width=16, height=9, res=1000, compression = 'lzw')
ggplot(data = as.data.frame(km_old), aes(x =SURVTIME2_mon))+ 
  
  geom_line(aes(y = SURVIVAL0*100,colour="0"), size=1) + 
  geom_line(aes(y = SURVIVAL1_2*100,colour="1-2"), size=1) + 
  geom_line(aes(y = SURVIVAL3_4*100,colour="3-4"), size=1) + 
  geom_line(aes(y = SURVIVAL5*100,colour="5+"), size=1) +  
  
  geom_ribbon(aes(ymin = LCL0*100, ymax = UCL0*100),alpha=0.1,linetype="solid",color="grey")+
  geom_ribbon(aes(ymin = LCL1_2*100, ymax = UCL1_2*100),alpha=0.1,linetype="solid",color="grey")+
  geom_ribbon(aes(ymin = LCL3_4*100, ymax = UCL3_4*100),alpha=0.1,linetype="solid",color="grey")+
  geom_ribbon(aes(ymin = LCL5*100, ymax = UCL5*100),alpha=0.1,linetype="solid",color="grey")+
  
  ggtitle("\nC. Colorectal cancer patients (aged 80+, n=42,174)") +
  
  xlab("\n Time since diagnosis (months)") +
  ylab("\n Survival probability (%)\n")+
  
  ylim(0,100)+
  scale_x_continuous(limit = c(0,132),breaks = c(0,12,24,36,48,60,72,84,96,108,120,132))+
  
  theme(axis.text=element_text(size=17,face="bold", family="A"),
        axis.title=element_text(size=19,face="bold", family="A"),
        strip.text.x = element_text(size=16,face="bold", family="A"),
        
        plot.title=element_text(size=20,face="bold", family="A",hjust = 0),
        plot.subtitle=element_text(size=18, family="A"),
        
        legend.title=element_text(size=19, family="A"),
        legend.text=element_text(size=19, family="A"),
        legend.position="bottom")+
  
  scale_color_manual(name = "Charlson Comorbiditiy Index",
                     values = c("0"=color[1],"1-2"=color[2],"3-4"=color[3],"5+"=color[4]), 
                     labels=c("0", "1-2","3-4", "5+"),
                     guide=guide_legend(title = "Charlson Comorbiditiy Index", title.position = "left",direction="horizontal"))+
  annotate(geom="text", x=15, y=5, label="Log rank : p <0.0001", color="black",fontface=4, family="A",size=7)
dev.off()