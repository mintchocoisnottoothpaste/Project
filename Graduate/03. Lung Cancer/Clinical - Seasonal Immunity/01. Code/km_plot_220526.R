library("openxlsx") # open xlsx
library("MatchIt") # propensity mathcing method
library("ggplot2")
library("survival")
library("survminer")
library("tidyverse")
library("grid")

load(file="/Users/jun/Library/CloudStorage/OneDrive-개인/lung/lung_data.RData")
load(file="/Users/jun/Library/CloudStorage/OneDrive-개인/lung/opt.data2.RData")

##############################################################################################################################################################
##############Color version###################################################################################################################################
##############################################################################################################################################################

# log-rank test
pchisq(survdiff(Surv(Censoring.PFS.60,new.PD.60==1) ~ factor(Season2), data = lung_data)$chisq, length(survdiff(Surv(Censoring.PFS.60,new.PD.60==1) ~ factor(Season2), data = lung_data)$n)-1, lower.tail = FALSE)
unmatched_PFS <- surv_summary(survfit(Surv(Censoring.PFS.60,new.PD.60==1) ~ factor(Season2), data = lung_data),lung_data)

unmatched_PFS$strata <- factor(unmatched_PFS$strata , levels=rev(levels(unmatched_PFS$strata )))

tiff('/Users/jun/Desktop/color_PFS_km.tiff',units="in", width=8, height=6, res=1000, compression = 'lzw')

ggsurvplot(survfit(Surv(Censoring.PFS.60,new.PD.60==1) ~ factor(Season2), data = lung_data), data = lung_data, censor.shape="|", censor.size = 4)


plot1<-
  ggplot(data = unmatched_PFS, aes(x =time),group=strata)+ 
  geom_step(aes(y = surv*100, color=strata,linetype=strata,size=strata), size=0.5) + 
  xlab("\n Follow-up time (months)") +
  ylab("\n\n Progression-free survival probability (%)\n")+
  ylim(0,100)+
  scale_x_continuous(limit = c(0,60.5),breaks = c(0,12,24,36,48,60),labels = c(0,12,24,36,48,60))+
  theme_bw()+
  theme(axis.text=element_text(size=13,face="bold", family="Helvetica",color="black"),
        axis.title=element_text(size=13,face="bold", family="Helvetica"),
        strip.text.x = element_text(size=13,face="bold", family="Helvetica"),
        panel.grid = element_blank(),
        
        plot.title=element_text(size=16,face="bold", family="Helvetica",hjust = 0.5),
        plot.subtitle=element_text(size=14, family="Helvetica"),
        
        legend.title= element_blank(),
        legend.text=element_text(size=11, family="Helvetica"),

        legend.position=c(0.65, 0.85),
        legend.key.width = unit(1.3,"cm"),
        legend.key = element_blank(),
        legend.background = element_blank()
  ) + 
  scale_linetype_manual(values = c("solid", "dashed"),
                        labels=c("Winter season (n=177)     2.7 (95% CI: 1.9-3.8)",
                                 "Other season  (n=376)      2.1 (95% CI: 1.8-2.7)")) +
  scale_color_manual(values= c("#26668a","#e5234c"),
                     labels=c("Winter season (n=177)     2.7 (95% CI: 1.9-3.8)",
                              "Other season  (n=376)      2.1 (95% CI: 1.8-2.7)"))+
  scale_size_manual(values=c(1.5, 1.5))+
  annotate('text', x = 28, y = 96.5, label ="Seasonal Infusion", color = 'black',size=4, fontface=2)+
  annotate('text', x = 49.5, y = 96.5, label ="Median PFS", color = 'black',size=4, fontface=2)+
  annotate('text', x = 51.5, y = 78.5, label ="Log rank-test, p=0.124", color = 'black',size=4, fontface=3)+
  annotate("rect", xmin = 20, xmax = 60.5, ymin = 75, ymax = 100,fill="white",alpha=0.01,colour = "black",lty = 1) 


### Create new dataframe for the table

PFS_time0<-as.data.frame(unmatched_PFS %>% group_by(strata) %>% filter(time <12 ) %>% slice(c(1)))[c(1,2,5,9)]
PFS_time12<-as.data.frame(unmatched_PFS %>% group_by(strata) %>% filter(time >=12 ) %>% slice(c(1)))[c(1,2,5,9)]
PFS_time24<-as.data.frame(unmatched_PFS %>% group_by(strata) %>% filter(time >=24 ) %>% slice(c(1)))[c(1,2,5,9)]
PFS_time36<-as.data.frame(unmatched_PFS %>% group_by(strata) %>% filter(time >=36 ) %>% slice(c(1)))[c(1,2,5,9)]
PFS_time48<-as.data.frame(unmatched_PFS %>% group_by(strata) %>% filter(time >=48 ) %>% slice(c(1)))[c(1,2,5,9)]
PFS_time60<-as.data.frame(unmatched_PFS %>% group_by(strata) %>% filter(time >=60 ) %>% slice(c(1)))[c(1,2,5,9)]

time <-c(0,0,12,12,24,24,36,36,48,48,60,60)
PFS_table<-rbind(PFS_time0,PFS_time12,PFS_time24,PFS_time36,PFS_time48,PFS_time60)
PFS_table$time <-time
PFS_table$strata <-factor(c("Winter season", "Other season"),levels = c("Winter season", "Other season"))
PFS_table$strata <- factor(PFS_table$strata , levels=rev(levels(PFS_table$strata )))

data_table <- ggplot(PFS_table, aes(x = time, y = strata,label = format(n.risk, nsmall = 0))) +
  geom_text(size=4,fontface=1, family="Helvetica") + 
  theme_bw()+
  ggtitle("Number of patient at risk") +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_blank(),
        text=element_text(size=10,face="bold", family="Helvetica"),
        plot.title=element_text(size=12,face="bold", family="Helvetica",hjust = 0),
        axis.text.y=element_text(size=11,face="bold", family="Helvetica",color="black"),
        legend.position = "none",
        #panel.border = element_blank(),
        axis.ticks =element_blank()) + 
  scale_color_manual(values= c("black","black"))+
  scale_size_manual(values=c(1.5, 1.5))+
  xlab(NULL) + ylab(NULL)

Layout <- grid.layout(nrow = 2, ncol = 1,heights = unit(c(2, 0.5), c("null", "null")))
grid.show.layout(Layout)

vplayout <- function(...) {
  grid.newpage()
  pushViewport(viewport(layout = Layout))
}

subplot <- function(x, y) viewport(layout.pos.row = x,layout.pos.col = y)
mmplot <- function(a, b) {
  vplayout()
  print(a, vp = subplot(1, 1))
  print(b, vp = subplot(2, 1))
}

mmplot(plot1, data_table)

dev.off()
##############################################################################################################################################################

# log-rank test
pchisq(survdiff(Surv(Censoring.OS.60,new.PD.60==1) ~ factor(Season2), data = lung_data)$chisq, length(survdiff(Surv(Censoring.OS.60,new.PD.60==1) ~ factor(Season2), data = lung_data)$n)-1, lower.tail = FALSE)
unmatched_OS <- surv_summary(survfit(Surv(Censoring.OS.60,new.PD.60==1) ~ factor(Season2), data = lung_data),lung_data)

unmatched_OS$strata <- factor(unmatched_OS$strata , levels=rev(levels(unmatched_OS$strata )))
tiff('/Users/jun/Desktop/color_OS_km.tiff',units="in", width=8, height=6, res=1000, compression = 'lzw')

plot2<-
  ggplot(data = unmatched_OS, aes(x =time),group=strata)+ 
  geom_step(aes(y = surv*100, color=strata,linetype=strata,size=strata), size=0.5) + 
  xlab("\n Follow-up time (months)") +
  ylab("\n\n Overall survival probability (%)\n")+
  ylim(0,100)+
  scale_x_continuous(limit = c(0,60.5),breaks = c(0,12,24,36,48,60),labels = c(0,12,24,36,48,60))+
  theme_bw()+
  theme(axis.text=element_text(size=13,face="bold", family="Helvetica",color="black"),
        axis.title=element_text(size=13,face="bold", family="Helvetica"),
        strip.text.x = element_text(size=13,face="bold", family="Helvetica"),
        panel.grid = element_blank(),
        
        plot.title=element_text(size=16,face="bold", family="Helvetica",hjust = 0.5),
        plot.subtitle=element_text(size=14, family="Helvetica"),
        
        legend.title= element_blank(),
        legend.text=element_text(size=11, family="Helvetica"),
        #legend.position="bottom",
        legend.position=c(0.65, 0.85),
        legend.key.width = unit(1.3,"cm"),
        legend.key = element_blank(),
        legend.background = element_blank()
  ) +
  
  scale_linetype_manual(values = c("solid", "dashed"), 
                        labels=c("Winter season (n=177)     11.8 (95% CI: 10.1-15.0)",
                                 "Other season  (n=376)       7.8 (95% CI:   6.7-10.3)")) +
  scale_color_manual(values= c("#26668a","#e5234c"), 
                     labels=c("Winter season (n=177)     11.8 (95% CI: 10.1-15.0)",
                              "Other season  (n=376)       7.8 (95% CI:   6.7-10.3)")) +
  scale_size_manual(values=c(1.5, 1.5))+
  annotate('text', x = 28, y = 96.5, label ="Seasonal Infusion", color = 'black',size=4, fontface=2)+
  annotate('text', x = 49.5, y = 96.5, label ="Median PFS", color = 'black',size=4, fontface=2)+
  annotate('text', x = 51.5, y = 78.5, label ="Log rank-test, p=0.036", color = 'black',size=4, fontface=3)+
  annotate("rect", xmin = 20, xmax = 60.5, ymin = 75, ymax = 100,fill="white",alpha=0.01,colour = "black",lty = 1) 

### Create new dataframe for the table

OS_time0<-as.data.frame(unmatched_OS %>% group_by(strata) %>% filter(time <12 ) %>% slice(c(1)))[c(1,2,5,9)]
OS_time12<-as.data.frame(unmatched_OS %>% group_by(strata) %>% filter(time >=12 ) %>% slice(c(1)))[c(1,2,5,9)]
OS_time24<-as.data.frame(unmatched_OS %>% group_by(strata) %>% filter(time >=24 ) %>% slice(c(1)))[c(1,2,5,9)]
OS_time36<-as.data.frame(unmatched_OS %>% group_by(strata) %>% filter(time >=36 ) %>% slice(c(1)))[c(1,2,5,9)]
OS_time48<-as.data.frame(unmatched_OS %>% group_by(strata) %>% filter(time >=48 ) %>% slice(c(1)))[c(1,2,5,9)]
OS_time60<-as.data.frame(unmatched_OS %>% group_by(strata) %>% filter(time >=60 ) %>% slice(c(1)))[c(1,2,5,9)]

time <-c(0,0,12,12,24,24,36,36,48,48,60,60)
OS_table<-rbind(OS_time0,OS_time12,OS_time24,OS_time36,OS_time48,OS_time60)
OS_table$time <-time
OS_table$strata <-factor(c("Winter season", "Other season"),levels = c("Winter season", "Other season"))
OS_table$strata <- factor(OS_table$strata , levels=rev(levels(OS_table$strata )))

data_table <- ggplot(OS_table, aes(x = time, y = strata,label = format(n.risk, nsmall = 0))) +
  geom_text(size=4,fontface=1, family="Helvetica") + 
  theme_bw()+
  ggtitle("Number of patient at risk") +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_blank(),
        text=element_text(size=10,face="bold", family="Helvetica"),
        plot.title=element_text(size=12,face="bold", family="Helvetica",hjust = 0),
        axis.text.y=element_text(size=11,face="bold", family="Helvetica",color="black"),
        legend.position = "none",
        #panel.border = element_blank(),
        axis.ticks =element_blank()) + 
  scale_color_manual(values= c("black","black"))+
  scale_size_manual(values=c(1.5, 1.5))+
  xlab(NULL) + ylab(NULL)

Layout <- grid.layout(nrow = 2, ncol = 1,heights = unit(c(2, 0.5), c("null", "null")))

grid.show.layout(Layout)

vplayout <- function(...) {
  grid.newpage()
  pushViewport(viewport(layout = Layout))
}

subplot <- function(x, y) viewport(layout.pos.row = x,
                                   layout.pos.col = y)
mmplot <- function(a, b) {
  vplayout()
  print(a, vp = subplot(1, 1))
  print(b, vp = subplot(2, 1))
}

mmplot(plot2, data_table)

dev.off()

##############################################################################################################################################################

# log-rank test
pchisq(survdiff(Surv(Censoring.PFS.60,new.PD.60==1) ~ factor(Season2), data = opt.data2)$chisq, length(survdiff(Surv(Censoring.PFS.60,new.PD.60==1) ~ factor(Season2), data = opt.data2)$n)-1, lower.tail = FALSE)
# Stratified log-rank test
pchisq(survdiff(Surv(Censoring.PFS.60,new.PD.60==1) ~ factor(Season2) + strata(subclass), data = opt.data2)$chisq, length(survdiff(Surv(Censoring.PFS.60,new.PD.60==1) ~ factor(Season2) + strata(subclass), data = opt.data2)$n)-1, lower.tail = FALSE)

matched_PFS <- surv_summary(survfit(Surv(Censoring.PFS.60,new.PD.60==1) ~ factor(Season2), data = opt.data2),opt.data2)

matched_PFS$strata <- factor(matched_PFS$strata , levels=rev(levels(matched_PFS$strata )))
tiff('/Users/jun/Desktop/color_matched_PFS_km.tiff',units="in", width=8, height=6, res=1000, compression = 'lzw')

plot3<-
  ggplot(data = matched_PFS, aes(x =time),group=strata)+ 
  geom_step(aes(y = surv*100, color=strata,linetype=strata,size=strata), size=0.5) + 
  xlab("\n Follow-up time (months)") +
  ylab("\n\n Progression-free survival probability (%)\n")+
  ylim(0,100)+
  scale_x_continuous(limit = c(0,60.5),breaks = c(0,12,24,36,48,60),labels = c(0,12,24,36,48,60))+
  theme_bw()+
  theme(axis.text=element_text(size=13,face="bold", family="Helvetica",color="black"),
        axis.title=element_text(size=13,face="bold", family="Helvetica"),
        strip.text.x = element_text(size=13,face="bold", family="Helvetica"),
        panel.grid = element_blank(),
        
        plot.title=element_text(size=16,face="bold", family="Helvetica",hjust = 0.5),
        plot.subtitle=element_text(size=14, family="Helvetica"),
        
        legend.title= element_blank(),
        legend.text=element_text(size=11, family="Helvetica"),
        #legend.position="bottom",
        legend.position=c(0.65, 0.85),
        legend.key.width = unit(1.3,"cm"),
        legend.key = element_blank(),
        legend.background = element_blank()
  ) +
  
  scale_linetype_manual(values = c("solid", "dashed"), 
                        labels=c("Winter season (n=177)     2.8 (95% CI: 1.9-4.0)",
                                 "Other season  (n=376)      1.5 (95% CI: 1.4-2.1)")) +
  scale_color_manual(values= c("#26668a","#e5234c"), 
                     labels=c("Winter season (n=177)     2.8 (95% CI: 1.9-4.0)",
                              "Other season  (n=376)      1.5 (95% CI: 1.4-2.1)"))+
  scale_size_manual(values=c(1.5, 1.5))+
  annotate('text', x = 28, y = 96.5, label ="Seasonal Infusion", color = 'black',size=4, fontface=2)+
  annotate('text', x = 49.5, y = 96.5, label ="Median PFS", color = 'black',size=4, fontface=2)+
  annotate('text', x = 51.5, y = 78.5, label ="Log rank-test, p=0.023", color = 'black',size=4, fontface=3)+
  annotate('text', x = 48.5, y = 74, label ="Stratified log rank-test, p=0.015", color = 'black',size=4, fontface=3)+
  annotate("rect", xmin = 20, xmax = 60.5, ymin = 70, ymax = 100,fill="white",alpha=0.01,colour = "black",lty = 1) 


### Create new dataframe for the table

PFS_time0<-as.data.frame(matched_PFS %>% group_by(strata) %>% filter(time <12 ) %>% slice(c(1)))[c(1,2,5,9)]
PFS_time12<-as.data.frame(matched_PFS %>% group_by(strata) %>% filter(time >=12 ) %>% slice(c(1)))[c(1,2,5,9)]
PFS_time24<-as.data.frame(matched_PFS %>% group_by(strata) %>% filter(time >=24 ) %>% slice(c(1)))[c(1,2,5,9)]
PFS_time36<-as.data.frame(matched_PFS %>% group_by(strata) %>% filter(time >=36 ) %>% slice(c(1)))[c(1,2,5,9)]
PFS_time48<-as.data.frame(matched_PFS %>% group_by(strata) %>% filter(time >=48 ) %>% slice(c(1)))[c(1,2,5,9)]
PFS_time60<-as.data.frame(matched_PFS %>% group_by(strata) %>% filter(time >=60 ) %>% slice(c(1)))[c(1,2,5,9)]

time <-c(0,0,12,12,24,24,36,36,48,48,60,60)
PFS_table<-rbind(PFS_time0,PFS_time12,PFS_time24,PFS_time36,PFS_time48,PFS_time60)
PFS_table$time <-time
PFS_table$strata <-factor(c("Winter season", "Other season"),levels = c("Winter season", "Other season"))
PFS_table$strata <- factor(PFS_table$strata , levels=rev(levels(PFS_table$strata )))

data_table <- ggplot(PFS_table, aes(x = time, y = strata,label = format(n.risk, nsmall = 0))) +
  geom_text(size=4,fontface=1, family="Helvetica") + 
  theme_bw()+
  ggtitle("Number of patient at risk") +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_blank(),
        text=element_text(size=10,face="bold", family="Helvetica"),
        plot.title=element_text(size=12,face="bold", family="Helvetica",hjust = 0),
        axis.text.y=element_text(size=11,face="bold", family="Helvetica",color="black"),
        legend.position = "none",
        #panel.border = element_blank(),
        axis.ticks =element_blank()) + 
  scale_color_manual(values= c("black","black"))+
  scale_size_manual(values=c(1.5, 1.5))+
  xlab(NULL) + ylab(NULL)

Layout <- grid.layout(nrow = 2, ncol = 1,heights = unit(c(2, 0.5), c("null", "null")))

grid.show.layout(Layout)

vplayout <- function(...) {
  grid.newpage()
  pushViewport(viewport(layout = Layout))
}

subplot <- function(x, y) viewport(layout.pos.row = x,
                                   layout.pos.col = y)
mmplot <- function(a, b) {
  vplayout()
  print(a, vp = subplot(1, 1))
  print(b, vp = subplot(2, 1))
}

mmplot(plot3, data_table)

dev.off()

##############################################################################################################################################################

# log-rank test
pchisq(survdiff(Surv(Censoring.OS.60,new.PD.60==1) ~ factor(Season2), data = opt.data2)$chisq, length(survdiff(Surv(Censoring.OS.60,new.PD.60==1) ~ factor(Season2), data = opt.data2)$n)-1, lower.tail = FALSE)
# Stratified log-rank test
pchisq(survdiff(Surv(Censoring.OS.60,new.PD.60==1) ~ factor(Season2) + strata(subclass), data = opt.data2)$chisq, length(survdiff(Surv(Censoring.OS.60,new.PD.60==1) ~ factor(Season2) + strata(subclass), data = opt.data2)$n)-1, lower.tail = FALSE)

matched_OS <- surv_summary(survfit(Surv(Censoring.OS.60,new.PD.60==1) ~ factor(Season2), data = opt.data2),opt.data2)

matched_OS$strata <- factor(matched_OS$strata , levels=rev(levels(matched_OS$strata )))
tiff('/Users/jun/Desktop/color_mathced_OS_km.tiff',units="in", width=8, height=6, res=1000, compression = 'lzw')

plot4<-
  ggplot(data = matched_OS, aes(x =time),group=strata)+ 
  geom_step(aes(y = surv*100, color=strata,linetype=strata,size=strata), size=0.5) + 
  xlab("\n Follow-up time (months)") +
  ylab("\n\n Overall survival probability (%)\n")+
  ylim(0,100)+
  scale_x_continuous(limit = c(0,60.5),breaks = c(0,12,24,36,48,60),labels = c(0,12,24,36,48,60))+
  theme_bw()+
  theme(axis.text=element_text(size=13,face="bold", family="Helvetica",color="black"),
        axis.title=element_text(size=13,face="bold", family="Helvetica"),
        strip.text.x = element_text(size=13,face="bold", family="Helvetica"),
        panel.grid = element_blank(),
        
        plot.title=element_text(size=16,face="bold", family="Helvetica",hjust = 0.5),
        plot.subtitle=element_text(size=14, family="Helvetica"),
        
        legend.title= element_blank(),
        legend.text=element_text(size=11, family="Helvetica"),
        #legend.position="bottom",
        legend.position=c(0.65, 0.85),
        legend.key.width = unit(1.3,"cm"),
        legend.key = element_blank(),
        legend.background = element_blank()
  ) +
  
  scale_linetype_manual(values = c("solid", "dashed"), 
                        labels=c("Winter season (n=177)     11.7 (95% CI: 9.9-15.0)",
                                 "Other season  (n=376)       5.1 (95% CI:   3.6-7.4)")) +
  scale_color_manual(values= c("#26668a","#e5234c"), 
                     labels=c("Winter season (n=177)     11.7 (95% CI: 9.9-15.0)",
                              "Other season  (n=376)       5.1 (95% CI:   3.6-7.4)")) +
  scale_size_manual(values=c(1.5, 1.5))+
  annotate('text', x = 28, y = 96.5, label ="Seasonal Infusion", color = 'black',size=4, fontface=2)+
  annotate('text', x = 49.5, y = 96.5, label ="Median PFS", color = 'black',size=4, fontface=2)+
  annotate('text', x = 51.5, y = 78.5, label ="Log rank-test, p=0.011", color = 'black',size=4, fontface=3)+
  annotate('text', x = 48.5, y = 74, label ="Stratified log rank-test, p=0.003", color = 'black',size=4, fontface=3)+
  annotate("rect", xmin = 20, xmax = 60.5, ymin = 70, ymax = 100,fill="white",alpha=0.01,colour = "black",lty = 1) 


### Create new dataframe for the table

OS_time0<-as.data.frame(matched_OS %>% group_by(strata) %>% filter(time <12 ) %>% slice(c(1)))[c(1,2,5,9)]
OS_time12<-as.data.frame(matched_OS %>% group_by(strata) %>% filter(time >=12 ) %>% slice(c(1)))[c(1,2,5,9)]
OS_time24<-as.data.frame(matched_OS %>% group_by(strata) %>% filter(time >=24 ) %>% slice(c(1)))[c(1,2,5,9)]
OS_time36<-as.data.frame(matched_OS %>% group_by(strata) %>% filter(time >=36 ) %>% slice(c(1)))[c(1,2,5,9)]
OS_time48<-as.data.frame(matched_OS %>% group_by(strata) %>% filter(time >=48 ) %>% slice(c(1)))[c(1,2,5,9)]
OS_time60<-as.data.frame(matched_OS %>% group_by(strata) %>% filter(time >=60 ) %>% slice(c(1)))[c(1,2,5,9)]

time <-c(0,0,12,12,24,24,36,36,48,48,60,60)
OS_table<-rbind(OS_time0,OS_time12,OS_time24,OS_time36,OS_time48,OS_time60)
OS_table$time <-time
OS_table$strata <-factor(c("Winter season", "Other season"),levels = c("Winter season", "Other season"))
OS_table$strata <- factor(OS_table$strata , levels=rev(levels(OS_table$strata )))

data_table <- ggplot(OS_table, aes(x = time, y = strata,label = format(n.risk, nsmall = 0))) +
  geom_text(size=4,fontface=1, family="Helvetica") + 
  theme_bw()+
  ggtitle("Number of patient at risk") +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_blank(),
        text=element_text(size=10,face="bold", family="Helvetica"),
        plot.title=element_text(size=12,face="bold", family="Helvetica",hjust = 0),
        axis.text.y=element_text(size=11,face="bold", family="Helvetica",color="black"),
        legend.position = "none",
        #panel.border = element_blank(),
        axis.ticks =element_blank()) + 
  scale_color_manual(values= c("black","black"))+
  scale_size_manual(values=c(1.5, 1.5))+
  xlab(NULL) + ylab(NULL)

Layout <- grid.layout(nrow = 2, ncol = 1,heights = unit(c(2, 0.5), c("null", "null")))

grid.show.layout(Layout)

vplayout <- function(...) {
  grid.newpage()
  pushViewport(viewport(layout = Layout))
}

subplot <- function(x, y) viewport(layout.pos.row = x,
                                   layout.pos.col = y)
mmplot <- function(a, b) {
  vplayout()
  print(a, vp = subplot(1, 1))
  print(b, vp = subplot(2, 1))
}

mmplot(plot4, data_table)

dev.off()