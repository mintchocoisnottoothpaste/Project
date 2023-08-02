## Library :

library(reshape)
library(ggplot2)
library(dplyr)
library(gridExtra)

## plot with default options:

colors_bor = rgb(0.3,0.3,0.3,0.4)

windowsFonts(A=windowsFont("Times New Roman"))

comorbid <- c("Myocardial\ninfarction", "Congestive\nheart failure", "Peripheral\nvascular\ndisease", 
              "Cerebrovascular\ndisease", "Dementia","Chronic pulmonary\ndisease",
              "Rheumatic\ndisease","Peptic ulcer\ndisease", "Mild liver\ndisease", 
              "DM\nwithout C.C", "DM\nwith C.C","Hemiplegia or\nparaplegia",
              "Renal disease", "Moderate or\nsevere liver\ndisease")

## data preparation for each cancer site :

age_a<- structure(list(
  Category = structure(1:14,.Label=comorbid, class="factor"),
  Caner = c(1.3787033324045,5.3149914022198,12.0790316112852,10.5181776783954, 5.2463450938280,
            19.5635182252550,2.0366204266946,18.2330712086508,14.6393350143749,20.9734862129667,
            8.4309221034316,1.1724245740191,1.9037456416391,0.3663402002297),
  General = c(1.3912771612,4.9452528699,11.7038557476, 10.7475650951,5.7244904201,
              20.0559365463,2.2452780175, 17.9146474910,13.3948657999,18.3390991701,
              7.3468541640, 1.3335055155,1.6889710530,0.2229305857
  )),
  .Names = c("Comorbid", "Cancer", "General"),row.names = c(NA,14L),class = "data.frame")
age_a <- melt(age_a, id.vars = c("Comorbid"), measure.vars = c("Cancer", "General"), variable.name = "Group", value.name = "Prevalence")


age_b<- structure(list(
  Category = structure(1:14,.Label=comorbid, class="factor"),
  Caner = c(0.195706875,0.531204374,1.888726663,0.981640831,0.080767916,
            9.434313939,0.736230623,9.142306856,8.166878941,5.110123948,
            1.48488708,0.20502625,0.481501041,0.164642291),
  General = c(0.195706875,0.469075207,1.491099997,0.876021248,0.062129167,
              9.260352272,0.68652729, 7.321922276,6.116616446,3.625236867,
              1.087260414,0.14911,0.292007083,0.099406666)),
  .Names = c("Comorbid", "Cancer", "General"),row.names = c(NA,14L),class = "data.frame")
age_b <- melt(age_b, id.vars = c("Comorbid"), measure.vars = c("Cancer", "General"))
age_b <- setNames(age_b,c("Comorbid","Group","Prevalence"))



age_c<- structure(list(
  Category = structure(1:14,.Label=comorbid, class="factor"),
  Caner = c(0.805997768,2.300670552,7.773824626,4.879863410,0.785013211, 
            13.202148056,1.456519043,14.248514389,14.459313805,16.275431853, 
            6.222875075,0.704890356,1.109320005,0.398706588 ),
  General = c(0.714428791,1.948702296, 7.124257194,4.609925696,0.674367363, 
              13.590362365,1.690210704,14.137868541,12.836825990,13.352855331, 
              4.792109806,0.632398249,0.853689944, 0.238460878)),
  .Names = c("Comorbid", "Cancer", "General"),row.names = c(NA,14L),class = "data.frame")
age_c <- melt(age_c, id.vars = c("Comorbid"), measure.vars = c("Cancer", "General"), variable.name = "Group", value.name = "Prevalence")

age_d<- structure(list(
  Category = structure(1:14,.Label=comorbid, class="factor"),
  Caner = c(1.819951677,6.667072259,16.113612265,13.977298406,5.578925412, 
            24.234733786,2.592605469,22.252255384,16.298736290,27.055050496, 
            11.435971423,1.506196875,2.384015019,0.406751378),
  General = c(1.855585878,6.136904865,15.842444680,14.366667246,5.822280937, 
              24.986528533,2.847259643,22.404352587,15.588659633,24.279928384, 
              10.274817918,1.735646370,2.157172904,0.239879018)),
  .Names = c("Comorbid", "Cancer", "General"),row.names = c(NA,14L),class = "data.frame")
age_d <- melt(age_d, id.vars = c("Comorbid"), measure.vars = c("Cancer", "General"), variable.name = "Group", value.name = "Prevalence")

age_e <- structure(list(
  Category = structure(1:14,.Label=comorbid, class="factor"),
  Caner = c(2.501541234,12.7709015,19.55233082,22.37634562,19.37212501,
            30.36467966,2.954426898,24.11201214,15.50007113, 28.16901408,
            11.02337933,2.162469768,3.653909992,0.329586949),
  General = c(2.719685114,12.55987101,19.59264002,23.66623986,22.33366529,
              30.91715275,3.172570778,23.1398492,14.35244463,25.75757576,
              10.48750415,2.883293024,3.554322568,0.232370655)),
  .Names = c("Comorbid", "Cancer", "General"),row.names = c(NA,14L),class = "data.frame")
age_e <- melt(age_e, id.vars = c("Comorbid"), measure.vars = c("Cancer", "General"), variable.name = "Group", value.name = "Prevalence")

## polar area chart for each cancer site :

tiff('C:/Users/SHLee/Desktop/age_a.tiff', units="in", width=10, height=8.5, res=1000, compression = 'lzw')

ggplot(data=age_a, aes(x=Comorbid,y=Prevalence,group=Group,colour=Group,fill=Group)) +
  geom_point() +
  # horizontal(inner) line 
  geom_hline(yintercept = seq(0, 35, by = 5), color = colors_bor, size = 0.2) +
  
  # vertical line 
  geom_vline(xintercept = seq(0, 14, by = 1), color = colors_bor, size = 0.2) +
  
  geom_polygon(alpha=0.4) +
  coord_polar() +
  theme_minimal() +
  labs(x = NULL, y = NULL, family="Times New Roman") + 
  
  # add label manually
  geom_text(x =rep(0.5,28), y = c(10,20,30,40,50,60,70,80,90,100,110,120,130,140,10,20,30,40,50,60,70,80,90,100,110,120,130,140), 
            label = c('10','20','30',NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),family="Times New Roman", color="black")+
  
  geom_text(aes(label = Comorbid, y = 35),family="Times New Roman", size=4.5,color="black",lineheight = 1) +
  theme(text=element_text(family="Times New Roman", face=2, size=15,color="black"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.grid = element_blank())

dev.off()

tiff('C:/Users/SHLee/Desktop/age_b.tiff', units="in", width=10, height=8.5, res=1000, compression = 'lzw')

ggplot(data=age_b, aes(x=Comorbid,y=Prevalence,group=Group,colour=Group,fill=Group)) +
  geom_point() +
  # horizontal(inner) line 
  geom_hline(yintercept = seq(0, 35, by = 5), color = colors_bor, size = 0.2) +
  
  # vertical line 
  geom_vline(xintercept = seq(0, 14, by = 1), color = colors_bor, size = 0.2) +
  
  geom_polygon(alpha=0.4) +
  coord_polar() +
  theme_minimal() +
  labs(x = NULL, y = NULL, family="Times New Roman") + 
  
  # add label manually
  geom_text(x =rep(0.5,28), y = c(10,20,30,40,50,60,70,80,90,100,110,120,130,140,10,20,30,40,50,60,70,80,90,100,110,120,130,140), 
            label = c('10','20','30',NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),family="Times New Roman", color="black")+
  
  geom_text(aes(label = Comorbid, y = 35),family="Times New Roman", size=4.5,color="black",lineheight = 1) +
  theme(text=element_text(family="Times New Roman", face=2, size=15,color="black"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.grid = element_blank())

dev.off()

tiff('C:/Users/Home/OneDrive/대장암/age_b_modif.tiff', units="in", width=10, height=8.5, res=1000, compression = 'lzw')

ggplot(data=age_b, aes(x=Comorbid,y=Prevalence,group=Group,colour=Group,fill=Group)) +
  geom_point() +
  # horizontal(inner) line 
  geom_hline(yintercept = seq(0, 15, by = 5), color = colors_bor, size = 0.2) +
  
  # vertical line 
  geom_vline(xintercept = seq(0, 14, by = 1), color = colors_bor, size = 0.2) +
  
  geom_polygon(alpha=0.4) +
  coord_polar() +
  theme_minimal() +
  labs(x = NULL, y = NULL, family="A") + 
  
  # add label manually
  geom_text(x =rep(0.5,28), y = c(5,10,15,40,50,60,70,80,90,100,110,120,130,140,10,20,30,40,50,60,70,80,90,100,110,120,130,140), 
            label = c('5','10','15',NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),family="A", color="black")+
  
  geom_text(aes(label = Comorbid, y = 15),family="A", size=4.5,color="black",lineheight = 1) +
  theme(text=element_text(family="A", face=2, size=15,color="black"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.grid = element_blank())

dev.off()



tiff('C:/Users/Home/OneDrive/대장암/age_b_modif2.tiff', units="in", width=10, height=8.5, res=1000, compression = 'lzw')

ggplot(data=age_b, aes(x=Comorbid,y=Prevalence,group=Group,colour=Group,fill=Group)) +
  geom_point() +
  # horizontal(inner) line 
  geom_hline(yintercept = seq(0, 10, by = 2), color = colors_bor, size = 0.2) +
  
  # vertical line 
  geom_vline(xintercept = seq(0, 14, by = 1), color = colors_bor, size = 0.2) +
  
  geom_polygon(alpha=0.4) +
  coord_polar() +
  theme_minimal() +
  labs(x = NULL, y = NULL, family="A") + 
  
  # add label manually
  
  geom_text(x =rep(0.5,28), y = c(5,10,15,40,50,60,70,80,90,100,110,120,130,140,10,20,30,40,50,60,70,80,90,100,110,120,130,140), 
            label = c('5','10',NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),family="A", color="black",size=6)+
  
  geom_text(aes(label = Comorbid, y = 10.5),family="A", size=6,color="black",lineheight = 1) +
  theme(text=element_text(family="A", face=2, size=15,color="black"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.grid = element_blank())

dev.off()

tiff('C:/Users/Shiny/Desktop/age_c.tiff', units="in", width=10, height=8.5, res=1000, compression = 'lzw')

  ggplot(data=age_c, aes(x=Comorbid,y=Prevalence,group=Group,colour=Group,fill=Group)) +
  geom_point() +
  # horizontal(inner) line 
  geom_hline(yintercept = seq(0, 35, by = 5), color = colors_bor, size = 0.2) +
  
  # vertical line 
  geom_vline(xintercept = seq(0, 14, by = 1), color = colors_bor, size = 0.2) +
  
  geom_polygon(alpha=0.4) +
  coord_polar() +
  theme_minimal() +
  labs(x = NULL, y = NULL, family="Times New Roman") + 
  
  # add label manually
  geom_text(x =rep(0.5,28), y = c(10,20,30,40,50,60,70,80,90,100,110,120,130,140,10,20,30,40,50,60,70,80,90,100,110,120,130,140), 
            label = c('10','20','30',NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),family="Times New Roman", color="black")+
  
  geom_text(aes(label = Comorbid, y = 35),family="Times New Roman", size=4.5,color="black",lineheight = 1) +
  theme(text=element_text(family="Times New Roman", face=2, size=15,color="black"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.grid = element_blank())

dev.off()


tiff('C:/Users/Shiny/Desktop/age_d.tiff', units="in", width=10, height=8.5, res=1000, compression = 'lzw')

ggplot(data=age_d, aes(x=Comorbid,y=Prevalence,group=Group,colour=Group,fill=Group)) +
  geom_point() +
  # horizontal(inner) line 
  geom_hline(yintercept = seq(0, 35, by = 5), color = colors_bor, size = 0.2) +
  
  # vertical line 
  geom_vline(xintercept = seq(0, 14, by = 1), color = colors_bor, size = 0.2) +
  
  geom_polygon(alpha=0.4) +
  coord_polar() +
  theme_minimal() +
  labs(x = NULL, y = NULL, family="Times New Roman") + 
  
  # add label manually
  geom_text(x =rep(0.5,28), y = c(10,20,30,40,50,60,70,80,90,100,110,120,130,140,10,20,30,40,50,60,70,80,90,100,110,120,130,140), 
            label = c('10','20','30',NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),family="Times New Roman", color="black")+
  
  geom_text(aes(label = Comorbid, y = 35),family="Times New Roman", size=4.5,color="black",lineheight = 1) +
  theme(text=element_text(family="Times New Roman", face=2, size=15,color="black"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.grid = element_blank())

dev.off()


tiff('C:/Users/Shiny/Desktop/age_e.tiff', units="in", width=10, height=8.5, res=1000, compression = 'lzw')

ggplot(data=age_e, aes(x=Comorbid,y=Prevalence,group=Group,colour=Group,fill=Group)) +
  geom_point() +
  # horizontal(inner) line 
  geom_hline(yintercept = seq(0, 35, by = 5), color = colors_bor, size = 0.2) +
  
  # vertical line 
  geom_vline(xintercept = seq(0, 14, by = 1), color = colors_bor, size = 0.2) +
  
  geom_polygon(alpha=0.4) +
  coord_polar() +
  theme_minimal() +
  labs(x = NULL, y = NULL, family="Times New Roman") + 
  
  # add label manually
  geom_text(x =rep(0.5,28), y = c(10,20,30,40,50,60,70,80,90,100,110,120,130,140,10,20,30,40,50,60,70,80,90,100,110,120,130,140), 
            label = c('10','20','30',NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),family="Times New Roman", color="black")+
  
  geom_text(aes(label = Comorbid, y = 35),family="Times New Roman", size=4.5,color="black",lineheight = 1) +
  theme(text=element_text(family="Times New Roman", face=2, size=15,color="black"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.grid = element_blank())

dev.off()