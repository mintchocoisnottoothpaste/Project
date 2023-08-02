# 65
for (i in 1:20){
  assign(paste0(M_Ca_inci_Name[i],"_M_DR_age"),((rowSums(get(M_Ca_inci_Name[i])[,14:18]) /rowSums(wh_pop_men[17:37,14:18])*100000)))
  assign(paste0(M_Ca_inci_Name[i],"_M_DR_age_case"),((rowSums(get(M_Ca_inci_Name[i])[,14:18]))))
  assign(paste0(M_Ca_inci_Name[i],"_M_DR_young"),((rowSums(get(M_Ca_inci_Name[i])[,1:13])  /rowSums(wh_pop_men[17:37,1:13])*100000)))
  assign(paste0(M_Ca_inci_Name[i],"_M_DR_young_case"),((rowSums(get(M_Ca_inci_Name[i])[,1:13]))))
  
}
for (i in 1:19){
  assign(paste0(M_Ca_D_Name[i],"_M_Death_DR_age"),((rowSums(get(M_Ca_D_Name[i])[,16:18]) /rowSums(wh_pop_men[1:39,14:18])*100000)))
  assign(paste0(M_Ca_D_Name[i],"_M_Death_DR_age_case"),((rowSums(get(M_Ca_D_Name[i])[,16:18]))))
  assign(paste0(M_Ca_D_Name[i],"_M_Death_DR_young"),((rowSums(get(M_Ca_D_Name[i])[,1:13])/rowSums(wh_pop_men[1:39,1:13])*100000)))
  assign(paste0(M_Ca_D_Name[i],"_M_Death_DR_young_case"),((rowSums(get(M_Ca_D_Name[i])[,1:13]))))
  
}
for (i in 1:22){
  assign(paste0(W_Ca_inci_Name[i],"_W_DR_age"),((rowSums(get(W_Ca_inci_Name[i])[,16:18]) /rowSums(wh_pop_women[17:37,14:18])*100000)))
  assign(paste0(W_Ca_inci_Name[i],"_W_DR_age_case"),((rowSums(get(W_Ca_inci_Name[i])[,16:18]))))
  assign(paste0(W_Ca_inci_Name[i],"_W_DR_young"),((rowSums(get(W_Ca_inci_Name[i])[,1:13])/rowSums(wh_pop_women[17:37,1:13])*100000)))
  assign(paste0(W_Ca_inci_Name[i],"_W_DR_young_case"),((rowSums(get(W_Ca_inci_Name[i])[,1:13]))))
  
}
for (i in 1:21){
  assign(paste0(W_Ca_D_Name[i],"_W_Death_DR_age"),((rowSums(get(W_Ca_D_Name[i])[,16:18]) /rowSums(wh_pop_women[1:39,14:18])*100000)))
  assign(paste0(W_Ca_D_Name[i],"_W_Death_DR_age_case"),((rowSums(get(W_Ca_D_Name[i])[,16:18]))))
  assign(paste0(W_Ca_D_Name[i],"_W_Death_DR_young"),((rowSums(get(W_Ca_D_Name[i])[,1:13])/rowSums(wh_pop_women[1:39,1:13])*100000)))
  assign(paste0(W_Ca_D_Name[i],"_W_Death_DR_young_case"),((rowSums(get(W_Ca_D_Name[i])[,1:13]))))
  
}

#####################################################################################################################
#####################################################################################################################

# 75
for (i in 1:20){
  assign(paste0(M_Ca_inci_Name[i],"_M_DR_age"),((rowSums(get(M_Ca_inci_Name[i])[,16:18]) /rowSums(wh_pop_men[17:37,16:18])*100000)))
  assign(paste0(M_Ca_inci_Name[i],"_M_DR_age_case"),((rowSums(get(M_Ca_inci_Name[i])[,16:18]))))
  
  assign(paste0(M_Ca_inci_Name[i],"_M_DR_young"),((rowSums(get(M_Ca_inci_Name[i])[,1:15])  /rowSums(wh_pop_men[17:37,1:15])*100000)))
  assign(paste0(M_Ca_inci_Name[i],"_M_DR_young_case"),((rowSums(get(M_Ca_inci_Name[i])[,1:15]))))
  
}
for (i in 1:19){
  assign(paste0(M_Ca_D_Name[i],"_M_Death_DR_age"),((rowSums(get(M_Ca_D_Name[i])[,16:18]) /rowSums(wh_pop_men[1:39,16:18])*100000)))
  assign(paste0(M_Ca_D_Name[i],"_M_Death_DR_age_case"),((rowSums(get(M_Ca_D_Name[i])[,16:18]) )))
  
  assign(paste0(M_Ca_D_Name[i],"_M_Death_DR_young"),((rowSums(get(M_Ca_D_Name[i])[,1:15])/rowSums(wh_pop_men[1:39,1:15])*100000)))
  assign(paste0(M_Ca_D_Name[i],"_M_Death_DR_young_Case"),((rowSums(get(M_Ca_D_Name[i])[,1:15]))))
  
}
for (i in 1:22){
  assign(paste0(W_Ca_inci_Name[i],"_W_DR_age"),((rowSums(get(W_Ca_inci_Name[i])[,16:18]) /rowSums(wh_pop_women[17:37,16:18])*100000)))
  assign(paste0(W_Ca_inci_Name[i],"_W_DR_age_case"),((rowSums(get(W_Ca_inci_Name[i])[,16:18]) )))
  
  assign(paste0(W_Ca_inci_Name[i],"_W_DR_young"),((rowSums(get(W_Ca_inci_Name[i])[,1:15])/rowSums(wh_pop_women[17:37,1:15])*100000)))
  assign(paste0(W_Ca_inci_Name[i],"_W_DR_young_case"),((rowSums(get(W_Ca_inci_Name[i])[,1:15]))))
  
}
for (i in 1:21){
  assign(paste0(W_Ca_D_Name[i],"_W_Death_DR_age"),((rowSums(get(W_Ca_D_Name[i])[,16:18]) /rowSums(wh_pop_women[1:39,16:18])*100000)))
  assign(paste0(W_Ca_D_Name[i],"_W_Death_DR_age_case"),((rowSums(get(W_Ca_D_Name[i])[,16:18]))))
  
  assign(paste0(W_Ca_D_Name[i],"_W_Death_DR_young"),((rowSums(get(W_Ca_D_Name[i])[,1:15])/rowSums(wh_pop_women[1:39,1:15])*100000)))
  assign(paste0(W_Ca_D_Name[i],"_W_Death_DR_young_case"),((rowSums(get(W_Ca_D_Name[i])[,1:15]))))
  
}

#####################################################################################################################
#####################################################################################################################


M_inci_Name <-c("C00_C96","C00_C14",'C15','C16','C18_C21','C23_C24',
                'C25','C32','C33_C34','C61','C60_C63',
                'C64','C67','C70_C72','C73','C81','C82_C86_C96','C90','C91_C95','etc(C00_C96)')

W_inci_Name <-c("C00_C96","C00_C14",'C15','C16','C18_C21','C23_C24','
                C25','C32','C33_C34','C50','C53','C54_C55','C56','C64',
                'C67','C70_C72','C73','C81','C82_C86_C96','C90','C91_C95','etc(C00_C96)')

M_D_Name    <-c("C00_C97","C00_C14",'C15','C16','C18_C21','C23_C24',
                'C25','C32','C33_C34','C61','C60_C63',
                'C64','C67','C70_C72','C73','C82_C86','C90','C91_C95','etc(C00_C97)')

W_D_Name    <-c("C00_C97","C00_C14",'C15','C16','C18_C21','C23_C24',
                'C25','C32','C33_C34','C50','C53','C54_C55','C56',
                'C64','C67','C70_C72','C73','C82_C86','C90','C91_C95','etc(C00_C97)')



MaleIncDR_age<-as.data.frame(cbind(get(paste0(M_Ca_inci_Name[1],"_M_DR_age")),get(paste0(M_Ca_inci_Name[2],"_M_DR_age")),get(paste0(M_Ca_inci_Name[3],"_M_DR_age")),get(paste0(M_Ca_inci_Name[4],"_M_DR_age")),
                               get(paste0(M_Ca_inci_Name[5],"_M_DR_age")),get(paste0(M_Ca_inci_Name[5],"_M_DR_age")),get(paste0(M_Ca_inci_Name[7],"_M_DR_age")),get(paste0(M_Ca_inci_Name[8],"_M_DR_age")),
                               get(paste0(M_Ca_inci_Name[9],"_M_DR_age")),get(paste0(M_Ca_inci_Name[10],"_M_DR_age")),get(paste0(M_Ca_inci_Name[11],"_M_DR_age")),get(paste0(M_Ca_inci_Name[12],"_M_DR_age")),
                               get(paste0(M_Ca_inci_Name[13],"_M_DR_age")),get(paste0(M_Ca_inci_Name[14],"_M_DR_age")),get(paste0(M_Ca_inci_Name[15],"_M_DR_age")),get(paste0(M_Ca_inci_Name[16],"_M_DR_age")),
                               get(paste0(M_Ca_inci_Name[17],"_M_DR_age")),get(paste0(M_Ca_inci_Name[18],"_M_DR_age")),get(paste0(M_Ca_inci_Name[19],"_M_DR_age")),get(paste0(M_Ca_inci_Name[20],"_M_DR_age"))))

MaleIncDR_age_case<-as.data.frame(cbind(get(paste0(M_Ca_inci_Name[1],"_M_DR_age_case")),get(paste0(M_Ca_inci_Name[2],"_M_DR_age_case")),get(paste0(M_Ca_inci_Name[3],"_M_DR_age_case")),get(paste0(M_Ca_inci_Name[4],"_M_DR_age_case")),
                                   get(paste0(M_Ca_inci_Name[5],"_M_DR_age_case")),get(paste0(M_Ca_inci_Name[5],"_M_DR_age_case")),get(paste0(M_Ca_inci_Name[7],"_M_DR_age_case")),get(paste0(M_Ca_inci_Name[8],"_M_DR_age_case")),
                                   get(paste0(M_Ca_inci_Name[9],"_M_DR_age_case")),get(paste0(M_Ca_inci_Name[10],"_M_DR_age_case")),get(paste0(M_Ca_inci_Name[11],"_M_DR_age_case")),get(paste0(M_Ca_inci_Name[12],"_M_DR_age_case")),
                                   get(paste0(M_Ca_inci_Name[13],"_M_DR_age_case")),get(paste0(M_Ca_inci_Name[14],"_M_DR_age_case")),get(paste0(M_Ca_inci_Name[15],"_M_DR_age_case")),get(paste0(M_Ca_inci_Name[16],"_M_DR_age_case")),
                                   get(paste0(M_Ca_inci_Name[17],"_M_DR_age_case")),get(paste0(M_Ca_inci_Name[18],"_M_DR_age_case")),get(paste0(M_Ca_inci_Name[19],"_M_DR_age_case")),get(paste0(M_Ca_inci_Name[20],"_M_DR_age_case"))))


MaleIncDR_young<-as.data.frame(cbind(get(paste0(M_Ca_inci_Name[1],"_M_DR_young")),get(paste0(M_Ca_inci_Name[2],"_M_DR_young")),get(paste0(M_Ca_inci_Name[3],"_M_DR_young")),get(paste0(M_Ca_inci_Name[4],"_M_DR_young")),
                               get(paste0(M_Ca_inci_Name[5],"_M_DR_young")),get(paste0(M_Ca_inci_Name[5],"_M_DR_young")),get(paste0(M_Ca_inci_Name[7],"_M_DR_young")),get(paste0(M_Ca_inci_Name[8],"_M_DR_young")),
                               get(paste0(M_Ca_inci_Name[9],"_M_DR_young")),get(paste0(M_Ca_inci_Name[10],"_M_DR_young")),get(paste0(M_Ca_inci_Name[11],"_M_DR_young")),get(paste0(M_Ca_inci_Name[12],"_M_DR_young")),
                               get(paste0(M_Ca_inci_Name[13],"_M_DR_young")),get(paste0(M_Ca_inci_Name[14],"_M_DR_young")),get(paste0(M_Ca_inci_Name[15],"_M_DR_young")),get(paste0(M_Ca_inci_Name[16],"_M_DR_young")),
                               get(paste0(M_Ca_inci_Name[17],"_M_DR_young")),get(paste0(M_Ca_inci_Name[18],"_M_DR_young")),get(paste0(M_Ca_inci_Name[19],"_M_DR_young")),get(paste0(M_Ca_inci_Name[20],"_M_DR_young"))))

MaleIncDR_young_case<-as.data.frame(cbind(get(paste0(M_Ca_inci_Name[1],"_M_DR_young_case")),get(paste0(M_Ca_inci_Name[2],"_M_DR_young_case")),get(paste0(M_Ca_inci_Name[3],"_M_DR_young_case")),get(paste0(M_Ca_inci_Name[4],"_M_DR_young_case")),
                                     get(paste0(M_Ca_inci_Name[5],"_M_DR_young_case")),get(paste0(M_Ca_inci_Name[5],"_M_DR_young_case")),get(paste0(M_Ca_inci_Name[7],"_M_DR_young_case")),get(paste0(M_Ca_inci_Name[8],"_M_DR_young_case")),
                                     get(paste0(M_Ca_inci_Name[9],"_M_DR_young_case")),get(paste0(M_Ca_inci_Name[10],"_M_DR_young_case")),get(paste0(M_Ca_inci_Name[11],"_M_DR_young_case")),get(paste0(M_Ca_inci_Name[12],"_M_DR_young_case")),
                                     get(paste0(M_Ca_inci_Name[13],"_M_DR_young_case")),get(paste0(M_Ca_inci_Name[14],"_M_DR_young_case")),get(paste0(M_Ca_inci_Name[15],"_M_DR_young_case")),get(paste0(M_Ca_inci_Name[16],"_M_DR_young_case")),
                                     get(paste0(M_Ca_inci_Name[17],"_M_DR_young_case")),get(paste0(M_Ca_inci_Name[18],"_M_DR_young_case")),get(paste0(M_Ca_inci_Name[19],"_M_DR_young_case")),get(paste0(M_Ca_inci_Name[20],"_M_DR_young_case"))))


MaleDeDR_age<-as.data.frame(cbind(get(paste0(M_Ca_D_Name[1],"_M_Death_DR_age")),get(paste0(M_Ca_D_Name[2],"_M_Death_DR_age")),get(paste0(M_Ca_D_Name[3],"_M_Death_DR_age")),get(paste0(M_Ca_D_Name[4],"_M_Death_DR_age")),
                              get(paste0(M_Ca_D_Name[5],"_M_Death_DR_age")),get(paste0(M_Ca_D_Name[5],"_M_Death_DR_age")),get(paste0(M_Ca_D_Name[7],"_M_Death_DR_age")),get(paste0(M_Ca_D_Name[8],"_M_Death_DR_age")),
                              get(paste0(M_Ca_D_Name[9],"_M_Death_DR_age")),get(paste0(M_Ca_D_Name[10],"_M_Death_DR_age")),get(paste0(M_Ca_D_Name[11],"_M_Death_DR_age")),get(paste0(M_Ca_D_Name[12],"_M_Death_DR_age")),
                              get(paste0(M_Ca_D_Name[13],"_M_Death_DR_age")),get(paste0(M_Ca_D_Name[14],"_M_Death_DR_age")),get(paste0(M_Ca_D_Name[15],"_M_Death_DR_age")),get(paste0(M_Ca_D_Name[16],"_M_Death_DR_age")),
                              get(paste0(M_Ca_D_Name[17],"_M_Death_DR_age")),get(paste0(M_Ca_D_Name[18],"_M_Death_DR_age")),get(paste0(M_Ca_D_Name[19],"_M_Death_DR_age"))))


MaleDeDR_age_case<-as.data.frame(cbind(get(paste0(M_Ca_D_Name[1],"_M_Death_DR_age_case")),get(paste0(M_Ca_D_Name[2],"_M_Death_DR_age_case")),get(paste0(M_Ca_D_Name[3],"_M_Death_DR_age_case")),get(paste0(M_Ca_D_Name[4],"_M_Death_DR_age_case")),
                                  get(paste0(M_Ca_D_Name[5],"_M_Death_DR_age_case")),get(paste0(M_Ca_D_Name[5],"_M_Death_DR_age_case")),get(paste0(M_Ca_D_Name[7],"_M_Death_DR_age_case")),get(paste0(M_Ca_D_Name[8],"_M_Death_DR_age_case")),
                                  get(paste0(M_Ca_D_Name[9],"_M_Death_DR_age_case")),get(paste0(M_Ca_D_Name[10],"_M_Death_DR_age_case")),get(paste0(M_Ca_D_Name[11],"_M_Death_DR_age_case")),get(paste0(M_Ca_D_Name[12],"_M_Death_DR_age_case")),
                                  get(paste0(M_Ca_D_Name[13],"_M_Death_DR_age_case")),get(paste0(M_Ca_D_Name[14],"_M_Death_DR_age_case")),get(paste0(M_Ca_D_Name[15],"_M_Death_DR_age_case")),get(paste0(M_Ca_D_Name[16],"_M_Death_DR_age_case")),
                                  get(paste0(M_Ca_D_Name[17],"_M_Death_DR_age_case")),get(paste0(M_Ca_D_Name[18],"_M_Death_DR_age_case")),get(paste0(M_Ca_D_Name[19],"_M_Death_DR_age_case"))))


MaleDeDR_young<-as.data.frame(cbind(get(paste0(M_Ca_D_Name[1],"_M_Death_DR_young")),get(paste0(M_Ca_D_Name[2],"_M_Death_DR_young")),get(paste0(M_Ca_D_Name[3],"_M_Death_DR_young")),get(paste0(M_Ca_D_Name[4],"_M_Death_DR_young")),
                              get(paste0(M_Ca_D_Name[5],"_M_Death_DR_young")),get(paste0(M_Ca_D_Name[5],"_M_Death_DR_young")),get(paste0(M_Ca_D_Name[7],"_M_Death_DR_young")),get(paste0(M_Ca_D_Name[8],"_M_Death_DR_young")),
                              get(paste0(M_Ca_D_Name[9],"_M_Death_DR_young")),get(paste0(M_Ca_D_Name[10],"_M_Death_DR_young")),get(paste0(M_Ca_D_Name[11],"_M_Death_DR_young")),get(paste0(M_Ca_D_Name[12],"_M_Death_DR_young")),
                              get(paste0(M_Ca_D_Name[13],"_M_Death_DR_young")),get(paste0(M_Ca_D_Name[14],"_M_Death_DR_young")),get(paste0(M_Ca_D_Name[15],"_M_Death_DR_young")),get(paste0(M_Ca_D_Name[16],"_M_Death_DR_young")),
                              get(paste0(M_Ca_D_Name[17],"_M_Death_DR_young")),get(paste0(M_Ca_D_Name[18],"_M_Death_DR_young")),get(paste0(M_Ca_D_Name[19],"_M_Death_DR_young"))))

MaleDeDR_young_case<-as.data.frame(cbind(get(paste0(M_Ca_D_Name[1],"_M_Death_DR_young_case")),get(paste0(M_Ca_D_Name[2],"_M_Death_DR_young_case")),get(paste0(M_Ca_D_Name[3],"_M_Death_DR_young_case")),get(paste0(M_Ca_D_Name[4],"_M_Death_DR_young_case")),
                                    get(paste0(M_Ca_D_Name[5],"_M_Death_DR_young_case")),get(paste0(M_Ca_D_Name[5],"_M_Death_DR_young_case")),get(paste0(M_Ca_D_Name[7],"_M_Death_DR_young_case")),get(paste0(M_Ca_D_Name[8],"_M_Death_DR_young_case")),
                                    get(paste0(M_Ca_D_Name[9],"_M_Death_DR_young_case")),get(paste0(M_Ca_D_Name[10],"_M_Death_DR_young_case")),get(paste0(M_Ca_D_Name[11],"_M_Death_DR_young_case")),get(paste0(M_Ca_D_Name[12],"_M_Death_DR_young_case")),
                                    get(paste0(M_Ca_D_Name[13],"_M_Death_DR_young_case")),get(paste0(M_Ca_D_Name[14],"_M_Death_DR_young_case")),get(paste0(M_Ca_D_Name[15],"_M_Death_DR_young_case")),get(paste0(M_Ca_D_Name[16],"_M_Death_DR_young_case")),
                                    get(paste0(M_Ca_D_Name[17],"_M_Death_DR_young_case")),get(paste0(M_Ca_D_Name[18],"_M_Death_DR_young_case")),get(paste0(M_Ca_D_Name[19],"_M_Death_DR_young_case"))))


colnames(MaleIncDR_age) <- M_inci_Name
colnames(MaleIncDR_age_case) <- M_inci_Name
colnames(MaleIncDR_young) <- M_inci_Name
colnames(MaleIncDR_young_case) <- M_inci_Name

colnames(MaleDeDR_age)  <- M_D_Name
colnames(MaleDeDR_age_case)  <- M_D_Name
colnames(MaleDeDR_young)  <- M_D_Name
colnames(MaleDeDR_young_case)  <- M_D_Name

MaleIncDR_age<-round(MaleIncDR_age,1)
MaleIncDR_age_case<-round(MaleIncDR_age_case,1)
MaleIncDR_young<-round(MaleIncDR_young,1)
MaleIncDR_young_case<-round(MaleIncDR_young_case,1)

MaleDeDR_age<-round(MaleDeDR_age,1)
MaleDeDR_age_case<-round(MaleDeDR_age_case,1)
MaleDeDR_young<-round(MaleDeDR_young,1)
MaleDeDR_young_case<-round(MaleDeDR_young_case,1)

MaleIncDR_age<-cbind(Ini_Year,MaleIncDR_age)
MaleIncDR_age_case<-cbind(Ini_Year,MaleIncDR_age_case)
MaleIncDR_young<-cbind(Ini_Year,MaleIncDR_young)
MaleIncDR_young_case<-cbind(Ini_Year,MaleIncDR_young_case)

MaleDeDR_age<-cbind(De_Year,MaleDeDR_age)
MaleDeDR_age_case<-cbind(De_Year,MaleDeDR_age_case)
MaleDeDR_young<-cbind(De_Year,MaleDeDR_young)
MaleDeDR_young_case<-cbind(De_Year,MaleDeDR_young_case)


MaleIncDR_age<-melt(data = MaleIncDR_age, id.vars = c("Ini_Year"))
MaleIncDR_age_case<-melt(data = MaleIncDR_age_case, id.vars = c("Ini_Year"))

MaleIncDR_young<-melt(data = MaleIncDR_young, id.vars = c("Ini_Year"))
MaleIncDR_young_case<-melt(data = MaleIncDR_young_case, id.vars = c("Ini_Year"))

MaleDeDR_age <-melt(data = MaleDeDR_age, id.vars = c("De_Year"))
MaleDeDR_age_case <-melt(data = MaleDeDR_age_case, id.vars = c("De_Year"))

MaleDeDR_young <-melt(data = MaleDeDR_young, id.vars = c("De_Year"))
MaleDeDR_young_case <-melt(data = MaleDeDR_young_case, id.vars = c("De_Year"))


colnames(MaleIncDR_age) <-c("Year","Cancer","IR")
colnames(MaleIncDR_age_case) <-c("Year","Cancer","Case")
colnames(MaleIncDR_young) <-c("Year","Cancer","IR")
colnames(MaleIncDR_young_case) <-c("Year","Cancer","Case")

colnames(MaleDeDR_age)  <-c("Year","Cancer","MR")
colnames(MaleDeDR_age_case)  <-c("Year","Cancer","Case")
colnames(MaleDeDR_young)  <-c("Year","Cancer","MR")
colnames(MaleDeDR_young_case)  <-c("Year","Cancer","Case")

## Heatmap
ggplot(MaleIncDR_age[-seq(1,21),], aes(x=Cancer, y=Year, fill=IR)) +
  geom_tile(colour="black", linetype="dashed") +
  scale_fill_gradient2(low="darkred",high="darkgreen") +
  geom_text(aes(label=IR),color='black')

ggplot(MaleIncDR_young[-seq(1,21),], aes(x=Cancer, y=Year, fill=IR)) +
  geom_tile(colour="black", linetype="dashed") +
  scale_fill_gradient2(low="darkred",high="darkgreen") +
  geom_text(aes(label=IR),color='black')

ggplot(MaleDeDR_age[-seq(1,39),], aes(x=Cancer, y=Year, fill=MR)) +
  geom_tile(colour="black", linetype="dashed") +
  scale_fill_gradient2(low="darkred",high="darkgreen") +
  geom_text(aes(label=MR),color='black')

ggplot(MaleDeDR_young[-seq(1,39),], aes(x=Cancer, y=Year, fill=MR)) +
  geom_tile(colour="black", linetype="dashed") +
  scale_fill_gradient2(low="darkred",high="darkgreen") +
  geom_text(aes(label=MR),color='black')



################################################################################################

FemaleIncDR_age<-as.data.frame(cbind(get(paste0(W_Ca_inci_Name[1],"_W_DR_age")),get(paste0(W_Ca_inci_Name[2],"_W_DR_age")),get(paste0(W_Ca_inci_Name[3],"_W_DR_age")),get(paste0(W_Ca_inci_Name[4],"_W_DR_age")),
                                 get(paste0(W_Ca_inci_Name[5],"_W_DR_age")),get(paste0(W_Ca_inci_Name[5],"_W_DR_age")),get(paste0(W_Ca_inci_Name[7],"_W_DR_age")),get(paste0(W_Ca_inci_Name[8],"_W_DR_age")),
                                 get(paste0(W_Ca_inci_Name[9],"_W_DR_age")),get(paste0(W_Ca_inci_Name[10],"_W_DR_age")),get(paste0(W_Ca_inci_Name[11],"_W_DR_age")),get(paste0(W_Ca_inci_Name[12],"_W_DR_age")),
                                 get(paste0(W_Ca_inci_Name[13],"_W_DR_age")),get(paste0(W_Ca_inci_Name[14],"_W_DR_age")),get(paste0(W_Ca_inci_Name[15],"_W_DR_age")),get(paste0(W_Ca_inci_Name[16],"_W_DR_age")),
                                 get(paste0(W_Ca_inci_Name[17],"_W_DR_age")),get(paste0(W_Ca_inci_Name[18],"_W_DR_age")),get(paste0(W_Ca_inci_Name[19],"_W_DR_age")),get(paste0(W_Ca_inci_Name[20],"_W_DR_age")),
                                 get(paste0(W_Ca_inci_Name[21],"_W_DR_age")),get(paste0(W_Ca_inci_Name[22],"_W_DR_age"))))

FemaleIncDR_young<-as.data.frame(cbind(get(paste0(W_Ca_inci_Name[1],"_W_DR_young")),get(paste0(W_Ca_inci_Name[2],"_W_DR_young")),get(paste0(W_Ca_inci_Name[3],"_W_DR_young")),get(paste0(W_Ca_inci_Name[4],"_W_DR_young")),
                                 get(paste0(W_Ca_inci_Name[5],"_W_DR_young")),get(paste0(W_Ca_inci_Name[5],"_W_DR_young")),get(paste0(W_Ca_inci_Name[7],"_W_DR_young")),get(paste0(W_Ca_inci_Name[8],"_W_DR_young")),
                                 get(paste0(W_Ca_inci_Name[9],"_W_DR_young")),get(paste0(W_Ca_inci_Name[10],"_W_DR_young")),get(paste0(W_Ca_inci_Name[11],"_W_DR_young")),get(paste0(W_Ca_inci_Name[12],"_W_DR_young")),
                                 get(paste0(W_Ca_inci_Name[13],"_W_DR_young")),get(paste0(W_Ca_inci_Name[14],"_W_DR_young")),get(paste0(W_Ca_inci_Name[15],"_W_DR_young")),get(paste0(W_Ca_inci_Name[16],"_W_DR_young")),
                                 get(paste0(W_Ca_inci_Name[17],"_W_DR_young")),get(paste0(W_Ca_inci_Name[18],"_W_DR_young")),get(paste0(W_Ca_inci_Name[19],"_W_DR_young")),get(paste0(W_Ca_inci_Name[20],"_W_DR_young")),
                                 get(paste0(W_Ca_inci_Name[21],"_W_DR_young")),get(paste0(W_Ca_inci_Name[22],"_W_DR_young"))))


FemaleDeDR_age<-as.data.frame(cbind(get(paste0(W_Ca_D_Name[1],"_W_Death_DR_age")),get(paste0(W_Ca_D_Name[2],"_W_Death_DR_age")),get(paste0(W_Ca_D_Name[3],"_W_Death_DR_age")),get(paste0(W_Ca_D_Name[4],"_W_Death_DR_age")),
                                get(paste0(W_Ca_D_Name[5],"_W_Death_DR_age")),get(paste0(W_Ca_D_Name[5],"_W_Death_DR_age")),get(paste0(W_Ca_D_Name[7],"_W_Death_DR_age")),get(paste0(W_Ca_D_Name[8],"_W_Death_DR_age")),
                                get(paste0(W_Ca_D_Name[9],"_W_Death_DR_age")),get(paste0(W_Ca_D_Name[10],"_W_Death_DR_age")),get(paste0(W_Ca_D_Name[11],"_W_Death_DR_age")),get(paste0(W_Ca_D_Name[12],"_W_Death_DR_age")),
                                get(paste0(W_Ca_D_Name[13],"_W_Death_DR_age")),get(paste0(W_Ca_D_Name[14],"_W_Death_DR_age")),get(paste0(W_Ca_D_Name[15],"_W_Death_DR_age")),get(paste0(W_Ca_D_Name[16],"_W_Death_DR_age")),
                                get(paste0(W_Ca_D_Name[17],"_W_Death_DR_age")),get(paste0(W_Ca_D_Name[18],"_W_Death_DR_age")),get(paste0(W_Ca_D_Name[19],"_W_Death_DR_age")),get(paste0(W_Ca_D_Name[20],"_W_Death_DR_age")),
                                get(paste0(W_Ca_D_Name[21],"_W_Death_DR_age"))))

FemaleDeDR_young<-as.data.frame(cbind(get(paste0(W_Ca_D_Name[1],"_W_Death_DR_young")),get(paste0(W_Ca_D_Name[2],"_W_Death_DR_young")),get(paste0(W_Ca_D_Name[3],"_W_Death_DR_young")),get(paste0(W_Ca_D_Name[4],"_W_Death_DR_young")),
                                get(paste0(W_Ca_D_Name[5],"_W_Death_DR_young")),get(paste0(W_Ca_D_Name[5],"_W_Death_DR_young")),get(paste0(W_Ca_D_Name[7],"_W_Death_DR_young")),get(paste0(W_Ca_D_Name[8],"_W_Death_DR_young")),
                                get(paste0(W_Ca_D_Name[9],"_W_Death_DR_young")),get(paste0(W_Ca_D_Name[10],"_W_Death_DR_young")),get(paste0(W_Ca_D_Name[11],"_W_Death_DR_young")),get(paste0(W_Ca_D_Name[12],"_W_Death_DR_young")),
                                get(paste0(W_Ca_D_Name[13],"_W_Death_DR_young")),get(paste0(W_Ca_D_Name[14],"_W_Death_DR_young")),get(paste0(W_Ca_D_Name[15],"_W_Death_DR_young")),get(paste0(W_Ca_D_Name[16],"_W_Death_DR_young")),
                                get(paste0(W_Ca_D_Name[17],"_W_Death_DR_young")),get(paste0(W_Ca_D_Name[18],"_W_Death_DR_young")),get(paste0(W_Ca_D_Name[19],"_W_Death_DR_young")),get(paste0(W_Ca_D_Name[20],"_W_Death_DR_young")),
                                get(paste0(W_Ca_D_Name[21],"_W_Death_DR_young"))))

colnames(FemaleIncDR_age) <- W_inci_Name
colnames(FemaleIncDR_young) <- W_inci_Name

colnames(FemaleDeDR_age)  <- W_D_Name
colnames(FemaleDeDR_young)  <- W_D_Name

FemaleIncDR_age<-round(FemaleIncDR_age,1)
FemaleIncDR_young<-round(FemaleIncDR_young,1)

FemaleDeDR_age<-round(FemaleDeDR_age,1)
FemaleDeDR_young<-round(FemaleDeDR_young,1)


FemaleIncDR_age<-cbind(Ini_Year,FemaleIncDR_age)
FemaleIncDR_young<-cbind(Ini_Year,FemaleIncDR_young)

FemaleDeDR_age<-cbind(De_Year,FemaleDeDR_age)
FemaleDeDR_young<-cbind(De_Year,FemaleDeDR_age)


FemaleIncDR_age<-melt(data = FemaleIncDR_age, id.vars = c("Ini_Year"))
FemaleIncDR_young<-melt(data = FemaleIncDR_young, id.vars = c("Ini_Year"))

FemaleDeDR_age <-melt(data = FemaleDeDR_age, id.vars = c("De_Year"))
FemaleDeDR_young <-melt(data = FemaleDeDR_young, id.vars = c("De_Year"))

colnames(FemaleIncDR_age) <-c("Year","Cancer","IR")
colnames(FemaleIncDR_young) <-c("Year","Cancer","IR")

colnames(FemaleDeDR_age)  <-c("Year","Cancer","MR")
colnames(FemaleDeDR_young)  <-c("Year","Cancer","MR")



## Heatmap DR

ggplot(FemaleIncDR_age[-seq(1,21),], aes(x=Cancer, y=Year, fill=IR)) +
  geom_tile(colour="black", linetype="dashed") +
  scale_fill_gradient2(low="darkred",high="darkgreen") +
  geom_text(aes(label=IR),color='black')

ggplot(FemaleIncDR_young[-seq(1,21),], aes(x=Cancer, y=Year, fill=IR)) +
  geom_tile(colour="black", linetype="dashed") +
  scale_fill_gradient2(low="darkred",high="darkgreen") +
  geom_text(aes(label=IR),color='black')

ggplot(FemaleDeDR_age[-seq(1,39),], aes(x=Cancer, y=Year, fill=MR)) +
  geom_tile(colour="black", linetype="dashed") +
  scale_fill_gradient2(low="darkred",high="darkgreen") +
  geom_text(aes(label=MR),color='black')

ggplot(FemaleDeDR_young[-seq(1,39),], aes(x=Cancer, y=Year, fill=MR)) +
  geom_tile(colour="black", linetype="dashed") +
  scale_fill_gradient2(low="darkred",high="darkgreen") +
  geom_text(aes(label=MR),color='black')


