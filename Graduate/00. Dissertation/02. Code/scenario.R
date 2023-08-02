## NORDPRED BASED --? SCENEARIO


library(openxlsx)
library(dplyr)

load(file="/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/rawdata/workspace.RData")
source("/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/code/canproj-2022_HJ.R")
agegroups <- c("00-04","05-09","10-14","15-19","20-24","25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84","85+" )

inci_cname_M <- inci_cname[inci_cname %in% c("C16_I","C18_C20_I","C22_I","C23_C24_I","C25_I","C33_C34_I","C61_I","C64_I","C67_I")]
inci_cname_F <- inci_cname[inci_cname %in% c("C16_I","C18_C20_I","C22_I","C23_C24_I","C25_I","C33_C34_I")]

mort_cname_M <- mort_cname[mort_cname %in% c("C15_D","C16_D","C18_C21_D","C22_D","C25_D","C33_C34_D","C61_D","C82_C86_D","C91_C95_D")]
mort_cname_F <- mort_cname[mort_cname %in% c("C16_D","C18_C21_D","C22_D","C25_D","C33_C34_D","C53_D","C82_C86_D","C91_C95_D")]

title_nameM_I<-c("Stomach (C16)","Colorectal (C18-C20)","Liver (C22)","Gallbladder (C23-C24)",
                 "Pancreas (C25)","Lung (C33-C34)","Prostate (C61)","Kidney (C64)","Bladder (C67)")

title_nameF_I<-c("Stomach (C16)","Colorectal (C18-C20)","Liver (C22)","Gallbladder (C23-C24)",
                 "Pancreas (C25)","Lung (C33-C34)")

title_nameM_D<-c("Esophagus (C15)","Stomach (C16)","Colorectal & Anus (C18-C21)","Liver (C22)",
                 "Pancreas (C25)","Lung (C33-C34)","Prostate (C61)","Non-Hodgkin lymphoma (C82-C86)","Leukemia (C91-C95)")

title_nameF_D<-c("Stomach (C16)","Colorectal & Anus (C18-C21)","Liver (C22)",
                 "Pancreas (C25)","Lung (C33-C34)","Cervix uteri (C53)","Non-Hodgkin lymphoma (C82-C86)","Leukemia (C91-C95)")


###########

NORDPRED_func <- function (Scenarios1_male,Scenarios1_female,comparison){
  
  if (comparison=="first"){
    
    NORDPRED_INCI_M <-c()
    NORDPRED_INCI_F <-c()
    
    for (i in 1: length(inci_cname_M)){
      male_case <-subset(get(inci_cname_M[i]),Sex=="남자")[,-c(1:3,24)]
      male_pop  <- Scenarios1_male[, c(as.character(2000:2040))]
      assign(paste0(inci_cname_M[i],"_M"),colSums(canproj(male_case,male_pop,startp=2020,startestage=7, methods="nordpred")$agsproj*(male_pop)/100000))
      NORDPRED_INCI_M <- cbind(NORDPRED_INCI_M,get(paste0(inci_cname_M[i],"_M")))
    }
    colnames(NORDPRED_INCI_M) <- inci_cname_M
    
    for (i in 1: length(inci_cname_F)){
      female_case <-subset(get(inci_cname_F[i]),Sex=="여자")[,-c(1:3,24)]
      female_pop  <- Scenarios1_female[, c(as.character(2000:2040))]
      assign(paste0(inci_cname_F[i],"_F"),colSums(canproj(female_case,female_pop,startp=2020,startestage=7, methods="nordpred")$agsproj*(female_pop)/100000))
      NORDPRED_INCI_F <- cbind(NORDPRED_INCI_F,get(paste0(inci_cname_F[i],"_F")))
    }
    colnames(NORDPRED_INCI_F) <- inci_cname_F
    
    NORDPRED_MORT_M <-c()
    NORDPRED_MORT_F <-c()
    
    for ( i in 1: length(mort_cname_M)){
      male_case <- subset(get(mort_cname_M[i]),Sex=="남자")[,-c(1:3,24,25)]
      male_pop  <- Scenarios1_male[, c(as.character(2000:2040))]
      if(length(which(rowSums(male_case/20) <1)[length(which(rowSums(male_case/20) <1))]) == 0){
        assign(paste0(mort_cname_M[i],"_M"),colSums(canproj(male_case,male_pop,startp=2020,startestage=7, methods="nordpred")$agsproj*(male_pop)/100000))
      } else if (length(which(rowSums(male_case/20) <1)[length(which(rowSums(male_case/20) <1))]) !=0) {
        if (which(rowSums(male_case/20) <1)[length(which(rowSums(male_case/20) <1))] >7){  
          num = as.numeric(which(rowSums(male_case/20) <1)[length(which(rowSums(male_case /20) <1))])+1
          assign(paste0(mort_cname_M[i],"_M"),colSums(canproj(male_case,male_pop,startp=2020,startestage=num, methods="nordpred")$agsproj*(male_pop)/100000))
        } else {
          assign(paste0(mort_cname_M[i],"_M"),colSums(canproj(male_case,male_pop,startp=2020,startestage=7, methods="nordpred")$agsproj*(male_pop)/100000))
        }
      }
      NORDPRED_MORT_M <- cbind(NORDPRED_MORT_M,get(paste0(mort_cname_M[i],"_M")))
    }
    colnames(NORDPRED_MORT_M) <- mort_cname_M
    
    for ( i in 1: length(mort_cname_F)){
      female_case <- subset(get(mort_cname_F[i]),Sex=="여자")[,-c(1:3,24,25)]
      female_pop  <- Scenarios1_female[, c(as.character(2000:2040))]
      if(length(which(rowSums(female_case/20) <1)[length(which(rowSums(female_case/20) <1))]) == 0){
        assign(paste0(mort_cname_F[i],"_F"),colSums(canproj(female_case,female_pop,startp=2020,startestage=7, methods="nordpred")$agsproj*(female_pop)/100000))
      } else if (length(which(rowSums(female_case/20) <1)[length(which(rowSums(female_case/20) <1))]) !=0) {
        if (which(rowSums(female_case/20) <1)[length(which(rowSums(female_case/20) <1))] >7){  
          num = as.numeric(which(rowSums(female_case/20) <1)[length(which(rowSums(female_case /20) <1))])+1
          assign(paste0(mort_cname_F[i],"_F"),colSums(canproj(female_case,female_pop,startp=2020,startestage=num, methods="nordpred")$agsproj*(female_pop)/100000))
        } else {
          assign(paste0(mort_cname_F[i],"_F"),colSums(canproj(female_case,female_pop,startp=2020,startestage=7, methods="nordpred")$agsproj*(female_pop)/100000))
        }
      }
      NORDPRED_MORT_F <- cbind(NORDPRED_MORT_F,get(paste0(mort_cname_F[i],"_F")))
      
    }
    colnames(NORDPRED_MORT_F) <- mort_cname_F
    NORDPRED_LIST <-list(NORDPRED_INCI_M,NORDPRED_INCI_F,NORDPRED_MORT_M,NORDPRED_MORT_F)
    return(NORDPRED_LIST)
  } else if (comparison=="second"){
    NORDPRED_INCI_M <-c()
    NORDPRED_INCI_F <-c()
    
    for (i in 1: length(inci_cname_M)){
      male_case <-subset(get(inci_cname_M[i]),Sex=="남자")[,-c(1:4)]
      male_pop  <- Scenarios1_male[, c(as.character(2001:2040))]
      assign(paste0(inci_cname_M[i],"_M"),colSums(canproj(male_case,male_pop,startp=2021,startestage=7, methods="nordpred")$agsproj*(male_pop)/100000))
      NORDPRED_INCI_M <- cbind(NORDPRED_INCI_M,get(paste0(inci_cname_M[i],"_M")))
    }
    colnames(NORDPRED_INCI_M) <- inci_cname_M
    
    for (i in 1: length(inci_cname_F)){
      female_case <-subset(get(inci_cname_F[i]),Sex=="여자")[,-c(1:4)]
      female_pop  <- Scenarios1_female[, c(as.character(2001:2040))]
      assign(paste0(inci_cname_F[i],"_F"),colSums(canproj(female_case,female_pop,startp=2021,startestage=7, methods="nordpred")$agsproj*(female_pop)/100000))
      NORDPRED_INCI_F <- cbind(NORDPRED_INCI_F,get(paste0(inci_cname_F[i],"_F")))
    }
    colnames(NORDPRED_INCI_F) <- inci_cname_F
    
    NORDPRED_MORT_M <-c()
    NORDPRED_MORT_F <-c()
    
    for ( i in 1: length(mort_cname_M)){
      male_case <- subset(get(mort_cname_M[i]),Sex=="남자")[,-c(1:4,25)]
      male_pop  <- Scenarios1_male[, c(as.character(2001:2040))]
      if(length(which(rowSums(male_case/20) <1)[length(which(rowSums(male_case/20) <1))]) == 0){
        assign(paste0(mort_cname_M[i],"_M"),colSums(canproj(male_case,male_pop,startp=2021,startestage=7, methods="nordpred")$agsproj*(male_pop)/100000))
      } else if (length(which(rowSums(male_case/20) <1)[length(which(rowSums(male_case/20) <1))]) !=0) {
        if (which(rowSums(male_case/20) <1)[length(which(rowSums(male_case/20) <1))] >7){  
          num = as.numeric(which(rowSums(male_case/20) <1)[length(which(rowSums(male_case /20) <1))])+1
          assign(paste0(mort_cname_M[i],"_M"),colSums(canproj(male_case,male_pop,startp=2021,startestage=num, methods="nordpred")$agsproj*(male_pop)/100000))
        } else {
          assign(paste0(mort_cname_M[i],"_M"),colSums(canproj(male_case,male_pop,startp=2021,startestage=7, methods="nordpred")$agsproj*(male_pop)/100000))
        }
      }
      NORDPRED_MORT_M <- cbind(NORDPRED_MORT_M,get(paste0(mort_cname_M[i],"_M")))
    }
    colnames(NORDPRED_MORT_M) <- mort_cname_M
    
    for ( i in 1: length(mort_cname_F)){
      female_case <- subset(get(mort_cname_F[i]),Sex=="여자")[,-c(1:4,25)]
      female_pop  <- Scenarios1_female[, c(as.character(2001:2040))]
      if(length(which(rowSums(female_case/20) <1)[length(which(rowSums(female_case/20) <1))]) == 0){
        assign(paste0(mort_cname_F[i],"_F"),colSums(canproj(female_case,female_pop,startp=2021,startestage=7, methods="nordpred")$agsproj*(female_pop)/100000))
      } else if (length(which(rowSums(female_case/20) <1)[length(which(rowSums(female_case/20) <1))]) !=0) {
        if (which(rowSums(female_case/20) <1)[length(which(rowSums(female_case/20) <1))] >7){  
          num = as.numeric(which(rowSums(female_case/20) <1)[length(which(rowSums(female_case /20) <1))])+1
          assign(paste0(mort_cname_F[i],"_F"),colSums(canproj(female_case,female_pop,startp=2021,startestage=num, methods="nordpred")$agsproj*(female_pop)/100000))
        } else {
          assign(paste0(mort_cname_F[i],"_F"),colSums(canproj(female_case,female_pop,startp=2021,startestage=7, methods="nordpred")$agsproj*(female_pop)/100000))
        }
      }
      NORDPRED_MORT_F <- cbind(NORDPRED_MORT_F,get(paste0(mort_cname_F[i],"_F")))
    }
    colnames(NORDPRED_MORT_F) <- mort_cname_F
    NORDPRED_LIST <-list(NORDPRED_INCI_M,NORDPRED_INCI_F,NORDPRED_MORT_M,NORDPRED_MORT_F)
    return(NORDPRED_LIST)
  }else if (comparison=="third"){
    
    NORDPRED_MORT_M <-c()
    NORDPRED_MORT_F <-c()
    
    for ( i in 1: length(mort_cname_M)){
      male_case <- subset(get(mort_cname_M[i]),Sex=="남자")[,-c(1:5)]
      male_pop  <- Scenarios1_male[, c(as.character(2002:2040))]
      if(length(which(rowSums(male_case/20) <1)[length(which(rowSums(male_case/20) <1))]) == 0){
        assign(paste0(mort_cname_M[i],"_M"),colSums(canproj(male_case,male_pop,startp=2022,startestage=7, methods="nordpred")$agsproj*(male_pop)/100000))
      } else if (length(which(rowSums(male_case/20) <1)[length(which(rowSums(male_case/20) <1))]) !=0) {
        if (which(rowSums(male_case/20) <1)[length(which(rowSums(male_case/20) <1))] >7){  
          num = as.numeric(which(rowSums(male_case/20) <1)[length(which(rowSums(male_case /20) <1))])+1
          assign(paste0(mort_cname_M[i],"_M"),colSums(canproj(male_case,male_pop,startp=2022,startestage=num, methods="nordpred")$agsproj*(male_pop)/100000))
        } else {
          assign(paste0(mort_cname_M[i],"_M"),colSums(canproj(male_case,male_pop,startp=2022,startestage=7, methods="nordpred")$agsproj*(male_pop)/100000))
        }
      }
      NORDPRED_MORT_M <- cbind(NORDPRED_MORT_M,get(paste0(mort_cname_M[i],"_M")))
    }
    colnames(NORDPRED_MORT_M) <- mort_cname_M
    
    for ( i in 1: length(mort_cname_F)){
      female_case <- subset(get(mort_cname_F[i]),Sex=="여자")[,-c(1:5)]
      female_pop  <- Scenarios1_female[, c(as.character(2002:2040))]
      if(length(which(rowSums(female_case/20) <1)[length(which(rowSums(female_case/20) <1))]) == 0){
        assign(paste0(mort_cname_F[i],"_F"),colSums(canproj(female_case,female_pop,startp=2022,startestage=7, methods="nordpred")$agsproj*(female_pop)/100000))
      } else if (length(which(rowSums(female_case/20) <1)[length(which(rowSums(female_case/20) <1))]) !=0) {
        if (which(rowSums(female_case/20) <1)[length(which(rowSums(female_case/20) <1))] >7){  
          num = as.numeric(which(rowSums(female_case/20) <1)[length(which(rowSums(female_case /20) <1))])+1
          assign(paste0(mort_cname_F[i],"_F"),colSums(canproj(female_case,female_pop,startp=2022,startestage=num, methods="nordpred")$agsproj*(female_pop)/100000))
        } else {
          assign(paste0(mort_cname_F[i],"_F"),colSums(canproj(female_case,female_pop,startp=2022,startestage=7, methods="nordpred")$agsproj*(female_pop)/100000))
        }
      }
      NORDPRED_MORT_F <- cbind(NORDPRED_MORT_F,get(paste0(mort_cname_F[i],"_F")))
    }
    colnames(NORDPRED_MORT_F) <- mort_cname_F
    NORDPRED_LIST <-list(NORDPRED_MORT_M,NORDPRED_MORT_F)
    return(NORDPRED_LIST)
  }
}

scene1_nd_first  <- NORDPRED_func(Scenarios1_male,Scenarios1_female,"first") # 중위 (fit:2000-2019)
scene2_nd_first  <- NORDPRED_func(Scenarios2_male,Scenarios2_female,"first") # 고위 (fit:2000-2019)
scene3_nd_first  <- NORDPRED_func(Scenarios3_male,Scenarios3_female,"first") # 저위 (fit:2000-2019)
scene4_nd_first  <- NORDPRED_func(Scenarios4_male,Scenarios4_female,"first") # 빠른 고령화 (fit:2000-2019)
scene5_nd_first  <- NORDPRED_func(Scenarios5_male,Scenarios5_female,"first") # 느린 고령화  (fit:2000-2019)

scene1_nd_first_M_inci_Rate<-data.frame(scene1_nd_first[[1]])/colSums(Scenarios1_male[,as.character(2000:2040)])*100000
scene1_nd_first_F_inci_Rate<-data.frame(scene1_nd_first[[2]])/colSums(Scenarios1_female[,as.character(2000:2040)])*100000
scene1_nd_first_M_mort_Rate<-data.frame(scene1_nd_first[[3]])/colSums(Scenarios1_male[,as.character(2000:2040)])*100000
scene1_nd_first_F_mort_Rate<-data.frame(scene1_nd_first[[4]])/colSums(Scenarios1_female[,as.character(2000:2040)])*100000

scene2_nd_first_M_inci_Rate<-data.frame(scene2_nd_first[[1]])/colSums(Scenarios2_male[,as.character(2000:2040)])*100000
scene2_nd_first_F_inci_Rate<-data.frame(scene2_nd_first[[2]])/colSums(Scenarios2_female[,as.character(2000:2040)])*100000
scene2_nd_first_M_mort_Rate<-data.frame(scene2_nd_first[[3]])/colSums(Scenarios2_male[,as.character(2000:2040)])*100000
scene2_nd_first_F_mort_Rate<-data.frame(scene2_nd_first[[4]])/colSums(Scenarios2_female[,as.character(2000:2040)])*100000

scene3_nd_first_M_inci_Rate<-data.frame(scene3_nd_first[[1]])/colSums(Scenarios3_male[,as.character(2000:2040)])*100000
scene3_nd_first_F_inci_Rate<-data.frame(scene3_nd_first[[2]])/colSums(Scenarios3_female[,as.character(2000:2040)])*100000
scene3_nd_first_M_mort_Rate<-data.frame(scene3_nd_first[[3]])/colSums(Scenarios3_male[,as.character(2000:2040)])*100000
scene3_nd_first_F_mort_Rate<-data.frame(scene3_nd_first[[4]])/colSums(Scenarios3_female[,as.character(2000:2040)])*100000

scene4_nd_first_M_inci_Rate<-data.frame(scene4_nd_first[[1]])/colSums(Scenarios4_male[,as.character(2000:2040)])*100000
scene4_nd_first_F_inci_Rate<-data.frame(scene4_nd_first[[2]])/colSums(Scenarios4_female[,as.character(2000:2040)])*100000
scene4_nd_first_M_mort_Rate<-data.frame(scene4_nd_first[[3]])/colSums(Scenarios4_male[,as.character(2000:2040)])*100000
scene4_nd_first_F_mort_Rate<-data.frame(scene4_nd_first[[4]])/colSums(Scenarios4_female[,as.character(2000:2040)])*100000

scene5_nd_first_M_inci_Rate<-data.frame(scene5_nd_first[[1]])/colSums(Scenarios5_male[,as.character(2000:2040)])*100000
scene5_nd_first_F_inci_Rate<-data.frame(scene5_nd_first[[2]])/colSums(Scenarios5_female[,as.character(2000:2040)])*100000
scene5_nd_first_M_mort_Rate<-data.frame(scene5_nd_first[[3]])/colSums(Scenarios5_male[,as.character(2000:2040)])*100000
scene5_nd_first_F_mort_Rate<-data.frame(scene5_nd_first[[4]])/colSums(Scenarios5_female[,as.character(2000:2040)])*100000

scene1_nd_first_M_inci_Rate<-cbind("Year"=c(2000:2040),scene1_nd_first_M_inci_Rate)
scene1_nd_first_F_inci_Rate<-cbind("Year"=c(2000:2040),scene1_nd_first_F_inci_Rate)
scene1_nd_first_M_mort_Rate<-cbind("Year"=c(2000:2040),scene1_nd_first_M_mort_Rate)
scene1_nd_first_F_mort_Rate<-cbind("Year"=c(2000:2040),scene1_nd_first_F_mort_Rate)

scene2_nd_first_M_inci_Rate<-cbind("Year"=c(2000:2040),scene2_nd_first_M_inci_Rate)
scene2_nd_first_F_inci_Rate<-cbind("Year"=c(2000:2040),scene2_nd_first_F_inci_Rate)
scene2_nd_first_M_mort_Rate<-cbind("Year"=c(2000:2040),scene2_nd_first_M_mort_Rate)
scene2_nd_first_F_mort_Rate<-cbind("Year"=c(2000:2040),scene2_nd_first_F_mort_Rate)

scene3_nd_first_M_inci_Rate<-cbind("Year"=c(2000:2040),scene3_nd_first_M_inci_Rate)
scene3_nd_first_F_inci_Rate<-cbind("Year"=c(2000:2040),scene3_nd_first_F_inci_Rate)
scene3_nd_first_M_mort_Rate<-cbind("Year"=c(2000:2040),scene3_nd_first_M_mort_Rate)
scene3_nd_first_F_mort_Rate<-cbind("Year"=c(2000:2040),scene3_nd_first_F_mort_Rate)

scene4_nd_first_M_inci_Rate<-cbind("Year"=c(2000:2040),scene4_nd_first_M_inci_Rate)
scene4_nd_first_F_inci_Rate<-cbind("Year"=c(2000:2040),scene4_nd_first_F_inci_Rate)
scene4_nd_first_M_mort_Rate<-cbind("Year"=c(2000:2040),scene4_nd_first_M_mort_Rate)
scene4_nd_first_F_mort_Rate<-cbind("Year"=c(2000:2040),scene4_nd_first_F_mort_Rate)

scene5_nd_first_M_inci_Rate<-cbind("Year"=c(2000:2040),scene5_nd_first_M_inci_Rate)
scene5_nd_first_F_inci_Rate<-cbind("Year"=c(2000:2040),scene5_nd_first_F_inci_Rate)
scene5_nd_first_M_mort_Rate<-cbind("Year"=c(2000:2040),scene5_nd_first_M_mort_Rate)
scene5_nd_first_F_mort_Rate<-cbind("Year"=c(2000:2040),scene5_nd_first_F_mort_Rate)

first_nd_Rate1<-rbind(
  cbind(melt(scene1_nd_first_M_inci_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Incidence"),"Comparison"=c("Median")),
  cbind(melt(scene1_nd_first_F_inci_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Incidence"),"Comparison"=c("Median")),
  cbind(melt(scene1_nd_first_M_mort_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Mortality"),"Comparison"=c("Median")),
  cbind(melt(scene1_nd_first_F_mort_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Mortality"),"Comparison"=c("Median")))
first_nd_Rate2<-rbind(
  cbind(melt(scene2_nd_first_M_inci_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Incidence"),"Comparison"=c("High")),
  cbind(melt(scene2_nd_first_F_inci_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Incidence"),"Comparison"=c("High")),
  cbind(melt(scene2_nd_first_M_mort_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Mortality"),"Comparison"=c("High")),
  cbind(melt(scene2_nd_first_F_mort_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Mortality"),"Comparison"=c("High")))

first_nd_Rate3<-rbind(
  cbind(melt(scene3_nd_first_M_inci_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Incidence"),"Comparison"=c("Low")),
  cbind(melt(scene3_nd_first_F_inci_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Incidence"),"Comparison"=c("Low")),
  cbind(melt(scene3_nd_first_M_mort_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Mortality"),"Comparison"=c("Low")),
  cbind(melt(scene3_nd_first_F_mort_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Mortality"),"Comparison"=c("Low")))

first_nd_Rate4<-rbind(
  cbind(melt(scene4_nd_first_M_inci_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Incidence"),"Comparison"=c("Fast Ageing")),
  cbind(melt(scene4_nd_first_F_inci_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Incidence"),"Comparison"=c("Fast Ageing")),
  cbind(melt(scene4_nd_first_M_mort_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Mortality"),"Comparison"=c("Fast Ageing")),
  cbind(melt(scene4_nd_first_F_mort_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Mortality"),"Comparison"=c("Fast Ageing")))

first_nd_Rate5<-rbind(
  cbind(melt(scene5_nd_first_M_inci_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Incidence"),"Comparison"=c("Slow Ageing")),
  cbind(melt(scene5_nd_first_F_inci_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Incidence"),"Comparison"=c("Slow Ageing")),
  cbind(melt(scene5_nd_first_M_mort_Rate,id.vars=c("Year")),"Sex"=c("Male"),"Case"=c("Mortality"),"Comparison"=c("Slow Ageing")),
  cbind(melt(scene5_nd_first_F_mort_Rate,id.vars=c("Year")),"Sex"=c("Female"),"Case"=c("Mortality"),"Comparison"=c("Slow Ageing")))


nd_comp_Rate1 <-first_nd_Rate1
colnames(nd_comp_Rate1) <-c("Year","Cancer","Rate","Sex","Type","Comparison")
nd_comp_Rate1$Sex <- factor(nd_comp_Rate1$Sex,levels=c("Male","Female"))

nd_comp_Rate1<-nd_comp_Rate1 %>% mutate(
  Comparison = case_when(
    Comparison = 0019 & Type=="Incidence" & Year <=2019 ~ "Observation",
    Comparison = 0019 & Type=="Mortality" & Year <=2019 ~ "Observation",
    TRUE ~ Comparison
  ))

nd_comp_Rate2 <-first_nd_Rate2
colnames(nd_comp_Rate2) <-c("Year","Cancer","Rate","Sex","Type","Comparison")
nd_comp_Rate2$Sex <- factor(nd_comp_Rate2$Sex,levels=c("Male","Female"))

nd_comp_Rate2<-nd_comp_Rate2 %>% mutate(
  Comparison = case_when(
    Comparison = 0019 & Type=="Incidence" & Year <=2019 ~ "Observation",
    Comparison = 0019 & Type=="Mortality" & Year <=2019 ~ "Observation",
    TRUE ~ Comparison
  ))

nd_comp_Rate3 <-first_nd_Rate3
colnames(nd_comp_Rate3) <-c("Year","Cancer","Rate","Sex","Type","Comparison")
nd_comp_Rate3$Sex <- factor(nd_comp_Rate3$Sex,levels=c("Male","Female"))

nd_comp_Rate3<-nd_comp_Rate3 %>% mutate(
  Comparison = case_when(
    Comparison = 0019 & Type=="Incidence" & Year <=2019 ~ "Observation",
    Comparison = 0019 & Type=="Mortality" & Year <=2019 ~ "Observation",
    TRUE ~ Comparison
  ))

nd_comp_Rate4 <-first_nd_Rate4
colnames(nd_comp_Rate4) <-c("Year","Cancer","Rate","Sex","Type","Comparison")
nd_comp_Rate4$Sex <- factor(nd_comp_Rate4$Sex,levels=c("Male","Female"))

nd_comp_Rate4<-nd_comp_Rate4 %>% mutate(
  Comparison = case_when(
    Comparison = 0019 & Type=="Incidence" & Year <=2019 ~ "Observation",
    Comparison = 0019 & Type=="Mortality" & Year <=2019 ~ "Observation",
    TRUE ~ Comparison
  ))

nd_comp_Rate5 <-first_nd_Rate5
colnames(nd_comp_Rate5) <-c("Year","Cancer","Rate","Sex","Type","Comparison")
nd_comp_Rate5$Sex <- factor(nd_comp_Rate5$Sex,levels=c("Male","Female"))

nd_comp_Rate5<-nd_comp_Rate5 %>% mutate(
  Comparison = case_when(
    Comparison = 0019 & Type=="Incidence" & Year <=2019 ~ "Observation",
    Comparison = 0019 & Type=="Mortality" & Year <=2019 ~ "Observation",
    TRUE ~ Comparison
  ))


model_obs<-subset(nd_comp_Rate1,Comparison=="Observation")

model_obs_pred1<-subset(model_obs,Year==2019);model_obs_pred1$Comparison <-c("Median")
model_obs_pred2<-subset(model_obs,Year==2019);model_obs_pred2$Comparison <-c("High")
model_obs_pred3<-subset(model_obs,Year==2019);model_obs_pred3$Comparison <-c("Low")
model_obs_pred4<-subset(model_obs,Year==2019);model_obs_pred4$Comparison <-c("Fast Ageing")
model_obs_pred5<-subset(model_obs,Year==2019);model_obs_pred5$Comparison <-c("Slow Ageing")

model_np_pred1<-subset(nd_comp_Rate1,Comparison=="Median");model_np_pred1 <-rbind(model_obs_pred1,model_np_pred1)
model_np_pred2<-subset(nd_comp_Rate2,Comparison=="High");model_np_pred2 <-rbind(model_obs_pred2,model_np_pred2)
model_np_pred3<-subset(nd_comp_Rate3,Comparison=="Low");model_np_pred3 <-rbind(model_obs_pred3,model_np_pred3)
model_np_pred4<-subset(nd_comp_Rate4,Comparison=="Fast Ageing");model_np_pred4 <-rbind(model_obs_pred4,model_np_pred4)
model_np_pred5<-subset(nd_comp_Rate5,Comparison=="Slow Ageing");model_np_pred5 <-rbind(model_obs_pred5,model_np_pred5)
mdata<-rbind(model_obs,model_np_pred1,model_np_pred2,model_np_pred3,model_np_pred4,model_np_pred5)
mdata$Comparison <-factor(mdata$Comparison)
mdata$Comparison <- factor(mdata$Comparison,levels=c("Observation","Median","High", "Low", "Fast Ageing","Slow Ageing"))

for ( i in 1:length(inci_cname_M)){
  assign(paste0("plot",i),ggplot(subset(mdata,Type==c("Incidence") & Sex=="Male" &  Cancer==inci_cname_M[i]), aes(x = Year, y = Rate, shape=Comparison, color = Comparison)) +
           geom_line(size = 0.6) +
           geom_point(size = 0.7,fill = "white") +
           scale_fill_viridis(discrete = T, option = "E") +
           scale_color_manual(values = c("black","black", "chartreuse3", "brown3",'cadetblue3',"cornsilk3")) + 
           scale_shape_manual(values = c(15,16,17,18,19,20)) +
           theme_classic() +
           facet_wrap(~ Sex, strip.position = "top", nrow = 1) +
           labs(x = "Year", y = "Incidence Rate (per 100,000)",title=title_nameM_I[i]) +
           theme_ipsum() +
           theme(
             strip.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5,size=15), # 패널 레이블 위치 조정
             axis.title.x = element_text(hjust = 0.5, vjust = 0.5,size=15), # X축 이름 중앙 정렬
             axis.title.y = element_text(hjust = 0.5, vjust = 0.5,size=15), # Y축 이름 중앙 정렬
             plot.margin = margin(1, 1, 1, 1, "cm"), # 그래프 안에 레이블 넣기 위해 여백 조정
             legend.text = element_text(size = 15),legend.title = element_text(size = 15),plot.title = element_text(size = 15, face = "bold")           ))
}

ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot5,plot6,plot7,plot8,labels = c("E","F","G","H"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot9,labels = c("I"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,labels = c("A","B","C","D","E","F","G","H","I"),ncol = 2, nrow = 5, common.legend = TRUE)


for ( i in 1:length(mort_cname_M)){
  assign(paste0("plot",i),ggplot(subset(mdata,Type==c("Mortality") & Sex=="Male" &  Cancer==mort_cname_M[i]), aes(x = Year, y = Rate, shape=Comparison, color = Comparison)) +
           geom_line(size = 0.6) +
           geom_point(size = 0.7,fill = "white") +
           scale_fill_viridis(discrete = T, option = "E") +
           scale_color_manual(values = c("black","black", "chartreuse3", "brown3",'cadetblue3',"cornsilk3")) + 
           scale_shape_manual(values = c(15,16,17,18,19,20)) +
           theme_classic() +
           facet_wrap(~ Sex, strip.position = "top", nrow = 1) +
           labs(x = "Year", y = "Mortality Rate (per 100,000)",title=title_nameM_D[i]) +
           theme_ipsum() +
           theme(
             strip.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5,size=15), # 패널 레이블 위치 조정
             axis.title.x = element_text(hjust = 0.5, vjust = 0.5,size=15), # X축 이름 중앙 정렬
             axis.title.y = element_text(hjust = 0.5, vjust = 0.5,size=15), # Y축 이름 중앙 정렬
             plot.margin = margin(1, 1, 1, 1, "cm"), # 그래프 안에 레이블 넣기 위해 여백 조정
             legend.text = element_text(size = 15),legend.title = element_text(size = 15),plot.title = element_text(size = 15, face = "bold")           ))
}

ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot5,plot6,plot7,plot8,labels = c("E","F","G","H"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot9,labels = c("I"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,labels = c("A","B","C","D","E","F","G","H","I"),ncol = 2, nrow = 5, common.legend = TRUE)


for ( i in 1:length(inci_cname_F)){
  assign(paste0("plot",i),ggplot(subset(mdata,Type==c("Incidence") & Sex=="Female" &  Cancer==inci_cname_F[i]), aes(x = Year, y = Rate, shape=Comparison, color = Comparison)) +
           geom_line(size = 0.6) +
           geom_point(size = 0.7,fill = "white") +
           scale_fill_viridis(discrete = T, option = "E") +
           scale_color_manual(values = c("black","black", "chartreuse3", "brown3",'cadetblue3',"cornsilk3")) + 
           scale_shape_manual(values = c(15,16,17,18,19,20)) +
           theme_classic() +
           facet_wrap(~ Sex, strip.position = "top", nrow = 1) +
           labs(x = "Year", y = "Incidence Rate (per 100,000)",title=title_nameF_I[i]) +
           theme_ipsum() +
           theme(
             strip.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5,size=15), # 패널 레이블 위치 조정
             axis.title.x = element_text(hjust = 0.5, vjust = 0.5,size=15), # X축 이름 중앙 정렬
             axis.title.y = element_text(hjust = 0.5, vjust = 0.5,size=15), # Y축 이름 중앙 정렬
             plot.margin = margin(1, 1, 1, 1, "cm"), # 그래프 안에 레이블 넣기 위해 여백 조정
             legend.text = element_text(size = 15),legend.title = element_text(size = 15),plot.title = element_text(size = 15, face = "bold")           ))
}

ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot5,plot6,labels = c("E","F"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,labels = c("A","B","C","D","E","F"),ncol = 2, nrow = 5, common.legend = TRUE)

for ( i in 1:length(mort_cname_F)){
  assign(paste0("plot",i),ggplot(subset(mdata,Type==c("Mortality") & Sex=="Female" &  Cancer==mort_cname_F[i]), aes(x = Year, y = Rate, shape=Comparison, color = Comparison)) +
           geom_line(size = 0.6) +
           geom_point(size = 0.7,fill = "white") +
           scale_fill_viridis(discrete = T, option = "E") +
           scale_color_manual(values = c("black","black", "chartreuse3", "brown3",'cadetblue3',"cornsilk3")) + 
           scale_shape_manual(values = c(15,16,17,18,19,20)) +
           theme_classic() +
           facet_wrap(~ Sex, strip.position = "top", nrow = 1) +
           labs(x = "Year", y = "Mortality Rate (per 100,000)",title=title_nameF_D[i]) +
           theme_ipsum() +
           theme(
             strip.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5,size=15), # 패널 레이블 위치 조정
             axis.title.x = element_text(hjust = 0.5, vjust = 0.5,size=15), # X축 이름 중앙 정렬
             axis.title.y = element_text(hjust = 0.5, vjust = 0.5,size=15), # Y축 이름 중앙 정렬
             plot.margin = margin(1, 1, 1, 1, "cm"), # 그래프 안에 레이블 넣기 위해 여백 조정
             legend.text = element_text(size = 15),legend.title = element_text(size = 15),plot.title = element_text(size = 15, face = "bold")           ))
}

ggarrange(plot1,plot2,plot3,plot4,labels = c("A","B","C","D"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot5,plot6,plot7,plot8,labels = c("E","F","G","H"),ncol = 2, nrow = 2, common.legend = TRUE)
ggarrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,labels = c("A","B","C","D","E","F","G","H"),ncol = 2, nrow = 5, common.legend = TRUE)




#######################################################################################################################




NORDPRED_func_modif <- function (Scenarios1_male,Scenarios1_female){
  
  NORDPRED_INCI_M <-list()
  NORDPRED_INCI_F <-list()
  
  for (i in 1: length(inci_cname_M)){
    male_case <-subset(get(inci_cname_M[i]),Sex=="남자")[,-c(1:3,24)]
    male_pop  <- Scenarios1_male[, c(as.character(2000:2040))]
    NORDPRED_INCI_M[[i]] <-canproj(male_case,male_pop,startp=2020,startestage=7, methods="nordpred")$agsproj*(male_pop)/100000
  }
  for (i in 1: length(inci_cname_F)){
    female_case <-subset(get(inci_cname_F[i]),Sex=="여자")[,-c(1:3,24)]
    female_pop  <- Scenarios1_female[, c(as.character(2000:2040))]
    NORDPRED_INCI_F[[i]] <-canproj(female_case,female_pop,startp=2020,startestage=7, methods="nordpred")$agsproj*(female_pop)/100000
  }
  
  NORDPRED_MORT_M <-list()
  NORDPRED_MORT_F <-list()
  
  for ( i in 1: length(mort_cname_M)){
    male_case <- subset(get(mort_cname_M[i]),Sex=="남자")[,-c(1:3,24,25)]
    male_pop  <- Scenarios1_male[, c(as.character(2000:2040))]
    if(length(which(rowSums(male_case/20) <1)[length(which(rowSums(male_case/20) <1))]) == 0){
      NORDPRED_MORT_M[[i]] <-canproj(male_case,male_pop,startp=2020,startestage=7, methods="nordpred")$agsproj*(male_pop)/100000
    } else if (length(which(rowSums(male_case/20) <1)[length(which(rowSums(male_case/20) <1))]) !=0) {
      if (which(rowSums(male_case/20) <1)[length(which(rowSums(male_case/20) <1))] >7){  
        num = as.numeric(which(rowSums(male_case/20) <1)[length(which(rowSums(male_case /20) <1))])+1
        NORDPRED_MORT_M[[i]] <-canproj(male_case,male_pop,startp=2020,startestage=num, methods="nordpred")$agsproj*(male_pop)/100000
      } else {
        NORDPRED_MORT_M[[i]] <-canproj(male_case,male_pop,startp=2020,startestage=7, methods="nordpred")$agsproj*(male_pop)/100000
      }
    }
  }
  for ( i in 1: length(mort_cname_F)){
    female_case <- subset(get(mort_cname_F[i]),Sex=="여자")[,-c(1:3,24,25)]
    female_pop  <- Scenarios1_female[, c(as.character(2000:2040))]
    if(length(which(rowSums(female_case/20) <1)[length(which(rowSums(female_case/20) <1))]) == 0){
      NORDPRED_MORT_F[[i]] <-canproj(female_case,female_pop,startp=2020,startestage=7, methods="nordpred")$agsproj*(female_pop)/100000
    } else if (length(which(rowSums(female_case/20) <1)[length(which(rowSums(female_case/20) <1))]) !=0) {
      if (which(rowSums(female_case/20) <1)[length(which(rowSums(female_case/20) <1))] >7){  
        num = as.numeric(which(rowSums(female_case/20) <1)[length(which(rowSums(female_case /20) <1))])+1
        NORDPRED_MORT_F[[i]] <-canproj(female_case,female_pop,startp=2020,startestage=num, methods="nordpred")$agsproj*(female_pop)/100000
      } else {
        NORDPRED_MORT_F[[i]] <-canproj(female_case,female_pop,startp=2020,startestage=7, methods="nordpred")$agsproj*(female_pop)/100000
      }
    }
  }
  NORDPRED_LIST <-list(NORDPRED_INCI_M,NORDPRED_INCI_F,NORDPRED_MORT_M,NORDPRED_MORT_F)
  return(NORDPRED_LIST)
}

scene1_nd_byage<-NORDPRED_func_modif(Scenarios1_male,Scenarios1_female)
scene1_nd_byage_male_inc <- scene1_nd_byage[[1]]
scene1_nd_byage_male_mort <- scene1_nd_byage[[3]]
scene1_nd_byage_female_inc <- scene1_nd_byage[[2]]
scene1_nd_byage_female_mort <- scene1_nd_byage[[4]]

scene2_nd_byage<-NORDPRED_func_modif(Scenarios2_male,Scenarios2_female)
scene2_nd_byage_male_inc <- scene2_nd_byage[[1]]
scene2_nd_byage_male_mort <- scene2_nd_byage[[3]]
scene2_nd_byage_female_inc <- scene2_nd_byage[[2]]
scene2_nd_byage_female_mort <- scene2_nd_byage[[4]]

scene3_nd_byage<-NORDPRED_func_modif(Scenarios3_male,Scenarios3_female)
scene3_nd_byage_male_inc <- scene3_nd_byage[[1]]
scene3_nd_byage_male_mort <- scene3_nd_byage[[3]]
scene3_nd_byage_female_inc <- scene3_nd_byage[[2]]
scene3_nd_byage_female_mort <- scene3_nd_byage[[4]]

scene4_nd_byage<-NORDPRED_func_modif(Scenarios4_male,Scenarios4_female)
scene4_nd_byage_male_inc <- scene4_nd_byage[[1]]
scene4_nd_byage_male_mort <- scene4_nd_byage[[3]]
scene4_nd_byage_female_inc <- scene4_nd_byage[[2]]
scene4_nd_byage_female_mort <- scene4_nd_byage[[4]]

scene5_nd_byage<-NORDPRED_func_modif(Scenarios5_male,Scenarios5_female)
scene5_nd_byage_male_inc <- scene5_nd_byage[[1]]
scene5_nd_byage_male_mort <- scene5_nd_byage[[3]]
scene5_nd_byage_female_inc <- scene5_nd_byage[[2]]
scene5_nd_byage_female_mort <- scene5_nd_byage[[4]]

###############################


ageing_effect_check_modif <- function(Sex,Method,Extract_M){
  if (Sex == 1 & Method == 1) {
    changes_history <-list()
    for (j in 1:length(inci_cname_M)){
      P_year <- A_year <- E_year <- net_change <-percent<- numeric(length = 41)
      sample<-data.frame(scene1_nd_byage_male_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios1_male[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
      }
      
      percent <- round(A_year/net_change*100,2)
      percent[is.na(percent)] <- 0
      years = c(2000:2040)
      change1 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change,percent), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change","Percent"), each = 41))
      change1$Group <- factor(change1$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk","Percent"))
      change1$Type <-c("Median")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <-percent<- numeric(length = 41)
      sample<-data.frame(scene2_nd_byage_male_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios2_male[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
      }
      percent <- round(A_year/net_change*100,2)
      percent[is.na(percent)] <- 0
      years = c(2000:2040)
      change2 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change,percent),
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change","Percent"), each = 41))
      change2$Group <- factor(change2$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk","Percent"))
      change2$Type <-c("High")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <-percent<- numeric(length = 41)
      sample<-data.frame(scene3_nd_byage_male_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios3_male[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
      }
      percent <- round(A_year/net_change*100,2)
      percent[is.na(percent)] <- 0
      years = c(2000:2040)
      change3 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change,percent), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change","Percent"), each = 41))
      change3$Group <- factor(change3$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk","Percent"))
      change3$Type <-c("Low")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <-percent<- numeric(length = 41)
      sample<-data.frame(scene4_nd_byage_male_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios4_male[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
      }
      percent <- round(A_year/net_change*100,2)
      percent[is.na(percent)] <- 0
      years = c(2000:2040)
      change4 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change,percent), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change","Percent"), each = 41))
      change4$Group <- factor(change4$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk","Percent"))
      change4$Type <-c("Fast Ageing")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <-percent<- numeric(length = 41)
      sample<-data.frame(scene5_nd_byage_male_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios5_male[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
      }
      percent <- round(A_year/net_change*100,2)
      percent[is.na(percent)] <- 0
      years = c(2000:2040)
      change5 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change,percent), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change","Percent"), each = 41))
      change5$Group <- factor(change5$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk","Percent"))
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
      P_year <- A_year <- E_year <- net_change <-percent<- numeric(length = 41)
      sample<-data.frame(scene1_nd_byage_male_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios1_male[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
      }
      percent <- round(A_year/net_change*100,2)
      percent[is.na(percent)] <- 0
      years = c(2000:2040)
      change1 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change,percent), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change","Percent"), each = 41))
      change1$Group <- factor(change1$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk","Percent"))
      change1$Type <-c("Median")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <-percent<- numeric(length = 41)
      sample<-data.frame(scene2_nd_byage_male_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios2_male[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
        
      }
      percent <- round(A_year/net_change*100,2)
      percent[is.na(percent)] <- 0
      years = c(2000:2040)
      change2 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change,percent), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change","Percent"), each = 41))
      change2$Group <- factor(change2$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk","Percent"))
      change2$Type <-c("High")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <-percent<- numeric(length = 41)
      sample<-data.frame(scene3_nd_byage_male_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios3_male[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
        
        
      }
      percent <- round(A_year/net_change*100,2)
      percent[is.na(percent)] <- 0
      years = c(2000:2040)
      change3 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change,percent), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change","Percent"), each = 41))
      change3$Group <- factor(change3$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk","Percent"))
      change3$Type <-c("Low")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <-percent<- numeric(length = 41)
      sample<-data.frame(scene4_nd_byage_male_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios4_male[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
      
        
      }
      percent <- round(A_year/net_change*100,2)
      percent[is.na(percent)] <- 0
      years = c(2000:2040)
      change4 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change,percent),  
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change","Percent"), each = 41))
      change4$Group <- factor(change4$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk","Percent"))
      change4$Type <-c("Fast Ageing")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <-percent<- numeric(length = 41)
      sample<-data.frame(scene5_nd_byage_male_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios5_male[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
        
      }
      percent <- round(A_year/net_change*100,2)
      percent[is.na(percent)] <- 0
      years = c(2000:2040)
      change5 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change,percent), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change","Percent"), each = 41))
      change5$Group <- factor(change5$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk","Percent"))
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
      P_year <- A_year <- E_year <- net_change <-percent<- numeric(length = 41)
      sample<-data.frame(scene1_nd_byage_female_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios1_female[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
        
        
      }
      
      percent <- round(A_year/net_change*100,2)
      percent[is.na(percent)] <- 0
      years = c(2000:2040)
      change1 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change,percent), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change","Percent"), each = 41))
      change1$Group <- factor(change1$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk","Percent"))
      change1$Type <-c("Median")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <-percent<- numeric(length = 41)
      sample<-data.frame(scene2_nd_byage_female_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios2_female[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
        
        
      }
      percent <- round(A_year/net_change*100,2)
      percent[is.na(percent)] <- 0
      years = c(2000:2040)
      change2 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change,percent), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change","Percent"), each = 41))
      change2$Group <- factor(change2$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk","Percent"))
      change2$Type <-c("High")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <-percent<- numeric(length = 41)
      sample<-data.frame(scene3_nd_byage_female_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios3_female[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
        
      }
      percent <- round(A_year/net_change*100,2)
      percent[is.na(percent)] <- 0
      years = c(2000:2040)
      change3 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change,percent), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change","Percent"), each = 41))
      change3$Group <- factor(change3$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk","Percent"))
      change3$Type <-c("Low")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <-percent<- numeric(length = 41)
      sample<-data.frame(scene4_nd_byage_female_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios4_female[,as.character(2000:2040)],2000,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
        
        
      }
      percent <- round(A_year/net_change*100,2)
      percent[is.na(percent)] <- 0
      years = c(2000:2040)
      change4 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change,percent), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change","Percent"), each = 41))
      change4$Group <- factor(change4$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk","Percent"))
      change4$Type <-c("Fast Ageing")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <-percent<- numeric(length = 41)
      sample<-data.frame(scene5_nd_byage_female_inc[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios5_female[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,1])
        
      }
      percent <- round(A_year/net_change*100,2)
      percent[is.na(percent)] <- 0
      years = c(2000:2040)
      change5 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change,percent), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change","Percent"), each = 41))
      change5$Group <- factor(change5$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk","Percent"))
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
      P_year <- A_year <- E_year <- net_change <-percent<- numeric(length = 41)
      sample<-data.frame(scene1_nd_byage_female_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios1_female[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
        
      }
      percent <- round(A_year/net_change*100,2)
      percent[is.na(percent)] <- 0
      years = c(2000:2040)
      change1 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change,percent), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change","Percent"), each = 41))
      change1$Group <- factor(change1$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk","Percent"))
      change1$Type <-c("Median")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <-percent<- numeric(length = 41)
      sample<-data.frame(scene2_nd_byage_female_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios2_female[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
        
      }
      percent <- round(A_year/net_change*100,2)
      percent[is.na(percent)] <- 0
      years = c(2000:2040)
      change2 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change,percent), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change","Percent"), each = 41))
      change2$Group <- factor(change2$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk","Percent"))
      change2$Type <-c("High")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <-percent<- numeric(length = 41)
      sample<-data.frame(scene3_nd_byage_female_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios3_female[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
        
        
      }
      percent <- round(A_year/net_change*100,2)
      percent[is.na(percent)] <- 0
      years = c(2000:2040)
      change3 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change,percent), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change","Percent"), each = 41))
      change3$Group <- factor(change3$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk","Percent"))
      change3$Type <-c("Low")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <-percent<- numeric(length = 41)
      sample<-data.frame(scene4_nd_byage_female_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios4_female[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
        
        
      }
      percent <- round(A_year/net_change*100,2)
      percent[is.na(percent)] <- 0
      years = c(2000:2040)
      change4 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change,percent), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change","Percent"), each = 41))
      change4$Group <- factor(change4$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk","Percent"))
      change4$Type <-c("Fast Ageing")
      ##############################################################################################################################
      P_year <- A_year <- E_year <- net_change <-percent<- numeric(length = 41)
      sample<-data.frame(scene5_nd_byage_female_mort[j])
      for (i in 1:41){
        case <- (as.numeric(as.vector(unlist(DA(sample,Scenarios5_female[,as.character(2000:2040)],2005,1999+i)))))
        P_year[i] <- case[1]
        A_year[i] <- case[2]
        E_year[i] <- case[3]
        net_change[i] <- sum(sample[,i])-sum(sample[,6])
        
      }
      percent <- round(A_year/net_change*100,2)
      percent[is.na(percent)] <- 0
      years = c(2000:2040)
      change5 <- data.frame(Year = years, Change = c(P_year, A_year, E_year, net_change,percent), 
                            Group = rep(c("Population Growth risk", "Population Aging risk", "Incidence Rate risk", "Net Change","Percent"), each = 41))
      change5$Group <- factor(change5$Group,levels=c("Net Change","Population Aging risk", "Population Growth risk", "Incidence Rate risk","Percent"))
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
      return(plt)
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



sample1<-ageing_effect_check_modif(1,1,"Table")
sample2<-ageing_effect_check_modif(1,2,"Table")
sample3<-ageing_effect_check_modif(2,1,"Table")
sample4<-ageing_effect_check_modif(2,2,"Table")

rbind(t(reshape(subset(subset(sample1[[1]],Group=="Percent"),Year %in% c(2000,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
t(reshape(subset(subset(sample1[[2]],Group=="Percent"),Year %in% c(2000,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
t(reshape(subset(subset(sample1[[3]],Group=="Percent"),Year %in% c(2000,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
t(reshape(subset(subset(sample1[[4]],Group=="Percent"),Year %in% c(2000,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
t(reshape(subset(subset(sample1[[5]],Group=="Percent"),Year %in% c(2000,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
t(reshape(subset(subset(sample1[[6]],Group=="Percent"),Year %in% c(2000,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
t(reshape(subset(subset(sample1[[7]],Group=="Percent"),Year %in% c(2000,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
t(reshape(subset(subset(sample1[[8]],Group=="Percent"),Year %in% c(2000,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
t(reshape(subset(subset(sample1[[9]],Group=="Percent"),Year %in% c(2000,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]))

rbind(t(reshape(subset(subset(sample2[[1]],Group=="Percent"),Year %in% c(2005,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
t(reshape(subset(subset(sample2[[2]],Group=="Percent"),Year %in% c(2005,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
t(reshape(subset(subset(sample2[[3]],Group=="Percent"),Year %in% c(2005,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
t(reshape(subset(subset(sample2[[4]],Group=="Percent"),Year %in% c(2005,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
t(reshape(subset(subset(sample2[[5]],Group=="Percent"),Year %in% c(2005,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
t(reshape(subset(subset(sample2[[6]],Group=="Percent"),Year %in% c(2005,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
t(reshape(subset(subset(sample2[[7]],Group=="Percent"),Year %in% c(2005,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
t(reshape(subset(subset(sample2[[8]],Group=="Percent"),Year %in% c(2005,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
t(reshape(subset(subset(sample2[[9]],Group=="Percent"),Year %in% c(2005,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
t(reshape(subset(subset(sample2[[10]],Group=="Percent"),Year %in% c(2005,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]))

####

rbind(t(reshape(subset(subset(sample3[[1]],Group=="Percent"),Year %in% c(2000,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample3[[2]],Group=="Percent"),Year %in% c(2000,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample3[[3]],Group=="Percent"),Year %in% c(2000,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample3[[4]],Group=="Percent"),Year %in% c(2000,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample3[[5]],Group=="Percent"),Year %in% c(2000,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample3[[6]],Group=="Percent"),Year %in% c(2000,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]))

rbind(t(reshape(subset(subset(sample4[[1]],Group=="Percent"),Year %in% c(2005,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample4[[2]],Group=="Percent"),Year %in% c(2005,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample4[[3]],Group=="Percent"),Year %in% c(2005,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample4[[4]],Group=="Percent"),Year %in% c(2005,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample4[[5]],Group=="Percent"),Year %in% c(2005,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample4[[6]],Group=="Percent"),Year %in% c(2005,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample4[[5]],Group=="Percent"),Year %in% c(2005,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]),
      t(reshape(subset(subset(sample4[[6]],Group=="Percent"),Year %in% c(2005,2010,2019,2030,2040))[,-3],idvar = "Type",timevar = "Year",direction = "wide")[,-1]))





