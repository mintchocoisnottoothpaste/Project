### APC EFFECT TOOLS
### DATA modeifed 

load(file="/Users/jun/Library/CloudStorage/OneDrive-Personal/Dissertation/newone/rawdata/workspace.RData")

inci_cname_M <- inci_cname[inci_cname %in% c("C16_I","C18_C20_I","C22_I","C23_C24_I","C25_I","C33_C34_I","C61_I","C64_I","C67_I")]
inci_cname_F <- inci_cname[inci_cname %in% c("C16_I","C18_C20_I","C22_I","C23_C24_I","C25_I","C33_C34_I","C50_I","C73_I")]

mort_cname_M <- mort_cname[mort_cname %in% c("C15_D","C16_D","C18_C21_D","C22_D","C25_D","C33_C34_D","C61_D","C67_D","C82_C86_D","C91_C95_D")]
mort_cname_F <- mort_cname[mort_cname %in% c("C16_D","C18_C21_D","C22_D","C25_D","C33_C34_D","C53_D","C56_D","C82_C86_D","C91_C95_D")]


pop_data_M<-cbind('2000-2004'=rowSums(Scenarios1_male[,as.character(2000:2004)]),
      '2005-2009'=rowSums(Scenarios1_male[,as.character(2005:2009)]),
      '2010-2014'=rowSums(Scenarios1_male[,as.character(2010:2014)]),
      '2015-2019'=rowSums(Scenarios1_male[,as.character(2015:2019)]))

pop_data_F<-cbind('2000-2004'=rowSums(Scenarios1_female[,as.character(2000:2004)]),
      '2005-2009'=rowSums(Scenarios1_female[,as.character(2005:2009)]),
      '2010-2014'=rowSums(Scenarios1_female[,as.character(2010:2014)]),
      '2015-2019'=rowSums(Scenarios1_female[,as.character(2015:2019)]))


for ( i in 1:length(inci_cname_M)){
  case_data_M<-cbind('2000-2004'=rowSums(subset(get(inci_cname_M[i]),Sex=="害切")[,as.character(2000:2004)]),
                     '2005-2009'=rowSums(subset(get(inci_cname_M[i]),Sex=="害切")[,as.character(2005:2009)]),
                     '2010-2014'=rowSums(subset(get(inci_cname_M[i]),Sex=="害切")[,as.character(2010:2014)]),
                     '2015-2019'=rowSums(subset(get(inci_cname_M[i]),Sex=="害切")[,as.character(2015:2019)]))
  
  print(cbind(case_data_M[,1],pop_data_M[,1],case_data_M[,2],pop_data_M[,2],case_data_M[,3],pop_data_M[,3],case_data_M[,4],pop_data_M[,4])
)
}

for ( i in 1:length(mort_cname_M)){
  case_data_M<-cbind('2000-2004'=rowSums(subset(get(mort_cname_M[i]),Sex=="害切")[,as.character(2000:2004)]),
                     '2005-2009'=rowSums(subset(get(mort_cname_M[i]),Sex=="害切")[,as.character(2005:2009)]),
                     '2010-2014'=rowSums(subset(get(mort_cname_M[i]),Sex=="害切")[,as.character(2010:2014)]),
                     '2015-2019'=rowSums(subset(get(mort_cname_M[i]),Sex=="害切")[,as.character(2015:2019)]))
  
  print(cbind(case_data_M[,1],pop_data_M[,1],case_data_M[,2],pop_data_M[,2],case_data_M[,3],pop_data_M[,3],case_data_M[,4],pop_data_M[,4])
  )
}


for ( i in 1:length(inci_cname_F)){
  case_data_M<-cbind('2000-2004'=rowSums(subset(get(inci_cname_F[i]),Sex=="食切")[,as.character(2000:2004)]),
                     '2005-2009'=rowSums(subset(get(inci_cname_F[i]),Sex=="食切")[,as.character(2005:2009)]),
                     '2010-2014'=rowSums(subset(get(inci_cname_F[i]),Sex=="食切")[,as.character(2010:2014)]),
                     '2015-2019'=rowSums(subset(get(inci_cname_F[i]),Sex=="食切")[,as.character(2015:2019)]))
  
  print(cbind(case_data_M[,1],pop_data_F[,1],case_data_M[,2],pop_data_F[,2],case_data_M[,3],pop_data_F[,3],case_data_M[,4],pop_data_F[,4])
  )
}

for ( i in 1:length(mort_cname_F)){
  case_data_M<-cbind('2000-2004'=rowSums(subset(get(mort_cname_F[i]),Sex=="食切")[,as.character(2000:2004)]),
                     '2005-2009'=rowSums(subset(get(mort_cname_F[i]),Sex=="食切")[,as.character(2005:2009)]),
                     '2010-2014'=rowSums(subset(get(mort_cname_F[i]),Sex=="食切")[,as.character(2010:2014)]),
                     '2015-2019'=rowSums(subset(get(mort_cname_F[i]),Sex=="食切")[,as.character(2015:2019)]))
  
  print(cbind(case_data_M[,1],pop_data_F[,1],case_data_M[,2],pop_data_F[,2],case_data_M[,3],pop_data_F[,3],case_data_M[,4],pop_data_F[,4])
  )
}



####################################################################################
####################################################################################


pop_data_M_proj<-cbind('2000-2004'=rowSums(Scenarios1_male[,as.character(2000:2004)]),
                  '2005-2009'=rowSums(Scenarios1_male[,as.character(2005:2009)]),
                  '2010-2014'=rowSums(Scenarios1_male[,as.character(2010:2014)]),
                  '2015-2019'=rowSums(Scenarios1_male[,as.character(2015:2019)]),
                  '2020-2024'=rowSums(Scenarios1_male[,as.character(2020:2024)]),
                  '2025-2029'=rowSums(Scenarios1_male[,as.character(2025:2029)]),
                  '2030-2034'=rowSums(Scenarios1_male[,as.character(2030:2034)]),
                  '2035-2039'=rowSums(Scenarios1_male[,as.character(2035:2039)]))

pop_data_F_proj<-cbind('2000-2004'=rowSums(Scenarios1_female[,as.character(2000:2004)]),
                  '2005-2009'=rowSums(Scenarios1_female[,as.character(2005:2009)]),
                  '2010-2014'=rowSums(Scenarios1_female[,as.character(2010:2014)]),
                  '2015-2019'=rowSums(Scenarios1_female[,as.character(2015:2019)]),
                  '2020-2024'=rowSums(Scenarios1_female[,as.character(2020:2024)]),
                  '2025-2029'=rowSums(Scenarios1_female[,as.character(2025:2029)]),
                  '2030-2034'=rowSums(Scenarios1_female[,as.character(2030:2034)]),
                  '2035-2039'=rowSums(Scenarios1_female[,as.character(2035:2039)]))


scene1_nd_byage_male_inc <- scene1_nd_byage[[1]]
scene1_nd_byage_female_inc <- scene1_nd_byage[[2]]
scene1_nd_byage_male_mort <- scene1_nd_byage[[3]]
scene1_nd_byage_female_mort <- scene1_nd_byage[[4]]

for ( i in 1:length(inci_cname_M)){
  case_data_M<-cbind('2000-2004'=rowSums(scene1_nd_byage_male_inc[[i]][,as.character(2000:2004)]),
                     '2005-2009'=rowSums(scene1_nd_byage_male_inc[[i]][,as.character(2005:2009)]),
                     '2010-2014'=rowSums(scene1_nd_byage_male_inc[[i]][,as.character(2010:2014)]),
                     '2015-2019'=rowSums(scene1_nd_byage_male_inc[[i]][,as.character(2015:2019)]),
                     '2020-2024'=rowSums(scene1_nd_byage_male_inc[[i]][,as.character(2020:2024)]),
                     '2025-2029'=rowSums(scene1_nd_byage_male_inc[[i]][,as.character(2025:2029)]),
                     '2030-2034'=rowSums(scene1_nd_byage_male_inc[[i]][,as.character(2030:2034)]),
                     '2035-2039'=rowSums(scene1_nd_byage_male_inc[[i]][,as.character(2035:2039)]))
  
  print(cbind(case_data_M[,1],pop_data_M_proj[,1],case_data_M[,2],pop_data_M_proj[,2],case_data_M[,3],pop_data_M_proj[,3],case_data_M[,4],pop_data_M_proj[,4],
              case_data_M[,5],pop_data_M_proj[,5],case_data_M[,6],pop_data_M_proj[,6],case_data_M[,7],pop_data_M_proj[,7],case_data_M[,8],pop_data_M_proj[,8])
  )
}

for ( i in 1:length(mort_cname_M)){
  case_data_M<-cbind('2000-2004'=rowSums(scene1_nd_byage_male_mort[[i]][,as.character(2000:2004)]),
                     '2005-2009'=rowSums(scene1_nd_byage_male_mort[[i]][,as.character(2005:2009)]),
                     '2010-2014'=rowSums(scene1_nd_byage_male_mort[[i]][,as.character(2010:2014)]),
                     '2015-2019'=rowSums(scene1_nd_byage_male_mort[[i]][,as.character(2015:2019)]),
                     '2020-2024'=rowSums(scene1_nd_byage_male_mort[[i]][,as.character(2020:2024)]),
                     '2025-2029'=rowSums(scene1_nd_byage_male_mort[[i]][,as.character(2025:2029)]),
                     '2030-2034'=rowSums(scene1_nd_byage_male_mort[[i]][,as.character(2030:2034)]),
                     '2035-2039'=rowSums(scene1_nd_byage_male_mort[[i]][,as.character(2035:2039)]))
  
  print(cbind(case_data_M[,1],pop_data_M_proj[,1],case_data_M[,2],pop_data_M_proj[,2],case_data_M[,3],pop_data_M_proj[,3],case_data_M[,4],pop_data_M_proj[,4],
              case_data_M[,5],pop_data_M_proj[,5],case_data_M[,6],pop_data_M_proj[,6],case_data_M[,7],pop_data_M_proj[,7],case_data_M[,8],pop_data_M_proj[,8])
  )
}


for ( i in 1:length(inci_cname_F)){
  case_data_F<-cbind('2000-2004'=rowSums(scene1_nd_byage_female_inc[[i]][,as.character(2000:2004)]),
                     '2005-2009'=rowSums(scene1_nd_byage_female_inc[[i]][,as.character(2005:2009)]),
                     '2010-2014'=rowSums(scene1_nd_byage_female_inc[[i]][,as.character(2010:2014)]),
                     '2015-2019'=rowSums(scene1_nd_byage_female_inc[[i]][,as.character(2015:2019)]),
                     '2020-2024'=rowSums(scene1_nd_byage_female_inc[[i]][,as.character(2020:2024)]),
                     '2025-2029'=rowSums(scene1_nd_byage_female_inc[[i]][,as.character(2025:2029)]),
                     '2030-2034'=rowSums(scene1_nd_byage_female_inc[[i]][,as.character(2030:2034)]),
                     '2035-2039'=rowSums(scene1_nd_byage_female_inc[[i]][,as.character(2035:2039)]))
  
  print(cbind(case_data_F[,1],pop_data_F_proj[,1],case_data_F[,2],pop_data_F_proj[,2],case_data_F[,3],pop_data_F_proj[,3],case_data_F[,4],pop_data_F_proj[,4],
              case_data_F[,5],pop_data_F_proj[,5],case_data_F[,6],pop_data_F_proj[,6],case_data_F[,7],pop_data_F_proj[,7],case_data_F[,8],pop_data_F_proj[,8])
  )
}

for ( i in 1:length(mort_cname_F)){
  case_data_F<-cbind('2000-2004'=rowSums(scene1_nd_byage_female_mort[[i]][,as.character(2000:2004)]),
                     '2005-2009'=rowSums(scene1_nd_byage_female_mort[[i]][,as.character(2005:2009)]),
                     '2010-2014'=rowSums(scene1_nd_byage_female_mort[[i]][,as.character(2010:2014)]),
                     '2015-2019'=rowSums(scene1_nd_byage_female_mort[[i]][,as.character(2015:2019)]),
                     '2020-2024'=rowSums(scene1_nd_byage_female_mort[[i]][,as.character(2020:2024)]),
                     '2025-2029'=rowSums(scene1_nd_byage_female_mort[[i]][,as.character(2025:2029)]),
                     '2030-2034'=rowSums(scene1_nd_byage_female_mort[[i]][,as.character(2030:2034)]),
                     '2035-2039'=rowSums(scene1_nd_byage_female_mort[[i]][,as.character(2035:2039)]))
  
  print(cbind(case_data_F[,1],pop_data_F_proj[,1],case_data_F[,2],pop_data_F_proj[,2],case_data_F[,3],pop_data_F_proj[,3],case_data_F[,4],pop_data_F_proj[,4],
              case_data_F[,5],pop_data_F_proj[,5],case_data_F[,6],pop_data_F_proj[,6],case_data_F[,7],pop_data_F_proj[,7],case_data_F[,8],pop_data_F_proj[,8])
  )
}






