
#trip_distance_distribution150321



library(tidyverse)
getwd()
library(foreign)


tripsfocus <- read_csv("tripsfocus020919.csv")


names(tripsfocus)


#-------------- recode age and gender ------------------
#recode sex and age variables so they match the  "sexage" constraint in the synthetic population

tripsfocus$SexAgeconst = ""

tripsfocus$SexAgeconst[tripsfocus$Sex_B01ID ==1 & tripsfocus$Age_B01ID <= 5 ] <- "munder16"
tripsfocus$SexAgeconst[tripsfocus$Sex_B01ID ==2 & tripsfocus$Age_B01ID <= 5 ] <- "funder16"

tripsfocus$SexAgeconst[tripsfocus$Sex_B01ID ==1 & tripsfocus$Age_B01ID >= 6 & tripsfocus$Age_B01ID <=10 ] <- "m1619"
tripsfocus$SexAgeconst[tripsfocus$Sex_B01ID ==2 & tripsfocus$Age_B01ID >= 6 & tripsfocus$Age_B01ID <=10 ] <- "f1619"

tripsfocus$SexAgeconst[tripsfocus$Sex_B01ID ==1 & tripsfocus$Age_B01ID >= 11 & tripsfocus$Age_B01ID <=12 ] <- "m2029"
tripsfocus$SexAgeconst[tripsfocus$Sex_B01ID ==2 & tripsfocus$Age_B01ID >= 11 & tripsfocus$Age_B01ID <=12 ] <- "f2029"

tripsfocus$SexAgeconst[tripsfocus$Sex_B01ID ==1 & tripsfocus$Age_B01ID == 13 ] <- "m3039"
tripsfocus$SexAgeconst[tripsfocus$Sex_B01ID ==2 & tripsfocus$Age_B01ID == 13 ] <- "f3039"

tripsfocus$SexAgeconst[tripsfocus$Sex_B01ID ==1 & tripsfocus$Age_B01ID == 14] <- "m4049"
tripsfocus$SexAgeconst[tripsfocus$Sex_B01ID ==2 & tripsfocus$Age_B01ID == 14] <- "f4049"

tripsfocus$SexAgeconst[tripsfocus$Sex_B01ID ==1 & tripsfocus$Age_B01ID == 15 ] <- "m5059"
tripsfocus$SexAgeconst[tripsfocus$Sex_B01ID ==2 & tripsfocus$Age_B01ID == 15 ] <- "f5059"

tripsfocus$SexAgeconst[tripsfocus$Sex_B01ID ==1 & tripsfocus$Age_B01ID >= 16 & tripsfocus$Age_B01ID <=17 ] <- "m6069"
tripsfocus$SexAgeconst[tripsfocus$Sex_B01ID ==2 & tripsfocus$Age_B01ID >= 16 & tripsfocus$Age_B01ID <=17 ] <- "f6069"

tripsfocus$SexAgeconst[tripsfocus$Sex_B01ID ==1 & tripsfocus$Age_B01ID >= 18 & tripsfocus$Age_B01ID <=21] <- "m70"
tripsfocus$SexAgeconst[tripsfocus$Sex_B01ID ==2 & tripsfocus$Age_B01ID >= 18 & tripsfocus$Age_B01ID <=21] <- "f70"

glimpse(tripsfocus)
gc()


#---------------- looped calculations to get the proportion of car km travelled which is under a given distance ---
#read in the subgroups
distribution <- read_csv("spss19/subpop_no_distribution.csv")

#make a results df
namesvec = names(distribution)
namesvec <- namesvec[5:39]
namesvec
resultsdf <- data.frame() 
#resultsdf <- rbind(resultsdf,vec_from_loop)
#colnames(resultsdf) <- namesvec 
resultsdf <- setNames(data.frame(matrix(ncol = 35, nrow = 0)), namesvec)


for(j in 1:128){
sexage_value <- distribution$SexAgeconst[j]
oac_value <- distribution$HHoldOAClass2011_B03ID[j]


  #get a subgroup for the morning
  subgroup <- tripsfocus %>% filter(SexAgeconst == sexage_value & HHoldOAClass2011_B03ID == oac_value & TripStartHours <= 11)
  
  vec_from_loop <- vector(mode = "numeric",length = 35)

  
  for(i in 1:35){
    # subgroup total carkm travelled am 
    totalkm <- subgroup %>% select(TripDisIncSW) %>% sum() # 1674km travelled by subgroup in the morning
    
    #subgroup car km made up of trips under a given distance.  
    
      
      #if there are trips of a particular distance
      kmunderdistance <-  subgroup %>% filter(TripDisIncSW <= i)
      if(nrow(kmunderdistance) > 0){#print(i)
        
        kmunderdistance2 <- kmunderdistance %>% select(TripDisIncSW) %>% sum() 
        
        pr_under_distance <- kmunderdistance2 / totalkm 
        vec_from_loop[[i]] <- pr_under_distance
        
        }
      
      
      #if thre are no trips of a particular distance
      if(nrow(kmunderdistance) == 0){#print(i)
        #if i is 1 and there are no trips length 1km then 
        vec_from_loop[[i]] <- 0
        
        #if i is greater than 1 and there are not trips then
        if(i>1){vec_from_loop[[i]] <- vec_from_loop[[i-1]]}
        
        
        }
      
  
  }

  resultsdf <- rbind(resultsdf,vec_from_loop)
  rm(vec_from_loop)
#end j loop  
}

colnames(resultsdf) <- namesvec  
tripdist <- distribution %>% select(SexAgeconst,HHoldOAClass2011_B03ID,HasBike_mean,tripsinweek_mean)
tripdist <- cbind(tripdist,resultsdf)
tripdist$sumweeklytripdist_mean <- distribution$sumweeklytripdist_mean  
  
write_csv(tripdist,"spss19/AMsubpopgroupvarsSexageOACbikeown150321.csv")  
  
  
