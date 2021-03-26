#' Draw the distance and co2 savings function
#'
#'
#' This calculates the maximum car km a person can replace and the co2 
#' savings (grams) if they use an e-bike, bike, or walking. Returns a dataframe, 
#'
#' @param draw_bike default = NULL, binary 0 is does not have a bike, 1 is has a bike
#' @param roaddist default = MDO1$maxroaddist, a vector the maximum distance the person can ride by e-bike or bike in 1 hour km
#' @param weightedkm_pp default = MDO1$weightedkm_pp, vector estimate of car km travelled per person pa
#' @param walkspeedkmh default = MDO1$walkspeedkmh, vector maximum distance a person can walk in an hour km
#' @param emfac_co2 = MDO1$emfac_co2_av_Y, vector car co2 emissions factor g/km 
#' @param HasBike_mean = MDO1$HasBike_mean, vector probabilty of having an ordinary bike passed to calling function


functionDODrawDistSavingsALL081220altemfac <- function(draw_bike = NULL,
  roaddist = MDO1$maxroaddist,
  ID = MDO1$personID,
  weightedkm_pp = MDO1$weightedkm_pp,
  walkspeedkmh = MDO1$walkspeedkmh,
  emfac_co2 = MDO1$emfac_co2_av_Y,
  HasBike_mean = MDO1$HasBike_mean,
  OAC = MDO1$Supergroup_Code,
  sexage = MDO1$sexageconst
  ){
  
 #if else statement depending whether there is a dice roll for a 
  #person having a bike instead of walking
  
  if(!is.null(draw_bike)){print("mdd")
    
    MDDdf <- function_MDD(ID = ID,
                          weightedkm_pp = weightedkm_pp,
                          roaddist = roaddist,
                          walkspeedkmh = walkspeedkmh,
                          emfac_co2 = emfac_co2,
                          HasBike_mean = HasBike_mean,
                          OAC = OAC,
                          sexage = sexage,
                          draw_bike = draw_bike,
                          carlifecycle_emissions)
    
    
  }else{
    print("allbike")
    
    #drawMDD is the maximum travel distance Max Distance Drawn
    #In this else part of the control structure,
    #instead of calling function_MDD to sample 
    # distance depending on whether people walk or bike
    # we know everyone is drawn as having a bike / e-bike 
    drawMDD = roaddist
    
    ##no need for the mdd function because everyone has a bike
    MDDdf <- data.frame(ID, weightedkm_pp,
                        roaddist,
                        walkspeedkmh,
                        emfac_co2,
                        HasBike_mean,
                        OAC,
                        sexage,
                        drawMDD
    )
    
    
    
  }
  
  gc()
  
  print("done MDDdf")
  
  
  
  #-------- estimate the car km which can be substituted in this draw ------- .   
  #Use tidyr function to reshape the lookup data 
  #so that instead of having a truly hideous loop 
  #we can do a join
  #NB gather is being replaced by pivot functions 
  #so there may be deprication issues in future
 
  #-------- organise the subpop tables  ------- .   
  
  #Use tidyr package to reshape the lookup data 
  #so that instead of having a truly hideous loop 
  #we can do a join
  #names(subpopAM)
  gatheredAM  <- subpopAM[,c(-3,-4,-40)]
  names(gatheredAM)
  gatheredAM <- gather(gatheredAM,
                               key = "roundedMaxActiveDistAM",
                               value = "pr_replaceabledistanceAM",
                               -SexAgeconst,-HHoldOAClass2011_B03ID)
  
  
  gatheredAM$roundedMaxActiveDistAM <- substring(gatheredAM$roundedMaxActiveDistAM, 9, 10)
  gatheredAM$roundedMaxActiveDistAM <- gsub('k', '', gatheredAM$roundedMaxActiveDistAM)
  gatheredAM$roundedMaxActiveDistAM <- as.numeric(gatheredAM$roundedMaxActiveDistAM)
  
  
  #names(subpopPM)
  gatheredPM  <- subpopPM[,c(-3,-4,-40)]
  #names(gatheredPM)
  gatheredPM <- gather(gatheredPM,
                       key = "roundedMaxActiveDistPM",
                       value = "pr_replaceabledistancePM",
                       -SexAgeconst,-HHoldOAClass2011_B03ID)
  
  
  gatheredPM$roundedMaxActiveDistPM <- substring(gatheredPM$roundedMaxActiveDistPM, 9, 10)
  gatheredPM$roundedMaxActiveDistPM <- gsub('k', '', gatheredPM$roundedMaxActiveDistPM)
  gatheredPM$roundedMaxActiveDistPM <- as.numeric(gatheredPM$roundedMaxActiveDistPM)
  
print("done subpopsetup")
  
  car_am_pm <- data.frame(SexAgeconst = subpopAM$SexAgeconst,
                          HHoldOAClass2011_B03ID = subpopAM$HHoldOAClass2011_B03ID,
                          kmam = subpopAM$sumweeklytripdist_mean,
                          kmpm = subpopPM$sumweeklytripdist_mean)
  
  #percent of carkm travelled before noon
  car_am_pm$pc_kmam <- car_am_pm$kmam / (car_am_pm$kmam + car_am_pm$kmpm)
  
  MDDdf <- left_join(MDDdf,car_am_pm, 
                     by = c("sexage" = "SexAgeconst",
                            "OAC" ="HHoldOAClass2011_B03ID"))
  
  
print("done car ampm")  
  
  #-------------------------- estimate the proportion km which can be substituted ------------
  
  
  MDDdf$roundedMDD <- ceiling(MDDdf$drawMDD)
  
  #join mdddf to the replaceable distance for each row, 
  MDDdf <- left_join(MDDdf,gatheredAM,
                     by = c("sexage" = "SexAgeconst",
                            "OAC" ="HHoldOAClass2011_B03ID",
                            "roundedMDD"  = "roundedMaxActiveDistAM" ))
  #glimpse(MDDdf)
  
  MDDdf <- left_join(MDDdf,gatheredPM,
                     by = c("sexage" = "SexAgeconst",
                            "OAC" ="HHoldOAClass2011_B03ID",
                            "roundedMDD"  = "roundedMaxActiveDistPM" ))
  
  
  
  glimpse(MDDdf)
  #summary(MDDdf$pr_replaceabledistanceAM)
  MDDdf$pr_replaceabledistanceAM[is.na(MDDdf$pr_replaceabledistanceAM)] <- 0
  #summary(MDDdf$pr_replaceabledistancePM)
  MDDdf$pr_replaceabledistancePM[is.na(MDDdf$pr_replaceabledistancePM)] <- 0
  
  #print(names(MDDdf))
  # [1] "ID"                       "weightedkm_pp"            "roaddist"                
  # [4] "walkspeedkmh"             "emfac_co2"                "HasBike_mean"            
  # [7] "OAC"                      "sexage"                   "draw_bike"               
  # [10] "drawMDD"                  "roundedMDD"               "pr_replaceabledistanceAM"
  # [13] "pr_replaceabledistancePM"
  
 
print("done car propn dist replace") 
  
#--------------------calcuate replaceable distance ------
  
  # MDDdf$pr_replaceabledistanceAM / PM is the % of trips under the maximum active travel distance
  #first calculate % of annual km pp which is theoretically replaceable 
  #there may be several trips under the mdd so not all of them are replaceable
  
  
  
  #CHANGES 081220
  #A previous version of this line of code double counted the km per person. This has been corrected
  MDDdf$replaceabledistance <- (MDDdf$pr_replaceabledistanceAM * (MDDdf$weightedkm_pp * MDDdf$pc_kmam)) + 
    (MDDdf$pr_replaceabledistancePM * (MDDdf$weightedkm_pp *(1- MDDdf$pc_kmam)))
  print("done new calculation")  
  #This double counted the annual car travel
  #MDDdf$replaceabledistance <- (MDDdf$pr_replaceabledistanceAM * MDDdf$weightedkm_pp) + 
  #  (MDDdf$pr_replaceabledistancePM * MDDdf$weightedkm_pp)
  #summary(MDDdf$replaceabledistance)
  
  
  
  #This line below is taking account of trip rate.  
  #the mean trip rate in the NTS data is ~ 2 trips per day
  #but where people have high trip rates they might make 3 or more trips 
  #at their maximimum ebike/walk/ cycle distance.  The line below 
  # takes account of this, so we don't over estimate the replaceable distance.    

  MDDdf$replaceabledistance[MDDdf$replaceabledistance > (MDDdf$drawMDD *365*2)] <- MDDdf$drawMDD *365 *2 
  
  
  #replaceMDDkmcannot be greater than total weightedkm_pp 
  #MDDdf$replaceabledistance[MDDdf$replaceabledistance > MDDdf$weightedkm_pp]<- MDDdf$weightedkm_pp 
  MDDdf <- MDDdf %>% 
    mutate(replaceabledistance = ifelse(test = replaceabledistance >  MDDdf$weightedkm_pp,
                                        yes = weightedkm_pp,
                                        no = replaceabledistance)) 
  
  gc()
  
  print("done calc replaceable distance")   
  #----------- calculate co2 savings  -------
  #annual co2 savings in grams per person
  MDDdf$co2save <- MDDdf$replaceabledistance * (MDDdf$emfac_co2 + carlifecycle_emissions)
  
  return(MDDdf)
  #remove mdddf ready to start next scenario 
  rm(MDDdf)
  gc()
  
  
}