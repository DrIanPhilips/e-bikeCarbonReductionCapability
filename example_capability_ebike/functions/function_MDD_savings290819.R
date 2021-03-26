#' functionMDD
#'
#'
#' This calculates the maximum car km a person can replace and the co2 
#' savings (grams) given that some people have a bike and those who don't walk. Returns dataframe  
#' 
#' @param ID identifier numeric
#' @param weightedkm_pp, vector estimate of car km travelled per person pa
#' @param walkspeedkmh, vector maximum distance a person can walk in an hour km
#' @param emfac_co2, vector car co2 emissions factor g/km 
#' @param HasBike_mean, vector probabilty of having an ordinary bike passed to calling function
#' @param OAC, geodemographic group Output Area Classification supergroup number 1-7
#' @param sexage, gender and age category numeric
#' @param draw_bike, binary 0 is does not have a bike, 1 is has a bike



function_MDD <- function(ID,
                         weightedkm_pp,
                         roaddist,
                         walkspeedkmh,
                         emfac_co2,
                         HasBike_mean,
                         OAC,
                         sexage,
                         draw_bike){
gc()
#---------- turn the inputs into a df -----------------  
df <- data.frame(ID, weightedkm_pp,
                 roaddist,
                 walkspeedkmh,
                 emfac_co2,
                 HasBike_mean,
                 OAC,
                 sexage,
                 draw_bike)
gc()


#---------- draw MDD using monte-carlo sample -------------- 
df <- df %>% mutate(drawMDD = ifelse(test = draw_bike <= HasBike_mean , yes = roaddist,no = walkspeedkmh)) 
  return(df)
}

