#' Naismith walkspeed function
#'
#'
#' This calculates the maximum distance a person can travel on foot
#' It returns a vector (km/hr) maximum walking speed is 6km/hr.
#'
#' @param vo2max 
#' @param slope slope percent
#' @param cargo default = 15, weight of cargo kg
#' @param dragSlow effect of cargo on drag not really necessary
#' @param cargoSlow default = 0.2, Langmuir slowing effect of carrying 


naismithwalkspeed <- function(vo2max,
                              slope,
                              cargo = 15,
                              dragSlow = 0,
                              cargoSlow = 0.2){
  
  flatwalkspeed <- (((vo2max*0.5)-5)/2.5)
  if (flatwalkspeed > 6) {flatwalkspeed <- 6} 
  #    flatwalkspeed <- 6)  
  
  #get walk speed in m/s
  flatwalkspeed <- flatwalkspeed / 3.6
  
  #
  sec_per_flat_m <- 1/ flatwalkspeed 
  sec_per_flat_m
  
  climb_per_flat_m <- slope/100 
  climb_per_flat_m
  
  #naismith says add 1 minute per 10 metres ascended 
  #= 6 seconds per metre ascended 
  
  climb_time_per_flat_m <- climb_per_flat_m *6
  climb_time_per_flat_m
  
  tot_time_m <- sec_per_flat_m + climb_time_per_flat_m
  tot_time_m
  
  
  naismith_speed_ms <- 1/tot_time_m
  
  naismith_speed_ms
  
  walk_speed_kmh <- naismith_speed_ms *3.6
  
  #------- Langmuir style corrections for cargo ---- 
  # Weight:  we have calculated the efect of an extra 15kg weight reasonable robustly. 
  # however there are 2 other elements which have less good data
  # Drag:  estimate 8% loss in speed due to paniers etc increasing drag on bikes 
  #but walking is not fast enough for drag to be significant  
  # muscles:  estimate 20% a further  loss in speed because carrying weight be it in panniers a child seat or a rucsac
  #increases physiological strain on muscles and feels harder.  An argument for this:  NAismith said 5km/hr.  
  #LAngmuir corrected the base speed to 4km/hr particularly if we assume a pack 4/5 gives us a 20% reduction in speed
  #The usefulness of this is even though it's a rough estimate, it will still illustrate a potential saving.  
  
  if(cargo > 0){
    walk_speed_kmh =  walk_speed_kmh * (1 - dragSlow  - cargoSlow)
   
  }
  
  
  #set ensible limits
  walk_speed_kmh[walk_speed_kmh > 6 ] <- 6
  walk_speed_kmh[walk_speed_kmh < 0 ] <- 0
  
  
  return(walk_speed_kmh)
  
  
}
#end naismithwalkspeed defn
