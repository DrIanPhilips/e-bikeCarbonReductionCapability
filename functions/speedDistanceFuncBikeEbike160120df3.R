#' Maximum cycle distance function (Langmuir 250variant)
#'
#'
#'This calculates the maximum distance a person can travel by bike or e-bike
#' It returns the dataframe with the distances and other variables.  It incorporates 
#' the most stringent constraints 1) langmuir style corrections for carrying bike and e-bike cargo
#' 2) maximum ebike output power of 250W 
#'
#' @param data default = NULL, a data frame
#' @param personID default = "personID"
#' @param eW default = 250, e-bike Watts the continuous power rating of the e-bike
#' @param assistpc default = 300, the assistance ratio percentage.
#' @param Ww default = "pedal_power", rider pedal power in Watts
#' @param Vw default = "windspeed", the mean windspeed experienced by rider
#' @param m default = "weight", rider weight kg
#' @param bicycle_weight default = 15, bike weight kg
#' @param ebike_weight default = 20, e-bike weight kg
#' @param s default = "slope_percent", average road slope percent 
#' @param circuity default = 1.4, circuity to compare to euclidean distance
#' @param coeff_rr default = 0.008, coefficient of rolling resistance
#' @param cargo default = 15, weight of cargo kg
#' @param dragSlow default = 0.08, effect of cargo on drag
#' @param cargoSlow default = 0.2, Langmuir slowing effect of carrying 
#' @param return_a_vector default = TRUE
#' 















# popfile1k <- read_csv("popfile1k.csv")
# popfile1k$wind <- 0
# #functioncycle_dist returns a vector of values 
# 
# #it calls several other functions when it runs.  
# #These are towards the bottom of the script
# 
# 
# 
# functioncycle_dist(data = NULL,
#   personID = 1,
#   Ww = 75,
#   Vw= 0,
#   m = 70,
#   s = 4,
#   circuity = 1.4,
#   coeff_rr = 0.008,
#   return_a_vector = TRUE
# )
# 
# test <- functioncycle_dist(data = NULL,
#                    personID = 1,
#                    eW = 250,
#                    assistpc = 300,
#                    Ww = 55,
#                    Vw= 0,
#                    m = 70,
#                    s = 1,
#                    circuity = 1.4,
#                    coeff_rr = 0.008,
#                    return_a_vector = F
# )
# 
# test
# 
# 
# test <- functioncycle_dist(data = popfile1k,
#                            personID = "personID",
#                            eW = 250,
#                            assistpc = 300,
#                            Ww = "power",
#                            Vw= "wind",
#                            m = "imputeweight",
#                            s = "slope",
#                            circuity = 1.4,
#                            coeff_rr = 0.008,
#                            return_a_vector = F
# )
# 
# test
# 




#------------ maximum cycle distance function  --------------------


functioncycle_dist <- function (data = NULL,
                                personID = "personID",
                                eW = 250,
                                assistpc = 300,
                                Ww = "pedal_power",
                                Vw= "windspeed",
                                m = "weight",
                                bicycle_weight = 15, 
                                ebike_weight = 20,
                                s = "slope_percent",
                                circuity = 1.4,
                                coeff_rr = 0.008,
                                cargo = 15,
                                dragSlow = 0.08,
                                cargoSlow = 0.2,
                                return_a_vector = TRUE
                                ){

  
  #set up the data input from the arguments 
  if (is.null(data)) {
    #make a df from the vectors
    total <- data.frame(personID,Ww,Vw,m,s)
  } else { 
    input_vars <- c(personID,Ww,Vw,m,s)
    #This subsets the input df to just keep the variables 
    #total <- data[input_vars]
    total <- data[,..input_vars]
    names(total) <- c("personID","Ww","Vw","m","s")
  }
  
  #in the data slope is a number 1-100 percent.  here the value should be between 0 and 1
  total$s <- total$s/100
  coesff_rr <- coeff_rr
  #ammend the bicycle weight variable to include any cargo carried.  
  bicycle_weight <- bicycle_weight + cargo
    
  total$upspeed <- speed_iterative(total$Ww,total$Vw,total$m,total$s,coeff_rr = coeff_rr,
                                   bike_mass = bicycle_weight) #Use s as it is for going up
  total$flatspeed <- speed_iterative(total$Ww,total$Vw,total$m,0, coeff_rr = coeff_rr,
                                     bike_mass = bicycle_weight) #s =0 when flat
  total$downspeed <- down_speed_iterative(Ww = total$Ww,
                                    m = total$m,
                                    Vw = total$Vw,
                                    slope01 = total$s,
                                    coeff_rr = coeff_rr,
                                    bike_mass = bicycle_weight)
  
  
  #sometimes going downhill without pedalling
  #is slower than riding on the flat
  #print(flatspeed)
  #print(downspeed)
  total$downspeed[total$downspeed < total$flatspeed] <- total$flatspeed
  
  
  
  #We now have upspeed, flatspeed and downspeed
  #convert from m/s to km/hr for the next bit of the function
  total$upspeed = total$upspeed *3.6
  total$flatspeed = total$flatspeed *3.6
  total$downspeed = total$downspeed * 3.6
   
  #------- Langmuir style corrections for cargo ---- 
  # Weight:  we have calculated the efect of an extra 15kg weight reasonable robustly. 
  # however there are 2 other elements which have less good data
  # Drag:  estimate 8% loss in speed due to paniers etc increasing drag.  
  # muscles:  estimate 20% a further  loss in speed because carrying weight be it in panniers a child seat or a rucsac
  #increases physiological strain on muscles and feels harder.  An argument for this:  NAismith said 5km/hr.  
  #LAngmuir corrected the base speed to 4km/hr particularly if we assume a pack 4/5 gives us a 20% reduction in speed
  
  #the latter 2 are really just an estimate for sensitivity purposes 
  #The usefulness of this is even though it's a rough estimate, it will still illustrate a potential saving.  
  #no cargo speed penalty downhill or on flat on a bike or e-bike
  
  if(cargo > 0){
    total$upspeed = total$upspeed * (1- dragSlow  - cargoSlow)
    total$flatspeed = total$flatspeed * (1- dragSlow)
    total$downspeed = total$downspeed * (1- dragSlow) 
    }

  
  #This accounts for slope profile and circuity
  #and estimates a maximum distance. 
  if (s >= 4){ 
    total$kmtime <-  (0.56* 60/ total$upspeed) + (0.44*60/total$downspeed)
    total$maxroaddist <- 60/total$kmtime
  }
  
  if (s < 4 & s >=2) {
    total$kmtime = (0.504*60/total$upspeed) + (0.1*60/total$flatspeed) +(0.396*60/total$downspeed)
    total$maxroaddist = 60/total$kmtime  
    
  }
  
  if (s <2) 
    #set kmtime = (0.375*60/upspeed) + (0.25*60/flatspeed) + (0.375*60/downspeed);
    total$kmtime = (0.42*60/total$upspeed) + (0.25*60/total$flatspeed) + (0.33*60/total$downspeed)
  total$maxroaddist = 60 / total$kmtime
  
  #circuity = 1.4 default
  total$maxeuclideandist = total$maxroaddist / circuity
  
  
#  if(cargo > 0){
#    total$maxroaddist = total$maxroaddist * (1- dragSlow  - cargoSlow)
#   
#  }  
  
# ---- include e-bike speed adjustments to account for varied assistance ------
  
  #the power with issistance is nominally 
  #the pedal_power * the % assistance #nb assist /100 to get assist ratio
  total$assistpower <- total$Ww * (assistpc/100)
  #assistpower is capped at 250W in the uk eW
  total$assistpower[total$assistpower > eW] <- eW
  
  
  total$ebikeupspeed <- speed_iterative(total$assistpower,total$Vw,
                                        total$m,total$s,coeff_rr = coeff_rr,
                                        bike_mass = ebike_weight) #Use s as it is for going up
  total$ebikeflatspeed <- speed_iterative(total$assistpower,total$Vw,total$m,0, 
                                          coeff_rr = coeff_rr,
                                          bike_mass = ebike_weight) #s =0 when flat
  total$ebikedownspeed <- down_speed_iterative(Ww = total$assistpower,
                                    m = total$m,
                                    Vw = total$Vw,
                                    slope01 = total$s,
                                    coeff_rr = coeff_rr,
                                    bike_mass = ebike_weight)
  total$ebikedownspeed[total$ebikedownspeed < total$ebikeflatspeed] <- total$ebikeflatspeed
  
  #We now have total$ebikeupspeed, total$ebikeflatspeed and total$ebikedownspeed
  #convert from m/s to km/hr for the next bit of the function
  total$ebikeupspeed = total$ebikeupspeed *3.6
  total$ebikeflatspeed = total$ebikeflatspeed *3.6
  total$ebikedownspeed = total$ebikedownspeed * 3.6
  
  #------- Langmuir style corrections for cargo ---- 
    if(cargo > 0){
    total$ebikeupspeed = total$ebikeupspeed * (1- dragSlow  - cargoSlow)
    total$ebikeflatspeed = total$ebikeflatspeed * (1- dragSlow)
    total$ebikedownspeed = total$ebikedownspeed * (1- dragSlow)
  }
  
  
  
  
  
  #print(summary(total$upspeed))
  #print(summary(total$flatspeed))
  #print(summary(total$downspeed))
  #print(summary(total$ebikeupspeed))
  #print(summary(total$ebikeflatspeed))
  #print(summary(total$ebikedownspeed))
  
  
  #if the e-bike speed exceeds 15mph (24.03km/hr)
  #the motor cuts out (UK rules) 
  #then check if human pedal_power is enough to go faster than 
  #15mph
  
  total$ebikeupspeed[total$ebikeupspeed > 24.03 & total$upspeed < 24.03] <- 24.03
  total$ebikeupspeed[total$ebikeupspeed > 24.03 & total$upspeed > 24.03] <- total$upspeed
  
  #NB these control structures might want to be changed later to increase runspeed
  #if ebike speed greater than 24.03
   ##if(total$ebikeupspeed > 24.03){
    #and if upspeed under pedal_power is less than 
    #24.03 then the motor takes the strain
    ##total$ebikeupspeed[upspeed < 24.03] <- 24.03
    #but if the speed under pedal_power is greater than upspeed
    #then the rider powers the bike without assistance.
    ##total$ebikeupspeed[upspeed > 24.03] <- upspeed
  ##}
  
  
  ##if(total$ebikeflatspeed > 24.03){
    total$ebikeflatspeed[total$ebikeflatspeed > 24.03 & total$flatspeed < 24.03] <- 24.03
    total$ebikeflatspeed[total$ebikeflatspeed > 24.03 & total$flatspeed > 24.03] <- total$flatspeed
  ##} 
  ##if(total$ebikedownspeed > 24.03){
    total$ebikedownspeed[total$ebikedownspeed > 24.03 & total$downspeed < 24.03] <- 24.03
    total$ebikedownspeed[total$ebikedownspeed > 24.03 & total$downspeed > 24.03] <- total$downspeed
  ##} 
  #also cap max downspeed at 35km/hr
  total$ebikedownspeed[total$ebikedownspeed > 35] <- 35
  
  
  
  #This accounts for slope profile and circuity
  #and estimates a maximum distance for ebikes. 
  if (s >= 4){ 
    total$ebikekmtime <-  (0.56* 60/ total$ebikeupspeed) + (0.44*60/total$ebikedownspeed)
    total$ebikemaxroaddist <- 60/total$ebikekmtime
  }
  
  if (s < 4 & s >=2) {
    totak$ebikekmtime = (0.504*60/total$ebikeupspeed) + (0.1*60/total$ebikeflatspeed) +(0.396*60/total$ebikedownspeed)
    total$ebikemaxroaddist = 60/total$ebikekmtime  
    
  }
  
  if (s <2) 
    #set kmtime = (0.375*60/total$ebikeupspeed) + (0.25*60/total$ebikeflatspeed) + (0.375*60/total$ebikedownspeed);
    total$ebikekmtime = (0.42*60/total$ebikeupspeed) + (0.25*60/total$ebikeflatspeed) + (0.33*60/total$ebikedownspeed)
    total$ebikemaxroaddist = 60 / total$ebikekmtime
  
  #circuity = 1.4 default
  total$ebikemaxeuclideandist = total$ebikemaxroaddist / circuity
  
  #if powwer is 0 then we have to assume that physically cycling 
  #is not an option
  #even though power  = 0 gives some downspeed and therefore some 
  #capability to travel some distance by bike
  total$ebikemaxeuclideandist[total$Ww <=0] <- 0 
  total$ebikemaxroaddist[total$Ww <=0] <- 0 
  total$maxeuclideandist[total$Ww <=0] <- 0  
  total$maxroaddist[total$Ww <=0] <- 0 
  
  
#-------- return the outputs --------------   
  
  
  if (return_a_vector == TRUE) {
    return(print("this isn't the vector you're looking for"))
    #return(c(upspeed,flatspeed,downspeed,maxroaddist,maxeuclideandist,total$ebikeupspeed,total$ebikeflatspeed,total$ebikedownspeed,ebikemaxeuclideandist,ebikemaxroaddist))
    
  } else { 
    #total <- data.frame(total$personID,total$upspeed,total$flatspeed,total$downspeed,total$maxroaddist,total$maxeuclideandist,total$ebikeupspeed,total$ebikeflatspeed,total$ebikedownspeed,total$ebikemaxeuclideandist,total$ebikemaxroaddist)
    return(total)
  }
  
  #end func defn
  
}



###############################################################
#------------- functions called by functioncycle_dist 
##############################################################

# --------------- speed functions ------------------------- 



speed_iterative <- function (Ww, Vw, m,slope01,coeff_rr,bike_mass = 15){
  
  
  m  = m + bike_mass #mass of bike and rider (I assumed a basic utility bike is 15kg and ebike is 20kg )
  s = sin(atan(slope01))  #slope sin of angle. Looks complicated. 
  #the number in brackets is the input a number between 0and 1 
  #that would give % slope if you multiply by 100
  
  #set an initial guess for 
  #the speed of a bike given the 
  #power input.  
  guess_V = 10
  diff = 999
  
  #Values assumed constant for 
  #a utility cyclist
  Ka = 0.5 * 0.5 * 1.2 * 1.226 # The drag factor 0.5 * frontal area of rider * coefficient of drag * density of air
  g = 9.807 # acceleration due to gravity
  Cr = coeff_rr #coeff of rolling resistance
  
  Kc = 1.5  # convergance coefficient, used to get a good estimate quickly in loop below

  counter <- 0
  while(diff > 0.01 & counter < 200 ) 
  {
    
    V <- guess_V
    
    
    estv <- (Ww / (Ka*(V+Vw)^2 +  (m*g*(s+Cr))  ) + Kc*V) /(Kc+1)
    

    diff <- abs(guess_V - estv)
    
    guess_V <- estv
    counter <- counter + 1
    #print(counter)
    
  }
  #multiply by 3.6 to get from m/s to km/hr
  return(estv) 
  
}
#speed_iterative(Ww,Vw,m,s)

#------------ downspeed downspeednopedalnowind

downspeednopedalnowind <- function(crr,weight,slope, bike_mass = 15){
  
  beet = (0.5* 0.5 * 1.2 * 1.226) / 0.95 #in the website above beet is called Cair
  #crr = 0.008
  bike_and_rider = weight + bike_mass
  #pcslope = slope/100
  
  V2 =( (9.81*bike_and_rider*slope) - (crr * 9.81  * bike_and_rider) ) / beet
  #V2 =( (9.81*weight*pcslope) - (friction  * weight) ) / Cair
  
  V = sqrt(V2)
  #downspeedkm = V * 3.6
  
  return(V)
  
}

#downspeednopedalnowind(crr=0.008, weight = 50,slope = 0.01)


#---- down_speed_iterative#


down_speed_iterative <- function (Ww, Vw, m,slope01,coeff_rr, bike_mass = 15){
  #Assumption If slope >= 4% the speed will be 35km hr or 9.72m/s
  
  #these first if statements dealt with 
  #steeper slopes.  
  # the routines below work well on gradients less than 4%
  #m/s 9.72 = 35km/hr ~20mph 
  #assumed to be fastest an untrained utility cyclist or
  #a "normal person oon a bike"
  #would go downhill
  if(slope01 >= 0.05){
    return(9.72) #35km/hr
    
  }  
  # error spotted by matt 040718 < was wrong > correct
  if(slope01 >= 0.04 & slope01 <0.05){
    #if(slope01 >= 0.04 & slope01 >0.05){
      
    if (m < 60) return(8.33) 
    if (m >= 60) return(9.72)
  }
  
  #run this part of the loop only if slope is less than 4% 
  if(slope01 < 0.04){
    
    m  = m + bike_mass #mass of bike and rider (I assumed a basic utility bike is 15kg and e-bike is 20kg )
    s = sin(atan(slope01))  #slope sin of angle. Looks complicated. 
    #the number in brackets is the input a number between 0and 1 
    #that would give % slope if you multiply by 100
    
    #set an initial guess for 
    #the speed of a bike given the 
    #power input.  
    
    #V2 =( (9.81*bike_and_rider*slope) - (crr * 9.81  * bike_and_rider) ) / beet
    #V2 =( (9.81*weight*pcslope) - (friction  * weight) ) / Cair
    
    #V = sqrt(V2)
    
    initial_guess <- 200
    nopedal <- sqrt( ( (9.81*m*s) - (0.008 * 9.81  * m) ) / 0.3678) # use the downhill nopedal or speed as the initial guess
    #print(paste0("initial guess ", initial_guess))
    guess_V = initial_guess
    diff = 999
    estv <- 0
    
    #Values assumed constant for 
    #a utility cyclist
    Ka = 0.5 * 0.5 * 1.2 * 1.226 # The drag factor 0.5 * frontal area of rider * coefficient of drag * density of air
    g = 9.807 # acceleration due to gravity
    Cr = coeff_rr #coeff of rolling resistance
    
    Kc = 5  # convergance coefficient, used to get a good estimate quickly in loop below
    
    
    outercounter <- 1
    while( (abs(nopedal - estv)) > 0.01 & outercounter < 20 ) 
    { 
      
      Kc = Kc +0.1
      
      
      
      counter <- 0
      while(diff > 0.01 & counter < 30) 
      {
        
        V <- guess_V
        
        
        estv <- (Ww / (Ka*(V+Vw)^2 +  (m*g*(-s+Cr))  ) + Kc*V) /(Kc+1)
        #print(paste0("estv after est before diff", estv))
        
        diff <- abs(V - estv)
        
        guess_V <- estv
        #print(paste0("iteration ",counter, " estv ", estv))
        
        counter <- counter + 1
        #print(counter)
        #end inner counter while loop  
      }
      
      outercounter <- outercounter +1  
      
      #print(paste0("KC is ", Kc))
      #end outer counter while loop  
    }   
    
    
    
    counter <- 0
    while(diff > 0.01 & counter < 100) 
    {
      
      V <- guess_V
      
      
      estv <- (Ww / (Ka*(V+Vw)^2 +  (m*g*(-s+Cr))  ) + Kc*V) /(Kc+1)
      #print(paste0("estv after est before diff", estv))
      
      diff <- abs(V - estv)
      
      guess_V <- estv
      #print(paste0("iteration ",counter, " estv ", estv))
      
      counter <- counter + 1
      #print(counter)
      
    }
    

    
    #result is m/s
    return(estv) 
    
    #end of else block
  }
  
  
}

#down_speed_iterative(Ww = 10,m = 35,Vw = 0, slope01 = 0.03)






