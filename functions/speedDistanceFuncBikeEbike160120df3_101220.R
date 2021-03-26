#' Maximum cycle distance function
#'
#'
#'This calculates the maximum distance a person can travel by bike or e-bike
#' It returns the dataframe with the distances and other variables.
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
#' 
#' 
#' @examples
#' \dontrun{
#'functioncycle_dist(data = NULL,
#'personID = 1,
#'eW = 250,
#'assistpc = 300,
#'Ww = 1,
#'Vw = 0,
#'m = 70,
#'bicycle_weight = 15, 
#'ebike_weight = 20,
#'s = 4,
#'circuity = 1.4,
#'coeff_rr = 0.008,
#'cargo = 15,
#'dragSlow = 0.08,
#'cargoSlow = 0.2,
#'return_a_vector = TRUE
#')
#'}



functioncycle_dist <- function (data = NULL,
                                personID = "personID",
                                eW = 250,
                                assistpc = 300,
                                Ww = "pedal_power",
                                Vw = "windspeed",
                                m = "weight",
                                bicycle_weight = 15, 
                                ebike_weight = 20,
                                s = "slope_percent",
                                circuity = 1.4,
                                coeff_rr = 0.008,
                                cargo = 15,
                                dragSlow = 0,
                                cargoSlow = 0,
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
  
  #In the data slope is a number 1-100 percent. Here the value should be between 0 and 1
  total$s <- total$s/100
  coesff_rr <- coeff_rr

  
  #--------- calculations for a normal bicycle -----------  
  
  
    #amend the bicycle weight variable to include any cargo carried.  
  bicycle_weight <- bicycle_weight + cargo
  
    
  total$upspeed <- speed_iterative(total$Ww,total$Vw,total$m,total$s,coeff_rr = coeff_rr,
                                   bike_mass = bicycle_weight) #Use s as it is for going up
  total$flatspeed <- speed_iterative(total$Ww,total$Vw,total$m,0, coeff_rr = coeff_rr,
                                     bike_mass = bicycle_weight) #s =0 when flat
  
  
  #if slope is close to 0 then when down_speed_iterative is 
  #called then it will return flatspeed 
  
  total$downspeed <- down_speed_iterative(Ww = total$Ww,
                                          m = total$m,
                                          Vw = total$Vw,
                                          slope01 = total$s,
                                          coeff_rr = coeff_rr,
                                          bike_mass = bicycle_weight,
                                          flatspeed = total$flatspeed)
  
  
  #sometimes going downhill on shallow slopes without pedalling
  #is slower than riding on the flat due to rolling resistance
  total$downspeed[total$downspeed < total$flatspeed] <- total$flatspeed
  
  
  
  #We now have upspeed, flatspeed and downspeed
  #convert from m/s to km/hr for the next bit of the function
  total$upspeed = total$upspeed *3.6
  total$flatspeed = total$flatspeed *3.6
  total$downspeed = total$downspeed * 3.6
  
  #to ensure that ordinary bikes going doen hill aren't going faster than 35kmhr
  total$downspeed[total$downspeed > 35] <- 35
  
  #total <- total %>% mutate(downspeed = if_else(downspeed > 35,35,downspeed))
  
  
   
  #------- Langmuir style corrections for cargo on a normal bike ---- 
  #To get a more conservative estimate, extra constraints can be added. 
  
  # Drag:  estimate 8% loss in speed due to panniers etc increasing drag. 
  #This is an estimate from cycle forums rather than published papers / texts 
  
  # "Langmuirs corrections" when walking with a heavy backpack instead of unladen 
  # Naismith estimated an acverage walking speed of 5km/hr.  
  #Langmuir corrected the base speed to 4km/hr a 20% reduction because of extra 
  #muscular effort and the effect of the weight of the load.  
  #we could apply this to a cyclist, but
  #1) we've already accounted for the extra weight 
  #2) loads may be carried on the bike so the muscular strain might be less than 
  # for a walker carrying a backback
  
  #penalties for drag and langmuir effect are really just an estimate for sensitivity purposes 
  #In this calculation, there is   
  #no cargo speed penalty downhill or on flat on a bike or e-bike
  
  if(cargo > 0){
    total$upspeed = total$upspeed * (1- dragSlow  - cargoSlow)
    total$flatspeed = total$flatspeed * (1- dragSlow)
    total$downspeed = total$downspeed * (1- dragSlow) 
    }

  
  #---account for slope profile and circuity and estimates a maximum distance.-- 
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
  
 
  # ------------- calculations for an e-bike -----------
  
  
  #-include e-bike speed adjustments to account for varied assistance -
  
  # CHNGES MADE TO THIS BLOCK ON 101220
  #An e-bike motor provides upto 250w steady power.  
  #In previous model runs the maximum output power is capped at 250 total$assistpower
  #BUT If the motor is at 250W and the person can add further pedal power and
  #the bike is going less than 25km/hr then the power moving the bike forward could be higher 
  #This case will only happen going up hill.  
  
  #the power with assistance is as follows if the total 
  #power from motor and rider is capped at 250W
  #the pedal_power * the % assistance #nb assist /100 to get assist ratio
  total$assistpower <- total$Ww * (assistpc/100)
  #assistpower is capped at 250W in the UK and EU 
  total$assistpower[total$assistpower > eW] <- eW
  
  
  total$ebikeupspeed <- speed_iterative(total$assistpower,total$Vw,
                                        total$m,total$s,coeff_rr = coeff_rr,
                                        bike_mass = ebike_weight) #Use s as it is for going up
  
  
  # --- changes on 101220
  # The power with assistance is limited 250w motor power plus the rider power
  
  #total$Ww >84 #84* 3 = 250 .*84w is minimum pedal power to get 250w from the motor
  #If the rider can give additional power and the bike is going slower than 24km hr
  #this will increase the speed uphill
  if(total$Ww > 84 & total$ebikeupspeed < 24 &total$assistpower == eW){
  total$max_motor_and_rider_power <- eW + (total$Ww - 84)
  total$ebikeupspeed_motor_and_rider <- speed_iterative(total$max_motor_and_rider_power,total$Vw,
                                        total$m,total$s,coeff_rr = coeff_rr,
                                        bike_mass = ebike_weight) #Use s as it is for going up
  
  }
  
  total$ebikeupspeed[total$ebikeupspeed_motor_and_rider > total$ebikeupspeed] <- total$ebikeupspeed_motor_and_rider
  
  
  total$ebikeflatspeed <- speed_iterative(total$assistpower,total$Vw,total$m,0, 
                                          coeff_rr = coeff_rr,
                                          bike_mass = ebike_weight) #s =0 when flat
  
  
  
  #if slope is close to 0 then when down_speed_iterative is 
  #called, it will return flatspeed
  
  total$ebikedownspeed <- down_speed_iterative(Ww = total$assistpower,
                                              m = total$m,
                                              Vw = total$Vw,
                                              slope01 = total$s,
                                              coeff_rr = coeff_rr,
                                              bike_mass = ebike_weight,
                                              total$ebikeflatspeed)
  
  
  total$ebikedownspeed[total$ebikedownspeed < total$ebikeflatspeed] <- total$ebikeflatspeed
  
  #We now have total$ebikeupspeed, total$ebikeflatspeed and total$ebikedownspeed
  #convert from m/s to km/hr for the next bit of the function
  total$ebikeupspeed = total$ebikeupspeed *3.6
  total$ebikeflatspeed = total$ebikeflatspeed *3.6
  total$ebikedownspeed = total$ebikedownspeed * 3.6
  
  #------- Langmuir style corrections for cargo with an e-bike ---- 
  #see description of this above in relation to a normal bike
    if(cargo > 0){
    total$ebikeupspeed = total$ebikeupspeed * (1- dragSlow  - cargoSlow)
    total$ebikeflatspeed = total$ebikeflatspeed * (1- dragSlow)
    total$ebikedownspeed = total$ebikedownspeed * (1- dragSlow)
  }
  
  #if the e-bike speed exceeds 15mph (24.03km/hr)
  #the motor cuts out (UK rules) 
  #then check if human pedal_power is enough to go faster than 
  #15mph
  
  total$ebikeupspeed[total$ebikeupspeed > 24.03 & total$upspeed < 24.03] <- 24.03
  total$ebikeupspeed[total$ebikeupspeed > 24.03 & total$upspeed > 24.03] <- total$upspeed
  
  #NB these control structures might want to be changed if you want to examine speed pedalecs
 
  #This accounts for the fact that on the flat some riders have sufficent power Ww to 
  #ride above 24km/hr
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
  
  
  
  #---------account for slope profile ----------
   
  if (s >= 4){ 
    total$ebikekmtime <-  (0.56* 60/ total$ebikeupspeed) + (0.44*60/total$ebikedownspeed)
    total$ebikemaxroaddist <- 60/total$ebikekmtime
  }
  
  if (s < 4 & s >=2) {
    total$ebikekmtime = (0.504*60/total$ebikeupspeed) + (0.1*60/total$ebikeflatspeed) +(0.396*60/total$ebikedownspeed)
    total$ebikemaxroaddist = 60/total$ebikekmtime  
    
  }
  
  if (s <2) 
    #set kmtime = (0.375*60/total$ebikeupspeed) + (0.25*60/total$ebikeflatspeed) + (0.375*60/total$ebikedownspeed);
    total$ebikekmtime = (0.42*60/total$ebikeupspeed) + (0.25*60/total$ebikeflatspeed) + (0.33*60/total$ebikedownspeed)
    total$ebikemaxroaddist = 60 / total$ebikekmtime
  
  #--------- make a separate indicator which accounts for circuity -------------------- 
  #circuity = 1.4 default
  #circuity is an estimate of how much a route deviates from the 
  #euclidean distance
  #It can be a useful indicator to make a simple service area or to make comparisons
  #with other measures which are based on euclidean rather than route distances.  
    
    
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
    return(print("Vector return not working at present"))
    #return(c(upspeed,flatspeed,downspeed,maxroaddist,maxeuclideandist,total$ebikeupspeed,total$ebikeflatspeed,total$ebikedownspeed,ebikemaxeuclideandist,ebikemaxroaddist))
    
  } else { 
    total <- data.frame(personID = total$personID,
                        upspeed = total$upspeed,
                        flatspeed = total$flatspeed,
                        downspeed = total$downspeed,
                        maxroaddist = total$maxroaddist,
                        maxeuclideandist = total$maxeuclideandist,
                        ebikeupspeed = total$ebikeupspeed,
                        ebikeflatspeed = total$ebikeflatspeed,
                        ebikedownspeed = total$ebikedownspeed,
                        ebikemaxeuclideandist = total$ebikemaxeuclideandist,
                        ebikemaxroaddist = total$ebikemaxroaddist)
    return(total)
  }
  
  #end func defn
  
}



###############################################################
#------------- functions called by functioncycle_dist 
##############################################################

# --------------- speed functions ------------------------- 

#' speed iterative
#'
#'
#' This calculates the a person can travel by bike or e-bike when slope is 
#' positive. It returns the vector of speeds (m/s).  
#' Method is based in Wilson 2004, Bicycling Science pp138, 
#' ISBN 978-0-262-73154-6
#'
#' 
#' @param Ww rider pedal power in Watts
#' @param Vw the mean windspeed experienced by rider
#' @param m rider weight kg
#' @param slope01 slope percent divided by 100
#' @param coeff_rr coefficient of rolling resistance
#' @param bike_mass default = 15, bike weight plus any cargo kg



speed_iterative <- function (Ww, Vw, m,slope01,coeff_rr,bike_mass = 15){
  
  
  m  = m + bike_mass #mass of bike and rider 
  #(I assumed a basic utility bike is 15kg and ebike is 20kg )
  s = sin(atan(slope01))  #slope sin of angle. Looks complicated. 
  #the number in brackets is the input, a number between 0and 1 
  #that would give % slope if you multiply by 100
  
  #set an initial guess for 
  #the speed of a bike given the 
  #power input.  
  guess_V = 10
  diff = 999
  
  #Values assumed constant for 
  #a utility cyclist
  Ka = 0.5 * 0.5 * 1.2 * 1.226 
  # The drag factor 0.5 * frontal area of rider * coefficient of drag * density of air
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

#' downspeed no pedal no wind
#'
#'
#' This calculates the speed a person can travel by bike or e-bike when slope is 
#' negative and they are note pedalling and there is no wind.
#' It returns the vector of speeds.  
#'
#' @param crr coefficient of rolling resistance
#' @param weight rider weight kg
#' @param slope slope percent divided by 100
#' @param bike_mass default = 15, bike weight plus any cargo kg


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


#---- down_speed_iterative#

#' down speed iterative
#'
#'
#' This calculates the speed a person can travel by bike or e-bike downhill
#' whilst pedalling. It returns a vector of speeds (m/s).  
#' 
#' @param Ww rider pedal power in Watts
#' @param Vw the mean windspeed experienced by rider
#' @param m rider weight kg
#' @param slope01 slope percent divided by 100
#' @param coeff_rr coefficient of rolling resistance
#' @param bike_mass default = 15, bike weight plus any cargo kg
#' @param flatspeed default = NA, speed of the rider on the flat


down_speed_iterative <- function (Ww, Vw, 
                                  m,slope01,coeff_rr, 
                                  bike_mass,flatspeed = NA){
  
  
  #return 0 if slope is less than 1%
  if(slope01 < 0.01){
    return(flatspeed) # if slope is less than 1% then code below will throw an error
    
  }
  
  
  #Assumption If slope >= 4% the downhill speed will be 35km hr or 9.72m/s
  #the model is about non-expert cyclists making utility trips
  #so this conservative estimate is used. 
  # 9.72 m/s = 35km/hr ~20mph 
  #assumed to be fastest an untrained utility cyclist or
  #a "normal person oon a bike"
  #would go downhill
  
  
  #------  deal with steeper slopes -------.  
  # the main algorithm below works best on gradients less than 4%
  if(slope01 >= 0.05){
    return(9.72) #35km/hr
    
  }  
  # error spotted by matt 040718 < was wrong > correct
  if(slope01 >= 0.04 & slope01 <0.05){
    #if(slope01 >= 0.04 & slope01 >0.05){
      
    if (m < 60) return(8.33) 
    if (m >= 60) return(9.72)
  }
  
  #-----run this part of the loop only if slope is less than 4% -----
  if(slope01 < 0.04){
    
    m  = m + bike_mass 
    #mass of bike and rider (I assumed a basic utility bike is 15kg and e-bike is 20kg )
    s = sin(atan(slope01))  
    #slope sin of angle. Looks complicated. 
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
    
  #end if slope is < 4%
  }
  
#end function  
}

