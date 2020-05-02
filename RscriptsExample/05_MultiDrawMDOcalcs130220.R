
# this script is called several times by 04_senstest
#once for each Monte-Carlo sampling draw.  



print(paste0("#########################",seed_number,"########"))


print("#--- read results of script 03, max dist by active mode in 1 hour--")
#In script 03 we estimated the physical capability to 
#travel by walking cycling or e-bike for one hour.  
#Below we read in the results of script 03 as MDO1

MDO1 <- fread(paste0("MDOfiles/MDO1_",datetime, ".csv"), 
              drop = c("upspeed","flatspeed","downspeed",
                       "ebikeupspeed","ebikeflatspeed","ebikedownspeed",
                       "kmtime","ebikekmtime",
                       "Ww","Vw","m","s",
                       "maxeuclideandist","ebikemaxeuclideandist"))
gc()
#convert MDO1 from data.table format to tibble 
#so dplyr functions will work on it 
MDO1 <- as_tibble(MDO1)
#glimpse(MDO1)
gc()


print("#---- read in cumulative probability of car trip lengths  -----------")

#These contain the proportion car trips recorded in the NTS travel diary 
#in 1km bins upto 35km
# by population subgroup (sex age and OAC supergroup n=128 subgroups)
#there is a file for AM car trips and one for PM car trips
subpopAM <- read_csv("input_data/AMsubpopgroupvarsSexageOACbikeown040919.csv")
subpopPM <- read_csv("input_data/PMsubpopgroupvarsSexageOACbikeown040919.csv")


#Physiologically, it is reasonable to assume that 
#a person is physically capable of travelling the equivalent of
#2 hours during the day
#by walkaing cycling / ebiking,
#particularly when it is likely that they will have
#a rest from walking / cycling / e-biking in between

#To estimate this I look at  car journey km in the AM which could be replaced,
#and separately car journey km in the PM which could be replaced.  
#to get estimates of the proportion of car km which could be replaced 
# by a person capable of travelling a given distance km by active modes.
#I used 2014 individual 
#anonymised records from the NTS.  the scripts used to get the subpop
#proportions are: 

#probtriplengthNTS040919AMv2.R and pprobtriplengthNTS040919AMv2.R
#they require the NTS individual data to run.  


print("#---------------------check summaries -------------------") 
#names(MDO1)
#summary(MDO1)
gc()
print("#-------- read in OAC  -----------")
OAC <- read_csv("input_data/2011OACsupergroups.csv")

#------ Join OAC to MDO1 --------
#names(OAC)
#names(MDO1)
MDO1 <- left_join(MDO1,OAC,by=c("ZoneID" = "OA11CD"))
gc()


print("#------ Assign annual car km travelled to individuals ----")
# The next code chunks: 

#Join MDO to a weighting factor of annual miles pp 
#by gender age and OAC

#join MDO to data on Vehicle km travelled by OA

#sample which individuals have no access to a car

#

print("#--- Join MDO to a weighting factor of annual miles pp by gender age and OAC ----") 

#this gives a weighting to each sex-age-oac segment of the population
#to account for different population segments living in the same
#area travelling different amounts. 
#this is derived from the 2016 NTS individual anonymous records from UKDS.  

#names(subpopAM)
#make a df which will hold the result of vehicle miles travelled weighting
vmt_weight <- subpopAM %>% select(SexAgeconst, HHoldOAClass2011_B03ID,sumweeklytripdist_mean)

#add together the am and pm mean weekly travel distance from NTS travel diary
vmt_weight$sumweeklytripdist_mean <- subpopPM$sumweeklytripdist_mean + vmt_weight$sumweeklytripdist_mean

#bar plot to see differences
# p1 <- ggplot( vmt_weight, aes( x = HHoldOAClass2011_B03ID, y = sumweeklytripdist_mean ) ) + 
#   geom_bar( stat = "identity" ) + 
#   facet_wrap( ~  SexAgeconst) + 
#   xlab("OAC supergroup")
# p1
#ggsave(filename = "vmt_weights_sexage_oac",device = "png")

gc()
#calculate the mean weekly trip distance for all supgroups of the population
meanofmeankmpp <- mean(vmt_weight$sumweeklytripdist_mean)
#to get vmt_weight as a proportion for each subgroup
#calculate ratio weekly trip distance for group x / weekly trip distance for all groups
vmt_weight$vmt_proportion <- vmt_weight$sumweeklytripdist_mean / meanofmeankmpp 

# names(vmt_weight)
# p1 <- ggplot( vmt_weight, aes( x = HHoldOAClass2011_B03ID, y = vmt_proportion ) ) + 
#   geom_bar( stat = "identity" ) + 
#   facet_wrap( ~  SexAgeconst) + 
#   xlab("OAC supergroup")
# p1
#ggsave(filename = "vmt_weights_sexage_oac_vmt_propn",device = "png")

#names(MDO1)
#names(vmt_weight)

gc()

#Join the vmt_weightings to each individual 
MDO1 <- left_join(MDO1,vmt_weight, by = c("sexageconst" = "SexAgeconst", "Supergroup_Code" = "HHoldOAClass2011_B03ID"))
#names(MDO1)
gc()  

print("#------------------------- join MDO to data on Vehicle km travelled by OA ---------")

#We use the km per person estimate to assign travel to simulted individuals.  
#The simulated individuals are those who are in a position to alter their emissions
#Children are not simulated, but, we assume chaildren are not making mode choice decisions.  

#MOT data is an estimate of the mean car km per person per LSOA, 
#and this has been allocated to OAs
MOTdata <- read_csv("input_data/super_abridged_lookup2014miles_ppOA.csv") #sum km 247bmkm priv cars
gc()
#glimpse(MOTdata)
#names(MDO1)
MDO1 <- left_join(MDO1,MOTdata,by=c("ZoneID" = "OA11CD"))
gc()


print("#---------------- sample which individuals have no access to a car --------")
#read in an LSOA estimate of % of people with no access to a car
#source is census taken from MOT project contextualising dataset and 
#uses data from the 2011 census
nocar <- read_csv("input_data/noCarPr2011LSOA.csv")
#names(nocar)
#names(MDO1)
#join to link each individual to a probability of having no car
MDO1 <- left_join(MDO1,nocar,by=c("LSOA11CD"= "LSOA11CD"))
#draw the possibility of having no acess to a car
MDO1$draw_NoCar <- runif(length(MDO1$NOCAR_pr),0,1) 
#assign whether person has access to a car
MDO1 <- MDO1 %>% mutate(hasCar = ifelse(test = draw_NoCar < NOCAR_pr,
                                        yes = 0,
                                        no = 1)
)

summary(MDO1$hasCar) #NA155133

#assume any NA has access to a car 
MDO1 <- MDO1 %>% mutate(hasCar = ifelse(test = is.na(hasCar),
                                        yes = 1,
                                        no = hasCar)
)
summary(MDO1$hasCar)

#check that number of people with car is sensible
sum(MDO1$hasCar)
sum(MDO1$hasCar) / nrow(MDO1)

#This is close to the mean % of LSOA residents with no car
mean(nocar$NOCAR_pr)
##[1] 0.1930372 # All england data
#This suggests a reasonable fit at national level

print("#-------- assign miles pp to each person with a car ----")
#names(MDO1)

# recalculate the km per person per OA 
#but divide total km per OA / the number with access to a car
MDO2 <- MDO1 %>% select(ZoneID, totprivkmOA,hasCar,privkm_pp)
groups <- group_by(MDO2,ZoneID)
gc()

#make a df of km per person (whole population) and include number of people with a car
test_cardraw <- dplyr::summarize(groups,adultsimOA = n(),
                                 totprivkm = mean(totprivkmOA),
                                 privkm_pp = mean(privkm_pp),
                                 hasCar = sum(hasCar)
)

summary(test_cardraw$hasCar)
#kmsimpp recalculate the km per simulated person accounting for 
#if they have access to a car or not
test_cardraw <- test_cardraw %>% 
  mutate(kmsimpp = ifelse(test = hasCar > 0,
                          yes =  totprivkm / hasCar,
                          no = NA
  )
  )

test_cardraw <- test_cardraw %>% 
  mutate(kmsimpp = totprivkm / hasCar)
summary(test_cardraw$hasCar)

#-check for particularly high values --
summary(test_cardraw$kmsimpp)
#hist(test_cardraw$kmsimpp)
#there are 517 OAs where the mean kksimpp is over 
#18000km or 12 miles pp pa.  
filtered <- test_cardraw %>% filter(kmsimpp > 18000)
filtered <- test_cardraw %>% filter(privkm_pp < 9000)
#plot(filtered$privkm_pp,filtered$kmsimpp)
cor(filtered$privkm_pp,filtered$kmsimpp)

#plot(filtered$totprivkm,filtered$kmsimpp)
#cor(filtered$totprivkm,filtered$kmsimpp)

#names(test_cardraw)
test_cardraw <- test_cardraw[,c(1,6)]
gc()


# join the recalculated car km per person per zone in test_cardraw 
#to MDO1
MDO1 <- left_join(MDO1,test_cardraw,by=c("ZoneID" = "ZoneID")) 
#names(MDO1)

#tidy up 
rm(groups)
rm(MDO2)
gc()      

print("#-----check if sum kmppsim is close to total (221bn km in MDO1) 247bn in MOTdata")
summary(MDO1$kmsimpp)
sum(MDO1$kmsimpp) #256bnkm 

MDO1 <- MDO1 %>% mutate(
  weightedkm_pp = ifelse(test = hasCar == 1,
                         yes = kmsimpp,
                         no = 0)
)
summary(MDO1$weightedkm_pp)
sum(MDO1$weightedkm_pp) #now matches 

#names(MDO1)

print("#----------- assign weighted car km per person ---------")
gc()
#if person has access to car weight vkm by demographics
#if no car then carkm = 0
MDO1 <- MDO1 %>% mutate(
  weightedkm_pp = ifelse(test = hasCar == 1,
                         yes = vmt_proportion * weightedkm_pp,
                         no = 0)
)

#check whether this is a close match: 
sum(MDO1$weightedkm_pp,na.rm = T) / 1000000000 # 247bn
sum(MOTdata$totprivkmOA)
#247172138224
sum(MOTdata$totprivkmOA) /sum(MDO1$weightedkm_pp)
#0.9975023 # so yes it is a close match at national level


print("#---- calibration of car km per person accounting for car availability-------") 


#---- assess the error within small areas OA -----
gc()
#make MDO2 and groups again 
MDO2 <- MDO1 %>% select(ZoneID, totprivkmOA,weightedkm_pp)
groups <- group_by(MDO2,ZoneID)
gc()
test_cardraw <- dplyr::summarize(groups,adultsimOA = n(),
                                 totprivkm = mean(totprivkmOA,na.rm = T),
                                 sumweightedkm_pp = sum(weightedkm_pp,na.rm = T)
)

#names(test_cardraw)                                   
rm(groups)
rm(MDO2)
gc()
summary(test_cardraw$sumweightedkm_pp)
#names(MOTdata)
#compare the MOT data (best estimate of total km per small area)
#with the weighted estimates used in this simulation
lsoa_test <- left_join(MOTdata,test_cardraw,by = c("OA11CD" = "ZoneID"))
#names(lsoa_test)


sum(lsoa_test$sumweightedkm_pp,na.rm = T) #247bnkm
lsoa_test <- lsoa_test %>% filter(!is.na(sumweightedkm_pp))
cor(lsoa_test$sumweightedkm_pp,lsoa_test$totprivkm)
#correlation is 0.94

#shows the relationship seems just off linear with lever of high outliers
# ggplot(lsoa_test, aes(x=sumweightedkm_pp, y=totprivkm) ) +
#   geom_bin2d() +
#   geom_hex(bins = 70) +
#   scale_fill_continuous(type = "viridis") +
#   geom_smooth(method='lm', formula = y~x)+
#   theme_bw()
#ggsave("lsoa_test_totkmOAsumweightedkm.png")  


#There is very close correspondence in the sums.  
sum(MOTdata$totprivkmOA)
#1] 247172138224

sum(lsoa_test$sumweightedkm_pp)
#247790839859

#The simulated population
#e.g. those who might have agency to reduce their car use (adults)
#TRA01 https://www.gov.uk/government/statistical-data-sets/road-traffic-statistics-tra
#estimates there were a total of 245bn car miles driven in 2014 (392bnkm in the whole uk) .  
#the corporate car fleet is ~9% but does higher miles per car.  

#calculate the demographically weighted km_pp without assuming 
#some people dont have acess to a car
MDO1$demogweighted <- MDO1$privkm_pp * MDO1$vmt_proportion
sum(MDO1$demogweighted)
#198bnkm
#write_csv(lsoa_test,paste0("Results",datetime,"/","lsoa_test",seed_number,".csv"))


print("#---------------- prepare lookup of bike availabilty  -----------------") 
#prepare bikelookup 
#Here I'm making a lookup which has for each comnbination of 
#sexage and OAC a proportion of that subgroup who own a bike.  
#Data for this is based on NTS individual anonymous records 2016.  
#names(subpopAM)
bikelookup <- subpopAM %>% select(SexAgeconst = SexAgeconst, 
                                  OAC = HHoldOAClass2011_B03ID,
                                  HasBike_mean = HasBike_mean)
MDO1 <- left_join(MDO1,bikelookup,by =c("sexageconst" = "SexAgeconst",
                                        "Supergroup_Code" = "OAC"))

gc()


#---------- tidy --------
rm(OAC)
rm(MOTdata)
gc()

print("#----subset the variables in MDO1 so that there is not too much data in memory ---") 
#names(MDO1)

MDOVarsKeep <- c("personID","weightedkm_pp","maxroaddist",
                 "walkspeedkmh","HasBike_mean","co2EF" ,
                 #"emfac_co2_av_Y",
                 "Supergroup_Code","sexageconst",
                 "ebikemaxroaddist","sexw","age",
                 "hasCar","demogweighted")

MDO1 <- MDO1 %>% select(MDOVarsKeep)
gc()
#filter any missing values from MDO1
MDO1 <- MDO1 %>% filter(weightedkm_pp >= 0)
gc()
#names(MDO1)
MDO1 <- MDO1 %>% rename(emfac_co2_av_Y = co2EF) #For renaming tibble column using dplyrpipe 
#operator 
#change the name of emfac so it works in the functions 


print("#---------- use functions to calculate MDD and km savings --------- ")

#read in functions
source("functions/function_MDD_savings290819.R")
source("functions/functionDODrawDistandSavings040919.R")
source("functions/functionDODrawDistandSavingsALLBIKE040919.R")
gc()

print("#------- draw a value to compare against probability of having a bike ----------") 

MDO1$draw_bike <- runif(length(MDO1$HasBike_mean),0,1) 
#names(MDO1)

#run multiple scenarios (maybe scenario isn't the best name)
print("########---- Scenario 1 current walking and cycling no e-bikes ------")

#ptm <- proc.time()
returned <- functionDODrawDistSavings040919()
MDO1$MDD <- returned$roundedMDD
MDO1$MDDkmreplace <- returned$replaceabledistance
MDO1$MDDco2save <- returned$co2save

rm(returned)
#proc.time() - ptm 
gc()

print("######## Scenario 2  everyone has a bike --------------")

returned <- functionDODrawDistSavingsALLBIKE040919() 
MDO1$AllBikekmreplace <- returned$replaceabledistance
MDO1$Allbikeco2save <- returned$co2save
rm(returned)
gc()

print("######## Scenario 3  current bike owners have an e-bike ----") 

returned <- functionDODrawDistSavings040919(roaddist = MDO1$ebikemaxroaddist) #need to keep an e-bike column 
MDO1$currentEBike <- returned$drawMDD
MDO1$currentEBikekmreplace <- returned$replaceabledistance
MDO1$currentEbikeco2save <- returned$co2save
rm(returned)
gc()


print("######## Scenario 4 everyone has  an e-bike ----") 
returned <- functionDODrawDistSavingsALLBIKE040919(roaddist = MDO1$ebikemaxroaddist)
MDO1$AllEBikekmreplace <- returned$replaceabledistance
MDO1$AllEbikeco2save <- returned$co2save
rm(returned)
gc()


print("####### Scenario 5 Cairns 20% km replaced with an e-bike ---")  
#Cairns et al
#https://www.sciencedirect.com/science/article/pii/S0965856415301865
#carried out a study with an intervention which 
#resulted in study participants replacing 20% of their car km 
#with e-bike use.  
#2/3 live within 5 miles of work where the scheme is based
#Brighton urban extent is about 5 miles from seafront to city edge.  
#there are other towns within 10 miles (92 % lived within 10 miles of work)
#it seems a reasonable assumption that virtually none of the
#people in the study live in rural areas
#Also, almost all the participants (95%) were under 60.   
#

MDO1$cairns<- 0
#Supergroup 1 is rural
MDO1$cairns[MDO1$Supergroup_Code != 1 & MDO1$age < 60 ]<-  0.2 * MDO1$weightedkm_pp
#MDO1$cairns[MDO1$age < 60 ]<-  0.2 * MDO1$weightedkm_pp
summary(MDO1$cairns)
MDO1$cairnsco2save <- MDO1$cairns * MDO1$emfac_co2_av_Y
summary(MDO1$cairnsco2save)

print("####### Scenario 6 Just walking --------")
#The most sustainable option 

returned <- functionDODrawDistSavingsALLBIKE040919(roaddist = MDO1$walkspeedkmh)
MDO1$Walkkmreplace <- returned$replaceabledistance
MDO1$Walkco2save <- returned$co2save
rm(returned)
gc()

print("########Scenario 7 walkers get e-bikes cyclists ride their bikes")
# 
# #If in MDD the person is deemed not to have a bike, then give them an 
# #ebike
MDO1 <- MDO1 %>% mutate(walktoebikekmreplace = ifelse(test = draw_bike <= HasBike_mean, 
                                                      yes = AllBikekmreplace,
                                                      no = AllEBikekmreplace
)
)


#estimate the co2saving using the emissions factor
MDO1$walktoebikeco2save <-  MDO1$walktoebikekmreplace * MDO1$emfac_co2_av_Y
#MDO1k <- MDO1[1:1000,]
gc()



print("#------------ scenario e-bikes 8 -30 minute neighbourhood ------")
#people are willing to travel upto 30 minutes in the morning
#and again in the evening
#for justification see literature on 'the 30 minute city'
#Cairns et al 2017 found the median time spent using an e-bike 
#amongst their participants is 60 minutes per day.
#the marchetti constant is also a well known hypothesis 
#that people are willing to commute 30 mins each way per day.  


#In this scenario we halve the distance people can travel am and pm 
#by halving the maxroad distance because we halve the time.  
returned <- functionDODrawDistSavingsALLBIKE040919(roaddist = MDO1$ebikemaxroaddist /2)
MDO1$AllEBikekmreplace30m <- returned$replaceabledistance
MDO1$AllEbikeco2save30m <- returned$co2save
rm(returned)
gc()


print("#-------------scenario 9 ebikes - flex mobility ---- ")
#people choose not to use active modes every day, 
#on average they use their e-bikes 1.88 days per week (0.26915 of the time)
#this is the mean of the distributaion of usage in the study by Cairns et al 2017
#see their figure 1.


MDO1$AllEBikekmreplaceFlex <- MDO1$AllEBikekmreplace * 0.26915
MDO1$AllEbikeco2saveFlex <- MDO1$AllEbikeco2save * 0.26915
gc()


print("#------------scenario 10 ebikes  - flex mobility and 30 minute neighbourhood")

MDO1$AllEBikekmreplace30mFlex <-  MDO1$AllEBikekmreplace30m * 0.26915
MDO1$AllEbikeco2save30mFlex <- MDO1$AllEbikeco2save30m * 0.26915
gc()





print("#-- accounting for emissions of building e-bikes and riding bikes ebikes and walking-")

#ECF (2011) estimate lifecycle values
#source: 
#https://ecf.com/sites/ecf.com/files/ECF_CO2_WEB.pdf (See p9-11)
#with e-bikes having lower energy use emissions
#because they consider the production emissions of food used by 
#bike riders vs the renewable electricity production of e-bike users.  
#Berners Lee (There is no Planet B) also points this out.  
ECFbikeLifecyle <- 21 #g/gm
ECFebikeLifecycle <- 22 #g/km
#we will use this with MDD because no new bikes are bought here
#we assum just continued use of current bike fleet
ECFbikewelltoWheel <- 21 - 5 #16g/km 
#names(MDO1)

#Alternative values
#Weiss et al 2015 estimate an al;ternative set of values
#http://dx.doi.org/10.1016/j.trd.2015.09.007
#tables A1 and A2 in Annexe A.  
#Weiss et al provide a central estimate and an upper and lower value.  

#curran et al 2014 
#https://www.sciencedirect.com/science/article/pii/S0360544214008573 
#have a nice diagram explaining what well to wheel is

#these values use Weiss central estimate but they are not 
#used in the simulation.   I could adapt the code to estimate 
#several different walk, bike and e-bike emissions values
WeissWellWheelBike <- 0 # no food emissions associated with this
WeissWellWheelEbike <- 1.5 * 1000 / 100 #12g/km
#Emissions associated with production use and disposal
WeisslifeCycleBike <- 0.5 *1000 / 100 #5g/km same value 
#as ECF for production and maint.  
WeisslifeCycleEBike <- 2.5 *1000 / 100 #25 g/km 

#Weiss et al have a production emissions value of 13g/km.  ECF 
#estimated this at 7g/km

#below the total gross co2saved minus the emissions 
#assiciated with all the saved km
#for MDD it is well to wheel because the bikes are already available
#and don't need making
gc()
#Berners-Lee in there is no planet B estimates the calories per km of
#walking is approx double that of cycling
#below yes is has bike, no is walk
MDO1 <- MDO1 %>% 
  mutate(MDDco2save = 
           ifelse(test = draw_bike <= HasBike_mean, 
                  yes = MDDco2save - (ECFbikewelltoWheel * MDO1$MDDkmreplace),
                  no = MDDco2save - (2 * ECFbikewelltoWheel *  MDO1$MDDkmreplace)
           )
  )
#summary(MDO1$MDDco2save)

#ggplot(MDO1)+
#  geom_histogram(aes(MDDco2save))
#ggsave("MDDco2Save121119.png")
gc()
#If we give everyone an ebike then we 
#have to produce them so we have to have a value for e-bike lifecycle
#including production
MDO1$AllEbikeco2save <- MDO1$AllEbikeco2save - (ECFebikeLifecycle * MDO1$AllEBikekmreplace)
#summary(MDO1$AllBikeco2save) 
MDO1$Allbikeco2save <- MDO1$Allbikeco2save - (MDO1$AllBikekmreplace * ECFbikeLifecyle)

#apply the ammended co2 savings to scenario 7 results
#and ammend the other scenarios where necessary
MDO1 <-  MDO1 %>% 
  mutate(walktoebikeco2save = 
           ifelse(test = draw_bike <= HasBike_mean, 
                  yes = Allbikeco2save,
                  no = AllEbikeco2save
           )
  )
gc()
MDO1  <-  MDO1 %>% 
  mutate(walktoebikeco2save = ifelse(test = draw_bike <= HasBike_mean, 
                                     yes = AllEbikeco2save,
                                     no = Walkco2save
  )
  )
gc()
MDO1$cairnsco2save <- MDO1$cairnsco2save - (MDO1$cairns * ECFebikeLifecycle)

gc()


#---------- account for ebike emissions scenarios 8-10 --- 

MDO1$AllEbikeco2save30m <- MDO1$AllEbikeco2save30m - (ECFebikeLifecycle * MDO1$AllEBikekmreplace30m)
MDO1$AllEbikeco2saveFlex <- MDO1$AllEbikeco2saveFlex - (ECFebikeLifecycle * MDO1$AllEBikekmreplaceFlex)
MDO1$AllEbikeco2save30mFlex <- MDO1$AllEbikeco2save30mFlex - (ECFebikeLifecycle * MDO1$AllEBikekmreplace30mFlex)
#summary(MDO1$AllEbikeco2save)
#summary(MDO1$AllEbikeco2save30m)
#summary(MDO1$AllEbikeco2saveFlex)
#summary(MDO1$AllEbikeco2save30mFlex)
# cor(MDO1$AllEbikeco2save,
#     MDO1$AllEbikeco2save30m,
#     use = "pairwise.complete.obs")# cor = 0.7568822
# cor(MDO1$AllEbikeco2save,MDO1$AllEbikeco2saveFlex,use = "pairwise.complete.obs")
# cor(MDO1$AllEbikeco2save,MDO1$AllEbikeco2save30mFlex,use = "pairwise.complete.obs")


#---- compare km saved  to total car travel ------------ 
#[removed block of non working commented out code ] 


MDO1  <-  MDO1 %>% 
  mutate(kmEbike_mdd = ifelse(test = (AllEBikekmreplace -  MDDkmreplace) < 1
                              & hasCar == 1 &
                                maxroaddist > 2 , 
                              yes = 1,
                              no = 0
  )
  )

gc()
MDO1  <-  MDO1 %>% 
  mutate(kmEbike_car = ifelse(test = (weightedkm_pp - AllEBikekmreplace) < 1 
                              & hasCar == 1 & maxroaddist > 2 , 
                              yes = 1,
                              no = 0
  )
  )

MDO1  <-  MDO1 %>% 
  mutate(kmEbike_car30m = ifelse(test = (weightedkm_pp - AllEBikekmreplace30m) < 1 
                                 & hasCar == 1 & maxroaddist > 2 , 
                                 yes = 1,
                                 no = 0
  )
  )

MDO1  <-  MDO1 %>% 
  mutate(kmmdd_car = ifelse(test = (weightedkm_pp - MDDkmreplace) < 1 
                            & hasCar == 1 & maxroaddist > 2 , 
                            yes = 1,
                            no = 0
  )
  )

MDO1  <-  MDO1 %>% 
  mutate(kmcarmddno_ebikeyes = ifelse(test =(kmmdd_car ==0 &
                                               kmEbike_car == 1 &
                                               hasCar == 1 &
                                               maxroaddist > 2), 
                                      yes = 1,
                                      no = 0
  )
  )



gc()


print("#--- calculate differences between key scenarios---------- ")
MDO1$diffMDD_currentEbikeco2save <- MDO1$currentEbikeco2save - MDO1$MDDco2save
MDO1$diffAllbike_AllEbikeco2save <- MDO1$AllEbikeco2save - MDO1$Allbikeco2save
MDO1$diffMDDco2save_AllEbikeco2save <- MDO1$AllEbikeco2save - MDO1$MDDco2save

MDO1$diffAllEbikeco2save_AllEbikeco2save30m <- MDO1$AllEbikeco2save - MDO1$AllEbikeco2save30m
MDO1$diffAllEbikeco2save_AllEbikeco2saveFlex <- MDO1$AllEbikeco2save - MDO1$AllEbikeco2saveFlex
MDO1$diffAllEbikeco2save_AllEbikeco2save30mFlex <- MDO1$AllEbikeco2save - MDO1$AllEbikeco2save30mFlex

gc()
#
#histogram of savings (not run)
#summary(MDO1$diffMDD_currentEbikeco2save) 
#png("diffMDD_currentEbikeco2save121119.png")
#hist(MDO1$diffMDD_currentEbikeco2save)
#dev.off()

#Net savings of current cyclists upgrading to ebikes is
mean(MDO1$diffMDD_currentEbikeco2save) * nrow(MDO1) /1000000 #total
mean(MDO1$diffMDD_currentEbikeco2save) #per person
#------ 
summary(MDO1$diffAllbike_AllEbikeco2save)
#png("diffAllbike_AllEbikeco2save.png")
#hist(MDO1$diffAllbike_AllEbikeco2save)
#dev.off()

print("#------- Make a sumary of all the variables --------") 
summarydf <-   do.call(cbind, lapply(MDO1, summary))
sumarydf <- as.data.frame(summarydf)
tsummarydf <- as.data.frame(t(summarydf)) 
tsummarydf$namesvec <- as.vector((names(MDO1)))
write_csv(tsummarydf, paste0("Results",datetime,"/","summaryMDO1vars",seed_number,"_", datetime, ".csv"))
gc()


#---- tidy ----- 
rm(MDO1k)
rm(MDOVarsKeep)
rm(bikelookup)
rm(vmt_weight)
rm(subpopAM)
rm(subpopPM)
rm(filtered)
rm(test_cardraw)
rm(lsoa_test)
rm(nocar)
rm(p1)
rm(ECFbikeLifecyle,ECFbikewelltoWheel,ECFebikeLifecycle,
   function_MDD,functionDODrawDistSavings040919,
   functionDODrawDistSavingsALLBIKE040919)

rm(summarydf,sumarydf,tsummarydf)
rm(WeisslifeCycleBike,WeisslifeCycleEBike,
   WeissWellWheelBike,WeissWellWheelEbike
)
gc()


#remove any unnecessary columns from MDO1
MDO1 <- MDO1 %>% select(-c(demogweighted,draw_bike))
gc()


print("#------- aggregate to LSOA -----------------------")
#names(MDO1)
gc()

# read in and join to geographical identifiers
MDO3 <- fread(paste0("MDOfiles/MDO1_",datetime, ".csv"),
              select = c("ZoneID","personID","LSOA11CD")
)
gc()
MDO3 <- as_tibble(MDO3)
#glimpse(MDO3)
gc()
# Join
MDO1 <- left_join(MDO1,MDO3, by = c("personID"="personID"))
gc()
#glimpse(MDO1)
#names(MDO1)
rm(MDO3)
gc()
##note that if package Hmisc is loaded then is makes dplyr
#summarize conflict so specify package::functoin notation 
#is used here


MDO1 <- as.data.table(MDO1)
gc()
MDO_LSOA <- MDO1[,.(totaladultsim = .N,                        
                        meanweightedkm_pp = mean(weightedkm_pp,na.rm = T),
                        sumweightedkm_pp = sum(weightedkm_pp,na.rm = T),
                        meanmaxbikedist = mean(maxroaddist,na.rm = T),                
                        meanwalkspeedkmh =   mean(walkspeedkmh,na.rm = T), 
                        HasBike_mean = mean(HasBike_mean,na.rm = T),
                        emfac_co2_av_Y = mean(emfac_co2_av_Y,na.rm = T),             
                        countMale = sum(sexw,na.rm = T),
                        meanage =  mean(age,na.rm = T),
                        meanMDD = mean(MDD,na.rm = T), 
                        meanMDDkmreplace = mean(MDDkmreplace,na.rm = T), 
                        sumMDDkmreplace = sum(MDDkmreplace,na.rm = T), 
                        meanMDDco2save = mean(MDDco2save,na.rm = T),    
                        sumMDDco2save = sum(MDDco2save,na.rm = T), 
                        meanAllBikekmreplace = mean(AllBikekmreplace,na.rm = T),  
                        sumAllBikekmreplace = sum(AllBikekmreplace,na.rm = T),  
                        meanAllbikeco2save =mean(Allbikeco2save,na.rm = T),
                        sumAllbikeco2save = sum(Allbikeco2save,na.rm = T),
                        meancurrentEBike = mean(currentEBike,na.rm=T),  
                        sumcurrentEBike = sum(currentEBike,na.rm=T), 
                        meancurrentEBikekmreplace = mean(currentEBikekmreplace,na.rm = T),
                        sumcurrentEBikekmreplace = sum(currentEBikekmreplace,na.rm = T),
                        meancurrentEbikeco2save = mean(currentEbikeco2save,na.rm = T), 
                        sumcurrentEbikeco2save = sum(currentEbikeco2save,na.rm = T),
                        meanAllEBikekmreplace = mean(AllEBikekmreplace,na.rm=T),          
                        sumAllEBikekmreplace = sum(AllEBikekmreplace,na.rm=T), 
                        meanAllEbikeco2save = mean(AllEbikeco2save,na.rm = T), 
                        sumAllEbikeco2save = sum(AllEbikeco2save,na.rm = T),
                        meancairns = mean(cairns,na.rm= T),  
                        sumcairns = sum(cairns,na.rm= T),    
                        meancairnsco2save = mean(cairnsco2save,na.rm = T),              
                        sumcairnsco2save = sum(cairnsco2save,na.rm = T), 
                        meanWalkkmreplace = mean(Walkkmreplace,na.rm = T),
                        sumWalkkmreplace = sum(Walkkmreplace,na.rm = T), 
                        meanWalkco2save = mean(Walkco2save,na.rm = T),
                        sumWalkco2save = sum(Walkco2save,na.rm = T),
                        meanwalktoebikekmreplace = mean(walktoebikekmreplace,na.rm=T), 
                        sumwalktoebikekmreplace = sum(walktoebikekmreplace,na.rm=T),
                        meanwalktoebikeco2save = mean(walktoebikeco2save,na.rm = T),
                        sumwalktoebikeco2save = sum(walktoebikeco2save,na.rm = T),
                        meandiffMDD_currentEbikeco2save = mean(diffMDD_currentEbikeco2save,na.rm = T),
                        sumdiffMDD_currentEbikeco2save = sum(diffMDD_currentEbikeco2save,na.rm = T),
                        meandiffAllbike_AllEbikeco2save = mean(diffAllbike_AllEbikeco2save,na.rm = T),
                        sumdiffAllbike_AllEbikeco2save = sum(diffAllbike_AllEbikeco2save,na.rm = T),
                        sumdiffMDDco2save_AllEbikeco2save = sum(diffMDDco2save_AllEbikeco2save,na.rm=T),
                        meandiffMDDco2save_AllEbikeco2save = mean(diffMDDco2save_AllEbikeco2save,na.rm=T),
                        caraccess = sum(hasCar,na.rm = T),
                        sumkmEbike_mdd = sum(kmEbike_mdd,na.rm=T),
                        sumkmmdd_car = sum(kmmdd_car,na.rm=T),
                        sumEbike_car = sum(kmEbike_car,na.rm=T),
                        sumkmcarmddno_ebikeyes = sum(kmcarmddno_ebikeyes,na.rm=T),
                        
                        
                        meanAllEBikekmreplace30m = mean(AllEBikekmreplace30m,na.rm=T),          
                        sumAllEBikekmreplace30m = sum(AllEBikekmreplace30m,na.rm=T), 
                        meanAllEbikeco2save30m = mean(AllEbikeco2save30m,na.rm = T), 
                        sumAllEbikeco2save30m = sum(AllEbikeco2save30m,na.rm = T),
                        
                        
                        
                        meanAllEBikekmreplaceFlex = mean(AllEBikekmreplaceFlex,na.rm=T),          
                        sumAllEBikekmreplaceFlex = sum(AllEBikekmreplaceFlex,na.rm=T), 
                        meanAllEbikeco2saveFlex = mean(AllEbikeco2saveFlex,na.rm = T), 
                        sumAllEbikeco2saveFlex = sum(AllEbikeco2saveFlex,na.rm = T),							 
                        
                        meanAllEBikekmreplace30mFlex = mean(AllEBikekmreplace30mFlex,na.rm=T),          
                        sumAllEBikekmreplace30mFlex = sum(AllEBikekmreplace30mFlex,na.rm=T), 
                        meanAllEbikeco2save30mFlex = mean(AllEbikeco2save30mFlex,na.rm = T), 
                        sumAllEbikeco2save30mFlex = sum(AllEbikeco2save30mFlex,na.rm = T),
                        
                        kmEbike_mdd  =      sum( kmEbike_mdd,na.rm = T),                       
                        kmEbike_car   =      sum(kmEbike_car,na.rm = T),                      
                        kmmdd_car      =     sum(kmmdd_car,na.rm = T),                      
                        kmcarmddno_ebikeyes= sum(kmcarmddno_ebikeyes,na.rm = T),
                        kmEbike_car30m = sum(kmEbike_car30m,na.rm = T),
                        
                        meandiffAllEbikeco2save_AllEbikeco2save30m   = mean(diffAllEbikeco2save_AllEbikeco2save30m,na.rm = T),
                        meandiffAllEbikeco2save_AllEbikeco2saveFlex  = mean(diffAllEbikeco2save_AllEbikeco2saveFlex,na.rm = T),
                        meandiffAllEbikeco2save_AllEbikeco2save30mFlex = mean(diffAllEbikeco2save_AllEbikeco2save30mFlex,na.rm = T),
                        sumdiffAllEbikeco2save_AllEbikeco2save30m   = sum(diffAllEbikeco2save_AllEbikeco2save30m,na.rm = T),
                        sumdiffAllEbikeco2save_AllEbikeco2saveFlex  = sum(diffAllEbikeco2save_AllEbikeco2saveFlex,na.rm = T),
                        sumdiffAllEbikeco2save_AllEbikeco2save30mFlex = sum(diffAllEbikeco2save_AllEbikeco2save30mFlex,na.rm = T),
                        ebikeDailyDist = mean(ebikemaxroaddist,na.rm = T)
),by = LSOA11CD
]
gc()

write_csv(MDO_LSOA,paste0("Results",datetime,"/","MDO_LSOA",seed_number,"_", datetime, ".csv"))


gc()



print("#---------------------- summarise results by OAC -------------")



MDO_supergroup <- MDO1[,.(totaladultsim = .N,                        
                    meanweightedkm_pp = mean(weightedkm_pp,na.rm = T),
                    sumweightedkm_pp = sum(weightedkm_pp,na.rm = T),
                    meanmaxbikedist = mean(maxroaddist,na.rm = T),                
                    meanwalkspeedkmh =   mean(walkspeedkmh,na.rm = T), 
                    HasBike_mean = mean(HasBike_mean,na.rm = T),
                    emfac_co2_av_Y = mean(emfac_co2_av_Y,na.rm = T),             
                    countMale = sum(sexw,na.rm = T),
                    meanage =  mean(age,na.rm = T),
                    meanMDD = mean(MDD,na.rm = T), 
                    meanMDDkmreplace = mean(MDDkmreplace,na.rm = T), 
                    sumMDDkmreplace = sum(MDDkmreplace,na.rm = T), 
                    meanMDDco2save = mean(MDDco2save,na.rm = T),    
                    sumMDDco2save = sum(MDDco2save,na.rm = T), 
                    meanAllBikekmreplace = mean(AllBikekmreplace,na.rm = T),  
                    sumAllBikekmreplace = sum(AllBikekmreplace,na.rm = T),  
                    meanAllbikeco2save =mean(Allbikeco2save,na.rm = T),
                    sumAllbikeco2save = sum(Allbikeco2save,na.rm = T),
                    meancurrentEBike = mean(currentEBike,na.rm=T),  
                    sumcurrentEBike = sum(currentEBike,na.rm=T), 
                    meancurrentEBikekmreplace = mean(currentEBikekmreplace,na.rm = T),
                    sumcurrentEBikekmreplace = sum(currentEBikekmreplace,na.rm = T),
                    meancurrentEbikeco2save = mean(currentEbikeco2save,na.rm = T), 
                    sumcurrentEbikeco2save = sum(currentEbikeco2save,na.rm = T),
                    meanAllEBikekmreplace = mean(AllEBikekmreplace,na.rm=T),          
                    sumAllEBikekmreplace = sum(AllEBikekmreplace,na.rm=T), 
                    meanAllEbikeco2save = mean(AllEbikeco2save,na.rm = T), 
                    sumAllEbikeco2save = sum(AllEbikeco2save,na.rm = T),
                    meancairns = mean(cairns,na.rm= T),  
                    sumcairns = sum(cairns,na.rm= T),    
                    meancairnsco2save = mean(cairnsco2save,na.rm = T),              
                    sumcairnsco2save = sum(cairnsco2save,na.rm = T), 
                    meanWalkkmreplace = mean(Walkkmreplace,na.rm = T),
                    sumWalkkmreplace = sum(Walkkmreplace,na.rm = T), 
                    meanWalkco2save = mean(Walkco2save,na.rm = T),
                    sumWalkco2save = sum(Walkco2save,na.rm = T),
                    meanwalktoebikekmreplace = mean(walktoebikekmreplace,na.rm=T), 
                    sumwalktoebikekmreplace = sum(walktoebikekmreplace,na.rm=T),
                    meanwalktoebikeco2save = mean(walktoebikeco2save,na.rm = T),
                    sumwalktoebikeco2save = sum(walktoebikeco2save,na.rm = T),
                    meandiffMDD_currentEbikeco2save = mean(diffMDD_currentEbikeco2save,na.rm = T),
                    sumdiffMDD_currentEbikeco2save = sum(diffMDD_currentEbikeco2save,na.rm = T),
                    meandiffAllbike_AllEbikeco2save = mean(diffAllbike_AllEbikeco2save,na.rm = T),
                    sumdiffAllbike_AllEbikeco2save = sum(diffAllbike_AllEbikeco2save,na.rm = T),
                    sumdiffMDDco2save_AllEbikeco2save = sum(diffMDDco2save_AllEbikeco2save,na.rm=T),
                    meandiffMDDco2save_AllEbikeco2save = mean(diffMDDco2save_AllEbikeco2save,na.rm=T),
                    caraccess = sum(hasCar,na.rm = T),
                    sumkmEbike_mdd = sum(kmEbike_mdd,na.rm=T),
                    sumkmmdd_car = sum(kmmdd_car,na.rm=T),
                    sumEbike_car = sum(kmEbike_car,na.rm=T),
                    sumkmcarmddno_ebikeyes = sum(kmcarmddno_ebikeyes,na.rm=T),
                    
                    
                    meanAllEBikekmreplace30m = mean(AllEBikekmreplace30m,na.rm=T),          
                    sumAllEBikekmreplace30m = sum(AllEBikekmreplace30m,na.rm=T), 
                    meanAllEbikeco2save30m = mean(AllEbikeco2save30m,na.rm = T), 
                    sumAllEbikeco2save30m = sum(AllEbikeco2save30m,na.rm = T),
                    
                    
                    
                    meanAllEBikekmreplaceFlex = mean(AllEBikekmreplaceFlex,na.rm=T),          
                    sumAllEBikekmreplaceFlex = sum(AllEBikekmreplaceFlex,na.rm=T), 
                    meanAllEbikeco2saveFlex = mean(AllEbikeco2saveFlex,na.rm = T), 
                    sumAllEbikeco2saveFlex = sum(AllEbikeco2saveFlex,na.rm = T),							 
                    
                    meanAllEBikekmreplace30mFlex = mean(AllEBikekmreplace30mFlex,na.rm=T),          
                    sumAllEBikekmreplace30mFlex = sum(AllEBikekmreplace30mFlex,na.rm=T), 
                    meanAllEbikeco2save30mFlex = mean(AllEbikeco2save30mFlex,na.rm = T), 
                    sumAllEbikeco2save30mFlex = sum(AllEbikeco2save30mFlex,na.rm = T),
                    
                    kmEbike_mdd  =      sum( kmEbike_mdd,na.rm = T),                       
                    kmEbike_car   =      sum(kmEbike_car,na.rm = T),                      
                    kmmdd_car      =     sum(kmmdd_car,na.rm = T),                      
                    kmcarmddno_ebikeyes= sum(kmcarmddno_ebikeyes,na.rm = T),
                    kmEbike_car30m = sum(kmEbike_car30m,na.rm = T),
                    
                    meandiffAllEbikeco2save_AllEbikeco2save30m   = mean(diffAllEbikeco2save_AllEbikeco2save30m,na.rm = T),
                    meandiffAllEbikeco2save_AllEbikeco2saveFlex  = mean(diffAllEbikeco2save_AllEbikeco2saveFlex,na.rm = T),
                    meandiffAllEbikeco2save_AllEbikeco2save30mFlex = mean(diffAllEbikeco2save_AllEbikeco2save30mFlex,na.rm = T),
                    sumdiffAllEbikeco2save_AllEbikeco2save30m   = sum(diffAllEbikeco2save_AllEbikeco2save30m,na.rm = T),
                    sumdiffAllEbikeco2save_AllEbikeco2saveFlex  = sum(diffAllEbikeco2save_AllEbikeco2saveFlex,na.rm = T),
                    sumdiffAllEbikeco2save_AllEbikeco2save30mFlex = sum(diffAllEbikeco2save_AllEbikeco2save30mFlex,na.rm = T),
                    ebikeDailyDist = mean(ebikemaxroaddist,na.rm = T)
),by = Supergroup_Code
]

gc()


write_csv(MDO_supergroup,paste0("Results",datetime,"/","MDO1_Supergroup",seed_number,"_", datetime, ".csv"))

#------------------- summarise by age and gender ----------------


gc()
MDO_sexage <- MDO1[,.(totaladultsim = .N,                        
                    meanweightedkm_pp = mean(weightedkm_pp,na.rm = T),
                    sumweightedkm_pp = sum(weightedkm_pp,na.rm = T),
                    meanmaxbikedist = mean(maxroaddist,na.rm = T),                
                    meanwalkspeedkmh =   mean(walkspeedkmh,na.rm = T), 
                    HasBike_mean = mean(HasBike_mean,na.rm = T),
                    emfac_co2_av_Y = mean(emfac_co2_av_Y,na.rm = T),             
                    countMale = sum(sexw,na.rm = T),
                    meanage =  mean(age,na.rm = T),
                    meanMDD = mean(MDD,na.rm = T), 
                    meanMDDkmreplace = mean(MDDkmreplace,na.rm = T), 
                    sumMDDkmreplace = sum(MDDkmreplace,na.rm = T), 
                    meanMDDco2save = mean(MDDco2save,na.rm = T),    
                    sumMDDco2save = sum(MDDco2save,na.rm = T), 
                    meanAllBikekmreplace = mean(AllBikekmreplace,na.rm = T),  
                    sumAllBikekmreplace = sum(AllBikekmreplace,na.rm = T),  
                    meanAllbikeco2save =mean(Allbikeco2save,na.rm = T),
                    sumAllbikeco2save = sum(Allbikeco2save,na.rm = T),
                    meancurrentEBike = mean(currentEBike,na.rm=T),  
                    sumcurrentEBike = sum(currentEBike,na.rm=T), 
                    meancurrentEBikekmreplace = mean(currentEBikekmreplace,na.rm = T),
                    sumcurrentEBikekmreplace = sum(currentEBikekmreplace,na.rm = T),
                    meancurrentEbikeco2save = mean(currentEbikeco2save,na.rm = T), 
                    sumcurrentEbikeco2save = sum(currentEbikeco2save,na.rm = T),
                    meanAllEBikekmreplace = mean(AllEBikekmreplace,na.rm=T),          
                    sumAllEBikekmreplace = sum(AllEBikekmreplace,na.rm=T), 
                    meanAllEbikeco2save = mean(AllEbikeco2save,na.rm = T), 
                    sumAllEbikeco2save = sum(AllEbikeco2save,na.rm = T),
                    meancairns = mean(cairns,na.rm= T),  
                    sumcairns = sum(cairns,na.rm= T),    
                    meancairnsco2save = mean(cairnsco2save,na.rm = T),              
                    sumcairnsco2save = sum(cairnsco2save,na.rm = T), 
                    meanWalkkmreplace = mean(Walkkmreplace,na.rm = T),
                    sumWalkkmreplace = sum(Walkkmreplace,na.rm = T), 
                    meanWalkco2save = mean(Walkco2save,na.rm = T),
                    sumWalkco2save = sum(Walkco2save,na.rm = T),
                    meanwalktoebikekmreplace = mean(walktoebikekmreplace,na.rm=T), 
                    sumwalktoebikekmreplace = sum(walktoebikekmreplace,na.rm=T),
                    meanwalktoebikeco2save = mean(walktoebikeco2save,na.rm = T),
                    sumwalktoebikeco2save = sum(walktoebikeco2save,na.rm = T),
                    meandiffMDD_currentEbikeco2save = mean(diffMDD_currentEbikeco2save,na.rm = T),
                    sumdiffMDD_currentEbikeco2save = sum(diffMDD_currentEbikeco2save,na.rm = T),
                    meandiffAllbike_AllEbikeco2save = mean(diffAllbike_AllEbikeco2save,na.rm = T),
                    sumdiffAllbike_AllEbikeco2save = sum(diffAllbike_AllEbikeco2save,na.rm = T),
                    sumdiffMDDco2save_AllEbikeco2save = sum(diffMDDco2save_AllEbikeco2save,na.rm=T),
                    meandiffMDDco2save_AllEbikeco2save = mean(diffMDDco2save_AllEbikeco2save,na.rm=T),
                    caraccess = sum(hasCar,na.rm = T),
                    sumkmEbike_mdd = sum(kmEbike_mdd,na.rm=T),
                    sumkmmdd_car = sum(kmmdd_car,na.rm=T),
                    sumEbike_car = sum(kmEbike_car,na.rm=T),
                    sumkmcarmddno_ebikeyes = sum(kmcarmddno_ebikeyes,na.rm=T),
                    
                    
                    meanAllEBikekmreplace30m = mean(AllEBikekmreplace30m,na.rm=T),          
                    sumAllEBikekmreplace30m = sum(AllEBikekmreplace30m,na.rm=T), 
                    meanAllEbikeco2save30m = mean(AllEbikeco2save30m,na.rm = T), 
                    sumAllEbikeco2save30m = sum(AllEbikeco2save30m,na.rm = T),
                    
                    
                    
                    meanAllEBikekmreplaceFlex = mean(AllEBikekmreplaceFlex,na.rm=T),          
                    sumAllEBikekmreplaceFlex = sum(AllEBikekmreplaceFlex,na.rm=T), 
                    meanAllEbikeco2saveFlex = mean(AllEbikeco2saveFlex,na.rm = T), 
                    sumAllEbikeco2saveFlex = sum(AllEbikeco2saveFlex,na.rm = T),							 
                    
                    meanAllEBikekmreplace30mFlex = mean(AllEBikekmreplace30mFlex,na.rm=T),          
                    sumAllEBikekmreplace30mFlex = sum(AllEBikekmreplace30mFlex,na.rm=T), 
                    meanAllEbikeco2save30mFlex = mean(AllEbikeco2save30mFlex,na.rm = T), 
                    sumAllEbikeco2save30mFlex = sum(AllEbikeco2save30mFlex,na.rm = T),
                    
                    kmEbike_mdd  =      sum( kmEbike_mdd,na.rm = T),                       
                    kmEbike_car   =      sum(kmEbike_car,na.rm = T),                      
                    kmmdd_car      =     sum(kmmdd_car,na.rm = T),                      
                    kmcarmddno_ebikeyes= sum(kmcarmddno_ebikeyes,na.rm = T),
                    kmEbike_car30m = sum(kmEbike_car30m,na.rm = T),
                    
                    meandiffAllEbikeco2save_AllEbikeco2save30m   = mean(diffAllEbikeco2save_AllEbikeco2save30m,na.rm = T),
                    meandiffAllEbikeco2save_AllEbikeco2saveFlex  = mean(diffAllEbikeco2save_AllEbikeco2saveFlex,na.rm = T),
                    meandiffAllEbikeco2save_AllEbikeco2save30mFlex = mean(diffAllEbikeco2save_AllEbikeco2save30mFlex,na.rm = T),
                    sumdiffAllEbikeco2save_AllEbikeco2save30m   = sum(diffAllEbikeco2save_AllEbikeco2save30m,na.rm = T),
                    sumdiffAllEbikeco2save_AllEbikeco2saveFlex  = sum(diffAllEbikeco2save_AllEbikeco2saveFlex,na.rm = T),
                    sumdiffAllEbikeco2save_AllEbikeco2save30mFlex = sum(diffAllEbikeco2save_AllEbikeco2save30mFlex,na.rm = T),
                    ebikeDailyDist = mean(ebikemaxroaddist,na.rm = T)
),by = sexageconst
]

gc()

write_csv(MDO_sexage,paste0("Results",datetime,"/","MDO1_sexage",seed_number,"_", datetime, ".csv"))



gc()
