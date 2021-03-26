#07_tabulate_and_map_results

#------------ packages required --------- 
require(tidyverse)
require(tmap)
require(tmaptools)
require(data.table)
require(sf)


#--------------- remove objects  to tidy up--------------- 
rm(list=setdiff(ls(), c("datetime")))
#datetime <- "car_lifecycle_ebike_lifecycle_no_langmuir2021-03-21_12-54-59"


#-------- read data-------------
#LSOA results
MDO_LSOA <- read_csv(paste0("Results", datetime, "/multidrawmeans.csv"))
#summaries at individual level
summary_lsoa <- read_csv(paste0("Results", datetime, "/multidrawSE_propn.csv"))



MDO_LSOA$sumdiffMDDco2save_AllEbikeco2save <- MDO_LSOA$sumdiffMDDco2save_AllEbikeco2save/ 1000000
MDO_LSOA$sumdiffAllbike_AllEbikeco2save <- MDO_LSOA$sumdiffAllbike_AllEbikeco2save /1000000
MDO_LSOA$sumdiffMDD_currentEbikeco2save <- MDO_LSOA$sumdiffMDD_currentEbikeco2save /1000000
MDO_LSOA$sumwalktoebikeco2save <- MDO_LSOA$sumwalktoebikeco2save /1000000

MDO_LSOA$sumWalkco2save <- MDO_LSOA$sumWalkco2save/1000000
MDO_LSOA$sumMDDco2save <- MDO_LSOA$sumMDDco2save / 1000000 
MDO_LSOA$sumAllbikeco2save <- MDO_LSOA$sumAllbikeco2save / 1000000
MDO_LSOA$sumAllEbikeco2save <- MDO_LSOA$sumAllEbikeco2save/1000000

MDO_LSOA$sumcurrentEbikeco2save <- MDO_LSOA$sumcurrentEbikeco2save /1000000
MDO_LSOA$sumcairnsco2save <- MDO_LSOA$sumcairnsco2save /1000000

#mean
MDO_LSOA$meandiffMDDco2save_AllEbikeco2save <- MDO_LSOA$meandiffMDDco2save_AllEbikeco2save/ 1000000
MDO_LSOA$meandiffAllbike_AllEbikeco2save <- MDO_LSOA$meandiffAllbike_AllEbikeco2save /1000000
MDO_LSOA$meandiffMDD_currentEbikeco2save <- MDO_LSOA$meandiffMDD_currentEbikeco2save /1000000
MDO_LSOA$meanwalktoebikeco2save <- MDO_LSOA$meanwalktoebikeco2save /1000000


MDO_LSOA$meanWalkco2save <- MDO_LSOA$meanWalkco2save/1000000
MDO_LSOA$meanMDDco2save <- MDO_LSOA$meanMDDco2save / 1000000 
MDO_LSOA$meanAllbikeco2save <- MDO_LSOA$meanAllbikeco2save / 1000000
MDO_LSOA$meanAllEbikeco2save <- MDO_LSOA$meanAllEbikeco2save/1000000

MDO_LSOA$meancurrentEbikeco2save <- MDO_LSOA$meancurrentEbikeco2save /1000000
MDO_LSOA$meancairnsco2save <- MDO_LSOA$meancairnsco2save /1000000




#--------------- prep data for mapping ---------------- 

names(MDO_LSOA)
#read spatial boundaries
lsoa <- st_read(dsn = "GISdata/LSOA11SG",layer = "LSOAEW11SG")
#library(dplyr)
england_bdy <- st_read(dsn = "GISdata/England_ol_2011", layer = "england_ol_2011")


#------------- join sf to mdo_lsoa data ------------- 
names(MDO_LSOA)
lsoa <- left_join(lsoa, MDO_LSOA, by = c("lsoa11cd"="lsoa1$LSOA11CD"))
#lsoa <- left_join(lsoa, MDO_LSOA, by = c("lsoa11cd"="LSOA11CD"))

names(lsoa)
gc()

#------ remove welsh LSOAs -------------------- 

lsoa <- lsoa %>% filter(substr(lsoa11cd,1,1) != "W")
lsoa <- lsoa %>% filter(substr(lsoa11cd,1,1) != "w")
lsoa <- lsoa %>% filter(substr(lsoa11cd,1,1) != "S") # no scottish or NI dzs 
lsoa <- lsoa %>% filter(substr(lsoa11cd,1,1) != "9")

#check for any codes that are not lsoas
lsoa <- lsoa %>% filter(substr(lsoa11cd,1,3) == "E01")
#remove duplicate rows
lsoa <- lsoa %>% distinct(.keep_all = TRUE)
# 
# lsoa_duplicates = lsoa %>% group_by(lsoa11cd)%>%
#   summarise(n = n()) %>% filter(n >1)
# filtered <- lsoa %>% filter(lsoa11cd =="E01003502")
# filtered = distinct(filtered)

dir.create(paste0("plotsmaps",datetime))


#--------------- histogram of each variable lsoa ------


dir.create(paste0("plotsmaps",datetime, "/histlooplsoa"))

namesvec <- names(lsoa)

#columns 1-6 are string info about the lsoa, final column 
#is the sf geometry so don't plot these
for(i in 7:(length(namesvec)-1)){
  lsoa %>% ggplot() +
    geom_histogram(aes_string(namesvec[i]))+
    ggtitle(paste0("LSOA histogram",namesvec))
  ggsave(file = paste0("plotsmaps",datetime,"/histlooplsoa","/LSOA histogram",namesvec[i],".png")) 
}





print("#------------- MAPPING -------------------")


  
#------ Figure 3 ------  
#[I've made fisher and quintile plots - both have similar patterns, 
#this is as a check to avoid misrepresentation through categorisation see 
#Monmornier 'how to lie with maps' for more info]
  
  
  #fisher 5 categories 
  namei <- "meanAllEbikeco2save"
  print(namei)
  lsoa2 <- lsoa %>% select((!!as.symbol(namei)))
  lsoa2 <- lsoa2 %>% filter((!!as.symbol(namei)) >= 0)
  lsoa2 <- lsoa2 %>% mutate_if(is.numeric, round, 2)
  
  m <- lsoa2 %>%
    tm_shape() +
    tm_fill(namei,lty = 0, lwd = 0,border.alpha = 0.2,style= 'fisher',n = 5,
            title = "",legend.hist = T,palette = "Blues",midpoint = NA,
            #alpha = 0.5,
            legend.format=list(fun=function(x) paste0(formatC(x, digits=2, format="f")," "))
    )+
    tm_scale_bar(position =  c("right", "bottom"),color.dark = "gray",text.color = "gray",text.size = 0.4)+
    tm_layout(frame = T, legend.position = c("left", "top"),
              legend.height = 0.5,legend.width = 0.5,
              legend.text.size = 1
    )
  m
  #map_save(m,"test.jpg")
  tmap_save(m,paste0("plotsmaps",datetime,"/",namei,"FIG3FillFisherBlues5.jpg"),
            height = 29.7,width = 21, unit = "cm",
            dpi = 300)
  
  
  
  
  
  #Quintile 5 categories 
  namei <- "meanAllEbikeco2save"
  print(namei)
  lsoa2 <- lsoa %>% select((!!as.symbol(namei)))
  lsoa2 <- lsoa2 %>% filter((!!as.symbol(namei)) >= 0)
  lsoa2 <- lsoa2 %>% mutate_if(is.numeric, round, 2)
  
  m <- lsoa2 %>%
    tm_shape() +
    tm_fill(namei,lty = 0, lwd = 0,border.alpha = 0.2,style= 'quantile',n = 5,
            title = "",legend.hist = T,palette = "Blues",midpoint = NA,
            #alpha = 0.5,
            legend.format=list(fun=function(x) paste0(formatC(x, digits=2, format="f")," "))
    )+
    tm_scale_bar(position =  c("right", "bottom"),color.dark = "gray",text.color = "gray",text.size = 0.4)+
    tm_layout(frame = T, legend.position = c("left", "top"),
              legend.height = 0.5,legend.width = 0.5,
              legend.text.size = 1
    )
  m
  #map_save(m,"test.jpg")
  tmap_save(m,paste0("plotsmaps",datetime,"/",namei,"FIG3FillQuintileBlues5.jpg"),
            height = 29.7,width = 21, unit = "cm",
            dpi = 300)
  
  
  
  
  
#----- figure 2  --------------------
  
  #fisher 5 categories 
  namei <- "meanAllEBikekmreplace"
  print(namei)
  lsoa2 <- lsoa %>% select((!!as.symbol(namei)))
  lsoa2 <- lsoa2 %>% filter((!!as.symbol(namei)) >= 0)
  lsoa2 <- lsoa2 %>% mutate_if(is.numeric, round, -2)
  
  m <- lsoa2 %>%
    tm_shape() +
    tm_fill(namei,lty = 0, lwd = 0,border.alpha = 0.2,style= 'fisher',n = 5,
            title = "",legend.hist = T,palette = "Blues",midpoint = NA,
            #alpha = 0.5,
            legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f")," "))
    )+
    tm_scale_bar(position =  c("right", "bottom"),color.dark = "gray",text.color = "gray",text.size = 0.4)+
    tm_layout(frame = T, legend.position = c("left", "top"),
              legend.height = 0.5,legend.width = 0.5,
              legend.text.size = 1
    )
 # m
  #map_save(m,"test.jpg")
  tmap_save(m,paste0("plotsmaps",datetime,"/",namei,"FIG2FillFisherBlues5.jpg"),
            height = 29.7,width = 21, unit = "cm",
            dpi = 300)
  
  
  
  
  
  
  
  #Quintile 5 categories 
  namei <- "meanAllEBikekmreplace"
  print(namei)
  lsoa2 <- lsoa %>% select((!!as.symbol(namei)))
  lsoa2 <- lsoa2 %>% filter((!!as.symbol(namei)) >= 0)
  lsoa2 <- lsoa2 %>% mutate_if(is.numeric, round, -2)
  
  m <- lsoa2 %>%
    tm_shape() +
    tm_fill(namei,lty = 0, lwd = 0,border.alpha = 0.2,style= 'quantile',n = 5,
            title = "",legend.hist = T,palette = "Blues",midpoint = NA,
            #alpha = 0.5,
            legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f")," "))
    )+
    tm_scale_bar(position =  c("right", "bottom"),color.dark = "gray",text.color = "gray",text.size = 0.4)+
    tm_layout(frame = T, legend.position = c("left", "top"),
              legend.height = 0.5,legend.width = 0.5,
              legend.text.size = 1
    )
  # m
  #map_save(m,"test.jpg")
  tmap_save(m,paste0("plotsmaps",datetime,"/",namei,"FIG2FillQuintileBlues5.jpg"),
            height = 29.7,width = 21, unit = "cm",
            dpi = 300)
  
  
  
  

  
#---------- Figure 4 net  ---------------------   

  #we didn't put the comparison of e-bikes and mdd (walk / cycle mix) in the paper for reasons of space, 
  #but the code is below for reference
  
  #-------------------- compare differences ----------------------- 
  
  #-----key comparable maps (maps with the same class boundaries and colour scheme)--

  
  #---------- Figure 4  net  savings fisher ---------------------   
  
  
  #-------------------- compare differences ----------------------- 
  
  #not used in paper
  #------- mdd e-bike -------------
  namei <- "meandiffMDDco2save_AllEbikeco2save"
  print(namei)
  lsoa2 <- lsoa %>% select((!!as.symbol(namei)))
  lsoa2 <- lsoa2 %>% filter((!!as.symbol(namei)) >= 0)
  lsoa2 <- lsoa2 %>% mutate_if(is.numeric, round, 2)
  
  m <- lsoa2 %>%
    tm_shape() +
    tm_fill(namei,lty = 0, lwd = 0,border.alpha = 0.2,style = 'fisher',n=5,
            title = "",legend.hist = T,palette = "RdYlBu",midpoint = NA,
            #alpha = 0.5,
            legend.format=list(fun=function(x) paste0(formatC(x, digits=2, format="f")," "))
    )+
    tm_scale_bar(position =  c("right", "bottom"),color.dark = "gray",text.color = "gray",text.size = 0.4)+
    tm_layout(frame = T, legend.position = c("left", "top"),
              legend.height = 0.5,legend.width = 0.5,
              legend.text.size = 1
    )
  #m
  
  tmap_save(m,paste0("plotsmaps",datetime,"/",namei,"FIG5fisherRdYlBu.jpg"),
            height = 29.7,width = 21, unit = "cm",
            dpi = 300)
  
  #----------- meandiffAllbike_AllEbikeco2save
  namei <- "meandiffAllbike_AllEbikeco2save"
  print(namei)
  lsoa2 <- lsoa %>% select((!!as.symbol(namei)))
  lsoa2 <- lsoa2 %>% filter((!!as.symbol(namei)) >= 0)
  lsoa2 <- lsoa2 %>% mutate_if(is.numeric, round, 2)
  
  m <- lsoa2 %>%
    tm_shape() +
    tm_fill(namei,lty = 0, lwd = 0,border.alpha = 0.2,style = 'fisher',n=5,
            title = "",legend.hist = T,palette = "RdYlBu",midpoint = NA,
            #alpha = 0.5,
            legend.format=list(fun=function(x) paste0(formatC(x, digits=2, format="f")," "))
    )+
    tm_scale_bar(position =  c("right", "bottom"),color.dark = "gray",text.color = "gray",text.size = 0.4)+
    tm_layout(frame = T, legend.position = c("left", "top"),
              legend.height = 0.5,legend.width = 0.5,
              legend.text.size = 1
    )
  #m
  
  tmap_save(m,paste0("plotsmaps",datetime,"/",namei,"FIG4fisherRdYlBu.jpg"),
            height = 29.7,width = 21, unit = "cm",
            dpi = 300)
  
  
  
  

  
  #---------- Figure 4  net  savings quantile ---------------------   
  
  
  #-------------------- compare differences ----------------------- 
  
  #not in paper
  #------- mdd e-bike -------------
  namei <- "meandiffMDDco2save_AllEbikeco2save"
  print(namei)
  lsoa2 <- lsoa %>% select((!!as.symbol(namei)))
  lsoa2 <- lsoa2 %>% filter((!!as.symbol(namei)) >= 0)
  lsoa2 <- lsoa2 %>% mutate_if(is.numeric, round, 2)
  
  m <- lsoa2 %>%
    tm_shape() +
    tm_fill(namei,lty = 0, lwd = 0,border.alpha = 0.2,style = 'quantile',n=5,
            title = "",legend.hist = T,palette = "RdYlBu",midpoint = NA,
            #alpha = 0.5,
            legend.format=list(fun=function(x) paste0(formatC(x, digits=2, format="f")," "))
    )+
    tm_scale_bar(position =  c("right", "bottom"),color.dark = "gray",text.color = "gray",text.size = 0.4)+
    tm_layout(frame = T, legend.position = c("left", "top"),
              legend.height = 0.5,legend.width = 0.5,
              legend.text.size = 1
    )
  #m
  
  tmap_save(m,paste0("plotsmaps",datetime,"/",namei,"FIG5quantileRdYlBu.jpg"),
            height = 29.7,width = 21, unit = "cm",
            dpi = 300)
  
  #----------- meandiffAllbike_AllEbikeco2save
  namei <- "meandiffAllbike_AllEbikeco2save"
  print(namei)
  lsoa2 <- lsoa %>% select((!!as.symbol(namei)))
  lsoa2 <- lsoa2 %>% filter((!!as.symbol(namei)) >= 0)
  lsoa2 <- lsoa2 %>% mutate_if(is.numeric, round, 2)
  
  m <- lsoa2 %>%
    tm_shape() +
    tm_fill(namei,lty = 0, lwd = 0,border.alpha = 0.2,style = 'quantile',n=5,
            title = "",legend.hist = T,palette = "RdYlBu",midpoint = NA,
            #alpha = 0.5,
            legend.format=list(fun=function(x) paste0(formatC(x, digits=2, format="f")," "))
    )+
    tm_scale_bar(position =  c("right", "bottom"),color.dark = "gray",text.color = "gray",text.size = 0.4)+
    tm_layout(frame = T, legend.position = c("left", "top"),
              legend.height = 0.5,legend.width = 0.5,
              legend.text.size = 1
    )
  #m
  
  tmap_save(m,paste0("plotsmaps",datetime,"/",namei,"FIG4quantileRdYlBu.jpg"),
            height = 29.7,width = 21, unit = "cm",
            dpi = 300)
  
  
  
  
  
  
