# calculate best estimate of indicators from the multiple 
#montecarlo draws.  


require(tidyverse)

#datetime <- "2020-02-13_20-30-30" # the full version 
#datetime <- "2020-04-28_19-56-19"

rm(list=setdiff(ls(), c("datetime")))
gc()

seeds <- c(1240,99,42,10,90210,8850,1345,978)

#read in all the result draws as a loop
for(i in 1:length(seeds)){
  assign(paste0("lsoa",i),read_csv(paste0("Results",datetime,"/","MDO_LSOA",seeds[i],"_",datetime,".csv")))
}

#make the dataframes into a list of dataframes (nonautomated version)
mylist <-  list(lsoa1,lsoa2,lsoa3,lsoa4,lsoa5,lsoa6,lsoa7,lsoa8)



#----using loop calculate best estimate of indicators from the multiple --- 
#-----montecarlo draws.  -----------

#lsoasumdiffMDDco2save_AllEbikeco2save is column 46
#make a df to take results from each variable
df <- as.data.frame(lsoa1$LSOA11CD)
#df <- lsoa1$LSOA11CD

#make an empty list
resultList <- list()

#append df to position 1 of the results list
#so variable numbers match
resultList <- append(resultList, list(df), 1)

#--run loop to summarise all variables across all draws--
for(j in 2:ncol(lsoa1)){
  #outer loop to get mean sd and se of every variable
  
  #This loop puts the result of every draw into df
  #j is the draw number 
  #df ends up having 9 cols
  #Lsoa code and then 8 columns one for each draw
  for(i in 1:length(mylist)){
    #print(i)
    indf <- as.data.frame(mylist[[i]])
    df[,i+1] <- indf[[j]]
    
  }
  
  
  #these 2 lines make the mean. first line sum each row
  #second line sum divide by the number of draws 
  df$sum <-  rowSums(df[,2:9])
  df$mean <- df$sum /length(mylist)
  #this uses apply to do in 1 line what I did in 2 above
  df$sdev <- apply(df[,2:9],MARGIN = 1,FUN = sd)
  df <- df %>% mutate(seMean  = sdev /sqrt(length(mylist))) 
  #SE mean expressed as a percentage or proportion is more useful than 
  #an absolute number 
  df <- df %>% mutate(seMean_prMean  = seMean / mean)
  
  resultList <- append(resultList, list(df), 0)
  #resultList[j] <- df 
}

#reverse the list so the list elements are in order of the 
#variables in lsoa1 - 8 
resultList <- rev(resultList)



#------------ best estimate results --------
# create an lsoa dataset which contains all the mean columns

#setting up for a loop
namesvec <- names(lsoa1) 
#set up empty df with just lsoa names to receive results.  
dfmeans <- as.data.frame(lsoa1$LSOA11CD)

#loop here
for(k in 2:ncol(lsoa1)){
  #variable we are dealing with
  namei <- namesvec[k]
  #get the results from the list
  tempdf <- as.data.frame(resultList[[k]])
  #get the variable we want
  #create result in df means
  #note the := notation
  #explained here
  #https://stackoverflow.com/questions/26003574/use-dynamic-variable-names-in-dplyr
  dfmeans <- dfmeans %>% mutate(!!namei := tempdf$mean) 
}


#------------ best estimate of Standard error of mean as a proportion --------
# create an lsoa dataset which contains all the mean columns

#setting up for a loop
#namesvec <- paste0("MR_",names(lsoa1)) # the MR prefix is to tell us its mean result
namesvec <- names(lsoa1) # keep with just variable names - then existing mapping code doesn't need name changes
#set up empty df with just lsoa names to receive results.  
dfsepc <- as.data.frame(lsoa1$LSOA11CD)

#loop here
for(k in 2:ncol(lsoa1)){
  #variable we are dealing with
  namei <- namesvec[k]
  #get the results from the list
  tempdf <- as.data.frame(resultList[[k]])
  #get the variable we want
  #create result in df means
  #note the := notation
  #explained here
  #https://stackoverflow.com/questions/26003574/use-dynamic-variable-names-in-dplyr
  dfsepc <- dfsepc %>% mutate(!!namei := tempdf$seMean_prMean) 
  
}

#save the results
write_csv(dfmeans,paste0("Results", datetime, "/multidrawmeans.csv"))
write_csv(dfsepc,paste0("Results", datetime, "/multidrawSE_propn.csv"))






#------------- get the best estimate of summaries of individuals all runs ----- 



#read in all the individual summaries for the different draws as a loop
for(i in 1:length(seeds)){
  assign(paste0("s",i),read_csv(paste0("Results",datetime,"/","summaryMDO1vars",seeds[i],"_",datetime,".csv")))
}

#make a dataframe with the vriable name in column 1 
#and the mean value for all individuals in each draw in subsequent columns
sdf <- data.frame(s1$namesvec,s1$Mean,s2$Mean,s3$Mean,s4$Mean,s5$Mean,s6$Mean,s7$Mean,s8$Mean)

#use apply to get the mean or sd of each row
sdf$meanresult <- apply(sdf[,2:9],MARGIN = 1,FUN = mean)
sdf$sdev <- apply(sdf[,2:9],MARGIN = 1,FUN = sd)

write_csv(sdf,paste0("Results", datetime, "/summary8drawindividuals.csv"))



#------------- summary of all individuals by OAC supergroup ------------------ 

#read in all the individual summaries for the different draws as a loop
for(i in 1:length(seeds)){
  assign(paste0("s",i),read_csv(paste0("Results",datetime,"/","MDO1_Supergroup",seeds[i],"_",datetime,".csv")))
}


sAll <- s1[,2:76]+s2[,2:76]+s3[,2:76]+s4[,2:76]+s5[,2:76]+s6[,2:76]+s7[,2:76]+s8[,2:76]
smean = sAll / 8
smean$Supergroup_Code = s1$Supergroup_Code


write_csv(smean,paste0("Results", datetime, "/summary8drawindividuals_Supergroup.csv"))



#----------- summary of all individuals by age and gender ------------- 

#read in all the individual summaries for the different draws as a loop
for(i in 1:length(seeds)){
  assign(paste0("s",i),read_csv(paste0("Results",datetime,"/","MDO1_sexage",seeds[i],"_",datetime,".csv")))
}

#very simply add all the dfs together and divide by the number of draws
sAll <- s1[,2:76]+s2[,2:76]+s3[,2:76]+s4[,2:76]+s5[,2:76]+s6[,2:76]+s7[,2:76]+s8[,2:76]
smean = sAll / 8
smean$sexage = s1$sexageconst
# 

write_csv(sAll,paste0("Results", datetime, "/summary8drawindividuals_sexage.csv"))



