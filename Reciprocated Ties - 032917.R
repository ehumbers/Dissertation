#friends data only has questionnaire id, merge with in school to get AID
friendsinschool <- merge(friends2, inschool, by="SQID")

#keep only variables of interest
friendvars <- c( "AID","MF1AID", "MF2AID", "MF3AID", "MF4AID", "MF5AID",
                 "FF1AID", "FF2AID", "FF3AID", "FF4AID", "FF5AID")
friends<-friendsinschool[friendvars]

#make sure ID is a factor
friends$AID<-as.factor(friends$AID)

#change data to long form
require(tidyr)
longfriends<-gather(friends, nom, friendid, MF1AID:FF5AID)

#rename variables
names(longfriends)[names(longfriends)=="AID"] <- "focal"
names(longfriends)[names(longfriends)=="friendid"] <- "AID"

#merge with wide friends dataframe
longfriends<-merge(longfriends, friends, by="AID", all=TRUE)

#create rtie variable
longfriends$rtie <- ifelse((longfriends$focal==longfriends$MF1AID | 
                              longfriends$focal==longfriends$MF2AID | 
                              longfriends$focal==longfriends$MF3AID | 
                              longfriends$focal==longfriends$MF4AID |
                              longfriends$focal==longfriends$MF5AID | 
                              longfriends$focal==longfriends$FF1AID | 
                              longfriends$focal==longfriends$FF2AID | 
                              longfriends$focal==longfriends$FF3AID | 
                              longfriends$focal==longfriends$FF4AID | 
                              longfriends$focal==longfriends$FF5AID ), 1, 0)

#total reciprocated ties
require(dplyr)
recipfriends<-longfriends %>% group_by(AID) %>%
  summarise(rtie=sum(rtie, na.rm=TRUE)) %>%
  select(AID, rtie)

#unload dplyr bc it blocks count() function
detach("package:tidyr", unload=TRUE)
detach("package:dplyr", unload=TRUE)
