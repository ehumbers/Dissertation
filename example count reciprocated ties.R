#make up some data
id<-1:10
friend1<-c(2,4,6,7,8,5,5,9,10,1)
friend2<-c(3,1,7,10,2,3,9,8,2,9)
friend3<-c(10,8,9,2,4,4,4,7,8,4)
friend4<-c(5,8,2,1,10,7,3,10,4,2)

#create network data frame
mydata<-data.frame(id, friend1, friend2, friend3, friend4)

#make sure ID is a factor
mydata$id<-as.factor(mydata$id)

#change data to long form
require(tidyr)
longdata<-gather(mydata, nom, friendid, friend1:friend4)

#rename variables
names(longdata)[names(longdata)=="id"] <- "focal"
names(longdata)[names(longdata)=="friendid"] <- "id"

#merge with wide dataframe
longdata<-merge(longdata, mydata, by="id", all=TRUE)

#create rtie variable
longdata$rtie <- ifelse((longdata$focal==longdata$friend1 | 
                         longdata$focal==longdata$friend2 | 
                         longdata$focal==longdata$friend3 | 
                         longdata$focal==longdata$friend4), 1, 0)

#total reciprocated ties
require(dplyr)
df<-longdata %>% group_by(id) %>%
        summarise(rtie=sum(rtie)) %>%
        select(id, rtie)

#unload dplyr
detach("package:tidyr", unload=TRUE)
detach("package:dplyr", unload=TRUE)
