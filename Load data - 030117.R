##List of dissertation analysis scripts & most recent version
#1 Load Data - 030117
#2 Reciprocated Ties - 032917
#3 Clean - 030117
#4 Analysis RQ1 Full Sample 032817



#clear global environment
rm(list=ls())

#set working directory
setwd("/Volumes/encrypted_addhealth/r_wd")

#open packages
require(plyr)
require(eeptools)
require(RCurl)
require(psych)
require(foreign)
require(car)
require(survival)
require(mice)
require(lme4)
require(arm)
require(car)
require(MASS)
require(TukeyC)
require(glmmML)
require(optimx)
require(stargazer)
require(caret)
#NOTE: dplyr also needed but breaks count function. 
#suggested to use summarise but that doesn't work with my character/date variables

#load data
inschool <-  read.xport("/Volumes/encrypted_addhealth/unzipped/Inschool.xpt")
wave1 <- read.xport("/Volumes/encrypted_addhealth/unzipped/allwave1.xpt")
wave2 <- read.xport("/Volumes/encrypted_addhealth/unzipped/wave2.xpt")
wave3 <- read.xport("/Volumes/encrypted_addhealth/unzipped/wave3.xpt")
network <- read.xport("/Volumes/encrypted_addhealth/unzipped/network.xpt")
schinfo <- read.xport("/Volumes/encrypted_addhealth/unzipped/Schinfo.xpt")
schadmin <- read.xport("/Volumes/encrypted_addhealth/unzipped/Schadm1.xpt")
friends2 <- read.xport("/Volumes/encrypted_addhealth/unzipped/sfriend.xpt")

#load functions
age_years <- function(earlier, later)
{
        lt <- data.frame(earlier, later)
        age <- as.numeric(format(lt[,2],format="%Y")) - as.numeric(format(lt[,1],format="%Y"))
        
        dayOnLaterYear <- ifelse(format(lt[,1],format="%m-%d")!="02-29",
                                 as.Date(paste(format(lt[,2],format="%Y"),"-",format(lt[,1],format="%m-%d"),sep="")),
                                 ifelse(as.numeric(format(later,format="%Y")) %% 400 == 0 | as.numeric(format(later,format="%Y")) %% 100 != 0 & as.numeric(format(later,format="%Y")) %% 4 == 0,
                                        as.Date(paste(format(lt[,2],format="%Y"),"-",format(lt[,1],format="%m-%d"),sep="")),
                                        as.Date(paste(format(lt[,2],format="%Y"),"-","02-28",sep=""))))
        
        age[which(dayOnLaterYear > lt$later)] <- age[which(dayOnLaterYear > lt$later)] - 1
        
        age
}

overdisp_fun <- function(model) {
        ## number of variance parameters in 
        ##   an n-by-n variance-covariance matrix
        vpars <- function(m) {
                nrow(m)*(nrow(m)+1)/2
        }
        ##next two lines calculate the residual degrees of freedom
        model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
        rdf <- nrow(model.frame(model))-model.df
        #extract the Pearson residuals
        rp <- residuals(model,type="pearson")
        Pearson.chisq <- sum(rp^2)
        prat <- Pearson.chisq/rdf
        #generate a p-value. if less than ,05, the data are overdispersed.
        pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
        c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
