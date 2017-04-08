
#MERGE SCHOOL INFO
#school info has the survey administration date
#rename school id column
names(schinfo)[names(schinfo) == 'SCID'] <- 'SSCHLCDE'
inschool <- merge(schinfo, inschool, by="SSCHLCDE")

#merge school admin survey with in school
names(schadmin)[names(schadmin)=='ASCHLCDE'] <- 'SSCHLCDE'
inschool <- merge(schadmin, inschool, by="SSCHLCDE")

#SCHOOL LEVEL VARIABLES
#make dummy for attend school that provides childcare
inschool$childcare <- ifelse(inschool$A28Q == 1, 1, 0)
inschool$childcare <- ifelse(inschool$A28Q==2, 1, inschool$childcare)
count(inschool$childcare)

#make dummy for alternative placement
inschool$altplace <-ifelse(inschool$A29A==0, 1, 0)
inschool$altplace <- ifelse(inschool$A29B==1, 1, inschool$altplace)
inschool$altplace <- ifelse(inschool$A29C==1, 1, inschool$altplace)
count(inschool$altplace)

##IN SCHOOL VARIABLES

##dummy for having inschool survey
inschool$hasinsch <- 1
count(inschool$hasinsch)

#in school survey date
#clean month
inschool$SCHMONTHcl <- ifelse(inschool$SCHMONTH == 1, "01", inschool$SCHMONTH)
inschool$SCHMONTHcl <- ifelse(inschool$SCHMONTH == 2, "02", inschool$SCHMONTHcl)
inschool$SCHMONTHcl <- ifelse(inschool$SCHMONTH == 3, "03", inschool$SCHMONTHcl)
inschool$SCHMONTHcl <- ifelse(inschool$SCHMONTH == 4, "04", inschool$SCHMONTHcl)
inschool$SCHMONTHcl <- ifelse(inschool$SCHMONTH == 10, "10", inschool$SCHMONTHcl)
inschool$SCHMONTHcl <- ifelse(inschool$SCHMONTH == 11, "11", inschool$SCHMONTHcl)
inschool$SCHMONTHcl <- ifelse(inschool$SCHMONTH == 12, "12", inschool$SCHMONTHcl)
count(inschool$SCHMONTHcl)
#clean year
inschool$SCHYEARcl <- ifelse(inschool$SCHYEAR == 94, "1994", inschool$SCHYEAR)
inschool$SCHYEARcl <- ifelse(inschool$SCHYEAR == 95, "1995", inschool$SCHYEARcl)
count(inschool$SCHYEARcl)
#paste together & as.Date to make in-school survey date
inschool$inschsurveydate<- paste(inschool$SCHYEARcl, "01", inschool$SCHMONTHcl, sep="/")
inschool$inschsurveydate<- as.Date(inschool$inschsurveydate, "%Y/%d/%m")
count(inschool$inschsurveydate)

#age
inschool$S1cl <-ifelse(inschool$S1==99, NA, inschool$S1)
#make dummy for any NA
inschool$S1na <-ifelse(is.na(inschool$S1cl), 1, 0)

#females (1 = female, 0 = male)
inschool$S2cl <-ifelse(inschool$S2 == 9, NA, inschool$S2)
inschool$S2cl <- ifelse(inschool$S2 ==2, 1, inschool$S2cl)
inschool$S2cl <- ifelse(inschool$S2==1, 0, inschool$S2cl)
#make dummy for any NA
inschool$S2na <-ifelse(is.na(inschool$S2cl), 1, 0)
count(inschool$S2cl)

#race
#white - clean
count(inschool$S6A)
names(inschool)[names(inschool) == 'S6A'] <- 'S6Acl'
#black - clean
count(inschool$S6B)
names(inschool)[names(inschool) == 'S6B'] <- 'S6Bcl'
#other race
inschool$otherraceinsch <- inschool$S6C
inschool$otherraceinsch <- ifelse(inschool$S6D == 1, 1, inschool$otherraceinsch)
inschool$otherraceinsch <- ifelse(inschool$S6E == 1, 1, inschool$otherraceinsch)
count(inschool$otherraceinsch)
#identify multiple race identifiers and change to other race
inschool$multiraceinsch <- (inschool$S6A==1 & inschool$S6B==1) | (inschool$S6A==1 & inschool$otherraceinsch == 1) | (inschool$S6B==1 & inschool$otherraceinsch == 1) 
count(inschool$multiraceinsch)
inschool$S6Acl <- ifelse(inschool$multiraceinsch==TRUE, 0, inschool$S6A)
inschool$S6Bcl <- ifelse(inschool$multiraceinsch==TRUE, 0, inschool$S6B)
inschool$otherraceinsch <- ifelse(inschool$multiraceinsch==TRUE, 1, inschool$otherraceinsch)

#check race missingness
length(which(is.na(inschool$S6Acl)))
length(which(is.na(inschool$S6Bcl)))
length(which(is.na(inschool$otherraceinsch)))
#check that race variables are mutually exclusive
r1<-(inschool$S6Acl==1 & inschool$S6Bcl==1)
r2<-(inschool$S6Acl==1 & inschool$otherraceinsch==1)
r3<-(inschool$otherraceinsch==1 & inschool$S6Bcl==1)
count(r1)
count(r2)
count(r3)

#years in current school
inschool$S9cl <-ifelse(inschool$S9==99, NA, inschool$S9)

#attend a school without grades = S10
count(inschool$S10)
names(inschool)[names(inschool) == 'S10'] <- 'S10cl'

#gpa
#english
inschool$enggrade <- ifelse(inschool$S10A==1, 4, NA)
inschool$enggrade <- ifelse(inschool$S10A==2, 3, inschool$enggrade)
inschool$enggrade <- ifelse(inschool$S10A==3, 2, inschool$enggrade)
inschool$enggrade <- ifelse(inschool$S10A==4, 1, inschool$enggrade)
#math
inschool$mathgrade <- ifelse(inschool$S10B==1, 4, NA)
inschool$mathgrade <- ifelse(inschool$S10B==2, 3, inschool$mathgrade)
inschool$mathgrade <- ifelse(inschool$S10B==3, 2, inschool$mathgrade)
inschool$mathgrade <- ifelse(inschool$S10B==4, 1, inschool$mathgrade)
#history/social studies
inschool$hisgrade <- ifelse(inschool$S10C==1, 4, NA)
inschool$hisgrade <- ifelse(inschool$S10C==2, 3, inschool$hisgrade)
inschool$hisgrade <- ifelse(inschool$S10C==3, 2, inschool$hisgrade)
inschool$hisgrade <- ifelse(inschool$S10C==4, 1, inschool$hisgrade)
#science
inschool$scigrade <- ifelse(inschool$S10D==1, 4, NA)
inschool$scigrade <- ifelse(inschool$S10D==2, 3, inschool$scigrade)
inschool$scigrade <- ifelse(inschool$S10D==3, 2, inschool$scigrade)
inschool$scigrade <- ifelse(inschool$S10D==4, 1, inschool$scigrade)
#make GPA variable
gpainschvars<-c("enggrade", "mathgrade", "hisgrade", "scigrade")
inschool$gpainsch<-rowMeans(inschool[gpainschvars], na.rm=TRUE)
#check that it worked
gpacheck <-c("scigrade", "enggrade", "mathgrade", "hisgrade", "gpainsch")
gpacheck<-inschool[gpacheck]


#mother education - dummy for college graduate - 1 = yes, 0 = no
inschool$momed <- ifelse(inschool$S12==1, 0, NA)
inschool$momed <- ifelse(inschool$S12==2, 0, inschool$momed)
inschool$momed <- ifelse(inschool$S12==3, 0, inschool$momed)
inschool$momed <- ifelse(inschool$S12==4, 0, inschool$momed)
inschool$momed <- ifelse(inschool$S12==5, 0, inschool$momed)
inschool$momed <- ifelse(inschool$S12==6, 0, inschool$momed)
inschool$momed <- ifelse(inschool$S12==7, 1, inschool$momed)
inschool$momed <- ifelse(inschool$S12==8, 1, inschool$momed)
inschool$momed <- ifelse(inschool$S12==10, 0, inschool$momed)
count(inschool$momed)
#make dummy for NA
inschool$momedna <- ifelse(is.na(inschool$momed), 1, 0)

#father education
inschool$daded <- ifelse(inschool$S18==1, 0, NA)
inschool$daded <- ifelse(inschool$S18==2, 0, inschool$daded)
inschool$daded <- ifelse(inschool$S18==3, 0, inschool$daded)
inschool$daded <- ifelse(inschool$S18==4, 0, inschool$daded)
inschool$daded <- ifelse(inschool$S18==5, 0, inschool$daded)
inschool$daded <- ifelse(inschool$S18==6, 0, inschool$daded)
inschool$daded <- ifelse(inschool$S18==7, 1, inschool$daded)
inschool$daded <- ifelse(inschool$S18==8, 1, inschool$daded)
inschool$daded <- ifelse(inschool$S18==10, 0, inschool$daded)
count(inschool$daded)
#make dummy for NA
inschool$dadedna <- ifelse(is.na(inschool$daded), 1, 0)

#one parent college dummy - 1=yes college, 0 = less than college
inschool$parented <- ifelse(is.na(inschool$momed), inschool$daded, inschool$momed)
count(inschool$parented)
#make dummy for NA
inschool$parentedna <- ifelse(is.na(inschool$parented), 1, 0)

#mother figure - 1 = yes, 0 = no mother figure
inschool$S11cl <- ifelse(inschool$S11==9, NA, inschool$S11)
count(inschool$S11cl)

#presence of father figure
inschool$S17cl <- ifelse(inschool$S17 ==9, NA, inschool$S17)
count(inschool$S17cl)

#no parental figure dummy - 1=true, 0=false
inschool$noparent <- inschool$S17cl==0 & inschool$S11cl==0
inschool$noparent <- ifelse(inschool$noparent == TRUE, 1, 0)
#no parent missing dummy
inschool$noparentna <- ifelse(is.na(inschool$noparent), 1, 0)
count(inschool$noparentna)

#deliquency behaviors - S59A-G, 1 = 1 or twice a week or more, 0 = 2 or 3 a month or less
#smoke
inschool$S59Acl <-ifelse(inschool$S59A==99, NA, inschool$S59A)
count(inschool$S59Acl)

#drink - S59B
inschool$S59Bcl <-ifelse(inschool$S59B==99, NA, inschool$S59B)
count(inschool$S59Bcl)

#get drunk - S59C
inschool$S59Ccl <-ifelse(inschool$S59C==99, NA, inschool$S59C)
count(inschool$S59Ccl)

#race - S59D
inschool$S59Dcl <-ifelse(inschool$S59D==99, NA, inschool$S59D)
count(inschool$S59Dcl)

#danger bc of dare - S59E
inschool$S59Ecl <-ifelse(inschool$S59E==99, NA, inschool$S59E)
count(inschool$S59Ecl)

#lie - S59F
inschool$S59Fcl <-ifelse(inschool$S59F==99, NA, inschool$S59F)
count(inschool$S59Fcl)

#skip - S59G
inschool$S59Gcl <-ifelse(inschool$S59G==99, NA, inschool$S59G)
count(inschool$S59Gcl)

#stigma
#item - feel socially accepted
# 1 = disagree/strongly disagree, 0 = neither agree disagree, agree, strongly agree
inschool$S62Ocl <-ifelse(inschool$S62O==4, 1, 0)
inschool$S62Ocl <- ifelse(inschool$S62O==5, 1, inschool$S62Ocl)
count(inschool$S62Ocl)

##WAVE 1

#dummy for having wave 1
wave1$hasw1 <- 1

#ever pregnant
#have you ever been pregnant
#6 = refused, 8 = don't know
unique(wave1$H1FP7)
wave1$H1FP7cl <- ifelse(wave1$H1FP7 == 6, NA, wave1$H1FP7)
wave1$H1FP7cl <- ifelse(wave1$H1FP7 == 7, 0, wave1$H1FP7cl)
wave1$H1FP7cl <- ifelse(wave1$H1FP7 == 8, NA, wave1$H1FP7cl)
count(wave1$H1FP7cl)
#missing ever pregnant variable, 1=yes 0=no
wave1$H1FP7na <- ifelse(is.na(wave1$H1FP7cl), 1, 0)

##most recent pregnancy date
#make month variable into characters for as.Date
wave1$H1FP9Mcl <- ifelse(wave1$H1FP9M == 1, "01", wave1$H1FP9M)
wave1$H1FP9Mcl <- ifelse(wave1$H1FP9M == 2, "02", wave1$H1FP9Mcl)
wave1$H1FP9Mcl <- ifelse(wave1$H1FP9M == 3, "03", wave1$H1FP9Mcl)
wave1$H1FP9Mcl <- ifelse(wave1$H1FP9M == 4, "04", wave1$H1FP9Mcl)
wave1$H1FP9Mcl <- ifelse(wave1$H1FP9M == 5, "05", wave1$H1FP9Mcl)
wave1$H1FP9Mcl <- ifelse(wave1$H1FP9M == 6, "06", wave1$H1FP9Mcl)
wave1$H1FP9Mcl <- ifelse(wave1$H1FP9M == 7, "07", wave1$H1FP9Mcl)
wave1$H1FP9Mcl <- ifelse(wave1$H1FP9M == 8, "08", wave1$H1FP9Mcl)
wave1$H1FP9Mcl <- ifelse(wave1$H1FP9M == 9, "09", wave1$H1FP9Mcl)
wave1$H1FP9Mcl <- ifelse(wave1$H1FP9M == 10, "10", wave1$H1FP9Mcl)
wave1$H1FP9Mcl <- ifelse(wave1$H1FP9M == 11, "11", wave1$H1FP9Mcl)
wave1$H1FP9Mcl <- ifelse(wave1$H1FP9M == 12, "12", wave1$H1FP9Mcl)
wave1$H1FP9Mcl <- ifelse(wave1$H1FP9M == 96, NA, wave1$H1FP9Mcl)
wave1$H1FP9Mcl <- ifelse(wave1$H1FP9M == 97, NA, wave1$H1FP9Mcl)
wave1$H1FP9Mcl <- ifelse(wave1$H1FP9M == 98, NA, wave1$H1FP9Mcl)
wave1$H1FP9Mcl <- ifelse(wave1$H1FP9M == 99, NA, wave1$H1FP9Mcl)
unique(wave1$H1FP9Mcl)
#make preg year 
wave1$H1FP9Ycl <- ifelse(wave1$H1FP9Y == 96, NA, wave1$H1FP9Y)
wave1$H1FP9Ycl <- ifelse(wave1$H1FP9Y == 97, NA, wave1$H1FP9Ycl)
wave1$H1FP9Ycl <- ifelse(wave1$H1FP9Y == 98, NA, wave1$H1FP9Ycl)
wave1$H1FP9Ycl <- ifelse(wave1$H1FP9Y == 99, NA, wave1$H1FP9Ycl)
#make year into character for as.date
wave1$pregyear1 <- paste("19", wave1$H1FP9Ycl, sep="")
unique(wave1$pregyear1)
#make preg date
wave1$pregdate1<- paste(wave1$pregyear1, "01", wave1$H1FP9Mcl, sep="/")
#View(wave1$pregdate1)
wave1$pregdate1 <- as.Date(wave1$pregdate1, "%Y/%d/%m")
#View(wave1$pregdate1)

#how many times pregnant
#note - wave 1 stops at 4 or more
unique(wave1$H1FP8)
wave1$H1FP8cl <- ifelse(wave1$H1FP8 == 96, NA, wave1$H1FP8)
wave1$H1FP8cl <- ifelse(wave1$H1FP8 == 97, 0, wave1$H1FP8cl)
wave1$H1FP8cl <- ifelse(wave1$H1FP8 == 98, NA, wave1$H1FP8cl)
wave1$H1FP8cl <- ifelse(wave1$H1FP8 == 99, NA, wave1$H1FP8cl)
unique(wave1$H1FP8cl)

#clean date of multiple pregnancy
#month
wave1$H1FP11M1cl <-ifelse(wave1$H1FP11M1==1, "01", wave1$H1FP11M1)
wave1$H1FP11M1cl <-ifelse(wave1$H1FP11M1==3, "03", wave1$H1FP11M1cl)
wave1$H1FP11M1cl <-ifelse(wave1$H1FP11M1==4, "04", wave1$H1FP11M1cl)
wave1$H1FP11M1cl <-ifelse(wave1$H1FP11M1==6, "06", wave1$H1FP11M1cl)
wave1$H1FP11M1cl <-ifelse(wave1$H1FP11M1==7, "07", wave1$H1FP11M1cl)
wave1$H1FP11M1cl <-ifelse(wave1$H1FP11M1==9, "09", wave1$H1FP11M1cl)
wave1$H1FP11M1cl <-ifelse(wave1$H1FP11M1==11, "11", wave1$H1FP11M1cl)
wave1$H1FP11M1cl <-ifelse(wave1$H1FP11M1==97, NA, wave1$H1FP11M1cl)
wave1$H1FP11M1cl <-ifelse(wave1$H1FP11M1==98, NA, wave1$H1FP11M1cl)
unique(wave1$H1FP11M1cl)
#year
wave1$H1FP11Y1cl <-ifelse(wave1$H1FP11Y1==94, "1994", wave1$H1FP11Y1)
wave1$H1FP11Y1cl <-ifelse(wave1$H1FP11Y1==95, "1995", wave1$H1FP11Y1cl)
wave1$H1FP11Y1cl <-ifelse(wave1$H1FP11Y1==97, NA, wave1$H1FP11Y1cl)
wave1$H1FP11Y1cl <-ifelse(wave1$H1FP11Y1==98, NA, wave1$H1FP11Y1cl)
unique(wave1$H1FP11Y1cl)
#make into date
wave1$multipregdate1 <-paste(wave1$H1FP11Y1cl, "01", wave1$H1FP11M1cl, sep="/")
wave1$multipregdate1 <-as.Date(wave1$multipregdate1, "%Y/%d/%m")

#mother education
#dummy coded for college graduate, 1 = college grad
#mother ed - child reported
wave1$H1RM1cl <- ifelse(wave1$H1RM1 == 1, 0, wave1$H1RM1)
wave1$H1RM1cl <- ifelse(wave1$H1RM1 == 2, 0, wave1$H1RM1cl)
wave1$H1RM1cl <- ifelse(wave1$H1RM1 == 3, 0, wave1$H1RM1cl)
wave1$H1RM1cl <- ifelse(wave1$H1RM1 == 4, 0, wave1$H1RM1cl)
wave1$H1RM1cl <- ifelse(wave1$H1RM1 == 5, 0, wave1$H1RM1cl)
wave1$H1RM1cl <- ifelse(wave1$H1RM1 == 6, 0, wave1$H1RM1cl)
wave1$H1RM1cl <- ifelse(wave1$H1RM1 == 7, 0, wave1$H1RM1cl)
wave1$H1RM1cl <- ifelse(wave1$H1RM1 == 8, 1, wave1$H1RM1cl)
wave1$H1RM1cl <- ifelse(wave1$H1RM1 == 9, 1, wave1$H1RM1cl)
wave1$H1RM1cl <- ifelse(wave1$H1RM1 == 10, 0, wave1$H1RM1cl)
wave1$H1RM1cl <- ifelse(wave1$H1RM1 == 11, NA, wave1$H1RM1cl)
wave1$H1RM1cl <- ifelse(wave1$H1RM1 == 12, NA, wave1$H1RM1cl)
wave1$H1RM1cl <- ifelse(wave1$H1RM1 == 13, NA, wave1$H1RM1cl)
wave1$H1RM1cl <- ifelse(wave1$H1RM1 == 96, NA, wave1$H1RM1cl)
wave1$H1RM1cl <- ifelse(wave1$H1RM1 == 97, NA, wave1$H1RM1cl)
wave1$H1RM1cl <- ifelse(wave1$H1RM1 == 98, NA, wave1$H1RM1cl)
count(wave1$H1RM1cl)

#father education - child reported
#father ed - child reported
wave1$H1RF1cl <- ifelse(wave1$H1RF1 == 1, 0, wave1$H1RF1)
wave1$H1RF1cl <- ifelse(wave1$H1RF1 == 2, 0, wave1$H1RF1cl)
wave1$H1RF1cl <- ifelse(wave1$H1RF1 == 3, 0, wave1$H1RF1cl)
wave1$H1RF1cl <- ifelse(wave1$H1RF1 == 4, 0, wave1$H1RF1cl)
wave1$H1RF1cl <- ifelse(wave1$H1RF1 == 5, 0, wave1$H1RF1cl)
wave1$H1RF1cl <- ifelse(wave1$H1RF1 == 6, 0, wave1$H1RF1cl)
wave1$H1RF1cl <- ifelse(wave1$H1RF1 == 7, 0, wave1$H1RF1cl)
wave1$H1RF1cl <- ifelse(wave1$H1RF1 == 8, 1, wave1$H1RF1cl)
wave1$H1RF1cl <- ifelse(wave1$H1RF1 == 9, 1, wave1$H1RF1cl)
wave1$H1RF1cl <- ifelse(wave1$H1RF1 == 10, 0, wave1$H1RF1cl)
wave1$H1RF1cl <- ifelse(wave1$H1RF1 == 11, NA, wave1$H1RF1cl)
wave1$H1RF1cl <- ifelse(wave1$H1RF1 == 12, NA, wave1$H1RF1cl)
wave1$H1RF1cl <- ifelse(wave1$H1RF1 == 13, NA, wave1$H1RF1cl)
wave1$H1RF1cl <- ifelse(wave1$H1RF1 == 96, NA, wave1$H1RF1cl)
wave1$H1RF1cl <- ifelse(wave1$H1RF1 == 97, NA, wave1$H1RF1cl)
wave1$H1RF1cl <- ifelse(wave1$H1RF1 == 98, NA, wave1$H1RF1cl)
wave1$H1RF1cl <- ifelse(wave1$H1RF1 == 99, NA, wave1$H1RF1cl)
count(wave1$H1RF1cl)

#parent ed - parent reported
wave1$PA12cl <- ifelse(wave1$PA12 == 1, 0, wave1$PA12)
wave1$PA12cl <- ifelse(wave1$PA12 == 2, 0, wave1$PA12cl)
wave1$PA12cl <- ifelse(wave1$PA12 == 3, 0, wave1$PA12cl)
wave1$PA12cl <- ifelse(wave1$PA12 == 4, 0, wave1$PA12cl)
wave1$PA12cl <- ifelse(wave1$PA12 == 5, 0, wave1$PA12cl)
wave1$PA12cl <- ifelse(wave1$PA12 == 6, 0, wave1$PA12cl)
wave1$PA12cl <- ifelse(wave1$PA12 == 7, 0, wave1$PA12cl)
wave1$PA12cl <- ifelse(wave1$PA12 == 8, 1, wave1$PA12cl)
wave1$PA12cl <- ifelse(wave1$PA12 == 9, 1, wave1$PA12cl)
wave1$PA12cl <- ifelse(wave1$PA12 == 10, 0, wave1$PA12cl)
wave1$PA12cl <- ifelse(wave1$PA12 == 96, NA, wave1$PA12cl)
count(wave1$PA12cl)

#female, 1 = female, 0 = male, 6 = refused, 8 = don't know
wave1$BIO_SEXcl <- ifelse(wave1$BIO_SEX == 1, 0, wave1$BIO_SEX)
wave1$BIO_SEXcl <- ifelse(wave1$BIO_SEX == 2, 1, wave1$BIO_SEXcl)
wave1$BIO_SEXcl <- ifelse(wave1$BIO_SEX == 6, NA, wave1$BIO_SEXcl)
wave1$BIO_SEXcl <- ifelse(wave1$BIO_SEX == 8, NA, wave1$BIO_SEXcl)


#gpa
#english
wave1$enggrade1 <- ifelse(wave1$H1ED11==1, 4, NA)
wave1$enggrade1 <- ifelse(wave1$H1ED11==2, 3, wave1$enggrade1)
wave1$enggrade1 <- ifelse(wave1$H1ED11==3, 2, wave1$enggrade1)
wave1$enggrade1 <- ifelse(wave1$H1ED11==4, 1, wave1$enggrade1)
count(wave1$enggrade1)
#math
wave1$mathgrade1 <- ifelse(wave1$H1ED12==1, 4, NA)
wave1$mathgrade1 <- ifelse(wave1$H1ED12==2, 3, wave1$mathgrade1)
wave1$mathgrade1 <- ifelse(wave1$H1ED12==3, 2, wave1$mathgrade1)
wave1$mathgrade1 <- ifelse(wave1$H1ED12==4, 1, wave1$mathgrade1)
#history/social studies
wave1$hisgrade1 <- ifelse(wave1$H1ED13==1, 4, NA)
wave1$hisgrade1 <- ifelse(wave1$H1ED13==2, 3, wave1$hisgrade1)
wave1$hisgrade1 <- ifelse(wave1$H1ED13==3, 2, wave1$hisgrade1)
wave1$hisgrade1 <- ifelse(wave1$H1ED13==4, 1, wave1$hisgrade1)
#science
wave1$scigrade1 <- ifelse(wave1$H1ED14==1, 4, NA)
wave1$scigrade1 <- ifelse(wave1$H1ED14==2, 3, wave1$scigrade1)
wave1$scigrade1 <- ifelse(wave1$H1ED14==3, 2, wave1$scigrade1)
wave1$scigrade1 <- ifelse(wave1$H1ED14==4, 1, wave1$scigrade1)
#make GPA variable
gpaw1vars<-c("enggrade1", "mathgrade1", "hisgrade1", "scigrade1")
wave1$gpa1<-rowMeans(wave1[gpaw1vars], na.rm=TRUE)
##check that it worked
gpacheck2 <-c("scigrade1", "enggrade1", "mathgrade1", "hisgrade1", "gpa1")
gpacheck2<-wave1[gpacheck2]

#presence of a mother figure in household
wave1$nomom1 <- ifelse(wave1$H1RM2==7, 1, 0)

#presece of a father figure in household
wave1$nodad1 <- ifelse(wave1$H1RF2 ==7, 1, 0)

#no parent figure in household
wave1$noparent1 = wave1$nomom1 ==1 & wave1$nodad1==1
count(wave1$noparent1)

##WAVE 2

#dummy for having wave 2 survey
wave2$hasw2 <- 1

#sex
count(wave2$BIO_SEX2)
wave2$BIO_SEX2cl <-ifelse(wave2$BIO_SEX2==2, 1, 0)

#ever pregnant
#have you ever been pregnant
wave2$H2FP10cl <- ifelse(wave2$H2FP10 == 7, 0, wave2$H2FP10)
wave2$H2FP10cl <- ifelse(wave2$H2FP10 == 6, NA, wave2$H2FP10cl)
wave2$H2FP10cl <- ifelse(wave2$H2FP10 == 8, NA, wave2$H2FP10cl)
count(wave2$H2FP10cl)
#missing ever preg wave 2, 1=yes 0=no
wave2$H2FP10na <- ifelse(is.na(wave2$H2FP10cl), 1, 0)

#most recent pregnancy date
#month - change to characters for as.Date
wave2$H2FP12M1cl <- ifelse(wave2$H2FP12M1 == 1, "01", wave2$H2FP12M1)
wave2$H2FP12M1cl <- ifelse(wave2$H2FP12M1 == 2, "02", wave2$H2FP12M1cl)
wave2$H2FP12M1cl <- ifelse(wave2$H2FP12M1 == 3, "03", wave2$H2FP12M1cl)
wave2$H2FP12M1cl <- ifelse(wave2$H2FP12M1 == 4, "04", wave2$H2FP12M1cl)
wave2$H2FP12M1cl <- ifelse(wave2$H2FP12M1 == 5, "05", wave2$H2FP12M1cl)
wave2$H2FP12M1cl <- ifelse(wave2$H2FP12M1 == 6, "06", wave2$H2FP12M1cl)
wave2$H2FP12M1cl <- ifelse(wave2$H2FP12M1 == 7, "07", wave2$H2FP12M1cl)
wave2$H2FP12M1cl <- ifelse(wave2$H2FP12M1 == 8, "08", wave2$H2FP12M1cl)
wave2$H2FP12M1cl <- ifelse(wave2$H2FP12M1 == 9, "09", wave2$H2FP12M1cl)
wave2$H2FP12M1cl <- ifelse(wave2$H2FP12M1 == 10, "10", wave2$H2FP12M1cl)
wave2$H2FP12M1cl <- ifelse(wave2$H2FP12M1 == 11, "11", wave2$H2FP12M1cl)
wave2$H2FP12M1cl <- ifelse(wave2$H2FP12M1 == 12, "12", wave2$H2FP12M1cl)
wave2$H2FP12M1cl <- ifelse(wave2$H2FP12M1 == 96, NA, wave2$H2FP12M1cl)
wave2$H2FP12M1cl <- ifelse(wave2$H2FP12M1 == 97, NA, wave2$H2FP12M1cl)
wave2$H2FP12M1cl <- ifelse(wave2$H2FP12M1 == 98, NA, wave2$H2FP12M1cl)
count(wave2$H2FP12M1cl)
#year - as characters for as.Date
wave2$H2FP12Y1cl <- ifelse(wave2$H2FP12Y1 == 90, "1990", wave2$H2FP12Y1)
wave2$H2FP12Y1cl <- ifelse(wave2$H2FP12Y1 == 91, "1991", wave2$H2FP12Y1cl)
wave2$H2FP12Y1cl <- ifelse(wave2$H2FP12Y1 == 92, "1992", wave2$H2FP12Y1cl)
wave2$H2FP12Y1cl <- ifelse(wave2$H2FP12Y1 == 93, "1993", wave2$H2FP12Y1cl)
wave2$H2FP12Y1cl <- ifelse(wave2$H2FP12Y1 == 94, "1994", wave2$H2FP12Y1cl)
wave2$H2FP12Y1cl <- ifelse(wave2$H2FP12Y1 == 95, "1995", wave2$H2FP12Y1cl)
wave2$H2FP12Y1cl <- ifelse(wave2$H2FP12Y1 == 96, "1996", wave2$H2FP12Y1cl)
wave2$H2FP12Y1cl <- ifelse(wave2$H2FP12Y1 == 996, NA, wave2$H2FP12Y1cl)
wave2$H2FP12Y1cl <- ifelse(wave2$H2FP12Y1 == 997, NA, wave2$H2FP12Y1cl)
wave2$H2FP12Y1cl <- ifelse(wave2$H2FP12Y1 == 998, NA, wave2$H2FP12Y1cl)
unique(wave2$H2FP12Y1cl)
#make preg date
wave2$pregdate2<- paste(wave2$H2FP12Y1cl, "01", wave2$H2FP12M1cl, sep="/")
#View(wave2$pregdate2)
wave2$pregdate2 <- as.Date(wave2$pregdate2, "%Y/%d/%m")
#View(wave2$pregdate2)

#multiple times pregnant
#how many times pregnant
count(wave2$H2FP11)
wave2$H2FP11cl <- ifelse(wave2$H2FP11 == 97, 0, wave2$H2FP11)
wave2$H2FP11cl <- ifelse(wave2$H2FP11 == 96, NA, wave2$H2FP11cl)
wave2$H2FP11cl <- ifelse(wave2$H2FP11 == 98, NA, wave2$H2FP11cl)
count(wave2$H2FP11cl)

#clean multiple pregnancy date
#month
wave2$H2FP12M2cl <- ifelse(wave2$H2FP12M2 ==1, "01", wave2$H2FP12M2)
wave2$H2FP12M2cl <- ifelse(wave2$H2FP12M2 ==2, "02", wave2$H2FP12M2cl)
wave2$H2FP12M2cl <- ifelse(wave2$H2FP12M2 ==3, "03", wave2$H2FP12M2cl)
wave2$H2FP12M2cl <- ifelse(wave2$H2FP12M2 ==4, "04", wave2$H2FP12M2cl)
wave2$H2FP12M2cl <- ifelse(wave2$H2FP12M2 ==5, "05", wave2$H2FP12M2cl)
wave2$H2FP12M2cl <- ifelse(wave2$H2FP12M2 ==6, "06", wave2$H2FP12M2cl)
wave2$H2FP12M2cl <- ifelse(wave2$H2FP12M2 ==7, "07", wave2$H2FP12M2cl)
wave2$H2FP12M2cl <- ifelse(wave2$H2FP12M2 ==8, "08", wave2$H2FP12M2cl)
wave2$H2FP12M2cl <- ifelse(wave2$H2FP12M2 ==9, "09", wave2$H2FP12M2cl)
wave2$H2FP12M2cl <- ifelse(wave2$H2FP12M2 ==10, "10", wave2$H2FP12M2cl)
wave2$H2FP12M2cl <- ifelse(wave2$H2FP12M2 ==11, "11", wave2$H2FP12M2cl)
wave2$H2FP12M2cl <- ifelse(wave2$H2FP12M2 ==12, "12", wave2$H2FP12M2cl)
wave2$H2FP12M2cl <- ifelse(wave2$H2FP12M2 ==97, NA, wave2$H2FP12M2cl)
wave2$H2FP12M2cl <- ifelse(wave2$H2FP12M2 ==96, NA, wave2$H2FP12M2cl)
wave2$H2FP12M2cl <- ifelse(wave2$H2FP12M2 ==98, NA, wave2$H2FP12M2cl)
unique(wave2$H2FP12M2cl)
#year
wave2$H2FP12Y2cl <-ifelse(wave2$H2FP12Y2==94, "1994", wave2$H2FP12Y2)
wave2$H2FP12Y2cl <-ifelse(wave2$H2FP12Y2==95, "1995", wave2$H2FP12Y2cl)
wave2$H2FP12Y2cl <-ifelse(wave2$H2FP12Y2==96, "1996", wave2$H2FP12Y2cl)
wave2$H2FP12Y2cl <-ifelse(wave2$H2FP12Y2==996, NA, wave2$H2FP12Y2cl)
wave2$H2FP12Y2cl <-ifelse(wave2$H2FP12Y2==997, NA, wave2$H2FP12Y2cl)
wave2$H2FP12Y2cl <-ifelse(wave2$H2FP12Y2==998, NA, wave2$H2FP12Y2cl)
count(wave2$H2FP12Y2cl)

#make into date
wave2$multipregdate2.2nd <-paste(wave2$H2FP12Y2cl, "01", wave2$H2FP12M2cl, sep="/")
wave2$multipregdate2.2nd <-as.Date(wave2$multipregdate2.2nd, "%Y/%d/%m")


#third preg date
wave2$H2FP12M3cl <- ifelse(wave2$H2FP12M3==3, "03", wave2$H2FP12M3)
wave2$H2FP12M3cl <- ifelse(wave2$H2FP12M3==5, "05", wave2$H2FP12M3cl)
wave2$H2FP12M3cl <- ifelse(wave2$H2FP12M3==6, "06", wave2$H2FP12M3cl)
wave2$H2FP12M3cl <- ifelse(wave2$H2FP12M3==97, NA, wave2$H2FP12M3cl)
wave2$H2FP12M3cl <- ifelse(wave2$H2FP12M3==98, NA, wave2$H2FP12M3cl)
count(wave2$H2FP12M3cl)
#year
count(wave2$H2FP12Y3)
wave2$H2FP12Y3cl <-ifelse(wave2$H2FP12Y3==94, "1994", wave2$H2FP12Y3)
wave2$H2FP12Y3cl <-ifelse(wave2$H2FP12Y3==997, NA, wave2$H2FP12Y3cl)
wave2$H2FP12Y3cl <-ifelse(wave2$H2FP12Y3==998, NA, wave2$H2FP12Y3cl)
wave2$H2FP12Y3cl <-ifelse(wave2$H2FP12Y3==95, "1995", wave2$H2FP12Y3cl)
count(wave2$H2FP12Y3cl)
#make into date
wave2$multipregdate2.3rd <-paste(wave2$H2FP12Y3cl, "01", wave2$H2FP12M3cl, sep="/")
wave2$multipregdate2.3rd <-as.Date(wave2$multipregdate2.3rd, "%Y/%d/%m")

#self reported gpa
#english
wave2$enggrade2 <- ifelse(wave2$H2ED7==1, 4, NA)
wave2$enggrade2 <- ifelse(wave2$H2ED7==2, 3, wave2$enggrade2)
wave2$enggrade2 <- ifelse(wave2$H2ED7==3, 2, wave2$enggrade2)
wave2$enggrade2 <- ifelse(wave2$H2ED7==4, 1, wave2$enggrade2)
count(wave2$enggrade2)
#math
wave2$mathgrade2 <- ifelse(wave2$H2ED8==1, 4, NA)
wave2$mathgrade2 <- ifelse(wave2$H2ED8==2, 3, wave2$mathgrade2)
wave2$mathgrade2 <- ifelse(wave2$H2ED8==3, 2, wave2$mathgrade2)
wave2$mathgrade2 <- ifelse(wave2$H2ED8==4, 1, wave2$mathgrade2)
#history/social studies
wave2$hisgrade2 <- ifelse(wave2$H2ED9==1, 4, NA)
wave2$hisgrade2 <- ifelse(wave2$H2ED9==2, 3, wave2$hisgrade2)
wave2$hisgrade2 <- ifelse(wave2$H2ED9==3, 2, wave2$hisgrade2)
wave2$hisgrade2 <- ifelse(wave2$H2ED9==4, 1, wave2$hisgrade2)
#science
wave2$scigrade2 <- ifelse(wave2$H2ED10==1, 4, NA)
wave2$scigrade2 <- ifelse(wave2$H2ED10==2, 3, wave2$scigrade2)
wave2$scigrade2 <- ifelse(wave2$H2ED10==3, 2, wave2$scigrade2)
wave2$scigrade2 <- ifelse(wave2$H2ED10==4, 1, wave2$scigrade2)
#make GPA variable
gpaw2vars<-c("enggrade2", "mathgrade2", "hisgrade2", "scigrade2")
wave2$gpa2<-rowMeans(wave2[gpaw2vars], na.rm=TRUE)
#check that it worked
gpacheck3 <-c("scigrade2", "enggrade2", "mathgrade2", "hisgrade2", "gpa2")
gpacheck3<-wave2[gpacheck3]

##WAVE 3

#dummy for having wave 3 survey
wave3$hasw3 <- 1

#high school diploma received
count(wave3$H3ED3)
wave3$H3ED3cl <- ifelse(wave3$H3ED3==6, NA, wave3$H3ED3)
wave3$H3ED3cl <- ifelse(wave3$H3ED3==9, NA, wave3$H3ED3cl)
wave3$H3ED3cl <- ifelse(wave3$H3ED3==8, NA, wave3$H3ED3cl)
count(wave3$H3ED3cl)

#some college - dummy 1 = at least one year of college or more as highest grade completed
wave3$H3ED1col <- ifelse(wave3$H3ED1 == 96, NA, wave3$H3ED1)
wave3$H3ED1col <- ifelse(wave3$H3ED1 == 98, NA, wave3$H3ED1col)
wave3$H3ED1col <- ifelse(wave3$H3ED1 == 99, NA, wave3$H3ED1col)
wave3$H3ED1col <- ifelse(wave3$H3ED1col < 13, 0, wave3$H3ED1col)
wave3$H3ED1col <- ifelse(wave3$H3ED1col >= 13, 1, wave3$H3ED1col)
count(wave3$H3ED1col)


###MERGE DATA FRAMES to make pregnancy variables and impute missing from other waves
#wave 1 and wave 2 keeping all participants
w1w2<-merge(wave1, wave2, by="AID", all=TRUE)
#merge inschool to wave 1 & wave 2 - keeping only those in both 
inschw1w2 <-merge(inschool, w1w2, by="AID")
#this drops anyone who did not have in school and at least w1 or w2

##PREGNANCY GROUP DUMMIES

#impute missing sex
#SEX
length(which(is.na(inschw1w2$S2cl)))
length(which(inschw1w2$S2na==1))
length(which(is.na(inschw1w2$BIO_SEXcl)))
inschw1w2$S2cl <- ifelse(is.na(inschw1w2$S2cl), inschw1w2$BIO_SEXcl, inschw1w2$S2cl)
count(inschw1w2$S2cl)
inschw1w2$S2cl <-ifelse(is.na(inschw1w2$S2cl), inschw1w2$BIO_SEX2cl, inschw1w2$S2cl)
count(inschw1w2$S2cl)

#never pregnant dummy
inschw1w2$neverpreg <- ((inschw1w2$H1FP7cl == 0 & inschw1w2$H2FP10cl == 0) | (is.na(inschw1w2$H1FP7cl) & inschw1w2$H2FP10cl == 0) | (inschw1w2$H1FP7cl == 0 & is.na(inschw1w2$H2FP10cl)))
count(inschw1w2$neverpreg)
inschw1w2$neverpreg<-ifelse(inschw1w2$neverpreg==T, 1, inschw1w2$neverpreg)
#visually check the variable
neverpreg_sub<-subset(inschw1w2, neverpreg==T, c("neverpreg", "H1FP7cl", "H2FP10cl"))

##pregnant at inschool
#Wave 1
inschw1w2$pregb4insch1 <- inschw1w2$inschsurveydate >= inschw1w2$pregdate1
count(inschw1w2$pregb4insch1)

#Wave 2
#calculate who was preg before in school (Wave 2 item)
inschw1w2$pregb4insch2 <- inschw1w2$inschsurveydate >= inschw1w2$pregdate2
count(inschw1w2$pregb4insch2)

#Preg before In-School composite
inschw1w2$preginsch<-((inschw1w2$pregb4insch1 == 1 & inschw1w2$pregb4insch2 == 1) | (is.na(inschw1w2$pregb4insch1) & inschw1w2$pregb4insch2 == 1) | (inschw1w2$pregb4insch1 == 1 & is.na(inschw1w2$pregb4insch2) | (inschw1w2$pregb4insch1 == 0 & inschw1w2$pregb4insch2 == 1) | (inschw1w2$pregb4insch1 == 1 & inschw1w2$pregb4insch2 == 0)))
inschw1w2$preginsch <-ifelse(is.na(inschw1w2$preginsch), 0, inschw1w2$preginsch)
count(inschw1w2$preginsch)

#future pregnant
inschw1w2$futurepreg <- (inschw1w2$preginsch == 0  & inschw1w2$neverpreg==0)
inschw1w2$futurepreg <- ifelse((is.na(inschw1w2$pregdate1) & is.na(inschw1w2$pregdate2)), 0, inschw1w2$futurepreg)
inschw1w2$futurepreg <- ifelse(is.na(inschw1w2$futurepreg), 0, inschw1w2$futurepreg)
count(inschw1w2$futurepreg)

#unknown preg
#anyone who reports pregnancy but no complete recent preg date
#wave1
inschw1w2$unknownpreg1 <- (inschw1w2$H1FP7cl == 1 & is.na(inschw1w2$pregdate1))
count(inschw1w2$unknownpreg1)
#wave2
inschw1w2$unknownpreg2 <- (inschw1w2$H2FP10cl == 1 & is.na(inschw1w2$pregdate2))
count(inschw1w2$unknownpreg2)

#composite of unknown preg
inschw1w2$unknownpreg <-(inschw1w2$unknownpreg2 ==TRUE | inschw1w2$unknownpreg1==TRUE)
inschw1w2$unknownpreg <-ifelse((!is.na(inschw1w2$pregdate1) | !is.na(inschw1w2$pregdate2)), 0, inschw1w2$unknownpreg) 
inschw1w2$unknownpreg <-ifelse(is.na(inschw1w2$unknownpreg), 0, inschw1w2$unknownpreg)
count(inschw1w2$unknownpreg)

#multiple pregnancies
#wave 1
#check dates of those with multiple pregnancy and most recent pregnancy since Jan 1 and most recent preg
inschw1w2$secpregb4insch1 <- inschw1w2$inschsurveydate > inschw1w2$multipregdate1
inschw1w2$secpregb4insch1 <- ifelse(is.na(inschw1w2$secpregb4insch1), 0, inschw1w2$secpregb4insch1)
count(inschw1w2$secpregb4insch1)
#intersection of those with dates before inschool for their second preg and those in the future preg group
#multipreg_sub1 <-subset(inschw1w2, secpregb4insch1==T & futurepreg==1, c("futurepreg", "secpregb4insch1", "pregdate1", "multipregdate1", "inschsurveydate"))
#17 need to be changed from future to inschool preg
#change to TRUE preg in school
count(inschw1w2$preginsch)
inschw1w2$preginsch <-ifelse(!is.na(inschw1w2$multipregdate1), inschw1w2$secpregb4insch1, inschw1w2$preginsch)
count(inschw1w2$preginsch)
#change to FALSE future preg
#reverse code secpregb4insch1
inschw1w2$nosecpregb4insch1 <- (1 + 0 - inschw1w2$secpregb4insch1)
count(inschw1w2$nosecpregb4insch1)
count(inschw1w2$secpregb4insch1)
count(inschw1w2$futurepreg)
inschw1w2$futurepreg <-ifelse(!is.na(inschw1w2$multipregdate1), inschw1w2$nosecpregb4insch, inschw1w2$futurepreg)
count(inschw1w2$futurepreg)

#only ask about second pregs happening after Jan 1, 94 - check who has multiple preg but is a legit skip for second date (indicating pregnancy before Jan 1, 94)
inschw1w2$missingmultipregdata <- inschw1w2$H1FP8cl>1 & (inschw1w2$H1FP11M1==97 | inschw1w2$H1FP11Y1==97)
count(inschw1w2$missingmultipregdata)
#mp_sub2 <-subset(inschw1w2, missingmultipregdata==TRUE, c("missingmultipregdata","H1FP7cl", "H1FP8cl", "futurepreg", "secpregb4insch1", "pregdate1", "multipregdate1", "pregdate2" , "inschsurveydate", "H1FP11M1", "H1FP11Y1"))
#mp_sub3 <-subset(inschw1w2, missingmultipregdata==TRUE & futurepreg==1, c("missingmultipregdata","H1FP7cl", "H1FP8cl", "futurepreg", "secpregb4insch1", "pregdate1", "multipregdate1", "pregdate2" , "inschsurveydate", "H1FP11M1", "H1FP11Y1"))
#there are 11 people who are legit skips on multiple pregs and in the future preg group
#change legit skips to preg before in school
count(inschw1w2$preginsch)
inschw1w2$preginsch <-ifelse(inschw1w2$missingmultipregdata==TRUE & inschw1w2$futurepreg==1, 1, inschw1w2$preginsch)
count(inschw1w2$preginsch)
#change legit skips to not future preg
count(inschw1w2$futurepreg)
inschw1w2$futurepreg <-ifelse(inschw1w2$missingmultipregdata==TRUE & inschw1w2$futurepreg==1, 0, inschw1w2$futurepreg)
count(inschw1w2$futurepreg)

#change those that aren't legit skips but are missing dates to unknown preg (H1FP11M1 = 98 or H1FP11Y1 = 98)
inschw1w2$multipregdontknow1 <- inschw1w2$H1FP8cl>1 & (inschw1w2$H1FP11M1==98 | inschw1w2$H1FP11Y1==98)
count(inschw1w2$multipregdontknow1)
#mp_idk1 <-subset(inschw1w2, multipregdontknow1==T, c("AID", "preginsch", "futurepreg", "unknownpreg"))
#change don't knows for second preg date from future preg to unknown preg
count(inschw1w2$unknownpreg)
inschw1w2$unknownpreg <-ifelse(inschw1w2$multipregdontknow1==TRUE & inschw1w2$futurepreg==1, 1, inschw1w2$unknownpreg)
count(inschw1w2$unknownpreg)
count(inschw1w2$futurepreg)
inschw1w2$futurepreg <-ifelse(inschw1w2$multipregdontknow1==TRUE & inschw1w2$futurepreg==1, 0, inschw1w2$futurepreg)
count(inschw1w2$futurepreg)

#wave 2
#identify those with multiple pregnancies whose most recent preg happened after in school
inschw1w2$multipregw2 <- inschw1w2$H2FP11cl>1 & inschw1w2$futurepreg==TRUE
count(inschw1w2$multipregw2)
#check dates of those with multiple pregnancy and most recent pregnancy since Jan 1 and most recent preg
inschw1w2$secpregb4insch2 <- inschw1w2$inschsurveydate > inschw1w2$multipregdate2.2nd
inschw1w2$secpregb4insch2 <- ifelse(is.na(inschw1w2$secpregb4insch2),0,inschw1w2$secpregb4insch2)
count(inschw1w2$secpregb4insch2)
#intersect those with dates before inschool and those in future preg
#multi_sub2 <-subset(inschw1w2, secpregb4insch2==T & futurepreg==1, c("AID", "futurepreg", "secpregb4insch2", "pregdate1", "pregdate2", "multipregdate2.2nd", "inschsurveydate"))
#change those that are true from future preg to preg before in school (1 person)
count(inschw1w2$preginsch)
inschw1w2$preginsch <- ifelse((inschw1w2$secpregb4insch2==T & inschw1w2$futurepreg==1), 1, inschw1w2$preginsch)
count(inschw1w2$preginsch)
#change from future pregnant to not future pregnant
count(inschw1w2$futurepreg)
inschw1w2$futurepreg <-ifelse((inschw1w2$secpregb4insch2==T & inschw1w2$futurepreg==1), 0, inschw1w2$futurepreg)
count(inschw1w2$futurepreg)
#3 participants also have a third pregnancy date
#check dates of those with multiple pregnancy and 2nd most recent pregnancy since Jan 1 and most recent preg
inschw1w2$thirdpregb4insch2 <- inschw1w2$inschsurveydate > inschw1w2$multipregdate2.3rd
#mp3preg_sub <-subset(inschw1w2, thirdpregb4insch2==T, c("inschsurveydate", "multipregdate2.3rd"))
#one person has date before in-school, check if they are listed as future
#mp3preg_sub <-subset(inschw1w2, thirdpregb4insch2==T & futurepreg==T, c("inschsurveydate", "multipregdate2.3rd"))
#not listed as future, no further action needed

#check who has multiple preg but are missing second date
#of those missing second date, check who is a legit skip (indicating pregnany before Jan 1)
inschw1w2$missingmultipregdata2 <- inschw1w2$H2FP11cl>1 & (inschw1w2$H2FP12M2==97 | inschw1w2$H2FP12Y2==997)
count(inschw1w2$missingmultipregdata2)
#mp2_sub <-subset(inschw1w2, missingmultipregdata2==TRUE, c("H2FP14_1", "missingmultipregdata2","H2FP10cl", "H2FP11cl", "futurepreg", "secpregb4insch2", "pregdate1", "multipregdate2.2nd", "multipregdate2.3rd", "pregdate2" , "inschsurveydate", "H2FP12M2", "H2FP12Y2"))
#change legit skips to preg before in school - indicates a preg before Jan 1994
#mp2_sub2 <-subset(inschw1w2, missingmultipregdata2==TRUE & futurepreg==1, c("H2FP14_1", "missingmultipregdata2","H2FP10cl", "H2FP11cl", "futurepreg", "secpregb4insch2", "pregdate1", "multipregdate2.2nd", "multipregdate2.3rd", "pregdate2" , "inschsurveydate", "H2FP12M2", "H2FP12Y2"))
#17 people need to change from future preg to preg in school
#of these, 3 people report two pregnancies and they have a pregdate1 prior to pregdate2, these can stay as future pregs. however, that makes a discrepancies as to why the did not report the date of the second preg

#change those that aren't legit skips but are missing dates to unknown preg
inschw1w2$multipregdontknow2 <- inschw1w2$H2FP11cl>1 & (inschw1w2$H2FP12M2==98 | inschw1w2$H2FP12Y2==998)
inschw1w2$multipregdontknow2 <- ifelse(is.na(inschw1w2$multipregdontknow2), 0, inschw1w2$multipregdontknow2)
count(inschw1w2$multipregdontknow2==T & inschw1w2$futurepreg==1)
#mp_idk2 <-subset(inschw1w2, multipregdontknow2==T, c("AID", "preginsch", "futurepreg", "unknownpreg"))
#change another dont know to unknown from future preg
count(inschw1w2$unknownpreg)
inschw1w2$unknownpreg <-ifelse((inschw1w2$multipregdontknow2==T & inschw1w2$futurepreg==1), 1, inschw1w2$unknownpreg)
count(inschw1w2$futurepreg)
inschw1w2$futurepreg <-ifelse((inschw1w2$multipregdontknow2==T & inschw1w2$futurepreg==1), 0, inschw1w2$futurepreg)
count(inschw1w2$futurepreg)

count(inschw1w2$S2cl)
count(inschw1w2$neverpreg)
count(inschw1w2$preginsch)
count(inschw1w2$futurepreg)
count(inschw1w2$unknownpreg)

#change any never preg NA to NA for other preg variables
inschw1w2$preginsch <- ifelse((is.na(inschw1w2$neverpreg)), NA, inschw1w2$preginsch)
inschw1w2$futurepreg <- ifelse((is.na(inschw1w2$neverpreg)), NA, inschw1w2$futurepreg)
inschw1w2$unknownpreg <- ifelse((is.na(inschw1w2$neverpreg)), NA, inschw1w2$unknownpreg)


#look at who is missing never preg to see if its imputable
missingneverpreg <-subset(inschw1w2, is.na(neverpreg) & S2cl==1, c("H1FP7", "H2FP10", "multipregdontknow1", "H1FP11M1","H1FP11Y1", "unknownpreg1", "AID", "H1FP8cl", "H2FP11cl" , "inschsurveydate", "multipregdate1", "multipregdate2.2nd", "multipregdate2.3rd", "pregdate1", "pregdate2", "H1FP7cl", "H2FP10cl", "neverpreg", "preginsch", "futurepreg", "unknownpreg"))

#check to make sure these categories are mutually exclusive
x<-subset(inschw1w2, neverpreg==1 & preginsch==1, c("neverpreg", "preginsch"))
x1<-subset(inschw1w2, neverpreg==1 & futurepreg==1, c("neverpreg", "futurepreg"))
x2<-subset(inschw1w2, neverpreg==1 & unknownpreg==1, c("neverpreg", "unknownpreg"))
x3<-subset(inschw1w2, preginsch==1 & futurepreg==1, c("preginsch", "futurepreg", "inschsurveydate", "pregdate1", "pregdate2"))
x4<-subset(inschw1w2, preginsch==1 & unknownpreg==1, c("preginsch", "unknownpreg", "inschsurveydate", "pregdate1", "pregdate2"))
x5<-subset(inschw1w2, futurepreg==1 & unknownpreg==1, c("futurepreg", "unknownpreg", "inschsurveydate", "pregdate1", "pregdate2"))




#IMPUTE MISSING IN IN-SCHOOL FROM WAVE 1

#AGE
#calculate age from survey date and birthday from wave 1
#month
inschw1w2$H1GI1Mcl <- ifelse(inschw1w2$H1GI1M==96, NA, inschw1w2$H1GI1M)
inschw1w2$H1GI1Mcl <- ifelse(inschw1w2$H1GI1M==1, "01", inschw1w2$H1GI1Mcl)
inschw1w2$H1GI1Mcl <- ifelse(inschw1w2$H1GI1M==2, "02", inschw1w2$H1GI1Mcl)
inschw1w2$H1GI1Mcl <- ifelse(inschw1w2$H1GI1M==3, "03", inschw1w2$H1GI1Mcl)
inschw1w2$H1GI1Mcl <- ifelse(inschw1w2$H1GI1M==4, "04", inschw1w2$H1GI1Mcl)
inschw1w2$H1GI1Mcl <- ifelse(inschw1w2$H1GI1M==5, "05", inschw1w2$H1GI1Mcl)
inschw1w2$H1GI1Mcl <- ifelse(inschw1w2$H1GI1M==6, "06", inschw1w2$H1GI1Mcl)
inschw1w2$H1GI1Mcl <- ifelse(inschw1w2$H1GI1M==7, "07", inschw1w2$H1GI1Mcl)
inschw1w2$H1GI1Mcl <- ifelse(inschw1w2$H1GI1M==8, "08", inschw1w2$H1GI1Mcl)
inschw1w2$H1GI1Mcl <- ifelse(inschw1w2$H1GI1M==9, "09", inschw1w2$H1GI1Mcl)
#year
inschw1w2$H1GI1Ycl <- ifelse(inschw1w2$H1GI1Y==96, NA, inschw1w2$H1GI1Y)
unique(inschw1w2$H1GI1Mcl)
unique(inschw1w2$H1GI1Ycl)
inschw1w2$birthyear <- paste("19", inschw1w2$H1GI1Ycl, sep="")
inschw1w2$birthdate<- paste(inschw1w2$birthyear, "01", inschw1w2$H1GI1Mcl, sep="/")
#View(inschw1w2$birthdate)
inschw1w2$birthdate<-as.Date(inschw1w2$birthdate, "%Y/%d/%m")
#View(inschw1w2$birthdate)

#age calculator function
inschw1w2$age = age_years(inschw1w2$birthdate, inschw1w2$inschsurveydate)
count(inschw1w2$age)

#fill in NA's for in-school age missing
inschw1w2$S1cl <- ifelse(is.na(inschw1w2$S1cl), inschw1w2$age, inschw1w2$S1cl)
length(which(is.na(inschw1w2$S1cl)))
#because dont have a day for survey or bday, check how many take place in the same month
length(which(inschw1w2$H1GI1Mcl == inschw1w2$SCHMONTHcl & inschw1w2$S1na==1))



#PARENTS EDUCATION
#mom
length(which(is.na(inschw1w2$momed)))
length(which(inschw1w2$momedna==1))
inschw1w2$momed<- ifelse(is.na(inschw1w2$momed), inschw1w2$H1RM1cl, inschw1w2$momed)
count(inschw1w2$momed)
#dad
length(which(is.na(inschw1w2$daded)))
length(which(inschw1w2$dadedna==1))
inschw1w2$daded<- ifelse(is.na(inschw1w2$daded), inschw1w2$H1RF1cl, inschw1w2$daded)
count(inschw1w2$daded)
#parent - fill in from student reported w1
#one parent college dummy - 1=yes college, 0 = less than college
inschw1w2$parentedimputed <- ifelse(is.na(inschw1w2$momed), inschw1w2$daded, inschw1w2$momed)
count(inschw1w2$parentedimputed)
#fill in any remaining parent ed na from parent survey
inschw1w2$parentedimputed <-ifelse(is.na(inschw1w2$parentedimputed), inschw1w2$PA12cl, inschw1w2$parentedimputed)
count(inschw1w2$parentedimputed)

#parental figure?
inschw1w2$noparentimputed <- ifelse(is.na(inschw1w2$noparent), inschw1w2$noparent1, inschw1w2$noparent)
count(inschw1w2$noparent)
count(inschw1w2$noparentimputed)

#Last GPA
inschw1w2$lastgpa <- inschw1w2$gpa2
inschw1w2$lastgpa <-ifelse(is.na(inschw1w2$lastgpa), inschw1w2$gpa1, inschw1w2$lastgpa)

#network
#dummy for having network data (all those who go to school with 50% response rate @ inschool)
network$hasnetwork <- 1

#merge reciprocated friend count data
network<-merge(network, recipfriends, by="AID")

#count rtie
count(network$rtie)
count(network$ODGX2)

#create best friend reciprocation variables
#reciprocate as any friend 1=yes
network$bfrecip <- network$BFFRECIP
count(network$bfrecip)
network$bfrecip <-ifelse(is.na(network$BFFRECIP), network$BMFRECIP, network$bfrecip)
count(network$bfrecip)

#reciprocated as best friend 1=yes
network$bfrecipbf <-network$BFFRECBF
count(network$bfrecipbf)
network$bfrecipbf <-ifelse(is.na(network$BFFRECBF), network$BMFRECBF, network$bfrecipbf)
count(network$bfrecipbf)

#reciprocated as best friend 1=yes
network$havebf <-network$HAVEBFF
count(network$havebf)
network$havebf <-ifelse(network$HAVEBFF==0, network$HAVEBMF, network$havebf)
count(network$havebf)

#no friends
network$nofriends <- ifelse((network$ODGX2==0 & network$NOUTNOM==0), 1, 0)
count(network$nofriends)

#nominated out of school friends
network$outschfriends <-ifelse(network$NOUTNOM > 0, 1, 0)
count(network$outschfriends)


#MERGE OTHER DATA FRAMES
inschw1w2w3 <-merge(inschw1w2, wave3, by="AID", all=TRUE)
inschw1w2w3net <-merge(inschw1w2w3, network, by="AID")

#change school id name and make into numeric for imputation
names(inschw1w2w3net)[names(inschw1w2w3net) == 'SSCHLCDE.x'] <- 'SSCHLCDE'
inschw1w2w3net$SSCHLCDE <- as.numeric(inschw1w2w3net$SSCHLCDE)


##CREATE VARIABLES BEFORE IMPUTATION

#Delinquency - determine which items to use
#factor analysis (fa function from psych package)
#subset delinquency measures
delinvars <-c("S59Acl", "S59Bcl", "S59Ccl", "S59Dcl","S59Ecl", "S59Fcl", "S59Gcl")
delin<-inschw1w2w3net[delinvars]
#calculate correlation matrix
corMat <-cor(delin, use="complete")
#without the complete command, you get NAs for all the correlations
corMat
#run factor analysis
fa <-fa(r=corMat, rotate="oblimin", fm="pa")
fa
#oblique rotation allows factors to be correlated, pa = principal axis factoring 
#results that S59Dcl has a factor load <.4, so exclude it in the measure

#average participant delinquency with available items
delinqvars <- c("S59Acl", "S59Bcl", "S59Ccl", "S59Ecl", "S59Fcl", "S59Gcl")
inschw1w2w3net$delinq <- rowMeans(inschw1w2w3net[delinqvars], na.rm=TRUE)
count(inschw1w2w3net$delinq)

#average friends delinquency with available items
#nominated others network
outnomdelinqvars<-c("AXSS59A", "AXSS59B", "AXSS59C", "AXSS59E", "AXSS59F", "AXSS59G")
#seems to be that missing is coded as 0 for many columns. changing them to NA so rowmeans does not divide by columns including 0's
inschw1w2w3net[outnomdelinqvars]<-lapply(inschw1w2w3net[outnomdelinqvars], function(x) ifelse(x==0, NA, x)) 
inschw1w2w3net$outnomdelinq <- rowMeans(inschw1w2w3net[outnomdelinqvars], na.rm=TRUE)
count(inschw1w2w3net$outnomdelinq)


#nominated BY others network
innomdelinqvars <-c("AXRS59A", "AXRS59B", "AXRS59C", "AXRS59E", "AXRS59F", "AXRS59G")
#remove 0's before doing rowMean calculation
inschw1w2w3net[innomdelinqvars]<-lapply(inschw1w2w3net[innomdelinqvars], function(x) ifelse(x==0, NA, x)) 
inschw1w2w3net$innomdelinq <- rowMeans(inschw1w2w3net[innomdelinqvars], na.rm=TRUE)
count(inschw1w2w3net$innomdelinq)

#send and receive network
outinnomdelinqvars <-c("AXS59A", "AXS59B", "AXS59C", "AXS59E", "AXS59F", "AXS59G")
#remove 0's
inschw1w2w3net[outinnomdelinqvars]<-lapply(inschw1w2w3net[outinnomdelinqvars], function(x) ifelse(x==0, NA, x)) 
inschw1w2w3net$outinnomdelinq <- rowMeans(inschw1w2w3net[outinnomdelinqvars], na.rm=TRUE)
count(inschw1w2w3net$outinnomdelinq)

#CREATE VARIABLES TO IDENTIFY EACH RESEARCH QUESTION POPULATION

#RQ1 Full sample
inschw1w2w3net$RQ1full <- inschw1w2w3net$S2cl==1
count(inschw1w2w3net$RQ1full)
#there are 3 people missing sex but in wave 3 they are listed as males
misssex <-subset(inschw1w2w3net, is.na(S2cl), c("S2cl", "BIO_SEXcl", "BIO_SEX2cl", "BIO_SEX3"))
#change any NAs to 0 so not imputed
inschw1w2w3net$RQ1full <-ifelse(is.na(inschw1w2w3net$RQ1full),0,inschw1w2w3net$RQ1full)
count(inschw1w2w3net$RQ1full)

#RQ1 limited sample
inschw1w2w3net$RQ1limited <- inschw1w2w3net$S2cl==1 & (inschw1w2w3net$hasw2==1 & !is.na(inschw1w2w3net$hasw2))
count(inschw1w2w3net$RQ1limited)

#RQ2 full sample
inschw1w2w3net$RQ2full <- inschw1w2w3net$S2cl==1 & (inschw1w2w3net$hasw3==1 & !is.na(inschw1w2w3net$hasw3))
count(inschw1w2w3net$RQ2full)
#channge NA to 0 so not imputed
inschw1w2w3net$RQ2full <-ifelse(is.na(inschw1w2w3net$RQ2full),0,inschw1w2w3net$RQ2full)
count(inschw1w2w3net$RQ2full)

#RQ2 limited sample
inschw1w2w3net$RQ2limited <- inschw1w2w3net$S2cl==1 & (inschw1w2w3net$hasw3==1 & !is.na(inschw1w2w3net$hasw3)) & (inschw1w2w3net$hasw2==1 & !is.na(inschw1w2w3net$hasw2))
count(inschw1w2w3net$RQ2limited)

#RQ3
inschw1w2w3net$RQ3 <- (inschw1w2w3net$preginsch==0 & inschw1w2w3net$futurepreg==1) & inschw1w2w3net$SAT_SCHL==1
count(inschw1w2w3net$RQ3)
#change NA to 0
inschw1w2w3net$RQ3 <-ifelse(is.na(inschw1w2w3net$RQ3),0,inschw1w2w3net$RQ3)
count(inschw1w2w3net$RQ3)

##CLEAN UP WORKSPACE
#drop data sets no longer needed
rm(gpacheck, gpacheck2, gpacheck3)
rm(x, x1, x2, x3, x4, x5)
rm(missingneverpreg, neverpreg_sub)
rm(inschool, inschw1w2, inschw1w2w3, network, schadmin, schinfo, w1w2, wave1, wave2, wave3)
rm(delin, corMat)
rm(misssex)
rm(friends, friends2, friendsinschool, longfriends, recipfriends)


#MICE - Multiple imputation
#Select variables for imputation
myvarsall <- c("AID", "neverpreg", "preginsch", "futurepreg", "unknownpreg", "delinq", "outnomdelinq", "innomdelinq", "outinnomdelinq", "ODGX2", "IDGX2", "BCENT10X", "AXSGPA", "AXRGPA", "AXGPA", "S1cl",  "S6Acl", "S6Bcl", "otherraceinsch", "gpainsch",  "parentedimputed", "noparentimputed", "S62Ocl", "childcare", "altplace", "H3ED3cl", "lastgpa", "H3ED1col", "SSCHLCDE", "RQ1full", "RQ1limited", "RQ2full", "RQ2limited", "RQ3", "bfrecipbf", "bfrecip", "NOUTNOM", "havebf", "nofriends", "outschfriends", "rtie")



#make dataframe of vars of interest
mydata <- inschw1w2w3net[myvarsall]

#decide on number of imputations by estimating percent of not complete cases
count(complete.cases(mydata))
md.pattern(mydata)
count(is.na(mydata$AID))

#impute
imp <-mice(mydata, pred=quickpred(mydata, exclude=c("AID", "SSCHLCDE", "RQ1full", "RQ1limited", "RQ2full", "RQ2limited", "RQ3")), m=30, maxit=10, seed=6876967)
#imp <-mice(mydata, pred=quickpred(mydata, exclude=c("AID", "SSCHLCDE", "RQ1full", "RQ1limited", "RQ2full", "RQ2limited", "RQ3")), m=30, maxit=10, seed=27)
#imp3 <-mice(mydata, pred=quickpred(mydata, exclude=c("AID", "SSCHLCDE", "RQ1full", "RQ1limited", "RQ2full", "RQ2limited", "RQ3")), seed=270988)
#this random imputation seed gives weird sig results for some of my RQ2 models