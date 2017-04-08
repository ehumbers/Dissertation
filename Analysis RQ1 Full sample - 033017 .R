
##imputation seed - 6876967

#load dplyr
#cannot load it before bc it breaks count() function in clean script\

#EXTRACT SOME COMPLETE DATA SETS FROM IMPUTATION FOR INSPECTION
complete1<-complete(imp,1)
complete18<-complete(imp,18)
complete30<-complete(imp, 30)

#SUBSET DATA
RQ1full <-subset(mydata,RQ1full==1)
RQ1fullpregfuture <-subset(mydata, (RQ1full==1 & (preginsch==1 | futurepreg==1)))
RQ1fullpreg <-subset(mydata, (RQ1full==1 & preginsch==1))
RQ1fullfuture <-subset(mydata, (RQ1full==1 & futurepreg==1))
RQ1fullnever <-subset(mydata, (RQ1full==1 & neverpreg==1))

#DESCRIPTIVE STATISTICS
#create a preg group variable
RQ1full$preggroup<-ifelse(RQ1full$neverpreg==1, 1, NA)
RQ1full$preggroup<-ifelse(RQ1full$preginsch==1, 2, RQ1full$preggroup)
RQ1full$preggroup<-ifelse(RQ1full$futurepreg==1, 3, RQ1full$preggroup)
RQ1full$preggroup<-ifelse(RQ1full$unknownpreg==1, 4, RQ1full$preggroup)
RQ1full$preggroup <-as.factor(RQ1full$preggroup)
count(RQ1full$preggroup)

#identify variables for descriptive statistics
descvars<-c("neverpreg", "preginsch", "futurepreg", "unknownpreg", "delinq", "outnomdelinq", "innomdelinq",  "ODGX2", "IDGX2", "BCENT10X", "AXSGPA", "AXRGPA", "bfrecipbf", "bfrecip", "rtie", "S1cl", "S6Acl", "S6Bcl", "otherraceinsch", "gpainsch",  "parentedimputed", "S62Ocl", "childcare", "altplace")

#descriptives for all pregnancy groups combined
# require(dplyr)
# lapply(RQ1full[descvars], function(x) summarize(RQ1full, count =n(), mean=mean(x, na.rm=TRUE), sd=sd(x, na.rm=TRUE)))

alldescriptives<-RQ1full[descvars]
stargazer(alldescriptives, type="text", title="All Girls")

pregdescriptives<-RQ1fullpreg[descvars]
stargazer(pregdescriptives, type="text", title="Pregnant")

futurepregdescriptives<-RQ1fullfuture[descvars]
stargazer(futurepregdescriptives, type="text", title="Future Pregnant")

neverpregdescriptives<-RQ1fullnever[descvars]
stargazer(neverpregdescriptives, type="text", title="Never Pregnant")

#descriptives by pregnancy groups
#not a loop because the loop had issues
# ddply(RQ1full, ~preggroup, summarise, count = n(),mean = mean(gpainsch, na.rm = TRUE),sd = sd(gpainsch, na.rm = TRUE))
# ddply(RQ1full, ~preggroup, summarise, count = n(),mean = mean(delinq, na.rm = TRUE),sd = sd(delinq, na.rm = TRUE))
# ddply(RQ1full, ~preggroup, summarise, count = n(),mean = mean(outnomdelinq, na.rm = TRUE),sd = sd(outnomdelinq, na.rm = TRUE))
# ddply(RQ1full, ~preggroup, summarise, count = n(),mean = mean(innomdelinq, na.rm = TRUE),sd = sd(innomdelinq, na.rm = TRUE))
# ddply(RQ1full, ~preggroup, summarise, count = n(),mean = mean(ODGX2, na.rm = TRUE),sd = sd(ODGX2, na.rm = TRUE))
# ddply(RQ1full, ~preggroup, summarise, count = n(),mean = mean(IDGX2, na.rm = TRUE),sd = sd(IDGX2, na.rm = TRUE))
# ddply(RQ1full, ~preggroup, summarise, count = n(),mean = mean(BCENT10X, na.rm = TRUE),sd = sd(BCENT10X, na.rm = TRUE))
# ddply(RQ1full, ~preggroup, summarise, count = n(),mean = mean(AXSGPA, na.rm = TRUE),sd = sd(AXSGPA, na.rm = TRUE))
# ddply(RQ1full, ~preggroup, summarise, count = n(),mean = mean(AXRGPA, na.rm = TRUE),sd = sd(AXRGPA, na.rm = TRUE))
# ddply(RQ1full, ~preggroup, summarise, count = n(),mean = mean(S1cl, na.rm = TRUE),sd = sd(S1cl, na.rm = TRUE))
# ddply(RQ1full, ~preggroup, summarise, count = n(),mean = mean(S10cl, na.rm = TRUE),sd = sd(S10cl, na.rm = TRUE))
# ddply(RQ1full, ~preggroup, summarise, count = n(),mean = mean(parentedimputed, na.rm = TRUE),sd = sd(parentedimputed, na.rm = TRUE))
# ddply(RQ1full, ~preggroup, summarise, count = n(),mean = mean(altplace, na.rm = TRUE),sd = sd(altplace, na.rm = TRUE))
# ddply(RQ1full, ~preggroup, summarise, count = n(),mean = mean(childcare, na.rm = TRUE),sd = sd(childcare, na.rm = TRUE))
# ddply(RQ1full, ~preggroup, summarise, count = n(),mean = mean(S6Acl, na.rm = TRUE),sd = sd(S6Acl, na.rm = TRUE))
# ddply(RQ1full, ~preggroup, summarise, count = n(),mean = mean(S6Bcl, na.rm = TRUE),sd = sd(S6Bcl, na.rm = TRUE))
# ddply(RQ1full, ~preggroup, summarise, count = n(),mean = mean(otherraceinsch, na.rm = TRUE),sd = sd(otherraceinsch, na.rm = TRUE))
# ddply(RQ1full, ~preggroup, summarise, count = n(),mean = mean(noparentimputed, na.rm = TRUE),sd = sd(noparentimputed, na.rm = TRUE))
# ddply(RQ1full, ~preggroup, summarise, count = n(),mean = mean(S62Ocl, na.rm = TRUE),sd = sd(S62Ocl, na.rm = TRUE))
# ddply(RQ1full, ~preggroup, summarise, count = n(),mean = mean(bfrecipbf, na.rm = TRUE),sd = sd(bfrecipbf, na.rm = TRUE))
# ddply(RQ1full, ~preggroup, summarise, count = n(),mean = mean(bfrecip, na.rm = TRUE),sd = sd(bfrecip, na.rm = TRUE))
# ddply(RQ1full, ~preggroup, summarise, count = n(),mean = mean(havebf, na.rm = TRUE),sd = sd(havebf, na.rm = TRUE))
# ddply(RQ1full, ~preggroup, summarise, count = n(),mean = mean(nofriends, na.rm = TRUE),sd = sd(nofriends, na.rm = TRUE))
# ddply(RQ1full, ~preggroup, summarise, count = n(),mean = mean(rtie, na.rm = TRUE),sd = sd(rtie, na.rm = TRUE))

#histograms
lapply(RQ1full[descvars], function(x) hist(x))
#PROBLEM: need to figure out how to change the histogram titles here

#MEAN DIFFERENCE TESTS

#TTESTS
ttestvars<-c("bfrecipbf", "bfrecip", "havebf", "nofriends", "rtie", "H3ED1col", "H3ED3cl", "lastgpa", "delinq", "outnomdelinq", "innomdelinq",  "ODGX2", "IDGX2", "BCENT10X", "AXSGPA", "AXRGPA", "S1cl",  "S6Acl", "S6Bcl", "otherraceinsch", "gpainsch", "S10cl", "parentedimputed", "noparentimputed", "S62Ocl", "childcare", "altplace")
#t.test preginsch vs never preg

#t.test of preginsch vs future preg only
lapply(RQ1fullpregfuture[ttestvars], function(x) t.test(x~RQ1fullpregfuture$preginsch))

#ANOVAS for outcome variables

#out-noms
ODGX2aov <-aov(ODGX2 ~ preggroup, data=RQ1full)
pairwise.t.test(RQ1full$ODGX2, RQ1full$preggroup, p.adjust="bonferroni")

#in-noms
IDGX2.aov <-aov(IDGX2 ~ preggroup, data = RQ1full)
pairwise.t.test(RQ1full$IDGX2, RQ1full$preggroup, p.adjust="bonferroni")

#reciprocated friendships
rtie.aov <-aov(rtie ~ preggroup, data = RQ1full)
pairwise.t.test(RQ1full$rtie, RQ1full$preggroup, p.adjust="bonferroni")

#centrality
BCENT.aov <-aov(BCENT10X ~ preggroup, data = RQ1full)
pairwise.t.test(RQ1full$BCENT10X, RQ1full$preggroup, p.adjust="bonferroni")

#friends gpa - out
gpaout.aov <-aov(AXSGPA ~ preggroup, data = RQ1full)
pairwise.t.test(RQ1full$AXSGPA, RQ1full$preggroup, p.adjust="bonferroni")

#friends gpa - in
gpain.aov <-aov(AXRGPA ~ preggroup, data = RQ1full)
pairwise.t.test(RQ1full$AXRGPA, RQ1full$preggroup, p.adjust="bonferroni")

#friends delinquency - out
delinout.aov<- aov(outnomdelinq ~ preggroup, data = RQ1full)
pairwise.t.test(RQ1full$outnomdelinq, RQ1full$preggroup, p.adjust="bonferroni")

#friends delinquency - in
delinin.aov<- aov(innomdelinq ~ preggroup, data = RQ1full)
pairwise.t.test(RQ1full$innomdelinq, RQ1full$preggroup, p.adjust="bonferroni")


#CORRELATIONS
corMat <-cor(RQ1full[descvars], use="complete")


##MODELS

#NULL MODEL - all participants
#out noms
RQ1fullODGX2nm <- with(imp, glmer(ODGX2 ~ 1 + (1|SSCHLCDE), family=poisson, control=glmerControl(optimizer = "bobyqa"), subset=(RQ1full==1)))
poolRQ1fullODGX2nm<-pool(RQ1fullODGX2nm)
summary(poolRQ1fullODGX2nm)

#in noms
RQ1fullIDGX2nm <- with(imp, glmer(IDGX2 ~ 1 + (1|SSCHLCDE), family=poisson, control=glmerControl(optimizer = "bobyqa"), subset=(RQ1full==1)))
poolRQ1fullIDGX2nm<-pool(RQ1fullIDGX2nm)
summary(poolRQ1fullIDGX2nm)

RQ1fullrtienm <- with(imp, glmer(rtie ~ 1 + (1|SSCHLCDE), family=poisson, control=glmerControl(optimizer = "bobyqa"), subset=(RQ1full==1)))
poolRQ1fullrtienm<-pool(RQ1fullrtienm)
summary(poolRQ1fullrtienm)

# #centrality
# RQ1fullBCENT10Xnm <- with(imp, lmer(BCENT10X ~ 1 + (1|SSCHLCDE), subset=(RQ1full==1)))
# poolRQ1fullBCENT10Xnm<-pool(RQ1fullBCENT10Xnm)
# summary(poolRQ1fullBCENT10Xnm)

# #out GPA
# RQ1fullAXSGPAnm <- with(imp, lmer(AXSGPA ~ 1 + (1|SSCHLCDE), subset=(RQ1full==1)))
# poolRQ1fullAXSGPAnm<-pool(RQ1fullAXSGPAnm)
# summary(poolRQ1fullAXSGPAnm)
# 
# #in gpa
# RQ1fullAXRGPAnm <- with(imp, lmer(AXRGPA ~ 1 + (1|SSCHLCDE), subset=(RQ1full==1)))
# poolRQ1fullAXRGPAnm<-pool(RQ1fullAXRGPAnm)
# summary(poolRQ1fullAXRGPAnm)

# #out delinq
# RQ1fulloutnomdelinqnm <- with(imp, lmer(outnomdelinq ~ 1 + (1|SSCHLCDE), subset=(RQ1full==1)))
# poolRQ1fulloutnomdelinqnm<-pool(RQ1fulloutnomdelinqnm)
# summary(poolRQ1fulloutnomdelinqnm)
# 
# #in delinq
# RQ1fullinnomdelinqnm <- with(imp, lmer(innomdelinq ~ 1 + (1|SSCHLCDE), subset=(RQ1full==1)))
# poolRQ1fullinnomdelinqnm <-pool(RQ1fullinnomdelinqnm)
# summary(poolRQ1fullinnomdelinqnm)

#bf recip bf
RQ1fullbfrecipbfnm <- with(imp, glmer(bfrecipbf ~ 1 + (1|SSCHLCDE), family=binomial, control=glmerControl(optimizer = "bobyqa"), subset=(RQ1full==1)))
poolRQ1fullbfrecipbfnm <-pool(RQ1fullbfrecipbfnm)
summary(poolRQ1fullbfrecipbfnm)

#bf recip any friend
RQ1fullbfrecipnm <- with(imp, glmer(bfrecip ~ 1 + (1|SSCHLCDE), family=binomial, control=glmerControl(optimizer = "bobyqa"), subset=(RQ1full==1)))
poolRQ1fullbfrecipnm<-pool(RQ1fullbfrecipnm)
summary(poolRQ1fullbfrecipnm)

#NULL MODEL - preg and future preg
#out noms
RQ1fullODGX2nm2 <- with(imp, glmer(ODGX2 ~ 1 + (1|SSCHLCDE), family=poisson, control=glmerControl(optimizer = "bobyqa"), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
poolRQ1fullODGX2nm2<-pool(RQ1fullODGX2nm2)
summary(poolRQ1fullODGX2nm2)

#in noms
RQ1fullIDGX2nm2 <- with(imp, glmer(IDGX2 ~ 1 + (1|SSCHLCDE), family=poisson, control=glmerControl(optimizer = "bobyqa"), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
poolRQ1fullIDGX2nm2<-pool(RQ1fullIDGX2nm2)
summary(poolRQ1fullIDGX2nm2)

RQ1fullrtienm2 <- with(imp, glmer(rtie ~ 1 + (1|SSCHLCDE), family=poisson, control=glmerControl(optimizer = "bobyqa"), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
poolRQ1fullrtienm2<-pool(RQ1fullrtienm2)
summary(poolRQ1fullrtienm2)

# #centrality
# RQ1fullBCENT10Xnm2 <- with(imp, lmer(BCENT10X ~ 1 + (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullBCENT10Xnm2<-pool(RQ1fullBCENT10Xnm2)
# summary(poolRQ1fullBCENT10Xnm2)
# 
# #out GPA
# RQ1fullAXSGPAnm2 <- with(imp, lmer(AXSGPA ~ 1 + (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullAXSGPAnm2<-pool(RQ1fullAXSGPAnm2)
# summary(poolRQ1fullAXSGPAnm2)
# 
# #in gpa
# RQ1fullAXRGPAnm2 <- with(imp, lmer(AXRGPA ~ 1 + (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullAXRGPAnm2<-pool(RQ1fullAXRGPAnm2)
# summary(poolRQ1fullAXRGPAnm2)
# 
# #out delinq
# RQ1fulloutnomdelinqnm2 <- with(imp, lmer(outnomdelinq ~ 1 + (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fulloutnomdelinqnm2<-pool(RQ1fulloutnomdelinqnm2)
# summary(poolRQ1fulloutnomdelinqnm2)
# 
# #in delinq
# RQ1fullinnomdelinqnm2 <- with(imp, lmer(innomdelinq ~ 1 + (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullinnomdelinqnm2 <-pool(RQ1fullinnomdelinqnm2)
# summary(poolRQ1fullinnomdelinqnm2)

#bf recip bf
RQ1fullbfrecipbfnm2 <- with(imp, glmer(bfrecipbf ~ 1 + (1|SSCHLCDE), family=binomial, control=glmerControl(optimizer = "bobyqa"), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
poolRQ1fullbfrecipbfnm2 <-pool(RQ1fullbfrecipbfnm2)
summary(poolRQ1fullbfrecipbfnm2)

#bf recip any friend
RQ1fullbfrecipnm2 <- with(imp, glmer(bfrecip ~ 1 + (1|SSCHLCDE), family=binomial, control=glmerControl(optimizer = "bobyqa"), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
poolRQ1fullbfrecipnm2<-pool(RQ1fullbfrecipnm2)
summary(poolRQ1fullbfrecipnm2)

#MODEL 1
#Y = β0 + β1 Pregnant + β2 Future Pregnant + β3 Unknown Pregnant
#+β4 Age + β5 Years in School + β6 Black + β7 Other Race + β8GPA + β9Parent Education+ β10Delinquency + β11Stigma + β12Lack of parental figure + s + μ

##Note - I thought I'd control for years in school (S9cl) and having no parents (noparentimputed)
#but I get convergence warnings (on some imputed data sets, not all) if I include those and I do not get them without them

#CONVERGENCE WARNING TESTS
#here are some tests I ran because of the covergence warnings I got - run over non-imputed datasets, and two imputed sets
#imputed data set 1
# fm1 <- glmer(ODGX2 ~ preginsch + futurepreg +  S1cl + S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + noparentimputed + (1|SSCHLCDE), family=poisson, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=200000)),  data=complete1)
# #check singularity
# diag.vals <- getME(fm1,"theta")[getME(fm1,"lower") == 0]
# any(diag.vals < 1e-6)
# #recompute gradient? no idea what this is doing
# devfun <- update(fm1, devFunOnly=TRUE)
# if (isLMM(fm1)) {
#   pars <- getME(fm1,"theta")
# } else {
#   ## GLMM: requires both random and fixed parameters
#   pars <- getME(fm1, c("theta","fixef"))
# }
# if (require("numDeriv")) {
#   cat("hess:\n"); print(hess <- hessian(devfun, unlist(pars)))
#   cat("grad:\n"); print(grad <- grad(devfun, unlist(pars)))
#   cat("scaled gradient:\n")
#   print(scgrad <- solve(chol(hess), grad))
# }
# ## compare with internal calculations:
# fm1@optinfo$derivs
# 
# #restart the fit from original value
# fm1.restart <- update(fm1, start=pars)
# 
# #try all available optimizers
# source(system.file("utils", "allFit.R", package="lme4"))
# fm1.all <- allFit(fm1)
# ss <- summary(fm1.all)
# ss$ fixef
# ss$ llik
# ss$ sdcor
# ss$ theta
# ss$ which.OK
# 
# #imputed data set 2
# fm1 <- glmer(ODGX2 ~ preginsch + futurepreg +  S1cl + S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + noparentimputed + (1|SSCHLCDE), family=poisson, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=200000)),  data=complete18)
# #check singularity
# diag.vals <- getME(fm1,"theta")[getME(fm1,"lower") == 0]
# any(diag.vals < 1e-6)
# #recompute gradient? no idea what this is doing
# devfun <- update(fm1, devFunOnly=TRUE)
# if (isLMM(fm1)) {
#   pars <- getME(fm1,"theta")
# } else {
#   ## GLMM: requires both random and fixed parameters
#   pars <- getME(fm1, c("theta","fixef"))
# }
# if (require("numDeriv")) {
#   cat("hess:\n"); print(hess <- hessian(devfun, unlist(pars)))
#   cat("grad:\n"); print(grad <- grad(devfun, unlist(pars)))
#   cat("scaled gradient:\n")
#   print(scgrad <- solve(chol(hess), grad))
# }
# ## compare with internal calculations:
# fm1@optinfo$derivs
# 
# #restart the fit from original value
# fm1.restart <- update(fm1, start=pars)
# 
# #try all available optimizers
# source(system.file("utils", "allFit.R", package="lme4"))
# fm1.all <- allFit(fm1)
# ss <- summary(fm1.all)
# ss$ fixef
# ss$ llik
# ss$ sdcor
# ss$ theta
# ss$ which.OK
# 
# #RQ1full not-imputed
# fm1 <- glmer(ODGX2 ~ preginsch + futurepreg +  S1cl + S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + noparentimputed + (1|SSCHLCDE), family=poisson, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=200000)),  data=RQ1full)
# #check singularity
# diag.vals <- getME(fm1,"theta")[getME(fm1,"lower") == 0]
# any(diag.vals < 1e-6)
# #recompute gradient? no idea what this is doing
# devfun <- update(fm1, devFunOnly=TRUE)
# if (isLMM(fm1)) {
#   pars <- getME(fm1,"theta")
# } else {
#   ## GLMM: requires both random and fixed parameters
#   pars <- getME(fm1, c("theta","fixef"))
# }
# if (require("numDeriv")) {
#   cat("hess:\n"); print(hess <- hessian(devfun, unlist(pars)))
#   cat("grad:\n"); print(grad <- grad(devfun, unlist(pars)))
#   cat("scaled gradient:\n")
#   print(scgrad <- solve(chol(hess), grad))
# }
# ## compare with internal calculations:
# fm1@optinfo$derivs
# 
# #restart the fit from original value
# fm1.restart <- update(fm1, start=pars)
# 
# #try all available optimizers
# source(system.file("utils", "allFit.R", package="lme4"))
# fm1.all <- allFit(fm1)
# ss <- summary(fm1.all)
# ss$ fixef
# ss$ llik
# ss$ sdcor
# ss$ theta
# ss$ which.OK
# 
# #preg future preg only - not imputed
# fm1 <- glmer(ODGX2 ~ preginsch + S1cl + S6Bcl + gpainsch +parentedimputed + delinq+ S62Ocl +  (1|SSCHLCDE), family=poisson, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=200000)),  data=RQ1fullpregfuture)
# #check singularity
# diag.vals <- getME(fm1,"theta")[getME(fm1,"lower") == 0]
# any(diag.vals < 1e-6)
# #recompute gradient? no idea what this is doing
# devfun <- update(fm1, devFunOnly=TRUE)
# if (isLMM(fm1)) {
#   pars <- getME(fm1,"theta")
# } else {
#   ## GLMM: requires both random and fixed parameters
#   pars <- getME(fm1, c("theta","fixef"))
# }
# if (require("numDeriv")) {
#   cat("hess:\n"); print(hess <- hessian(devfun, unlist(pars)))
#   cat("grad:\n"); print(grad <- grad(devfun, unlist(pars)))
#   cat("scaled gradient:\n")
#   print(scgrad <- solve(chol(hess), grad))
# }
# ## compare with internal calculations:
# fm1@optinfo$derivs
# 
# #restart the fit from original value
# fm1.restart <- update(fm1, start=pars)
# 
# #try all available optimizers
# source(system.file("utils", "allFit.R", package="lme4"))
# fm1.all <- allFit(fm1)
# ss <- summary(fm1.all)
# ss$ fixef
# ss$ llik
# ss$ sdcor
# ss$ theta
# ss$ which.OK

#out noms
RQ1fullODGX2m1 <- with(imp, glmer(ODGX2 ~ preginsch + futurepreg +  S1cl +  S6Bcl + otherraceinsch +  gpainsch +parentedimputed + delinq+ S62Ocl + (1|SSCHLCDE), family=poisson, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=200000)), subset=(RQ1full==1)))
poolRQ1fullODGX2m1<-pool(RQ1fullODGX2m1)
summary(RQ1fullODGX2m1)
stargazer(summary(poolRQ1fullODGX2m1), type="text", title="out nom m1 all participant")
#this still have warnings but they didnt pop up for some reason

#in noms
RQ1fullIDGX2m1 <- with(imp, glmer(IDGX2 ~ preginsch + futurepreg +  S1cl + S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + (1|SSCHLCDE), family=poisson, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=200000)), subset=(RQ1full==1)))
poolRQ1fullIDGX2m1<-pool(RQ1fullIDGX2m1)
summary(RQ1fullIDGX2m1)
stargazer(summary(poolRQ1fullIDGX2m1), type="text", title="in nom m1 all participants")
warnings()
#this actually had warnings but they didn't pop up

#reciprocated ties
RQ1fullrtiem1 <- with(imp, glmer(rtie ~ preginsch + futurepreg +  S1cl + S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + (1|SSCHLCDE), family=poisson, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=200000)), subset=(RQ1full==1)))
poolRQ1fullrtiem1<-pool(RQ1fullrtiem1)
stargazer(summary(poolRQ1fullrtiem1), type="text", title="rtie m1 all participants")

# #centrality
# RQ1fullBCENT10Xm1 <- with(imp, lmer(BCENT10X ~ preginsch + futurepreg + unknownpreg  + S1cl + S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + (1|SSCHLCDE), subset=(RQ1full==1)))
# poolRQ1fullBCENT10Xm1<-pool(RQ1fullBCENT10Xm1)
# summary(poolRQ1fullBCENT10Xm1)

# #out GPA
# RQ1fullAXSGPAm1 <- with(imp, lmer(AXSGPA ~ preginsch + futurepreg + unknownpreg  + S1cl + S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + (1|SSCHLCDE), subset=(RQ1full==1)))
# poolRQ1fullAXSGPAm1<-pool(RQ1fullAXSGPAm1)
# summary(poolRQ1fullAXSGPAm1)
# 
# #in gpa
# RQ1fullAXRGPAm1 <- with(imp, lmer(AXRGPA ~ preginsch + futurepreg + unknownpreg  + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + (1|SSCHLCDE), subset=(RQ1full==1)))
# poolRQ1fullAXRGPAm1<-pool(RQ1fullAXRGPAm1)
# summary(poolRQ1fullAXRGPAm1)

# #out delinq
# RQ1fulloutnomdelinqm1 <- with(imp, lmer(outnomdelinq ~ preginsch + futurepreg + unknownpreg  + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl +  (1|SSCHLCDE), subset=(RQ1full==1)))
# poolRQ1fulloutnomdelinqm1<-pool(RQ1fulloutnomdelinqm1)
# summary(poolRQ1fulloutnomdelinqm1)

# #in delinq
# RQ1fullinnomdelinqm1 <- with(imp, lmer(innomdelinq ~ preginsch + futurepreg + unknownpreg  + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl +  (1|SSCHLCDE), subset=(RQ1full==1)))
# poolRQ1fullinnomdelinqm1<-pool(RQ1fullinnomdelinqm1)
# summary(poolRQ1fullinnomdelinqm1)

#bf recip bf
RQ1fullbfrecipbfm1 <- with(imp, glmer(bfrecipbf ~ preginsch + futurepreg + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl +  (1|SSCHLCDE), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=200000)), subset=(RQ1full==1)))
poolRQ1fullbfrecipbfm1<-pool(RQ1fullbfrecipbfm1)
stargazer(summary(poolRQ1fullbfrecipbfm1), type="text", title="bf as bf m1 all participants")
#no convergence warnings

#bf recip any friend
RQ1fullbfrecipm1 <- with(imp, glmer(bfrecip ~ preginsch + futurepreg +  S1cl + S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + (1|SSCHLCDE), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=200000)), subset=(RQ1full==1)))
poolRQ1fullbfrecipm1<-pool(RQ1fullbfrecipm1)
stargazer(summary(poolRQ1fullbfrecipm1), type="text", title="bf recip m1 all participants")
#no covergence warnings

#MODEL 2
#Y = β0 + β1 Pregnant +β2 Age + β3 Years in School + β4 Black + β5 Other Race + β6GPA 
#+ β7Parent Education+ β8Delinquency + β9Stigma + β10Lack of parental figure + μ

#out noms
RQ1fullODGX2m2 <- with(imp, glm(ODGX2 ~ preginsch + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl , family=poisson, subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
poolRQ1fullODGX2m2<-pool(RQ1fullODGX2m2)
stargazer(summary(poolRQ1fullODGX2m2), type="text", title="out nom m2 glm")
#summary(RQ1fullODGX2m2)

#in noms
RQ1fullIDGX2m2 <- with(imp, glm(IDGX2 ~ preginsch + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl , family=poisson, subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
poolRQ1fullIDGX2m2<-pool(RQ1fullIDGX2m2)
stargazer(summary(poolRQ1fullIDGX2m2), type="text", title="in nom m2 glm")
#summary(RQ1fullIDGX2m2)

# rtie
RQ1fullrtiem2 <- with(imp, glm(rtie ~ preginsch + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl , family=poisson, subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
poolRQ1fullrtiem2<-pool(RQ1fullrtiem2)
stargazer(summary(poolRQ1fullrtiem2), type="text", title="rtie m2 glm")

# #centrality
# RQ1fullBCENT10Xm2 <- with(imp, lm(BCENT10X ~ preginsch + S1cl + S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl , subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullBCENT10Xm2<-pool(RQ1fullBCENT10Xm2)
# summary(poolRQ1fullBCENT10Xm2)

# #out gpa
# RQ1fullAXSGPAm2 <- with(imp, lm(AXSGPA ~ preginsch + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl , subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullAXSGPAm2<-pool(RQ1fullAXSGPAm2)
# summary(poolRQ1fullAXSGPAm2)
# 
# #in gpa
# RQ1fullAXRGPAm2 <- with(imp, lm(AXRGPA ~ preginsch + S1cl + S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl , subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullAXRGPAm2<-pool(RQ1fullAXRGPAm2)
# summary(poolRQ1fullAXRGPAm2)
# 
# #out delinq
# RQ1fulloutnomdelinqm2 <- with(imp, lm(outnomdelinq ~ preginsch + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl , subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fulloutnomdelinqm2<-pool(RQ1fulloutnomdelinqm2)
# summary(poolRQ1fulloutnomdelinqm2)
# 
# 
# #in delinq
# RQ1fullinnomdelinqm2 <- with(imp, lm(innomdelinq ~ preginsch + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl , subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullinnomdelinqm2<-pool(RQ1fullinnomdelinqm2)
# summary(poolRQ1fullinnomdelinqm2)


#best friend lists as best friend
RQ1fullbfrecipbfm2 <- with(imp, glm(bfrecipbf ~ preginsch + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl , family=binomial, subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
poolRQ1fullbfrecipbfm2<-pool(RQ1fullbfrecipbfm2)
stargazer(summary(poolRQ1fullbfrecipbfm2), type="text", title="bf as bf m2 glm")

#best friend lists as any friend
RQ1fullbfrecipm2 <- with(imp, glm(bfrecip ~ preginsch + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl , family=binomial, subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
poolRQ1fullbfrecipm2<-pool(RQ1fullbfrecipm2)
stargazer(summary(poolRQ1fullbfrecipm2), type="text", title="bf as friend glm m2")

##MODEL 3
#Y = β0 + β1 Pregnant +β2 Age + β3 Years in School + β4 Black + β5 Other Race + β6GPA 
#+ β7Parent Education+ β8Delinquency + β9Stigma + β10Lack of parental figure + s + μ

#out noms
RQ1fullODGX2m3 <- with(imp, glmer(ODGX2 ~ preginsch + S1cl +  S6Bcl +  otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl +   (1|SSCHLCDE), family=poisson, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=200000)), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
poolRQ1fullODGX2m3<-pool(RQ1fullODGX2m3)
stargazer(summary(poolRQ1fullODGX2m3), type="text", title="out nom m3")
#summary(RQ1fullODGX2m3)
#no convergence errors

#in noms
RQ1fullIDGX2m3 <- with(imp, glmer(IDGX2 ~ preginsch + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + (1|SSCHLCDE), family=poisson, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=200000)), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
poolRQ1fullIDGX2m3<-pool(RQ1fullIDGX2m3)
stargazer(summary(poolRQ1fullIDGX2m3), type="text", title="in nom m3")
#summary(RQ1fullIDGX2m3)
#no convergence errors

#rtie
RQ1fullrtiem3 <- with(imp, glmer(rtie ~ preginsch + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + (1|SSCHLCDE), family=poisson, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=200000)), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
poolRQ1fullrtiem3<-pool(RQ1fullrtiem3)
stargazer(summary(poolRQ1fullrtiem3), type="text", title="rtie m3")

# #centrality
# RQ1fullBCENT10Xm3 <- with(imp, lmer(BCENT10X ~ preginsch + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullBCENT10Xm3<-pool(RQ1fullBCENT10Xm3)
# stargazer(summary(poolRQ1fullBCENT10Xm3), type="text", title="cent m3")
# #summary(RQ1fullBCENT10Xm3)
# 
# #out gpa
# RQ1fullAXSGPAm3 <- with(imp, lmer(AXSGPA ~ preginsch + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl  + (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullAXSGPAm3<-pool(RQ1fullAXSGPAm3)
# stargazer(summary(poolRQ1fullAXSGPAm3), type="text", title="out gpa m3")
# 
# #in gpa
# RQ1fullAXRGPAm3 <- with(imp, lmer(AXRGPA ~ preginsch + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl +  (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullAXRGPAm3<-pool(RQ1fullAXRGPAm3)
# stargazer(summary(poolRQ1fullAXRGPAm3), type="text", title="in gpa m3")
# 
# #out delinq
# RQ1fulloutnomdelinqm3 <- with(imp, lmer(outnomdelinq ~ preginsch + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl +  (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fulloutnomdelinqm3<-pool(RQ1fulloutnomdelinqm3)
# stargazer(summary(poolRQ1fulloutnomdelinqm3), type="text", title="out delin m3")
# 
# #in delinq
# RQ1fullinnomdelinqm3 <- with(imp, lmer(innomdelinq ~ preginsch + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl +  (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullinnomdelinqm3<-pool(RQ1fullinnomdelinqm3)
# stargazer(summary(poolRQ1fullinnomdelinqm3), type="text", title="in delin m3")

#best friend recip best friend
RQ1fullbfrecipbfm3 <- with(imp, glmer(bfrecipbf ~ preginsch + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl +  (1|SSCHLCDE), family=binomial, control=glmerControl(optimizer = "bobyqa"), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
poolRQ1fullbfrecipbfm3<-pool(RQ1fullbfrecipbfm3)
stargazer(summary(poolRQ1fullbfrecipbfm3), type="text", title="bf recip bf m3")
#no converge warnings

#best friend recip as any friend
RQ1fullbfrecipm3 <- with(imp, glmer(bfrecip ~ preginsch + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl +  (1|SSCHLCDE), family=binomial, control=glmerControl(optimizer = "bobyqa"), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
poolRQ1fullbfrecipm3<-pool(RQ1fullbfrecipm3)
stargazer(summary(poolRQ1fullbfrecipm3), type="text", title="bf recip m3")

#MODEL 5
#school level predictors
#Y = β0 + β1 Pregnant +β2 Age + β3 Years in School + β4 Race + β5 Other Race 
#+ β6GPA + β7Parent Education + β8Delinquency + β9Stigma + β10Alternate Placement 
#+ β11Childcare + β12Lack of parental figure + s + μ

#out noms
RQ1fullODGX2m5 <- with(imp, glmer(ODGX2 ~ preginsch + altplace + childcare + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + (1|SSCHLCDE), family=poisson, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=200000)), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
poolRQ1fullODGX2m5<-pool(RQ1fullODGX2m5)
stargazer(summary(poolRQ1fullODGX2m5), type="text", title="Out Nom - Model 4")
#summary(RQ1fullODGX2m5)
#no convergence warnings

#in noms
RQ1fullIDGX2m5 <- with(imp, glmer(IDGX2 ~ preginsch + altplace + childcare + S1cl + S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl +  (1|SSCHLCDE), family=poisson, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=200000)), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
poolRQ1fullIDGX2m5<-pool(RQ1fullIDGX2m5)
stargazer(summary(poolRQ1fullIDGX2m5), type="text", title="In Nom - m4")
#summary(RQ1fullIDGX2m5)
#no convergence warnings

#rtie
RQ1fullrtiem5 <- with(imp, glmer(rtie ~ preginsch + altplace + childcare + S1cl + S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + (1|SSCHLCDE), family=poisson, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=200000)), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
poolRQ1fullrtiem5<-pool(RQ1fullrtiem5)
stargazer(summary(poolRQ1fullrtiem5), type="text", title="rtie - m4")
#no convergence warnings

#centrality
RQ1fullBCENT10Xm5 <- with(imp, lmer(BCENT10X ~ preginsch + altplace + childcare + S1cl + S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl +  (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
poolRQ1fullBCENT10Xm5<-pool(RQ1fullBCENT10Xm5)
stargazer(summary(poolRQ1fullBCENT10Xm5), type="text", title = "centrality-m4")
#summary(RQ1fullBCENT10Xm5)

#out gpa
RQ1fullAXSGPAm5 <- with(imp, lmer(AXSGPA ~ preginsch + altplace + childcare + S1cl + S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
poolRQ1fullAXSGPAm5<-pool(RQ1fullAXSGPAm5)
stargazer(summary(poolRQ1fullAXSGPAm5), type="text", title="out gpa - m4")
#summary(RQ1fullAXSGPAm5)

#in gpa
RQ1fullAXRGPAm5 <- with(imp, lmer(AXRGPA ~ preginsch + altplace + childcare + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
poolRQ1fullAXRGPAm5<-pool(RQ1fullAXRGPAm5)
stargazer(summary(poolRQ1fullAXRGPAm5), type="text", title="in gpa- m4")
#summary(RQ1fullAXRGPAm5)

#out delinq
RQ1fulloutnomdelinqm5 <- with(imp, lmer(outnomdelinq ~ preginsch + altplace + childcare + S1cl + S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl +  (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
poolRQ1fulloutnomdelinqm5<-pool(RQ1fulloutnomdelinqm5)
stargazer(summary(poolRQ1fulloutnomdelinqm5), type="text", title="out delin- m4")
#summary(RQ1fulloutnomdelinqm5)

#in delinq
RQ1fullinnomdelinqm5 <- with(imp, lmer(innomdelinq ~ preginsch + altplace + childcare + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
poolRQ1fullinnomdelinqm5 <-pool(RQ1fullinnomdelinqm5)
stargazer(summary(poolRQ1fullinnomdelinqm5), type="text", title="in delin- m4")
#summary(RQ1fullinnomdelinqm5)

#best friend recip best friend
RQ1fullbfrecipbfm5 <- with(imp, glmer(bfrecipbf ~ preginsch + altplace + childcare  + S1cl + S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl +  (1|SSCHLCDE), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=200000)), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
poolRQ1fullbfrecipbfm5<-pool(RQ1fullbfrecipbfm5)
stargazer(summary(poolRQ1fullbfrecipbfm5), type="text", title="Best Friend Reciprocated Best Friend - model 4")
#summary(RQ1fullbfrecipbfm5)
#no conv warning

#best friend recip as any friend
RQ1fullbfrecipm5 <- with(imp, glmer(bfrecip ~ preginsch + altplace + childcare + S1cl + S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl +  (1|SSCHLCDE), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=200000)), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
poolRQ1fullbfrecipm5<-pool(RQ1fullbfrecipm5)
stargazer(summary(poolRQ1fullbfrecipm5), type="text", title="Best friend recip - model 4")
#summary(RQ1fullbfrecipm5)
#1 convergence warning


#MODEL 6
#try interactions separately - alt placement
#Y = β0 + β1 Pregnant +β2 Age + β3 Years in School + β4 Race + β5 Other Race 
#+ β6GPA + β7Parent Education + β8Delinquency + β9Stigma + β10Alternate Placement 
#+  β12Pregnancy x Alternate Placement + β14Lack of parental figure + s + μ

# #out noms
# RQ1fullODGX2m6 <- with(imp, glmer(ODGX2 ~ preginsch + altplace +  preginsch:altplace +  S1cl + S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + (1|SSCHLCDE), family=poisson, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=200000)), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullODGX2m6<-pool(RQ1fullODGX2m6)
# summary(poolRQ1fullODGX2m6)
# #summary(RQ1fullODGX2m6)
# #6 conv warnings, no sig interactions

# #in noms
# RQ1fullIDGX2m6 <- with(imp, glmer(IDGX2 ~ preginsch + altplace +  preginsch:altplace +  S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl  + (1|SSCHLCDE), family=poisson, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=200000)), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullIDGX2m6<-pool(RQ1fullIDGX2m6)
# summary(poolRQ1fullIDGX2m6)
# #summary(RQ1fullIDGX2m6)
# #8 convergence warnings, no sig interaction

# #recip ties
# RQ1fullrtiem6 <- with(imp, glmer(rtie ~ preginsch + altplace +  preginsch:altplace +  S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl  + (1|SSCHLCDE), family=poisson, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=200000)), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullrtiem6<-pool(RQ1fullrtiem6)
# summary(poolRQ1fullrtiem6)
# #summary(RQ1fullIDGX2m6)
# #1 conv error, no sig interaction

# #centrality
# RQ1fullBCENT10Xm6 <- with(imp, lmer(BCENT10X ~ preginsch + altplace + preginsch:altplace +  S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl  + (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullBCENT10Xm6<-pool(RQ1fullBCENT10Xm6)
# summary(poolRQ1fullBCENT10Xm6)
# #summary(RQ1fullBCENT10Xm6)
# #no warnings, non sign interaction

# #out gpa
# RQ1fullAXSGPAm6 <- with(imp, lmer(AXSGPA ~ preginsch + altplace + preginsch:altplace +  S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl  + (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullAXSGPAm6<-pool(RQ1fullAXSGPAm6)
# summary(poolRQ1fullAXSGPAm6)
# #summary(RQ1fullAXSGPAm6)
# #no warnings, non sign interaction

# #in gpa
# RQ1fullAXRGPAm6 <- with(imp, lmer(AXRGPA ~ preginsch + altplace +  preginsch:altplace +  S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl  + (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullAXRGPAm6<-pool(RQ1fullAXRGPAm6)
# summary(poolRQ1fullAXRGPAm6)
# #summary(RQ1fullAXRGPAm6)
# #no warnings, non sign interaction

# #out delinq
# RQ1fulloutnomdelinqm6 <- with(imp, lmer(outnomdelinq ~ preginsch + altplace +  preginsch:altplace +  S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fulloutnomdelinqm6<-pool(RQ1fulloutnomdelinqm6)
# summary(poolRQ1fulloutnomdelinqm6)
# #summary(RQ1fulloutnomdelinqm6)
# #no warnings, non sign interaction

# #in delinq
# RQ1fullinnomdelinqm6 <- with(imp, lmer(innomdelinq ~ preginsch + altplace +  preginsch:altplace +  S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullinnomdelinqm6<-pool(RQ1fullinnomdelinqm6)
# summary(poolRQ1fullinnomdelinqm6)
# #summary(RQ1fullinnomdelinqm6)
# #no warnings, non sign interaction

# #best friend recip best friend
# RQ1fullbfrecipbfm6 <- with(imp, glmer(bfrecipbf ~ preginsch + altplace  + preginsch:altplace +  S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl +  (1|SSCHLCDE), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=200000)), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullbfrecipbfm6<-pool(RQ1fullbfrecipbfm6)
# summary(poolRQ1fullbfrecipbfm6)
# #summary(RQ1fullbfrecipbfm6)
# #no conv warnings, no sig interaction

# #best friend recip as any friend
# RQ1fullbfrecipm6 <- with(imp, glmer(bfrecip ~ preginsch + altplace +  preginsch:altplace +  S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl +  (1|SSCHLCDE), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=200000)), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullbfrecipm6 <-pool(RQ1fullbfrecipm6)
# summary(poolRQ1fullbfrecipm6)
# #no conver, non sig interactions

#MODEL 7
#interaction of childcare only
#Y = β0 + β1 Pregnant +β2 Age + β3 Years in School + β4 Race + β5 Other Race 
#+ β6GPA + β7Parent Education + β8Delinquency + β9Stigma  
#+ β11Childcare + β13Pregnancy x Childcare+ β14Lack of parental figure + s + μ

# #out noms
# RQ1fullODGX2m7 <- with(imp, glmer(ODGX2 ~ preginsch + childcare +  preginsch:childcare  + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + (1|SSCHLCDE), family=poisson, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=200000)), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullODGX2m7<-pool(RQ1fullODGX2m7)
# summary(poolRQ1fullODGX2m7)
# #7 convergence warnings, no sig interaction

# #in noms
# RQ1fullIDGX2m7 <- with(imp, glmer(IDGX2 ~ preginsch +  childcare +  preginsch:childcare + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl +  (1|SSCHLCDE), family=poisson, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=200000)), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullIDGX2m7<-pool(RQ1fullIDGX2m7)
# summary(poolRQ1fullIDGX2m7)
# #4 converge warnings, no sig interaction

# #reciprocated ties
# RQ1fullrtiem7 <- with(imp, glmer(rtie ~ preginsch +  childcare +  preginsch:childcare + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl +  (1|SSCHLCDE), family=poisson, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=200000)), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullrtiem7<-pool(RQ1fullrtiem7)
# summary(poolRQ1fullrtiem7)
# #7 convergence, non sig interaction

# #centrality
# RQ1fullBCENT10Xm7 <- with(imp, lmer(BCENT10X ~ preginsch + childcare +  preginsch:childcare + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullBCENT10Xm7<-pool(RQ1fullBCENT10Xm7)
# summary(poolRQ1fullBCENT10Xm7)
# #no convergence, insig interaction

# #out gpa
# RQ1fullAXSGPAm7 <- with(imp, lmer(AXSGPA ~ preginsch + childcare  + preginsch:childcare + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullAXSGPAm7<-pool(RQ1fullAXSGPAm7)
# summary(poolRQ1fullAXSGPAm7)
# #no convergence, insig interaction

# #in gpa
# RQ1fullAXRGPAm7 <- with(imp, lmer(AXRGPA ~ preginsch + childcare  + preginsch:childcare + S1cl + S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl +  (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullAXRGPAm7 <-pool(RQ1fullAXRGPAm7)
# stargazer(summary(poolRQ1fullAXRGPAm7), type="text")
# #no warning, insig interaction

# #out delinq
# RQ1fulloutnomdelinqm7 <- with(imp, lmer(outnomdelinq ~ preginsch + childcare +  preginsch:childcare + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl +  (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fulloutnomdelinqm7 <-pool(RQ1fulloutnomdelinqm7)
# stargazer(summary(poolRQ1fulloutnomdelinqm7), type="text")
# #no conv warning, insig interaction

# #in delinq
# RQ1fullinnomdelinqm7 <- with(imp, lmer(innomdelinq ~ preginsch +  childcare +  preginsch:childcare + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl +  (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullinnomdelinqm7 <-pool(RQ1fullinnomdelinqm7)
# stargazer(summary(poolRQ1fullinnomdelinqm7), type="text")
# #no conv warning, insig interaction

# #best friend recip best friend
# RQ1fullbfrecipbfm7 <- with(imp, glmer(bfrecipbf ~ preginsch +  childcare + preginsch:childcare +  S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl +  (1|SSCHLCDE), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=200000)), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullbfrecipbfm7<-pool(RQ1fullbfrecipbfm7)
# stargazer(summary(poolRQ1fullbfrecipbfm7), type="text")
# #one convergence warning, insig interaction

# #best friend recip as any friend
# RQ1fullbfrecipm7 <- with(imp, glmer(bfrecip ~ preginsch +  childcare +  preginsch:childcare + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl +  (1|SSCHLCDE), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=200000)), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullbfrecipm7<-pool(RQ1fullbfrecipm7)
# stargazer(summary(poolRQ1fullbfrecipm7), type="text")
# #no convergence warnings, no sig interaction term



# #MODEL 4
# #both interactions

# #these models not run again bc neither interaction found sig individually

# #Y = β0 + β1 Pregnant +β2 Age + β3 Years in School + β4 Race + β5 Other Race 
# #+ β6GPA + β7Parent Education + β8Delinquency + β9Stigma + β10Alternate Placement 
# #+ β11Childcare + β12Pregnancy x Alternate Placement 
# #+ β13Pregnancy x Childcare+ β14Lack of parental figure + s + μ
# 
# #out noms
# RQ1fullODGX2m4 <- with(imp, glmer(ODGX2 ~ preginsch + altplace + childcare + preginsch:altplace + preginsch:childcare  + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + noparentimputed + (1|SSCHLCDE), family=poisson, control=glmerControl(optimizer = "bobyqa"), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullODGX2m4<-pool(RQ1fullODGX2m4)
# summary(poolRQ1fullODGX2m4)
# summary(RQ1fullODGX2m4)
# 
# #in noms
# RQ1fullIDGX2m4 <- with(imp, glmer(IDGX2 ~ preginsch + altplace + childcare + preginsch:altplace + preginsch:childcare + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + noparentimputed + (1|SSCHLCDE), family=poisson, control=glmerControl(optimizer = "bobyqa"), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullIDGX2m4<-pool(RQ1fullIDGX2m4)
# summary(poolRQ1fullIDGX2m4)
# summary(RQ1fullIDGX2m4)
# 
# #centrality
# RQ1fullBCENT10Xm4 <- with(imp, lmer(BCENT10X ~ preginsch + altplace + childcare + preginsch:altplace + preginsch:childcare + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + noparentimputed + (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullBCENT10Xm4<-pool(RQ1fullBCENT10Xm4)
# summary(poolRQ1fullBCENT10Xm4)
# summary(RQ1fullBCENT10Xm4)
# 
# #out gpa
# RQ1fullAXSGPAm4 <- with(imp, lmer(AXSGPA ~ preginsch + altplace + childcare + preginsch:altplace + preginsch:childcare + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + noparentimputed + (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullAXSGPAm4<-pool(RQ1fullAXSGPAm4)
# summary(poolRQ1fullAXSGPAm4)
# summary(RQ1fullAXSGPAm4)
# 
# #in gpa
# RQ1fullAXRGPAm4 <- with(imp, lmer(AXRGPA ~ preginsch + altplace + childcare + preginsch:altplace + preginsch:childcare + S1cl + S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + noparentimputed + (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullAXRGPAm4<-pool(RQ1fullAXRGPAm4)
# summary(poolRQ1fullAXRGPAm4)
# summary(RQ1fullAXRGPAm4)
# 
# #out delinq
# RQ1fulloutnomdelinqm4 <- with(imp, lmer(outnomdelinq ~ preginsch + altplace + childcare + preginsch:altplace + preginsch:childcare + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + noparentimputed + (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fulloutnomdelinqm4<-pool(RQ1fulloutnomdelinqm4)
# summary(poolRQ1fulloutnomdelinqm4)
# summary(RQ1fulloutnomdelinqm4)
# 
# #in delinq
# RQ1fullinnomdelinqm4 <- with(imp, lmer(innomdelinq ~ preginsch + altplace + childcare + preginsch:altplace + preginsch:childcare + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + noparentimputed + (1|SSCHLCDE), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullinnomdelinqm4<-pool(RQ1fullinnomdelinqm4)
# summary(poolRQ1fullinnomdelinqm4)
# summary(RQ1fullinnomdelinqm4)
# 
# #best friend recip best friend
# RQ1fullbfrecipbfm4 <- with(imp, glmer(bfrecipbf ~ preginsch + altplace + childcare + preginsch:altplace + preginsch:childcare + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + noparentimputed + (1|SSCHLCDE), family=binomial, control=glmerControl(optimizer = "bobyqa"), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullbfrecipbfm4<-pool(RQ1fullbfrecipbfm4)
# summary(poolRQ1fullbfrecipbfm4)
# #summary(RQ1fullbfrecipbfm4)
# 
# #best friend recip as any friend
# RQ1fullbfrecipm4 <- with(imp, glmer(bfrecip ~ preginsch + altplace + childcare + preginsch:altplace + preginsch:childcare + S1cl +  S6Bcl + otherraceinsch + gpainsch +parentedimputed + delinq+ S62Ocl + (1|SSCHLCDE), family=binomial, control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=200000)), subset=(RQ1full==1 & (preginsch==1 | futurepreg ==1))))
# poolRQ1fullbfrecipm4<-pool(RQ1fullbfrecipm4)
# summary(poolRQ1fullbfrecipm4)
# #summary(RQ1fullbfrecipm4)



