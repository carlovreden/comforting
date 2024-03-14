setwd("C:/Users/Carlo/OneDrive - Durham University/Previous projects/Comforting/R")
xdata=read.table(file="comforting_data_FINAL.txt", header=T, sep="\t", stringsAsFactors=T)
str(xdata)

library(lme4)
library(glmmTMB)

#Prepare data
xdata$age<-as.factor(xdata$age)
xdata$comforts.num=as.numeric(xdata$comf=="Y")
xdata$hypothesis.num=as.numeric(xdata$hypoth=="Y")
age<-subset(xdata, condition=="experimental" & adult=="Mother")
fam<-subset(xdata, condition=="experimental" & age=="18")
long<-subset(xdata, condition=="experimental" & age=="18" & longitudinal=="Y")

##COMFORTING##
#Full vs. null model
comforting<-glmer(comforts.num~age*culture+sex+(1|ID), family=binomial, data=age)
comforting.null<-glmer(comforts.num~sex+(1|ID), family=binomial, data=age)
as.data.frame(anova(comforting.null, comforting, test="Chisq"))
#Full vs. reduced model
comforting.red<-glmer(comforts.num~age+culture+sex+(1|ID), family=binomial, data=age)
as.data.frame(anova(comforting.red, comforting, test="Chisq"))
drop1(comforting.red, test="Chisq")
summary(comforting.red)
source("boot_glmm.r")
boot.res=boot.glmm.pred(model.res=comforting.red, excl.warnings=T,
 nboots=1000, para=T)
round(boot.res$ci.estimates, 3)

##CONCERN##
#Full vs. null model
age$tr.concern.exp=(age$concern.exp*(nrow(age) - 1) + 0.5)/nrow(age)
prop_full=glmmTMB(tr.concern.exp~age*culture+sex+(1|ID),
 family=beta_family(link="logit"), data=age)
prop_null=glmmTMB(tr.concern.exp~sex+(1|ID),
 family=beta_family(link="logit"), data=age)
as.data.frame(anova(prop_null, prop_full, test="Chisq"))
drop1(prop_full, test="Chisq")
summary(prop_full)
source("boot_glmmTMB.r")
full.boot=boot.glmmTMB(prop_full, data=age,
 excl.non.conv=F, nboots=1000, para=T, resol=100,
 level=0.95, use=NULL, contr=NULL, n.cores=c("all-1", "all"))
full.boot$ci.estimates$fe

##HYPOTHESIS TESTING##
#Full vs. null model
hypothesis<-glmer(hypothesis.num~age*culture+sex+(1|ID), family=binomial, data=age)
hypothesis.null<-glmer(hypothesis.num~sex+(1|ID), family=binomial, data=age)
as.data.frame(anova(hypothesis.null, hypothesis, test="Chisq"))
#Full vs. reduced model
hypothesis.red<-glmer(hypothesis.num~age+culture+sex+(1|ID), family=binomial, data=age)
as.data.frame(anova(hypothesis.red, hypothesis, test="Chisq"))
drop1(hypothesis.red, test="Chisq")
summary(hypothesis.red)
boot.res=boot.glmm.pred(model.res=hypothesis.red, excl.warnings=T,
 nboots=1000, para=T)
round(boot.res$ci.estimates, 3)

##COMFORTING FAMILIARITY##
#Full vs. null model
comforting<-glmer(comforts.num~adult*culture+sex+(1|ID), family=binomial, data=fam)
comforting.null<-glmer(comforts.num~sex+(1|ID), family=binomial, data=fam)
as.data.frame(anova(comforting.null, comforting, test="Chisq"))
#Full vs. reduced
comforting.red<-glmer(comforts.num~adult+culture+sex+(1|ID), family=binomial, data=fam)
as.data.frame(anova(comforting.red, comforting, test="Chisq"))
drop1(comforting.red, test="Chisq")
summary(comforting.red)
boot.res=boot.glmm.pred(model.res=comforting.red, excl.warnings=T,
 nboots=1000, para=T)
round(boot.res$ci.estimates, 3)

##CONCERN FAMILIARITY##
#Full vs. null model
fam$tr.concern.exp=(fam$concern.exp*(nrow(fam) - 1) + 0.5)/nrow(fam)
prop_full=glmmTMB(tr.concern.exp~adult*culture+sex+(1|ID),
 family=beta_family(link="logit"), data=fam)
prop_null=glmmTMB(tr.concern.exp~sex+(1|ID),
 family=beta_family(link="logit"), data=fam)
as.data.frame(anova(prop_null, prop_full, test="Chisq"))
drop1(prop_full, test="Chisq")
summary(prop_full)
source("boot_glmmTMB.r")
full.boot=boot.glmmTMB(prop_full, data=fam,
 excl.non.conv=F, nboots=1000, para=T, resol=100,
 level=0.95, use=NULL, contr=NULL, n.cores=c("all-1", "all"))
full.boot$ci.estimates$fe


##HYPOTHESIS TESTING##
#Full vs. null model
hypothesis<-glmer(hypothesis.num~adult*culture+sex+(1|ID), family=binomial, data=fam)
hypothesis.null<-glmer(hypothesis.num~sex+(1|ID), family=binomial, data=fam)
as.data.frame(anova(hypothesis.null, hypothesis, test="Chisq"))
drop1(hypothesis, test="Chisq")
summary(hypothesis)
boot.res=boot.glmm.pred(model.res=hypothesis, excl.warnings=T,
 nboots=1000, para=T)
round(boot.res$ci.estimates, 3)

##LONGITUDINAL PREDICTIVENESS
full_concern=glm(comforts.num~conc9prop*culture+sex, data=long, family=binomial)
null_concern<-glm(comforts.num~sex, data=long, family=binomial)
anova(null_concern, full_concern, test="Chisq")
summary(full_concern)
drop1(full_concern, test="Chisq")
cbind(orig=coef(full_concern), confint(object=full_concern))

##EFFECT OF VERBAL CUES
cuesY<-subset(xdata, cue2!="NA" & comf=="Y" & age=="18")
cuesY<-droplevels(cuesY)
chisq.test(cuesY$culture, cuesY$cue2)$expected
chisq.test(cuesY$culture, cuesY$cue2)

##COMFORTING CONTROL VS EXPERIMENTAL##
mother<-subset(xdata, =="Mother")
#Full vs. null model
comforting<-glmer(comforts.num~condition*age+condition*culture+sex+(1|ID), family=binomial, data=xdata)
comforting.null<-glmer(comforts.num~sex+(1|ID), family=binomial, data=xdata)
as.data.frame(anova(comforting.null, comforting, test="Chisq"))
#Full vs. reduced model
comforting.red<-glmer(comforts.num~condition+age+culture+sex+(1|ID), family=binomial, data=xdata)
as.data.frame(anova(comforting.red, comforting, test="Chisq"))
drop1(comforting.red, test="Chisq")
summary(comforting.red)
boot.res=boot.glmm.pred(model.res=comforting, excl.warnings=T,
 nboots=1000, para=T)
round(boot.res$ci.estimates, 3)

##CONCERN CONTROL VS EXPERIMENTAL##
#Full vs. null model
xdata$tr.concern.exp=(xdata$concern.exp*(nrow(xdata) - 1) + 0.5)/nrow(xdata)
prop_full=glmmTMB(tr.concern.exp~condition*culture+condition*age+sex+(1|ID),
 family=beta_family(link="logit"), data=xdata)
prop_null=glmmTMB(tr.concern.exp~sex+(1|ID),
 family=beta_family(link="logit"), data=xdata)
as.data.frame(anova(prop_null, prop_full, test="Chisq"))
#Full vs. reduced model
prop_red=glmmTMB(tr.concern.exp~condition+culture+age+sex+(1|ID),
 family=beta_family(link="logit"), data=xdata)
as.data.frame(anova(prop_red, prop_full, test="Chisq"))
drop1(prop_red, test="Chisq")
summary(prop_red)
boot.res=boot.glmm.pred(model.res=concern, excl.warnings=T,
 nboots=1000, para=T)
round(boot.res$ci.estimates, 3)

##HYPOTHESIS CONTROL VS EXPERIMENTAL##
#Full vs. null model
hypothesis<-glmer(hypothesis.num~condition*age+condition*culture+sex+(1|ID), family=binomial, data=xdata)
hypothesis.null<-glmer(hypothesis.num~sex+(1|ID), family=binomial, data=xdata)
as.data.frame(anova(hypothesis.null, hypothesis, test="Chisq"))
#Full vs. reduced model
hypothesis.red<-glmer(hypothesis.num~condition+age+culture+sex+(1|ID), family=binomial, data=xdata)
as.data.frame(anova(hypothesis.red, hypothesis, test="Chisq"))
drop1(hypothesis.red, test="Chisq")
summary(hypothesis)
boot.res=boot.glmm.pred(model.res=hypothesis, excl.warnings=T,
 nboots=1000, para=T)
round(boot.res$ci.estimates, 3)