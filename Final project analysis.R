##Load packages
library(lme4)
library(lmerTest)
library(dplyr)
library(ggplot2)
library(car)
library(psych)
library(texreg)
##Set working directory
setwd('C:/Users/ktcv3/My Drive/Course work/MLM/Data')
##Load data
csemdata<-foreign::read.spss('DKJRRKmg4_KS.sav', 
                                             to.data.frame=TRUE, use.missings = TRUE, max.value.labels = TRUE)


##Specify variables for final project analysis
myvars<-c("respondentID", "loc_code", "Age", "race_0", "race_1", "race_2", "race_3", "race_4","race_5","race_6", "Adminty", "SxOfftype", "HypSxRat", "impul", "conchr", "lkempcal", "lkprtkg")
working<-csemdata[myvars]
##Recode small categories
working$loc_code<-dplyr::recode(working$loc_code, `3` = 1)
working$loc_code<-dplyr::recode(working$loc_code, `1` = 9)
cbind(table(working$loc_code))
##Check if location coding is factor
is.factor(working$loc_code)
##Make location a factor
working$loc_code<-ordered(working$loc_code,
                          levels = c(2, 4, 5, 6, 7, 8, 9),
                          labels = c("Secure Treatment Unit", "Dr. James Reavis", 
                                     "Bridgewater Treatment Center", "MSPP", "Linolakes", 
                                     "New Jersey ADTC", "Other"))
##Remove 3 observations with location code "Other" and administration type "1"
##Subset observations with both values
toremove<-subset(working, working$loc_code == "Other" & working$Adminty == 1)
##Get respondent IDs
exclusion_values <- toremove$respondentID
##Filter out excluded respondent IDs 
working <- working %>% filter(! respondentID %in% exclusion_values)
##Check work
sum(working$loc_code == "Other" & working$Adminty == 1)
table(working$loc_code)

##Filter out missing values for age
working <- working %>% filter(! Age == -1)

##Check if sex offense type is a factor
is.factor(working$SxOfftype)
##Coerce Sex offense type into a factor
working$SxOfftype<-factor(working$SxOfftype,
                         levels = c(1, 2, 3, 5),
                         labels = c("Rapist", "Extrafamilial CM", "Incest", "CSEM Only"))
##Get frequency table for sex offense type
sxofftable<-as.data.frame(table(working$SxOfftype))
sxofftable$relFreq<-sxofftable$Freq/length(working$SxOfftype)
sxofftable
##Create dummy-coded sex offense variables
working$extrafamCM<-ifelse(working$SxOfftype == "Extrafamilial CM", 1, 0)
working$incest<-ifelse(working$SxOfftype == "Incest", 1, 0)
working$CSEM<-ifelse(working$SxOfftype == "CSEM Only", 1, 0)

##Dummy code admin type
working$adminty_dc<-ifelse(working$Adminty==1, 0, 1)
##Check if administration type is a factor
is.factor(working$adminty_dc)
##Coerce administration type into factor
working$rschadmin<-factor(working$adminty_dc,
                        levels = c(0, 1),
                        labels = c("Clinical", "Research"))
##Create table of frequencies for admin type
admintable<-as.data.frame(table(working$rschadmin))
admintable$relFreq<-admintable$Freq/length(working$rschadmin)
admintable

##Get frequencies for race
##Race
##Start by summing each of the race variables
afam.sum<-sum(working$race_0=='1', na.rm = TRUE)
asian.sum<-sum(working$race_1=='1', na.rm=TRUE)
cauc.sum<-sum(working$race_2=='1', na.rm=TRUE)
hisp.sum<-sum(working$race_3=='1', na.rm=TRUE)
na.sum<-sum(working$race_4=='1', na.rm=TRUE)
oth.sum<-sum(working$race_5=='1', na.rm=TRUE)
##Get the relative frequencies
afam.rf<-afam.sum/nrow(working)
asian.rf<-asian.sum/nrow(working)
cauc.rf<-cauc.sum/nrow(working)
hisp.rf<-hisp.sum/nrow(working)
na.rf<-na.sum/nrow(working)
oth.rf<-oth.sum/nrow(working)

#collect into a table
afam.data<-cbind(afam.sum, afam.rf)
asian.data<-cbind(asian.sum, asian.rf)
cauc.data<-cbind(cauc.sum, cauc.rf)
hisp.data<-cbind(hisp.sum, hisp.rf)
na.data<-cbind(na.sum, na.rf)
oth.data<-cbind(oth.sum, oth.rf)

race.labs<-c('African American', 'Asian', 'Caucasian', 'Hispanic', 'Native American or Aboriginal', 'Other')
races<-rbind(race.labs)
race.col.labs<-c('Race','Frequency', 'Relative Freqency')
race.data<-rbind(afam.data, asian.data, cauc.data, hisp.data, na.data, oth.data)
race.table<-cbind(race.labs, race.data)
colnames(race.table)<-race.col.labs
##row.names(race.data)<-race.labs
race.table
##Graph race data
race.data.df<-(as.data.frame.matrix(race.table))
race.graph<-ggplot(race.data.df, aes(x=Race, y=as.numeric(Frequency)))+
  geom_bar(color = "#b1b3b4", fill = "#03a9fc", stat="identity")+
  scale_y_continuous(breaks=c(0,25,50,75,100, 125, 150, 175, 200, 225, 250))+
  ylab("Frequency")+
  ggtitle("Frequency of Races")+
  theme(plot.title=element_text(hjust=0.5))
race.graph

##Get descriptives for age
describe(working$Age)
ggplot(working, aes(Age))+
  geom_bar(color = "#b1b3b4", fill = "#03a9fc")
##Create group mean variables
grp.mean<-aggregate(cbind(Age, impul, conchr, lkempcal, lkprtkg) ~ loc_code,
                    data = working, mean)
##Double check variables
head(grp.mean)
##Give new variable names
colnames(grp.mean)<- c('loc_code', 'loc_age', 'loc_impul', 'loc_conchr', 'loc_lkemp', 'loc_prtkg')
##Merge data sets
working<-merge(working, grp.mean, by = "loc_code")
##Double check work
head(working)

##Generate group-mean centered variables
working<-within(working, {
  age_grp<-Age-loc_age
  impul_grp<-impul-loc_impul
  conchr_grp<-conchr-loc_conchr
  emp_grp<-lkempcal-loc_lkemp
  prtkg_grp<-lkprtkg-loc_prtkg
})
##Check work
head(working)

##Look at plots of predictors against hypersexuality
##Create title formatting
c.title<-theme(plot.title = element_text(hjust = 0.5))
##Overall association between impulsivity and hypersexuality
ggplot(data = working, aes(x = impul_grp, y = HypSxRat)) + 
  geom_point(position = 'jitter') +
  geom_smooth(method = 'lm', se = FALSE)+
  theme_classic()+
  ggtitle('The Total Effect of Impulsivity on Hypersexuality \nWithout Consideration of Between-location Variation') +
  ylab('Hypersexuality') + xlab('Group-centered Impulsivity') + 
  c.title
##Overall association between conning and charm and hypersexuality
ggplot(data = working, aes(x = conchr_grp, y = HypSxRat)) + 
  geom_point(position = 'jitter') +
  geom_smooth(method = 'lm', se = FALSE)+
  theme_classic()+
  ggtitle('The Total Effect of Conning and Superficial Charm on Hypersexuality \nWithout Consideration of Between-location Variation') +
  ylab('Hypersexuality') + xlab('Group-centered Cunning and Superficial charm') + 
  c.title
##Overall association between empathy and hypersexuality
ggplot(data = working, aes(x = emp_grp, y = HypSxRat)) + 
  geom_point(position = 'jitter') +
  geom_smooth(method = 'lm', se = FALSE)+
  theme_classic()+
  ggtitle('The Total Effect of Empathy on Hypersexuality \nWithout Consideration of Between-location Variation') +
  ylab('Hypersexuality') + xlab('Group-centered Empathy') + 
  c.title
##Overall association between impulsivity and hypersexuality
ggplot(data = working, aes(x = prtkg_grp, y = HypSxRat)) + 
  geom_point(position = 'jitter') +
  geom_smooth(method = 'lm', se = FALSE)+
  theme_classic()+
  ggtitle('The Total Effect of Perspective-taking on Hypersexuality \nWithout Consideration of Between-location Variation') +
  ylab('Hypersexuality') + xlab('Group-centered Perspective-taking') + 
  c.title
##Overall differences in hypersexuality by sexual offense
ggplot(working, aes(x = SxOfftype, y = HypSxRat)) +
  geom_boxplot()+
  xlab("Sex Offense Type") +ylab("Hypersexuality")



##Graph association in each location on one graph
##Between-location variation in association between impulsivity and hypersexuality
ggplot(working, aes(x = impul_grp, y = HypSxRat, col = loc_code)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, na.rm = TRUE) +
  ggtitle('Within-Group Slopes of Impulsivity on Hypersexuality by Location') +
  guides() + c.title
##Between-location variation in association between charm/conning and hypersexuality
ggplot(working, aes(x = conchr_grp, y = HypSxRat, col = loc_code)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, na.rm = TRUE) +
  ggtitle('Within-Group Slopes of Conning and Superficial Charm on Hypersexuality by location') +
  guides() + c.title
##Between-location variation in association between empathy and hypersexuality
ggplot(working, aes(x = emp_grp, y = HypSxRat, col = loc_code)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, na.rm = TRUE) +
  ggtitle('Within-Group Slopes of Empathy on Hypersexuality by Location') +
  guides() + c.title
##Between-location variation in association between perspective taking and hypersexuality
ggplot(working, aes(x = prtkg_grp, y = HypSxRat, col = loc_code)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, na.rm = TRUE) +
  ggtitle('Within-Group Slopes of Perspective-Taking on Hypersexuality by Location') +
  guides() + c.title
##Between-location variation in differences in hypersexuality by sex offense type
ggplot(working, aes(x = SxOfftype, y = HypSxRat, fill = loc_code)) +
  geom_boxplot()+
  xlab("Sex Offense Type") +ylab("Hypersexuality")

##Compare intercept only ols model with null multilevel model
m0.ols<-lm(HypSxRat ~ 1, data = working)
##Null ml model
m0<-lmer(HypSxRat ~ + (1 | loc_code), data = working)
##Compare models
anova(m0, m0.ols)
summary(m0)
var.cov<-data.frame(summary(m0)$varcor)
##Calculate ICC
var.cov[1, 4]/(var.cov[1, 4]+var.cov[2, 4])
##Check confidence intervals of random effects
confint(m0)

##Model 1: Additive model, no random variation
m1<-lmer(HypSxRat~ rschadmin + extrafamCM + incest + CSEM + impul_grp+ loc_impul + age_grp + loc_age + (1 | loc_code), data = working)
summary(m1)
anova(m0, m1)

##Model 2: Additive model, random variation
m1.rand<-lmer(HypSxRat~rschadmin + extrafamCM + incest + CSEM + impul_grp+ loc_impul + age_grp + loc_age + (1 +impul_grp||loc_code), data = working)
summary(m1.rand)
anova(m1, m1.rand)

##Model 3: Interactive model
m2<-lmer(HypSxRat ~ rschadmin + (extrafamCM + incest + CSEM)*impul_grp + loc_impul + age_grp + loc_age + (1 | loc_code), data = working)
summary(m2)
anova(m1, m2)

##Model 4: double check random variation
m2.rand<-lmer(HypSxRat ~ rschadmin + (extrafamCM + incest + CSEM)*impul_grp + loc_impul + age_grp + loc_age + (1 +impul_grp||loc_code), data = working)
summary(m2.rand)
anova(m1, m2.rand)

##Show model results in one table -- Save to word file
wordreg(list(m0, m1, m1.rand, m2),
          file = "finaltable2.doc",
          #single.row=T,
          #stars=numeric(0),
          caption= "",
          custom.note="Note: ***p<.001, **p<.01, *p<.05
          Model 0: Null model
          Model 1: Additive model, no random variation in impulsivity
          Model 2: Additive model, random variation in impulsivity
          Model 3: Interactive model"
          
)

##Examine model 1 further
##Get information about parameter estimates
(m1.coef<-coef(summary(m1)))
confint(m1, c('impul_grp', 'age_grp', 'loc_age'), method = 'Wald')
##Contribution of impulsivity
##Create restricted model
m1r<-lmer(HypSxRat~rschadmin + extrafamCM + incest + CSEM + loc_impul + age_grp + loc_age + (1 | loc_code), data = working)
##Get variance information
ran0<-data.frame(VarCorr(m0))
ran1<-data.frame(VarCorr(m1))
ran1r<-data.frame(VarCorr(m1r))
##Calculate variance reduction due to impulsivity
(ran1r[2,4]-ran1[2, 4])/ran0[2, 4]
##Graph association from m1
ggplot(working, aes(x = impul_grp, y = HypSxRat))+
  geom_blank()+
  geom_abline(aes(slope = m1.coef['impul_grp', 'Estimate'], intercept = m1.coef['(Intercept)', 'Estimate']),
              lwd=2)+
  ylim(0, 6)+
  ggtitle("Relationship Between Hypersexuality and Impulsivity in Sex Offenders")
##Check if all sex offense types simultaneously = 0
lht(m1, c('extrafamCM', 'incest', 'CSEM'))
##Sum of child offenses is 0
lht(m1, c('extrafamCM + incest + CSEM'))
##check if CSM is different from extrafamilial CM and incest
csemcomp<-multcomp::glht(m1, linfct = c('extrafamCM-CSEM = 0', 'incest - CSEM =0'))
summary(csemcomp, test = multcomp::adjusted(type = 'none'))

##Residual analysis
##Raw level 1 residuals
res.m1.ij<-resid(m1, scaled = FALSE)
##QQ Plot of residuals
layout(1, 1)
qqnorm(res.m1.ij, col = "blue", main = 'Normal QQ Plot for Level 1 Residuals')
qqline(res.m1.ij, lty = 'dotted')
##check homoscedasticity of level 1 residuals
plot(m1, main = "Fitted Values vs. Residuals: Checking Homoscedasticity")
##Residuals vs. impulsivity
ggplot(data = working, aes(x = impul_grp, y = res.m1.ij))+
  geom_point(alpha = 0.2)+
  geom_smooth(se = FALSE)+
  labs(y = 'Level 1 Residuals', title = "Level 1 Residuals vs Impulsivity")

##Level 2 residuals
##Prepare level 2 residuals
##Retrieve residuals
m1.lv2.resid<-ranef(m1, condVar = TRUE)[[1]]
head(m1.lv2.resid)
##Get institution level data
loc.only<-working[!duplicated(working$loc_code), c('loc_code','rschadmin', 'loc_age', 'loc_impul')]
##Add loc_code to residual data
m1.lv2.resid$loc_code<-row.names(m1.lv2.resid)
##Combine location level data and residuals
m1.lv2.resid<-merge(loc.only, m1.lv2.resid, by = 'loc_code')
head(m1.lv2.resid)
##Check distribution of level 2 variables
##normal qq plots
qqnorm(m1.lv2.resid$`(Intercept)`, col = "blue", main = 'Normal QQ Plot for U0j')
qqline(m1.lv2.resid$`(Intercept)`, lty = 'dotted')
##Plots of residuals vs predictor
plot(mfrow = c(2, 2))
##administration
plot(`(Intercept)`~rschadmin, data = m1.lv2.resid, xlab = "Administration Type", 
     ylab = "Location level intercept residual", main = "Administration Type Vs. Location Level Residual", col = "blue")
##Age
plot(`(Intercept)`~loc_age, data = m1.lv2.resid, main = "Institution average age vs Location level residual", col = "blue")
##Impulsivity
plot(`(Intercept)`~loc_impul, data = m1.lv2.resid, main = "Institution average impulsivity vs Location level residual", col = "blue")


##Create sample removing "other" group
##Subset observations with both values
removeoth<-subset(working, working$loc_code == "Other")
##Get respondent IDs
exclusion_oth <- removeoth$respondentID
##Filter out excluded respondent IDs 
working2 <- working %>% filter(! respondentID %in% exclusion_oth)
##Check work
table(working2$loc_code)

##Graph association in each location on one graph
##Between-location variation in association between impulsivity and hypersexuality
ggplot(working2, aes(x = impul_grp, y = HypSxRat, col = loc_code)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, na.rm = TRUE) +
  ggtitle('Within-Group Slopes of Impulsivity on Hypersexuality by Location') +
  guides() + c.title

##Compare intercept only ols model with null multilevel model
m0.ols2<-lm(HypSxRat ~ 1, data = working2)
##Null ml model
m02<-lmer(HypSxRat ~ + (1 | loc_code), data = working2)
##Compare models
anova(m02, m0.ols2)
summary(m02)
var.cov2<-data.frame(summary(m02)$varcor)
##Calculate ICC
var.cov2[1, 4]/(var.cov2[1, 4]+var.cov2[2, 4])
##Check confidence intervals of random effects
confint(m02)

##Model 1: Additive model, no random variation
m12<-lmer(HypSxRat~ rschadmin + extrafamCM + incest + CSEM + impul_grp+ loc_impul + age_grp + loc_age + (1 | loc_code), data = working2)
summary(m12)
anova(m02, m12)

##Model 2: Additive model, random variation
m1.rand2<-lmer(HypSxRat~rschadmin + extrafamCM + incest + CSEM + impul_grp+ loc_impul + age_grp + loc_age + (1 +impul_grp||loc_code), data = working2)
summary(m1.rand2)
anova(m12, m1.rand2)

##Model 3: Interactive model
m22<-lmer(HypSxRat ~ rschadmin + (extrafamCM + incest + CSEM)*impul_grp + loc_impul + age_grp + loc_age + (1 | loc_code), data = working2)
summary(m22)
anova(m12, m22)

##Model 4: double check random variation
m2.rand2<-lmer(HypSxRat ~ rschadmin + (extrafamCM + incest + CSEM)*impul_grp + loc_impul + age_grp + loc_age + (1 +impul_grp||loc_code), data = working2)
summary(m2.rand2)
anova(m12, m2.rand2)

##Test dichotomous sex offense variables
working3<-working2
##Dummy code sex offense to be rapist vs. no
working3$rape.d<-ifelse(working3$SxOfftype=="Rapist", 1, 0)
table(working3$rape.d)
##Dummy code sex offense to be CSEM vs. no
working3$CSEM.d<-ifelse(working3$SxOfftype=="CSEM Only", 1, 0)
table(working3$CSEM.d)

##Compare intercept only ols model with null multilevel model
m0.ols3<-lm(HypSxRat ~ 1, data = working3)
##Null ml model
m03<-lmer(HypSxRat ~ + (1 | loc_code), data = working3)
##Compare models
anova(m03, m0.ols3)
summary(m03)
var.cov3<-data.frame(summary(m03)$varcor)
##Calculate ICC
var.cov3[1, 4]/(var.cov3[1, 4]+var.cov3[2, 4])
##Check confidence intervals of random effects
confint(m03)

##Model 1: Additive model, no sex offense
m13<-lmer(HypSxRat~ rschadmin + impul_grp+ loc_impul + age_grp + loc_age + (1 | loc_code), data = working3)
summary(m13)
anova(m03, m13)
##Model 1: Additive model, rapist only
m13.2<-lmer(HypSxRat~ rschadmin + rape.d + impul_grp+ loc_impul + age_grp + loc_age + (1 | loc_code), data = working3)
summary(m13.2)
anova(m13, m13.2)
##Model 2: Additive model, CSEM only
m13.3<-lmer(HypSxRat~ rschadmin + CSEM.d + impul_grp+ loc_impul + age_grp + loc_age + (1 | loc_code), data = working3)
summary(m13.3)
anova(m13, m13.3)

##Model 2: Interactive model, rapist only
m23.2<-lmer(HypSxRat~ rschadmin + rape.d*impul_grp+ loc_impul + age_grp + loc_age + (1 | loc_code), data = working3)
summary(m23.2)
anova(m13, m23.2)
##Model 2: Interactive model, CSEM only
m33.3<-lmer(HypSxRat~ rschadmin + CSEM.d*impul_grp+ loc_impul + age_grp + loc_age + (1 | loc_code), data = working3)
summary(m33.3)
anova(m13, m33.3)