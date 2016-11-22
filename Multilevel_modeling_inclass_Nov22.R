library(tidyverse)

#load data
indiv.data <- read.csv("bh1996.csv")

#View(indiv.data)

#create the group level data
indiv.grouped <- group_by(indiv.data, GRP)
group.data <- indiv.grouped %>% summarise(HRS.GRP=mean(HRS, na.rm=TRUE), N.GRP=n())
#na.rm = if it's missing, remove it (TRUE)

View(group.data)

#could also divide up by group data only - create data that only has group level stuff


multilevel.data <- full_join(indiv.data, group.data, by="GRP")
View(multilevel.data)
#can see mean value is the same for everyone in that group, value changes when group changes 

library(nlme)

#Compare these two
Intercept.Model.Ignoring.Groups <- gls(WBEING ~ 1, data = multilevel.data)
Intercept.Model.With.Groups     <- lme(WBEING ~ 1, random = ~1|GRP, data = multilevel.data)
#each group can have it's own mean and then there will be an overall grand mean

#with anova command, say "are those different"
anova(Intercept.Model.Ignoring.Groups,Intercept.Model.With.Groups)
#basically saying there is a sig difference with including groups
#by taking group info into account, can better predict 
#group mean does help you understand variance in people's well-beng scores 

#                                Model df      AIC      BIC    logLik   Test  L.Ratio
#Intercept.Model.Ignoring.Groups     1  2 19540.17 19553.98 -9768.084                
#Intercept.Model.With.Groups         2  3 19353.34 19374.06 -9673.669 1 vs 2 188.8303
#p-value
#Intercept.Model.Ignoring.Groups        
#Intercept.Model.With.Groups      <.0001

##saying, if we DONT take groups into account, versus when we do take group into account 

VarCorr(Intercept.Model.With.Groups)
#Variance   StdDev   
#(Intercept) 0.03580077 0.1892109
#Residual    0.78949727 0.8885366

VarCorr(Intercept.Model.With.Groups)
#> # effect / (effect + error)
#> 0.03580077 / (0.03580077 + 0.78949727)
#[1] 0.0433792


## 4% of variance accounted for by group level 
## if p value wasn't significant, then dont need to do multilevel modelling - can just do what we've done for the rest of the course

#fixed effects - will give us fixed effects 
Model.1<-lme(WBEING ~ HRS + HRS.GRP, random=~1|GRP, data=multilevel.data)
summary(Model.1)
#for every hour you work as an individual, you work score goes down by .04
#also look at group mean
#group norms have negative effect on wellbeing 

##random effects 
VarCorr(Model.1)

