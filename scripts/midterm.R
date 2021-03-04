### QUESTION 1
# use the datasets 'pinelands_bees.csv' and 'land_cover.csv' to answer the question:
# Does natural land cover in the surrounding landscape predict the number of individual bees collected at a site?
# follow the analysis workflow that we learned in class, for which there are prompts below
# make sure to complete all 6 prompts
# use comment lines to briefly explain your reasoning at each step (ie, why you are doing what you are doing)
# you will turn in your code (ie, this R file) to your github repo at the end of class

## brief metadata
# the datasets you will use are 'pinelands_bees.csv'  and 'land_cover.csv'
# these are data I collected on wild bees of 117 ish species (ish because the taxonomy of some specimens is not fully sorted out) 
# data were collected at 27 study sites in the New Jersey pinelands
# all sites were in forest habitat, but sites differed in how much of the surrounding landscape was forested
# bees were collected on four days at each site between April and September 2003, with collection events organized into rounds, such that each site was sampled once in round 1 before round 2 was begun, etc
# 'pinelands_bees.csv'contains data on the bees collected, where each row is one individual bee
# 'land_cover.csv' column 'Nat1600' is the percentage of the natural land cover surrounding the site within a 1600m radius (roughly an 800 ha area), where the natural habitat is predominantly forest, with a small amount of other natural habitats such as open wetlands
library(tidyverse)
library(ggfortify)
library(broom)
library(MuMIn)
library(lme4)

##  1 Get and format the data
# you will need to create a new dataframe that has the variables you need: site, land cover, number of bees collected
# you may want to use group_by, summarize, n(), left_join
setwd("C:/github/aeda_bos/raw-data")

#Read in data
bees <-read_csv("pinelands_bees.csv")
land_cover <- read_csv("land_cover.csv")

#Check that all site names in "bees" dataset have corresponding rows in land_cover
table(bees$site_name %in% land_cover$site_name)

#Aggregate variables of interest and join into new dataframe 
bee_join <- left_join(bees,land_cover,by="site_name")
nbees <- as.data.frame(summarize(group_by(bee_join,site_name),n()))
lc <- as.data.frame(summarize(group_by(bee_join,site_name),median(Nat1600)))
bee_sum <- inner_join(nbees,lc,by="site_name")
colnames(bee_sum) <- c("site_name","nbees","land_cover")

## 2 Data picture
# plot the data and figure out what type of model might be best
hist(bee_sum$nbees,breaks=8)
plot(bee_sum$land_cover,bee_sum$nbees,xlab="Forested landscape",ylab="Number of bees collected")

## 3 Test model assumptions
# you can test the assumptions of more than one type of model here if you aren't sure
# explain your reasoning for the model you choose

#Since the response variable is count data,my first guess is to use a GLM from the Poisson or negative binomial family. 
# I will also make a normal linear model, for comparison
bee_mod_norm <- glm(nbees ~ land_cover, data=bee_sum,family=gaussian)
bee_mod_nb <- glm.nb(nbees ~ land_cover, data=bee_sum)
bee_mod_pois <- glm(nbees ~ land_cover, data=bee_sum,family=poisson)

autoplot(bee_mod_norm)
autoplot(bee_mod_pois)
autoplot(bee_mod_nb)

#None is perfect, but based on distribution of residuals, the Poisson distribution looks best.
#There isn't a clear pattern in the residuals vs. fit plot, the normal Q/Q plot looks okay, the scale-location plot shows
#residuals that are mostly homoskedastic, and the residuals vs. legerage plot only shows one concerning point.

## 4 Report and interpret results
# make sure you interpret the following things: coefficients, p value, explanatory value of model
# state your conclusions using numbers and units
# discuss the magnitude and biological significance of your result, not just its significance
# how confident are you in this analysis and why

#Summarize model results (on scale of link function)
summary(bee_mod_pois)

#The p-value of land_cover is <2e-16, indicating a highly significant relationship between proportion of forested 
#land cover and number of bees collected. The coefficient for land cover is negative, indicating that as proportion
#of forest increases, the number of collected bees decreases. 

#Get coefficient for land cover on scale of data 
exp(1)^(bee_mod_pois$coefficients[2])

#Get intercept on scale of data 
exp(1)^(bee_mod_pois$coefficients[1])

#According to this model, at a site with zero surroudning forest cover we predict we will find about 82 individual bees.
# For every additional percentage of forest cover surrouding the site, we expect to find 0.99 fewer bees. 

var_explained <- (1 - (bee_mod_pois$deviance/bee_mod_pois$null.deviance))
# Variation in forest cover explains about 19% of variation in number of bees collected. Could be worse. 

#I am confident that there is some negative relationship between forest cover and bees collected, (extremely low p-value)
#but due to the small sample size and relatively low explained variance, I am less confident in the exact values
# for the coefficient and intercept.

## 5 Plot model results back onto the data picture
# geom_smooth is the easy way to do this, but you can alternatively do it manually using the model output (coefficients) if you want
bee_pois_aug <- augment(bee_mod_pois, data=bee_sum, type.predict="response", se_fit=T)  
bee_pois_link <- augment(bee_mod_pois, data=bee_sum, type.predict="link", se_fit=T)
bee_pois_link <- mutate(bee_pois_link, ci_lwr = .fitted - (1.96 * .se.fit), ci_upr = .fitted + (1.96 * .se.fit))
bee_pois_aug_scaled <- mutate(bee_pois_link, .fitted = exp(.fitted), ci_lwr = exp(ci_lwr), ci_upr = exp(ci_upr))

ggplot(bee_pois_aug, aes(x=land_cover, y=nbees)) +
  geom_point() +
  xlab("% forest cover")+
  ylab("bee abundance")+
  geom_smooth(aes(x = land_cover, y = .fitted))+
  geom_ribbon(data = bee_pois_aug_scaled, aes(ymin = ci_lwr, ymax = ci_upr), alpha = 0.1) 
#There's a lot of variability but the model fit is reasonable. 
  
## 6  If you were to add a random effect to this model, what would it be, what would adding it accomplish?
# please answer this question in comment lines
# you do not need to format any data here or code or run anything
# explain your reasoning for using this random effect
# you will need to go back to the original data file and metadata to answer this question
# extra credit: write out the model code for your model now including the random effect

#We might want to use either "round" or "method" as a random effect, since they
#might affect the number of bees sampled, but aren't really of interest. Here I use round:
bees$round <-as.factor(bees$round)
nbees_r <- as.data.frame(summarize(group_by(bee_join,site_name,round),n()))
lc_r <- as.data.frame(summarize(group_by(bee_join,site_name,round),median(Nat1600)))
bee_rand <- inner_join(nbees_r,lc_r,by=c("site_name","round"))
colnames(bee_rand) <- c("site_name","round","nbees","land_cover")

#Rescale land_cover
bee_rand$land_cover <- scale(bee_rand$land_cover)

#Run models
bees_randintercept <-  glmer(nbees~ land_cover + (1|round), data=bee_rand,family="poisson")
bees_randslope <-  glmer(nbees ~ land_cover+ (land_cover|round), data=bee_rand,family="poisson")

#The AIC on both of the random effects models is much worse than the fixed effects only model.
#Using round as a random effect is a bad idea. 

### QUESTION 2
# The file "modSel.csv" contains simulated dataset of observations of a focal species at a series of sites.
# For each site, you have observed abundance, and measurements of environmental variables you hypothesize
# to affect the distribution of this species.
# Specifically, you hypothesize the species is more abundant in warmer, wetter regions,
# and that it prefers core over edge habitat.
# You have measured each of these in a couple ways, as mean annual and summer temperature,
# cumulative annual and summer precipitation, and distance to nearest edge and total edge within 500 m.
# Your goal here is to find the best model you can, given your hypotheses,
# to describe the distribution of this species.
# In doing so, you will also assess the more relevant measure of each environmental condition,
# and weigh their relative importance (or at least predictive power).
# For simplicity, do not consider interactions between variables.
# Please give your models interpretable names.

modSel = read_csv("modSel.csv")

# Step 1. Find the best error structure/error distribution for these data.
# State your conclusion in comment lines
# (Hints: you want to assess model-error distributions, not the data distribution; these are count data.)

#Data are count data; our best choices are a Poisson distribution or a negative binomial distribution
summary(fitdist(modSel$observedAbundance,"pois"))
summary(fitdist(modSel$observedAbundance,"nbinom"))

summary(glm.nb(observedAbundance ~ 1, data=modSel))
summary(glm(observedAbundance ~ 1, data=modSel,family=poisson))

#By AIC, the negative binomial model is better

# Step 2: Having determined the best error structure, determine the more effective method of measuring each variable.
# For each variable, compare methods as a pair of single-variable models (e.g., summer temp vs annual temp).
# State your conclusion in comment lines
glm.nb(observedAbundance ~ meanAnnualTemp,data=modSel)$aic
glm.nb(observedAbundance ~ meanSummerTemp,data=modSel)$aic
#Summer temperature outperforms annual temperature

glm.nb(observedAbundance ~ annualPrecipitation,data=modSel)$aic
glm.nb(observedAbundance ~ summerPrecipitation,data=modSel)$aic
#Summer precipitation outperforms annual precipitation

glm.nb(observedAbundance ~ totalEdge,data=modSel)$aic
glm.nb(observedAbundance ~ distance2edge,data=modSel)$aic
#Total edge outperforms distance2edge

# Step 3: Having determined which method of measurement for each variable is best,
# determine the most effective combination of predictors;
# run a set of competing models and create a table comparing these models to each other and to a null.
# state your conclusion in comment lines
Null <-glm.nb(observedAbundance ~ 1,data=modSel)
Temp<-glm.nb(observedAbundance ~ meanSummerTemp,data=modSel)
Precip <-glm.nb(observedAbundance ~ summerPrecipitation,data=modSel)
Edge <-glm.nb(observedAbundance ~ totalEdge,data=modSel)
TempPrecip <- glm.nb(observedAbundance ~ meanSummerTemp + summerPrecipitation,data=modSel)
TempEdge <-glm.nb(observedAbundance ~ meanSummerTemp + totalEdge,data=modSel)
PrecipEdge <-glm.nb(observedAbundance ~ summerPrecipitation + totalEdge,data=modSel)
TempPrecipEdge <-glm.nb(observedAbundance ~ meanSummerTemp + summerPrecipitation + totalEdge,data=modSel)

model.sel(Null,Temp,Precip,Edge,TempPrecip,TempEdge,PrecipEdge,TempPrecipEdge)
summary(TempEdge)
summary(Precip)

#The model including mean summer temperature and total edge area is the most effective, with the lowest AIC and the 
#highest weight. 

# Step 4: Interpret these results.
# Were your hypotheses supported? What is the relative importance of each predictor?
#The most important single predictor is temperature, followed by total edge. Higher temperatures are associated
# with increased abundance of the species, whereas higher amount of edge is associated with decreased abundance.
#The best model does not include precipitation, and even in a model by itself, summer precipitation is not a significant 
# predictor of species abundance. 

# What is your general conclusion?
# We are more likely to find the focal species in sites with hot summer temperatures and minimal edge within 500 meters. 
