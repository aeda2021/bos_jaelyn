# Class 6: Mixed Effects Models
# Turn this exercise sheet in by 
# 1) Going through the code below, filling in any sections that are missing
# 2) If the code asks you to make a figure, be sure to save it by name from this script into your figures/ directory. Name must start with 6_
# 3) Answering any questions (labeled Q#) by typing comments in this text (start line with #)
# 4) Committing this completed script and any figures to your Git repo
# 5) Pushing your commits to Github so that we can see.
# 6) This is due no later than the start of class 7.

# Read in and examine bee data
# Spobee column has density of P. larvae spores (the bacterium). 
# Hive has the ID of the hive sampled (3 samples/hive)
# Infection has a metric quantifying the degree of infection. We will turn this into yes/no whether infection is present. 
Bees <- read.table(url('https://raw.githubusercontent.com/aeda2021/2021_master/main/data/Bees.txt'), header=TRUE)
head(Bees)

setwd("C:/github/aeda_bos/raw-data")

Bees<-read.table("Bees.txt",header=TRUE)

# make hive a factor
Bees$fhive <- factor(Bees$Hive)

# Make a yes/no infection column (Infection01)
Bees$Infection01 <- Bees$Infection 
Bees$Infection01[Bees$Infection01 > 0] <- 1
Bees$fInfection01 <- factor(Bees$Infection01) # turn this into a factor

# Scale BeesN to improve model convergence (mean 0, standard deviation 1)
Bees$sBeesN <- scale(Bees$BeesN)

# Make a Cleveland dot-chart of spores vs. hive
setwd("C:/github/aeda_bos/figures")
dotchart(Bees$Spobee, groups = Bees$fhive, xlab='Spores', ylab='Hive ID')
dev.copy(pdf,'6_ClevelandDot.pdf')
dev.off()

# Q1. Does variance of spore density appear homogenous among hives? Why or why not?
# A: No, some hives appear to have much higher variance

# Q2. Try some transformations of the response variable to homogenize the variances (or at least improve it). Which transformation of spore density seems reasonable? Why?
Bees$LogSpores <-log(Bees$Spobee + 1)
dotchart(Bees$LogSpore, groups = Bees$fhive, xlab='Natural log spores', ylab='Hive ID')

# Q3. Develop a simple linear model for transformed spore density. Include infection (fInfection01), number of bees (sBeesN) and their interaction as explanatory variables. Check for a hive effect by plotting standardized residuals (see the residuals(yourmodel, type='pearson') function) against hive ID (fhive). Show your code and your plots. Do residuals look homogenous among hives?
mod1 <- lm(LogSpores ~ fInfection01 + sBeesN + sBeesN*fInfection01,data=Bees)
plot(Bees$fhive,residuals(mod1, type='pearson'),xlab="Hive",ylab="Standardized residuals",main="Log transformed linear mod residuals by hive")
#The residuals are definitely not homogeneous among hives
dev.copy(pdf,'6_LmResiduals.pdf')
dev.off()

# Q4. What are the advantages of including hive as a random effect, rather than as a fixed effect?
# A: We might choose to include hive as a random effect, since hive is a factor and we aren't actually interested in the effect of hive. We just want to control for hive
#while we evaluate other variables that actually are of interest. Including hive as a random, rather than fixed, effect, also uses fewer degrees of freedom

# Apply the Zuur protocol (10-step version outlined here, as used with the barn owl nesting data in Zuur Ch. 5):
# Step 1: Fit and check a "beyond optimal" linear regression (already done above)
# Step 2: Fit a generalized least squares version of the "beyond optimal" model (no need: we will use the linear regression model).
# Q5. Step 3. Choose a variance structure or structures (the random effects). What random effects do you want to try?
# A: We are interested in hive as a random effect

# We will now fit a mixed effects (ME) model. Zuur et al. used the nlme package in R, but Douglas Bates
#now has a newer package that is widely used and that is called lme4. The benefits of lme4 include greater 
#flexibility in the structure of the random effects, the option to use non-Gaussian error structures 
#(for generalized linear mixed effects models, or GLMMs), and more efficient code to fit models. 
#The main difference between nlme's lme() function and the lmer() function in lme4 is in how random effects are specified:
# model <- lmer(response ~ explanantoryvars + (1|random), data=mydata) # a random intercept model
# model <- lmer(response ~ explanantoryvars + (slope|random), data=mydata) # a random intercept and slope model
# One of the frustrations some people run into is that the lme4 package doesn't provide p-values. 
#This stems from disagreements and uncertainty about how best to calculate p-values. 
#However, if p-values are important to you, approximate p-values can be derived from the lmerTest package

# install.packages('lme4') # if needed
# install.packages('lmerTest') if needed
require(lmerTest)

# Q6. Step 4. Fit the "beyond optimal" ME model(s) with lmer() in the lme4 package (transformed spore density is response, fInfection01, sBeesN, and interaction are the explanatory variables). Show your code.
randi <- lmer(LogSpores ~ fInfection01 + sBeesN + sBeesN*fInfection01 + (1|fhive), data=Bees)
rands <- lmer(LogSpores ~ fInfection01 + sBeesN + sBeesN*fInfection01 + (sBeesN-1|fhive), data=Bees)
randb <- lmer(LogSpores ~ fInfection01 + sBeesN + sBeesN*fInfection01 + (sBeesN|fhive), data=Bees)

# Q7. Step 5. Compare the linear regression and ME model(s) with a likelihood ratio test, including 
#correction for testing on the boundary if needed. Use the anova() command. This will re-fit your lmer 
#model with maximum likelihood, but this is OK (note there are some debates about exactly how to best 
#compare an lm and lmer model). Show your work and the results. Which random effect structure do you 
#choose based on the results?
lrtest <- anova(mod1,randi,rands,randb)
#Based on AIC, random intercept only seems the best

# Q8. Step 6. Check the model: plot standardized residuals vs. fitted values and vs. each predictor. (You can get standardized residuals with residuals(yourmodel, type='pearson')). How do they look?
par(mfrow=c(2,2))
plot(fitted.values(randi),residuals(randi,type="pearson"),xlab="Fitted values",ylab="Standardized residuals")
plot(Bees$fInfection01,residuals(randi,type="pearson"),xlab="Infection",ylab="Standardized residuals")
plot(Bees$sBeesN,residuals(randi,type="pearson"),xlab="Standard N. Bees",ylab="Standardized residuals")
plot(Bees$fhive,residuals(randi,type="pearson"),xlab="Hive",ylab="Standardized residuals")
mtext("Residuals by variable, random intercept model",outer=T,line=-2,cex=1.5)
dev.copy(pdf,'6_RandInterceptResiduals.pdf')
dev.off()
#The residuals look okay. 

# Q9. Step 7. Re-fit the full model with ML (set REML=FALSE) and compare against a reduced model without the interaction term, also fit with ML. Use anova() to compare the models. Which model do you choose? Why?
mod2 <- lmer(LogSpores ~ fInfection01 + sBeesN + sBeesN*fInfection01 + (1|fhive), data=Bees,REML=F)
mod3 <- lmer(LogSpores ~ fInfection01 + sBeesN + (1|fhive), data=Bees,REML=F)
anova(mod2,mod3)
#The model without the interaction term has a lower AIC, therefore, we pick that one

# Q10. Step 8. Iterate #7 to arrive at the final model. Show your work. What is your final set of fixed effects?
mod4 <- lmer(LogSpores ~ fInfection01 + (1|fhive), data=Bees,REML=F)
mod5 <- lmer(LogSpores ~ sBeesN + (1|fhive), data=Bees,REML=F)
anova(mod2,mod3,mod4,mod5)
#The best model uses both fixed effects but no interaction

# Q11. Step 9. Fit the final model with REML. Check assumptions by plotting a histogram of residuals, plotting Pearson standardized residuals vs. fitted values, and plotting Pearson standardized residuals vs. explanatory variables. Are there issues with the model? If so, how might you address them?
mod_best <- lmer(LogSpores ~ fInfection01 + sBeesN + (1|fhive), data=Bees,REML=T)
par(mfrow=c(1,1))
hist(residuals(mod_best,type="pearson"),xlab="residuals",ylab="frequency",main="Residuals, random intercept model, no interactions")
dev.copy(pdf,'6_FinalModResidHist.pdf')
dev.off()

par(mfrow=c(2,2))
plot(fitted.values(mod_best),residuals(mod_best,type="pearson"),xlab="Fitted values",ylab="Standardized residuals")
plot(Bees$fInfection01,residuals(mod_best,type="pearson"),xlab="Infection",ylab="Standardized residuals")
plot(Bees$sBeesN,residuals(mod_best,type="pearson"),xlab="Standard N. Bees",ylab="Standardized residuals")
plot(Bees$fhive,residuals(mod_best,type="pearson"),xlab="Hive",ylab="Standardized residuals")
mtext("Residuals by variable, final model",outer=T,line=-2,cex=1.5)
dev.copy(pdf,'6_FinalModResiduals.pdf')
dev.off()

# Q12. Step 10. Interpret the model. The summary() command is useful here. What have you learned about American Foulbrood? 
summary(mod_best)
#The best predictor of American Foulbrood spores is whether or not a hive is infected. There is a slight negative relationship between the number 
#of bees in the hive and the number of spores measured. 

# Q13. Calculate the correlation between observations from the same hive as variance(fhive random effect)/(variance(fhive random effect) + variance(residual)). Given the correlation among observations from the same hive, do you think it's a good use of time to sample each hive multiple times? Why or why not?
4.8222/(4.8222+0.6033)
#Observations within the same hive are highly correlated (~0.89). This suggests that hives should not be sampled multiple times. 
#However, if sampling additional hives is impossible, then maybe resampling the same hive and including hive as a random effect 
#would still be better than simply having a smaller sample size.