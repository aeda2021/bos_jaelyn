## Load some packages that will be useful
library(mgcv) # load the MGCV package with Simon Wood's gam() functions
library(readr) # Hadley Wickham's package for reading in data easily
library(ggplot2) # for plotting with ggplot
library(ggfortify) # for autoplot

##################################################################################################
## We'll first examine the bias-variance tradeoff in a simplified form with a small exercise.
## If you remember, this tradeoff is one of the concepts behind finding the optimal amount of smoothing.
## We'll fit linear regressions to parts of the data, increasing the number of parts from 1 to 10.
##################################################################################################

# First, read in the bioluminescence data.
# Sources is the number of bioluminescent plankton seen in each water sample
# SampleDepth is the depth of the water sample, in m
# Station is the ID of the location at which multiple water samples were taken
ISIT <- read_tsv(url('https://github.com/aeda2021/2021_master/raw/main/raw-data/ISIT.txt')) # tsv for tab-separated values

# Subset it to one station. This is what we'll work with for now.
ISITsub <- subset(ISIT, Station == 8)

# Plot the data
ggplot(ISITsub, aes(x=SampleDepth, y=Sources)) +
	geom_point()

# Now you can fit a linear regression to the ISITsub dataset, with Sources as the response and SampleDepth as the explanatory variable
# Please call the output "mod1", as I've started to fill in for you
mod1 <- lm(ISITsub$Sources ~ ISITsub$SampleDepth)

# Q1: What was your code for fitting a linear regression of Sources vs. SampleDepth?
# A: mod1 <- lm(ISITsub$Sources ~ ISITsub$SampleDepth)

# We'll predict the mean response and confidence intervals from this model and save the output
ISITsub$mod1 <- predict(mod1)
ISITsub$mod1lwr <- predict(mod1, interval='confidence', level=0.95)[,'lwr']
ISITsub$mod1upr <- predict(mod1, interval='confidence', level=0.95)[,'upr']

# Now plot the linear model fit to the full dataset on top of the data
ggplot(ISITsub, aes(x=SampleDepth, y=Sources)) +
	geom_point() +
	geom_line(aes(y=mod1)) + 
	geom_ribbon(aes(ymin=mod1lwr, ymax=mod1upr), alpha=0.3)

# Q2: Use your skills from earlier in this course and evaluate this linear model (autoplot may be useful here). How does this fit look to you? Does a linear regression look reasonable? Any well-justified answer is appropriate. 
# A2: No, this does not look like a valid linear regression. There is a clear pattern in the residulas (Residuals vs Fitted graph), and some of the larger residuals have disproportionately hig leverage (residuals vs leverage graph)

# For comparison, let's fit linear models to two halves of the data. This involves a fair bit of coding in R, so please ask for help understanding the code where things are confusing. 
# First, we have to find the halves of the data
quants2 <- quantile(ISITsub$SampleDepth, probs=seq(0, 1, by=0.5)) # define the ends and the half-way point in the data with the quantile function
ISITsub$split2 <- cut(ISITsub$SampleDepth, breaks=quants2, include.lowest=TRUE, labels=1:2) 

# Q3: In your own words, what does the cut() function do? How are we using it here?
# A3: the cut() function dividies a data range into parts (two parts here) and defines each value according to the part of the range in which it falls
# Here we use it to create a new column assigning rows in a dataframe to either side of the split

# Next, let's fit a linear model to each subset (half) of the data. We do this in a loop to make it easier.
mod2s <- vector('list', length=2) # A list to hold each of our models in a single, convenient object
ISITsub$mod2 <- ISITsub$mod2lwr <- ISITsub$mod2upr <- NA # Initialize the variables we'll use and fill with NA
for(i in 1:length(mod2s)){ # Loop through each model
	mod2s[[i]] <- lm(Sources ~ SampleDepth, data=ISITsub[ISITsub$split2==i,]) # Fit a linear regression to the appropriate subset of the data. Now see why we made the split2 vector?
	ISITsub$mod2[ISITsub$split2==i] <- predict(mod2s[[i]]) # Now predict from that model. Notice we're only predicting for part of the data.
	ISITsub$mod2lwr[ISITsub$split2==i] <- predict(mod2s[[i]], interval='confidence', level=0.95)[,'lwr'] # Same for the confidence intervals
	ISITsub$mod2upr[ISITsub$split2==i] <- predict(mod2s[[i]], interval='confidence', level=0.95)[,'upr']
}

# Plot the data, the 1-model fit, and the 2-model fit
# There will be an odd 3rd line segment connecting the end of the first model fit to the beginning of the second model fit. Just pretend it's not there.
ggplot(ISITsub, aes(x=SampleDepth, y=Sources)) +
	geom_point() +
	geom_line(aes(y=mod1)) +
	geom_ribbon(aes(ymin=mod1lwr, ymax=mod1upr), alpha=0.3) +
	geom_line(aes(y=mod2, color='red')) +
	geom_ribbon(aes(ymin=mod2lwr, ymax=mod2upr), alpha=0.3, fill='red')


# Q4: Has the fit between the model and the data improved? What's the downside of continuing the split the data into finer and finer chunks and fitting more models?
# A4: The fit looks much better. However, by fitting two models instead of one we halved the sample size, and also are no longer able to say anything about the relationship 
# between the predictor and the response across the whole data set. 

# Now, it's your turn. Please fit 10 linear regression models. I'd recommend using the 2-model code above as a guide and tweaking it.
quants10 <- quantile(ISITsub$SampleDepth, probs=seq(0, 1, by=0.1))
ISITsub$split10 <- cut(ISITsub$SampleDepth, breaks=quants10, include.lowest=TRUE, labels = 1:10) 
mod10s <- vector('list', length=10) # A list to hold each of our models in a single, convenient object
ISITsub$mod10 <- ISITsub$mod10lwr <- ISITsub$mod10upr <- NA # Initialize the variables we'll use and fill with NA
for(i in 1:length(mod10s)){ # Loop through each model
  mod10s[[i]] <- lm(Sources ~ SampleDepth, data=ISITsub[ISITsub$split10==i,]) # Fit a linear regression to the appropriate subset of the data. Now see why we made the split2 vector?
  ISITsub$mod10[ISITsub$split10==i] <- predict(mod10s[[i]]) # Now predict from that model. Notice we're only predicting for part of the data.
  ISITsub$mod10lwr[ISITsub$split10==i] <- predict(mod10s[[i]], interval='confidence', level=0.95)[,'lwr'] # Same for the confidence intervals
  ISITsub$mod10upr[ISITsub$split10==i] <- predict(mod10s[[i]], interval='confidence', level=0.95)[,'upr']
}

# Now plot the 10 models on top of the data and on top of the 1- and 2-model fits. The plot with the data, 1-model, and 2-model fit is probably a useful guide here.

ggplot(ISITsub, aes(x=SampleDepth, y=Sources)) +
  geom_point() +
  geom_line(aes(y=mod1)) +
  geom_ribbon(aes(ymin=mod1lwr, ymax=mod1upr), alpha=0.3) +
  geom_line(aes(y=mod2, color='red')) +
  geom_ribbon(aes(ymin=mod2lwr, ymax=mod2upr), alpha=0.3, fill='red') +
  geom_line(aes(y=mod10, color='blue')) +
  geom_ribbon(aes(ymin=mod10lwr, ymax=mod10upr), alpha=0.3, fill='blue') + 
  theme(legend.position = "none")

# Q5: Compare your 10-model fit to the 1-model fit. Which model's predictions are furthest from the observed data? Which model has the widest confidence bounds? How does this (or does this not) illustrate the bias-variance tradeoff?
# A5: The 10 model fit has the widest confidence bounds, especially for the intermediate depths. Since the number of observations is so low, the confidence bounds widen. However, the observed data are all within the confidence bounds. 
# By decreasing the bias (adding more models), we have increased the variance so far as to make the model (probably) useless. 


####################################################
## Now you'll fit some GAMs and evaluate your models
####################################################
#install.packages("maps") # if needed
library(maps) # has map data in it

# load the data
spdata <- readRDS(url('https://github.com/aeda2021/2021_master/raw/main/raw-data/gadusmorhua.rds')) # load the spdata data.frame with abundance and environmental data from cod surveys
spdata$presfit01 <- as.numeric(spdata$presfit) # make a vector that's nice for plotting
spdata <- spdata[order(spdata$presfit01),] # order the data from absent to present for ease of plotting

# examine the data
head(spdata) # look at the dataset
summary(spdata)

# make a map of the data
world <- map_data('world')

ggplot() + 
	geom_polygon(data=world, aes(x=long, y=lat, group=group), color='black', fill=NA) + # the map
	xlim(-100, -45) + 
	ylim(23, 62) +
	geom_point(data=spdata, aes(x=lon, y=lat, color=presfit, alpha=0.1), size=0.01) # the data points. note that because the dataframe is sorted from absent to present, the presences are plotted on top of the absences


# Q6: Based on the map, do you think cod prefer warmer or cooler waters? Why do you think that?
# A6: Based on the map, cod seem to prefer cooler waters. They are mostly present in the Gulf of Maine and Canada, and absent further south

# Q7: Plot cod presence/absence (the presfit vector) vs. the bottom temperature (SBT.actual). At what range of temperatures have cod been observed?
ggplot() +
  geom_boxplot(data=spdata,aes(x=presfit,y=SBT.actual))
range(spdata$SBT.actual[spdata$presfit==TRUE])
#A7: Cod have been observed from -1.8 to 18.3 degrees (presumably Celsius?)

# Q8: Fit a GAM for presfit against SBT.actual. Check the model. Does it look like the assumptions have been met? Why or why not?
library(mgcv)
gam1 <- gam(spdata$presfit ~ s(spdata$SBT.actual))
#Some assumptions do not appear to be met. The residual histogram is more bimodal than normal, the residuals vs. linear predictor plot is clearly structured, and the 
#response vs. fitted values plot reflects the fact that the response variable is binomial. 

# Q9: Fit a new GAM with a more appropriate error structure and save it as "mod2". The gam() function takes the same family= argument as does glm(). Which error structure and link function did you choose?
mod2 <-gam(presfit ~s(SBT.actual),data=spdata,family='binomial')
#We chose the binomial family and the logit() link

# Q10: Check your mod2 GAM. Does it look like the assumptions have been met? Make sure to read the text output as well as look at the graphs. If something doesn't look right, what would you do to fix it?
gam.check(mod2)
# A10: It's definitely better, but still not perfect.  The Q/Q plot looks okay, the residuals vs. linear
#predictors plot is problematic, the residuals are still slightly bimodal.

# Q11: Interpret your mod2 GAM (summary function). How much deviance is explained? Is the SBT.actual term significant at alpha=0.05? How wiggly do you expect the smooth fit to be?
# A11: The model explains about 26.5% of deviance, and the SBT.actual term is significant at the alpha = 0.05 level. With 8.96 estimated degrees of freedom, I would expect the fit to be fairly
# Now let's make predictions from our last model (mod2) and plot them.
nd <- data.frame(SBT.actual = seq(-5,30, by=0.5)) # make a data.frame of new explanatory variables
nd$mod2 <- predict(mod2, newdata=nd, type='response') # predict on the scale of the response
nd$mod2se <- predict(mod2, newdata=nd, type='response', se.fit=TRUE)$se.fit # get the standard errors of the fit

ggplot(spdata, aes(x=SBT.actual, y=presfit01)) +
	geom_point() +
	geom_line(data=nd, aes(x=SBT.actual, y=mod2, color='red')) +
	geom_ribbon(data=nd, aes(x=SBT.actual, ymin=mod2-mod2se, ymax=mod2+mod2se), alpha=0.3, fill='red', inherit.aes=FALSE)

# Q12: Does the fit look realistic? Why or why not? Does it look overfit? Why or why not?
# A12: To me, the model looks overfit. 

# Q13: This dataset is actually a concatenation of seven different surveys which each have slightly different abilities to catch cod. Fit a new GAM that includes a categorical predictor for survey (the region vector). Based on AIC, which model would you choose? How confident would you be?
gam3 <- gam(presfit ~s(SBT.actual) + as.factor(region),data=spdata,family='binomial')
AIC(mod2)
AIC(gam3)
# A13: By AIC, the model including region is definitely better. The adjusted R^2 is also higher (0.33 vs 0.26). With a deltaAIC of more than 800, I would be confident in choosing the model including region over the model excluding region. 
