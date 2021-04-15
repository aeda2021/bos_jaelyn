#######################################
##AEDA Machine Learning assignment  ##
##Based on scripts from Lucas 2020 ##
####################################

## If you don't have these loaded yet, here are the libraries you will need
## Load general data manipulation / plotting libraries
library(dplyr)
library(ggplot2)

# Load modeling libraries
library(caret)
library(ranger)
library(pdp)
library(traitdata)
library(kernlab)

## Load helper scripts from Lucas 2020 - these are on the Github site as ML_helpers.R
## You can download and then source from this file (change file path to the location on your computer)
source('[your.directory]/ML_helpers.R')

set.seed(100)
data(pantheria)
p<-pantheria
p <- p %>% 
  filter(!is.na(LitterSize)) %>% 
  filter(LitterSize >= 1) %>% 
  mutate(y = log1p(LitterSize)) %>% 
  dplyr::select(-LitterSize, -References, -TeatNumber)

#caret_Setup

folds <- createFolds(p$y, k = 5, returnTrain = TRUE)
trcntrl <- trainControl(index = folds, savePredictions = TRUE, search = 'random')

## Now let's use similar methods to those we used in the exercise to evaluate covariates of litter/clutch size in reptiles!
## Load a larger dataset for amniotes

data(amniota)

amniote_data<-amniota

names(amniote_data)
dim(amniote_data)

sum(!is.na(amniote_data$litter_or_clutch_size_n))

#The trait names should be pretty self-explanatory. "svl" = snout-vent length 

#Q1: Write some code to clean the data.
#Rename the variable of interest to "y", log transform variable of interest and remove any taxa with missing litter/clutch size data.
#Then, retain only taxa in the class Reptilia and remove any variables with no data (all NA).

reptiles <- amniote_data[amniote_data$Class=="Reptilia",]
reptiles <- reptiles[!is.na(reptiles[,9]),]

keep<-vector(length=ncol(reptiles))
for (i in (1:ncol(reptiles))){
  x <- is.na(reptiles[,i])
  keep[i]<-(FALSE %in% x)
  print(i)
}
reptiles <- reptiles[,keep]

reptiles$y <- log(reptiles$litter_or_clutch_size_n)
colnames(reptiles)
reptiles <- reptiles[,-8]

##Q2: Plot the distribution of log-transformed litter/clutch size in reptiles.
##Histogram or boxplot (or both if you want) are fine.
##Visualizing by order may be useful.

par(mfrow=c(1,1))
hist(reptiles$y)
ggplot(reptiles, aes(x = Order, y = y)) + geom_boxplot()

##Q3: Write a little more data-cleaning code!
##Impute missing data and remove taxonomic data, common name, and scientific name.
reptiles<-reptiles[,-c(1,2,3,4,5,6,30)]
preprocesses <- preProcess(reptiles, method = 'medianImpute')
rep_impute <- predict(preprocesses, reptiles)

##Q4: Visualize the distributions for the predictor variables.
##Identify which variables look like they have a highly non-normal distribution.
##Log-transform these variables and visualize again.
##Which of the four models we will fit need the input variables to be log-transformed?
for(i in 0:11){
  for( j in 1:4){
    
    if(j + 4 * i <= ncol(rep_impute)){
      hist(rep_impute[, j + 4 * i], breaks = 100, ylim = c(0, 80), main = j + 4 * i,ylab = colnames(rep_impute)[j + 4 * i])
    }
    
  }
  print(i)
  par(mfrow = c(2, 2))
}

logcols <- c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23)
rep_impute[,logcols]<-log1p(rep_impute[,logcols])
#We need to transform input variables for the parametric models: in this case, the simple linear model and the elastic net model.

##Q5: Fit a linear model relating your response variable to some potential predictors.
##To make this similar to our model for mammals, use adult body mass, age to maturity for females, incubation length, litters/clutches per year, and maximum longevity.
##Visualize model fit and get R2.
folds <- createFolds(p$y, k = 5, returnTrain = TRUE)
trcntrl <- trainControl(index = folds, savePredictions = TRUE, search = 'random')

apriori_reptiles <- y ~ adult_body_mass_g + female_maturity_d + incubation_d + litters_or_clutches_per_y + maximum_longevity_y
reptile_lm <- train(apriori_reptiles, data = rep_impute, method = 'lm', trControl = trcntrl, na.action = na.omit)
plotCV(reptile_lm)
summary(reptile_lm$finalModel)

##How does this model compare to the mammal model?
## With a final adjusted R-squared of 0.3319, the model fit is very similar to the mammal model. 

##Q6: Fit an elastic net to the data. Use the same hyperparameters used for the mammal dataset.
##Visualize model fit and get maximum R2.
##Plot R2 vs lasso/ridge fraction and strength of regularization (lambda).
##Does using the elastic net improve prediction relative to the linear model for this dataset?
enet_gr <- expand.grid(lambda = 10 ^ seq(0, -4, length.out = 20), fraction = c(seq(0.01, 1, length.out = 25)))
reptile_enet <- train(y ~ ., data = rep_impute, method = 'enet', tuneGrid = enet_gr, trControl = trcntrl, na.action = na.omit)

plotCV(reptile_enet)
reptile_enet$results$Rsquared %>% max

reptile_enet$results %>%
  ggplot(aes(fraction, Rsquared, colour = lambda, group = factor(lambda))) +
  geom_line() +
  geom_point() + scale_color_viridis_c(trans = 'log10') + xlab('Lasso/Ridge fraction')
#No, the elastic net does not improve prediction

##Q7: Fit a Gaussian process model to the data. Use the same range of sigma values used for the mammal dataset. 
##Visualize model fit and get R2.
##Plot R2 vs sigma. How does this plot compare to the plot from the mammal dataset?
##Overall, does the Gaussian process model perform better than the linear model?

gp_gr <- data.frame(sigma = c(0.01, 0.02, 0.04, 0.08, 0.16))
reptile_gp <- train(y ~ ., data = rep_impute, method = 'gaussprRadial', tuneGrid = gp_gr, trControl = trcntrl, na.action = na.omit)

reptile_gp$results %>% ggplot(aes(sigma, Rsquared)) +
  geom_line() + geom_point() + xlab('Sigma')
plotCV(reptile_gp)

reptile_gp$results$Rsquared %>% max
#No, the Gaussian process model performs worse than the linear model. 

##Q7: Train a random forest on the data. Note - use a lower maximum number of random predictors by setting mtry = c(2, 5, 10, 20).
rf_gr <- expand.grid(mtry = c(2, 5, 10, 20), splitrule = 'variance', min.node.size = c(5, 10, 20, 50))
reptile_rf <- train(y ~ ., data = rep_impute, method = 'ranger', tuneGrid = rf_gr, trControl = trcntrl, na.action = na.omit, importance = 'impurity', num.trees = 1000)

##Visualize model fit and get R2.
plotCV(reptile_rf)
reptile_rf$results$Rsquared %>% max

##Plot R2 vs node size and number of random predictors.
reptile_rf$results %>%
  ggplot(aes(mtry, Rsquared, colour = factor(min.node.size), group = factor(min.node.size))) +
  geom_line() +
  geom_point() +
  labs(colour = 'min.node.size')

##What does the node size selected indicate about the amount of noise in the model?
## Node size is the minimum size of terminal nodes. Here, the smallest node sizes perform best, indicating that
#the modelexhibits a relatively high signal to noise ratio. 

##What does the number of random predictors selected indicate about interaction depth?
#The model performs better with more random predictors. This indicates that the independent variables have 
#relatively high numbers of intereractions.

##Q8: Overall, which model(s) perform best at predicting litter/clutch size, and which perform the worst?
## The random forests model performs the best, and the elastic net and gaussian process models perform the worst

##Compare this to the mammal analysis. What does this say about the universality of these methods?
#In the mammal analysis, the pure linear model was the worst. The differing performance of models across even
#similar datasets shows that choosing the most useful model depends on the specifics of the problem and its
#associated data set. 

##Q9: Evaluate variable importance for the elastic net, gaussian process, and random forest.
##Which variable is most important across models? 
varImp(reptile_enet)
varImp(reptile_gp)
varImp(reptile_rf)
#In every model, adult body size is the most important variable. 

##Q10: Plot functional forms for the relationship between litter/clutch size and the most important variable for the Gaussian Process and the random forest models.
##How do they differ?
partial(reptile_enet,pred.var = c('adult_body_mass_g'), plot = TRUE)
partial(reptile_gp, pred.var = c('adult_body_mass_g'), plot = TRUE)
partial(reptile_rf, pred.var = c('adult_body_mass_g'), plot = TRUE)
# In all models, clutch size increases with body size. The elastic net and Gaussian process models show log linear (elastic net)
#and nearly log linear (gp) relationships between clutch size and body size, but the relationship show in the random forests model 
#is somewhat more complex. 

##What does this say about the likely relationship between litter/clutch size and the best predictor variable?
#Clutch size increases consistently with body size. There may be an exception at the lowest body sizes, but 
#random forets can perform poorly at extreme variable values, so we should interpret this with caution.