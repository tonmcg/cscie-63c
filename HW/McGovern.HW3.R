# List of packages required for this analysis
pkg <- c("devtools", "knitr", "tidyr", "readr", "dplyr", "skimr", "ggplot2", "corrplot", "scales", "GGally", "modelr")

# Check if packages are not installed
# Assign not installed packages to new.pkg
new.pkg <- pkg[!(pkg %in% installed.packages())]

# Install any packages not installed
if (length(new.pkg)) {
  install.packages(new.pkg, repos = "http://cran.rstudio.com")
}

# load packages
library(devtools)
library(knitr)
library(tidyr)
library(readr)
library(dplyr)
library(skimr)
library(ggplot2)
library(corrplot)
library(scales)
library(GGally)
library(modelr)

rm(list=ls())

###### QUESTION 1 ##########
# Abalone

# load data
######################### CHANGE THIS TO URL SOURCE ###############################
#url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"
url <- "/Users/Luxive/OneDrive/Software_Engineering/Harvard/CSCI E-63c/HW/abalone.data"
###################################################################################

abalone.raw <- read_delim(url, ",", col_names = c("sex","length","diameter","height","weight.whole","weight.shucked","weight.viscera","weight.shell","rings"))

# name the data set attributes
attributes <- data.frame(
  "col_name" = c(
    "sex",
    "length",
    "diameter",
    "height",
    "weight.whole",
    "weight.shucked",
    "weight.viscera",
    "weight.shell",
    "rings"), 
  "attribute_name" = c(
    "Sex",
    "Length",
    "Diameter", 
    "Height",
    "Whole Weight",
    "Shucked Weight",
    "Viscera Weight",
    "Shell Weight",
    "Rings"
  ))

kable(attributes)

# convert 'sex' variable from a character to a factor
abalone.raw$sex <- factor(abalone.raw$sex)

# convert 'rings' variable from an int to a numeric
abalone.raw$rings <- as.numeric(abalone.raw$rings)

# create 'age' variable which represents the snail age equal to rings + 1.5 per data set description
# drop 'rings' variable
abalone.age <- abalone.raw %>% mutate( age = rings + 1.5 ) %>% select( -c(rings) )

#Calculate correlations between all continuous attributes in this dataset. 
#Given potential non-linear relationship between some of the attributes and snail age, 
#it might be prudent to use both Pearson and Spearman correlations to determine which variable is most robustly correlated with age.

# drop categorical 'sex' variable
abalone.cont <- abalone.age %>% select(-c(sex))

# create Pearson correlation matrix
correlations.pearson <- cor(abalone.cont, method = c("pearson"))
View(correlations.pearson)
corrplot(correlations.pearson, order = "hclust")

# create Spearman correlation matrix
correlations.spearman <- cor(abalone.cont, method = c("spearman"))
View(correlations.spearman)
corrplot(correlations.spearman, order = "hclust")

######## FIX THIS ##########
#ggcorr(data = abalone.cont, method = c("spearman","pearson"))
############################

ggpairs(abalone.age, aes( color = sex, alpha = 0.9 )) + 
  labs(
    title = "Pair-wise Combinations of Continuous Variables in Abalone Data Set"
  ) +
  theme_linedraw()

#Fit linear model of age as outcome and shell weight as predictor using R function lm, 
model.linear <- lm(age ~ weight.shell, data = abalone.age)

#display the result using summary function, use its output to answer the following questions:
summary(model.linear)

#model_matrix(abalone.age, age ~ weight.shell)

##Does this predictor explain significant amount of variability in response? I.e. is there significant association between them?

##What is the RSE and R2R2 of this model? 
summary(model.linear)$sigma
summary(model.linear)$r.squared

##What are the model coefficients and what would be their interpretation? 
##What is the meaning of the intercept of the model, for example? 
##How sensible is it?
model.linear$coefficients[1]
model.linear$coefficients[2]

#Create scatterplot of age and shell weight and add regression line from the model to the plot
ggplot( data = abalone.age, aes( x = weight.shell, y = age )) + 
  geom_point( aes(color = sex )) + 
  geom_smooth( method="lm", formula = y ~ x ) +
  labs(
    title = "Scatter Plot of Abalone Shell Weight by Age, Grouped by Sex",
    subtitle = "Regressing Shell Weight on Age",
    x = "Shell Weight (in grams)",
    y = "Age (in years)",
    color = "Snail Sex"
  ) +
  theme_linedraw()


old.par <- par(mfrow=c(2,2))
#Create diagnostic plots of the model and comment on any irregularities that they present. For instance, does plot of residuals vs. fitted values suggest presence of non-linearity that remained unexplained by the model? How does it compare to the plot of the predictor and outcome with regression line added to it that was generated above?
plot(model.linear)

#Use function confint to obtain confidence intervals on model parameters
confint(model.linear)

#Use this model and predict function to make predictions for shell weight values of 0.1, 0.2 and 0.3. 
#Use confidence and prediction settings for parameter interval in the call to predict to obtain confidence and prediction intervals on these model predictions. 
predict(model.linear, data.frame(weight.shell = c(0.1, 0.2, 0.3)), interval = 'confidence')
predict(model.linear, data.frame(weight.shell = c(0.1, 0.2, 0.3)), interval = 'prediction')

#Explain the differences between interpretation of:
###confidence intervals on model parameters and model predictions
###confidence and prediction intervals on model predictions
###Comment on whether confidence or predicion intervals (on predictions) are wider and why

###### QUESTION 2 ##########

#Use lm() to fit a regression model of log-transformed age as linear function of log-transformed shell weight and use summary to evaluate its results. 
# create new tibble with log-transform rings and shell weight
abalone.log.shell <- abalone.age %>%
  mutate(lage = log2(age), lshell = log2(weight.shell))

model.shell.log <- lm(lage ~ lshell, data = abalone.log.shell)

summary(model.shell.log)
  
#Can we compare fits obtained from using untransformed (above) and log-transformed attributes? 
#Can we directly compare RSE from these two models? 
#What about comparing R2R2? What would we conclude from this? (Please consult ISLR Ch.3.1.3 if unsure) 
#What would be the physical meaning of model coefficients this time? 
#What does model intercept represent in this case, for example? 
#How sensible is this and how does it compare to that from the fit on untransformed data?


#Create a XY-scatterplot of log-transformed predictor and response and add corresponding regression line to it. Compared it to the same plot but in untransformed coordinates obtained above. What would you conclude from such comparison?
ggplot( data = abalone.log.shell, aes( x = lshell, y = lage )) + 
  geom_point( aes(color = sex )) + 
  geom_smooth( method="lm", formula = y ~ x ) +
  labs(
    title = "Scatter Plot of Abalone Shell Weight by Age, Grouped by Sex",
    subtitle = "Regressing Log-Transform of Shell Weight on Age",
    x = "log(Shell Weight)",
    y = "log(Age)",
    color = "Snail Sex"
  ) +
  theme_linedraw()


#Make diagnostic plots for model fit on log-transformed age and shell weight.
plot(model.shell.log)

#Compare their appearance to that for the model using original scale of measurements.

#What would you conclude from this comparison about their relative quality?

###### QUESTION 3 ##########
#To explore effects of adding another variable to the model, continue using log-transformed attributes and fit a model of log-transformed age as a function of shell weight and shucked weight (both log-transformed also). 
abalone.log.shucked <- abalone.log.shell %>%
  mutate(lshucked = log2(weight.shucked))

model.shucked.log <- lm(lage ~ lshell + lshucked, data = abalone.log.shucked)

#Just an additive model – no interaction term is necessary at this point. 
#Please obtain and evaluate the summary of this model fit, confidence intervals on its parameters and its diagnostic plots. 
summary(model.shucked.log)

#Where applicable, compare them to the model obtained above and reflect on pros and cons of including shucked weight as another variable into the model.
plot(model.shucked.log)
