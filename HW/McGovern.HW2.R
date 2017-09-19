# List of packages required for this analysis
pkg <- c("devtools", "knitr", "tidyr", "readr", "dplyr", "skimr", "ggplot2", "corrplot", "scales", "GGally", "randomForest")

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
library(randomForest)

rm(list=ls())

# Banknote Authentication

# 

# load data
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00267/data_banknote_authentication.txt"
banknote.df <- read_delim(url,",", col_names = c("moment2","moment3","moment4","entropy","class"))

# name the data set attributes
attributes <- data.frame(
  "col_name" = c(
    "moment2",
    "moment3",
    "moment4",
    "entropy",
    "class"), 
  "attribute_name" = c(
    "variance of Wavelet Transformed image (continuous)",
    "skewness of Wavelet Transformed image (continuous)",
    "curtosis of Wavelet Transformed image (continuous)", 
    "entropy of image (continuous)",
    "class (integer)"))

kable(attributes)

# convert 'class' variable from an integer to a factor
# I am running this analysis from the perspective of determining which banknotes are counterfeit:
# '0' equals 'no' (not counterfeit), '1' equals yes (counterfeit)
banknote.df$class <- factor(banknote.df$class, levels=c(0,1),
                            labels=c("no", "yes"))
summary(banknote.df$class)

# determine number of variables
# - which one are the predictor variables and which is the response variable
# determine number of observations
dim(banknote.df)[1]
dim(banknote.df)[2]

# summarize data
banknote.skim <- data.frame(skim(banknote.df))
banknote.skim$value <- as.integer(banknote.skim$value)

banknote.skim.tranform <- banknote.skim %>% 
  filter(stat %in% c("missing","complete","n","n_unique","min","max","median","sd")) %>%
  spread("stat","value") %>% 
  select(-c(level))

View(banknote.skim.tranform)

set.seed(123)
train <- sample(nrow(banknote.df %>% select(-c(class))), 0.7*nrow(banknote.df %>% select(-c(class))))
df.train <- banknote.df[train,]
model.forest <- randomForest(class ~ ., 
                             data = df.train,
                             importance = TRUE)

model.forest

View(importance(model.forest))


# generate pair-wise XY-scatterplots of each pair of continuous variables including the outcome using color and/or shape (use pairs function)
oldPar <- par(mfrow=c(2:2),ps=16)
for (i in 3:ncol(banknote.df %>% select(-c(class)) )) { 
  var1 <- paste0("moment", i-1)
  var2 <- paste0("moment", i)
  for ( iFake in sort(unique(banknote.df$class)) ) {
    plot(banknote.df[,c(var1,var2)],type="n",
         main=paste("Counterfeit:",iFake))
    iTmp <- (1:length(levels(banknote.df$class)))[levels(banknote.df$class)==iFake]
    points(banknote.df[banknote.df$class==iFake,c(var1,var2)],col=iTmp,pch=iTmp)
  }
}

ggpairs(banknote.df, aes(color = class, alpha = 0.9)) + 
  labs(
    title = "Pair-wise Combinations of Continuous Variables in Banknote Data Set"
  ) +
  theme_linedraw()

rm(list=ls())

# Abalone

# load data
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"
abalone.df <- read_delim(url, ",", col_names = c("sex","length","diameter","height","weight.whole","weight.shucked","weight.viscera","weight.shell","rings"))

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
abalone.df$sex <- factor(abalone.df$sex)
summary(abalone.df$sex)

# determine number of variables
# - which one are the predictor variables and which is the response variable
# determine number of observations
dim(abalone.df)[1]
dim(abalone.df)[2]

# summarize data
abalone.skim <- data.frame(skim(abalone.df))
abalone.skim$value <- as.integer(abalone.skim$value)

abalone.skim.tranform <- abalone.skim %>% 
  filter(stat %in% c("missing","complete","n","n_unique","min","max","median","sd")) %>%
  spread("stat","value") %>% 
  select(-c(level))

View(abalone.skim.tranform)

ggpairs(abalone.df, aes(color = sex, alpha = 0.9)) + 
  labs(
    title = "Pair-wise Combinations of Continuous Variables in Abalone Data Set"
  ) +
  theme_linedraw()

cut <- 0.2

# length by rings
ggplot(abalone.df, aes(x=length, y=rings)) + 
  geom_boxplot(fill="steelblue", aes(group = cut_width(length, cut))) + 
  labs(
    title = "Box Plot of Abalone Length by Rings",
    subtitle = c("Continuous variable Rings is cut into several sections")
    ) + 
  theme_linedraw()

cut <- 0.2

# diamter by rings
ggplot(abalone.df, aes(x=diameter, y=rings)) + 
  geom_boxplot(fill="steelblue", aes(group = cut_width(diameter, cut))) + 
  labs(
    title = "Box Plot of Abalone Diameter by Rings",
    subtitle = c("Continuous variable Diameter is cut into several sections")
  ) + 
  theme_linedraw()


set.seed(123)
train <- sample(nrow(abalone.df %>% select(-c(rings))), 0.7*nrow(abalone.df %>% select(-c(rings))))
df.train <- abalone.df[train,]
model.forest <- randomForest(rings ~ ., 
                             data = df.train,
                             importance = TRUE)

model.forest

View(importance(model.forest))

rm(list=ls())


