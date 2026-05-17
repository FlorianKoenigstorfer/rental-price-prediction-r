# =============================================================================
# 02 — Data cleaning and management
# Accompanies: slides/02_data_cleaning_and_management.pdf
# Required packages: tidyverse, stargazer, car, caTools, caret
# =============================================================================

### Clearing environment and console ---------------------------------------

rm(list = ls())
cat("\014")

### Libraries ---------------------------------------------------------------

library(tidyverse)
library(stargazer)
library(car)
library(caTools)
library(caret)

### Load Data ---------------------------------------------------------------


# For you, the starting point of the analysis will be the training dataset for the analysis.
# I will show you how to do it using the observations from the city Mittelsachsen_Kreis.


# this is where we set the working directory. You will have to set it to the folder in which you store the dataset.
setwd("./")

# This is where we load the dataset. You will need to set the name to the name of the file you need to load.
df <- read.csv("data/Dataset_Teachers.csv")
df$X <- NULL
df$X.1 <- NULL
df$X.2 <- NULL


# Here you filter the observations relating to your city.
df.my.city <- df %>%
  filter(regio2=="Mittelsachsen_Kreis")



### Messy Data #1: Redundant Data -------------------------------------------


# Dealing with redundant data means 2 things:
# (1) Removing variables that for some reason we think are irrelevant for the task at hand
# (2) Removing observations that are either duplicates of other observations or unusable for analysis 
#      (i.e. because of too many missing values)


# Key issue for this step is the definition of "relevance". Since we usually don't know how variables
# impact each other at this point of the project, we will need to use some domain knowledge or common sense.
# Also, one should note that it is always possible to return to this step of the project and add or remove
# variables and observations by simply re-running the code. Fixing mistakes is rather simple. :)


# Inspecting the original (i.e. messy) dataset
head(df.my.city)
summary(df.my.city)


# Keeping only relevant variables
df.non.redundant.variables <- df.my.city %>%
  select(regio3,
         balcony,
         totalRent,
         serviceCharge,
         baseRent,
         yearConstructed,
         firingTypes,
         livingSpace,
         noRooms,
         cellar,
         hasKitchen,
         heatingType)


# Removing duplicates
df.non.redundant <- unique(df.non.redundant.variables)



# Inspecting all non-redundant observations
head(df.non.redundant)
summary(df.non.redundant)


### Since it came up as a question, I have also added the code for filtering out the observations that were duplicated.
# Finding duplicated elements in data frame "df.non.redundant"
df.duplicated.observations <- df.non.redundant.variables[duplicated(df.non.redundant.variables),]
head(df.duplicated.observations)
summary(df.duplicated.observations)






### Messy Data #2: Missing Values -------------------------------------------

# Dealing with missing data works in 2 steps:
# (1) Detecting missing values
# (2.1) Removing missing values (Default option at this time)
# (2.2) Replacing/Estimating missing values

# Note: For now, we will only remove observations with missing values. There is the option to replace
# the values with averages or estimated values as well, but we will leave that out for now.

# The first thing to do is to check if all of the elements that are actually missing are marked as "NA".
# In the current case, the variable "firingType" actually has elements labelled "no_information".
# We treat them the same as "NA". To make further analysis easier, we can replace the observations of the
# variable firingType with NA. That way, all the methods we learned earlier still apply.
df.non.redundant$firingTypes[which(df.non.redundant$firingTypes=="no_information")] <- NA

### How do we detect missing values? How many missing values are there for each one of our variables?
# Detecting missing values
is.na(df.non.redundant)                # TRUE if value is missing; FALSE if value is not missing
colSums(!(is.na(df.non.redundant)))    # Yields number of existing values in each column
colSums((is.na(df.non.redundant)))     # Yields number of missing values in each column


# Now that we know how many missing values there are in each column, we have choose how to remove them.

# The easiest way to remove them is to use the "na.omit" command.
# I just checked whether the command "complete" works. On one computer it does the same as the command
# "na.omit", on another not. "na.omit" works as intended on both computers.
# Recommendation: Use "na.omit"
df.complete <- na.omit(df.non.redundant)

# Alternatively, it is possible to remove missing values from only a selected subset of variables.
df.complete.selective <- df.non.redundant %>%
  filter(!is.na(totalRent))


### Messy Data #3: Outliers -------------------------------------------------

## Dealing with outliers works in 2 steps:
# (1) Detecting outliers
#   (1.1) Option 1: Detection using standard deviations and z-scores
#   (1.2) Option 2: Detection using the inter-quartile range and related boundaries
#   (1.3) Option 3: Visual inspection of data (will be in file relating to LV 8 - Exploratory Data Analysis)
# (2) Dealing with outliers
#   (2.1) Removal of outliers (This is the one we want you to do.)
#   (2.2) Replacing outliers with truncated or estimated values (This one we did not implement.)

# We will show you how to implement the steps for the variable "totalRent".
# The method for the remaining numerical attributes is the same.



## Option 1: Outlier Detection using standard deviations and z-scores
# Normalize the variable using z-scores
scaledRent <- scale(df.complete$totalRent)
df.complete$scaledRent <- scaledRent[,1]
mean.rent <- mean(df.complete$scaledRent)
sd_rent <- sd(df.complete$scaledRent)
# The idea of detecting outliers using standard deviations relies on the assumption
# of the data being normally distributed. Since practically all data out there is
# NOT normally distributed, we need to make sure that we normalize the dataset.
# For a short recap of normalization, please look at the heading "standardization"
# In the following Wikipedia article: https://en.wikipedia.org/wiki/Feature_scaling

# The lower- and upper bounds are then calculated using the normalized data.
lower.bound.z.scores <- mean.rent-3*sd_rent
upper.bound.z.scores <- mean.rent+3*sd_rent

# Using conditional statements, you can then filter out the observations above and
# below the boundaries.
outliers.below.z.scores <- df.complete$scaledRent < lower.bound.z.scores
outliers.above.z.scores <- df.complete$scaledRent > upper.bound.z.scores

# You can then combine the conditional statements from above with the "filter" command
#  from the dplyr package to remove all outliers above and below the thresholds.
df.no.outliers.zscores <- df.complete %>%
  filter(!(scaledRent < lower.bound.z.scores)) %>%
  filter(!(scaledRent > upper.bound.z.scores))
# If you are unsure about how and why this step works, I would recommend you to have another
# look at the section about the filter command in combination with logical statements
# in the following DataCamp course: https://www.datacamp.com/courses/dplyr-data-manipulation-r-tutorial





## Option 2: Detection using the inter-quartile range and related boundaries
#  First step is computing the quartiles.
quartiles <- quantile(df.complete$totalRent)
Q.1 <- quartiles[2]
Q.3 <- quartiles[4]

# Computing Inter Quartile Range
inter.quartile.range <- IQR(df.complete$totalRent)

# Computing Boundary Points
lower.bound.IQR <- Q.1-1.5*inter.quartile.range
upper.bound.IQR <- Q.3+1.5*inter.quartile.range

# Find outliers
outliers.below.IQR <- df.complete$totalRent < lower.bound.IQR
outliers.above.IQR <- df.complete$totalRent > upper.bound.IQR

# Remove outliers
df.no.outliers.quartiles <- df.complete %>%
  filter(!(totalRent > upper.bound.IQR)) %>%
  filter(!(totalRent < lower.bound.IQR))

# In most cases the remaining observations will be similar
# in both cases, but not always equal. Also, please don't
# forget to take a look at the visualisations of the remaining 
# dataset. Surprises do happen sometimes.


### Messy Data #4: Inconsistencies ------------------------------------------

# As discussed during the last feedback session, dealing with
# inconsistencies is a bit trickier. You will need to find them
# manually, using descriptive statistics, conditional statements
# and visual analytics.

# The basic recipe for dealing with inconsistencies is the same as 
# with dealing with outliers and missing values:
# (1) Detect inconsistencies
# (2) Deal with them
#  (2.1) Remove inconsistencies
#  (2.2) Replace inconsistencies

## (1) Detection of inconsistencies
# By inspecting the descriptive statistics and the visualisations.
# If you have a suspicion about potentially inconsistent measurements,
# you can write a conditional statement to verify it.

# One example of an inconsistency that you found was the totalRent
# being something other than the sum of baseRent and serviceCharge.
# You would write the conditional statement like this:
total.rent.unequal.base.plus.service <- df.no.outliers.quartiles$totalRent != (df.no.outliers.quartiles$baseRent + df.no.outliers.quartiles$serviceCharge)
   # This is another conditional statement.


# Filtering the unequal values out works with the filter command:
df.inconsistencies.removed <- df.no.outliers.quartiles %>%
  filter(totalRent == (baseRent + serviceCharge))


# Replacing inconsistencies works with the "mutate" command from the dplyr package
df.inconsistencies.replaced <- df.no.outliers.quartiles %>%
  mutate(totalRent = baseRent + serviceCharge)


### Messy Data #5: Dealing with "yearConstructed" and "regio3" --------------

# Continue working with the cleaned dataset
df <- df.inconsistencies.removed
df$heatingType <- as.factor(df$heatingType)


## yearConstructed

# For the variable "yearConstructed", we talked about splitting it into 3 groups:
# Altbau, Neubau, and Mid-Life-Crisis.
# The first step in the process is to define appropriate cut-off points. These
# might be different for every city. You will need to provide an appropriate
# argument for each cut-off point. That argument can be based on an industry-specific
# event, industry specific knowledge or a historic event.

# In this example, I take the beginning of the second world war (1939) as a cut-off
# point between the category "Altbau" and "Mid-Life-Crisis".

# According to the Wikipedia Article, every building that was build (in Berlin) after 1950
# is defined as a Neubau. So I will use 1950 as an upper point for the definition of
# a Mid-Life-Crisis building.

df$buildingType <- "Altbau"
df$buildingType[which(df$yearConstructed > 1939)] <- "Mid-Life-Crisis"
df$buildingType[which(df$yearConstructed > 1950)] <- "Neubau"

# The result can be visualised in the following way:
bp<- ggplot(df, aes(x="", y=buildingType, fill=buildingType))+
  geom_bar(width = 1, stat = "identity")

pie <- bp + 
  coord_polar("y", start=0) + 
  labs(x = NULL, y = NULL, fill = NULL, title = "Proportion of each Building Type in the dataset") + theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666")) 

pie




## regio3
# The main issue with regio3 was that (1) there were simply too many different districts,
# (2) of which many/most had very few observations and that (3) most of the Dummy variables
# were not statistically different from 0. (see summary of model.1 below.)
model.1 <- lm(totalRent ~ livingSpace + regio3, data = df)
summary(model.1)

# To fix this, one can summarize some of the observations under the category "Other". For
# the dataset from Mittelsachsen_Kreis, the code looks like this:

df$regio3 <- as.character(df$regio3)

df$regio3[which(df$regio3 == "Ziegra_Knobelsdorf")] <- "Other"
df$regio3[which(df$regio3 == "Weißenborn/Erzgebirge")] <- "Other" 
df$regio3[which(df$regio3 == "Rossau")] <- "Other" 
df$regio3[which(df$regio3 == "Augustusburg")] <- "Other" 
df$regio3[which(df$regio3 == "Falkenau")] <- "Other" 
df$regio3[which(df$regio3 == "Frankenberg/Sachsen ")] <- "Other" 
df$regio3[which(df$regio3 == "Hartha")] <- "Other" 
df$regio3[which(df$regio3 == "Lichtenau")] <- "Other" 
df$regio3[which(df$regio3 == "Waldheim")] <- "Other" 
df$regio3[which(df$regio3 == "Oederan")] <- "Other" 
df$regio3[which(df$regio3 == "Mühlau")] <- "Other" 
df$regio3[which(df$regio3 == "Ostrau")] <- "Other" 
df$regio3[which(df$regio3 == "Mittweida")] <- "Other" 

df$regio3 <- as.factor(df$regio3)

# You will need to continue this process by (1) copy-pasting the statement above and (2) replacing
# the word inside the ... of the "which()"-statement with the names of the districts you want
# to replace.

bp<- ggplot(df, aes(x="", y=regio3, fill=regio3))+
  geom_bar(width = 1, stat = "identity")

pie <- bp + 
  coord_polar("y", start=0) + 
  labs(x = NULL, y = NULL, fill = NULL, title = "Proportion of each District in the dataset") + theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666")) 

pie


### Export Cleaned Dataset --------------------------------------------------

write.csv(df, "output/Cleaned_Dataset_Mittelsachsen_Kreis.csv")
