# =============================================================================
# 01 — Exploratory data analysis
# Accompanies: slides/01_data_visualization.pdf
# Required packages: tidyverse
# =============================================================================

rm(list = ls())
cat("\014")

### Libraries ---------------------------------------------------------------

library(tidyverse)

### Load Data ---------------------------------------------------------------


setwd("./")

# This is where we load the dataset. You will need to set the name to the name of the file you need to load.
df <- read.csv("data/Dataset_Teachers.csv")
df$X <- NULL
df$heatingType<-as.factor(df$heatingType)

df <- df[df$totalRent<5500,]


### Distributional Plots ----------------------------------------------------------------

# The first thing to do is to look into the rear window and check if there are any obvious signs of outliers.
# Inconsistencies need to be watched out for as well. A violation of an upper- or lower bound would represent an 
# inconsistency that will be visible in histograms. An example of this would be a negative value for the 
# variables "totalRent" or "livingSpace".


# Creating a histogram
hist(df$totalRent)


# Creating a Box Plot for a single variable
p <- ggplot(df, aes(x="", y=totalRent)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(face = "bold", color = "#993333", 
                                                    angle = 90))
p

# Creating a Box Plot for a multiple variables
df.subset.for.boxplot <- df %>%
  select(totalRent,
         serviceCharge,
         baseRent)

p <- ggplot(stack(df.subset.for.boxplot), aes(x = ind, y = values)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(face = "bold", color = "#993333", 
                                   angle = 90))
p


# Heating type vs total rent
p <- ggplot(df, aes(x=heatingType, y=totalRent)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(face = "bold", 
                                   color = "#993333", angle = 90))
p

df$noRooms <- as.factor(df$noRooms)
p <- ggplot(df, aes(x=hasKitchen, y=totalRent)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(face = "bold", 
                                   color = "#993333", angle = 90))
p
# One thing to note is that box-plots and histograms make sense for numeric
# variables only. For visualizing the distribution of categoric or character
# variables one needs a different type of plot. One can for instance use a
# pie chart.

# Creating a Pie Chart for the variable "heatingType"
bp<- ggplot(df, aes(x="", y=heatingType, fill=heatingType))+
  geom_bar(width = 1, stat = "identity")

pie <- bp + 
  coord_polar("y", start=0) + 
  labs(x = NULL, y = NULL, fill = NULL, title = "Proportion of each Heating Type in the dataset") + theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666")) 

pie



### Relationships between variables -----------------------------------------

# In addition to looking back at the data cleaning step, you will also need to look
# ahead to the feature engineering and the predictive modelling step of the data 
# science life cycle. During those steps you will not only need to build a 
# predictive model, you will also need to interpret it and evaluate whether the
# outputs of the model make sense. Specifically, you will also need to tell us 
# whether you think that the computed impact of the explanatory variables (i.e. living space
# and number of rooms) on the target variable (in our case totalRent) actually make
# sense. In that evaluation you will need to argue with the help of the data that
# underlies the model - so the data you are working with right now.

# You also have the option to check whether the hypotheses you made when you selected the
# variables in the first report-related homework actually hold. Please do this.

# We can evaluate the impact of one variable on another by plotting the variables
# against each other. ggplots offers several ways to plot 2 and sometimes even three 
# variables in the same plot. This file introduces two of them: Scatter plots and
# bar charts.



# You can quickly and easily create scatter plots between variables by using the function
# "pairs()". The function creates scatter plots for all variables contained in the dataframe
# you use as an input.
# Please note that even though it is possible to create a scatter plot for categorical variables
# as well, the number of insights you are able to derive from those plots is fairly limited. It is
# therefore useful to select numeric variables into a separate data frame before plotting.
df.numeric.variables <- df %>%
  select(totalRent,
         serviceCharge,
         baseRent,
         livingSpace)
pairs(df.numeric.variables)


## Scatter plot
# Even though you already created a scatter plot for the dataset, there are occasions on which you
# may want to take a closer look at a specific scatter plot. For that ggplot also provides a function.
# I assume that most of you know scatter plots and how they are used. I will therefore
# not describe them in much detail. 
# The code below plots the variable "livingSpace" against "totalRent". 
scatter <- ggplot(df, aes(x=livingSpace, y= totalRent)) +
  geom_point() + 
  ggtitle("Scatter Plot of Rent vs Living Space")
scatter
# The plot shows
# a clear upwards trend. This means that an increase in living space actually does come
# with an increase in rent.



# You can also add a third variable to a scatter plot. The function below creates the
# same scatter plot as above with a small change. This time the apartments with more 
# rooms have a larger point.
scatter <- ggplot(df, aes(x=livingSpace, y= totalRent)) +
  geom_point(aes(size=noRooms)) + 
  ggtitle("Scatter Plot of Rent vs Living Space and Number of Rooms")
scatter
# This time the plot shows that apartments with a larger living area are not only 
# more expensive, but also have a higher number of rooms.



## Histograms for different entries of categorical variables
# You will also be able to create histograms for each entries of categorical or
# character variables. For instance, you are able to create a histogram for each 
# heating type.
k <- ggplot(df, aes(totalRent, stat(count)))+
  geom_histogram(binwidth = 0.2) + 
  geom_histogram(aes(y = stat(count))) +
  ggtitle("Total Rent per Heating Type - Overall")+
  facet_grid(. ~ heatingType)
k 
# Don't get discouraged by the following warning: "`stat_bin()` using `bins = 30`. Pick better value with `binwidth`."
# This one always pops up. Unless you get an additional warning or error message, the code should be running.
