# Basic Data Manipulation
# Juan Andrade, Workshop 2
# 19.01.2024

# WD
setwd("C:/Users/Juan/Documents/1. ENREM/6 sem/Data science for environmental scientists/Basic_data_manipulation/CC-3-DataManip-master")

# Load data
elongation <- read.csv("EmpetrumElongation.csv", header = TRUE)

# Check data
head(elongation)  # preview
str(elongation)  # types of variables

# Playing with the data
elongation$Indiv  # Print all shrubs in the area
length(unique(elongation$Indiv)) # Print total number of shrubs
elongation[2, 5] # Return value second row, fifth column
elongation[6, ]  # Return values for row 6
elongation[6, ]$Indiv  # Return value of Indiv for the 6th row
elongation[elongation$Indiv == 603, ]  # Access the value for Individual number 603
elongation[elongation$Zone < 4, ]  # Returns the data for zones 2-3
elongation[elongation$Zone <= 4, ]  # Returns the data for zones 2-3-4
elongation[!elongation$Zone >= 5, ]  # Returns the data for zones 2-3-4 (excluding 5)
elongation[elongation$Zone == 2 | elongation$Zone == 7, ] # Returns only data for zones 2 and 7
elongation[elongation$Zone == 2 & elongation$Indiv %in% c(300:400), ] # Returns data for shrubs in zone 2, whose ID numbers are between 300 and 400

## Changing variable names and values in a dataframe

# Let's create a working copy of our object
elong2 <- elongation

# Use names( ) function to change the name of a column
# Used in its own,  it returns a vector of the names, Used on the left side of the assign arow, it overwrites the names to value(s)

names(elong2) # Returns the names of the columns
names(elong2) [1] <- "zone" # Changes Zone to zone. The first element is called with brackets
names(elong2) [2] <- "ID" # Changes Indiv to ID.

# Suppose a mistake in the data. The value for individual 373 in year 2008 should be 5.7
elong2[1,4]
## option 1, change with row and column number
elong2[1,4] <- 5.7
## option 2, use logical conditions for more control
elong2[elong2$ID == 373, ]$X2008 <- 5.7



### CREATING A FACTOR
# Check the classes
str(elong2)

# The zone column shows as integer (whole numbers), but should be a grouping factor, so lets turn it into a factor
elong2$zone <- as.factor(elong2$zone) #convert and overwrite original class
str(elong2) # check modification

## CHANGING A FACTOR'S LEVELS
levels(elong2$zone) # shows different factors
levels(elong2$zone) <- c("A", "B", "C", "D", "E", "F") # overwrite original names
levels(elong2$zone) # check changes


### TIDYVERSE ###
install.packages("tidyr") # install package
library(tidyr) # load package

# We want the lenghts(value) gathered by year (key)
# in this order: data frame, key, value
elongation_long <- gather(elongation, Year, Lenght, 
                            c(X2007, X2008, X2009, X2010, X2011, X2012))
# we need to specify which columns to gather, thats why we used c(X...)

# Use spread() as the inverse function, to go to a wide format
elongation_wide <- spread(elongation_long, Year, Lenght)

# Another way to gather
elongation_long2 <- gather(elongation, Year, Lenght, c(3:8))


## QUICK BOXPLOT 
boxplot(Lenght ~ Year, data = elongation_long,
        xlab = "Year", ylab = "Elongation (cm",
        main = "Annual growth of E. hermaphroditum")

### DPLYR ###
install.packages("dplyr") # install package
library(dplyr) # load package

# CHANGE NAMES of columns, overwriting the data frame
elongation_long <- rename(elongation_long, zone = Zone, indiv = Indiv, year = Year, lenght = Lenght)
# Classic R is
names(elongation_long) <- c("zone", "indiv", "year", "lenght")

# FILTERING using filter() and select()
elong_subset <- filter(elongation_long, zone %in% c(2, 3), year %in% c("X2009", "X2010", "X2011"))
# Classic R is
elongation_long[elongation_long$zone %in% c(2,3) & elongation_long$year %in% c("X2009", "X2010", "X2011"), ]

# SELECT COLUMNS
# Here, we ditch the "zone"
elong_no.zone <- dplyr::select(elongation_long, indiv, year, lenght)
# another way
elong_no.zone <- dplyr::select(elongation_long, -zone) # the minus removes the column
# Classic R is
elongation_long [ ,-1] # removes first column
# select() also lets you rename and reorder columns at the same time
elong_no.zone <- dplyr::select(elongation_long, Year = year, Shrub.ID = indiv, Growth = lenght)

# CREATE A NEW COLUMN with mutate
elong_total <- mutate(elongation, total.growth = X2007 + X2008 + X2009 + X2010 + X2011 + X2012)

# GROUP DATA
elong_grouped <- group_by(elongation_long, indiv) # grouping dataset by individual

# SuMMARISE DATA
summary1 <- summarise(elongation_long, total.growth = sum(lenght)) # sum of all growth increments in the dataset
summary2 <- summarise(elong_grouped, total.growth = sum(lenght)) # breaks total growth per individual
summary3 <- summarise(elong_grouped, total.growth = sum(lenght),
                                     mean.growth = mean(lenght),
                                     sd.growth = sd(lenght)) # Here we have multiple statistics

# JOIN DATA
# First load the other table
treatments <- read.csv("EmpetrumTreatments.csv", header = TRUE, sep = ";")
head(treatments)
# Join by ID code using DPLYR joint.
experiment <- left_join(elongation_long, treatments, by = c("indiv" = "Indiv", "zone" = "Zone"))
# Classic R is
experiment2 <- merge(elongation_long, treatments, by.x = c("zone", "indiv"), by.y = c("Zone", "Indiv"))

# Now plot to  check new data
boxplot(lenght ~ Treatment, data = experiment)
