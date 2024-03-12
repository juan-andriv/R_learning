## Modelling
## Juan Andrade Rivera, Workshop 5
## 03.02.2024

## WD
setwd("C:/Users/Juan/Documents/1. ENREM/6 sem/Data science for environmental scientists/Modelling")

## Load data
install.packages("agridat")
library(agridat)

# Loading the dataset from agridat
apples <- agridat::archbold.apple
head(apples)
summary(apples)

## Define a ggplot theme
theme.clean <- function(){
  theme_bw()+
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text (size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),
          axis.title.y = element_text(size = 14, face = "plain"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size=12, face = "italic"),
          legend.position = "right")
}

## Turn "spacing" into a categorical variable
apples$spacing2 <- as.factor(apples$spacing)

## Run boxplot
library(ggplot2)
(apples.p <- ggplot(apples, aes(spacing2, yield)) +
    geom_boxplot(fill = "#CD3333", alpha = 0.8, colour = "#8B2323") +
    theme.clean() + 
    theme(axis.text.x = element_text(size = 12, angle = 0)) +
    labs(x = "Spacing (m)", y = "Yield (kg)"))

## Run a model to test
apples.m <- lm(yield ~ spacing2, data = apples)
summary (apples.m)
## things to notice: intercept is the value oof the first categorical value (6), under it comes the values for 10 and 14, as relative to the intercept, so (intercept + spacing2 10) and so on...
# another thing, R-squared refer to how much of the variation in yield is explained by predictor "spacing2".

## Run another model
sheep <- agridat::ilri.sheep # Load the data
library(dplyr)
sheep <- filter(sheep, ewegen == "R") # well only consider lambs from breed "R".
head(sheep) # Check data

sheepm1 <- lm(weanwt ~ weanage, data = sheep) # run the model
summary (sheepm1) # check output
## things to notice: this is not categorical variables, but continuous variable.
# intercept is the value of Y when X = 0. (usually pointless, here is the weight of a newborn)
# weanage estimate is the SLOPE of the relationship. in this case, everyday is an avg increase of 0.08 kg of weight
# linear model would be lamb weight = 2.50 + 0.08(age)
# according to R2, age explains around 20% of lamb weight

## Another model
sheep.m2 <- lm(weanwt ~ weanage*sex, data = sheep)
summary(sheep.m2)
## things to notice: female is the reference group
# intercept is the weight at age 0 for females
# weanage is the effect of age
# sexM is the difference of intercept value for males
# weanage:sexM is the difference in slope for males
# Models would be:
# Female weight = 3.66 + 0.06(age)
# Male weight = 3.66 + [-2.52] + 0.06(age) + [0.03(age)]

## PLOT
(sheep.p <- ggplot(sheep, aes(x = weanage, y= weanwt)) +
    geom_point(aes(colour = sex)) + # scatter plot, coloured by sex
    labs(x = "Age at weaning (days)", y = "Wean weight (kg)") +
    stat_smooth(method = "lm", aes(fill = sex, colour = sex)) + # adding regression lines for each sex
    scale_colour_manual(values = c("#FFC125", "#36648B")) +
    scale_fill_manual(values = c("#FFC125", "#36648B")) +
    theme.clean()
)

## Checking assumptions for linear models
## normal distribution and equal variances, no way to test independence 
# Check that residuals are normally distributed
apples.resid <- resid(apples.m) # extracting residuals
shapiro.test(apples.resid) # using Shapiro-Wilk test
# The null hypothesis of normal distribution is accepted,
# meaning there is no significant difference from a normal distribution (p>0.05)

## Checking homoscedasticity
bartlett.test(apples$yield, apples$spacing2) # one way to do it
bartlett.test(yield ~ spacing2, data = apples) # another way to do it
# the null hypothesis of homeoscedasticity is accepted

## we can examine data with these 4plots: 
plot(apples.m)
## complex issue, not covered here.



## POISSON DISTRIBUTION
shag <- read.csv("shagLPI.csv", header = TRUE)
shag$year <- as.numeric(shag$year) # transform year from character to numeric value
# Make histogram to assess data distribution
(shag.hist <- ggplot(shag, aes(pop)) + geom_histogram() + theme.clean())

# Create a Poisson model
shag.m <- glm(pop ~ year, family = poisson, data = shag)
summary(shag.m)
## Notice how abundance varies significantly based on predictor year

# Visualize
(shag.p <- ggplot(shag, aes(x = year, y = pop)) +
    geom_point(colour = "#483D8B") +
    geom_smooth(method = glm, colour = "#483D8B", fill = "#483D8B", alpha = 0.6) +
    scale_x_continuous(breaks = c(1975, 1980, 1985, 1990, 1995, 2000, 2005)) +
    theme.clean() +
    labs(x = " ", y = "European Shag abundance"))

## BINOMIAL DISTRIBUTION

Weevil_damage <- read.csv("Weevil_damage.csv")

# making block a categorical variable
Weevil_damage$block <- as.factor(Weevil_damage$block)

# Running model
weevil.m <- glm(damage_T_F ~ block, family = binomial, data = Weevil_damage)
summary(weevil.m)

## things to notice: check Null deviance = variability explained by null model; and Residual deviance = variability that remains afteryou explain some with explanatory variable.
# shortly, a bigger reduction from null to residual means the model is explaining better the relationship
