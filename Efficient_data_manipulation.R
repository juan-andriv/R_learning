## Juan Andrade Rivera
## juan.andriv@gmail.com
## 04.02.2024

# Libraries
library(dplyr)
library(ggplot2)

# WD
setwd("C:/Users/Juan/Documents/1. ENREM/6 sem/Data science for environmental scientists/Efficient_data_manipulation")

# load data
trees <- read.csv(file = "trees.csv")
head(trees)

# to know how many trees of each species
# using classic R
trees.grouped <- group_by(trees, CommonName) # first create an internal group
trees.summary <- summarise(trees.grouped, count = length(CommonName))
print(trees.summary)
# using dplyr
trees.summary2 <- tally(trees.grouped)
print(trees.summary2)
# using pipes %>%
trees.summary3 <- trees %>%
  group_by(CommonName) %>%
  tally()
print(trees.summary3)

#### you can use ctrl + shift + m to type %>% 

# now count only three species, and break them by age group
trees.subset <- trees %>% 
  filter(CommonName %in% c("Common Ash", "Rowan", "Scots Pine")) %>% 
  group_by(CommonName, AgeGroup) %>% 
  tally()
print(trees.subset)

# Summarize all
sum.all <- summarise_all(trees, mean)
print(sum.all)

# if else
vector <- c(4, 13, 15, 6) # create a vector
ifelse(vector < 10, "A", "B") # give the conditions
# if inferior to 10, returns A, else, B.

# case when
vector2 <- c("What am I?", "A", "B", "C", "D")
case_when(vector2 == "What am I?" ~ "I am the Walrus",
          vector2 %in% c("A", "B") ~ "goo",
          vector2 == "C" ~ "ga",
          vector2 == "D" ~ "joob")

## Create a column with the Genus
unique(trees$LatinName) # shows all species
trees.genus <- trees %>%  mutate(Genus = case_when( # creates the genus column and specifies conditions
  grepl("Acer", LatinName) ~ "Acer",
  grepl("Fraxinus", LatinName) ~ "Fraxinus",
  grepl("Sorbus", LatinName) ~ "Sorbus",
  grepl("Betula", LatinName) ~ "Betula",
  grepl("Populus", LatinName) ~ "Populus",
  grepl("Laburnum", LatinName) ~ "Laburnum",
  grepl("Aesculus", LatinName) ~ "Aesculus",
  grepl("Fagus", LatinName) ~ "Fagus",
  grepl("Prunus", LatinName) ~ "Prunus",
  grepl("Pinus", LatinName) ~ "Pinus",
  grepl("Sambucus", LatinName) ~ "Sambucus",
  grepl("Crataegus", LatinName) ~ "Crataegus",
  grepl("Ilex", LatinName) ~ "Ilex",
  grepl("Quercus", LatinName) ~ "Quercus",
  grepl("Larix", LatinName) ~ "Larix",
  grepl("Salix", LatinName) ~ "Salix",
  grepl("Alnus", LatinName) ~ "Alnus")
)

## Also possible with Tidyr
library(tidyr)
trees.genus.2 <- trees %>% 
  tidyr::separate(LatinName, c("Genus", "Species"), sep = " ", remove = FALSE) %>% # create two columns, remove=FALSE means we keep the original
                    dplyr::select(-Species) # Delete new species column

## Create a column with height categories
trees.genus <- trees.genus %>% 
  mutate(Height.cat =
           case_when(Height %in% c("Up to 5 meters", "5 to 10 meters") ~ "Short",
                     Height %in% c("10 to 15 meters", "15 to 20 meters") ~ "Medium",
                     Height == "20 to 25 meters" ~ "Tall")
  )
## Reordering a factor's levels
levels(trees.genus$Height.cat) # shows the different factor levels in default order
trees.genus$Height.cat <- factor(trees.genus$Height.cat,
                                 levels = c("Short", "Medium", "Tall"), # Select new order
                                 labels = c("SHORT", "MEDIUM", "TALL") # Select new labels
                                 )
levels(trees.genus$Height.cat) # check modification


## ADVANCED PIPING FOR PLOTTING
# Subset data frame to a few genra
trees.five <- trees.genus %>% 
  filter(Genus %in% c("Acer", "Fraxinus", "Salix", "Aesculus", "Pinus"))

# Plot a map of all trees
(map.all <- ggplot(trees.five) +
    geom_point(aes(x = Easting, y = Northing, size = Height.cat, colour = Genus), alpha = 0.5) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12))
)

# Plot a map for each genus
tree.plots <- trees.five %>% # dataframe
  group_by(Genus) %>% # by genus
  do(plots = # plotting call within the do function
       ggplot(data = .) +
       geom_point(aes(x = Easting, y = Northing, size = Height.cat), alpha = 0.5) +
       labs(title = paste("Map of", .$Genus, "at Craigmillar Castle", sep = " ")) +
       theme_bw() +
       theme(panel.grid = element_blank(),
             axis.text = element_text(size = 14),
             legend.text = element_text(size = 12),
             plot.title = element_text(hjust = 0.5),
             legend.position = "bottom")
  )
# View the maps
tree.plots$plots

# Save the maps
tree.plots %>% 
  do(., 
     ggsave(.$plots, filename = paste(getwd(), "/", "map-", .$Genus, ".png", sep = ""), # paste() is used to define the filename
            device = "png", height = 12, width = 16, units = "cm"))