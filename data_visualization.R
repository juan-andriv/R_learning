## Juan Andrade Rivera
## juan.andriv@gmail.com
## 04.02.2024
## Data Viz practicum

# Set WD
setwd("C:/Users/Juan/Documents/1. ENREM/6 sem/Data science for environmental scientists/data_visualization")

# install packages
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readr")
install.packages("gridExtra")
install.packages("colourpicker")

# load libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)
library(colourpicker)
# import data
LPI <- read.csv("LPIdata_CC.csv")
View(LPI) # notice the data is wide.. use gather

# reshape data, columnds 9 to 53
LPI2 <- gather(LPI, "year", "abundance", 9:53)
View(LPI2)
# check data
str(LPI2) # notice that "year" and "abundance" are chr, but they need to be numberic
# transoform 
LPI2$year <- parse_number(LPI2$year)
LPI2$abundance <- as.numeric(LPI2$abundance)
# check data again
str(LPI2)

# dataset is too large, filter for one species
vulture <- filter(LPI2, Common.Name == "Griffon vulture / Eurasian griffon")
head(vulture)
# get rid of NAs
vulture <- na.omit(vulture)
View(vulture)


## HISTOGRAM
# with basic R
base_hist <- hist(vulture$abundance)
# with ggplot2.. notice that putting extra parenthesis shows the graph
(vulture_hist <- ggplot(vulture, aes(x = abundance)) +
  geom_histogram())
# now customize!
(vulture_hist <- ggplot(vulture, aes(x = abundance)) + #create graph
    geom_histogram(binwith = 250, colour = "#8B5A00", fill = "#CD8500") + # change bandwidth and colors
    geom_vline(aes(xintercept = mean(abundance)), # add a line for mean abundance
               colour = "red", linetype = "dashed", size = 1) + # change colour of line
    theme_bw() + # change theme to bw
    ylab("Count/n") + # changing y axis label
    xlab("\nGriffon vulture abundance") + #\n adds a blank line between axis and text
    theme(axis.text = element_text(size = 12), # change font size
          axis.title.x = element_text(size = 14, face = "plain"), # plain could be bold, italic, or others
          panel.grid = element_blank(), # removing gray grid lines
          plot.margin = unit(c(1,1,1,1), units = , "cm")) # putting a 1cm margin around the plot
)


## SCATTER PLOT
# filter data to get only Croatia and Italy
vultureITCR <- filter(vulture, Country.list %in% c("Croatia", "Italy"))
# Plot with basic R
plot(vultureITCR$year, vultureITCR$abundance, col = c("#1874CD", "#68228B"))
# using ggplot2
(vulture_scatter <- ggplot(vultureITCR, aes(x = year, y = abundance, colour = Country.list)) + # linking colour to a factor
    geom_point())
# Customize!
(vulture_scatter <- ggplot(vultureITCR, aes(x = year, y = abundance, colour = Country.list)) + 
    geom_point(size = 2) + # point size
    geom_smooth(method = "lm", aes(fill = Country.list)) + # add linear model fit, colourcoded with country
    theme_bw() +
    scale_fill_manual(values = c("#EE7600", "#00868B")) + # adding custom colours for solid geoms(ribbon)
    scale_colour_manual(values = c("#EE7600", "#00868B"), # adding custom colours for points and lines
    labels = c("Croatia", "Italy")) + # adding labels for legend
ylab("Griffon vulture abundance\n") +
  xlab("\nYear") +
  theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1), # making the years in the x axis a little angled
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "plain"),
        panel.grid = element_blank(), # removing background grid
        plot.margin = unit(c(1,1,1,1), units = "cm"), # add 1cm margin
        legend.text = element_text(size = 12, face = "italic"), # setting the font for the legend
        legend.title = element_blank(), # removing legend title
        legend.position = c(0.9, 0.9)) # position legend where 0 is left / bottom, 1 is right / top
)

## BOXPLOT
(vulture_boxplot <- ggplot(vultureITCR, aes(Country.list, abundance)) +
    geom_boxplot())
# Customize!
(vulture_boxplot <- ggplot(vultureITCR, aes(Country.list, abundance)) +
    geom_boxplot(aes(fill = Country.list)) +
    theme_bw() +
    scale_fill_manual(values = c("#EE7600", "#00868B")) + # adding custom colors
    scale_colour_manual(values = c("#EE7600", "#00868B")) + # adding custom colors
    ylab("Griffon vulture abundance\n") +
    xlab("\nCountry") +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),
          panel.grid = element_blank(), # removing grid
          plot.margin = unit(c(1,1,1,1), units = "cm"), # adding margins
  legend.position = "none") # removing legend
)

## BARPLOT
# calculate richness using pipes %>%
richness <- LPI2 %>% filter(Country.list %in% c("United Kingdom", "Germany", "France", "Netherlands", "Italy")) %>%
  group_by(Country.list) %>%
  mutate(richness = (length(unique(Common.Name)))) # create a new column based on how many unique common names are in each country
# plot species richness
(richness_barplot <- ggplot(richness, aes(x = Country.list, y = richness))+
    geom_bar(position = position_dodge(), stat = "identity", colour = "black", fill = "#00868B") +
    theme_bw() +
    ylab("Species richness\n") +
    xlab("Country") +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          panel.grid = element_blank(),
          plot.margin = unit(c(1,1,1,1), units = , "cm"))
)

## FACETS
# Plot the population change for all countries with NO facets
(vulture_scatter_all <- ggplot(vulture, aes(x = year, y = abundance, colour = Country.list)) +
    geom_point(size = 2) +
    geom_smooth(method = "lm", aes(fill = Country.list)) +
    theme_bw() +
    ylab("Griffon vulture abundance \n") +
    xlab("\nYear") +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),
          panel.grid = element_blank(),
          plot.margin = unit(c(1,1,1,1), units = , "cm"),
          legend.text = element_text(size = 12, face = "italic"),
          legend.title = element_blank(),
          legend.position = "right")
  )
# Its too cluttered. Lets use facets.
(vulture_scatter_facets <- ggplot(vulture, aes(x = year, y = abundance, colour = Country.list)) +
  geom_point(size = 2)+
  geom_smooth(method = "lm", aes(fill = Country.list)) +
  facet_wrap( ~ Country.list, scales = "free_y") + # this line creates the facets
  theme_bw() +
  ylab("Griffon vulture abundance\n") +
  xlab("\nYear") +
  theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "plain"),
        panel.grid = element_blank(),
        plot.margin = unit(c(1,1,1,1), units = , "cm"),
        legend.text = element_text(size = 12, face = "italic"),
        legend.title = element_blank(),
        legend.position = "right")
)

## PANELS
grid.arrange(vulture_hist, vulture_scatter, vulture_boxplot, ncol = 1)
# Fix the display
(panel <- grid.arrange(
  vulture_hist + ggtitle("(a)") + ylab("Count") + xlab("Abundance") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")),
  
  vulture_boxplot + ggtitle("(b)") + ylab("Abundance") + xlab("Country") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")),
  
  vulture_scatter + ggtitle("(c)") + ylab("Abundance") + xlab("Year") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")) +
    
    theme(legend.text = element_text(size = 12, face = "italic"),
          legend.title = element_blank(),
          legend.position = c(0.85, 0.85)),
 
   ncol = 1))

# Save the image to wd
ggsave(panel, file = "vulture_panel2.png", width = 5, height = 12) 

