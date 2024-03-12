## Data visualization 2
## Juan Andrade Rivera
## 26.02.2024

## load libraries
library(dplyr) # Data manipulation
library(ggplot2) # Data visualization

## WD
setwd("C:/Users/Juan/Documents/1. ENREM/6 More Courses/Data science for environmental scientists/Data_visualization_2")

## Read data
magic_veg <- read.csv("magic_veg.csv")
str(magic_veg) # check data

# Count species
species_counts <- magic_veg %>%
  group_by(land, plot) %>%
  summarise(Species_number = length(unique(species)))

# Histogram
(hist <- ggplot(species_counts, aes(x = plot)) +
    geom_histogram()) # note this is wrong because the Data is already Summarised

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number)) +
    geom_histogram(stat = "identity")) # this one is right, but it aggregates both lands

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity")) # this separates by land. since its from the data, it is specified inside aes()

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge")) # This is to separate stacked columns

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = c(1:6)) + 
    scale_y_continuous(limits = c(0, 50))) # this adds titles to the axis

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = c(1:6)) + 
    scale_y_continuous(limits = c(0, 50)) +
    labs(title = "Species richness by plot",
         subtitle = "In the magical lands",
         captions = "Data from the Ministry of Magic",
         x = "\n Plot number", y = "Number of species \n")) # this adds labels and titles

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = c(1:6)) + 
    scale_y_continuous(limits = c(0, 50)) +
    labs(title = "Species richness by plot",
         x = "\n Plot number", y = "Number of species \n") +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 12, face = "italic"),
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"))) # theme() to customise fonts and labels

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = c(1:6)) + 
    scale_y_continuous(limits = c(0, 50)) +
    labs(title = "Species richness by plot",
         x = "\n Plot number", y = "Number of species \n") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"))) # change background with theme_bw()

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = c(1:6)) + 
    scale_y_continuous(limits = c(0, 50)) +
    scale_fill_manual(values = c("rosybrown1", "#deebf7"), # Scale_Fill is used to specify colors and Name of legend
                      name = "Land of Magic") + 
    labs(title = "Species richness by plot",
         x = "\n Plot number", y = "Number of species \n") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          legend.title = element_text(face = "bold"),
          legend.position = "bottom",
          legend.box.background = element_rect(color = "grey", size = 0.3)))

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = c(1:6)) + 
    scale_y_continuous(limits = c(0, 50)) +
    scale_fill_manual(values = c("rosybrown1", "#deebf7"), 
                      labels = c("HOGSMEADE", "NARNIA"), # Changing labels
                      name = "Land of Magic") + 
    labs(title = "Species richness by plot",
         x = "\n Plot number", y = "Number of species \n") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          legend.title = element_text(face = "bold"),
          legend.position = "bottom",
          legend.box.background = element_rect(color = "grey", size = 0.3)))

# Now save
ggsave("magical-sp-rich-hist.png", width = 7, height = 5, dpi = 300)



## Now to assign specific colors to each factor

# create vectors
land <- factor(c("Narnia, ", "Hogsmeade", "Westeros", "The Shire", "Mordor", "Forbidden Forest", "Oz"))
counts <- as.numeric(c(55, 48, 37, 62,11, 39, 51))

# create dataframe
more_magic <- data.frame(land,counts)

# check factor levels
length(levels(more_magic$land)) # check how many levels (7)

# CREATE COLOR PALETTE
magic.palette <- c("#698B69", "#5D478B", "#5C5C5C", "#CD6090", "#EEC900", "#5F9EA0", "#6CA6CD") # defining 7 colours
names(magic.palette) <- levels(more_magic$land) # linking factor names to each color

# now create a bar plot to see
(hist <- ggplot(more_magic, aes(x = land, y = counts, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
                  scale_y_continuous(limits = c(0, 65)) +
                  scale_fill_manual(values = magic.palette, # Using the new palette
                                    name = "Land of Magic") +
    labs(title = "Species richness in magical lands",
         x = "", y = "Number of species \n") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_text(size = 12),
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          legend.title = element_text(face = "bold"),
          legend.position = "bottom",
          legend.box.background = element_rect(color = "grey", size = 0.3)))

## Colorus remain linked even when filtering factors
(hist <- ggplot(filter(more_magic, land %in% c("Hogsmeade", "Oz", "The Shire")), # filtering
                aes(x = land, y = counts, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_y_continuous(limits = c(0, 65)) +
    scale_fill_manual(values = magic.palette,
                      name = "Land of Magic") +
    labs(title = "Species richness in magical lands",
         x = "", y = "Number of species \n") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_text(size = 12),
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          legend.title = element_text(face = "bold"),
          legend.position = "bottom",
          legend.box.background = element_rect(color = "grey", size = 0.3)))

## Now make BOXPLOTS
yearly_counts <- magic_veg %>%
  group_by(land, plot, year) %>%  # added year
  summarise(Species_number = length(unique(species))) %>%
  ungroup() %>% # ungroup after summarising
  mutate(plot = as.factor(plot)) # change plot to "factor" type data

(boxplot <- ggplot(yearly_counts, aes(plot, Species_number, fill = land)) +
    geom_boxplot())

# now to make it beautiful
(boxplot <- ggplot(yearly_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_boxplot() +
    scale_x_discrete(breaks = 1:6) +
    scale_fill_manual(values = c("rosybrown1", "#deebf7"),
                      breaks = c("Hogsmeade", "Narnia"),
                      name = "Land of magic",
                      labels = c("Hogsmeade", "Narnia")) +
    labs(title = "Species richness by plot",
         x = "\b Plot number", y = "number of species \n") +
    theme_bw() +
    theme() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3)))

# Saving Boxplot
ggsave("magical.sp.rich-boxplot1.png", width = 7, height = 5, dpi = 300)


## Creating a Dot Plot

#First summarise data
summary <- species_counts %>%
  group_by(land) %>%
  summarise(mean = mean(Species_number),
            sd = sd(Species_number))

# Then make dotplot
(dot <- ggplot(summary, aes(x = land, y = mean, colour = land)) +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
    geom_point(size = 3) +
    scale_y_continuous(limits = c(0, 50)) +
    scale_colour_manual(values = c('#CD5C5C', '#6CA6CD'),
                        labels = c("HOGSMEADE", "NARNIA"),
                        name = "Land of Magic") +
    labs(title = "Average species richness",
         x = "",y = "Number of species\n") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"),
          legend.title = element_text(face = "bold"),
          legend.position = "bottom",
          legend.box.background = element_rect(color = "grey", size = 0.3)))

## REORDERING DATA
yearly_counts$land <- factor(yearly_counts$land,
                             levels = c("Narnia", "Hogsmeade"),
                             labels = c("Narnia", "Hogsmeade"))

# Plot the reordered boxplot
(boxplot <- ggplot(yearly_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_boxplot() +
    scale_x_discrete(breaks = 1:6) +
    scale_fill_manual(values = c("#deebf7", "rosybrown1"),
                      breaks = c("Narnia", "Hogsmeade"),
                      name = "Land of magic",
                      labels = c("Narnia", "Hogsmeade")) +
    labs(title = "Species richness by plot",
         x = "\b Plot number", y = "number of species \n") +
    theme_bw() +
    theme() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3)))

# Reordering plots
yearly_counts$plot <- factor(yearly_counts$plot,
                             levels = c("6", "1", "2", "3", "4", "5"),
                             labels = c("6", "1", "2", "3", "4", "5"))

# Plot the reordered boxplot
(boxplot <- ggplot(yearly_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_boxplot() +
    scale_x_discrete(breaks = 1:6) +
    scale_fill_manual(values = c("#deebf7", "rosybrown1"),
                      breaks = c("Narnia", "Hogsmeade"),
                      name = "Land of magic",
                      labels = c("Narnia", "Hogsmeade")) +
    labs(title = "Species richness by plot",
         x = "\b Plot number", y = "number of species \n") +
    theme_bw() +
    theme() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3)))


## REGRESSION LINES
heights <- magic_veg %>%
  filter(!is.na(height)) %>% #removing NA values
  group_by(year, land, plot, id) %>%
  summarise(Max_Height = max(height)) %>% # Calculating Max Height
  ungroup() %>% # ungroup so pipeline does not get confused
  group_by(year, land, plot) %>%
  summarise(Height = mean(Max_Height)) # Calculating Mean Max Height
  
# basic scatterplot
(basic_mm_scatter <- ggplot(heights, aes(year, Height, colour = land)) +
    geom_point() +
    theme_bw())

# Add linear regression
(basic_mm_scatter <- ggplot(heights, aes(year, Height, colour = land)) +
    geom_point() +
    theme_bw() +
    stat_smooth(method = "lm"))

# Change for quadratic regression
(basic_mm_scatter <- ggplot(heights, aes(year, Height, colour = land)) +
    geom_point() +
    theme_bw() +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2)))


## CREATING A THEME
theme_coding <- function(){ # creating a new theme function
  theme_bw() + # using a predefined theme as base
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14),
          panel.grid = element_blank(),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"),
          plot.title = element_text( size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "italic"),
          legend.title = element_blank(),
          legend.position = c(0.9,0.9))
}


## Boxplot using custom theme
(boxplot <- ggplot(yearly_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_boxplot() +
    scale_x_discrete(breaks = 1:6) +
    scale_fill_manual(values = c("#deebf7", "rosybrown1"),
                      breaks = c("Narnia","Hogsmeade"),
                      name = "Land of magic",
                      labels = c("Narnia", "Hogsmeade")) +
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") +
    theme_coding()
)

## to overwrite something from the custom theme, just overwrite it
(boxplot <- ggplot(yearly_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_boxplot() +
    scale_x_discrete(breaks = 1:6) +
    scale_fill_manual(values = c("#deebf7", "rosybrown1"),
                      breaks = c("Narnia","Hogsmeade"),
                      name = "Land of magic",
                      labels = c("Narnia", "Hogsmeade")) +
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") +
    theme_coding() +
    theme(legend.position = "right") # this overwrites the theme's legend position
)
)