## Intro to Spatial Analysis
## Juan Andrade Rivera
## 11.03.2024
# Satellite data available from https://scihub.copernicus.eu/

#########

# Set wd
setwd("C:/Users/Juan/Documents/1. ENREM/6 More Courses/Data science for environmental scientists/Spatial_analysis")

# install and load packages
install.packages("sp")
install.packages("rgdal")
install.packages("raster")
install.packages("ggplot2")
install.packages("viridis")
install.packages("rasterVis")
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(viridis)
library(rasterVis)

# Load raster
tay <- raster('taycrop.tif')

# Get properties of the Tay Raster
tay # shows number of bands, dimension, extent, coordinate reference system, e.g.

# to create individual raster layers for each spectral band
b1 <- raster('taycrop.tif', band = 1)
b2 <- raster('taycrop.tif', band = 2)
b3 <- raster('taycrop.tif', band = 3)
b4 <- raster('taycrop.tif', band = 4)
b5 <- raster('taycrop.tif', band = 5)
b6 <- raster('taycrop.tif', band = 6)
b7 <- raster('taycrop.tif', band = 7)
b8 <- raster('taycrop.tif', band = 8)
b9 <- raster('taycrop.tif', band = 9)
b10 <- raster('taycrop.tif', band = 10)
b11 <- raster('taycrop.tif', band = 11)
b12 <- raster('taycrop.tif', band = 12)

# Good Practice is comparing 2 bands to see if they match extent, number of rows and column, projection, resolution and origin
compareRaster(b2, b3)
# it should say TRUE

# to see the raster use plot or image
plot(b8) # this caps at 100,000 pixels
image(b8) # this stretches the image

zoom(b8)    # run this line, then click twice on your plot to define a box

# to crop an image
plot(tay)
e <- drawExtent() # run this line, then click twice on your pot to define a box
cropped_tay <- crop(b7, e)
plot(cropped_tay)

# now to visualize with different color palettes
png('tayplot.png', width = 4, height = 4, units = 'in', res = 300) # to save plot
image(b8, col = viridis_pal(option = "D")(10), main = "Sentinel 2 image of Loch Tay") # this is to view the plot
dev.off()
# to save plot
# dev.off() is a function that clears the slate, you're done with that plot
# if you dont dev.off() it can clash when saving a differnet plot

# To create a photorealistic view, stack red, green and blue bands
# create a raster stack, a multi-layered raster object, of the red(b4), green(b3) and blue(b2) bands

png('RGB.png', width = 5, height = 4, units = "in", res = 300) # this specifies how to save the image
tayRGB <- stack(list(b4,b3,b2)) # This creates a raster stack
plotRGB(tayRGB, axes = TRUE, stretch = "lin", main = "Sentinel RGB colour composite")
dev.off()

## A False colour composite (FCC) is used to visualize vegetation
# Replace red with near infrared(b8), green with red, and blue with green
# This creates an image with red vegetation
png('FCC_ex.png', width = 5, height = 4, units = "in", res = 300) # this specifies how to save the image
tayFCC_ex <- stack(list(b8,b4,b3)) # This creates a raster stack
plotRGB(tayFCC_ex, axes = TRUE, stretch = "lin", main = "Sentinel FCC example")
dev.off()

## Create a FCC using a Raster Stack, with rasterVis
gplot(b8) +
  geom_raster(aes(x = x, y = y, fill = value)) + # value is the specific value of reflectance each pixel is associated with
  scale_fill_viridis_c() +
  coord_quickmap() +
  ggtitle("West of Loch tay, raster plot") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() + #removes gray background
  theme(plot.title = element_text(hjust = 0.5), # centers title
        text = element_text(size = 20), # font size
        axis.text.x = element_text(angle = 90, hjust = 1)) # rotates x axis text

ggsave("ggtay.png", scale = 1.5, dpi = 300) # to save plot

# visualize all bands together with a facet wrap
t <- stack(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12)
gplot(t) + 
  geom_raster(aes(x = x, y = y, fill = value))+
  scale_fill_viridis_c() +
  facet_wrap(~variable) + # this is the facet wrap
  coord_quickmap()+
  ggtitle("Sentinel 2 Loch tay, raster plots") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("allbands.png", scale = 1.5, dpi = 300)

# for a quick viz, just use Plot
s_tay <- brick('taycrop.tif')
plot(s_tay)
  

# Now to manipulate rasters: NDVI and KMN classification
# NDVI

# Created a Vegetation Index function
VI <- function(img, k, i) {
  bk <- img[[k]]
  bi <- img[[i]]
  vi <- (bk - bi) / (bk + bi)
  return(vi)
}

# For Sentinel 2, the relevant bands are:
# NIR = 8, red = 4

ndvi <- VI(s_tay, 8, 4)
# 8 and 4 refer to the bands

png('ndviplot.png', width = 4, height = 4, units = "in", res = 300)
plot(ndvi, col = rev(terrain.colors(10)), main = 'Sentinel 2, Loch Tay-NDVI')
dev.off()

# Create a histogram of NDVI data
png('ndvihist.png', width = 4, height = 4, units = "in", res = 300)
hist(ndvi,
     main = "Distribution of NDVI values",
     xlab = "NDVI",
     ylab = "Frequency",
     col = "aquamarine3",
     xlim = c(-0.5, 1),
     breaks = 30,
     xaxt = 'n')
axis(side = 1, at = seq(-0.5, 1, 0.05), labels = seq(-0.5, 1, 0.05))
dev.off()

# mask the pixels with a value less than 0.4 to highlight the vegetated areas
png('ndvimask.png', width = 4, height = 4, units = "in", res = 300)
veg <- reclassify(ndvi, cbind(-Inf, 0.4, NA))
# reclassifying and making al values between negative infinity and 0.4 be NAs
plot(veg, main = 'Veg cover')
dev.off()

# To save as a raster file use writeraster function
writeRaster(x = ndvi,
            filename = "wd",
            format = "GTiff", # Save as a tif
            datatype = 'INT2S') # Save as INTEGER instead of FLOAT

# Now, an unsupervised classification
# Clustering the pixels using kmean algorithms

# convert the raster to vector/matrix ('getValues' converts the Raster to Array)
nr <- getValues(ndvi)
str(nr)

# its important to set the seed generator, because 'kmeans' initiates centres in random locations
# the seed generator just generates random numbers
set.seed(99)
#Create 10 clusters, allow 500 iterations, start with 5 random sets using LLoyd method
kmncluster <- kmeans(na.omit(nr), centers = 10, iter.max = 500,
                     nstart = 5, algorithm = "Lloyd")

# kmeans returns an object of class 'kmeans'
str(kmncluster)

# cell values of kmncluster$cluster range between 1 and 10 and are the groups
# clasification is complete, now visualize

# first, create a copy of ndvi layer
knr <- ndvi

# now, rel¿place raster cell values with kmncluster$cluster
# array
knr[] <- kmncluster$cluster

# alternative wayto achieve the same
values(knr) <- kmncluster$cluster
knr

# it has no reference, we need to plot it side by side with NDVI
png('ndvi_kmeans.png', width = 10, height = 8, units = "in", res = 300)
par(mfrow = c(1, 2))
plot(ndvi, col = rev(terrain.colors(19)), main = "NDVI")
plot(knr, main = "Kmeans", col = viridis_pal(option = "D")(10))
dev.off()

# the same but with RGB
png('rgb_kmeans.png', width = 10, height = 8, units = "in", res = 300)
par(mar = c(10.8, 5, 10.8, 2), mfrow = c(1, 2))
plotRGB(tayRGB, axes = TRUE, stretch = "lin", main = "RGB")
plot(knr, main = "Kmeans", col = viridis_pal(option = "D")(10))
dev.off()
