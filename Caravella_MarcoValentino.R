#-------------------------------------TAKE-HOME EXAM--------------------------------------
# Caravella Marco Valentino 0001134954

path <- "C:/Users/user/Documents/Unibo/Economics and Econometrics/Second Year/First Semester/GIS Tools Laboratory/Take-Home Exam/additional_exam_data"
setwd(path)

# Load libraries
library(ggplot2)
library(sf)
library(spData)
library(tidyverse) 
library(readxl)
library(dplyr)
library(viridis)
library(terra)
library(exactextractr)

# SECTION B: TASK REPLICATION 

# -TASK 1-

# Load data
world <- world
shp <- world %>% filter(name_long %in% "Serbia")
shp3 <- st_read("NUTS_RG_01M_2021_4326_LEVL_3_repaired.shp") # Load he shapefile of European NUTS 3 regions
shp3 <- shp3 %>% filter(CNTR_CODE == "RS") # Keep only Serbia

print(shp)
print(shp3)

# Create a boundary box containing the shapefile of Serbia with a small margin (0.5 in 
# all directions)
box <- st_bbox(shp)
box <- box+c(-.5,-.5,.5,.5)

box # These are the coordinates of the borders of the shapefile

# -TASK 2-

# Load SPEI
r.spei <- rast("spei01.nc")
r.spei
plot(r.spei)

# Crop "spei01" around the boxplot
r.spei <- crop(r.spei, box) 
r.spei # Note that the extent of raster data is slightly different from that of the 
# box plot. However, we expected that.
plot(r.spei)

# Keep only the last six layers
nlyr(r.spei)
num_layers <- nlyr(r.spei)
reduced_spei <- r.spei[[ (num_layers - 5):num_layers ]]
nlyr(reduced_spei)
plot(reduced_spei)

# or
layers_to_keep <- c(1440, 1428, 1416, 1404, 1392, 1380)
reduced_spei1 <- r.spei[[layers_to_keep]]
names(reduced_spei1)
plot(reduced_spei1)

# -TASK 3-
set.seed(954)

# Generate a random vector 
random_vector <- runif(6, min = 0, max = 1)

# Multiply the six layers of the reduced_spei by the corresponding element of the 
# random vector
reduced_spei[[1]] <- reduced_spei[[1]] * random_vector[1]
reduced_spei[[2]] <- reduced_spei[[2]] * random_vector[2]
reduced_spei[[3]] <- reduced_spei[[3]] * random_vector[3]
reduced_spei[[4]] <- reduced_spei[[4]] * random_vector[4]
reduced_spei[[5]] <- reduced_spei[[5]] * random_vector[5]
reduced_spei[[6]] <- reduced_spei[[6]] * random_vector[6]

# -TASK 4-
# Extract the grid points of the spei raster data
points <- as.points(reduced_spei)
points_sf <- st_as_sf(points)
r.extract <- extract(reduced_spei, points_sf)
r.extract

# or
points <- as.points(reduced_spei) %>% 
  st_as_sf() 
points # No need for a downgrade in resolution
extract <- extract(reduced_spei, points)
extract

# -TASK 5- 
#Extract the spei data for each polygon in shp and shp3
shp.points <- st_cast(shp, 'POLYGON')
shp3.points <- st_cast(shp3, 'POLYGON')

shp.points <- extract(r.spei,shp.points) 
shp3.points <- extract(r.spei,shp3.points) 
# Extract SPEI values for shp and shp3
spei_values_shp <- extract(r.spei, shp.points, fun = mean, na.rm = TRUE)
spei_values_shp3 <- extract(r.spei, shp3.points, fun = mean, na.rm = TRUE)

spei_2015 <- r.spei[[1435]]  
# or
spei_2015 <- r.spei[[1380]]

spei_2020 <- r.spei[[1440]]

# Extract 2015 SPEI for `shp` and `shp3`
spei_2015_shp <- extract(spei_2015, shp, fun = mean, na.rm = TRUE)
spei_2015_shp3 <- extract(spei_2015, shp3, fun = mean, na.rm = TRUE)

# Extract 2020 SPEI for `shp` and `shp3`
spei_2020_shp <- extract(spei_2020, shp, fun = mean, na.rm = TRUE)
spei_2020_shp3 <- extract(spei_2020, shp3, fun = mean, na.rm = TRUE)

# Combine the SPEI values for both shapefiles for 2015 and 2020
all_spei_2015 <- c(spei_2015_shp$mean, spei_2015_shp3$mean)
all_spei_2020 <- c(spei_2020_shp$mean, spei_2020_shp3$mean)

# Calculate the average SPEI for Serbia in 2015 and 2020
# Convert extracted values to numeric, if they are not already
all_spei_2015 <- as.numeric(c(spei_2015_shp$mean, spei_2015_shp3$mean))
all_spei_2020 <- as.numeric(c(spei_2020_shp$mean, spei_2020_shp3$mean))

avg_spei_2015 <- mean(all_spei_2015, na.rm = TRUE)
avg_spei_2020 <- mean(all_spei_2020, na.rm = TRUE)

# Print the results
cat("Average SPEI level in Serbia in 2015:", avg_spei_2015, "\n")
cat("Average SPEI level in Serbia in 2020:", avg_spei_2020, "\n")

# -TASK 6-
aqueduct <- "Aqueduct_baseline.shp"
aqueduct <- st_read(aqueduct)
