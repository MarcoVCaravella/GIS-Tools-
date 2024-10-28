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
library(ncdfCF)
library(ncdf4)
library(reshape2)

# SECTION B: TASK REPLICATION 

# -TASK 1-
# Load data
shp <- st_read("ne_10m_admin_0_countries.shp")
shp <- shp %>% filter(SU_A3 == "SRB")
shp3 <- st_read("NUTS_RG_01M_2021_4326_LEVL_3_repaired.shp") # Load he shapefile of European NUTS 3 regions
shp3 <- shp3 %>% filter(CNTR_CODE %in% "RS") # Keep only Serbia

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
spei.points <- as.points(reduced_spei) %>% 
  st_as_sf() 
spei.points

# -TASK 5- 
# Extract SPEI data for shp and shp3
spei_values_shp <- extract(r.spei, shp, fun = mean, na.rm = TRUE)
spei_values_shp3 <- extract(r.spei, shp3, fun = mean, na.rm = TRUE)

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

# Calculate the average SPEI for Serbia in 2015 and 2020
avg_spei_2015 <- mean(c(spei_2015_shp$spei_1435, spei_2015_shp3$spei_1435), na.rm = TRUE)
avg_spei_2020 <- mean(c(spei_2020_shp$spei_1440, spei_2020_shp3$spei_1440), na.rm = TRUE)

# Print the results
cat("Average SPEI level in Serbia in 2015:", avg_spei_2015, "\n")
cat("Average SPEI level in Serbia in 2020:", avg_spei_2020, "\n")

# -TASK 6-
aqueduct <- st_read("Aqueduct_baseline.shp")
aqueduct <- aqueduct %>% filter(gid_0 %in% "SRB")
print(aqueduct)
summary(aqueduct$bwd_raw)

# -TASK 7-
# Set seed for reproducibility
set.seed(954)

# Depending on your choices, the number of features may vary
# Assuming aqueduct_shapefile$bws_raw has got 99 elements
length(aqueduct$bws_raw) # 99
water_stress_2015 <- aqueduct$bws_raw # A vector of length 99

# Add this vector to the Aqueduct shapefile
aqueduct$water_stress_2015 <- water_stress_2015

# Generate a matrix of random multipliers for 5 years (2016 to 2020) for 99 elements
multipliers <- matrix(rnorm(5 * 142, mean = 1, sd = 1), nrow = 142, ncol = 5)

# Calculate the annual values by multiplying the 2015 values by the random multipliers
water_stress_ts <- water_stress_2015 * multipliers

# Add those attributes to the Aqueduct shapefile
aqueduct$water_stress_2016 <- water_stress_ts[, 1] # Values for 2016
aqueduct$water_stress_2017 <- water_stress_ts[, 2] # Values for 2017
aqueduct$water_stress_2018 <- water_stress_ts[, 3] # Values for 2018
aqueduct$water_stress_2019 <- water_stress_ts[, 4] # Values for 2019
aqueduct$water_stress_2020 <- water_stress_ts[, 5] # Values for 2020

# -TASK 8- 
# To rasterize, we first need a "raster template"
r.template <- rast() %>% 
  crop(aqueduct)
r.template

# Create a raster for all five years
rast.water_stress_2015 <- rasterize(aqueduct, r.template)
print(aqueduct.rast)
plot(aqueduct.rast)

# Extraction
aqueduct_2015_shp <- extract(rast.water_stress_2015, shp, fun = mean, na.rm = TRUE)
aqueduct_2015_shp3 <- extract(rast.water_stress_2015, shp3, fun = mean, na.rm = TRUE)

# Compute the average
aqueduct_2015_shp <- as.numeric(aqueduct_2015_shp)
aqueduct_2015_shp3 <- unlist(aqueduct_2015_shp3)
aqueduct_2015_shp3 <- as.numeric(aqueduct_2015_shp3)
avg_aqueduct_2015_shp <- mean(aqueduct_2015_shp, na.rm = TRUE)
avg_aqueduct_2015_shp3 <- mean(aqueduct_2015_shp3, na.rm = TRUE)


cat("Average water stress level in Serbia in 2015:", avg_aqueduct_2015_shp, "\n")
cat("Average water stress level in NUTS 3 in 2015", avg_aqueduct_2015_shp3, "\n")
# They are of course different since the mean of in shp tells us only that there 
# is a water stress level. By increasing the resolution, that is by considering shp3
# we are able to see how water stress level changes across 25 Serbian regions

# -TASK 9-
# Load population density data and crop it around the boundary box
population_density <- rast("gpw-v4-population-density-rev11_2015_15_min_asc/gpw_v4_population_density_rev11_2015_15_min.asc")
print(population_density)

population_density <- crop(population_density, box)
plot(population_density)

# Compute the average population density in Serbia
avg_population_density <- mean(population_density, na.rm = TRUE)
mean_value <- global(population_density, mean, na.rm = TRUE)
mean_value
avg_population_density <- extract(population_density, shp.points, fun = mean, na.rm = TRUE)
print(avg_population_density)

# Now resample it on the resolution of SPEI data using the “bilinear” method
resample.population_density <- resample(population_density, reduced_spei, method = "bilinear")
avg_rsmpl.population_density <- extract(resample.population_density, shp.points, fun = mean, na.rm = TRUE)
avg_rsmpl.population_density
