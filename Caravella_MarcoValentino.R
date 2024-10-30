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
library(tidyr)
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
rm(num_layers)

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

rm(random_vector)
# -TASK 4-
# Extract the grid points of the spei raster data
spei.points <- as.points(reduced_spei) %>% 
  st_as_sf() 
spei.points

rm(spei.points)
# -TASK 5- 
# Extract SPEI data for shp and shp3
spei_values_shp <- extract(reduced_spei, shp, fun = mean, na.rm = TRUE)
spei_values_shp3 <- extract(reduced_spei, shp3, fun = mean, na.rm = TRUE)

# Calculate the average SPEI for Serbia in 2015 and 2020 
years_seq <- 1435:1440
year_names <- 2015:2020
spei_matrix <- matrix(NA, nrow = 25, ncol = 12)
colnames(spei_matrix) <- c(paste0("avg_spei_", year_names, "_shp"), 
                           paste0("avg_spei_", year_names, "_shp3"))
for (i in seq_along(years_seq)) {
  field_name <- paste0("spei_", years_seq[i])  
  value_name_shp <- paste0("avg_spei_", year_names[i], "_shp")  
  value_name_shp3 <- paste0("avg_spei_", year_names[i], "_shp3")

    if (field_name %in% names(spei_values_shp) && field_name %in% names(spei_values_shp3)) {

    avg_shp <- mean(spei_values_shp[[field_name]], na.rm = TRUE)
    avg_shp3 <- mean(spei_values_shp3[[field_name]], na.rm = TRUE)
    
    spei_matrix[, paste0("avg_spei_", year_names[i], "_shp")] <- rep(avg_shp, 25)
    spei_matrix[, paste0("avg_spei_", year_names[i], "_shp3")] <- rep(avg_shp3, 25)
  } 
    else {
    warning(paste("Field", field_name, "does not exist in spei_values_shp"))
  }
}
avg_spei <- as.data.frame(spei_matrix)

# Print the results
cat("Average SPEI level in Serbia in 2015:", avg_spei$avg_spei_2015_shp[[1]], "\n")
cat("Average SPEI level in Serbia in 2020:", avg_spei$avg_spei_2020_shp[[1]], "\n")
cat("Average SPEI level in Serbian regions in 2015:", avg_spei$avg_spei_2015_shp3[[1]], "\n")
cat("Average SPEI level in Serbian regions in 2020:", avg_spei$avg_spei_2020_shp3[[1]], "\n")

rm(spei_values_shp, spei_values_shp3, year_names, years_seq, value_name_shp, value_name_shp3, spei_matrix)
rm(avg_shp, avg_shp3)

# -TASK 6-
aqueduct <- st_read("Aqueduct_baseline.shp")
aqueduct <- aqueduct %>% filter(gid_0 %in% "SRB")
print(aqueduct)
summary(aqueduct$bwd_raw) # We will consider this variable as the water stress
# level for 2015

# -TASK 7-
# Set seed for reproducibility
set.seed(954)

length(aqueduct$bws_raw)
water_stress_2015 <- aqueduct$bws_raw

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

rm(water_stress_ts, multipliers)

# -TASK 8- 
# To rasterize, we first need a "raster template"
r.template <- rast() %>% 
  crop(aqueduct)
r.template

# Rasterize using for loops
years <- 2015:2020
for (year in years) {
  field_name <- paste0("water_stress_", year)
  
  raster_var_name <- paste0("rast.water_stress_", year)
  
  assign(raster_var_name, rasterize(aqueduct, r.template, field = field_name))
}

# Extraction
years <- 2015:2020
num_years <- length(years)
matrix_shp <- matrix(NA, nrow = 25, ncol = num_years)
colnames(matrix_shp) <- paste0("avg_", years)

matrix_shp3 <- matrix(NA, nrow = 25, ncol = num_years)
colnames(matrix_shp3) <- paste0("avg_", years)

for (i in seq_along(years)) {
  year <- years[i]
  
  raster_name <- paste0("rast.water_stress_", year)
  water_stress_raster <- get(raster_name)
  
  shp_values <- extract(water_stress_raster, shp, fun = mean, na.rm = TRUE)[,2]
  shp3_values <- extract(water_stress_raster, shp3, fun = mean, na.rm = TRUE)[,2]
  
  matrix_shp[, i] <- rep(shp_values, 25)
  matrix_shp3[, i] <- rep(mean(shp3_values, na.rm = TRUE), 25)
}

water_stress_shp <- as.data.frame(matrix_shp)
water_stress_shp3 <- as.data.frame(matrix_shp3)

cat("Average water stress level in Serbia in 2020:", water_stress_shp$avg_2020[[1]], "\n")
cat("Average water stress level in NUTS 3 in 2020", water_stress_shp3$avg_2020[[1]], "\n")
# These two means are different because the resolution of aqueduct_2020_shp and that
# of aqueduct_2020_shp3 are different. Thus, the first average concerns the whole 
#country, while the second refers 25 regions of Serbia
rm(r.template, years, raster_var_name, field_name, water_stress_raster, matrix_shp, matrix_shp3, i, num_years, raster_name)
rm(rast.water_stress_2015, rast.water_stress_2016, rast.water_stress_2017, rast.water_stress_2018, rast.water_stress_2019, rast.water_stress_2020)

# -TASKdata:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAbElEQVR4Xs2RQQrAMAgEfZgf7W9LAguybljJpR3wEse5JOL3ZObDb4x1loDhHbBOFU6i2Ddnw2KNiXcdAXygJlwE8OFVBHDgKrLgSInN4WMe9iXiqIVsTMjH7z/GhNTEibOxQswcYIWYOR/zAjBJfiXh3jZ6AAAAAElFTkSuQmCC 9-
# Load population density data and crop it around the boundary box
population_density <- rast("gpw-v4-population-density-rev11_2015_15_min_asc/gpw_v4_population_density_rev11_2015_15_min.asc")
print(population_density)

population_density <- crop(population_density, box)
plot(population_density)

# Compute the average population density in Serbia
avg_population_density <- extract(population_density, shp, fun = mean, na.rm = TRUE)
cat("Average population density in Serbia in 2015:", avg_population_density$gpw_v4_population_density_rev11_2015_15_min, "\n")

# Now resample it on the resolution of SPEI data using the “bilinear” method
resample.avg_population_density <- resample(population_density, reduced_spei, method = "bilinear")
avg_resample.avg_population_density <- extract(resample.avg_population_density, shp, fun = mean, na.rm = TRUE)
cat("Average population density in Serbia in 2015 (resampled resolution):", avg_resample.avg_population_density$gpw_v4_population_density_rev11_2015_15_min, "\n")
# Serbian population density is changed only slightly. It is probably due to resolution adjustment
rm(resample.avg_population_density, avg_resample.avg_population_density)

# -TASK 10-

# -TASK 11-

# -TASK 12-
# Load "Serbian_gva_sector_a" xlsx 
Serbian_gva_sector_a <- read_xlsx("Serbian_gva_sector_a.xlsx", sheet = "Sheet2")
Serbian_gva_sector_a <- Serbian_gva_sector_a %>% select(TERRITORY_ID, LEVEL_ID, NAME_HTML, VERSIONS,
                                                        UNIT, SECTOR, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`)
Serbian_gva_sector_a <- Serbian_gva_sector_a %>% filter(Serbian_gva_sector_a$TERRITORY_ID != "RSZZZ" )

# Merge Serbian_gva_sector_a with shp3
Serbian_gva_sector_a <- Serbian_gva_sector_a %>%
  rename(NUTS_ID = TERRITORY_ID )
merged.Serbian_gva_sector_a <- shp3 %>% left_join(Serbian_gva_sector_a, by = shp$NUTS_ID)

# -TASK 13- I NEED TO MERGE ONLY SHP3 CORRELATED AVERAGES
avg_spei <- avg_spei %>% mutate(id = row_number())
shp3 <- shp3 %>% mutate(id = row_number())
water_stress_shp3 <- water_stress_shp3 %>% mutate(id = row_number())
Serbian_gva_sector_a <- Serbian_gva_sector_a %>% mutate(id = row_number())

final.sf <- Serbian_gva_sector_a %>% left_join(select(shp3, id, geometry), by = "id")
columns_to_merge <- c("avg_spei_2015_shp3", "avg_spei_2016_shp3", "avg_spei_2017_shp3", "avg_spei_2018_shp3", "avg_spei_2019_shp3",
                      "avg_spei_2020_shp3")
fina.sf <- final.sf %>% left_join(select(avg_spei, id, columns_to_merge), by = "id") %>% left_join(water_stress_shp3, by = "id")

# -TASK 14-
spei_columns <- c("avg_spei_2015_shp", "avg_spei_2016_shp", "avg_spei_2017_shp", 
                  "avg_spei_2018_shp", "avg_spei_2019_shp", "avg_spei_2020_shp")
water_stress_columns <- c("avg_2015", "avg_2016", 
                          "avg_2017", "avg_2018", 
                          "avg_2019", "avg_2020")
water_stress_shp <- water_stress_shp %>% mutate(id = row_number())

# Select and rename the columns for clarity
final_data <- avg_spei %>%
  select(id, all_of(spei_columns)) %>%  
  rename_with(~ gsub("avg_spei_", "SPEI_", .), starts_with("avg_spei")) %>%
  left_join(water_stress_shp %>% 
              select(id, all_of(water_stress_columns)) %>% 
              rename_with(~ gsub("avg_", "Water Stress ", .), starts_with("avg")),
            by = "id")

# Step 2: Reshape the Data
final_long <- final_data %>%
  pivot_longer(
    cols = -id,
    names_to = c(".value", "Year"),
    names_pattern = "(.*)_(\\d+)"
  )

# Convert Year to numeric
final_long$Year <- as.numeric(final_long$Year)

# Step 3: Create the Plots
# Plotting SPEI
ggplot(final_long, aes(x = Year, y = SPEI, group = id, color = id)) +
  geom_line() +
  labs(title = "Time Series of SPEI in Serbia (2015 - 2020)",
       x = "Year",
       y = "SPEI") +
  theme_minimal() +
  theme(legend.position = "none")

# Plotting Water Stress
ggplot(final_long, aes(x = Year, y = `Water Stress`, group = id, color = id)) +
  geom_line() +
  labs(title = "Time Series of Water Stress in Serbia (2015 - 2020)",
       x = "Year",
       y = "Water Stress Level") +
  theme_minimal() +
  theme(legend.position = "none")

# -TASK 15-
# Step 1: Prepare the Data
# Select the relevant columns for SPEI and water stress
spei_columns <- c("avg_spei_2015_shp3", "avg_spei_2016_shp3", "avg_spei_2017_shp3", 
                  "avg_spei_2018_shp3", "avg_spei_2019_shp3", "avg_spei_2020_shp3")
water_stress_columns <- c("avg_spei_2015_shp", "avg_spei_2016_shp", 
                          "avg_spei_2017_shp", "avg_spei_2018_shp", 
                          "avg_spei_2019_shp", "avg_spei_2020_shp")
Agricultural_GVA_columns <- c("2015", "2016", "2017", "2018", "2019", "2020")

# Select and rename the columns for clarity
final_data <- final.sf %>%
  select(id, contains("avg_spei"), contains("avg")) %>%
  rename_with(~ gsub("avg_spei_", "SPEI_", .), starts_with("avg_spei")) %>%
  rename_with(~ gsub("avg", "Water Stress", .), starts_with("avg"))


# Step 2: Reshape the Data
final_long <- final_data %>%
  pivot_longer(
    cols = -id,
    names_to = c(".value", "Year"),
    names_pattern = "(.*)_(\\d+)"
  )

# Convert Year to numeric
final_long$Year <- as.numeric(final_long$Year)

# Step 3: Create the Plots
# Plotting SPEI
ggplot(final_long, aes(x = Year, y = SPEI, group = id, color = id)) +
  geom_line() +
  labs(title = "Time Series of SPEI in Serbia (2015 - 2020)",
       x = "Year",
       y = "SPEI") +
  theme_minimal() +
  theme(legend.position = "none")

# Plotting Water Stress
ggplot(final_long, aes(x = Year, y = `Water Stress`, group = id, color = id)) +
  geom_line() +
  labs(title = "Time Series of Water Stress in Serbia (2015 - 2020)",
       x = "Year",
       y = "Water Stress Level") +
  theme_minimal() +
  theme(legend.position = "none")

# -TASK 16-
