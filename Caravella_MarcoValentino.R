#-------------------------------------TAKE-HOME EXAM--------------------------------------
# Caravella Marco Valentino 0001134954

path <- "C:/Users/user/Documents/Unibo/Economics and Econometrics/Second Year/First Semester/GIS Tools Laboratory/Caravella_MarcoValentino/additional_exam_data"
setwd(path)

# Load libraries
library(ggplot2)
library(sf)
library(spData)
library(spdep)
library(tidyverse) 
library(readxl)
library(dplyr)
library(tidyr)
library(viridis)
library(terra)
library(exactextractr)
#library(ncdfCF)
#library(ncdf4)
#library(reshape2)

# SECTION B: TASK REPLICATION 

# -TASK 1-
# Load the shapefile of countries and European NUTS 3 Regions
shp <- st_read("ne_10m_admin_0_countries.shp")
shp <- shp %>% filter(SU_A3 == "SRB") # Keep only Serbia
shp3 <- st_read("NUTS_RG_01M_2021_4326_LEVL_3_repaired.shp") 
shp3 <- shp3 %>% filter(CNTR_CODE %in% "RS") # Keep only Serbia

print(shp)
print(shp3)

# Create a boundary box containing the shapefile of Serbia with a small 
# margin (0.5 in all directions)
box <- st_bbox(shp)
box <- box+c(-.5,-.5,.5,.5)
box # These are the coordinates of the borders of the shapefile

# -TASK 2-
# Load SPEI data
r.spei <- rast("spei01.nc")
r.spei

# Crop "spei01" around the boxplot
r.spei <- crop(r.spei, box) 
r.spei # Note that the extent of raster data is slightly different from 
# that of the box plot. However, we expected that.
plot(r.spei)

# Keep only the last six layers of the raster
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
spei_shp <- extract(reduced_spei, shp, fun = mean, na.rm = TRUE)
spei_shp3 <- extract(reduced_spei, shp3, fun = mean, na.rm = TRUE)
colnames(spei_shp) <- c("id", "spei_2015", "spei_2016", "spei_2017", "spei_2018", "spei_2019", "spei_2020")
colnames(spei_shp3) <- c("id", "spei_2015", "spei_2016", "spei_2017", "spei_2018", "spei_2019", "spei_2020")

# Calculate the average SPEI for Serbia in 2015 and 2020
avg_spei_2015 <- mean(c(spei_shp$spei_2015, spei_shp3$spei_2015), na.rm = TRUE)
avg_spei_2020 <- mean(c(spei_shp$spei_2020, spei_shp3$spei_2020), na.rm = TRUE)

# Print the results
cat("Average SPEI level in Serbia in 2015:", avg_spei_2015, "\n")
cat("Average SPEI level in Serbia in 2020:", avg_spei_2020, "\n")

rm(avg_spei_2015, avg_spei_2020)

# -TASK 6-
# Load Aqueduct 4.0 shapefile
aqueduct <- st_read("Aqueduct_baseline.shp")
aqueduct <- aqueduct %>% filter(gid_0 %in% "SRB")

print(aqueduct)
summary(aqueduct$bwd_raw) # We will consider this variable as the water stress
# level for 2015

# -TASK 7-
# Set seed for reproducibility
set.seed(954)

# Create a vector with bws_raw observations
length(aqueduct$bws_raw)
water_stress_2015 <- aqueduct$bws_raw

# Add this vector to the Aqueduct shapefile
aqueduct$water_stress_2015 <- water_stress_2015

# Generate a matrix of random multipliers for 5 years (2016 to 2020) for 142 elements
multipliers <- matrix(rnorm(5 * 142, mean = 1, sd = 1), nrow = 142, ncol = 5)

# Calculate the annual values by multiplying the 2015 values by the random multipliers
water_stress_ts <- water_stress_2015 * multipliers

# Add those attributes to the Aqueduct shapefile
aqueduct$water_stress_2016 <- water_stress_ts[, 1] # Values for 2016
aqueduct$water_stress_2017 <- water_stress_ts[, 2] # Values for 2017
aqueduct$water_stress_2018 <- water_stress_ts[, 3] # Values for 2018
aqueduct$water_stress_2019 <- water_stress_ts[, 4] # Values for 2019
aqueduct$water_stress_2020 <- water_stress_ts[, 5] # Values for 2020

rm(water_stress_ts, multipliers, water_stress_2015)

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
num_years <- length(years)
matrix_shp <- matrix(NA, nrow = 1, ncol = num_years)
colnames(matrix_shp) <- paste0("avg_", years)
matrix_shp3 <- matrix(NA, nrow = 25, ncol = num_years)
colnames(matrix_shp3) <- paste0("avg_", years)

for (i in seq_along(years)) {
  year <- years[i]
  
  raster_name <- paste0("rast.water_stress_", year)
  water_stress_raster <- get(raster_name)
  
  shp_values <- exact_extract(water_stress_raster, shp, "mean")
  shp3_values <- exact_extract(water_stress_raster, shp3, "mean")
  
  matrix_shp[, i] <- rep(shp_values)
  matrix_shp3[, i] <- shp3_values
}
water_stress_shp <- as.data.frame(matrix_shp)
water_stress_shp3 <- as.data.frame(matrix_shp3)

# Compute the average water stress level in Serbia and in NUTS 3 Regions
avg_wtr_2020_shp <- mean(water_stress_shp$avg_2020)
avg_wtr_2020_shp3 <- mean(water_stress_shp3$avg_2020)

cat("Average water stress level in Serbia in 2020:", avg_wtr_2020_shp, "\n")
cat("Average water stress level in NUTS 3 in 2020", avg_wtr_2020_shp3, "\n")
# These two means are different because the resolution of aqueduct_2020_shp and that
# of aqueduct_2020_shp3 are different. Thus, the first average concerns the whole 
#country, while the second refers 25 regions of Serbia
rm(r.template, years, raster_var_name, field_name, water_stress_raster, matrix_shp, matrix_shp3, i, num_years, raster_name, year)
rm(shp_values, shp3_values, avg_wtr_2020_shp, avg_wtr_2020_shp3)

# -TASK 9-
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
# Serbian population density is changed only slightly. It is probably due to 
# resolution adjustment (the lower the resolution the higher the average)

rm(resample.avg_population_density, avg_resample.avg_population_density)
rm(r.spei)

# -FIRST NOT MANDATORY TASK-
# Calculate the average population density in Serbia
serbia_avg_pop_density <- avg_population_density$gpw_v4_population_density_rev11_2015_15_min

# Divide each grid point's population density by the average population density of Serbia
pop_density_ratio <- population_density / serbia_avg_pop_density

# Some checkings
print(pop_density_ratio)
print(reduced_spei)

# Resample SPEI to match population density resolution
reduced_spei <- resample(reduced_spei, pop_density_ratio, method = "bilinear")

# Change the projections of SPEI
pop_weighted_spei <- reduced_spei

for (i in 1:nlyr(reduced_spei)) {
  pop_weighted_spei[[i]] <- reduced_spei[[i]] * pop_density_ratio
}

# Some checks
print(pop_density_ratio)
print(rast.water_stress_2015)

# We now perform the same steps as before for the Water Stress index
# Resample SPEI to match population density resolution
rast.water_stress_2015 <- resample(rast.water_stress_2015, pop_density_ratio, method = "bilinear")
rast.water_stress_2016 <- resample(rast.water_stress_2016, pop_density_ratio, method = "bilinear")
rast.water_stress_2017 <- resample(rast.water_stress_2017, pop_density_ratio, method = "bilinear")
rast.water_stress_2018 <- resample(rast.water_stress_2018, pop_density_ratio, method = "bilinear")
rast.water_stress_2019 <- resample(rast.water_stress_2019, pop_density_ratio, method = "bilinear")
rast.water_stress_2020 <- resample(rast.water_stress_2020, pop_density_ratio, method = "bilinear")

# Rasterized water stress data for each year was created earlier
pop_weighted_water_stress <- water_stress_raster
years <- 2015:2020

for (year in years) {

    water_stress_raster <- get(paste0("rast.water_stress_", year))
  
  # Multiply the raster by the population density ratio
  pop_weighted_water_stress[[as.character(year)]] <- water_stress_raster * pop_density_ratio
}

rm(rast.water_stress_2015, rast.water_stress_2016, rast.water_stress_2017, rast.water_stress_2018, rast.water_stress_2019, rast.water_stress_2020)
rm(serbia_avg_pop_density, years, years, i, population_density, pop_weighted_spei, pop_weighted_water_stress, pop_density_ratio, avg_population_density)

# -TASK 10-
# Variables like GDP may be affected by population-weighted climate variables. That is,
# high populated areas are those with higher GDP, usually. Think about big cities
# like New York, and London, they are highly density populated and very rich. But what
# about their relevance in investigating Agricultural GVA or Agricultural yields?
# Let's analyse them separately:
# - Agricultural GVA and yields usually depends on soil fertility, land use, climate, etc.; but
# not on population-weighted climate variables. Thus, population-weighted climate variables
# are less relevant for Agricultural GVA and Yields.
# - Industrial GVA may be affected by population-weighted climate variables, but this
# depends on the nature of the industrial activity and on its location. That is,
# if industries are located in highly populated urban areas, then population-weighted
# climate variables may do explain some variability. Whereas, if industries are located
# in low populated areas and perform activity such as mining, then population weighting 
# may cover the real relationship between climate and industrial output. 

# -TASK 11-
# In this case it is preferable to reduce the SPEI resolution to that of Density Population, 
# because by doing so we preserve the more detailed information contained in Densirt Population
# data. Otherwise, we would lose much of the information insights in (0.25X0.25) resolution.
# Of course, by doing the opposite we would dilute the information about population density.
# Moreover, a pop-weighted SPEI index would be more precise if we reduce SPEI resolution. In
# this way, the more accurate population distribution would improve the accuracy of 
# the index, making it more representative.

# -TASK 12-
# Load "Serbian_gva_sector_a" xlsx, keep only data from 2015 to 2020, and drop RSZZZ
Serbian_gva_sector_a <- read_xlsx("Serbian_gva_sector_a.xlsx", sheet = "Sheet2")
Serbian_gva_sector_a <- Serbian_gva_sector_a %>% select(TERRITORY_ID, LEVEL_ID, NAME_HTML, VERSIONS,
                                                        UNIT, SECTOR, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`)
Serbian_gva_sector_a <- Serbian_gva_sector_a %>% filter(Serbian_gva_sector_a$TERRITORY_ID != "RSZZZ" )
Serbian_gva_sector_a <- Serbian_gva_sector_a %>%
  rename_with(~ paste0("agr_GVA_", .), .cols = c("2015", "2016", "2017", "2018", "2019", "2020"))

# Merge Serbian_gva_sector_a with shp3
Serbian_gva_sector_a <- Serbian_gva_sector_a %>%
  rename(NUTS_ID = TERRITORY_ID )
merged.Serbian_gva_sector_a <- shp3 %>% left_join(Serbian_gva_sector_a, by = shp$NUTS_ID)

rm(merged.Serbian_gva_sector_a)

# -TASK 13- 
# Create an id index used to merge data
spei_shp3 <- spei_shp3 %>% mutate(id = row_number())
shp3 <- shp3 %>% mutate(id = row_number())
water_stress_shp3 <- water_stress_shp3 %>% mutate(id = row_number())
Serbian_gva_sector_a <- Serbian_gva_sector_a %>% mutate(id = row_number())

# Generate the new simple feature by merging previous data
final_sf <- Serbian_gva_sector_a %>% left_join(select(shp3, id, geometry), by = "id")
columns_to_merge <- c("spei_2015", "spei_2016", "spei_2017", "spei_2018", "spei_2019",
                      "spei_2020")
final_sf <- final_sf %>% left_join(select(spei_shp3, id, columns_to_merge), by = "id") %>% left_join(water_stress_shp3, by = "id")
final_sf <- final_sf %>% rename_with(~ gsub("avg_", "Water_Stress_", .), starts_with("avg"))

rm(agr_GVA_columns, columns_to_merge)

# -TASK 14-
# Bind SPEI and Water Stress into one data frame
final_data <- cbind(spei_shp, water_stress_shp) %>%
  rename_with(~ gsub("spei_", "SPEI_", .), starts_with("spei")) %>%
  rename_with(~ gsub("avg_", "Water_Stress_", .), starts_with("avg"))


# Reshape the data to long format with separate columns for SPEI and Water Stress
final_long <- final_data %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Year"),
    names_pattern = "(SPEI|Water_Stress)_(\\d+)",
    values_to = "Value"
  ) %>%
  mutate(Year = as.integer(Year))

# Convert Year to numeric
final_long$Year <- as.numeric(final_long$Year)

# Remove NA or problematic values
final_long <- final_long %>%
  filter(!is.na(Variable))

# Plot
ggplot(final_long, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(linewidth = 1.2) +  # Use linewidth
  geom_point(size = 3) +        # Points remain the same
  scale_color_viridis_d(name = "Variable") +
  labs(
    title = "Time Series of SPEI and Water Stress in Serbia (2015–2020)",
    x = "Year",
    y = "Average Value"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom"
  )

rm(final_data, final_long)

# -TASK 15-
# Select the relevant columns for SPEI and water stress
spei_columns <- c("spei_2015", "spei_2016", "spei_2017", 
                  "spei_2018", "spei_2019", "spei_2020")
water_stress_columns <- c("Water_Stress_2015","Water_Stress_2016","Water_Stress_2017",
                          "Water_Stress_2018","Water_Stress_2019","Water_Stress_2020")
agr_GVA_columns <- c("agr_GVA_2015","agr_GVA_2016","agr_GVA_2017",
                     "agr_GVA_2018","agr_GVA_2019","agr_GVA_2020")

# Bind SPEI and Water Stress into one data frame
final_data <- final_sf %>%
  select(id, all_of(spei_columns)) %>%  
  left_join(
    final_sf %>% 
      select(id, all_of(water_stress_columns))) %>% 
  left_join(final_sf %>% select(id, all_of(agr_GVA_columns))) 

# Summarise using the updated `across` syntax
final_data <- final_data %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))

# Reshape the data to long format with separate columns for SPEI, Water Stress, and agr_GVA
final_long <- final_data %>%
  pivot_longer(
    cols = -id,
    names_to = c("Variable", "Year"),
    names_pattern = "(spei|Water_Stress|agr_GVA)_(\\d+)",
    values_to = "Value"
  ) %>%
  mutate(Year = as.integer(Year))

final_long <- final_long %>%
  filter(!is.na(Variable))

# Plotting SPEI
# Filter the final_long dataframe to keep only rows where Variable is "spei"
final_long_spei <- final_long %>%
  filter(Variable == "spei")

# Create the plot
ggplot(final_long_spei, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(linewidth = 1.2) +  
  geom_point(size = 3) +        
  scale_color_viridis_d(name = "Variable") +
  labs(
    title = "Time Series of SPEI in Serbian NUTS 3 Regions (2015–2020)",
    x = "Year",
    y = "Average Value"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom"
  )


# Plotting Water Stress
final_long_water_stress <- final_long %>%
  filter(Variable == "Water_Stress")

# Create the plot
ggplot(final_long_water_stress, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(linewidth = 1.2) +  
  geom_point(size = 3) +        
  scale_color_manual(
    name = "Variable",
    values = "red") +
  labs(
    title = "Time Series of Water Stress in Serbian NUTS 3 Regions (2015–2020)",
    x = "Year",
    y = "Average Value"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom"
  )


# Plotting agricultural GVA
final_long_agr_GVA <- final_long %>%
  filter(Variable == "agr_GVA")

# Create the plot
ggplot(final_long_agr_GVA, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(linewidth = 1.2) +  # Use linewidth
  geom_point(size = 3) +        # Points remain the same
  scale_color_manual(
    name = "Variable",
    values = "black") +
  labs(
    title = "Time Series of agricultural GVA in Serbia NUTS 3 Regions (2015–2020)",
    x = "Year",
    y = "Average Value"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom"
  )

rm(final_data, final_long, final_long_agr_GVA, final_long_spei, final_long_water_stress)
rm(agr_GVA_columns, spei_columns, water_stress_columns)

# -TASK 16-
# Assume that:
final_sf$speigr <- (spei_shp3$spei_2020 - spei_shp3$spei_2015) / spei_shp3$spei_2015
final_sf <- st_as_sf(final_sf)
final_sf <- st_transform(final_sf, 4326)
st_crs(final_sf)

# Plot the results 
ggplot() +
  geom_sf(data = final_sf, aes(fill = speigr), color = "black") +
  scale_fill_viridis_c(option = "D", name = "SPEI growth rate") +
  labs(title = "The growth rate of SPEI from 2015 to 2020 of each Serbian NUTS 3 region") +
  theme_minimal() +
  theme(legend.position = "right")

# -TASK 17-
# Assume
final_sf$wsgr <- (water_stress_shp3$avg_2020 - water_stress_shp3$avg_2015) / water_stress_shp3$avg_2015 

# Plot the results
ggplot(data = st_transform(final_sf, crs = 32634)) +
  geom_sf(aes(fill = wsgr), color = "black") +
  scale_fill_viridis_c(option = "G", name = "Water stress index growth rate") +
  labs(title = "The growth rate of the water stress index from 2015 to 2020 of each Serbian NUTS 3 region") +
  theme_minimal() +
  theme(legend.position = "right")

# -TASK 18-
# Assume
final_sf$agrgvagr <- (final_sf$agr_GVA_2020 - final_sf$agr_GVA_2015) / final_sf$agr_GVA_2015

# Plot the results
ggplot(data = st_transform(final_sf, crs = 3035)) +
  geom_sf(aes(fill = agrgvagr), color = "black") +
  scale_fill_viridis_c(option = "D", name = "Agricultural GVA growth rate") +
  labs(title = "The growth rate of the Agricultural GVA from 2015 to 2020 of each Serbian NUTS 3 region") +
  theme_minimal() +
  theme(legend.position = "right")

# Plot the map again using blue for regions experiencing a negative growth,
# lightblue for those having a growth rate between 0% and 5%, and white for 
# regions with a growth rate >5%. 
ggplot(data = st_transform(final_sf, crs = 3035)) +
  geom_sf(aes(fill = cut(agrgvagr, breaks = c(-Inf, 0, 0.05, Inf), labels = c("Negative growth", "0 - 0.05", "> 0.05"))), color = "black") +
  scale_fill_manual(
    values = c("Negative growth" = "blue", "0 - 0.05" = "lightblue", "> 0.05" = "white"),
    name = "Agricultural GVA growth rate"
  ) +
  labs(title = "The growth rate of the Agricultural GVA from 2015 to 2020 of each Serbian NUTS 3 region")

# -TASK 19-
# Yes, GDP per capita in one region could potentially be affected by the lagged 
# investment in a neighboring region. This could happen due to several economic 
# mechanisms such as regional economic spillovers, trade, or investment flows 
# between regions. If a neighboring region increases its investment, it could 
# lead to higher economic activity, which may indirectly benefit the neighboring 
# region, either through improved infrastructure, increased trade opportunities, 
# or enhanced regional integration. To check for this effect statistical tests, 
# life Moron's I test, may be performed.

# -SECOND NOT MANDATORY TASK-
# Compute Moran's I Test for SPEI 2015 and create a spatial autocorrelation
# scatterplot for it
final_sf <- st_make_valid(final_sf)
spei_2015 <- final_sf$spei_2015

# Create spatial weights using the "queen" criterion
k <- poly2nb(final_sf, queen = TRUE)
k_weights <- nb2listw(k, style = "W")

print(summary(k))

# Create centroids
coords <- st_coordinates(st_centroid(st_geometry(final_sf)))
p <- plot(k, coords)

# Create Spatial Lags variable
names(final_sf)[15] <- "index_1" # Spei_2015 is now "index"
index_lag <- lag.listw(k_weights, final_sf$index_1)

# Moran’s I for SPEI 2015
moran_spei <- moran.test(spei_2015, k_weights)
print(moran_spei)

# Precompute scaled values
final_sf <- final_sf %>%
  mutate(index_scaled = scale(index_1),
         index_lag_scaled = scale(index_lag))

# Compute mean values for horizontal and vertical lines
mean_index_scaled <- mean(final_sf$index_scaled, na.rm = TRUE)
mean_index_lag_scaled <- mean(final_sf$index_lag_scaled, na.rm = TRUE)

# Create the plot
ggplot1 <- ggplot(final_sf) +
  geom_point(aes(x = index_scaled, y = index_lag_scaled, fill = NAME_HTML), shape = 22, size = 3, alpha = 0.8) +
  geom_hline(yintercept = mean_index_lag_scaled, linetype = "dashed") +
  geom_vline(xintercept = mean_index_scaled, linetype = "dashed") +
  geom_smooth(aes(x = index_scaled, y = index_lag_scaled), method = "lm", color = "black") +
  labs(x = "Index (scaled)", y = "Lagged Index (scaled)", fill = "City") +
  theme_classic()
ggplot1

rm(moran_spei, mean_index_lag_scaled, mean_index_scaled, p, spei_2015, index_lag)

# Compute Moran's I Test for Agricultural GVA in 2015 and create a spatial autocorrelation
# scatterplot for it
agr_gva_2015 <- final_sf$agr_GVA_2015

# Create Spatial Lags variable
names(final_sf)[7] <- "index_2" # Spei_2015 is now "index"
index_lag <- lag.listw(k_weights, final_sf$index_2)

# Moran’s I for Agricultural GVA 2015
moran_gva <- moran.test(agr_gva_2015, k_weights)
print(moran_gva)

# Precompute scaled values
final_sf <- final_sf %>%
  mutate(index_scaled = scale(index_2),
         index_lag_scaled = scale(index_lag))

# Compute mean values for horizontal and vertical lines
mean_index_scaled <- mean(final_sf$index_scaled, na.rm = TRUE)
mean_index_lag_scaled <- mean(final_sf$index_lag_scaled, na.rm = TRUE)

# Create the plot
ggplot_2 <- ggplot(final_sf) +
  geom_point(aes(x = index_scaled, y = index_lag_scaled, fill = NAME_HTML), shape = 22, size = 3, alpha = 0.8) +
  geom_hline(yintercept = mean_index_lag_scaled, linetype = "dashed") +
  geom_vline(xintercept = mean_index_scaled, linetype = "dashed") +
  geom_smooth(aes(x = index_scaled, y = index_lag_scaled), method = "lm", color = "black") +
  labs(x = "Index (scaled)", y = "Lagged Index (scaled)", fill = "City") +
  theme_classic()
ggplot_2

rm(k, k_weights, moran_gva, agr_gva_2015, index_lag, mean_index_scaled, mean_index_lag_scaled, coords)
# No, the Moran’s I Test cannot directly test the effect of a lagged variable on 
# the dependent variable in a neighboring region. Moran’s I measures spatial 
# autocorrelation, indicating whether a variable's values are clustered or dispersed 
# across space. It does not account for temporal dependencies or causal relationships.

# -THIRD NOT MANDATORY TASK-
gdb_path <- "C:/Users/user/Documents/Unibo/Economics and Econometrics/Second Year/First Semester/GIS Tools Laboratory/Caravella_MarcoValentino/additional_exam_data/Aqueduct40_waterrisk_download_Y2023M07D05/GDB/Aq40_Y2023D07M05.gdb"

# List all layers in the .gdb file
layers <- st_layers(gdb_path)
print(layers)

aqueduct <- st_read(gdb_path, layer = "baseline_annual")

# Check the structure of the data
head(aqueduct)

# Filter the data for Serbia
serbia <- aqueduct %>%
  filter(gid_0 == "SRB") %>%   # Adjust if needed
  drop_na()

# Crop aqueduct around Serbia
serbia <- st_intersection(serbia, shp) 

# Save as a shapefile
output_path <- "serbia_aqueduct.shp"
st_write(serbia, output_path)

cat("Shapefile saved to:", output_path)
