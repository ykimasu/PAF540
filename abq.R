# Load Libraries
library(dplyr)
library(sp)
library(sf)
library(mclust)
library(geojsonio)
#library(rgdal)

# Load the tidycensus package
library(tidycensus)
library(dplyr)
library(sf)

# Set your Census API key

census_api_key("56653f97c495e43affd518fba73c381505dc81d9")

# Retrieve 2010 median home value data from ACS
abq_2010_home_value <- get_acs(
  geography = "tract",  # Adjust for your required level (e.g., "county" or "state")
  variables = "B25077_001",  # Median Home Value in 2010 (ACS variable for median home value)
  year = 2010,
  state = "NM",  # State code for New Mexico
  county = "Bernalillo",  # Adjust to your county if necessary
  geometry = TRUE  # To include geographic data (important for mapping)
)

# View the data
head(abq_2010_home_value)

# Retrieve 2000 median home value data
abq_2000_home_value <- get_decennial(
  geography = "tract",  # Adjust as necessary
  variables = "H037001",  # Median Home Value for 2000 (Decennial Census variable for median home value)
  year = 2000,
  state = "NM",
  county = "Bernalillo",  # Adjust to your county if necessary
  geometry = TRUE
)

# View the 2000 data
head(abq_2000_home_value)

#  Extract non-spatial attributes
abq_2000_attr <- st_drop_geometry(abq_2000_home_value)
abq_2010_attr <- st_drop_geometry(abq_2010_home_value)

# Perform a left join (ensure that we keep all rows from the 2000 dataset)
merged_data <- dplyr::left_join(
  abq_2000_attr, 
  abq_2010_attr, 
  by = "GEOID",  # Merge by GEOID
  suffix = c("_2000", "_2010")  # Suffix for columns
)

# Check the number of rows in the merged data
nrow(merged_data)

# Extract the geometry (spatial data)
abq_2000_geom <- st_geometry(abq_2000_home_value)
abq_2010_geom <- st_geometry(abq_2010_home_value) 

# Step 6: Create the merged sf object
# Here we ensure we are using the geometry from the 2000 dataset since it contains the spatial data
merged_sf <- st_sf(merged_data, geometry = abq_2000_geom)

# Check the merged spatial data
head(merged_sf)

# Check the column names of the merged data
colnames(merged_sf)

# Calculate the change in median home values from 2000 to 2010
merged_sf$mhv_change <- merged_sf$estimate - merged_sf$value

# View the first few rows to verify the result
head(merged_sf)


# Load Albuquerque Dorling cartogram
github.url <- "https://github.com/DS4PS/cpp-529-fall-2020/raw/main/data/phx_dorling.geojson"
abq <- geojson_read(x = github.url, what = "sp")


# Reproject the map
abq <- spTransform(abq, CRS("+init=epsg:3395"))

# Convert to sf format
abq_sf <- st_as_sf(abq)

# Select relevant variables
keep.these <- c("pnhwht12", "pnhblk12", "phisp12", "pntv12", "pfb12", "polang12", 
                "phs12", "pcol12", "punemp12", "pflabf12", "pprof12", "pmanuf12", 
                "pvet12", "psemp12", "hinc12", "incpc12", "ppov12", "pown12", 
                "pvac12", "pmulti12", "mrent12", "mhmval12", "p30old12", "p10yrs12", 
                "p18und12", "p60up12", "p75up12", "pmar12", "pwds12", "pfhh12")

# Standardize the data
clustering_data <- abq_sf %>%
  st_drop_geometry() %>%
  select(all_of(keep.these)) %>%
  na.omit() %>%
  apply(2, scale)

# Perform clustering
set.seed(1234)
clustering_model <- Mclust(clustering_data, verbose = TRUE)

# Save model to avoid rerun
saveRDS(clustering_model, "clustering_model.rds")

# Add cluster classifications
abq_sf$cluster <- as.factor(clustering_model$classification)

# Add descriptive cluster labels
abq_sf$cluster_label <- NA
abq_sf$cluster_label[abq_sf$cluster == "1"] <- "Affluent, Predominantly White Neighborhoods"
abq_sf$cluster_label[abq_sf$cluster == "2"] <- "Diverse, Low-Income Communities"
abq_sf$cluster_label[abq_sf$cluster == "3"] <- "Working-Class Neighborhoods"
abq_sf$cluster_label[abq_sf$cluster == "4"] <- "Established, Wealthy Suburban Areas"
abq_sf$cluster_label[abq_sf$cluster == "5"] <- "Suburban Mixed Communities"
abq_sf$cluster_label[abq_sf$cluster == "6"] <- "Middle-Income Suburbs"
abq_sf$cluster_label[abq_sf$cluster == "7"] <- "Hispanic, Low-Income Communities"
abq_sf$cluster_label[abq_sf$cluster == "8"] <- "Middle-Class, Diverse Communities"
abq_sf$cluster_label <- as.factor(abq_sf$cluster_label)


# Check
str(abq_sf)

# Check

str(loaded_abq_sf)
head(loaded_abq_sf@data)  # View the attribute table

colnames(loaded_abq_sf@data)



