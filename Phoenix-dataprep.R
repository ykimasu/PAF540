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

census_api_key("d1900aa09bd0ab195cedda8e76532282be5d1d9b", install=TRUE, overwrite=TRUE)

# Retrieve 2010 median home value data from ACS
phx_2010_home_value <- get_acs(
  geography = "tract",  # Adjust for your required level (e.g., "county" or "state")
  variables = "B25077_001",  # Median Home Value in 2010 (ACS variable for median home value)
  year = 2010,
  state = "AZ",  # State code for New Mexico
  county = "Maricopa",  # Adjust to your county if necessary
  geometry = TRUE  # To include geographic data (important for mapping)
)

# View the data
head(phx_2010_home_value)

# Retrieve 2000 median home value data
phx_2020_home_value <- get_acs(
  geography = "tract",  # Adjust as necessary
  variables = "B25077_001",  # Median Home Value for 2000 (Decennial Census variable for median home value)
  year = 2020,
  state = "AZ",
  county = "Maricopa",  # Adjust to your county if necessary
  geometry = TRUE
)

# View the 2000 data
head(phx_2020_home_value)


#  Extract non-spatial attributes
phx_2010_attr <- st_drop_geometry(phx_2010_home_value)
phx_2020_attr <- st_drop_geometry(phx_2020_home_value)

# Perform a left join (ensure that we keep all rows from the 2000 dataset)
merged_data <- dplyr::left_join(
  phx_2010_attr, 
  phx_2020_attr, 
  by = "GEOID",  # Merge by GEOID
  suffix = c("_2010", "_2020")  # Suffix for columns
)

# Check the number of rows in the merged data
nrow(merged_data)

# Extract the geometry (spatial data)
phx_2010_geom <- st_geometry(phx_2010_home_value)
phx_2020_geom <- st_geometry(phx_2020_home_value) 

# Step 6: Create the merged sf object
# Here we ensure we are using the geometry from the 2000 dataset since it contains the spatial data
merged_sf <- st_sf(merged_data, geometry = phx_2010_geom)

# Check the merged spatial data
head(merged_sf)

# Check the column names of the merged data
colnames(merged_sf)

# Calculate the change in median home values from 2000 to 2010
merged_sf$mhv_change <- merged_sf$estimate_2020 - merged_sf$estimate_2010

# View the first few rows to verify the result
head(merged_sf)


# Load Albuquerque Dorling cartogram
github.url <- "https://github.com/DS4PS/cpp-529-fall-2020/raw/main/data/phx_dorling.geojson"
phx <- geojson_read(x = github.url, what = "sp")


# Reproject the map
phx <- spTransform(phx, CRS("+init=epsg:3395"))

# Convert to sf format
phx_sf <- st_as_sf(phx)

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

colnames(phx_sf)

# Perform clustering
set.seed(1234)
clustering_model <- Mclust(clustering_data, verbose = TRUE)

# Save model to avoid rerun
saveRDS(clustering_model, "clustering_model.rds")

# Add cluster classifications
phx_sf$cluster <- as.factor(clustering_model$classification)

# Add descriptive cluster labels
phx_sf$cluster_label <- NA
phx_sf$cluster_label[phx_sf$cluster == "1"] <- "Affluent, Predominantly White Neighborhoods"
phx_sf$cluster_label[phx_sf$cluster == "2"] <- "Diverse, Low-Income Communities"
phx_sf$cluster_label[phx_sf$cluster == "3"] <- "Working-Class Neighborhoods"
phx_sf$cluster_label[phx_sf$cluster == "4"] <- "Established, Wealthy Suburban Areas"
phx_sf$cluster_label[phx_sf$cluster == "5"] <- "Suburban Mixed Communities"
phx_sf$cluster_label[phx_sf$cluster == "6"] <- "Middle-Income Suburbs"
phx_sf$cluster_label[phx_sf$cluster == "7"] <- "Hispanic, Low-Income Communities"
phx_sf$cluster_label[phx_sf$cluster == "8"] <- "Middle-Class, Diverse Communities"
phx_sf$cluster_label <- as.factor(phx_sf$cluster_label)


# Check
str(phx_sf)


