library(dplyr)
library(FedData)
library(sf)
library(terra)
library(tigris)

# 1. load the NLCD raster
nlcd = rast("NLCD_data/Annual_NLCD_LndCov_2023_CU_C1V0.tif")

# 2. Get map of counties for the contiguous US using `tigris`
options(tigris_use_cache = TRUE)

counties_sf = counties(cb = TRUE, resolution = "20m") %>%
  filter(!STATEFP %in% c("02", "15", "72")) %>%  # remove AK, HI, PR
  st_transform(5070) %>%                         # transform to Albers Equal Area (NLCD CRS)
  mutate(id = paste0(STATE_NAME, "-", NAMELSAD))  # Create unique ID

ids = counties_sf$id # make list of counties to loop over

# 3. Create custom function to parallelize over
extract_nlcd = function(county){
  
  # 3.1 Narrow geographic scope from counties_sf (needed for filter nlcd raster)
  county_sf = filter(counties_sf, id == county)   # select a county
  county_sf = st_transform(county_sf, crs(nlcd))  # re-project just in case
  
  # 3.2 'filter' nlcd raster for county area only
  selected_vect = vect(county_sf)                    # convert to SpatVector (must be a native terra-class for cropping/masking)
  nlcd_cropped  = crop(nlcd, selected_vect)          # crop raster for county boundary
  nlcd_masked   = mask(nlcd_cropped, selected_vect)  # mask cells outside of county as NAs
  
  # 3.3 convert polygon
  nlcd_polygon_vect = as.polygons(nlcd_masked) # convert
  nlcd_polygon = nl
}

# 3. pick one county 
selected_county = counties_sf[1,]
selected_county = st_transform(selected_county, crs(nlcd)) # reproject just in case


# 4. Convert to terra vector and crop/mask

selected_vect = vect(selected_county)
nlcd_cropped = crop(nlcd, selected_vect)
nlcd_masked = mask(nlcd_cropped, selected_vect)

nlcd_polygon_vect = as.polygons(nlcd_masked, dissolve = FALSE, aggregate = FALSE) # convert to polygon (spatvector), want ea cell as a sep polygon
val_col = names(nlcd_polygon_vect)[1] # get name of the value column
nlcd_polygon_vect = nlcd_polygon_vect[!is.na(nlcd_polygon_vect[[val_col]]), ] # remove NA areas

nlcd_polygon_sf = st_as_sf(nlcd_polygon_vect) %>%  # convert to sf polygon
  mutate(landcover = as.integer(.data[[val_col]])) # make landcover values integer (recommended)

# manually dissolve by landcover type
nlcd_dissolved_sf = nlcd_polygon_sf %>%
  group_by(landcover) %>%
  summarize(do_union = TRUE, .groups = "drop")

nlcd_classes <- data.frame(
  landcover = c(11, 12, 21, 22, 23, 24, 31, 41, 42, 43, 52, 71, 81, 82, 90, 95),
  class_name = c(
    "Open Water",
    "Perennial Ice/Snow",
    "Developed, Open Space",
    "Developed, Low Intensity",
    "Developed, Medium Intensity",
    "Developed, High Intensity",
    "Barren Land (Rock/Sand/Clay)",
    "Deciduous Forest",
    "Evergreen Forest",
    "Mixed Forest",
    "Shrub/Scrub",
    "Grassland/Herbaceous",
    "Pasture/Hay",
    "Cultivated Crops",
    "Woody Wetlands",
    "Emergent Herbaceous Wetlands"
  ),
  hex = c(
    "#466b9f",  # Open Water
    "#d1def8",  # Snow/Ice
    "#dec5c5",  # Developed, Open
    "#d99282",  # Developed, Low
    "#eb0000",  # Developed, Medium
    "#ab0000",  # Developed, High
    "#b3ac9f",  # Barren
    "#68ab5f",  # Deciduous Forest
    "#1c5f2c",  # Evergreen Forest
    "#b5ca8f",  # Mixed Forest
    "#a68c30",  # Shrub/Scrub
    "#ccba7c",  # Grassland/Herbaceous
    "#e3e3c2",  # Pasture/Hay
    "#caca78",  # Cultivated Crops
    "#64b3d5",  # Woody Wetlands
    "#eaf3e3"   # Emergent Herbaceous Wetlands
  ),
  stringsAsFactors = FALSE
)

# join the dissolved nlcd sf object with the associated color pallete
nlcd_dissolved_sf = nlcd_dissolved_sf %>%
  left_join(nlcd_classes, by = "landcover") %>%
  mutate(class_name = factor(class_name, levels = nlcd_classes$class_name))

plot(
  nlcd_dissolved_sf["class_name"],
  col = nlcd_dissolved_sf$hex,
  main = "NLCD Land Cover (2023)",
  axes = FALSE,
  key.pos = 1  # bottom legend
)

