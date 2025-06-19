library(terra)
library(tigris)

################################################################################
###### Below does not work due to corrupted web coverage at mrlc ###############
################################################################################
# reconsider when source is known to be fixed
options(tigris_use_cache = TRUE)

# Get contiguous U.S. states
states <- states(cb = TRUE)  # cb = cartographic boundary (simplified)

# Remove Alaska, Hawaii, Puerto Rico
conus <- states[!states$STUSPS %in% c("AK", "HI", "PR"), ]

# Example: get NLCD for CONUS extent
nlcd_data <- get_nlcd(
  template = conus,
  label = "CONUS",
  year = 2019,
  dataset = "landcover"
)
################################################################################

# Load the NLCD raster (manually downloaded via web browser)
nlcd = rast("NLCD_data/Annual_NLCD_LndCov_2023_CU_C1V0.tif")

# creates mask for the target land cover types (codes T or F)
mosq_areas = nlcd %in% c(11, 90, 95)

# apply the mask to filter the raster
nlcd_mask = mask(nlcd, mosq_areas, maskvalue = FALSE)
plot(nlcd_mask)

# save filtered NLCD for L48 to keep only open water (11), woody wetlands (90), and emergent herbaceous wetlands (95)
writeRaster(nlcd_mask, "NLCD_data/NLCD_mosquitoes_11_90_95.tif", overwrite = TRUE)

