library(dplyr)
# library(FedData)
# library(parallel)
library(sf)
library(terra)
library(tigris) # tigris loads maps as actual sf objects which is needed for get_nlcd

# 1. specify file path to be loaded within the custom fxn (necessary for nodes to work properly)
nlcd_filepath = "NLCD_data/NLCD_mosquitoes_11_90_95.tif"
tmp = rast(nlcd_filepath)

# 2. Get map of counties for the contiguous US using `tigris`
options(tigris_use_cache = TRUE)

counties_sf = counties(cb = TRUE, resolution = "20m") %>%
  filter(!STATEFP %in% c("02", "15", "72")) %>%  # remove AK, HI, PR
  st_transform(crs(tmp)) %>%                         # transform to Albers Equal Area (NLCD CRS)
  mutate(id = paste0(STATE_NAME, "-", NAMELSAD)) # Create unique ID
rm(tmp)

# 3. Create custom function to parallelize over
extract_nlcd = function(county_id){
  tryCatch({ # ensures errors in one instance don't stop all instances
    nlcd = rast(nlcd_filepath)
    # 3.1 Narrow geographic scope from counties_sf (needed for filter nlcd raster)
    county_sf = counties_sf[counties_sf$id==county_id,]   # select a county
    
    # 3.2 'filter' nlcd raster for county area only
    selected_vect = simplifyGeom(vect(county_sf))      # simplify then convert to SpatVector (must be a native terra-class for cropping/masking)
    nlcd_cropped  = crop(nlcd, selected_vect)          # crop raster for county boundary
    nlcd_masked   = mask(nlcd_cropped, selected_vect)  # mask cells outside of county as NAs
    nlcd_trim     = trim(nlcd_masked)                  # remove NAs
    # 3.3 Convert to polygon (SpatVector of polygons is terra-native class)
    nlcd_polygon_vect = as.polygons(nlcd_trim, dissolve = TRUE, aggregate = TRUE) 
    
    return(wrap(nlcd_polygon_vect))
  }, error = function(e){
    return(NULL)
  }
  )
}

# 4. Run in parallel - apply to all counties in L48
ids = as.list(counties_sf$id)      # make list of counties to parallelize over


###############################################################################
#                    ALTERNATIVE PARALLELIZATION APPROACH
#------------------------------------------------------------------------------
library(doParallel)
library(foreach)
library(doSNOW) # needed for progress bar
num_cores = detectCores() - 2       # use 5 less avoids memory issues

cl = makeCluster(num_cores)         # initiate cluster environment
registerDoSNOW(cl)
pb = txtProgressBar(max = length(ids), style = 3)
progress = function(n) setTxtProgressBar(pb,n)
opts = list(progress = progress)

results = foreach(
  i = 1:length(ids),
  .options.snow = opts,
  .packages = c("terra", "sf")#, 
  # .export = c("nlcd_filepath","ids","counties_sf", "extract_nlcd")
) %dopar% {
  extract_nlcd(ids[[i]])
}
close(pb)
stopCluster(cl)
###############################################################################

# 5. Clean and Combine output
results = results[lengths(results)!=0]      # some list elements are NULL which need removal
results = lapply(results, unwrap)           # need to unwrap (byprod issue from parallelization)
combined_results <- do.call(rbind, results) # do.call executes fxn on ea elem inside results (efficiently ofc)

writeVector(combined_results, "gisele_nlcd/NLCD_data/WAMA.shp")
