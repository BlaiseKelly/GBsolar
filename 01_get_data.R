library(ecmwfr)
library(dplyr)
library(sf)

source(c("R/calc.R","R/create_dates.R","R/create_df_from_nc.R","R/get.R","R/plot.R"))

site = "Manchester"

dir.create(paste0("sites/",site,"/DAT/"),recursive = TRUE)
dir.create(paste0("sites/",site,"/MET/"))
dir.create(paste0("sites/",site,"/OUT/"))
dir.create(paste0("sites/",site,"/SUR/"))

# coords of rooftop
lat = 53.43994
lon = -2.28379

# create data frame of days for period of interest
date_df = create_dates_df(start_date = "2025-03-01", end_date = "2025-03-31", time_zone = "UTC", timestep = "day")

# paste ECMWF API here
api = ""

pt = st_sfc(
  st_point(c(lon,lat)),
  crs = 4326
) |> 
  st_transform(27700)

domain = pt |> 
  st_buffer(20)

# save for 02_calc_solar script - temporary
saveRDS(pt, paste0("sites/",site,"/DAT/site_location.RDS"))
saveRDS(date_df, paste0("sites/",site,"/DAT/date_range.RDS"))

# ensure domain is at least 1x1 grid cell or ecmwf returns error
sl_grid = snap_to_era5_grid(domain,"single_level")

# variables from single level
sl_varz = c("total_sky_direct_solar_radiation_at_surface")

# download ERA data single level - resolution 28km/0.25 degrees
get_era(ecmwf_format_bb = sl_grid, months = 3, years = 2025,
               variables = sl_varz, dataset = "single-level", api_key = api, path_out = paste0("sites/",site,"/MET/"))

# variables available from land 
land_varz = c("surface_solar_radiation_downwards", "2m_temperature")

# keep the same grid as single level which should be larger
# download land (resolution 9km/0.1deg)
get_era(ecmwf_format_bb = sl_grid, months = 3, years = 2025,
               variables = land_varz, dataset = "land", api_key = api, path_out = paste0("sites/",site,"/MET/"))

# brick the ncdfs
dir.create("meteo/bricks")

# for larger areas it can be neater to use the era data as raster bricks
rasterise_meteo(variables = land_varz,meteo_dir = paste0("sites/",site,"/MET/"))

rasterise_meteo(variables = sl_varz,meteo_dir = paste0("sites/",site,"/MET/"))

# get LIDAR - sometimes eng_composite returns a file path and sometimes a raster, not figured out yet
# the elevation data for the first return DSM
dsm_file <- eng_composite(domain, product = "dsm")
# import temporary rastrer
dsm = rast(dsm_file)
# write out for 02 script
terra::writeRaster(dsm,filename = paste0("sites/",site,"/SUR/dsm.TIF"))
