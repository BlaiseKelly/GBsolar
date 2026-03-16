library(sf)
library(dplyr)
library(ecmwfr)


snap_to_era5_grid = function(domain, product = NULL) {
  
  # Get bounding box in WGS84
  bbox = domain |> 
    st_transform(4326) |> 
    st_bbox()
  
  # single level or land
  if(!is.null(product) & product == "single_layer"){
    grid_deg = 0.25
  }
  if(!is.null(product) & product == "land"){
    grid_deg = 0.1
  }
  if(!is.null(product)){grid_deg = 0.25}
  
  # Snap outward to nearest 0.25 degree grid
  north = ceiling(bbox["ymax"] / grid_deg) * grid_deg
  south = floor(bbox["ymin"] / grid_deg) * grid_deg
  east  = ceiling(bbox["xmax"] / grid_deg) * grid_deg
  west  = floor(bbox["xmin"] / grid_deg) * grid_deg
  
  # Check minimum span — must cover at least 2 grid cells in each direction
  # otherwise CDS sometimes returns nothing
  if ((north - south) < 2 * grid_deg) {
    north = north + grid_deg
    south = south - grid_deg
  }
  if ((east - west) < 2 * grid_deg) {
    east = east + grid_deg
    west = west - grid_deg
  }
  
  paste0(north, "/", west, "/", south, "/", east)
}


#' Download ERA5 reanalysis data for a single location
#'
#' @param lon Longitude of the location (default: -2.283846)
#' @param lat Latitude of the location (default: 53.439953)
#' @param dataset Character string specifying the dataset. Options:
#'     - "reanalysis-era5-single-levels"
#'     - "reanalysis-era5-land"
#' @param variables A vector of variables to download. Options:
#'      - "2m_dewpoint_temperature",
#'      -"2m_temperature",
#'      -"soil_temperature_level_1",
#'      - "snow_cover",
#'      -"surface_solar_radiation_downwards",
#'      -"10m_u_component_of_wind",
#'      -"10m_v_component_of_wind",
#'      -"surface_pressure"
#' @param years A vector of years for which data is requested
#' @param api_key API key for authentication (must be provided)
#' @param path_out Directory to save output files (default: "out/")
#' @return A dataframe containing the combined data for all variables and years

get_era <- function(ecmwf_format_bb,
                    dataset = "land",
                    variables,
                    years,
                    months = seq(1,12),
                    api_key,
                    path_out) {
  
  
  if (is.null(api_key)) {
    stop("API key must be provided.")
  }
  
  # Create output directory if it doesn't exist
  if (!dir.exists(path_out)) {
    dir.create(path_out, recursive = TRUE)
  }
  
  if(dataset == "land"){
    ds = "reanalysis-era5-land"
  }
  if(dataset == "single-level"){
    ds = "reanalysis-era5-single-levels"
  }
  
  # Set the API key
  wf_set_key(key = api_key)
  
  for (y in years) {
    
    for (m in months){
      
      for (v in variables) {
        
        tryCatch({
          
          request <- list(
            dataset_short_name = ds,
            product_type = "reanalysis",
            variable = v,
            year = as.character(y),
            month = m,
            #month = unique(month(kr8_d8)),
            day = sprintf("%02d", 1:31),
            time = sprintf("%02d:00", 0:23),
            area = ecmwf_format_bb,
            format = "netcdf",
            download_format = "unarchived",
            target = paste0(v, "_",sprintf("%02d",m),"_", y, ".nc")
          )
          
          print(paste("requesting", v, "for ",y, " in ", sprintf("%02d",m)))
          
          tryCatch({
            file <- wf_request(request = request, transfer = TRUE, path = paste0(path_out, "/"))
          }, error = function(e) {
            message("Error requesting variable ", v, " for month ", m, " year ", y, ": ", conditionMessage(e))
          })
          
        }, error = function(e) {
          message("Error requesting variable ", v, " for month ", m, " year ", y, ": ", conditionMessage(e))
        })
        
      }
      
    }
    
    
    
  }
  
}
