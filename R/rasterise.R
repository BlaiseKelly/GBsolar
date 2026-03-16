


rasterise_meteo = function(variables, meteo_dir){
  
  meteo_out = paste0(meteo_dir,"/bricks/")
  
  if(!dir.exists(meteo_out)){
    dir.create(meteo_out,recursive = TRUE)
  }
  
  for (v in variables){

  files <- list.files(meteo_dir, v, full.names = TRUE)
  
  ## open one file to get species
  lefile <- nc_open(files[1])
  
  longitude <- ncvar_get(lefile, "longitude")
  latitude <- ncvar_get(lefile, "latitude")
  lon_res <- (longitude[2]-longitude[1])/2
  lat_res <- (latitude[1]-latitude[2])/2
  
  ##create min x and y
  x_min <- min(longitude)
  x_max <- max(longitude)
  y_min <- min(latitude)
  y_max <- max(latitude)
  ##determine number of lat and lon points
  n_y <- NROW(latitude)
  n_x <- NROW(longitude)
  
  ##work out number of x and y cells
  res_x <- (x_max-x_min)/n_x
  res_y <- (y_max-y_min)/n_y
  
  ##create a data frame to setup polygon generation
  df <- data.frame(X = c(x_min, x_max, x_max, x_min),
                   Y = c(y_max, y_max, y_min, y_min))
  
  ##generate a polygon of the area
  vgt_area <- df %>%
    st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
    dplyr::summarise(data = st_combine(geometry)) %>%
    st_cast("POLYGON")
  
  
  nc_close(lefile)
  bricks <- list()
  for (f in files){
    
    lefile <- nc_open(f)
    
    TIME <- lefile$dim$valid_time$vals
    
    time_since <- str_sub(lefile$dim$valid_time$units, 15, -1)
    
    d8s <- paste0((lubridate::ymd(time_since) + lubridate::seconds(TIME)))
    #d8 <- lubridate::ymd(lubridate::ymd(time_since) + lubridate::seconds(TIME))
    
    
    # d8_in <- colsplit(d8_time, " ", c("date", "time")) %>%
    #   mutate(hour_no = seq(1:NROW(d8)))
    
    variable <- names(lefile$var)[3]
    
    var_in <- ncvar_get(lefile, variable, start = c(1,1,1), count = c(n_x, n_y, NROW(d8s)))
    
    ##generate raster from it
    var <- t(brick(var_in[1:n_x,1:n_y, 1:NROW(d8s)]))
    
    ##define the extent
    crs(var) <- 4326
    bb <- extent(vgt_area)
    extent(var) <- bb
    
    names(var) <- format(d8s)
    
    nam_out <- str_sub(d8s[1],1,-4)
    
    bricks[[nam_out]] = rast(var)
    
  }
  
  var_bricks = rast(bricks)
    
    terra::writeRaster(var_bricks, filename=paste0(meteo_out, v,".TIF"), overwrite = TRUE)
    
    #rstz[[nam_out]] <- var
    
    print(paste(variable,nam_out))
    
  }
  
}
    
