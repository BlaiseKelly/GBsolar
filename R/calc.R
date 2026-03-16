
calc_solar_potential = function(meteo_dat,lat,lon,aspect_rast,slope_rast,shade_rast){
  
  # calculate solar position (sp)
  sp = meteo_dat |>
    mutate(getSunlightPosition(date = date, lat = lat, lon = lon)) |> 
    mutate(solar_zenith = 90 - (altitude * 180/pi)) |>
    mutate(solar_azimuth = (azimuth * 180/pi + 180) %% 360)
  
  theta_rad_rast = acos(cos(sp$solar_zenith * pi/180) * cos(slope_rast*pi/180) +
                        sin(sp$solar_zenith * pi/180) * sin(slope_rast * pi/180) *
                        cos((sp$solar_azimuth - aspect_rast) * pi/180))

# only dir affected by shade, multiply the time series by shaded area
shade_dir = (shade_rast/100)*meteo_dat$tdir_wm2
# add diffuse in to give diffuse direct 
global_wm2 = shade_dir+meteo_dat$tdiff_wm2

tilted_wm2 = global_wm2*cos(theta_rad_rast)

}

# calculate how the incident irradition translates into PV yield with some estimated paramerter
calc_pv_potential = function(tilted_wm2,meteo_dat){
  
  # constants
  eta_ref = 0.20           # 20% efficiency
  gamma = -0.004           # -0.4% per °C
  T_ref = 25               # Reference temperature (°C)
  panel_area_m2 = 1.6     # e.g., 1.6 m² typical for one panel
  
  #Nominal operating cell temperature (NOCT)
  NOCT = 45
  #Temp panel tested at
  P_amb = 20
  #W panel tested at
  P_t = 800
  # panel size
  effic = 0.153
  
  T_cell = meteo_dat$air_temp+(NOCT-20)*(tilted_wm2/800) 
  # 
  eta_T = effic*(1+gamma*(T_cell-T_ref)) 
  # combined incident irraditaion with cell efficiency
  yield_W_m2 = tilted_wm2*eta_T 
  # each grid cell is 1m2
  yield_kWh = yield_W_m2/1000
  
  return(yield_kWh)
}
