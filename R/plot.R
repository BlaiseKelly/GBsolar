

library(tmap)

#titled_wm2[tilted_wm2<=0] = NA
max(titled_wm2)

tmap_options(facet.max = 800)  # or whatever you need

d8s = gsub("\\."," ", names(titled_wm2))

tm1 <- tm_shape(titled_wm2[[1:24]])+
  tm_raster(col.scale = tm_scale_intervals(values = "viridis", breaks = seq(0,600,50)),
            col.legend = tm_legend(show = TRUE,title = "incident\nirradtion (W)", frame = FALSE)
  )+
  tm_animate(fps = 4)+
tm_layout(frame = FALSE)
#tm_components(c("tm_legend", "tm_credits"), position = c("left", "top"), bg.color = "grey95")
tmap_save(tm1,paste0(sdir,"/OUT/yield_wm2.png"))
tmap_animation(tm1, filename = paste0(sdir,"OUT/yield_wm2.gif"))
