# clean workspace
rm(list=ls())

install.packages()

# load libraries
library(tidyverse)

library(ggmap)
library(sf)
library(raster)
library(tmap)
library(sp)
library(rgdal)

library(lubridate)

library(RColorBrewer)
library(viridisLite)

library(stringr)

library(units)

# GET shape files -----------
# "/geoserver/bm_bike/wfs?service=wfs&amp;version=1.1.0&amp;request=GetFeature&amp;typeName=bm_bike:villo&amp;outputFormat=shape-zip"
villo_shp <- st_read("villo/villo.shp")
plot(villo_shp$geometry)
head(villo_shp)
st_crs(villo_shp)

municipalities_shp <- st_read("municipality/municipality.shp")
plot(municipalities_shp$geometry)
head(municipalities_shp)
st_crs(municipalities_shp)

# visualise villo stations and municipalities Brussels
tm_shape(municipalities_shp) +
  tm_borders() +
  tm_fill(col = "national_c", alpha = 0.3, legend.show = FALSE) +
  tm_text("name_dut", size = 0.5) +
  tm_shape(villo_shp) +
  tm_dots(size = 0.1, shape = 1, col = "blue")

# possible key to link on  
unique(villo_shp$mu_nl)
unique(municipalities_shp$name_dut)

# make levels the same, they are in same position so ok to do this
levels(municipalities_shp$name_dut) = levels(villo_shp$mu_nl)

# some extra calculations
municipalities_shp$area <- st_area(municipalities_shp)
municipalities_centroids_shp <- st_centroid(municipalities_shp)
dim(st_distance(villo_shp))

plot(st_geometry(st_centroid(municipalities_shp)))

# spatial join
villo_and_municipalities_shp <- st_join(villo_shp, municipalities_shp)

# some plots, check crs's first
st_crs(municipalities_shp)
st_crs(villo_shp)
st_crs(villo_and_municipalities_shp)

tmap_mode("view")

tm_shape(municipalities_shp) +
  tm_borders() +
  tm_fill(col = "national_c", alpha = 0.3, legend.show = FALSE) +
  tm_text("name_dut", size = 0.5) +
tm_shape(villo_and_municipalities_shp) +
  tm_symbols(col = "national_c", size = 0.1) +
  tm_legend(show = FALSE) +
tm_shape(municipalities_centroids_shp) +
  tm_dots()

tmap_mode("plot")
  
ggplot(villo_and_municipalities_shp) +
  geom_sf()

# calculate villo station density
villo_density_shp <- villo_and_municipalities_shp %>%
  group_by(national_c, name_dut, area) %>%
  summarise(count = n()) %>%
  mutate(density = count / area)  %>%
  arrange(desc(density)) %>%
  ungroup()

municipalities_centroids_shp
villo_density <- st_set_geometry(villo_density_shp, NULL)

municipalities_centroids_shp <- left_join(municipalities_centroids_shp, 
          select(villo_density, national_c, count, density),
          by = "national_c")

villo_density_shp
class(villo_density_shp$area[1])
municipalities_centroids_shp
class(municipalities_centroids_shp$area[1])
set_units(municipalities_centroids_shp$area, km^2)

units(municipalities_centroids_shp$area) <- with(ud_units, km^2)
municipalities_centroids_shp$density <- municipalities_centroids_shp$density * 1000000
municipalities_centroids_shp$density <- set_units(municipalities_centroids_shp$density, km^-2)

tm_shape(municipalities_shp) +
  tm_borders() +
  tm_fill(col = "national_c", alpha = 0.3, legend.show = FALSE) +
  tm_text("name_dut", size = 0.5) +
  # tm_shape(villo_and_municipalities_shp) +
  # tm_symbols(col = "national_c", size = 0.1) +
  # tm_legend(show = FALSE) +
  tm_shape(municipalities_centroids_shp) +
  tm_symbols(size = "density", alpha = 0.5, scale = 2) +
  tm_scale_bar()



select(villo_density_shp, national_c, count, density)

st_join(municipalities_centroids_shp, villo_density_shp)

