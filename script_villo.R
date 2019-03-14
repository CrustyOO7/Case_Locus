# clean workspace
rm(list=ls())

install.packages("shinyjs")

# load libraries
library(tidyverse)
library(shinyjs)

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
  
# ggplot(villo_and_municipalities_shp) +
#   geom_sf()

# calculate villo station density
villo_density_shp <- villo_and_municipalities_shp %>%
  group_by(national_c, name_dut, area) %>%
  summarise(count = n()) %>%
  mutate(density = count / area)  %>%
  arrange(desc(density)) %>%
  ungroup()

# join density data to centroid data
municipalities_centroids_shp
villo_density <- st_set_geometry(villo_density_shp, NULL)

municipalities_centroids_shp <- left_join(municipalities_centroids_shp, 
          select(villo_density, national_c, count, density),
          by = "national_c")

# villo_density_shp
# class(villo_density_shp$area[1])
# municipalities_centroids_shp
# class(municipalities_centroids_shp$area[1])
# set_units(municipalities_centroids_shp$area, km^2)

# transform units to be more informative
units(municipalities_centroids_shp$area) <- with(ud_units, km^2)
municipalities_centroids_shp$density <- municipalities_centroids_shp$density * 1000000
municipalities_centroids_shp$density <- set_units(municipalities_centroids_shp$density, km^-2)

# plot villo station density's
tm_shape(municipalities_shp) +
  tm_borders() +
  tm_fill(col = "national_c", alpha = 0.3, legend.show = FALSE) +
  tm_text("name_dut", size = 0.5) +
  tm_shape(villo_and_municipalities_shp) +
  tm_symbols(col = "black", size = 0.07) +
  # tm_legend(show = FALSE) +
  tm_shape(municipalities_centroids_shp) +
  tm_symbols(size = "density", alpha = 0.5, scale = 2) +
  tm_scale_bar()

villo_and_municipalities_shp

# calculate distances
villo_distances <- st_distance(villo_and_municipalities_shp)
dim(villo_distances)
str(villo_distances)
villo_distances <- as.tibble(villo_distances)


# Find distance to the nearest vello station
min_distance <- c()
for (i in 1:nrow(villo_distances)) {
  villo_distances[i,i] <- 500000
  min_distance[i] <- min(villo_distances[i,])
}

length(min_distance)
names(min_distance)
str(min_distance)

min_distance <- tibble(min_distance)

summary(min_distance)

boxplot(min_distance)

ggplot() + 
  geom_boxplot(data = min_distance, aes(x = as.factor(""), y = min_distance)) + 
  ggtitle("distance to nearest villo station in meters") +
  labs(y = "minimum distance in meter", x = "")

arrange(min_distance, min_distance)
arrange(min_distance, desc(min_distance))

villo_and_municipalities_shp$min_distance <- min_distance$min_distance

municipalities_min_distance <- villo_and_municipalities_shp %>%
  group_by(national_c, name_dut) %>%
  summarise(avg_min_distance = mean(min_distance)) %>%
  arrange(desc(avg_min_distance)) %>%
  ungroup() %>%
  st_drop_geometry()

municipalities_min_distance

municipalities_centroids_shp <- left_join(municipalities_centroids_shp, municipalities_min_distance, by = "national_c")

# plot station
tm_shape(municipalities_shp) +
  tm_borders() +
  tm_fill(col = "national_c", alpha = 0.3, legend.show = FALSE) +
  tm_text("name_dut", size = 0.5) +
  tm_shape(villo_and_municipalities_shp) +
  tm_symbols(col = "black", size = 0.07) +
  tm_shape(municipalities_centroids_shp) +
  tm_bubbles(size = "avg_min_distance", alpha = 0.5, scale = 3) +
  tm_scale_bar() +
  tm_layout(title = "Average distance to nearest villo station per district")


