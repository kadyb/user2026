# title:     Introduction to spatial data science
# subtitle:  Exercise Solutions
# author:    Krzysztof Dyba

library("terra")

# Raster Data Analysis ---------------------------------------------------------
## Data loading and preprocessing ----------------------------------------------
files = list.files("data/landsat", pattern = "\\.TIF$", full.names = TRUE)
landsat = rast(files)
names(landsat) = c("Blue", "Green", "Red", "NIR")

scale_factor = 0.0000275
offset = -0.2
landsat = landsat * scale_factor + offset

landsat = clamp(landsat, lower = 0, upper = 1, values = TRUE)

## Exercise 1: False color composition -----------------------------------------
plotRGB(landsat, r = 4, g = 3, b = 2, stretch = "lin")

## Exercise 2: Water detection -------------------------------------------------
calculate_ndwi = function(green, nir) {
  ndwi = (green - nir) / (green + nir)
  names(ndwi) = "NDWI"
  return(ndwi)
}

ndwi = calculate_ndwi(landsat[["Green"]], landsat[["NIR"]])

colors = colorRampPalette(
  c("#8B4513", "#D2B48C", "#F5F5DC", "#4FC3F7", "#0288D1", "#01579B")
)
plot(ndwi, col = colors(100), range = c(-1, 1))

water = ndwi > 0
plot(water, col = c("white", "blue"), main = "Water surfaces")

writeRaster(water, "water_surfaces.tif", datatype = "INT1U")

# Vector Data Analysis ---------------------------------------------------------
## Data loading ----------------------------------------------------------------

districts = vect("data/districts.gpkg")

parks = vect("data/parks.gpkg")
parks$area_m2 = expanse(parks, unit = "m")
parks = parks[parks$area_m2 > 2000, ]

## Exercise 1: Proximity analysis-----------------------------------------------
cent = centroids(districts)
near = nearby(cent, parks, k = 1, centroid = FALSE)[, 2]
dist_m = distance(cent, parks[near, ], pairwise = TRUE, unit = "m")

districts$park = parks[near, ]$name
districts$dist_m = dist_m

writeVector(districts, "districts.gpkg")

# Data Visualization -----------------------------------------------------------
# we loaded the vector and raster data in the previous sections

library("ggplot2")
library("tidyterra")
library("ggspatial")

ggplot() +
  geom_spatraster_rgb(data = landsat, r = 3, g = 2, b = 1, stretch = "lin") +
  geom_spatvector(data = districts, fill = NA, color = "red", linewidth = 1) +
  annotation_north_arrow(
    location = "tr", height = unit(1, "cm"), width = unit(1, "cm"),
    pad_x = unit(1, "cm"), pad_y = unit(1, "cm")) +
  annotation_scale(
    location = "bl", pad_x = unit(0.5, "cm"), pad_y = unit(1, "cm"),
    line_col = "white", text_cex = 1) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
ggsave("RGB.png")
