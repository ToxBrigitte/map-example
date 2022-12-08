# Choropleth.R
# Brigitte Desharnais, 2019-08-03.

# Set working directory.
setwd("V:/LSJML/Groupes/Toxicologie/10 - Pr?sentations et m?dias/Congr?s et colloques/SOFT 2019 - San Antonio, Texas USA/Graphiques")
setwd("D:/RECHERCHE/GHB dans les DRE/Graphiques")


# Load required packages.
library(dplyr)
library(ggplot2)
library(geojsonio)
library(sp)
library(broom)
library(viridis)
library(extrafont)
library(rgdal)

# Read the geojson file stored in the working directory.
raq <- geojson_read("regions_quebec.geojson", what = "sp")
water <- readOGR("hydro_s")

# Quick view of the file
plot(raq)

# Fortify (tidy) the data AND keep trace of the region.
raq_fortified <- tidy(raq, region = "res_co_reg")
water_fortified <- tidy(water)

# Define numerical data to be plotted on map.
region <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
GHBPrev <- c(0, 0, 38.7, 5.0, 10.0, 24.16, 32.56, 4.44, 0, 0, 0, 35.48, 13.79, 23.81, 17.50, 14.73, 5.0)
data <- tbl_df(cbind(region, GHBPrev))
data$region <- as.character(data$region)

# Merge the numerical data with the map data.
# NB in by=c(A=B), B is the appelation in the numerical data frame (NOT the map data).
raq_fortified = raq_fortified %>%
  left_join(. , data, by=c("id"="region")) 

# General
ggplot() +
  geom_polygon(data = raq_fortified, aes(fill = GHBPrev, x = long, y = lat, group = group)) +
  theme_void() +
  # Choose one of the following color scales.
  #scale_fill_viridis(name = "GHB Prevalence in DUID (%)") +
  scale_fill_gradient(name = "GHB Prevalence in DRE (%)",
                       low = "#c7d7f2", high = "#003299",
                       limits = c(0, 40)) +
  geom_path(data = raq_fortified, aes(x = long, y = lat, group = group), 
            color = "white", size = 0.5) +
  geom_polygon(data = water_fortified, aes(x = long, y = lat, group = group), color = NA, fill = "#e6f0f7") +
  coord_map(ylim = c(45, 50), xlim = c(-80, -64)) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
        legend.title = element_text(size = 12, family = "Century Gothic"),
        legend.text = element_text(size = 12, family = "Century Gothic"),
        legend.spacing.x = unit(1.0, 'cm'))

# Metropolitain
ggplot() +
  geom_polygon(data = raq_fortified %>% filter(id == "6"|id == "13"), aes(fill = GHBPrev, x = long, y = lat, group = group)) +
  theme_void() +
  # Choose one of the following color scales.
  #scale_fill_viridis(name = "GHB Prevalence in DUID (%)") +
  scale_fill_gradient(name = "GHB Prevalence in DUID (%)",
                       low = "#c7d7f2", high = "#003299",
                       limits = c(0, 40)) +
    geom_path(data = raq_fortified, aes(x = long, y = lat, group = group), 
            color = "white", size = 0.5) +
  #geom_polygon(data = water_fortified, aes(x = long, y = lat, group = group), color = NA, fill = "#e6f0f7") +
  coord_map(ylim = c(45.35, 45.75), xlim = c(-74.1, -73.4)) +
  theme(legend.position = "none")








