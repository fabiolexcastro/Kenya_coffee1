
# Load libraries ----------------------------------------------------------
require(pacman)
p_load(terra, sf, fs, glue, showtext, ggrepel, extrafont, rnaturalearthdata, rnaturalearth, colourpicker, tidyverse, gtools, rgeos, stringr)

g <- gc(reset = TRUE); rm(list = ls()); options(scipen = 999, warn = -1)

# Fonts
font_add_google('Roboto Condensed', 'Roboto Condensed')
showtext_auto()

# Load data ---------------------------------------------------------------
keny <- geodata::gadm(country = 'KEN', level = 0, path = '.')
ken1 <- geodata::gadm(country = 'KEN', level = 1, path = '.')
keny <- st_as_sf(keny)
ken1 <- st_as_sf(ken1)

# World data
wrld <- ne_countries(returnclass = 'sf', scale = 50)
afrc <- filter(wrld, continent == 'Africa')

# Get the coordinates -----------------------------------------------------
crds <- mutate(as_tibble(st_coordinates(st_centroid(ken1))), name = ken1$NAME_1)

# To make the map --------------------------------------------------------

gmap <- ggplot() + 
  geom_sf(data = keny, fill = NA, lwd = 0.3, col = 'grey50') +
  geom_sf(data = ken1, fill = NA, lwd = 0.3, col = 'grey50') +
  geom_text_repel(data = crds, aes(x = X, y = Y, label = name), family = 'Roboto Condensed', size = 5) +
  labs(x = 'Lon', y = 'Lat', fill = '') +
  coord_sf(xlim = ext(afrc)[1:2], ylim = ext(afrc)[3:4]) + 
  theme_minimal() +
  theme(axis.text.x = element_text(family = 'Roboto Condensed', size = 26), 
        axis.text.y = element_text(family = 'Roboto Condensed', size = 26), 
        axis.title.x = element_text(family = 'Roboto Condensed', size = 30), 
        strip.text.x = element_text(family = 'Roboto Condensed', size = 45),
        axis.title.y = element_text(family = 'Roboto Condensed', size = 30), 
        legend.text = element_text(family = 'Roboto Condensed', size = 36), 
        legend.title = element_text(family = 'Roboto Condensed', size = 45), 
        legend.position = 'bottom') +
  ggspatial::annotation_scale(location = "bl", bar_cols = c("grey60", "white"), text_cex = 2.2, text_family = "Roboto", text_col="grey60") +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    width = unit(1.4, 'cm'),
    height = unit(1.3, 'cm'),
    pad_x = unit(0.14, "in"), pad_y = unit(0.14, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("grey50", "white"),
      line_col = "grey15", text_family = "Roboto Condensed" , text_col = 'grey60', text_size = 20))

glct <- ggplot() + 
  geom_sf(data = wrld, fill = 'white', lwd = 0.3) +
  geom_sf(data = afrc, fill = NA, lwd = 0.4, col = 'grey50') +
  geom_sf(data = keny, fill = NA, lwd = 0.9, col = 'red') +
  coord_sf(xlim = ext(afrc)[1:2], ylim = ext(afrc)[3:4]) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect(color = "grey20", fill = NA, size = 1))

glct

gfnl <- gmap + 
  annotation_custom(ggplotGrob(glct), xmin = 33.9, xmax = 35.9, ymin = -4, ymax = -2)

ggsave(plot = gfnl, filename = './png/maps/location_map.png', units = 'in', width = 9, height = 7, dpi = 300)
