
# Load libraries ----------------------------------------------------------
require(pacman)
p_load(terra, sf, fs, glue, showtext, rnaturalearthdata, ggspatial, RColorBrewer, rnaturalearth, geodata, extrafont, colourpicker, tidyverse, gtools, rgeos, stringr)

g <- gc(reset = TRUE); rm(list = ls()); options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
path <- '//alliancedfs.alliance.cgiar.org/CL9_Coffee_Cocoa2/_coffeeEastAfrica/rf/output/run9/results/process'
fles <- dir_ls(path)
fles <- as.character(fles)
fles <- grep('2050.tif$', fles, value = TRUE)
rstr <- terra::rast(fles)

plot(rstr)

# Administrative data
keny <- geodata::gadm(country = 'KEN', level = 0, path = './tmpr') %>% st_as_sf()
ken1 <- geodata::gadm(country = 'KEN', level = 1, path = './tmpr') %>% st_as_sf()

# Extract by mask 
rstr <- terra::crop(rstr, keny) %>% terra::mask(., vect(keny))

# Colors ------------------------------------------------------------------
clrs <- tibble(value = c(0, 1, 2, 3, 4, 5), 
               class = c('Unsuitable', 'Incremental adaptation', 'Systemic adaptation', 'Transform to other crops', 'Expansion', 'Systemic adaptation'))
clrs <- mutate(clrs, color = c('#ffffff', '#80bc79', '#e5d51c', '#c95855', '#5c8958', '#e5d51c'))

# Tibble ------------------------------------------------------------------
tble <- as_tibble(terra::as.data.frame(rstr, xy = TRUE))
colnames(tble)[3] <- c('value')
tble <- inner_join(tble, clrs, by = 'value')
tble <- mutate(tble, class = factor(class, levels = unique(pull(clrs, class))))

# To make the maps --------------------------------------------------------

# Fonts
font_add_google('Roboto Condensed', 'Roboto Condensed')
showtext_auto()

# To make the map
gmap <- ggplot() + 
  geom_tile(data = tble, aes(x = x, y = y, fill = class)) + 
  scale_fill_manual(values = clrs$color)  +
  geom_sf(data = keny, fill = NA, col = 'grey50', lwd = 0.4) +
  geom_sf(data = ken1, fill = NA, col = 'grey50', lwd = 0.4) +
  coord_sf() + 
  labs(x = 'Lon', y = 'Lat', fill = '') +
  ggtitle(label = 'Impact gradient', subtitle = '2050s vs Baseline') +
  theme_minimal() +
  theme(axis.text.x = element_text(family = 'Roboto Condensed', size = 26), 
        axis.text.y = element_text(family = 'Roboto Condensed', size = 26), 
        axis.title.x = element_text(family = 'Roboto Condensed', size = 30), 
        strip.text.x = element_text(family = 'Roboto Condensed', size = 45),
        axis.title.y = element_text(family = 'Roboto Condensed', size = 30),
        plot.title = element_text(family = 'Roboto Condensed', size = 55, face = 'bold', hjust = 0.5),
        plot.subtitle = element_text(family = 'Roboto Condensed', size = 55, face = 'bold', hjust = 0.5),
        legend.text = element_text(family = 'Roboto Condensed', size = 30),
        legend.title = element_text(family = 'Roboto Condensed', size = 36), 
        legend.position = 'bottom') +
  ggspatial::annotation_scale(location = "bl", bar_cols = c("grey60", "white"), text_cex = 2.2, text_family = "Roboto", text_col="grey60") +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    width = unit(1.4, 'cm'),
    height = unit(1.3, 'cm'),
    pad_x = unit(0.14, "in"), pad_y = unit(0.14, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("grey50", "white"),
      line_col = "grey15", text_family = "Roboto Condensed" , text_col = 'grey60', text_size = 20)) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

gmap
ggsave(plot = gmap, filename = './png/maps/impact_gradient.png', units = 'in', width = 6.7, height = 8, dpi = 300)

