
# Load libraries ----------------------------------------------------------
require(pacman)
p_load(terra, sf, fs, glue, showtext, rnaturalearthdata, rnaturalearth, geodata, extrafont, colourpicker, tidyverse, gtools, rgeos, stringr)

g <- gc(reset = TRUE); rm(list = ls()); options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
path <- '//alliancedfs.alliance.cgiar.org/CL9_Coffee_Cocoa2/_coffeeEastAfrica'
fles <- dir_ls(glue('{path}/rf/output/run9/results/process'), regexp = '.asc$') %>% as.character()

# Zone 
keny <- geodata::gadm(country = 'KEN', level = 0, path = '.')
ken1 <- geodata::gadm(country = 'KEN', level = 1, path = '.')
ken1 <- st_as_sf(ken1)

# Current
crnt <- grep('unc_current.asc', fles, value = T) %>% terra::rast() %>% terra::crop(., keny) %>% terra::mask(., keny)

# Future 
ft50 <- grep('50', fles, value = T) %>% grep('.asc$', ., value = T) %>% terra::rast() %>% terra::crop(., keny) %>% terra::mask(., keny)

plot(crnt)
plot(ft30)

# World shapefile
wrld <- ne_countries(returnclass = 'sf', scale = 50)
wrld <- filter(wrld, region_un == 'Africa')

# Stacking ----------------------------------------------------------------
c(crnt, ft50)
crnt <- terra::resample(crnt, ft50, method = 'near')
stck <- c(crnt, ft50)
names(stck) <- c('current', 'future')

# Labels ------------------------------------------------------------------
lbl_crn <- tibble(value = c(1, 2, 3, 4, 8, 9), class_baseline = c('Unsuitable', 'Unsuitable', 'Arabica', 'Robusta', 'Limitations', 'Mixed'))
lbl_ftr <- tibble(value = c(1, 2, 3, 4, 5, 6), class_future = c('Unsuitable', 'Unsuitable', 'Arabica', 'Robusta', 'Limitations', 'Mixed'))

tble <- terra::as.data.frame(stck, xy = TRUE) %>% as_tibble()
tble <- inner_join(tble, lbl_crn, by = c('current' = 'value'))
tble <- inner_join(tble, lbl_ftr, by = c('future' = 'value'))
tble <- dplyr::select(tble, x, y, class_baseline, class_future)
tble <- gather(tble, var, class, -x, -y)

tble <- mutate(tble, var = ifelse(var == 'class_baseline', 'Baseline', 'Future'))
tble <- mutate(tble, var = factor(var, levels = c('Baseline', 'Future')))
tble <- mutate(tble, class = factor(class, levels = c('Unsuitable', 'Arabica', 'Robusta', 'Limitations', 'Mixed')))

# To make the map ---------------------------------------------------------
font_add_google('Roboto Condensed', 'Roboto Condensed')
showtext_auto()

gmap <- ggplot() + 
  geom_tile(data = tble, aes(x = x, y = y, fill = class)) +
  facet_wrap(.~var) +
  scale_fill_manual(values = c('#FFFFFF', '#8DC280', '#C4C081', '#C4C4C4', '#FFFFCC')) +
  geom_sf(data = st_as_sf(keny), fill = NA, col = 'grey50', lwd = 0.35) +
  geom_sf(data = ken1, fill = NA, col = 'grey50', lwd = 0.2) +
  # geom_sf(data = wrld, fill = NA, col = 'grey40', lwd = 0.1) +
  coord_sf(xlim = ext(keny)[1:2], ylim = ext(keny)[3:4]) + 
  labs(x = 'Lon', y = 'Lat', fill = '') +
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

ggsave(plot = gmap, filename = './png/maps/suit_coffee.png', units = 'in', width = 9, height = 7, dpi = 300)
