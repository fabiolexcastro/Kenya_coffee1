
# Load libraries ----------------------------------------------------------
require(pacman)
p_load(terra, sf, fs, glue, ggpubr, cowplot, gridExtra, showtext, RColorBrewer, rnaturalearthdata, rnaturalearth, geodata, extrafont, colourpicker, tidyverse, gtools, rgeos, stringr)

g <- gc(reset = TRUE); rm(list = ls()); options(scipen = 999, warn = -1)

# Fonts -------------------------------------------------------------------
font_add_google('Roboto Condensed', 'Roboto Condensed')
showtext_auto()

# Load data ---------------------------------------------------------------

# Limit country 
keny <- geodata::gadm(country = 'KEN', level = 0, path = './tmpr') %>% st_as_sf()
ken1 <- geodata::gadm(country = 'KEN', level = 1, path = './tmpr') %>% st_as_sf()

# Current
c_pp <- terra::rast('./rst/vars/baseline/prec.tif')
c_et <- terra::rast('./rst/vars/baseline/etp.tif')
c_df <- terra::rast('./rst/vars/baseline/deficit.tif')

# Future
f_pp <- dir_ls('./rst/vars/rcp60/50s/ensemble', regexp = 'prec') %>% as.character() %>% mixedsort() %>% terra::rast()
names(f_pp) <- glue('prec_{1:12}')
f_et <- terra::rast('./rst/vars/rcp60/50s/ensemble/etp.tif')
f_df <- terra::rast('./rst/vars/rcp60/50s/ensemble/deficit.tif')

# Stacking ----------------------------------------------------------------

# Current
c_et_tt <- sum(c_et)
c_pp_tt <- sum(c_pp)
c_df_tt <- c_pp_tt - c_et_tt

c_st <- c(c_pp_tt, c_et_tt, c_df_tt)
names(c_st) <- c('prec', 'etp', 'deficit')
c_tb <- terra::as.data.frame(c_st, xy = T) %>% as_tibble()

# Future
f_et_tt <- sum(f_et)
f_pp_tt <- sum(f_pp)
f_df_tt <- f_pp_tt - f_et_tt

f_st <- c(f_pp_tt, f_et_tt, f_df_tt)
names(f_st) <- c('prec', 'etp', 'deficit')
f_st <- terra::crop(f_st, vect(keny)) %>% terra::mask(., vect(keny))
f_tb <- terra::as.data.frame(f_st, xy = T) %>% as_tibble()

# To make the map ----------------------------------------------------------

# Current -----------------------------------------------------------------
g_cr_pp <- ggplot() + 
  geom_tile(data = c_tb, aes(x = x, y = y, fill = prec)) + 
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'BrBG')) + 
  geom_sf(data = keny, fill = NA, col = 'grey50', lwd = 0.35) +
  geom_sf(data = ken1, fill = NA, col = 'grey50', lwd = 0.2) +
  coord_sf(xlim = ext(keny)[1:2], ylim = ext(keny)[3:4]) + 
  labs(x = 'Lon', y = 'Lat', fill = '') +
  ggtitle(label = 'Precipitation (mm)', subtitle = 'Baseline') +
  theme_minimal() +
  theme(axis.text.x = element_text(family = 'Roboto Condensed', size = 26), 
        axis.text.y = element_text(family = 'Roboto Condensed', size = 26), 
        axis.title.x = element_text(family = 'Roboto Condensed', size = 30), 
        strip.text.x = element_text(family = 'Roboto Condensed', size = 45),
        axis.title.y = element_text(family = 'Roboto Condensed', size = 30),
        plot.title = element_text(family = 'Roboto Condensed', size = 55, face = 'bold', hjust = 0.5),
        plot.subtitle = element_text(family = 'Roboto Condensed', size = 55, face = 'bold', hjust = 0.5),
        legend.text = element_text(family = 'Roboto Condensed', size = 36),
        legend.key.width = unit(3, 'line'),
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

g_cr_et <- ggplot() + 
  geom_tile(data = c_tb, aes(x = x, y = y, fill = etp)) + 
  scale_fill_gradientn(colors = rev(brewer.pal(n = 9, name = 'BrBG'))) + 
  geom_sf(data = keny, fill = NA, col = 'grey50', lwd = 0.35) +
  geom_sf(data = ken1, fill = NA, col = 'grey50', lwd = 0.2) +
  coord_sf(xlim = ext(keny)[1:2], ylim = ext(keny)[3:4]) + 
  labs(x = 'Lon', y = 'Lat', fill = '') +
  ggtitle(label = 'ETP (mm)', subtitle = 'Baseline') +
  theme_minimal() +
  theme(axis.text.x = element_text(family = 'Roboto Condensed', size = 26), 
        axis.text.y = element_text(family = 'Roboto Condensed', size = 26), 
        axis.title.x = element_text(family = 'Roboto Condensed', size = 30), 
        strip.text.x = element_text(family = 'Roboto Condensed', size = 45),
        axis.title.y = element_text(family = 'Roboto Condensed', size = 30),
        plot.title = element_text(family = 'Roboto Condensed', size = 55, face = 'bold', hjust = 0.5),
        plot.subtitle = element_text(family = 'Roboto Condensed', size = 55, face = 'bold', hjust = 0.5),
        legend.text = element_text(family = 'Roboto Condensed', size = 36),
        legend.key.width = unit(3, 'line'),
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

g_cr_bl <- ggplot() + 
  geom_tile(data = c_tb, aes(x = x, y = y, fill = deficit)) + 
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'BrBG')) + 
  geom_sf(data = keny, fill = NA, col = 'grey50', lwd = 0.35) +
  geom_sf(data = ken1, fill = NA, col = 'grey50', lwd = 0.2) +
  coord_sf(xlim = ext(keny)[1:2], ylim = ext(keny)[3:4]) + 
  labs(x = 'Lon', y = 'Lat', fill = '') +
  ggtitle(label = 'Déficit (mm)', subtitle = 'Baseline') +
  theme_minimal() +
  theme(axis.text.x = element_text(family = 'Roboto Condensed', size = 26), 
        axis.text.y = element_text(family = 'Roboto Condensed', size = 26), 
        axis.title.x = element_text(family = 'Roboto Condensed', size = 30), 
        strip.text.x = element_text(family = 'Roboto Condensed', size = 45),
        axis.title.y = element_text(family = 'Roboto Condensed', size = 30),
        plot.title = element_text(family = 'Roboto Condensed', size = 55, face = 'bold', hjust = 0.5),
        plot.subtitle = element_text(family = 'Roboto Condensed', size = 55, face = 'bold', hjust = 0.5),
        legend.text = element_text(family = 'Roboto Condensed', size = 36),
        legend.key.width = unit(3, 'line'),
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

g_cr <- ggarrange(g_cr_pp, g_cr_et, g_cr_bl, nrow = 1, ncol = 3)
ggsave(plot = g_cr, filename = './png/maps/current_prec_etp_deficit.png', units = 'in', width = 14, height = 7, dpi = 300)

# Future ------------------------------------------------------------------
g_ft_pp <- ggplot() + 
  geom_tile(data = f_tb, aes(x = x, y = y, fill = prec)) + 
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'BrBG')) + 
  geom_sf(data = keny, fill = NA, col = 'grey50', lwd = 0.35) +
  geom_sf(data = ken1, fill = NA, col = 'grey50', lwd = 0.2) +
  coord_sf(xlim = ext(keny)[1:2], ylim = ext(keny)[3:4]) + 
  labs(x = 'Lon', y = 'Lat', fill = '') +
  ggtitle(label = 'Precipitation (mm)', subtitle = 'Future') +
  theme_minimal() +
  theme(axis.text.x = element_text(family = 'Roboto Condensed', size = 26), 
        axis.text.y = element_text(family = 'Roboto Condensed', size = 26), 
        axis.title.x = element_text(family = 'Roboto Condensed', size = 30), 
        strip.text.x = element_text(family = 'Roboto Condensed', size = 45),
        axis.title.y = element_text(family = 'Roboto Condensed', size = 30),
        plot.title = element_text(family = 'Roboto Condensed', size = 55, face = 'bold', hjust = 0.5),
        plot.subtitle = element_text(family = 'Roboto Condensed', size = 55, face = 'bold', hjust = 0.5),
        legend.text = element_text(family = 'Roboto Condensed', size = 36),
        legend.key.width = unit(3, 'line'),
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

g_ft_et <- ggplot() + 
  geom_tile(data = f_tb, aes(x = x, y = y, fill = etp)) + 
  scale_fill_gradientn(colors = rev(brewer.pal(n = 9, name = 'BrBG'))) + 
  geom_sf(data = keny, fill = NA, col = 'grey50', lwd = 0.35) +
  geom_sf(data = ken1, fill = NA, col = 'grey50', lwd = 0.2) +
  coord_sf(xlim = ext(keny)[1:2], ylim = ext(keny)[3:4]) + 
  labs(x = 'Lon', y = 'Lat', fill = '') +
  ggtitle(label = 'ETP (mm)', subtitle = 'Future') +
  theme_minimal() +
  theme(axis.text.x = element_text(family = 'Roboto Condensed', size = 26), 
        axis.text.y = element_text(family = 'Roboto Condensed', size = 26), 
        axis.title.x = element_text(family = 'Roboto Condensed', size = 30), 
        strip.text.x = element_text(family = 'Roboto Condensed', size = 45),
        axis.title.y = element_text(family = 'Roboto Condensed', size = 30),
        plot.title = element_text(family = 'Roboto Condensed', size = 55, face = 'bold', hjust = 0.5),
        plot.subtitle = element_text(family = 'Roboto Condensed', size = 55, face = 'bold', hjust = 0.5),
        legend.text = element_text(family = 'Roboto Condensed', size = 36),
        legend.key.width = unit(3, 'line'),
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

g_ft_bl <- ggplot() + 
  geom_tile(data = f_tb, aes(x = x, y = y, fill = deficit)) + 
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'BrBG')) + 
  geom_sf(data = keny, fill = NA, col = 'grey50', lwd = 0.35) +
  geom_sf(data = ken1, fill = NA, col = 'grey50', lwd = 0.2) +
  coord_sf(xlim = ext(keny)[1:2], ylim = ext(keny)[3:4]) + 
  labs(x = 'Lon', y = 'Lat', fill = '') +
  ggtitle(label = 'Déficit (mm)', subtitle = 'Future') +
  theme_minimal() +
  theme(axis.text.x = element_text(family = 'Roboto Condensed', size = 26), 
        axis.text.y = element_text(family = 'Roboto Condensed', size = 26), 
        axis.title.x = element_text(family = 'Roboto Condensed', size = 30), 
        strip.text.x = element_text(family = 'Roboto Condensed', size = 45),
        axis.title.y = element_text(family = 'Roboto Condensed', size = 30),
        plot.title = element_text(family = 'Roboto Condensed', size = 55, face = 'bold', hjust = 0.5),
        plot.subtitle = element_text(family = 'Roboto Condensed', size = 55, face = 'bold', hjust = 0.5),
        legend.text = element_text(family = 'Roboto Condensed', size = 36),
        legend.key.width = unit(3, 'line'),
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

g_ft <- ggarrange(g_ft_pp, g_ft_et, g_ft_bl, nrow = 1, ncol = 3)
ggsave(plot = g_ft, filename = './png/maps/future_prec_etp_deficit.png', units = 'in', width = 14, height = 7, dpi = 300)

