

# Load libraries ----------------------------------------------------------
require(pacman)
p_load(terra, sf, fs, glue, showtext, rnaturalearthdata, ggspatial, RColorBrewer, rnaturalearth, geodata, extrafont, colourpicker, tidyverse, gtools, rgeos, stringr)

g <- gc(reset = TRUE); rm(list = ls()); options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

# Climate -------------------
vars <- c(glue('prec_{1:12}'), glue('tmax_{1:12}'), glue('tmean_{1:12}'), glue('tmin_{1:12}'))
vars <- glue('{vars}$')

keny <- geodata::gadm(country = 'KEN', level = 0, path = './tmpr')
ken1 <- geodata::gadm(country = 'KEN', level = 1, path = '.')

# Current 
c_ppt <- geodata::worldclim_country(country = 'KEN', var = 'prec', path = './tmpr')
c_tmx <- geodata::worldclim_country(country = 'KEN', var = 'tmax', path = './tmpr')
c_tmn <- geodata::worldclim_country(country = 'KEN', var = 'tmin', path = './tmpr')
c_tav <- (c_tmx + c_tmn) / 2

c_ppt <- c_ppt %>% terra::crop(., keny) %>% terra::mask(., keny)
c_tmx <- c_tmx %>% terra::crop(., keny) %>% terra::mask(., keny)
c_tmn <- c_tmn %>% terra::crop(., keny) %>% terra::mask(., keny)
c_tav <- c_tav %>% terra::crop(., keny) %>% terra::mask(., keny)

# To write current rasters 
terra::writeRaster(x = c_ppt, filename = glue('./rst/vars/baseline/prec.tif'), overwrite = TRUE)
terra::writeRaster(x = c_tmx, filename = glue('./rst/vars/baseline/tmax.tif'), overwrite = TRUE)
terra::writeRaster(x = c_tmn, filename = glue('./rst/vars/baseline/tmin.tif'), overwrite = TRUE)
terra::writeRaster(x = c_tav, filename = glue('./rst/vars/baseline/tavg.tif'), overwrite = TRUE)

# Future
f_ftr <- dir_ls('//alliancedfs.alliance.cgiar.org/CL9_Coffee_Cocoa2/_coffeeEastAfrica/rst/clm/ftr/50s')
f_ftr <- as.character(f_ftr)
gcms <- basename(f_ftr)
f_ftr <- map(f_ftr, dir_ls) %>% flatten() %>% as.character()
f_ftr <- grep(paste0(vars, collapse = '|'), f_ftr, value = TRUE)
f_ftr <- mixedsort(f_ftr)

# To calc the ensemble for the future -------------------------------------
f_ftr <- purrr::map(.x = 1:length(vars), .f = function(i){
  
  cat(vars[i], '\t')
  fls <- grep(vars[i], f_ftr, value = TRUE)
  rst <- terra::rast(fls)
  rst <- mean(rst)
  out <- './rst/vars/rcp60/50s/ensemble'
  var <- gsub('\\$', '', vars[i])
  terra::writeRaster(x = rst, filename = glue('{out}/{var}.tif'), overwrite = TRUE)
  cat('Done!\n')
  return(rst)
  
})

# To extract by mask ------------------------------------------------------
f_ftr <- map(.x = 1:length(f_ftr), .f = function(i){names(f_ftr[[i]]) <- gsub('\\$', '', vars[i]); return(f_ftr[[i]])})
f_ftr <- do.call('c', f_ftr)
f_ftr <- terra::crop(f_ftr, keny) %>% terra::mask(., keny)

c_crn <- c(c_ppt, c_tmx, c_tav, c_tmn)
names(c_crn) <- gsub('\\$', '', vars)
c_crn <- terra::crop(c_crn, keny) %>% terra::mask(., keny)

# To calc the difference --------------------------------------------------
dfrn <- purrr::map(.x = 1:length(vars), .f = function(i){
  
  cat(i, '\n')
  crn <- c_crn[[i]]
  ftr <- f_ftr[[i]]
  var <- str_sub(names(crn), 1, 4)
  
  if(var == 'prec'){
    cat('prec', '\n')
    dfr <- ftr - crn
    dfr <- (dfr / crn) * 100
  } else {
    cat('temperature', '\n')
    ftr <- ftr/10
    dfr <- ftr - crn
  }
  names(dfr) <- glue('dfr_{vars[i]}')
  names(dfr) <- gsub('\\$', '', names(dfr))
  return(dfr)
  
})

dfrn <- do.call('c', dfrn)
writeRaster(x = dfrn, filename = glue('./rst/vars/rcp60/50s/diferences_50s.tif'))

# To calc the summary by all the year -------------------------------------

# Current
prec_crn <- sum(c_ppt) %>% terra::crop(., keny) %>% terra::mask(., keny)
tmax_crn <- mean(c_tmx) %>% terra::crop(., keny) %>% terra::mask(., keny)
tavg_crn <- mean(c_tav) %>% terra::crop(., keny) %>% terra::mask(., keny)
tmin_crn <- mean(c_tmn) %>% terra::crop(., keny) %>% terra::mask(., keny)

# Future
prec_ftr <- sum(f_ftr[[grep('prec', names(f_ftr))]])
tmax_ftr <- mean(f_ftr[[grep('tmax', names(f_ftr))]]) / 10
tavg_ftr <- mean(f_ftr[[grep('tmean', names(f_ftr))]]) / 10
tmin_ftr <- mean(f_ftr[[grep('tmin', names(f_ftr))]]) / 10

# Difference
prec_dfr <- ((prec_ftr - prec_crn) / prec_crn) * 100
tmax_dfr <- tmax_ftr - tmax_crn
tavg_dfr <- tavg_ftr - tavg_crn
tmin_dfr <- tmin_ftr - tmin_crn

names(prec_dfr) <- 'prec_dfr'
names(tmax_dfr) <- 'tmax_dfr'
names(tavg_dfr) <- 'tavg_dfr'
names(tmin_dfr) <- 'tmin_dfr'

dfrn <- c(prec_dfr, tmax_dfr, tavg_dfr, tmin_dfr)
writeRaster(x = dfrn, filename = glue('./rst/vars/rcp60/50s/diferencesTotals_50s.tif'))

# To make the maps --------------------------------------------------------
dfrn <- terra::rast('./rst/vars/rcp60/50s/diferencesTotals_50s.tif')
tble <- as_tibble(terra::as.data.frame(dfrn, xy = TRUE))
ken1 <- st_as_sf(ken1)

font_add_google('Roboto Condensed', 'Roboto Condensed')
showtext_auto()

# Precipitation
gppt <- ggplot() +
  geom_tile(data = tble, aes(x = x, y = y, fill = prec_dfr)) + 
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'BrBG')) +
  geom_sf(data = ken1, fill = NA, col = 'grey50', lwd = 0.2) +
  labs(x = 'Lon', y = 'Lat', fill = 'Prec (%)') +
  ggtitle(label = 'Changes in the precipitation (%)', subtitle = '2050s vs Baseline') +
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

ggsave(plot = gppt, filename = './png/maps/prec_dfrn.png', units = 'in', width = 6.7, height = 8, dpi = 300)

# Tmean
gtav <- ggplot() +
  geom_tile(data = tble, aes(x = x, y = y, fill = tavg_dfr)) + 
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'YlOrRd')) +
  geom_sf(data = ken1, fill = NA, col = 'grey50', lwd = 0.2) +
  labs(x = 'Lon', y = 'Lat', fill = 'Tavg (C)') +
  ggtitle(label = 'Changes in the average temperature (C)', subtitle = '2050s vs Baseline') +
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

ggsave(plot = gtav, filename = './png/maps/tavg_dfrn.png', units = 'in', width = 6.7, height = 8, dpi = 300)
