
# Load libraries ----------------------------------------------------------
require(pacman)
p_load(terra, sf, fs, glue, showtext, rnaturalearthdata, rnaturalearth, geodata, extrafont, colourpicker, tidyverse, gtools, rgeos, stringr)

g <- gc(reset = TRUE); rm(list = ls()); options(scipen = 999, warn = -1)

# Functions ---------------------------------------------------------------
calcETP <- function(pp, tx, tn, ta){
  
  # pp <- c_pp; tx <- c_tx; tn <- c_tn; ta <- c_ta
  
  cat('Start\n')
  srd <- terra::resample(srad, pp)
  etp <- 0.0013 * 0.408 * srd * (ta + 17) * (tx - tn - 0.0123 * pp) ^ 0.76
  names(etp) <- glue('etp_{1:12}')
  etp <- etp * c(31,29,31,30,31,30,31,31,30,31,30,31)
  cat('Done!\n')
  return(etp)
  
}

# Load data ---------------------------------------------------------------

# Limit country 
keny <- geodata::gadm(country = 'KEN', level = 0, path = './tmpr')

# Current
crnt <- dir_ls('./rst/vars/baseline', regexp = '.tif')
c_pp <- grep('prec', crnt, value = TRUE) %>% terra::rast()
c_tx <- grep('tmax', crnt, value = TRUE) %>% terra::rast()
c_ta <- grep('tavg', crnt, value = TRUE) %>% terra::rast()
c_tn <- grep('tmin', crnt, value = TRUE) %>% terra::rast()

# Solar radiation
srad <- dir_ls('D:/DATA/climate/SRAD/ET_SolRad') %>% as.character() %>% mixedsort() 
srad <- srad[1:12]
srad <- terra::rast(srad)
srad <- terra::crop(srad, keny) %>% terra::mask(., keny)

# Future
ftre <- dir_ls('./rst/vars/rcp60/50s/ensemble', regexp = '.tif$') %>% as.character() %>% mixedsort()
f_pp <- grep('prec', ftre, value = TRUE) %>% terra::rast()
f_tx <- grep('tmax', ftre, value = TRUE) %>% terra::rast()
f_ta <- grep('tmean', ftre, value = TRUE) %>% terra::rast()
f_tn <- grep('tmin', ftre, value = TRUE) %>% terra::rast()

f_tx <- f_tx / 10
f_ta <- f_ta / 10
f_tn <- f_tn / 10

# Calc etp  ---------------------------------------------------------------

# Current
c_et <- calcETP(pp = c_pp, tx = c_tx, tn = c_tn, ta = c_ta)
terra::writeRaster(c_et, './rst/vars/baseline/etp.tif', overwrite = TRUE)

# Future 
f_et <- calcETP(pp = f_pp, tx = f_tx, tn = f_tn, ta = f_ta)
terra::writeRaster(f_et, './rst/vars/rcp60/50s/ensemble/etp.tif', overwrite = TRUE)

# Calc déficit ------------------------------------------------------------

# Current 
c_df <- c_pp - c_et
terra::writeRaster(c_df, './rst/vars/baseline/deficit.tif', overwrite = TRUE)

# Future
f_df <- f_pp - f_et
terra::writeRaster(f_df, './rst/vars/rcp60/50s/ensemble/deficit.tif', overwrite = TRUE)


