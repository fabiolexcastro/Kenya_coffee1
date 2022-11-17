
# Load libraries ----------------------------------------------------------
require(pacman)
p_load(terra, sf, fs, glue, showtext, extrafont, colourpicker, tidyverse, gtools, rgeos, stringr)

g <- gc(reset = TRUE); rm(list = ls()); options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

keny <- geodata::gadm(country = 'KEN', level = 0, path = '.')

# Presences
allb <- read_csv('./tbl/Presence_data_all_2019_oct.csv')
sort(unique(allb$Country))
cffe <- filter(allb, Country == 'Kenya')

# Variables
vars <- c(glue('prec_{1:12}'), glue('tmax_{1:12}'), glue('tmean_{1:12}'), glue('tmin_{1:12}'))
vars <- glue('{vars}.asc')

# Climate -------------------
# Current 
f_crn <- '//alliancedfs.alliance.cgiar.org/CL9_Coffee_Cocoa2/_africaCoffee/_raster/_arabica/_climate/_current/_asc'
f_crn <- dir_ls(f_crn, regexp = '.asc$')
f_crn <- grep(paste0(vars, collapse = '|'), f_crn, value = T)
f_crn <- as.character(f_crn)
f_crn <- mixedsort(f_crn)
s_crn <- terra::rast(f_crn)

# Future
f_ftr <- dir_ls('//alliancedfs.alliance.cgiar.org/CL9_Coffee_Cocoa2/_africaCoffee/_raster/_arabica/_climate/_future_others/_rcp85')
f_ftr <- glue('{f_ftr}/_asc') %>% dir_ls() %>% as.character()
f_ftr <- map(f_ftr, dir_ls, regexp = '.asc$') %>% unlist() %>% as.character()
f_ftr <- grep(paste0(vars, collapse = '|'), f_ftr, value = T)
gcms <- str_split(f_ftr, pattern = '/') %>% map(., 13) %>% unique() %>% unlist()

# Extract values - current ------------------------------------------------
v_crn <- terra::extract(s_crn, cffe[,c('Longitude', 'Latitude')])
v_crn <- cbind(cffe, v_crn)
v_crn <- as_tibble(v_crn)
v_crn <- dplyr::select(v_crn, Species, Longitude, Latitude, prec_1:tmin_12)
v_crn <- mutate(v_crn, gid = 1:nrow(v_crn)) %>% gather(var, value, -Species, -Longitude, -Latitude)
v_crn <- separate(data = v_crn, col = 'var', into = c('variable', 'month'), sep = '_')
v_crn <- inner_join(v_crn, tibble(month = as.character(1:12), month_abb = month.abb), by = 'month')
v_crn <- mutate(v_crn, month_abb = factor(month_abb, levels = month.abb))
s_crn <- v_crn %>% group_by(variable, month_abb) %>% summarise(value = mean(value, na.rm = T)) %>% ungroup()
s_crn <- s_crn %>% spread(variable, value)
s_crn <- s_crn %>% mutate(period = 'Current', gcm = 'Current')

# Extract values - future -------------------------------------------------
prds <- c('2020_2049', '2040_2069')
s_ftr <- purrr::map(.x = 1:length(prds), .f = function(i){

  cat(prds[i], '\t')
  
  rslt <- purrr::map(.x = 1:length(gcms), .f = function(j){
  
    try(expr = {
      
      cat(gcms[j], '\t')
      fls <- grep(prds[i], f_ftr, value = TRUE) %>% 
        grep(paste0('/', gcms[j], '/'), ., value = TRUE) %>% 
        mixedsort() %>% 
        as.character()
      stk <- terra::rast(fls)
      vls <- terra::extract(stk, cffe[,c('Longitude', 'Latitude')])
      vls <- cbind(vls, cffe)
      vls <- as_tibble(vls)
      vls <- dplyr::select(vls, Species, Longitude, Latitude, prec_1:tmin_12)
      vls <- gather(vls, var, value, -Species, -Longitude, -Latitude)
      vls <- separate(vls, col = 'var', into = c('variable', 'month'), sep = '_')
      vls <- inner_join(vls, tibble(month = as.character(1:12), month_abb = month.abb), by = 'month')
      vls <- dplyr::select(vls, Longitude, Latitude, variable, month_abb, value)
      vls <- vls %>% group_by(variable, month_abb) %>% dplyr::summarise(value = mean(value)) %>% ungroup() %>% spread(variable, value)
      vls <- mutate(vls, period = prds[i], gcm = gcms[j])
      cat('Done!\n')
      return(vls)
      
    })
      
  })
  
  return(rslt)
  
})
v_ftr <- flatten(s_ftr)
v_ftr <- v_ftr[map_lgl(v_ftr, is.data.frame)]
v_ftr <- bind_rows(v_ftr)
v_ftr <- mutate(v_ftr, period = factor(period, levels = c('2020_2049', '2040_2069')), month_abb = factor(month_abb, levels = month.abb))

# Median
m_ftr <- v_ftr %>% group_by(month_abb, period) %>% dplyr::summarise(prec = median(prec, na.rm = T), tmax = median(tmax, na.rm = T), tmean = median(tmean, na.rm = T), tmin = median(tmin, na.rm = T)) %>% ungroup()
m_ftr <- m_ftr %>% mutate(month_abb = factor(month_abb, levels = month.abb), period = factor(period, levels = c('2020_2049', '2040_2069')))

# Average
s_ftr <- v_ftr %>% group_by(period, month_abb) %>% dplyr::summarise(prec = mean(prec, na.rm = T), tmax = mean(tmax, na.rm = T), tmean = mean(tmean, na.rm = T), tmin = mean(tmin, na.rm = T)) %>% ungroup()
s_ftr <- mutate(s_ftr, month_abb = factor(month_abb, levels = month.abb))

# Join all the tables into only one ---------------------------------------
s_crn <- dplyr::select(s_crn, period, month_abb, prec, tmax, tmean, tmin)
s_all <- rbind(s_crn, s_ftr)
write.csv(s_all, './tbl/data_climatogram.csv', row.names = FALSE)
s_all <- s_all %>% mutate(period = factor(period, levels = c('Current', '2020_2049', '2040_2069')))

# To make the graph -------------------------------------------------------

font_add_google('Roboto Condensed', 'Roboto Condensed')
showtext_auto()

colourWidget()

# Precipitation
gbr <- ggplot() + 
  geom_col(data = s_all %>% filter(period == 'Current'), aes(x = month_abb, y = prec, fill = 'C', group = period), stat = 'identity', position = 'dodge') + 
  scale_fill_manual(labels = c('C' = 'Prec.'), values = c('C'  = '#288249'), name = 'Precipitation') +
  geom_point(data = m_ftr %>% filter(period == '2040_2069'), aes(x = month_abb, y = prec, group = period, size = 'A'), colour = 'grey60') +
  geom_point(data = v_ftr %>% filter(period == '2040_2069'), aes(x = month_abb, y = prec, group = period, size = 'B'), colour = 'grey60') +
  scale_size_manual(name = ' ', values = c('A' = 6, 'B' = 3), labels = c('A' = 'Future (median)', 'B' = 'Future (GCM)')) +
  # geom_point(aes(y = median))
  labs(x = '', y = 'Precipitation', fill = 'Period') + 
  theme_minimal() + 
  theme(axis.text.x = element_text(family = 'Roboto Condensed', size = 26), 
        axis.text.y = element_text(family = 'Roboto Condensed', size = 26), 
        axis.title.x = element_text(family = 'Roboto Condensed', size = 30), 
        axis.title.y = element_text(family = 'Roboto Condensed', size = 30), 
        legend.text = element_text(family = 'Roboto Condensed', size = 36), 
        legend.title = element_text(family = 'Roboto Condensed', size = 45), 
        legend.position = 'bottom')

pr <- pull(s_all, prec)
tm <- pull(s_all, tmean) / 10
rl <- mean(pr) / mean(tm) * 2
rl <- 15

# Temperature
gln <- gbr + 
  geom_line(data = s_all, aes(x = month_abb, y = tmin / 10 * rl, colour = period, group = period, linetype = 'D'), size = 1.2) +
  geom_line(data = s_all, aes(x = month_abb, y = tmean / 10 * rl, colour = period, group = period, linetype = 'E'), size = 1.2) +
  geom_line(data = s_all, aes(x = month_abb, y = tmax / 10 * rl, colour = period, group = period, linetype = 'D'), size = 1.2) +
  scale_color_manual(name = 'Temperature',
                     values = c('2020_2049' = '#63BE5C', 'Current' = '#009933', '2040_2069' = '#67C27E'),
                     labels = c('2020_2049' = '2020-2049', '2040_2069' = '2040-2069', 'Current' = 'Current'),
                     breaks = c('Current', '2020-2049', '2040-2069')) +
  scale_y_continuous(sec.axis = sec_axis(~./rl, name = 'Temperature ºC')) +
  scale_linetype_manual(name = ' ', 
                        values = c("D" = 2, 'E' = 1), 
                        labels = c("D" = "Min. and Max.", 'E' = 'Mean'))  + 
  ggtitle(label = paste0('Climatogram for coffee farms in Kenya')) +
  guides(linetype = guide_legend(nrow = 2, keywidth = 3, order = 4, title.position = 'top', size = 15),
         color = guide_legend(nrow = 2, keywidth = 3, order = 3, title.position = 'top', size = 15),
         fill = guide_legend(order = 1, title.position = 'top', size = 15),
         size = guide_legend(order = 2, nrow = 2, title.position = 'top', size = 15)) + 
  theme(plot.title = element_text(size = 50, hjust = 0.5, family = 'Roboto Condensed', face = 'bold'))

ggsave(plot = gln, filename = './png/graphs/climatogram.png', units = 'in', width = 8, height = 6.5, dpi = 300)
