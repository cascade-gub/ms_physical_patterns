# clear environment
#rm(list = ls())
# Load packages.
library(here)
source(here('src', 'setup.R'))
#source(here('src', 'mega_zipper_data.R'))
flag_colors <- c('increasing' = "red", 'decreasing' = 'blue', 'flat' = 'green', 'non-significant' = "grey", 'insufficient data' = 'black')

target_q_trend <- "q_mean"

q_trends <- read_csv(here('data_working', 'trends', 'best_run_prisim.csv')) %>%
    add_flags()%>%
    filter(
        var == target_q_trend
        ) %>%
    select(site_code, q_trend = trend, q_flag = flag)

# Macroshed Groups ####
# read in data####
full_prism_trends <- read_csv(here('data_working', 'trends', 'full_prisim_climate.csv')) %>%
    add_flags() %>%
    select(site_code, var, trend, flag) %>%
    pivot_wider(id_cols = site_code, values_from = c(trend, flag), names_from = var) %>%
    mutate(wetting = case_when(flag_precip_mean == 'increasing' ~ 'W',
                               flag_precip_mean == 'decreasing' ~ 'D',
                               flag_precip_mean == 'non-significant' ~ '-'),
           warming = case_when(flag_temp_mean == 'increasing' ~ 'H',
                               flag_temp_mean == 'decreasing' ~ 'C',
                               flag_temp_mean == 'non-significant' ~ '-'),
           greening = case_when(flag_gpp_CONUS_30m_median == 'increasing' ~ 'G',
                                flag_gpp_CONUS_30m_median == 'decreasing' ~ 'B',
                                flag_gpp_CONUS_30m_median == 'non-significant' ~ '-',
                                is.na(flag_gpp_CONUS_30m_median) ~ '-'),
           grouping = as.factor(paste0(warming, wetting, greening)),
           coarse_grouping = case_when(grouping %in% c('HDG', 'HD-', 'H-G', '-DG', 'HDB', 'H--', '--G', '-D-') ~ '(+)',
                                       grouping %in% c('--B', '-W-', '-WB') ~ '(-)',
                                       grouping %in% c('---') ~ 'no change',
                                       grouping %in% c('HWG', 'HW-', 'H-B', '-DB', '-WG', 'HDB') ~ 'variable')
           ) %>%
    left_join(., ms_site_data, by = 'site_code') %>%
    mutate(grouping_exp = case_when(ws_status == 'experimental' ~ 'EXP',
                                .default = 'NON')) %>%
    left_join(., q_trends, by = 'site_code')

full_prism_trends$grouping <- factor(full_prism_trends$grouping, levels = c('HDG', #strong down
                                                                            'HD-', 'H-G', '-DG', # mid down
                                                                            'H--', '--G', '-D-', # light down
                                                                            '---', # no change
                                                                            'HWG', 'HW-', 'H-B', '-DB', '-WG', 'HDB',# variable
                                                                            '--B', '-W-', #light up
                                                                            '-WB')) # strong up

full_prism_trends$grouping <- factor(full_prism_trends$ws_status, levels = c('experimental', 'non-experimental'))

full_prism_trends %>%
    #select(site_code, grouping, streamflow) %>%
    write_csv(here('data_working', 'site_groupings_by_prsim_trend.csv'))
# plots ####
# figure 2 for the paper
# make plot of warming on x, wetting on y, and gpp as point size or color

gpp_plot <- ggplot(full_prism_trends, aes(x = trend_temp_mean, y = trend_precip_mean, text = paste("Site:", site_code, "<br>Domain:", domain))) +
    # Points with 'non-significant' flag
    geom_point(data = subset(full_prism_trends, flag_gpp_CONUS_30m_median == "non-significant"),
               color = "grey", size = 10) +
    # Points with other flags
    geom_point(data = subset(full_prism_trends, flag_gpp_CONUS_30m_median != "non-significant"),
               aes(color = trend_gpp_CONUS_30m_median, shape = ws_status), size = 10) +
    scale_color_distiller(palette = 'BrBG', direction = 1) +
    theme_few(base_size = 20) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    labs(x = 'Temperature trend (mean annual, degrees C)',
         y = 'Precipitation trend (mean annual, mm)',
         color = 'GPP trend \n (mean, kgC/m^2/d/yr)',
         shape = 'Condition')+
    lims(x = c(-.06, .06),
         y = c(-.04, .04))+
    scale_shape_manual(values = c(17,16), labels = c('experimental', 'non-experimental'))

gpp_plot
library(plotly)
ggplotly(gpp_plot, tooltip = 'text')


limit <- max(abs(full_prism_trends$q_trend), na.rm = T) * c(-1, 1)

q_plot <- ggplot(full_prism_trends, aes(x = trend_temp_mean, y = trend_precip_mean, text = paste("Site:", site_code, "<br>Domain:", domain))) +
    # Points with 'non-significant' flag
    geom_point(data = subset(full_prism_trends, is.na(q_flag)),
               color = "black", size = 7, shape = 4 ) +
    geom_point(data = subset(full_prism_trends, q_flag == "non-significant"),
               color = "grey", size = 7, aes(shape = ws_status)) +
    # Points with other flags
    geom_point(data = subset(full_prism_trends, q_flag != "non-significant"),
               aes(color = q_trend, shape = ws_status), size = 7) +
    #scale_color_distiller(palette = 'RdBu', direction = 1, limit = limit) +
    scale_color_gradientn(colors = c(  "#b2182b", "#d6604d", "#4393c3", "#2166ac"),  # No white
                          values = rescale(c(min(limit), 0, max(limit))),  # Centered around 0
                          limits = limit)+
    scale_shape_manual(values = c(17,16), labels = c('experimental', 'non-experimental'))+
    theme_few(base_size = 20) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    lims(x = c(-.06, .06),
         y = c(-.04, .04))+
    labs(x = 'Temperature trend (mean annual, degrees C)',
         y = 'Precipitation trend (mean annual, mm)',
         color = 'Q trend (mean, mm/yr)',
         shape = 'Condition')

q_plot
ggplotly(q_plot, tooltip = 'text')

full_prism_trends %>%
    filter(ws_status == 'non-experimental') %>%
ggplot(., aes(x = trend_temp_mean, y = trend_precip_mean, text = paste("Site:", site_code, "<br>Domain:", domain))) +
    # Points with 'non-significant' flag
    geom_point(data = subset(full_prism_trends %>%
                                 filter(ws_status == 'non-experimental'), is.na(q_flag)),
               color = "black", size = 10, shape = 4 ) +
    geom_point(data = subset(full_prism_trends %>%
                                 filter(ws_status == 'non-experimental'), q_flag == "non-significant"),
               color = "grey", size = 10) +
    # Points with other flags
    geom_point(data = subset(full_prism_trends %>%
                                 filter(ws_status == 'non-experimental'), q_flag != "non-significant"),
               aes(color = q_trend, shape = ws_status), size = 10) +
    #scale_color_distiller(palette = 'RdBu', direction = 1, limit = limit) +
    scale_color_gradientn(colors = c(  "#b2182b", "#d6604d", "#4393c3", "#2166ac"),  # No white
                          values = rescale(c(min(limit), 0, max(limit))),  # Centered around 0
                          limits = limit)+
    theme_few(base_size = 20) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    lims(x = c(-.06, .06),
         y = c(-.04, .04))+
    labs(x = 'Temperature trend (mean annual, degrees C)',
         y = 'Precipitation trend (mean annual, mm)',
         color = 'Q trend (mean, mm/yr)')

## bar charts  by group ####
ggplot(full_prism_trends, aes(x = coarse_grouping, fill = q_flag))+
    geom_bar()+
    theme_few(base_size = 20)+
    scale_fill_manual(values = flag_colors)+#,
    labs(fill = target_q_trend)

ggplot(full_prism_trends, aes(x = coarse_grouping, fill = q_flag))+
    geom_bar()+
    theme_few(base_size = 20)+
    theme(axis.text.x = element_text(angle = 45, vjust = .5, hjust = .5))+
    scale_fill_manual(values = flag_colors)+#,
    facet_wrap(~grouping_exp) +
    labs(fill = target_q_trend)


full_prism_trends %>%
    filter(grouping_exp == 'EXP',
           q_flag == 'decreasing') %>%
    select(domain, site_code)

full_prism_trends %>%
    filter(grouping_exp == 'NON',
           q_flag == 'increasing') %>%
    select(domain)

full_prism_trends %>%
    filter(grouping_exp == 'NON',
           streamflow == 'decreasing') %>%
    select(domain)

View(full_prism_trends %>%
    filter(grouping == 'NNN',
           streamflow == 'decreasing') %>%
    select(domain, site_code, ws_status))


metrics <- readRDS(here('data_working', 'discharge_metrics_siteyear.RDS')) %>%
    distinct()

metrics %>%
    filter(site_code == 'GSWS06', water_year > 1980) %>%
    ggplot(aes(x = water_year, y = gpp_CONUS_30m_median))+
    geom_point()

## hydrographs ####
q_data <- ms_load_product(prodname = 'discharge', macrosheds_root = my_ms_dir)

group_counts <- full_prism_trends %>%
    filter(streamflow != 'data limited') %>%
    group_by(grouping) %>%
    summarize(n = n())

options(scipen = 999) # turn off scientific notation
q_data %>%
    right_join(., full_prism_trends, by = 'site_code') %>%
    right_join(., group_counts) %>%
    filter(val != 0) %>%
    mutate(facet_lab = paste0(grouping, ', n=', n)) %>%
    ggplot(aes(x = date, y = val, color = site_code))+
        geom_line()+
        scale_y_log10()+
    #geom_label_repel(aes(label = domain))+
    theme_few(base_size = 20)+
    scale_color_viridis(discrete = T)+
    theme(#axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.position = 'none',
          )+
    facet_wrap(~facet_lab, scales = 'fixed')


# Grid Groups ####

# handle setup
library(RColorBrewer)
source(here('src', 'setup.R'))
library(tmap)
library(usmap)
# read in full q_metrics.R output
#source(here('src', 'q_metrics.R'))

grid_metrics <- read_csv(here('data_raw', 'grid_csvs', 'et2.csv')) %>%
    full_join(.,  read_csv(here('data_raw', 'grid_csvs', 'gpp.csv')), by = c('FID', 'year')) %>%
    full_join(.,  read_csv(here('data_raw', 'grid_csvs', 'pdsi.csv')), by = c('FID', 'year')) %>%
    full_join(.,  read_csv(here('data_raw', 'grid_csvs', 'ppt.csv')), by = c('FID', 'year')) %>%
    full_join(.,  read_csv(here('data_raw', 'grid_csvs', 'spei30d.csv')), by = c('FID', 'year')) %>%
    full_join(.,  read_csv(here('data_raw', 'grid_csvs', 'temperature.csv')), by = c('FID', 'year')) %>%
    select(site_code = FID, water_year = year, geometry = geometry.x, ET, GPP, pdsi, ppt, spei30d, tmean) %>%
    pivot_longer(cols = -c('water_year', 'site_code', 'geometry'), names_to = 'var', values_to = 'val')

grid_trends <- grid_metrics %>%
    detect_trends()

#write_csv(grid_trends, here('data_working', 'grid_trends.csv'))
grid_trends <- read_csv(here('data_working', 'grid_trends.csv')) %>%
    add_flags()

# data vis ####
test <- grid_trends %>%
    add_flags()

ggplot(test, aes(x = flag)) +
    geom_bar() +
    facet_wrap(~var, ncol = 1) +
    theme_few()

# data grouping ####
grid_groups <- grid_trends %>%
    select(site_code, var, trend, flag) %>%
    pivot_wider(id_cols = site_code, values_from = c(trend, flag), names_from = var) %>%
    na.omit() %>%
    mutate(wetting = case_when(flag_ppt == 'increasing' ~ 'W',
                               flag_ppt == 'decreasing' ~ 'D',
                               flag_ppt == 'non-significant' ~ '-'),
           warming = case_when(flag_tmean == 'increasing' ~ 'H',
                               flag_tmean == 'decreasing' ~ 'C',
                               flag_tmean == 'non-significant' ~ '-'),
           greening = case_when(flag_GPP == 'increasing' ~ 'G',
                                flag_GPP == 'decreasing' ~ 'B',
                                flag_GPP == 'non-significant' ~ '-',
                                is.na(flag_GPP) ~ '-'),
           grouping = as.factor(paste0(warming, wetting, greening)),
           coarse_grouping = case_when(grouping %in% c('HDG', 'HD-', 'H-G', '-DG', 'HDB', 'H--', '--G', '-D-') ~ '(+)',
                                       grouping %in% c('--B', '-W-', '-WB') ~ '(-)',
                                       grouping %in% c('---') ~ 'no change',
                                       grouping %in% c('HWG', 'HW-', 'H-B', '-DB', '-WG', 'HDB') ~ 'variable')
    )
#
# grid_groups$grouping <- factor(grid_groups$grouping, levels = c('HDG', #strong down
#                                                                             'HD-', 'H-G', '-DG', # mid down
#                                                                             'HDB', 'H--', '--G', '-D-', # light down
#                                                                             '---', # no change
#                                                                             'HWG', 'HW-', 'H-B', '-DB', '-WG', 'HWB', 'C-G', 'CWG', # variable
#                                                                             'C--', '--B', '-W-', #light up
#                                                                             '-WB')) # strong up
# write out data ####
write_csv(grid_groups, here('data_working', 'grid_groups.csv'))
grid_groups <- read_csv(here('data_working', 'grid_groups.csv'))

# read in ms data ####
ms_groups <- read_csv(here('data_working', 'site_groupings_by_prsim_trend.csv')) %>%
    st_as_sf(coords = c("longitude","latitude"), crs = 4326)

# munge ms and grid together ####
ms_temp <- ms_groups %>%
    mutate(source = 'ms') %>%
    tibble() %>%
    select(source, coarse_grouping)


grid_temp <- grid_groups  %>%
    mutate(source = 'grid') %>%
    tibble()%>%
    select(source, coarse_grouping)

both_groups <- rbind(ms_temp, grid_temp) %>% na.omit()

both_groups %>%
    group_by(source, coarse_grouping) %>%
    summarize(n =n()) %>%
    mutate(density = case_when(source == 'grid' ~ n/nrow(grid_temp),
                               source == 'ms' ~ n/nrow(ms_temp))) %>%
    ggplot(aes(y = coarse_grouping, x = density, fill = source))+
    #geom_density(linewidth=2)+
    geom_col(position = 'dodge')+
    theme_few(base_size = 20)+
    labs(y = 'Net Water Demand Group',
         x = 'Density',
         fill = 'Dataset')+
    scale_fill_manual(labels = c('Grid', 'MacroSheds'), values = c('blue', 'black'))


# gpp scatter
# these are annual rn, need to make mean
grid_groups$trend_ppt_scaled <- grid_groups$trend_ppt/365

ggplot(grid_groups, aes(x = trend_tmean, y = trend_ppt_scaled)) +
    # Points with 'non-significant' flag
    geom_point(data = subset(grid_groups, flag_GPP == "non-significant"),
               color = "grey", size = 2) +
    # Points with other flags
    geom_point(data = subset(grid_groups, flag_GPP != "non-significant"),
               aes(color = trend_GPP), size = 5) +
    scale_color_distiller(palette = 'BrBG', direction = 1) +
    theme_few(base_size = 20) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    labs(x = 'Temperature trend (mean annual, degrees C)',
         y = 'Precipitation trend (mean annual, mm)',
         color = 'GPP trend (mean annual)')


# maps ####
lat_lon_pattern <- "c\\((-?\\d+\\.\\d+),\\s*(-?\\d+\\.\\d+)\\)"


map_data <- grid_metrics %>%
    select(site_code, geometry) %>%
    distinct() %>%
    mutate(
        Longitude = as.numeric(gsub("c\\((.*),.*", "\\1", geometry)),
        Latitude = as.numeric(gsub("c\\(.*,(.*)\\)", "\\1", geometry))
    ) %>%
    left_join(grid_groups, by = 'site_code', relationship = 'many-to-many') %>%
    na.omit() %>%
    st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326)



tm_shape(usmap::us_map(exclude = c('AK', 'HI'))) +
    tm_polygons() + # Base US map
    tm_shape(map_data) +
    tm_symbols(
        col = "trend_tmean", # Color by effect size
        title.col = "Temperature (C/yr)",
        #shape = "significant", # Different shape for significant points
        #shapes = c(21, 24), # 21 (circle), 24 (cross-hatch-like triangle)
        border.col = "black",
        size = 0.1,
        palette = "-RdBu", # Diverging palette for effect size
        legend.col.show = TRUE,
        border.alpha = 0,
        style = 'cont'
    ) +
    tm_shape(ms_groups)+
    tm_symbols(col = 'black',
               title.col = 'MS sites',
               legend.col.show = TRUE,
               size = .2)+
    # tm_symbols(
    #     col = "trend_precip_mean", # Color by effect size
    #     title.col = "Sen's slope",
    #     #shape = "significant", # Different shape for significant points
    #     #shapes = c(21, 24), # 21 (circle), 24 (cross-hatch-like triangle)
    #     border.col = "black",
    #     size = 0.5,
    #     palette = "-RdBu", # Diverging palette for effect size
    #     legend.col.show = TRUE#,
    #     #legend.shape.show = TRUE
    # )  +
    tm_layout(
        legend.outside = TRUE,
        legend.outside.position = "right",
        frame = F,
        legend.title.size = 1,
        legend.text.size = .9,
        legend.outside.size = .4
    )


tm_shape(usmap::us_map(exclude = c('AK', 'HI'))) +
    tm_polygons() + # Base US map
    tm_shape(map_data) +
    tm_symbols(
        col = "trend_ppt", # Color by effect size
        title.col = "PPT (mm/yr)",
        #shape = "significant", # Different shape for significant points
        #shapes = c(21, 24), # 21 (circle), 24 (cross-hatch-like triangle)
        border.alpha = 0,
        palette = "Spectral", # Diverging palette for effect size
        legend.col.show = TRUE,
        size = .1,
        style = 'cont'
        #legend.shape.show = TRUE
    ) +
    tm_shape(ms_groups)+
    tm_symbols(col = 'black',
               title.col = 'MS sites',
               legend.col.show = TRUE,
               size = .2)+
    # tm_symbols(
    #     col = "trend_precip_mean", # Color by effect size
    #     title.col = "Sen's slope",
    #     #shape = "significant", # Different shape for significant points
    #     #shapes = c(21, 24), # 21 (circle), 24 (cross-hatch-like triangle)
    #     border.col = "black",
    #     size = 0.5,
    #     palette = "-RdBu", # Diverging palette for effect size
    #     legend.col.show = TRUE#,
    #     #legend.shape.show = TRUE
    # ) +
    tm_layout(
        legend.outside = TRUE,
        legend.outside.position = "right",
        frame = F,
        legend.title.size = 1,
        legend.text.size = .9,
        legend.outside.size = .4
    )

tm_shape(usmap::us_map(exclude = c('AK', 'HI'))) +
    tm_polygons() + # Base US map
    tm_shape(map_data) +
    tm_symbols(
        col = "trend_GPP", # Color by effect size
        title.col = "GPP (kgC/sqm/d/yr)",
        #shape = "significant", # Different shape for significant points
        #shapes = c(21, 24), # 21 (circle), 24 (cross-hatch-like triangle)
        border.alpha = 0,
        palette = "BrBG", # Diverging palette for effect size
        legend.col.show = TRUE,
        size = .1,
        style = 'cont'
        #legend.shape.show = TRUE
    ) +
    tm_shape(ms_groups)+
    tm_symbols(col = 'black',
               title.col = 'MS sites',
               legend.col.show = TRUE,
               size = .2)+
    tm_add_legend(
        type = "symbol",
        labels = "MS sites", # Legend label
        col = "black", # Color matching the points
        shape = 16, # Default filled circle shape
        size = .5
    ) +
    # tm_symbols(
    #     col = "trend_precip_mean", # Color by effect size
    #     title.col = "Sen's slope",
    #     #shape = "significant", # Different shape for significant points
    #     #shapes = c(21, 24), # 21 (circle), 24 (cross-hatch-like triangle)
    #     border.col = "black",
    #     size = 0.5,
    #     palette = "-RdBu", # Diverging palette for effect size
    #     legend.col.show = TRUE#,
    #     #legend.shape.show = TRUE
    # ) +
    tm_layout(
        legend.outside = TRUE,
        legend.outside.position = "right",
        frame = F,
        legend.title.size = 1,
        legend.text.size = .9,
        legend.outside.size = .4
    )

# map_data %>%
#     filter(var == 'tmean') %>%
#     mapview(., zcol = 'flag')
#
# map_data %>%
#     filter(var == 'ET') %>%
#     mapview(., zcol = 'flag')
#
# map_data %>%
#     filter(var == 'ppt') %>%
#     mapview(., zcol = 'flag')
#
# map_data %>%
#     filter(var == 'pdi') %>%
#     mapview(., zcol = 'flag')
#
# map_data %>%
#     filter(var == 'GPP') %>%
#     mapview(., zcol = 'flag')

pal = mapviewPalette("mapviewTopoColors")
map_data %>%
    mapview(., zcol = 'trend_ET', col.regions=rev(brewer.pal(10, "Spectral")))

map_data %>%
    mapview(., zcol = 'trend_GPP', col.regions=brewer.pal(10, "PiYG"))

map_data %>%
    filter(p_GPP < 0.1) %>%
    mapview(., zcol = 'trend_GPP', col.regions=brewer.pal(10, "PiYG"))


map_data %>%
    mapview(., zcol = 'trend_ppt', col.regions=brewer.pal(10, "Spectral"))

map_data %>%
    filter(p_tmean < 0.05) %>%
    mapview(., zcol = 'trend_tmean', col.regions=rev(brewer.pal(10, "Spectral")))


