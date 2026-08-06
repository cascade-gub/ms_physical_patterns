# clear environment
#rm(list = ls())
# Load packages.
library(here)
library(RColorBrewer)
source(here('src', 'setup.R'))

# data made in data_groups.R
ms_groups <- read_csv(here('data_working', 'site_groupings_by_prsim_trend.csv')) %>%
    select(site_code, grouping, coarse_grouping) %>%
    mutate(source = 'ms')
# camels_groups <- read_csv(here('data_working', 'camels_groups.csv'))%>%
#     select(site_code, grouping) %>%
#     mutate(source = 'camels')

grid_groups <- read_csv(here('data_working', 'grid_groups.csv'))%>%
    select(site_code, grouping, coarse_grouping) %>%
    mutate(source = 'grid',
           site_code = as.character(site_code))

groups <- rbind(ms_groups, grid_groups)

ggplot(groups, aes(y = coarse_grouping)) +
    geom_bar()+
    facet_wrap(~source,
               ncol = 1, scales = 'free_y')+
    theme_few()


grid_groups %>%
    mutate(grouping = str_sub(grouping, end = 2)) %>%
    left_join(., ms_site_data, by = 'site_code') %>%
    st_as_sf(., coords = c('longitude', 'latitude'),  crs = 4326) %>%
    mapview(., zcol = 'grouping', col.regions=brewer.pal(5, "Spectral"))

