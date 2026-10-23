library(tidyverse)
library(here)
library(sf)
library(nhdplusTools)
library(rnaturalearth)
source(here('src', 'setup.R'))

# NHDPlus stream order for every MacroSheds stream gauge in CONUS (plus Hawaii
# where the state layer resolves). Non-interactive: run with Rscript.
#
# Output: data_working/ms_stream_order.csv, one row per gauge with the matched
# NHDPlus flowline (comid), its Strahler order, GNIS name and total drainage
# area, the snap distance from the gauge point to the flowline, and a status
# column. Failed lookups are kept as NA so they can be told apart from
# higher-order streams. Sites in the 166-site climate pool are looked up first
# and flagged with in_climate_pool so a partial run is still useful.

# ms gauges ####
climate_pool <- read_csv(here('data_working', 'trends', 'full_prisim_climate.csv'),
                         show_col_types = FALSE) %>%
    distinct(site_code) %>%
    pull(site_code)

gauges <- ms_load_sites() %>%
    filter(site_type == 'stream_gauge') %>%
    distinct(site_code, .keep_all = TRUE) %>%
    select(site_code, domain, ws_status, ws_area_ha, latitude, longitude) %>%
    drop_na(latitude, longitude) %>%
    st_as_sf(coords = c('longitude', 'latitude'), crs = 4326, remove = FALSE)

# state layer to drop Alaska and non-US sites; fall back to a CONUS bounding box
us_only <- tryCatch({
    us_states <- ne_states(country = "United States of America", returnclass = "sf") %>%
        st_transform(crs = 4326) %>%
        select(state_name = name)
    st_join(gauges, us_states) %>%
        filter(!is.na(state_name), state_name != 'Alaska')
}, error = function(e) {
    message('ne_states() failed (', conditionMessage(e), '); using CONUS bounding box')
    gauges %>%
        filter(longitude > -125, longitude < -66, latitude > 24, latitude < 50) %>%
        mutate(state_name = NA_character_)
})

us_only <- us_only %>%
    mutate(in_climate_pool = site_code %in% climate_pool) %>%
    arrange(desc(in_climate_pool), site_code)

message(nrow(us_only), ' gauges to look up (', sum(us_only$in_climate_pool), ' in the climate pool)')

# nhd lookup ####
empty_row <- function(status, comid = NA_integer_) {
    tibble(comid = comid,
           stream_order = NA_integer_,
           nhd_name = NA_character_,
           nhd_totda_km2 = NA_real_,
           snap_dist_m = NA_real_,
           status = status)
}

lookup_order <- function(pt) {
    comid <- try(discover_nhdplus_id(pt), silent = TRUE)
    if (inherits(comid, 'try-error') || is.null(comid) || length(comid) == 0 || is.na(comid[1])) {
        return(empty_row('no_comid'))
    }
    comid <- as.integer(comid[1])
    fl <- try(get_nhdplus(comid = comid), silent = TRUE)
    if (inherits(fl, 'try-error') || is.null(fl) || nrow(fl) == 0) {
        return(empty_row('no_flowline', comid))
    }
    fl <- fl[1, ]
    tibble(comid = comid,
           stream_order = as.integer(fl$streamorde),
           nhd_name = as.character(fl$gnis_name),
           nhd_totda_km2 = as.numeric(fl$totdasqkm),
           snap_dist_m = as.numeric(st_distance(st_transform(pt, 5070),
                                                st_transform(fl, 5070))),
           status = 'ok')
}

out_path <- here('data_working', 'ms_stream_order.csv')
output <- vector('list', nrow(us_only))

for (i in seq_len(nrow(us_only))) {
    pt <- us_only[i, ]
    res <- lookup_order(pt)
    if (res$status != 'ok') {          # one retry after a pause
        Sys.sleep(3)
        res <- lookup_order(pt)
    }
    output[[i]] <- bind_cols(
        st_drop_geometry(pt) %>%
            select(site_code, domain, ws_status, ws_area_ha, latitude, longitude,
                   state_name, in_climate_pool),
        res)
    message(sprintf('%3d/%d %-20s order=%s status=%s', i, nrow(us_only),
                    pt$site_code, res$stream_order, res$status))
    # checkpoint so a partial run is usable
    bind_rows(output) %>%
        mutate(ms_area_km2 = ws_area_ha / 100,
               area_ratio_nhd_to_ms = nhd_totda_km2 / ms_area_km2) %>%
        write_csv(out_path)
}

# summary ####
ms_sites_str_ord <- read_csv(out_path, show_col_types = FALSE)

pool <- ms_sites_str_ord %>% filter(in_climate_pool)
message('\nClimate pool (', nrow(pool), ' sites) by stream order:')
print(table(order = pool$stream_order, useNA = 'ifany'))
message('\nLookup status, climate pool:')
print(table(pool$status))
message('\nClimate-pool sites with order >= 2 or a failed lookup:')
pool %>%
    filter(is.na(stream_order) | stream_order >= 2) %>%
    arrange(desc(stream_order), desc(ms_area_km2)) %>%
    select(site_code, domain, ws_status, ms_area_km2, stream_order, nhd_name,
           nhd_totda_km2, snap_dist_m, status) %>%
    print(n = Inf, width = Inf)
message('\nPossible bad snaps (snap distance > 250 m or NHD drainage area off by > 3x):')
pool %>%
    filter(snap_dist_m > 250 | area_ratio_nhd_to_ms > 3 | area_ratio_nhd_to_ms < 1/3) %>%
    arrange(desc(snap_dist_m)) %>%
    select(site_code, domain, ms_area_km2, stream_order, nhd_name, nhd_totda_km2,
           area_ratio_nhd_to_ms, snap_dist_m) %>%
    print(n = Inf, width = Inf)
