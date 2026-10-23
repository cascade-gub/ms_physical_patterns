# Export MacroSheds watershed boundaries for the CONUS gauges as GeoJSON, for
# src/modis_gpp_wateryear_gee.py (Earth Engine needs the polygons client-side).
# Output: data_working/ms_ws_boundaries.geojson (WGS84, simplified to ~30 m,
# one feature per site_code).
library(here)
source(here('src', 'setup.R'))
library(sf)

gauges <- read_csv(here('data_working', 'ms_stream_order.csv'), show_col_types = FALSE) %>%
    pull(site_code) %>%
    unique()

wb <- ms_load_spatial_product(my_ms_dir, spatial_product = 'ws_boundary', warn = FALSE) %>%
    filter(site_code %in% gauges) %>%
    st_transform(4326) %>%
    st_make_valid()

# simplify in a projected CRS so the tolerance is in metres, then back to WGS84
wb_simple <- wb %>%
    st_transform(5070) %>%
    st_simplify(dTolerance = 30, preserveTopology = TRUE) %>%
    st_transform(4326) %>%
    select(site_code, geometry)

out <- here('data_working', 'ms_ws_boundaries.geojson')
if (file.exists(out)) file.remove(out)
st_write(wb_simple, out, driver = 'GeoJSON', quiet = TRUE)
message(nrow(wb_simple), ' boundaries written to ', out,
        ' (', sum(!gauges %in% wb_simple$site_code), ' gauges without a boundary: ',
        paste(setdiff(gauges, wb_simple$site_code), collapse = ', '), ')')
message('vertices: ', sum(sapply(st_geometry(wb_simple), function(g) nrow(st_coordinates(g)))))
