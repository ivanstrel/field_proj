library(tidyverse)
library(sf)

# Get all GPX filenames
gpx_files <- list.files(
    path = "observations/raw/locations",
    pattern = "*.gpx",
    full.names = TRUE
)

# read GPX files
locations <- lapply(
    gpx_files,
    function(x) {
        gpx <- read_sf(x) |>
            dplyr::select(c(name, time, ele, geometry)) |>
            mutate(patch_id = as.numeric(str_split(name, "_", simplify = TRUE)[, 2])) |>
            mutate(plot_id = as.numeric(str_split(name, "_", simplify = TRUE)[, 3])) |>
            mutate(plot_type = str_split(name, "_", simplify = TRUE)[, 1])
        return(gpx)
    }) |>
    # merge list into single sf
    reduce(rbind)

# save to disk
locations |>
    st_write("processed_obs_data/locations/locations.geojson", delete_dsn = TRUE)