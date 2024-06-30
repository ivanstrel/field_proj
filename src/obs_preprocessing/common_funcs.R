require(tidyverse)
require(sf)
require(stringr)
require(stringi)

# =========================================================================== #
# Here will be all the common functions for species preparation            ####
# =========================================================================== #

# Get base path
# As targets work from the current wd, we need to know how deep in project we are
get_path_deps <- function() {
    base_path <- getwd()
    # Find, how many '..' are in the path up to field_proj folder
    # split path into folders
    folders <- strsplit(base_path, "/")[[1]]
    tot_depth <- length(folders)
    rel_depth <- which(folders == "field_proj")
    base_depth <- tot_depth - rel_depth

    return(paste0(rep("../", base_depth), collapse = ""))
}

# Get all species data files
get_species_files <- function(path_depth) {
    path <- paste0(path_depth, "observations/preprocessed/species")
    species_files <- list.files(
        path = path,
        pattern = "*.csv",
        full.names = TRUE
    )
    return(species_files)
}

# Replace names with common mistakes
fix_names <- function(path_depth, species_files) {
    # Read file with common substitutions
    f_name <- paste0(path_depth, "observations/preprocessed/common_subs.csv")
    subs <- read_csv(f_name)
    # Find source string in species_files and replace with target string
    apply(subs, 1, function(x) {
        source <- x[1]
        target <- x[2]
        # Read file as text
        lapply(species_files, function(y) {
            dat <- read_file(y)
            # Check if source string is in the file
            if (stri_detect(dat, fixed=source)) {
                message <- sprintf(
                    "\nThe string %s in the file:\n%s\nwas replaced with %s",
                    source, y, target
                )
                cat(message)
                # Replace
                dat <- gsub(source, target, dat, fixe = TRUE)
                # Write
                write_file(dat, y)
            }
            
        })
    })
    return(TRUE)
}

# read all species data files
get_spec_data <- function(species_files, fix_typos) {
    species <- lapply(
    species_files,
    function(x) {
        spec_tab <- read_csv(x)
        # Check that the first column is "spec"
        if (names(spec_tab)[1] != "spec") {
            stop(
                sprintf(
                    "The first column in the following files is not 'spec':\n%s",
                    x
                )
            )
        }
        # Remove leading and trailing spaces, also remove duble spaces
        spec_tab <- spec_tab |>
            mutate(spec = str_squish(spec))
        # Drop duplicated rows, if an entire row is duplicated
        spec_tab <- spec_tab[!duplicated(spec_tab), ]
        # Check if there still duplicated specs, if so, sum values
        spec_tab <- spec_tab |>
            group_by(spec) |>
            summarise_all(sum, na.rm = TRUE)
        return(spec_tab)
    })
    # Assign names
    names(species) <- species_files
    return(species)
}

# Get all plot names from all species data files
get_plot_names <- function(species_files, species_data) {
    plot_names <- lapply(species_data, function(x) {
        return(colnames(x)[-1])
    })
    names(plot_names) <- species_files
    all_names <- unlist(plot_names, use.names = TRUE)
    return(all_names)
}

# =========================================================================== #
# Functions for coordinates processing                                     ####
# =========================================================================== #
get_bbox <- function(path_deps) {
    bbox_path <- paste0(path_deps, "data/bbox/Selected_S2_tiles.geojson")
    bbox <- read_sf(bbox_path) |>
        st_bbox() |>
        st_as_sfc()
    return(bbox)
}

process_coords <- function(path_deps) {
    gpx_folder <- paste0(path_deps, "observations/raw/locations")

    # Get all GPX filenames
    gpx_files <- list.files(
        path = gpx_folder,
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
        }
    ) |>
        # merge list into single sf
        reduce(rbind)

    # Save to geojson
    out_path <- paste0(path_deps, "processed_obs_data/locations/locations.geojson")
    st_write(locations, out_path, delete_dsn = TRUE)
}

# =========================================================================== #
# Process biomass data files                                               ####
# =========================================================================== #

process_biomass <- function(path_deps, locations, checks) {
    bm_file <- paste0(path_deps, "observations/raw/biomass/biomass_obs.csv")
    
    biomass <- read_csv(bm_file) |>
        mutate(plot_id = as.numeric(str_split(plot, "_", simplify = TRUE)[, 3])) |>
        mutate(plot_type = str_split(plot, "_", simplify = TRUE)[, 1]) |>
        mutate(patch_id = str_split(plot, "_", simplify = TRUE)[, 2] |> as.numeric())    
    # Check that all plots names are in locations
    if (!all(biomass$plot %in% locations$name)) {
        err_plots <- biomass$plot[!biomass$plot %in% locations$name]
        stop(
            sprintf(
                "There are plots in the biomass data that are not in the locations data:\n%s",
                paste(err_plots, collapse = ",\n")
            )
        )
    }

    # Calculate biomass
    biomass <- biomass |>
        mutate(bimass = full - package)
    # Add date from locationa
    st_geometry(locations) <- NULL
    biomass <- biomass |>
        left_join(locations |> dplyr::select(name, time), by = c("plot" = "name"))
    
    # Save to file
    out_path <- paste0(path_deps, "processed_obs_data/biomass/biomass.csv")
    write_csv(biomass, out_path)
    return(biomass)
}

# =========================================================================== #
# Knit the ongoing report                                                  ####
# =========================================================================== #
knit_report <- function(path_deps, graph_verb) {
    out_file = paste0(path_deps, "output/reports/observations_report.html")
    rmarkdown::render("report.Rmd", output_file = out_file)
}