require(tidyverse)
require(sf)
require(stringr)

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

# read all species data files
get_spec_data <- function(species_files) {
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
# Knit the ongoing report                                                  ####
# =========================================================================== #
knit_report <- function(path_deps) {
    out_file = paste0(path_deps, "output/reports/observations_report.html")
    rmarkdown::render("report.Rmd", output_file = out_file)
}