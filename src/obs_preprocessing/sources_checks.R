require(tidyverse)
require(testthat)

# =========================================================================== #
# Tests for species data files                                             ####
# =========================================================================== #

# ........................................................................... #
# Check that plots IDs are follow a common template S_XXX_XX               ####
# ........................................................................... #
check_species_plot_names <- function(spec_plot_names) {
    plots_name_err <- sapply(spec_plot_names, function(x) {
        return(grepl("^S_[[:digit:]]{3}_[[:digit:]]{2}$", x))
    })
    if (any(!plots_name_err)) {
        # Get file names where plot ids are not in the right format
        err_file_names <- names(spec_plot_names)[!plots_name_err]
        # Rise an error
        stop(
            sprintf(
                "The plot ids in the following files are not in the right format:\n%s",
                paste(err_file_names, collapse = ",\n")
            )
        )
    }
    return(TRUE)
}

# ........................................................................... #
# Check that there are no duplicated plot ids                              ####
# ........................................................................... #
check_plots_duplicates <- function(spec_plot_names) {
    # Get duplicates
    duplicates <- spec_plot_names[duplicated(spec_plot_names)]
    # Get file names, that contain duplicates
    if (length(duplicates) > 0) {
        dupl_err_str <- lapply(duplicates, function(x) {
            dupl_pos <- which(spec_plot_names == x)
            dupl_f_names <- names(spec_plot_names)[dupl_pos]
            return(dupl_f_names)
        }) |>
            unlist(use.names = FALSE) |>
            unique()
        # Rise an error
        err_str = sprintf(
            "There are duplicated plots ids in the following files:\n%s\nThe duplicated plots ids are:\n%s ",
            paste(dupl_err_str, collapse = ",\n"),
            paste(duplicates, collapse = ",\n")
        )
        stop(err_str)
    }
    return(TRUE)
}


