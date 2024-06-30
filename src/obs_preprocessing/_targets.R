# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tidyverse", "sf", "stringr", "rgbif", "stringdist", "network", 
               "proxy", "igraph", "GGally", "rjson", "Polychrome", "rlang",
               "DT", "plotly", "sf", "leaflet", "RColorBrewer"),
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source(c("common_funcs.R", "sources_checks.R", "species_preparation.R"))
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  # =========================================================================== #
  # Species preparation                                                      ####
  # =========================================================================== #
  tar_target(
    name = path_deps,
    command = get_path_deps(),
    format = "qs",
    cue = tar_cue(mode = "always"),
    priority = 1
  ),
  tar_target(
    name = species_files,
    command = get_species_files(path_deps),
    format = "qs",
    cue = tar_cue(mode = "always"),
    priority = 1
  ),
  tar_target(
    name = fix_typos,
    command = fix_names(path_deps, species_files),
    format = "qs",
    cue = tar_cue(mode = "always"),
    priority = 1
  ),
  tar_target(
    name = species_data,
    command = get_spec_data(species_files, fix_typos),
    format = "qs",
    cue = tar_cue(mode = "always"),
    priority = 1
  ),
  tar_target(
    name = spec_plot_names,
    command = get_plot_names(species_files, species_data),
    format = "qs"
  ),
  # Check species data files ---------------------------
  tar_target(
    name = species_data_checks_plot_names,
    command = check_species_plot_names(spec_plot_names),
    format = "qs"
  ),
  tar_target(
    name = species_data_checks_plots_duplicates,
    command = check_plots_duplicates(spec_plot_names, species_data_checks_plot_names),
    format = "qs"
  ),
  # Process scientific names
  tar_target(
    name = species_data_gbif,
    command = check_sci_names(species_data, species_data_checks_plots_duplicates),
    format = "qs"
  ),
  tar_target(
    name = spec_d_verb_clean,
    command = clean_verbatim_names(species_data_gbif),
    format = "qs"
  ),
  tar_target(
    name = spec_d_acc_clean,
    command = clean_accepted_names(species_data_gbif),
    format = "qs"
  ),
  # Join all separate tables
  tar_target(
    name = spec_d_verb_full,
    command = species_join_tab(spec_d_verb_clean),
    format = "qs"
  ),
  tar_target(
    name = spec_d_acc_full,
    command = species_join_tab(spec_d_acc_clean),
    format = "qs"
  ),
  # Compute cooccurrences matrix
  tar_target(
    name = cooccur_matrix_verb,
    command = cooccur_matrix(spec_d_verb_full),
    format = "qs"
  ),
  tar_target(
    name = cooccur_matrix_acc,
    command = cooccur_matrix(spec_d_acc_full),
    format = "qs"
  ),
  # Preapre graph
  tar_target(
    name = graph_verb,
    command = gen_graph(path_deps, spec_d_verb_full, cooccur_matrix_verb),
    format = "qs"
  ),
  # .....
  # =========================================================================== #
  # Coordinates processing                                                   ####
  # =========================================================================== #
  tar_target(
    name = bbox,
    command = get_bbox(path_deps),
    format = "qs",
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    name = locations,
    command = process_coords(path_deps),
    format = "qs",
    cue = tar_cue(mode = "always"),
    priority = 1
  ),
  tar_target(
    name = locations_checks,
    command = check_coords_data(locations, bbox),
    format = "qs",
    cue = tar_cue(mode = "always")
  ),
  # =========================================================================== #
  # Process biomass                                                          ####
  # =========================================================================== #
  tar_target(
    name = biomass,
    command = process_biomass(path_deps, locations, locations_checks),
    format = "qs",
    cue = tar_cue(mode = "always"),
    priority = 1
  ),
  # =========================================================================== #
  # Prepare report                                                           ####
  # =========================================================================== #
  tar_target(
    name = observations_report,
    command = knit_report(path_deps, graph_verb),
    format = "file",
    cue = tar_cue(mode = "always"),
    priority = 0
  )
)
