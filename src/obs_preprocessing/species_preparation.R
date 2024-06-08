require(tidyverse)
require(rgbif)
require(stringdist)
require(proxy)
require(igraph)
require(rjson)

##################
#    For test    #
##################
# tar_load(species_data)
# foo <- species_data[[5]][species_data[[5]]$spec == "Elytrigia repens (L.) Nevski", ]
# species_data[[5]]$spec[species_data[[5]]$spec == "Elytrigia repens (L.) Nevski"] <- "Elytrigia reppens (L.) Nevski"
# species_data[[5]] <- rbind(species_data[[5]], foo)


check_sci_names <- function(species_data) {
    # In each species data table, check check names against GBIF
    # get accepted names, than join them by full join
    spec_d_gbif <- mapply(function(x, y) {
        gbif_info <- name_backbone_checklist(x$spec) |>
            dplyr::select(c(scientificName, class, order, family, genus, species, synonym, verbatim_name, rank)) |>
            # remove genus form species
            mutate(species = str_remove(species, paste0("^", genus, " ")))

        # If there was not a match for species, i.e. rank column contains non SPECIES values
        # rise an error, print which species was not found and in what file
        if (any(gbif_info$rank != "SPECIES")) {
            not_found <- gbif_info |>
                filter(rank != "SPECIES") |>
                pull(verbatim_name)
            stop(
                sprintf(
                    "The following species were not found in GBIF:\n%s\nin the following files:\n%s",
                    not_found,
                    y
                )
            )
        }
        return(gbif_info)
    }, species_data, names(species_data), SIMPLIFY = FALSE)

    # Get accepted names, the problem is,
    # current name_backbone_checklist does not contains authors for accepted names
    # therefore, perform additional call to backbone with accepted genus and species
    # and derive the author
    spec_d_gbif_acc <- lapply(spec_d_gbif, function(x) {
        accept_names <- paste(x$genus, x$species, sep = " ")
        accept_rgbif_info <- name_backbone_checklist(accept_names) |>
        # Get autthor, i.e. rmove genus and species from accepted scientificName
            mutate(author = str_remove(scientificName, paste0("^", species, " ")))
        x <- x |>
            mutate(author = accept_rgbif_info$author) |>
            rename(sci_name = scientificName) |>
            mutate(sci_name_acc = paste(genus, species, author, sep = " "))
    })

    # Merge species data with accepted names
    spec_d_gbif_merged <- mapply(function(x, y) {
        dplyr::left_join(x, y, by = join_by(verbatim_name == spec))
    }, spec_d_gbif_acc, species_data, SIMPLIFY = FALSE)

    return(spec_d_gbif_merged)
}

##################
#    For test    #
##################
# spec_d_gbif_merged <- check_sci_names(species_data)


clean_verbatim_names <- function(spec_d_gbif_merged) {
    # It is possible, that verbatim names contains typos,
    # in such a case we sill see duplicates (same sci_name, but different verbatim_name)
    # we have to retain only one version. If such pairs occur in single table,
    # sum observation coverage percentage

    # Get all verbatim names ~ scientifi_name pairs
    verbatim_names <- lapply(spec_d_gbif_merged, function(x) {
        return(x |> dplyr::select(c(sci_name, verbatim_name)))
    }) |>
    reduce(rbind) |>
    unique()
    # If there are no duplicates, return original table
    if (!any(table(verbatim_names$sci_name) > 1)) {
        return(spec_d_gbif_merged)
    }
    # Now we know, that there are duplicates, we have to remove them
    # Prepare a table with sci_name ~ vector of verbatim_names
    dupl_sci_names <- which(table(verbatim_names$sci_name) > 1)
    # Prepare a table with sci_name ~ verbatim_name
    dupl_verbatim_names <- lapply(spec_d_gbif_merged, function(x) {
        return(x |> dplyr::select(c(sci_name, verbatim_name)))
    }) |>
        reduce(rbind) |>
        filter(sci_name %in% names(dupl_sci_names)) |>
        group_by(sci_name) |>
        summarise(verbatim_repl = verbatim_name[which.min(stringdist(sci_name, verbatim_name))])
    # Loop over each species table,
    # check if it contains sci_names from dupl_verbatim_names
    # replace verbatim_name with verbatim_repl
    spec_d_gbif_verb <- lapply(spec_d_gbif_merged, function(x) {
        merged_df <- merge(x, dupl_verbatim_names, by = "sci_name", all.x = TRUE) |>
            # Replace verbatim_name with verbatim_repl
            mutate(verbatim_name = ifelse(is.na(verbatim_repl), verbatim_name, verbatim_repl)) |>
            select(-verbatim_repl) |>
            # Sum up observation if there were duplicates in same table
            group_by(across(!where(is.numeric))) |>
            summarise_all(sum, na.rm = TRUE) |>
            as_tibble()
        return(merged_df)
    })
    return(spec_d_gbif_verb)
}

clean_accepted_names <- function(spec_d_gbif_merged) {
    # There could be synonymous names, therefore we will see
    # duplicates in aceppted names
    # here we want to merge duplicated names, and sum up
    # observation coverage percentage
    
    spec_d_gbif_acc <- lapply(spec_d_gbif_merged, function(x) {
        x <- x |>
            # Drop verbatim names and replace sci_name with sci_name_acc
            select(-c(verbatim_name, sci_name, synonym)) |>
            rename(sci_name = sci_name_acc) |>
            # Sum up observation if there were duplicates in same table
            group_by(across(!where(is.numeric))) |>
            summarise_all(sum, na.rm = TRUE) |>
            as_tibble()
        return(x)
    })
    return(spec_d_gbif_acc)
}


##################
#    For test    #
##################
# spec_d_gbif_merged <- clear_verbatim_names(spec_d_gbif_merged)
# spec_d_gbif_acc <- clear_aceppted_names(spec_d_gbif_merged)

# names_names <- lapply(spec_d_gbif_acc, function(x) {
#     return(x |> dplyr::select(c(sci_name)))
# }) |>
#     reduce(rbind) |>
#     unique()

species_join_tab <- function(spec_d_gbif) {
    # Full join all tables
    res <- Reduce(function(x, y) {
        dplyr::full_join(x, y)
    }, spec_d_gbif) |>
        # Replace NA with 0
        mutate_all(~replace(., is.na(.), 0))

    return(res)
}

cooccur_matrix <- function(spec_d_gbif) {
    # Retain only species names
    spec_mat <- spec_d_gbif |>
        dplyr::select(-c(1:11)) |>
        as.matrix()
    # Assign row names as simplified sci_name (first two words)
    rownames(spec_mat) <- sapply(spec_d_gbif$sci_name, function(x) {
        return(paste(strsplit(x, " ")[[1]][1:2], collapse = " "))
    })
    # Calculate cooccurence matrix
    spec_cooccur <- proxy::dist(spec_mat, method = function(x, y) {
        x <- x > 0
        y <- y > 0
        return(sum(x & y))
    })

    # Return
    return(spec_cooccur)
}

gen_graph_html <- function(path_deps, spec_d_gbif, spec_cooccur) {
    # Generate JSON for html demonstration
    spec_cooccur_mat <- as.matrix(spec_cooccur)
    graph <- graph.empty(n = nrow(spec_cooccur_mat))
    
    # Iterate over the matrix and add edges to the graph
    for (i in 1:nrow(spec_cooccur_mat)) {
        for (j in i:ncol(spec_cooccur_mat)) {
            if (spec_cooccur_mat[i, j] != 0) {
                graph <- add_edges(graph, c(i, j), attr = list(weight = spec_cooccur_mat[i, j]))
            }
        }
    }

    spec_names <- sapply(spec_d_gbif$sci_name, function(x) {
        return(paste(strsplit(x, " ")[[1]][1:2], collapse = " "))
    })

    V(graph)$name <- spec_names
    V(graph)$strength <- strength(graph)

    order_group <- spec_d_gbif$order |>
        as.factor()

    # Generate nodes
    nodes <- apply(bind_cols(spec_names, order_group, V(graph)$strength), 1, function(x) {
        list(id = as.character(x[1]), group = as.character(x[2]), str = as.numeric(x[3]))
    })

    # Generate edges
    edge_list <- as_edgelist(graph, names = TRUE) |>
        cbind(E(graph)$weight) |>
        apply(1, function(x) {
            list(source = as.character(x[1]), target = as.character(x[2]), value = as.numeric(x[3]))
        })
    
    graph_list <- list(nodes = nodes, links = edge_list)
    # Convert to json
    graph_json <- rjson::toJSON(graph_list)
    # Write to output json file
    f_name <- paste0(path_deps, "docs/html/graph_json.json")
    write(graph_json, f_name)
}