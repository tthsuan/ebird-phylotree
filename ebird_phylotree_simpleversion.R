install.packages(c("readr", "dplyr", "ape", "phytools", "taxize", "rotl", "tidytree"))

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

BiocManager::install(c("ggtree", "treeio", "ggtreeExtra"))


# Load packages
library(readr)
library(dplyr)
library(rotl)
library(purrr)
library(ape)
library(ggtree)
library(treeio)
library(tidytree)

# Read eBird data
ebird_data <- read_csv("MyEBirdData.csv")

# Extract species list
species_list <- ebird_data %>%
  distinct(`Scientific Name`) %>%
  pull()

# Match to OpenTree
matched <- tnrs_match_names(species_list)
valid_matched <- matched %>% filter(!is.na(ott_id))

# Try to build induced subtree
ott_ids <- valid_matched$ott_id
repeat {
  result <- tryCatch({
    tree <- tol_induced_subtree(ott_ids = ott_ids)
    break
  }, error = function(e) {
    pruned_ids <- stringr::str_extract_all(e$message, "ott[0-9]+")[[1]]
    pruned_ids_num <- as.integer(gsub("ott", "", pruned_ids))
    ott_ids <<- setdiff(ott_ids, pruned_ids_num)
    NULL
  })
}

matched_subset <- valid_matched %>% filter(ott_id %in% ott_ids)
tree$tip.label <- matched_subset$unique_name[match(tree$tip.label, matched_subset$ott_id)]

# Get full bird tree from OpenTree
bird_node <- tnrs_match_names("Aves")
bird_ott_id <- bird_node$ott_id
bird_tree <- tol_subtree(ott_id = bird_ott_id)

# Clean tip labels
bird_species_clean <- gsub("_", " ", gsub("_ott[0-9]+$", "", bird_tree$tip.label))
your_species <- matched_subset$unique_name

# Build tip annotation dataframe
tip_df <- data.frame(
  label = bird_tree$tip.label,
  species_clean = bird_species_clean,
  color = ifelse(bird_species_clean %in% your_species, "#00356B", "white")
)

# Plot only observed species on full tree
ggtree(bird_tree, layout = "circular") %<+%
  filter(tip_df, color == "#00356B") +
  geom_tiplab(aes(label = species_clean), color = "#00356B", size = 1.5, offset = 5) +
  theme(legend.position = "none")

# Print
cat("Total unique species in your eBird data:", length(unique(ebird_data$`Scientific Name`)), "\n")
cat("Species matched to OpenTree taxonomy database:", length(valid_matched$unique_name), "\n")
cat("Species successfully included in induced subtree (OpenTree synthesis tree):", length(matched_subset$ott_id), "\n")
cat("Species highlighted in blue on the full global bird tree:", sum(tip_df$color == "#00356B"), "\n\n")

cat("Note: The number of species highlighted in blue is lower because the full bird tree includes thousands of species globally. Some species from your checklist or induced subtree may not exactly match tip labels in this larger tree, due to taxonomy differences, naming formats, or pruning in OpenTree synthesis versions. All data is from OpenTree sources.")



