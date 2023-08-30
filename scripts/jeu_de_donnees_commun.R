# rassemblement des occurrences des espèces en commune pour les deux îles

common_species <- species
common_muri <- intersect(
  unique(species$GLP$muri$aphiaID), 
  unique(species$MTQ$muri$aphiaID)
)
common_majo <- intersect(
  unique(species$GLP$majo$aphiaID), 
  unique(species$MTQ$majo$aphiaID)
)
common_vec <- list(majo = common_majo, muri = common_muri)
common_species <- list(
  majo = rbind(species$GLP$majo, species$MTQ$majo),
  muri = rbind(species$GLP$muri, species$MTQ$muri)
)

common_species <- mapply(
  function(tb, vec) {
    tb_out <- tb %>% filter(aphiaID %in% vec) 
    return(tb_out)
  },
  common_species, 
  common_vec, 
  SIMPLIFY  = F, 
  USE.NAMES = T
)
names(common_species) <- taxa

# sauvegarde
path_data_common <- here(
  "data", "tidy", "occ", "ANT"
)
makeMyDir(path_data_common)

mapply(
  function(tb, tax) {
    write.csv(
      tb, 
      here(
        path_data_common,
        paste(
          "common",
          "dataset", 
          tax,
          sep = "_"
        ) %>% paste0(".csv")
      ),
      row.names = F
    )
  },
  common_species,
  tolower(Taxa),
  SIMPLIFY = F, 
  USE.NAMES = T
)