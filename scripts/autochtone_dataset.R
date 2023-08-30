# rassemblement des occurrences des espèces autochtones aux îles

all_muri <- unique(
  c(
    unique(species$GLP$muri$aphiaID), 
    unique(species$MTQ$muri$aphiaID)
  )
)
common_muri <- intersect(
  unique(species$GLP$muri$aphiaID), 
  unique(species$MTQ$muri$aphiaID)
)
autoch_muri <- setdiff(all_muri, common_muri)

all_majo <- unique(
  c(
    unique(species$GLP$majo$aphiaID), 
    unique(species$MTQ$majo$aphiaID)
  )
)
common_majo <- intersect(
  unique(species$GLP$majo$aphiaID), 
  unique(species$MTQ$majo$aphiaID)
)
autoch_majo <- setdiff(all_majo, common_majo)

# 
autoch_species <- list(
  majo = rbind(species$GLP$majo, species$MTQ$majo), 
  muri = rbind(species$GLP$muri, species$MTQ$muri)
)

autoch_vec <- list(
  majo = autoch_majo,
  muri = autoch_muri
)

autoch_species <- mapply(
  function(tb, vec) {
    tb_out <- tb %>% filter(aphiaID %in% vec)
    tbs_out <- split(tb_out, f = tb_out$country)
    return(tbs_out)
  },
  autoch_species, 
  autoch_vec, 
  SIMPLIFY  = F, 
  USE.NAMES = T
)
names(autoch_species) <- taxa

autoch_species <- list(
  GLP = list(
    majo = autoch_species$majo$GP,
    muri = autoch_species$muri$GP
  ),
  MTQ = list(
    majo = autoch_species$majo$MQ,
    muri = autoch_species$muri$MQ
  )
)

# sauvegarde
lapply(
  islands, 
  function(isl) {
    path_occ_isl <- here("data",  "tidy", "occ", isl)
    makeMyDir(path_occ_isl)
    lapply(
      names(Taxa), 
      \(taxon) {
        
        tax <- ifelse(taxon == "Muricidae", "muri", "majo")
        tb <- autoch_species[[isl]][[tax]]
        
        write.csv(
          tb, 
          here(
            path_occ_isl,
            paste(
              "autoch",
              "dataset", 
              taxon,
              sep = "_"
            ) %>% paste0(".csv")
          ),
          row.names = F
        )
      }
    )
  }
)