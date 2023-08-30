# Routine occurrences mondiales des espèces
database <- "gbif"
species <- lapply(species, \(tb) {
  tb <- tb %>% 
    add_column(superfamily = ifelse(
      tb$family == "Muricidae", "Muricoidea", "Majoidea" 
    ))
  return(tb %>% split(f = tb$superfamily))
}) %>% 
  transpose() %>% 
  lapply(., \(tbs) {do.call(rbind, tbs)})
# sns <- lapply(species, \(tb) sort(unique(tb$scientificName)))
sns <- lapply(
  species, 
  \(tb) {
    v <- sort(unique(tb$scientificName))
    vkeys <- lapply(
      v, 
      \(bn) {print(bn); rgbif::name_backbone(bn)$usageKey}
    ) %>% unlist(use.names = F)
    tb2 <- tb %>% 
      filter(scientificName %in% v) %>%
      select(aphiaID, superfamily, family, scientificName) %>% 
      filter(!duplicated(.)) %>% 
      group_by(scientificName) %>% 
      arrange(.by_group = T) %>% 
      add_column(usageKey = vkeys, .after = "aphiaID") %>% 
      group_by(family, scientificName) %>% 
      arrange(.by_group = T)
    row.names(tb2) <- NULL
    return(tb2)
  }
)

path_gbif <- here("data", "raw", "occ", "gbif")
makeMyDir(path_gbif)
lapply(
  names(sns), 
  \(n) {path <- here(path_gbif, n); makeMyDir(path)}
)

# création des queries à envoyer sur le GBIF
lapply(
  names(sns), 
  \(superfamily) {
    
    path_query <- here(path_gbif, superfamily, "query")
    makeMyDir(path_query)
    
    gbif_keys <- sns[[superfamily]]$usageKey
    taxa_bn   <- sns[[superfamily]]$scientificName
    
    source(here("scripts", "gbif_query_maker.R"), local = TRUE)
  }
)

print("envoi des requêtes")
# envoi des queries
lapply(
  names(sns),
  \(superfamily) {
    
    path_occurrences <- here(path_gbif, superfamily, "occ")
    makeMyDir(path_occurrences)
    
    queries <- list.files(here(path_gbif, superfamily, "query"))
    outputf <- "occurrences" %>% paste0(queries %>% substr(6, nchar(.)-5))
    binom.names <- str_split(outputf, "_") %>% lapply(pluck, 5) %>% unlist(.) %>% 
      paste(str_split(outputf, "_") %>% lapply(pluck, 6) %>% unlist(.), sep = "_")
    
    source(here("scripts", "gbif_query_poster.R"), local = TRUE)
    
  }
)

