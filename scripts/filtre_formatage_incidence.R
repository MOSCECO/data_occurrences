# Nouvelles tables d'occurrences

# Formatage des données
########################################################################
THRESH <- 0
########################################################################

stations_islands <- stations
stations <- stations %>% 
  append(list(ANT = do.call(rbind, stations)))
stations <- stations[sort(names(stations))]

species_all_incidences <- species
occurrences <- lapply(
  names(species), 
  function(isl) {
    
    path_data_incidence_isl <- here(path_figures_occfor, isl)
    makeMyDir(path_data_incidence_isl)
    
    dataset_threshold <- mapply(
      function(tax, title_tax, col_tax) {
        
        path_data_incidence_isl_tax <- here(path_data_incidence_isl, title_tax)
        makeMyDir(path_data_incidence_isl_tax)
        
        tb <- species[[isl]][[tax]]
        tc <- count(tb, scientificName)
        tc <- tc[order(-tc$n), ]
        tc_threshold <- tc[tc$n >= THRESH, ]
        tb_threshold <- tb %>% 
          filter(scientificName %in% tc_threshold$scientificName)
        tb_split <- split(tb_threshold, f = tb_threshold$scientificName)
        
        tbs_absences <- lapply(
          tb_split,
          function(spe) {
            
            # presences/incidence ----
            path_data_incidence_isl_tax_inci <- here(
              path_data_incidence_isl_tax, "incidence"
            )
            makeMyDir(path_data_incidence_isl_tax_inci)
            
            # spe <- tb_split$`Coralliophila salebrosa`
            # spe <- tb_split$`Mithraculus coryphe`
            
            bn_spe <- unique(spe$scientificName)
            print(bn_spe)
            # sauvegarde
            write.csv(
              spe, 
              here(
                path_data_incidence_isl_tax_inci,
                paste(
                  "incidence",
                  "dataset", 
                  gsub(" ", "-", bn_spe),
                  sep = "_"
                ) %>% paste0(".csv")
              ),
              row.names = F
            )
            
            # absences ----
            path_data_incidence_isl_tax_abse <- here(
              path_data_incidence_isl_tax, "absences"
            )
            makeMyDir(path_data_incidence_isl_tax_abse)
            
            # in the same table format as presences
            absences <- stations[[isl]] %>% 
              filter(!collectEvent %in% spe$collectStation)
            absences <- absences %>% 
              add_column(
                occurrenceID = NA, 
                database = NA, 
                CD_NOM = unique(spe$CD_NOM)[[1]],
                CD_REF = unique(spe$CD_REF)[[1]],
                aphiaID = unique(spe$aphiaID),
                family = unique(spe$family),
                scientificName = unique(spe$scientificName),
                author = unique(spe$author)[[1]],
                coordinateUncertaintyInMeters = NA, 
                individualCount = 0, 
                citation = NA, 
                expedition = NA, 
                basisOfRecord = "absence", 
                institutionCode = NA, 
                collectionCode = NA, 
                catalogNumber = NA
              ) %>% 
              select(
                collectStation = collectEvent, 
                country = country_iso2,
                everything())
            absences <- absences %>% 
              select(all_of(names(spe)))
            
            # suppression des dupliqués
            absences <- absences %>%
              st_drop_geometry() %>% 
              add_column(ABS = TRUE)
            spe <- spe %>% 
              add_column(ABS = FALSE)
            all_occ <- spe %>% 
              rbind(absences)
            all_occ <- all_occ %>% 
              filter(
                !duplicated(
                  all_occ %>% 
                    select(eventDate, decimalLongitude, decimalLatitude)
                )
              )
            absences <- all_occ %>% 
              filter(ABS)
            absences <- absences %>% 
              select(-ABS)
            
            # sauvegarde
            write.csv(
              absences, 
              here(
                path_data_incidence_isl_tax_abse,
                paste(
                  "absence",
                  "dataset", 
                  gsub(" ", "-", bn_spe),
                  sep = "_"
                ) %>% paste0(".csv")
              ),
              row.names = F
            )
            
            return(absences)
          }
        )
        
        list_out <- list(
          incidnc = tb_threshold, 
          absence = do.call(rbind, tbs_absences)
        )
        
        return(list_out)
      },
      as.character(taxa),
      Taxa, 
      colors_taxa,
      SIMPLIFY = F, 
      USE.NAMES = T
    )
    return(dataset_threshold)
  }
)

names(occurrences) <- names(species)

# séparation présences / absences
species <- list(
  ANT = list(
    majo = occurrences$ANT$majo$incidnc,
    muri = occurrences$ANT$muri$incidnc
  ),
  GLP = list(
    majo = occurrences$GLP$majo$incidnc,
    muri = occurrences$GLP$muri$incidnc
  ),
  MTQ = list(
    majo = occurrences$MTQ$majo$incidnc,
    muri = occurrences$MTQ$muri$incidnc
  )
)
absence <- list(
  ANT = list(
    majo = occurrences$ANT$majo$absence,
    muri = occurrences$ANT$muri$absence
  ),
  GLP = list(
    majo = occurrences$GLP$majo$absence,
    muri = occurrences$GLP$muri$absence
  ),
  MTQ = list(
    majo = occurrences$MTQ$majo$absence,
    muri = occurrences$MTQ$muri$absence
  )
)

# sauvegarde en rds
saveRDS(
  species, 
  here(
    "data", 
    "tidy", 
    "occ_threshold", 
    paste("species", paste0("threshold", THRESH), "incidence", sep = "_") %>% 
      paste0(".rds")
  )
)
saveRDS(
  absence, 
  here(
    "data", 
    "tidy", 
    "occ_threshold", 
    paste("species", paste0("threshold", THRESH), "absence", sep = "_") %>% 
      paste0(".rds")
  )
)

# sets of presence/absences
pa <- mapply(
  \(s, a) {
    mapply(
      \(x, y) {
        z <- rbind(x, y)
        return(split(z, f = z$scientificName))
      },
      s,
      a,
      SIMPLIFY = F, 
      USE.NAMES = T
    )
  },
  species, 
  absence, 
  SIMPLIFY = F, 
  USE.NAMES = T
)

# source(here("scripts", "figures_cartes_de_distribution.R"))