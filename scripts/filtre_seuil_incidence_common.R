# Nouvelles tables d'occurrences pour les espèces communes

path_figures_occ <- here("data",  "tidy", "occ")
makeMyDir(path_figures_occ)

path_data_incidence_isl <- here(path_figures_occ, "ANT")
makeMyDir(path_data_incidence_isl)

# extraction des jeux de données pour le seuil défini
# pour le moment, 10 occurrences, mais à définir plus objectivement
# 2023-02-22 : Nombre d'occurrences par espèces déterminé par le nombre de 
# variables environnementales nourrit au SDM (au doigt mouillé).
THRESH <- 30

common_species_all_incidences <- common_species
occurrences <- mapply(
  function(tb, tax, title_tax, col_tax) {
    
    path_data_incidence_isl_tax <- here(path_data_incidence_isl, title_tax)
    makeMyDir(path_data_incidence_isl_tax)
    
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
        # spe <- tb_split$`Epialtus longirostris`
        
        bn_spe <- unique(spe$scientificName)
        # print(bn_spe)
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
        absences <- do.call(rbind, stations) %>% 
          filter(!collectEvent %in% spe$collectStation)
        absences <- absences %>% 
          add_column(
            occurrenceID = NA, 
            database = NA, 
            CD_NOM = unique(spe$CD_NOM)[[1]],
            CD_REF = unique(spe$CD_REF)[[1]], 
            aphiaID = unique(spe$aphiaID),
            family = unique(spe$family),
            scientificName = unique(spe$scientificName)[[1]],
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
  common_species, 
  taxa,
  Taxa, 
  colors_taxa,
  SIMPLIFY = F, 
  USE.NAMES = T
)

# séparation présences / absences
species <- list(
    majo = occurrences$majo$incidnc,
    muri = occurrences$muri$incidnc
)
absence <- list(
    majo = occurrences$majo$absence,
    muri = occurrences$muri$absence
)

# sauvegarde en rds
saveRDS(
  species, 
  here("data", "tidy", "occ", "common_species_threshold_incidence.rds")
)
saveRDS(
  absence, 
  here("data", "tidy", "occ", "common_species_threshold_absences.rds")
)