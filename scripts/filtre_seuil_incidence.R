# Nouvelles tables d'occurrences

# extraction des jeux de données pour le seuil défini
# pour le moment, 10 occurrences, mais à définir plus objectivement
# Modification du 01/02/2023 : sauvegarde des données de non détection qui 
# peuvent être assimilées à de "vraies absences" en tables de stations dans une 
# autre liste. 

# Seuil en dessous duquel les modèles d'habitats ne sont plus pertinents
########################################################################
THRESH <- 30
########################################################################

stations_islands <- stations
stations <- stations %>% 
  append(list(ANT = do.call(rbind, stations)))
stations <- stations[sort(names(stations))]

depth_mosaic <- terra::project(
  depth_mosaic, st_as_text(st_crs(stations$ANT))
)

# AJOUT des stations de substitution qui ne correspondent pas aux grilles du 
# GEBCO (profondeurs) sur lesquelles les autres grilles seront basées.

# état initial : quelles sont les stations qui tombent sur la terre ?
r <- subset(r0, "chla")
d <- terra::extract(r, stations$ANT, ID = F)
table(is.na(d))

# on sélectionne le vecteur spatial des stations initiales
svx <- stations$ANT %>% 
  st_drop_geometry() %>% 
  filter(is.na(d)) %>% 
  select(decimalLongitude, decimalLatitude) %>% 
  as_tibble() %>% 
  vect(
    ., geom = c("decimalLongitude","decimalLatitude"), crs = "epsg:4326"
  )
  
# puis la matrice des coordonnées des cellules du raster à extraire
# que l'on convertit en vecteur spatial aussi
svy <- as.data.frame(r, xy = T) %>% 
  select(x, y) %>% 
  as_tibble() %>% 
  vect(., geom = c("x", "y"), crs = "epsg:4326")

# détection des stations les plus proches de celles observées et qui possèdent
# une valeur dans le raster d'entrée
# set.seed(123)
# ngb <- nearest(x = svx, y = svy)
# ngb_val <- ngb %>% values()
# ngb_val <- ngb_val %>% 
#   cbind(
#     stn_id = stations$ANT %>% 
#       st_drop_geometry() %>% 
#       filter(is.na(d)) %>% 
#       select(collectEvent) %>% 
#       row.names(),
#     collectEvent = stations$ANT %>% 
#       st_drop_geometry() %>% 
#       filter(is.na(d)) %>% 
#       select(collectEvent)
#   ) %>% 
#   select(., stn_id, collectEvent, everything())
# write.csv(
#   ngb_val, here("data", "analysis", "stations_nearest.csv"), row.names = F
# )
ngb_val <- read.csv(here("data", "analysis", "stations_nearest.csv"))

# création d'une nouvelle variable de stations en remplaçant les coordonnées
# des stations terrestres par les stations "les plus proches"
s <- stations$ANT %>% st_drop_geometry()
s$decimalLongitude[is.na(d)] <- ngb_val$to_x
s$decimalLatitude[is.na(d)]  <- ngb_val$to_y
s_sf <- st_as_sf(
  s, coords = c("decimalLongitude", "decimalLatitude"), remove = F
)
st_crs(s_sf) <- st_crs(stations$ANT)

# Visualisation des stations concernées
# ggplot() +
#   geom_sf(data = stations$ANT, col = "red", shape = "+") +
#   geom_sf(data = s_sf, col = "green", alpha = 0.7, shape = "+")

# Création du nouvel objet de stations les plus proches
stations_nearest <- list(
  ANT = s_sf,
  GLP = s_sf %>% 
    st_crop(
      st_bbox(
        c(
          xmin = extents$GLP$x[[1]],
          ymin = extents$GLP$y[[1]],
          xmax = extents$GLP$x[[2]],
          ymax = extents$GLP$y[[2]]
        )
      )
    ),
  MTQ = s_sf %>% 
    st_crop(
      st_bbox(
        c(
          xmin = extents$MTQ$x[[1]],
          ymin = extents$MTQ$y[[1]],
          xmax = extents$MTQ$x[[2]],
          ymax = extents$MTQ$y[[2]]
        )
      )
    )
)

# Vérification que chaque station a bien une valeur 
d2 <- terra::extract(r, stations_nearest$ANT, ID = F)
table(is.na(d2))

# Pour toutes les espèces, création d'un jeu de données présences/absences
# avec les coordonnées nouvelles créées plus haut
species_all_incidences <- species
occurrences <- lapply(
  # names(stations_nearest), 
  names(stations),
  function(isl) {
    # isl <- "ANT"
    path_data_incidence_isl <- here(path_figures_occthr, isl)
    makeMyDir(path_data_incidence_isl)
    
    dataset_threshold <- mapply(
      function(tax, title_tax, col_tax) {
        
        # tax     <- taxa[[2]]
        # title_tax    <- names(taxa)[[2]]
        # col_tax <- colors_taxa[[1]]
        
        path_data_incidence_isl_tax <- here(path_data_incidence_isl, title_tax)
        makeMyDir(path_data_incidence_isl_tax)
        
        tb <- species[[isl]][[tax]]
        tc <- count(tb, scientificName)
        tc <- tc[order(-tc$n), ]
        tc_threshold <- tc[tc$n >= THRESH, ]
        tb_threshold <- tb %>% 
          filter(scientificName %in% tc_threshold$scientificName)
        # Changer les stations de présence pour les stations corrigées
        # à partir de l'objet stations_nearest
        # names(ngb_val)[2] <- "collectStation"
        # v <- tb_threshold$collectStation %in% ngb_val$collectStation
        # vs <- tibble(collectStation = tb_threshold[v, "collectStation"])
        # vs <- vs %>% left_join(ngb_val[, c("collectStation", "to_x", "to_y")])
        # tb_threshold[v, c("decimalLongitude", "decimalLatitude")] <- 
        #   vs[, c("to_x", "to_y")]
        
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
            # absences <- stations_nearest[[isl]] %>% 
            absences <- stations[[isl]] %>% 
              filter(!collectEvent %in% spe$collectStation)
            absences <- absences %>% 
              add_column(
                occurrenceID = NA, 
                database = NA, 
                CD_NOM = unique(spe$CD_NOM)[[1]],
                CD_REF = unique(spe$CD_REF),
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
      taxa,
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
    majo = occurrences$ANT$Majoidea$incidnc,
    muri = occurrences$ANT$Muricidae$incidnc
  ),
  GLP = list(
    majo = occurrences$GLP$Majoidea$incidnc,
    muri = occurrences$GLP$Muricidae$incidnc
  ),
  MTQ = list(
    majo = occurrences$MTQ$Majoidea$incidnc,
    muri = occurrences$MTQ$Muricidae$incidnc
  )
)
absence <- list(
  ANT = list(
    majo = occurrences$ANT$Majoidea$absence,
    muri = occurrences$ANT$Muricidae$absence
  ),
  GLP = list(
    majo = occurrences$GLP$Majoidea$absence,
    muri = occurrences$GLP$Muricidae$absence
  ),
  MTQ = list(
    majo = occurrences$MTQ$Majoidea$absence,
    muri = occurrences$MTQ$Muricidae$absence
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
# source(here("scripts", "figures_cartes_de_distribution_seuil.R"))