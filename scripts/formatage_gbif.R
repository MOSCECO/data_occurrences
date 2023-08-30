# formatage des données du gbif
# importation des occurrences brutes provenant du GBIF
OCC <- lapply(
  superfamilies,
  \(supfam) {
    
    # supfam <- "Muricoidea"
    
    tbs_out <- here("data", "raw", "occ", "gbif", supfam, "occ") %>% 
      list.files(full.names = T) %>% 
      lapply(read.csv)
    
    v <- here("data", "raw", "occ", "gbif", supfam, "occ") %>% 
      list.files() %>% 
      substr(1, nchar(.) - 4) %>% 
      str_split("_")
    
    vg <- v %>% lapply(\(x) pluck(x, length(x) - 1)) %>% unlist(use.names = F)
    vs <- v %>% lapply(\(x) pluck(x, length(x))) %>% unlist(use.names = F)
    names(tbs_out) <- paste(vg, vs, sep = "_")
    
    return(tbs_out)
  }
)

# nettoyage pour correspondre aux formats du MNHN
occ <- sapply(
  names(OCC), 
  \(supfam) {
    sapply(
      OCC[[supfam]], 
      \(tb) {
        tb %>% 
          nettoyageBase("GBIF") %>% 
          filter(institutionCode != "MNHN") %>% 
          filter(basisOfRecord != "HUMAN_OBSERVATION")
      },
      simplify = F, 
      USE.NAMES = T
    )
  },
  simplify = F, 
  USE.NAMES = T
)

# conversion en simple feature
occ_sf <- sapply(
  names(occ),
  \(supfam) {
    sapply(
      occ[[supfam]], 
      \(tb) {
        st_as_sf(
          tb, 
          coords = c("decimalLongitude", "decimalLatitude"),
          crs = wgs84, 
          remove = FALSE
        )
      },
      simplify = F, 
      USE.NAMES = T
    )
  },
  simplify = F, 
  USE.NAMES = T
)

# filtre des occurrences tombant sur la terre et étendue des coordonnées avec
# le plus d'occurrences

# dossiers de destination
path_gocc <- here("data", "tidy", "global_occ")
makeMyDir(path_gocc)
path_gocc_filtered <- here("data", "tidy", "global_occ_filtered")
makeMyDir(path_gocc_filtered)

# visualisation des graphes ?
show_p <- FALSE

# fonction
occ_sf_reduced <- sapply(
  names(occ_sf), 
  \(supfam) {
    # supfam <- "Majoidea"
    # supfam <- "Muricoidea"
    
    # dossier superfamille
    psf  <- here(path_gocc, supfam)
    makeMyDir(psf)
    psff <- here(path_gocc_filtered, supfam)
    makeMyDir(psff)
    
    mapply(
      \(sf, n) {
        # sf <- occ_sf[[supfam]][[75]]
        # n <- names(occ_sf[[supfam]][75])
        print(n)
        # intersection avec les polygons érodés
        sf$land <- st_intersects(sf, wrld_sf_buffer) %>% lengths()
        p1 <- ggplot() + 
          geom_sf(data = wrld_sf_buffer) + 
          geom_sf(data = sf, aes(col = factor(land))) + 
          labs(title = paste("Intersection avec la terre :", n))
        # filtre des occurrences sur terre
        sf <- sf %>% filter(land == 0) %>% select(-land)
        p2 <- ggplot() + 
          geom_sf(data = wrld_sf) + 
          geom_sf(data = sf) +
          labs(title = paste("Occurrences marines :", n))
        # au sein de quelle emprise se trouve le plus grand nombre 
        # d'occurrences ?
        if(nrow(sf) > 0) {
          # filtre des quantiles 0.05 et 0.95 ou 0.90 pour les coordonnées
          # max
          x1 <- sf$decimalLongitude %>% unique() %>% 
            quantile(0.9) %>% as.numeric()
          x2 <- sf$decimalLongitude %>% unique() %>% 
            quantile(0.95) %>% as.numeric()
          qtx <- if ((x2 - x1) > 50) {0.90} else {0.95}
          y1 <- sf$decimalLatitude %>% unique() %>% 
            quantile(0.9) %>% as.numeric()
          y2 <- sf$decimalLatitude %>% unique() %>% 
            quantile(0.95) %>% as.numeric()
          qty <- if ((y2 - y1) > 50) {0.90} else {0.95}
          
          bbox_qt005 <- st_bbox(
            c(
              xmin = sf$decimalLongitude %>% unique() 
              %>% quantile(0.05) %>% as.numeric(),
              ymin = sf$decimalLatitude %>% unique() %>% 
                quantile(0.05) %>% as.numeric(),
              xmax = sf$decimalLongitude %>% unique() %>% 
                quantile(qtx) %>% as.numeric(),
              ymax = sf$decimalLatitude %>% unique() %>% 
                quantile(qty) %>% as.numeric()
            )
          )
          offset <- 5
          bbox_qt005e <- bbox_qt005 + c(rep(-offset, 2), rep(offset, 2))
          
          sf_out <- sf %>% st_crop(bbox_qt005e)
          p3 <- ggplot() + 
            geom_sf(data = wrld_sf %>% st_crop(st_bbox(sf_out))) + 
            geom_sf(data = sf_out) +
            labs(title = paste("Occurrences filtrées (q95%) :", n))
          
          if (show_p) print({x11(); (p1 / p2) | p3})
        } else { sf_out <- sf }
        
        # sauvegarde .rds
        file_name <- paste("global", "occ", n, sep = "_") %>%
          paste0(".rds")
        saveRDS(sf, here(psf, file_name))
        file_name <- paste("global", "occ", "filtered", n, sep = "_") %>%
          paste0(".rds")
        saveRDS(sf_out, here(psff, file_name))
        
        # sauvegarde .csv
        file_name <- paste("global", "occ", n, sep = "_") %>%
          paste0(".csv")
        write.csv(
          sf %>% 
            st_drop_geometry() %>% 
            select(LON = decimalLongitude, LAT = decimalLatitude) %>% 
            filter(!duplicated(.)), 
          here(psf, file_name), 
          row.names = F
        )
        file_name <- paste("global", "occ", "filtered", n, sep = "_") %>%
          paste0(".csv")
        
        # ajout des dates inconnues pour pointer
        d0 <- "1990-01-01 00:00:00" %>% as.character()
        sf_out$eventDate[is.na(sf_out$eventDate)] <- d0
        write.csv(
          sf_out %>% 
            st_drop_geometry() %>% 
            select(
              TIME = eventDate,
              LAT = decimalLatitude,
              LON = decimalLongitude
            ) %>% 
            filter(!duplicated(.)), 
          here(psff, file_name),
          row.names = F
        )
        
        return(sf_out)
      },
      occ_sf[[supfam]], 
      names(occ_sf[[supfam]]),
      SIMPLIFY = F, 
      USE.NAMES = T
    )
  },
  simplify = F, 
  USE.NAMES = T
)

# On détermine la plus grande emprise correspondant aux occurrences pour chaque
# superfamille qui nous servira de base pour créer une climatologie pour toutes
# les espèces. 
bboxes <- lapply(
  superfamilies, 
  \(supfam) {
    lapply(
      occ_sf_reduced[[supfam]], 
      \(occ) {
        st_bbox(occ)
      }
    )
  }
)
# lapply(
#   superfamilies, 
#   \(supfam) {
#     mapply(
#       \(occ, occ0) {
#         if (nrow(occ) > 0) {
#           bb <- st_bbox(occ)
#           print(
#             ggplot() + 
#               geom_sf(data = wrld_sf %>% st_crop(bb)) + 
#               geom_sf(data = occ, shape = "+", col = "blue") +
#               labs(
#                 title = unique(occ$scientificName), 
#                 subtitle = paste0(
#                   nrow(occ), " occurrences filtrées ", 
#                   "(", nrow(occ0), " occurrences initiales)"
#                 )
#               )
#           )
#           invisible(readline(prompt = "Press [enter] to continue"))
#         }
#       }, 
#       occ_sf_reduced[[supfam]][order(names(occ_sf_reduced[[supfam]]))], 
#       occ_sf[[supfam]][order(names(occ_sf[[supfam]]))],
#       SIMPLIFY = FALSE, 
#       USE.NAMES = TRUE
#     )
#   }
# )

# Vérification de la distribution pour une seule espèce ----
supfam <- "Muricoidea"
occ <- occ_sf_reduced[[supfam]][["Siratus_motacilla"]]
occ0 <- occ_sf[[supfam]][["Siratus_motacilla"]]
bb <- st_bbox(occ)
ggplot() + 
  geom_sf(data = wrld_sf %>% st_crop(bb)) + 
  geom_sf(data = occ, shape = "+", col = "blue") +
  labs(
    title = unique(occ$scientificName), 
    subtitle = paste0(
      nrow(occ), " occurrences filtrées ", 
      "(", nrow(occ0), " occurrences initiales)"
    )
  )

# Trois climatologies à faire : autour des côtes de l'amérique latine, 
# + Europe pour Stramonita haemastoma
# + Cap vert et pacifique sud-ouest pour Cytharomorula grayi

# sélection de l'emprise pour l'Amérique latine
occs <- Reduce(append, occ_sf_reduced)
occs_ameriques <- sapply(
  names(occs), 
  \(n) {
    sf <- occs[[n]]
    print(n)
    if(nrow(sf) > 0) {
      b <- c(-130, min(sf$decimalLatitude), -30, max(sf$decimalLatitude))
      names(b) <- c("xmin", "ymin", "xmax", "ymax")
      st_crop(sf, b)
    }
  },
  USE.NAMES = T, 
  simplify = F
)

# Création du vecteur qui servira à sélectionner les zones pour lesquelles 
# on veut une climatologie
am <- do.call(rbind, occs_ameriques) 
ggplot() + 
  geom_sf(data = wrld_sf %>% st_crop(st_bbox(am))) + 
  geom_sf(data = am, shape = "+", col = "blue")
am_buffer <- am %>% 
  st_transform("EPSG:4087") %>% 
  st_buffer(50000) %>% 
  st_union() %>% 
  st_transform("EPSG:4326")
wrld_sf_union <- wrld_sf %>% 
  st_union()
ggplot() + 
  geom_sf(data = wrld_sf_union %>% st_crop(st_bbox(am_buffer))) + 
  geom_sf(data = am_buffer, fill = "red", alpha = 0.5)
am_buffer_crop <- st_difference(am_buffer, wrld_sf_union)
ggplot() +
  geom_sf(data = am_buffer_crop)
st_write(
  am_buffer_crop, 
  here("data", "tidy", "buffer_climatology_species.shp")
)