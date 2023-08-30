# Nombre d'espèces par groupe et par îles ----
lapply(
  islands,
  \(isl) {
    lapply(
      species[[isl]],
      \(tb) {
        tb$aphiaID %>% unique() %>% length()
      }
    )
  }
)

# Carte des évènements de collecte pour la Martinique ----
length(unique(stations$MTQ$collectEvent)) # 503
# Majoidea ----
dpth <- bathy$MTQ 
names(dpth) <- c("Longitude", "Latitude", "Depth")

names(taxa) <- Taxa
lapply(
  Taxa,
  \(Tax) {
    
    tax <- taxa[Tax]
    
    stn <- stations$MTQ %>% 
      filter(
        collectEvent %in% (
          species$MTQ[[tax]] %>%
            select(collectStation) %>% 
            unique() %>% 
            unlist(use.names = F)
        )
      )
    
    print(paste(nrow(stn), "collect events for the", Tax))
    
    p <- ggplot() + 
      geom_tile(data = dpth, aes(x = Longitude, y = Latitude, fill = Depth)) + 
      new_scale_fill() +
      geom_sf(data = maps$MTQ, fill = "lightgreen") +
      geom_sf(
        data = stations$MTQ, shape = 16, size = 3, col = "grey", alpha = 0.3
      ) +
      geom_sf(data = stn, shape = "+", size = 4, col = colors_taxa[[Tax]]) +
      guides(fill = "none")
    
    ggexport(
      p, 
      filename = here(
        "figures", 
        paste(
          "carte", 
          "collectEvent",
          tolower(Tax),
          "mq",
          sep = "_"
        ) %>% paste0(".png")
      ),
      width  = 3000, 
      height = 2800, 
      res = 400
    )
  }
)

# Carte des évènements de collecte pour la Martinique ----
length(unique(stations$MTQ$geometry)) # 372
table(
  duplicated(
    paste(stations$MTQ$decimalLongitude, stations$MTQ$decimalLatitude)
  )
)
paste(stations$MTQ$decimalLongitude, stations$MTQ$decimalLatitude) %>% 
  unique() %>% length()
# Majoidea ----
lapply(
  Taxa,
  \(Tax) {
    
    tax <- taxa[Tax]
    
    spe <- species$MTQ[[tax]] %>%
      filter(
        !duplicated(
          paste(species$MTQ[[tax]]$decimalLongitude, 
                species$MTQ[[tax]]$decimalLatitude)
        )
      )
    
    stn <- stations$MTQ %>% 
      filter(!duplicated(stations$MTQ$geometry)) %>% 
      filter(
        !(
          paste(.$decimalLongitude, .$decimalLatitude) %in%
            paste(spe$decimalLongitude, spe$decimalLatitude)
        )
      )
    
    print(
      paste(nrow(spe), "stations out of", nrow(spe) + nrow(stn), "for the", Tax)
    )
    
    p <- ggplot() + 
      geom_tile(data = dpth, aes(x = Longitude, y = Latitude, fill = Depth)) + 
      new_scale_fill() +
      geom_sf(data = maps$MTQ, fill = "lightgreen") +
      geom_sf(
        data = stn, shape = 16, size = 3, col = "grey", alpha = 0.3
      ) +
      geom_point(
        data = spe, 
        aes(x = decimalLongitude, y = decimalLatitude),
        shape = "+", 
        size = 4, 
        col = colors_taxa[[Tax]]
      ) +
      guides(fill = "none")
    
    ggexport(
      p, 
      filename = here(
        "figures", 
        paste(
          "carte", 
          "stations",
          tolower(Tax),
          "mq",
          sep = "_"
        ) %>% paste0(".png")
      ),
      width  = 3000, 
      height = 2800, 
      res = 400
    )
  }
)
# [1] "205 stations out of 372 for the Majoidea"
# [1] "203 stations out of 372 for the Muricidae"


# Majoidea ----
lapply(
  Taxa,
  \(Tax) {
    
    colloquial_tax <- switch(
      Tax, 
      Majoidea = "majoids", 
      Muricidae = "muricids", 
    )
    
    tax <- taxa[Tax]
    
    spe <- species$MTQ[[tax]] %>%
      filter(
        !duplicated(
          paste(species$MTQ[[tax]]$decimalLongitude, 
                species$MTQ[[tax]]$decimalLatitude)
        )
      )
    
    # stn <- stations$MTQ %>% 
    #   filter(!duplicated(stations$MTQ$geometry)) %>% 
    #   filter(
    #     !(
    #       paste(.$decimalLongitude, .$decimalLatitude) %in%
    #         paste(spe$decimalLongitude, spe$decimalLatitude)
    #     )
    #   )
    
    stn <- stations$MTQ
    stn$Stations <- factor(stn$collectEvent %in% spe$collectStation)
    stn <- stn %>% filter(!duplicated(stn$geometry))
    levels(stn$Stations) <- paste(
      table(stn$Stations), c("without", "with"), colloquial_tax
    )
    
    print(
      paste(nrow(spe), "stations out of", nrow(stn), "for the", Tax)
    )
    
    p <- ggplot() +
      geom_tile(data = dpth, aes(x = Longitude, y = Latitude, fill = Depth)) +
      new_scale_fill() +
      geom_sf(data = maps$MTQ, fill = "lightgreen") +
      geom_sf(
        data = stn, aes(fill = Stations), col = "black", 
        alpha = 0.3, shape = 21, size = 3
      ) +
      scale_fill_manual(values = c("white", colors_taxa[[Tax]])) + 
      guides(fill = guide_legend(order = 2)) + 
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) + 
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()
      )
    p
    # p <- ggplot() + 
    #   geom_tile(data = dpth, aes(x = Longitude, y = Latitude, fill = Depth)) + 
    #   new_scale_fill() +
    #   geom_sf(data = maps$MTQ, fill = "lightgreen") +
    #   geom_sf(
    #     data = stn, shape = 16, size = 3, col = "grey", alpha = 0.3
    #   ) +
    #   geom_point(
    #     data = spe, 
    #     aes(x = decimalLongitude, y = decimalLatitude),
    #     shape = "+", 
    #     size = 4, 
    #     col = colors_taxa[[Tax]]
    #   ) +
    #   guides(fill = "none")
    
    ggexport(
      p, 
      filename = here(
        "figures", 
        paste(
          "carte", 
          "stations",
          tolower(Tax),
          "mq",
          sep = "_"
        ) %>% paste0(".png")
      ),
      width  = 3000, 
      height = 2800, 
      res = 400
    )
  }
)


# # ajout d'une colonne de couleur pour avoir les collectes où des spécimens
# # ont été collectés
# STN <- stations$MTQ
# STN$spcm <- factor(stations$MTQ$collectEvent %in% stn$collectEvent)
# # puis filtre pour n'avoir que des stations
# STN %>% dim()
# STN <- STN %>%
#   filter(
#     !(STN %>%
#         select(decimalLongitude, decimalLatitude) %>%
#         duplicated())
#   )
# STN %>% dim()
# 
# p <- ggplot() +
#   geom_tile(data = dpth, aes(x = Longitude, y = Latitude, fill = Depth)) +
#   new_scale_fill() +
#   geom_sf(data = maps$MTQ, fill = "lightgreen") +
#   geom_sf(data = STN, aes(col = spcm), alpha = 0.3, shape = 21, size = 3) +
#   scale_color_manual(values = c("grey", "red"))