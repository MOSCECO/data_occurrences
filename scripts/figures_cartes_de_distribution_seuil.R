# Cartes de distribution avec absences
# Données filtré par le seuil arbitraire de 30 spécimens par espèces

path_figures_carte_pas <- here("figures", "cartes_de_distribution_pa_seuil")
makeMyDir(path_figures_carte_pas)

# Occurrences spatialisées de chaque espèce
offsets_title_tax <- list(
  GLP = c(x = - 1.01,  y = - 0.05),
  MTQ = c(x = - 0.47,  y = - 0.03)
)
dimensions_ggexport <- list(
  ANT = c(width = 4200, height = 2000, res = 200), 
  GLP = c(width = 2300, height = 2000, res = 200), 
  MTQ = c(width = 1850, height = 2000, res = 200)
)

# dataframe of all shared species of more than 30 specimens ----
TBC <- do.call(
  rbind, 
  lapply(
    names(pa), 
    \(isl) {
      do.call(
        rbind, 
        lapply(
          names(pa[[isl]]), 
          \(tax) {
            tb2 <- do.call(
              rbind, 
              lapply(
                pa[[isl]][[tax]],
                \(tb1) {
                  tb1 <- tb1 %>% 
                    add_column(TAX = tax)
                }
              )
            )
            tb2 <- if(!is.null(tb2)) {
              tb2 %>% add_column(ISL = isl) 
            }
          }
        )
      )
    }
  )
)

TBC_list <- TBC %>% split(f = TBC$scientificName)
TBC_list <- TBC_list[sort(names(TBC_list))]

saveRDS(TBC_list, here("data", "tidy", "occ_threshold", "list_occ_thresh.rds"))

figures_cartes_distributions <- lapply(
  names(TBC_list), 
  function(bn) {
    
    tb <- TBC_list[[bn]]
    isl <- unique(tb$ISL)
    tax <- unique(tb$TAX)
    title_tax <- names(taxa[taxa == tax])
    
    path_figures_carte_pas_isl <- here(path_figures_carte_pas, isl)
    makeMyDir(path_figures_carte_pas_isl)
    
    path_figures_carte_pas_isl_tax <- here(
      path_figures_carte_pas_isl, title_tax
    )
    makeMyDir(path_figures_carte_pas_isl_tax)
    
    mq <- if (isl %in% c("MTQ", "ANT")) {
      
      depths <- bathy$MTQ 
      depths_bbox <- c(
        xmin = min(depths$x),
        ymin = min(depths$y),
        xmax = max(depths$x),
        ymax = max(depths$y)
      )
      offs <- offsets_title_tax$MTQ
      tb_sf <- st_as_sf(
        tb, 
        coords = c("decimalLongitude", "decimalLatitude"), 
        remove = F
      )
      tb_sf <- tb_sf %>% st_crop(depths_bbox)
      tb <- tb_sf %>% 
        st_drop_geometry()
      tb$individualCount[tb$individualCount > 0] <- 1
      
      p_out <- ggplot() + 
        geom_tile(
          data = depths,
          aes(x = x, y = y, fill = value)
        ) +
        guides(fill = "none") +
        new_scale("fill") +
        geom_sf(data = maps$MTQ, col = NA, fill = "lightgreen") + 
        geom_point(
          data = subset(tb, individualCount == 0), 
          aes(
            x   = decimalLongitude, 
            y   = decimalLatitude, 
            col = individualCount %>% as.factor()
          ),
          fill = "white", 
          shape = 21, 
          size = 4,
          alpha = 0.2
        ) + 
        geom_point(
          data = subset(tb, individualCount == 1), 
          aes(
            x   = decimalLongitude, 
            y   = decimalLatitude,
            col = individualCount %>% as.factor()
          ),
          shape = "+", 
          size = 8
        ) + 
        scale_color_manual(
          values = c("white", "#C01417"),
          labels = c("Non-détection", "Présence") 
        ) + 
        guides(
          col = guide_legend(
            title = NULL, 
            reverse = T, 
            override.aes = list(
              shape = c(3, 21),
              size = c(6, 12)
            )
          )
        ) + 
        theme(
          axis.title = element_blank(),
          legend.justification = c(1,1), 
          legend.position = c(1,1),
          legend.text = element_text(
            face = "bold", colour = "white", size = 16.5
          ),
          legend.key = element_rect(fill = "transparent"),
          legend.background = element_rect(fill = "transparent")
        ) + 
        scale_x_continuous(expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0))
      # p_out
      p <- p_out  + 
        annotate(
          "text", 
          label = bn, 
          size = 11,
          col = "white", 
          x = depths_bbox[[3]] + offs[[1]],
          y = depths_bbox[[4]] + offs[[2]]
        )
      list(n = p_out, a = p)
    }
    
    tb <- TBC_list[[bn]]
    
    gp <- if (isl %in% c("GLP", "ANT")) {
      
      depths <- bathy$GLP
      depths_bbox <- c(
        xmin = min(depths$x),
        ymin = min(depths$y),
        xmax = max(depths$x),
        ymax = max(depths$y)
      )
      offs <- offsets_title_tax$GLP
      tb_sf <- st_as_sf(
        tb, 
        coords = c("decimalLongitude", "decimalLatitude"), 
        remove = F
      )
      tb_sf <- tb_sf %>% st_crop(depths_bbox)
      tb <- tb_sf %>% 
        st_drop_geometry()
      tb$individualCount[tb$individualCount > 0] <- 1
      
      p_out <- ggplot() + 
        geom_tile(
          data = depths,
          aes(x = x, y = y, fill = value)
        ) +
        guides(fill = "none") +
        new_scale("fill") +
        geom_sf(data = maps$GLP, col = NA, fill = "lightgreen") + 
        geom_point(
          data = subset(tb, individualCount == 0), 
          aes(
            x   = decimalLongitude, 
            y   = decimalLatitude, 
            col = individualCount %>% as.factor()
          ),
          fill = "white", 
          shape = 21, 
          size = 4,
          alpha = 0.2
        ) + 
        geom_point(
          data = subset(tb, individualCount == 1), 
          aes(
            x   = decimalLongitude, 
            y   = decimalLatitude,
            col = individualCount %>% as.factor()
          ),
          shape = "+", 
          size = 8
        ) + 
        scale_color_manual(
          values = c("white", "#C01417"),
          labels = c("Non-détection", "Présence") 
        ) + 
        guides(
          col = guide_legend(
            title = NULL, 
            reverse = T, 
            override.aes = list(
              shape = c(3, 21),
              size = c(6, 12)
            )
          )
        ) + 
        theme(
          axis.title = element_blank(),
          legend.justification = c(1,1), 
          legend.position = c(1,1),
          legend.text = element_text(
            face = "bold", colour = "white", size = 16.5
          ),
          legend.key = element_rect(fill = "transparent"),
          legend.background = element_rect(fill = "transparent")
        ) + 
        scale_x_continuous(expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0))
      # p_out
      p <- p_out  + 
        annotate(
          "text", 
          label = bn, 
          size = 11,
          col = "white", 
          x = depths_bbox[[3]] + offs[[1]],
          y = depths_bbox[[4]] + offs[[2]]
        )
      list(n = p_out, a = p)
    }
    
    p_save <- if(FALSE %in% (list(mq, gp) %>% lapply(is.null) %>% unlist())) {
      (gp$a + theme(legend.position = "none")) + (mq$n + theme(
        axis.title.y = element_blank(), 
        axis.line.y  = element_blank(), 
        axis.text.y  = element_blank(), 
        axis.ticks.y = element_blank()
      ))
    } else {
      list(mq, gp)[!(list(mq, gp) %>% lapply(is.null) %>% unlist())][[1]]$a
    }
    
    dims <- dimensions_ggexport[[isl]]
    
    ggexport(
      p_save,
      filename = here(
        path_figures_carte_pas_isl_tax,
        paste(
          "carte",
          "distribution", 
          isl %>% tolower(),
          tax %>% tolower(),
          gsub(" ", "-", bn),
          sep = "_"
        ) %>% paste0(".png")
      ),
      width    = dims[[1]],
      height   = dims[[2]],
      res      = dims[[3]]
    )
    
  }
)