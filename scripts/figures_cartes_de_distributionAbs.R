# Cartes de distribution avec absences

path_figures_carteAbs <- here("figures", "cartes_de_distribution_absences")
makeMyDir(path_figures_carteAbs)

# Occurrences spatialisées de chaque espèce
offsets_title_tax <- list(
  GLP = c(x = - 1.01,  y = - 0.05),
  MTQ = c(x = - 0.47,  y = - 0.03)
)
dimensions_ggexport <- list(
  GLP = c(width = 2300, height = 2000, res = 200), 
  MTQ = c(width = 1850, height = 2000, res = 200)
)

occurrences_all <- occurrences
occurrences <- occurrences[-1]

figures_cartes_distributions <- mapply(
  function(isl, offs, dims) {
    
    path_figures_carteAbs_isl <- here(path_figures_carteAbs, isl)
    makeMyDir(path_figures_carteAbs_isl)
    
    depths <- bathy[[isl]] 
    depths_bbox <- c(
      xmin = min(depths$x),
      ymin = min(depths$y),
      xmax = max(depths$x),
      ymax = max(depths$y)
    )
    
    p_distribution_taxon <- mapply(
      function(tax, title_tax, col_tax) {
        
        path_figures_carteAbs_isl_tax <- here(
          path_figures_carteAbs_isl, title_tax
        )
        makeMyDir(path_figures_carteAbs_isl_tax)
        
        tb_split_inc <- if (
          !is.null(occurrences[[isl]][[tax]]$absence)
        ) {
          split(
            occurrences[[isl]][[tax]]$incidnc, 
            f = occurrences[[isl]][[tax]]$incidnc$scientificName
          )
        }
        tb_split_abs <- if (
          !is.null(occurrences[[isl]][[tax]]$absence)
        ) {
          split(
            occurrences[[isl]][[tax]]$absence, 
            f = occurrences[[isl]][[tax]]$absence$scientificName
          )
        }
        
        p_distribution_species <- if (
          !is.null(occurrences[[isl]][[tax]]$absence)
        ) { 
          mapply(
            function(spe_inc, spe_abs) {
              
              # spe_inc <- tb_split_inc$`Coralliophila salebrosa`
              # spe_abs <- tb_split_abs$`Coralliophila salebrosa`
              
              bn_spe <- unique(spe_inc$scientificName)
              
              spe_inc <- spe_inc %>% add_column(IC = spe_inc$individualCount > 0)
              spe_abs <- spe_abs %>% add_column(IC = spe_abs$individualCount > 0)
              
              p_out <- ggplot() + 
                geom_tile(
                  data = depths,
                  aes(x = x, y = y, fill = value)
                ) +
                guides(fill = "none") +
                new_scale("fill") +
                geom_sf(data = maps[[isl]], col = NA, fill = "lightgreen") + 
                geom_point(
                  data = spe_abs, 
                  aes(
                    x = decimalLongitude, 
                    y = decimalLatitude, 
                    col = IC
                  ),
                  fill = "lightgrey", 
                  shape = 21, 
                  size = 4,
                  alpha = 0.2
                ) + 
                geom_point(
                  data = spe_inc, 
                  aes(
                    x = decimalLongitude, 
                    y = decimalLatitude,
                    col = IC
                  ),
                  shape = "+", 
                  size = 8
                ) + 
                scale_color_manual(
                  values = c("grey", "#C01417"),
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
                  label = bn_spe, 
                  size = 11,
                  col = "white", 
                  x = depths_bbox[[3]] + offs[[1]],
                  y = depths_bbox[[4]] + offs[[2]]
                )
              
              ggexport(
                p,
                filename = here(
                  path_figures_carteAbs_isl_tax,
                  paste(
                    "carte",
                    "distribution", 
                    isl %>% tolower(),
                    tax %>% tolower(),
                    gsub(" ", "-", bn_spe),
                    sep = "_"
                  ) %>% paste0(".png")
                ),
                width    = dims[[1]],
                height   = dims[[2]],
                res      = dims[[3]]
              )
              
              return(p_out)
            },
            tb_split_inc,
            tb_split_abs,
            SIMPLIFY = F, 
            USE.NAMES = T
          )
        } else {
          NULL
        }
        
        return(p_distribution_species)
      },
      taxa,
      Taxa, 
      colors_taxa,
      SIMPLIFY = F, 
      USE.NAMES = T
    )
    return(p_distribution_taxon)
  },
  islands, 
  offsets_title_tax,
  dimensions_ggexport, 
  SIMPLIFY = F, 
  USE.NAMES = T
)

#  faire les versions synthétiques des cartes
#             -----------
#   |    |    -----------
#   | GLP|    | MTQ|
# ----------- |    |
# -----------  