# Études des données

path_figures_diagrammes <- here("figures", "diagrammes_distribution_especes")
makeMyDir(path_figures_diagrammes)

species_a <- species[2:3]

# Enregistrer les figures ?
sauvegarde <- TRUE

# Seuil espèces communes
Th <- 30

# Type de diagramme en sortie
# diagram <- "preston"
diagram <- "whittaker"

# Nombre d'occurrences de chaque espèces
figures_distributions <- lapply(
  islands,
  function(isl) {

    # isl <- "MTQ"
    map_inset <- ggplot() +
      geom_sf(data = maps[[isl]], col = NA, fill = "lightgreen") +
      theme(panel.background = element_rect(fill = "transparent")) +
      theme_void()

    mapply(
      function(tax, title_tax, col_tax) {

        # tax <- "majo"
        # title_tax <- "Majoidea"
        # col_tax <- "#d04c4e"

        tb <- species_a[[isl]][[tax]]
        tc   <- count(tb, scientificName)

        p <- if(diagram == "preston") {

          # données
          t_preston <- tc

          # Diagramme de Preston
          ggplot(data = t_preston, aes(x = n)) +
            geom_histogram(
              binwidth = 1, fill = col_tax, color = "#e9ecef", alpha = 0.9
            ) +
            scale_x_continuous(breaks = seq(1, max(t_preston$n), by = 3)) +
            scale_y_continuous(breaks = seq(0, max(table(t_preston$n)), by = 2)) +
            theme(
              axis.ticks.x = element_blank(),
              panel.grid.major.x = element_line(colour = NA),
              panel.grid.minor.x = element_line(colour = NA),
              plot.title = element_text(size = 20, face = "bold", colour = col_tax),
              # axis.text.x = element_text(
              #   vjust = 17.5, size = 6, face = "bold", colour = "white"
              # )
              axis.text  = element_text(size = 14),
              axis.title = element_text(size = 18, face = "bold")
            ) +
            geom_vline(xintercept = Th, linetype = "dashed") +
            xlab("Nombre d'occurrences de l'espèce") +
            ylab("Nombre d'espèces") +
            labs(
              title = paste(title_tax, ":", nrow(t_preston), "espèces", "autochtones")
            )
        } else if (diagram == "whittaker") {

          # Données
          t_whittaker <- sort(tc$n, decreasing = T) # Ns dans entropart

          # Diagramme de Whittaker
          speciesDistributionMarcon(
            Ns = t_whittaker, distribution = "lnorm", col = col_tax,
            main = paste(
              title_tax, ":", length(t_whittaker), "espèces", "autochtones"
            )
          ) +
            theme(
              axis.ticks.x = element_blank(),
              panel.grid.major.x = element_line(colour = NA),
              panel.grid.minor.x = element_line(colour = NA),
              plot.title = element_text(
                size = 20, face = "bold", colour = col_tax
              ),
              axis.text  = element_text(size = 14),
              axis.title = element_text(size = 18, face = "bold")
            ) +
            geom_hline(yintercept = Th, linetype = "dashed")
        }

        p2 <- p + inset_element(map_inset, 0.55, 0.55, 1.15, 1)

        if (sauvegarde) {
          ggexport(
            p2,
            filename = here(
              path_figures_diagrammes,
              paste(
                "diagramme",
                diagram,
                isl %>% tolower(),
                tax %>% tolower(),
                sep = "_"
              ) %>% paste0(".png")
            ),
            width    = 3000,
            height   = 2000,
            res      = 200
          )
        }

        p_out <- p +
          theme(
            axis.text = element_text(size = 8),
            axis.title = element_text(size = 14, face = "bold")
          ) +
          inset_element(map_inset, 0.52, 0.55, 1.17, 1) #0.5, 0.5, 1.2, 1)

        return(p_out)
      },
      taxa,
      Taxa,
      colors_taxa,
      SIMPLIFY = F,
      USE.NAMES = T
    )
  }
)

# lignes = taxon
# p_distributions <- (figures_distributions[[1]][[1]] /
#   figures_distributions[[1]][[2]]) |
#   (figures_distributions[[2]][[1]] /
#   figures_distributions[[2]][[2]] )

# lignes = îles
p_distributions <- ((figures_distributions[[1]][[1]] /
                      figures_distributions[[2]][[1]]) |
  (figures_distributions[[1]][[2]] /
     figures_distributions[[2]][[2]] )) +
  plot_layout(guides = "collect")


ggexport(
  p_distributions,
  filename = here(
    path_figures_diagrammes,
    paste(
      "diagrammes",
      diagram,
      sep = "_"
    ) %>% paste0(".png")
  ),
  width    = 3000,
  height   = 2000,
  res      = 200
)
