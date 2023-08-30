# histogrammes espèces communes antilles (guadeloupe + martinique)

path_figures_diagrammes <- here("figures", "diagrammes_distribution_especes")
makeMyDir(path_figures_diagrammes)
species_c <- species[[1]]

# Enregistrer les figures ?
sauvegarde <- TRUE

# Seuil espèces communes
Th <- 30

# Type de diagramme en sortie
diagram <- "preston"
# diagram <- "whittaker"

# Nombre d'occurrences de chaque espèces
figures_distributions <- mapply(
  function(tb, tax, title_tax, col_tax) {

    map_inset_glp <- ggplot() +
      geom_sf(data = maps$GLP, col = NA, fill = "lightgreen") +
      theme(panel.background = element_rect(fill = "transparent")) +
      theme_void()
    map_inset_mtq <- ggplot() +
      geom_sf(data = maps$MTQ, col = NA, fill = "lightgreen") +
      theme(panel.background = element_rect(fill = "transparent")) +
      theme_void()

    tc <- count(tb, scientificName)

    p <- if (diagram == "preston") {

      # données
      t_preston <- tc

      # Diagramme de Preston
      ggplot(data = t_preston, aes(x = n)) +
        geom_histogram(
          binwidth = 1, fill = col_tax, color = "#e9ecef", alpha = 0.9
        ) +
        geom_vline(xintercept = 30, linetype = "dashed") +
        scale_x_continuous(breaks = seq(1, max(t_preston$n), by = 3)) +
        scale_y_continuous(breaks = seq(0, max(table(t_preston$n)), by = 2)) +
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
        xlab("Nombre d'occurrences de l'espèce") +
        ylab("Nombre d'espèces") +
        labs(title = paste(title_tax, ":", nrow(t_preston), "espèces", "partagées"))
    } else if (diagram == "whittaker") {
      # Données
      t_whittaker <- sort(tc$n, decreasing = T) # Ns dans entropart

      # Diagramme de Whittaker
      speciesDistributionMarcon(
        Ns = t_whittaker,
        distribution = "lnorm",
        col = col_tax,
        main = paste(
          title_tax, ":", length(t_whittaker), "espèces", "partagées"
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

    p2 <- p +
      inset_element(map_inset_glp, 0.46, 0.65, 1.10, 1) +
      inset_element(map_inset_mtq, 0.61, 0.4, 1.21, 0.7)

    if (sauvegarde) {
      ggexport(
        p2,
        filename = here(
          path_figures_diagrammes,
          paste(
            "diagramme",
            diagram,
            "common",
            "species",
            "antilles",
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
      inset_element(map_inset_glp, 0.46, 0.65, 1.10, 1) +
      inset_element(map_inset_mtq, 0.61, 0.4, 1.21, 0.7)

    return(p_out)
  },
  species_c,
  taxa,
  Taxa,
  colors_taxa,
  SIMPLIFY = F,
  USE.NAMES = T
)

p_distributions <- ((
  (
    plot_spacer() +
      figures_distributions[[1]] +
      plot_spacer()
  ) +
    plot_layout(widths = c(1, 2, 1))
) / (
  (
    plot_spacer() +
      figures_distributions[[2]] +
      plot_spacer()
  ) +
    plot_layout(widths = c(1, 2, 1))
)) + plot_layout(guides = "collect")

ggexport(
  p_distributions,
  filename = here(
    path_figures_diagrammes,
    paste(
      "diagrammes",
      diagram,
      "common",
      "species",
      "antilles",
      "total",
      sep = "_"
    ) %>% paste0(".png")
  ),
  width    = 3000,
  height   = 2000,
  res      = 200
)
