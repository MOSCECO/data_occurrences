speciesDistributionMarcon <- function(
    Ns,
    distribution = "none",
    pch = ggplot2::GeomPoint$default_aes$shape,
    col = ggplot2::GeomPoint$default_aes$colour,
    cex = ggplot2::GeomPoint$default_aes$size,
    main = NULL,
    xlab = "Rang",
    ylab = "Nombre d'occurrences"
) {
  # https://ericmarcon.github.io/MesuresBioDiv2/
  # Marcon 2022 - entropart
  N <- sum(Ns)
  S <- length(Ns)
  df <- data.frame(Rank = seq_len(S), Ns)

  thePlot <- ggplot() +
    geom_point(
      data = df,
      mapping = aes(x = .data$Rank, y = .data$Ns),
      shape = pch,
      color = col,
      size = cex
    ) +
    # scale_x_continuous(limits = c(0.01, S), expand = c(0, 0)) +
    labs(title = main, x = xlab, y = ylab)

  FittedRACgeom <- entropart::RACgeom(Ns)
  FittedRAClnorm <- entropart::RAClnorm(Ns)
  FittedRACbstick <- entropart::RACbstick(Ns)
  FittedRAClseries <- entropart::RAClseries(Ns)

  thePlot <- thePlot +
    {
      if (distribution == "lnorm" | distribution == "all") {
        list(
          geom_line(
            data = with(FittedRAClnorm, data.frame(Rank, Abundance)),
            mapping = aes(x = .data$Rank, y = .data$Abundance, col = "darkgrey"),
            alpha = 0.7
          ),
          scale_color_manual(values = "darkgrey", labels = "Log-normale")
        )
      }
    }  +
    {
      if (distribution == "lseries" | distribution == "all") {
        list(
          geom_line(
            data    = with(FittedRAClseries, data.frame(Rank, Abundance)),
            mapping = aes(x = .data$Rank, y = .data$Abundance, col = "magenta"),
            alpha = 0.7
          ),
          scale_color_manual(values = "magenta", labels = "Log-séries de Fisher")
        )
      }
    }  +
    {
      if (distribution == "bstick" | distribution == "all") {
        list(
          geom_line(
            data    = with(FittedRACbstick, data.frame(Rank, Abundance)),
            mapping = aes(x = .data$Rank, y = .data$Abundance, col = "yellow"),
            alpha = 0.7
          ),
          scale_color_manual(values = "yellow", labels = "Bâton brisé")
        )
      }
    }  +
    {
      if (distribution == "geom" | distribution == "all") {
        list(
          geom_line(
            data    = with(FittedRACgeom, data.frame(Rank, Abundance)),
            mapping = aes(x = .data$Rank, y = .data$Abundance, col = "green"),
            alpha = 0.7
          ),
          scale_color_manual(values = "green", labels = "Géométrique")
        )
      }
    }  +
  {
    if (distribution == "all") {
      scale_color_manual(
        values = c("darkgrey", "magenta", "yellow", "green"),
        labels = c(
          "Log-normale", "Log-séries de Fisher", "Bâton brisé", "Géométrique"
        )
      )
    }
  } +
  {
    if (distribution != "none") guides(
      col = guide_legend(title = "Distribution")
    )
  } +
    NULL

  thePlot

}
