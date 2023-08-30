# Méthode dans Guisan et al. 2017
spec_pjs <- lapply(
  clim, 
  \(rs) {
    stk_spec_ef <- get_predictions(spec_ensemble_models_proj_current)
    stk_spec_ef <- subset(
      stk_spec_ef, 
      grep("EMca", names(stk_spec_ef))
    )
    names(stk_spec_ef) <- sapply(
      strsplit(names(stk_spec_ef), "_"), getElement, 2
    )
    return(terra::crop(stk_spec_ef, rs[["chla"]]))
  }
)

spec_pjs_tb <- lapply(
  spec_pjs, \(sr) {
    tb <- as_tibble(
      crds(sr[[1]]) %>% 
        cbind(terra::values(sr[[1]]) %>% na.omit())
    )
    names(tb)[3] <- "value"
    return(tb)
  }
)

spec_pjs_plots <- lapply(
  spec_pjs_tb,
  \(tb) {
    p <- levelplot(
      x = value ~ x * y,
      data = tb, 
      aspect = "iso",
      main = paste(bn, "ensemble projections"), 
      col.regions = colorRampPalette(c("grey90", "yellow4", "green4"))(100)
    )
    x11()
    print(p)
    return(p)
  }
)

x11()
plot(
  climosaic_rast %>% 
    subset(varenv_subset)
)