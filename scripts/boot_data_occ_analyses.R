# boot data_occ_preparation

# libraries ----
libs_to_call <- list(
  
  "data.table",
  "ggnewscale",
  "ggplot2",
  "ggpubr", 
  "here",
  "patchwork", 
  "purrr",
  "remotes",
  "reshape2",
  "sf", 
  "sp", 
  "stringr",
  "terra",
  "tidyverse",
  "vegan"
  
)

# library calls
lapply(libs_to_call, function(i) {
  
  bool <- is.element(i, .packages(all.available = TRUE))
  
  if (!bool) {
    install.packages(i)
  }
  
  library(i, character.only = TRUE)
  
}
)

# remote libraries ----
# remote_libs_to_call <- list(
#   "RCMEMS"
# )
# 
# github_accounts <- list(
#   "markpayneatwork"
# )
# 
# mapply(
#   function(pckg, usr) {
#     
#     bool <- is.element(pckg, .packages(all.available = TRUE))
#     
#     if (!bool) {
#       remotes::install_github(paste0(usr, "/", pckg))
#     }
#     
#     library(pckg, character.only = TRUE)
#     
#   }, 
#   remote_libs_to_call, 
#   github_accounts, 
#   SIMPLIFY = FALSE
# )

# functions
lapply(
  list.files(
    here("scripts", "FUN"),
    full.names = T
  ),
  source
)

# importations d'autres projets R ----
# data_occ_prep : "data", "raw", "shp"
#                (polygones des îles, masses d'eau DCE, bathymétrie mondiale,
#                stations et masses d'eaux modifiées)
#                 "data", "tidy", "occ"
#                (occurrences formatées à partir des extractions)

# shapefiles ----
sf::sf_use_s2(FALSE)
wgs <- "EPSG:4326"
utm20n <- "EPSG:32620"
islands <- c("GLP", "MTQ")
names(islands) <- islands
Taxa <- c("Majoidea", "Muricidae")
names(Taxa) <- Taxa
taxa <- c("majo", "muri")
names(taxa) <- Taxa
colors_taxa <- c("#d04c4e", "#5765b4")
names(colors_taxa) <- Taxa

# polygones îles ----
maps <- list.files(
  here("data", "raw", "shp", "polygones_iles"),
  pattern = "*.shp", 
  full.names = T
) %>% 
  lapply(st_read)
names(maps) <- islands

# stations & évènements de collectes
stations <- readRDS(
  here("data", "raw", "shp", "stations_me_sf.rds")
)

# masses d'eau de la DCE et artificielles
me <- readRDS(
  here("data", "raw", "shp", "ART_masses_d-eaux", "me.rds")
)

# profondeurs
bathy <- readRDS(
  here("data", "raw", "shp", "bathymetries.rds")
)

r <- terra::rast("/home/borea/Documents/mosceco/r_projects/MOSCECO_L2/data_occ_analyses/data/raw/shp/MNT_FACADE_ANTS_HOMONIM_PBMA/MNT_FACADE_ANTS_HOMONIM_PBMA/DONNEES/MNT_ANTS100m_HOMONIM_WGS84_PBMA_ZNEG.grd")

# profondeurs
extents <- list(
  GLP = list(
    x = c(-62.00949, -60.647),
    y = c(15.65, 16.802)
  ),
  MTQ = list(
    x = c(-61.33667, -60.64367 ),
    y = c(14.27667, 15.02067)
  )
)

R <- rast(
  "/home/borea/Documents/mosceco/r_projects/MOSCECO_L2/species_distribution_modeling/data/tidy/climatologies_mosaic/climatologies_mosaic_pca.tif"
)

# ocurrences d'espèces  ----
species <- list.files(
  here("data", "raw", "occ"),
  pattern = "*.csv", 
  full.names = T
) %>% 
  lapply(read.csv)
names(species) <- islands

# split by taxa
species <- lapply(
  species, 
  function(tb) {
    tb <- tb %>% add_column(
      split = ifelse(tb$family == "Muricidae", "muri", "majo")
    )
    tb_split <- split(tb, f = tb$split)
    tb_out <- lapply(tb_split, function(tb) return(tb %>% select(-split)))
    return(tb_out)
  }
)

# délétion des dupliqués (mêmes coordonnées, même jour)
species_withdups <- species
species <- lapply(
  islands, 
  function(isl) {
    out <- lapply(
      taxa, 
      function(tax) {
        tb <- species[[isl]][[tax]]
        return(
          tb %>% 
            filter(
              !duplicated(
                tb %>% select(aphiaID, eventDate, decimalLongitude, decimalLatitude)
              )
            )
        )
      }
    )
    names(out) <- as.character(taxa)
    return(out)
  }
)

# Liste des espèces par campagne d'exploration
A <- do.call(rbind, species %>% 
  lapply(\(l) do.call(rbind, l))) %>% 
  select(scientificName, expedition) %>% 
  filter(!duplicated(.))
library(ggvenn)
A$expedition <- factor(A$expedition) 
B <- dcast(A, scientificName ~ expedition)
B[, -1] <- apply(B[, -1], 2, \(x) ifelse(is.na(x), FALSE, TRUE))
ggvenn(B)
# 33 + 3 + 11 + 5 = 52 potentiellement profondes
# 39 + 62 + 13 = 114 côtières
cotieres <- B$scientificName[!B$`KARUBENTHOS 2`]
profonde <- B$scientificName[B$`KARUBENTHOS 2`]
write.csv(cotieres, here("data", "analysis", "liste_espece_cotieres.csv"))
write.csv(profonde, here("data", "analysis", "liste_espece_profondes.csv"))
C <- B %>% transmute(KARUBENTHOS = `KARUBENTHOS 2012` | `KARUBENTHOS 2`)
C <- cbind(B$scientificName, C, MADIBENTHOS = B$MADIBENTHOS)
ggvenn(C)

# figures pour datapaper
# source(here("scripts", "summary_for_datapaper.R"))

path_figures_occ <- here("data",  "tidy", "occ")
makeMyDir(path_figures_occ)

# jeu de données commun
source(here("scripts", "jeu_de_donnees_commun.R"))

# jeu de données commun
source(here("scripts", "autochtone_dataset.R"))

# modification du jeu de données en autochtones / partagées
by_islands_species <- species
species <- list(
  ANT = common_species,
  GLP = autoch_species$GLP,
  MTQ = autoch_species$MTQ
)

# Histogrammes
# source(here("scripts", "figures_histogrammes.R"))
# source(here("scripts", "figures_histogrammes_common.R"))

# Cartes
# source(here("scripts", "figures_cartes_de_distribution.R"))

# formatage des données
path_figures_occfor <- here("data",  "tidy", "occ_format")
makeMyDir(path_figures_occfor)

# Jeux de données formatés + cartes
source(here("scripts", "filtre_formatage_incidence.R"))
# source(here("scripts", "figures_cartes_de_distribution.R"))

# dossier des occurrences filtrées au seuil 
path_figures_occthr <- here("data",  "tidy", "occ_threshold")
makeMyDir(path_figures_occthr)

# Jeux de données réduits
source(here("scripts", "filtre_seuil_incidence.R"))
# source(here("scripts", "figures_cartes_de_distribution_seuil.R"))