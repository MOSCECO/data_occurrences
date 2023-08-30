# boot data_occ_preparation

# libraries ----
libs_to_call <- list(

  "data.table",
  "ggnewscale",
  "ggplot2",
  "ggpubr",
  "ggvenn",
  "here",
  "httr",
  "jsonlite",
  "patchwork",
  "purrr",
  "remotes",
  "reshape2",
  "rgbif",
  "sf",
  "sp",
  "terra",
  "stringr",
  "tidyverse",
  "vegan",
  "worrms"

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
# data_stn_prep : "data", "raw", "shp"
#                (polygones des îles, masses d'eau DCE, bathymétrie mondiale)
#                 "data", "tidy", "shp"
#                (stations et masses d'eaux modifiées)

# shapefiles ----
sf::sf_use_s2(FALSE)
wgs84 <- "EPSG:4326"
utm20n <- "EPSG:32620"
islands <- c("GLP", "MTQ")
names(islands) <- islands
# data_occ_prep
Taxa <- c("muricidae", "majoidea")
names(Taxa) <- Taxa
taxa <- c("muri", "majo")
names(taxa) <- taxa
superfamilies <- sort(c("Muricoidea", "Majoidea"))
names(superfamilies) <- superfamilies
# data_occ_analyses
Taxa <- c("Majoidea", "Muricidae")
names(Taxa) <- Taxa
taxa <- c("majo", "muri")
names(taxa) <- Taxa
colors_taxa <- c("#d04c4e", "#5765b4")
names(colors_taxa) <- Taxa

# gbif id ----
user = "igregman2"
pwd = "BahaMut822?Majoidea"
email = "gregoire.maniel4@mnhn.fr"

# databases informations ----
databases_infos <- list(
  gbif = list(
    name = "gbif",
    download_url = "http://api.gbif.org/v1/occurrence/download/request",
    output_file_extension = "csv",
    output_file_separator = "\t"
  ),
  obis = list(
    name = "obis",
    download_url = "https://api.obis.org/v3/occurrence",
    output_file_extension = "csv",
    output_file_separator = "\t"
  ),
  oobs = here("data", "raw", "swagger.json") %>%
    fromJSON(),
  inat = list(
    name = "iNaturalist",
    output_file_extension = "csv",
    output_file_separator = "\t"
  )
)

# mappemonde
# wrld <- map_data('world') %>% filter(region != "Antarctica") %>% fortify()
# wrld_sf <- wrld %>%
#   split(f = .$group) %>%
#   lapply(
#     ., \(tb) {
#       tb %>% select(long, lat) %>% as.matrix() %>% list() %>% st_polygon()
#     }
#   ) %>%
#   st_sfc(crs = st_crs(maps$GLP))
# st_write(wrld_sf, here("data", "raw", "mappemonde.shp"))
wrld_sf <- st_read(here("data", "raw", "mappemonde.shp"))

# erosion de la mappemonde
wrld_sf_buffer <- wrld_sf %>%
  st_union() %>%
  # st_crop(st_bbox(occ_sf)) %>%
  st_wrap_dateline() %>%
  st_transform(crs = "EPSG:4087") %>%
  st_buffer(-20000) %>%
  st_transform(crs = "EPSG:4326")
# ggplot() + geom_sf(data = wrld_sf_buffer)

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

# folder creation: occurrences ----
p <- here("data", "tidy", "occ")
makeMyDir(p)

# data ----
phyla <- list.files(
  here("data", "raw", "occ"),
  pattern = "*.csv",
  full.names = T
) %>%
  lapply(read.csv, fileEncoding = "UTF-16")
names(phyla) <- c("crusta", "mollsc")
lapply(phyla, dim)

# 2023-03-25
# Pour enrichir la niche écologique observée des espèces, téléchargement
# des occurrences d'espèces pour une répartition mondiale.
# ocurrences d'espèces  ----
species <- here("data", "tidy", "occ") %>%
  list.files(pattern = "invmar_clean", full.names = T) %>%
  lapply(read.csv)
names(species) <- c("GLP", "MTQ")

# data_occ_analyses
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

# Import d'une copie de climatologies comme raster de références
# notamment pour les extractions de valeurs de données environnementales
# à partir des stations de collect des explorations scientifiques du MNHN
# cmd <- paste(
#   "rsync",
#   "-avuc",
#   "--delete",
#   "/home/borea/Documents/mosceco/r_projects/MOSCECO_L2/habitat_suitability/data/tidy/climatologies_mosaic",
#   here("data", "raw") %>% paste0("/")
# )
# system(cmd)

R <- here("data", "raw", "climatologies_mosaic") %>%
  list.files(full.names = T) %>%
  rast()

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

path_figures_occ <- here("data",  "tidy", "occ")
makeMyDir(path_figures_occ)

# formatage des données
path_figures_occfor <- here("data",  "tidy", "occ_format")
makeMyDir(path_figures_occfor)

# dossier des occurrences filtrées au seuil
path_figures_occthr <- here("data",  "tidy", "occ_threshold")
makeMyDir(path_figures_occthr)
