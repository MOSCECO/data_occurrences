# boot data_occ_preparation

# libraries ----
libs_to_call <- list(
  
  "data.table",
  "ggplot2",
  "ggpubr", 
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
Taxa <- c("muricidae", "majoidea")
names(Taxa) <- Taxa
taxa <- c("muri", "majo")
names(taxa) <- taxa
superfamilies <- sort(c("Muricoidea", "Majoidea"))
names(superfamilies) <- superfamilies

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

stations <- readRDS(
  here("data", "raw", "shp", "stations_me_sf.rds")
)

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

# source(here("scripts", "jacim_filter_muri_majo.R"))

# 2023-03-25
# Pour enrichir la niche écologique observée des espèces, téléchargement
# des occurrences d'espèces pour une répartition mondiale. 
species <- here("data", "tidy", "occ") %>% 
  list.files(pattern = "invmar_clean", full.names = T) %>% 
  lapply(read.csv)
names(species) <- c("GLP", "MTQ")

# téléchargement des occurrences globales depuis le GBIF
# source(here("scripts", "global_occurrences.R"))
# formatage des données pour coller aux données du MNHN
source(here("scripts", "formatage_gbif.R"))