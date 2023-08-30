# Acquisition et néttoyage des données d'occurrences
# - du Muséum national d'Histoire naturelle (MNHN)
# - de bases de données ouvertes

# nettoyage des données issues de Jacim (base de données du MNHN)
# source(here("scripts", "jacim_filter_muri_majo.R"))
# Variante utilisée pour traiter toutes les espèces de mollusques
# et de crustacés échantillonnés pendant les campagnes caribéennes
# * : non-fonctionnelle
# source(here("scripts", "jacim_filter_crusta_mollsc*.R"))

# téléchargement des occurrences globales depuis le GBIF
# source(here("scripts", "global_occurrences.R"))
# Appelle les scripts :
# source(here("scripts", "gbif_query_maker.R"))
# source(here("scripts", "gbif_query_poster.R"))

# formatage des données pour coller aux données du MNHN
# Filtre et visualisation des occurrences à l'échelle globale
# (occurrences sur terre, troncature de la distribution des
# coordonnées pour enlever les coordonnées extrêmes)
# Détermination des emprises pertinentes pour le téléchargement
# des données environnementales.
# source(here("scripts", "formatage_gbif.R"))

# Résumé des occurrences d'espèces, du nombre d'espèces etc...
# pour le datapaper
# source(here("scripts", "summary_for_datapaper.R"))
