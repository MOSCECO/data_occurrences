# Filtre donnnées provenant de Jacim ----
# importation données ----

jacim <- do.call(rbind, phyla)

# filtre 1
strg <- 5
jacimfs <- lapply(
  phyla, 
  function(tb) {

    # filtre des espèces non déterminées
    tb.non.det <- tb %>%
      filter(
        grepl("sp\\.|aff\\.|cf\\.|Gen\\.", tb$INVMAR.TAXON.TAXON_LIBRE)
      ) %>% 
      filter(
        !grepl("n\\. sp\\.", .$INVMAR.TAXON.TAXON_LIBRE)
      )
    
    tb <- tb %>%
      filter(
        !grepl("sp\\.|aff\\.|cf\\.|Gen\\.", tb$INVMAR.TAXON.TAXON_LIBRE)
      )
    
    tbs <- list(tb, tb.non.det)
    
    tbs_out <- lapply(
      tbs, 
      function(tb) {
        # séparation en noms de genre/espèce
        maxspace <- lengths(
          regmatches(
            tb$INVMAR.TAXON.TAXON_LIBRE, 
            gregexpr(" ", tb$INVMAR.TAXON.TAXON_LIBRE)
          )
        ) %>% 
          max()
        
        tb <- tb %>% 
          separate(
            INVMAR.TAXON.TAXON_LIBRE,
            c("sp", "gen", paste0("a", 1:(maxspace-2+1))),
            sep = " "
          ) %>% 
          unite(scientificName, sp, gen, sep = " ") %>% 
          unite(author, paste0("a", 1:(maxspace-2+1)), sep = " ", na.rm = TRUE)
        
        # Ajout des données de stations (via collectes)
        STN <- do.call(rbind, stations) %>% 
          select(
            INVMAR.STATION.NUM_STATION = collectEvent, 
            decimalLongitude,
            decimalLatitude,
            eventDate
          )
        
        tb <- tb %>% 
          left_join(
            STN
          )
        
        # ajout de la provenance, standardisation des noms et rangement
        tb <- tb %>% 
          add_column(
            occurrenceID = NA,
            database = "INVMAR",
            CD_REF = NA, 
            CD_NOM = NA,
            aphiaID = NA, 
            id_itis = NA, 
            country_iso2 = ifelse(
              grepl("KARU", tb$INVMAR.CAMPAGNE.NOM),
              "GP", 
              "MQ"
            ), 
            coordinateUncertaintyInMeters = NA, 
            basisOfRecord = "preservedspecimen",
            institutionCode = "MNHN", 
            catalogNumber = paste(
              tb$INVMAR.LOT.NUM_MNHN_ANNEE, tb$INVMAR.LOT.NUM_MNHN_NUM, sep = "-"
            )
          ) %>% 
          select(
            occurrenceID, 
            database, 
            CD_NOM, 
            CD_REF, 
            aphiaID, 
            id_itis, 
            scientificName, 
            author, 
            decimalLatitude, 
            decimalLongitude,
            coordinateUncertaintyInMeters,
            eventDate, 
            country_iso2, 
            individualCount = "INVMAR.LOT.NB_SPECIMEN",
            citation = "INVMAR.DETERMIN.DETERMINATEUR",
            expedition = "INVMAR.CAMPAGNE.NOM",
            basisOfRecord, 
            institutionCode, 
            collectionCode = "INVMAR.LOT.NUM_MNHN_ACCRO", 
            catalogNumber,
            collectStation = "INVMAR.STATION.NUM_STATION"
          )
        
        return(tb)
      }
    )
    
    return(tbs_out)
  }
)

# Sélection des tables d'espèces non-determinées
jacimf.non.det <- list(
  crusta = jacimfs$crusta[[2]], 
  mollsc = jacimfs$mollsc[[2]]
)

# Sélection des tables d'espèces determinées
jacimf <- list(
  crusta = jacimfs$crusta[[1]], 
  mollsc = jacimfs$mollsc[[1]]
)

# dimensions après le premier filtre de mise en forme
lapply(jacimf, dim)
lapply(phyla, dim)

# Recherche de la taxonomie des espèces ----
# recours à l'API de la plateforme TaxRef (INPN)
taxref_ids <- lapply(
  jacimf, 
  function(tb) {
    sci_names <- tb %>% pull("scientificName") %>% unique()
    taxrefs <- lapply(
      sci_names, 
      function(sn) {
        print(sn)
        url <- modify_url(
          "https://taxref.mnhn.fr/api/taxa/search", 
          query = list(
            scientificNames = sn
          )
        )
        GET(url) %>% 
          content(as = "text") %>% 
          fromJSON() %>% 
          pluck("_embedded", "taxa") %>% 
          { 
            if (!is.null(.)) { 
              cbind(
                .,
                scientificName_original = sn
              ) %>% 
                select(
                  .,
                  scientificName_original, 
                  "scientificName", 
                  CD_NOM = "id", 
                  CD_REF = "referenceId"
                )
            } 
          }
      }
    )
    taxrefs <- do.call(rbind, taxrefs)
    return(taxrefs)
  }
)

# Rechercher de l'identifiant de l'espèce dans d'autres bases de données de 
# biodiversité existantes (WoRMS, ITIS, PaléoDB...) à partir du CD_NOM, 
# l'identifiant du nom binomial dans TAXREF.
alldb_ids <- lapply(
  taxref_ids, 
  function(tb) {
    cdnom <- tb %>% pull("CD_NOM")
    alldbid <- lapply(
      cdnom, 
      function(ref_id) {
        print(ref_id)
        url <- modify_url(
          "https://taxref.mnhn.fr/", 
          path = c("api", "taxa", ref_id, "externalIds")
        )
        GET(url) %>% 
          content(as = "text") %>% 
          fromJSON() %>% 
          pluck("_embedded", "externalDb") %>% 
          {
            if (!is.null(.)) {
              select(
                ., 
                taxrefId, 
                externalId, 
                externalDbName
              )
            }
          }
        
      }
    )
    alldbid <- do.call(rbind, alldbid)
    return(alldbid)
  }
)

# Formatage de la table obtenue en une table mieux utilisable. 
external_databases_ids <- alldb_ids %>% 
  sapply(
    function(tb) {
      tb <- tb %>% 
        filter(
          !duplicated(tb)
        )
    },
    simplify = F, 
    USE.NAMES = T
  ) %>% 
  sapply(
    reshape2::dcast, 
    taxrefId ~ externalDbName,
    value.var = "externalId", 
    simplify = FALSE, 
    USE.NAMES = TRUE
  ) %>% 
  sapply(
    function(tb) {
      tb <- tb %>% 
        select(CD_NOM = taxrefId, everything())
    }, 
    simplify = FALSE, 
    USE.NAMES = TRUE
  )

# CRéation d'une table de traduction entre toutes ces identifiants de bases de
# données et les noms scientifiques. 
rosette <- mapply(
  left_join, 
  taxref_ids, 
  external_databases_ids,
  SIMPLIFY = F, 
  USE.NAMES = T
)

# Attribution à chaque espèce d'un aphiaID, d'un CD_NOM et d'un CD_REF
jacimf2 <- mapply(
  function(occ, txr, ids) {
    sn_match_cdnom <- txr$CD_NOM[
      match(
        occ$scientificName, 
        txr$scientificName_original
      )
    ]
    sn_match_cdref <- txr$CD_REF[
      match(
        occ$scientificName, 
        txr$scientificName_original
      )
    ]
    sn_match_aphia <- txr$WoRMS[
      match(
        occ$scientificName, 
        txr$scientificName_original
      )
    ]
    
    occ$CD_NOM <- sn_match_cdnom
    occ$CD_REF <- sn_match_cdref
    occ$aphiaID <- as.numeric(sn_match_aphia)
    
    return(occ)
  },
  jacimf, 
  rosette,
  SIMPLIFY = FALSE, 
  USE.NAMES = TRUE
)

lapply(jacimf2, function(tb) table(is.na(tb[["aphiaID"]])))

# recherche des aphiaID manquant via le paquet "worrms"
jacim_worms <- lapply(
  jacimf2, 
  function(tb) {
    tb <- tb %>% 
      filter(is.na(aphiaID))
    if (
      length(
        tb %>% 
        pull("scientificName") %>% 
        unique()
      ) > 0
    ) {
      
      my_vec <- tb %>% 
        pull("scientificName") %>% 
        unique()
      my_vec <- my_vec[!grepl("Gen.|sp.|\\(", my_vec)]
      
      # out <- lapply(
      #   my_vec, 
      #   function(x) worrms::wm_records_name(x)
      # )
      # out <- do.call(rbind, out)
      
      out <- worrms::wm_records_names(my_vec)
      
      return(out)
    }
  }
)

# formatage de la table de sortie de worrms
jacimw <- lapply(
  jacim_worms, 
  function(l) {
    if (length(l) > 0) {
      l <- do.call(rbind, l)
      names(l)[which(names(l) %in% "scientificname")] <- "scientificName"
      return(l)
    }
  }
)

# complétion de la table 
jacimf_illed <- mapply(
  #columns receiver, common, provider
  function(tb1, tb2, col_rcv, col_cmn, col_prv) { 
    if (length(tb2) > 0) {
      tb3 <- left_join(tb1, tb2, by = col_cmn)
      tb1[is.na(tb1[[col_rcv]]), col_rcv] <- tb3[is.na(tb1[[col_rcv]]), col_prv]
    }
    return(tb1)
  },
  jacimf2, 
  jacimw, 
  "aphiaID", 
  "scientificName", 
  "valid_AphiaID", 
  USE.NAMES = TRUE, 
  SIMPLIFY = FALSE
)

# formatage final
jacim_final <- lapply(
  jacimf_illed,  
  function(tb) {
    tb[tb == "no data"] <- NA
    tb <- tb %>% 
      mutate(
        method = substr(collectStation, 1, 2)
      )
    tb <- tb %>% 
      select(
        occurrenceID, 
        database, 
        CD_NOM, 
        CD_REF, 
        aphiaID, 
        scientificName, 
        author, 
        decimalLatitude, 
        decimalLongitude,
        coordinateUncertaintyInMeters,
        eventDate, 
        country_iso2, 
        individualCount,
        citation,
        expedition,
        basisOfRecord, 
        institutionCode, 
        collectionCode, 
        catalogNumber,
        method,
        collectStation
      )
    return(tb)
  }
)

jacim_final <- lapply(
  jacim_final, 
  function(tb) {
    tb <- tb %>% 
      filter(!duplicated(tb)) %>%
      filter(!is.na(decimalLongitude)) 
  }
)
lapply(jacim_final, dim)
lapply(jacim_final, function(tb) table(is.na(tb$aphiaID)))

# attribution d'une classification taxonomique à chaque occurrence de la table
worms_classification <- lapply(
  jacim_final, 
  function(tb) {
    tb <- tb %>% filter(!is.na(aphiaID))
    w <- worrms::wm_classification_(unique(tb[["aphiaID"]]))
    return(w)
  }
)

# formatage de la table
worms_classification_casted <- mapply(
  function(tb_classification, phylum) {
    tb_classification$rank <- factor(
      tb_classification$rank, 
      levels = as.character(unique(tb_classification$rank))
    )
    levels(tb_classification$rank)
    tb_classification_casted <- dcast(
      tb_classification,
      id ~ rank, 
      value.var = "scientificname"
    )
    tb_classification_casted <- tb_classification_casted %>% 
      filter(Phylum %in% c(phylum))
    table(tb_classification_casted$Phylum)
    return(tb_classification_casted)
  },
  worms_classification, 
  c("Arthropoda", "Mollusca"),
  SIMPLIFY = F, 
  USE.NAMES = T
)





















###################################################
# SI UTILISATION DE TOUTE LA TABLE
###################################################

# Extraction des espèces manquantes après tous les appels d'API (orthographe 
# différent, synonymes, fossiles...)
jacim_missing_worms <- lapply(
  jacim_final, 
  function(tb) tb %>% 
    filter(is.na(tb$aphiaID)) %>% 
    group_by(scientificName) %>% 
    arrange(.by_group = T) %>% 
    ungroup()
)

mapply(
  function(x, fn) {
    write.csv(
      x,
      here(
        "data", "tidy", fn %>% 
          paste("invmar", "manuscriptError", sep = "_") %>% 
          paste0(".csv")
      ),
      row.names = FALSE
    )
  },
  jacim_missing_worms, 
  names(jacim_final),
  SIMPLIFY = FALSE
)

jacim_corrected_errors <- lapply(
  names(jacim_final),
  function(fn) {
    read.csv(
      here(
        "data", "tidy", fn %>% 
          
          paste("invmar", "manuscriptError", sep = "_") %>% 
          paste0(".csv")
      )
    )
  }
)
names(jacim_corrected_errors) <- names(jacim_final)

# on enlève les lignes à aphia manquants de la table finale
jacim_final <- jacim_final %>% 
  lapply(function(tb) tb %>% filter(!is.na(aphiaID)))



############################################################################
#  deuxième boucle
#######################################################
jacimf <- jacim_filling_worms
taxref_ids <- lapply(
  jacimf, 
  function(tb) {
    sci_names <- tb %>% pull("scientificName") %>% unique()
    taxrefs <- lapply(
      sci_names, 
      function(sn) {
        print(sn)
        url <- modify_url(
          "https://taxref.mnhn.fr/api/taxa/search", 
          query = list(
            scientificNames = sn
          )
        )
        GET(url) %>% 
          content(as = "text") %>% 
          fromJSON() %>% 
          pluck("_embedded", "taxa") %>% 
          { 
            if (!is.null(.)) { 
              cbind(
                .,
                scientificName_original = sn
              ) %>% 
                select(
                  .,
                  scientificName_original, 
                  "scientificName", 
                  CD_NOM = "id", 
                  CD_REF = "referenceId"
                )
            } 
          }
      }
    )
    taxrefs <- do.call(rbind, taxrefs)
    return(taxrefs)
  }
)

alldb_ids <- lapply(
  taxref_ids, 
  function(tb) {
    cdnom <- tb %>% pull("CD_NOM")
    alldbid <- lapply(
      cdnom, 
      function(ref_id) {
        print(ref_id)
        url <- modify_url(
          "https://taxref.mnhn.fr/", 
          path = c("api", "taxa", ref_id, "externalIds")
        )
        GET(url) %>% 
          content(as = "text") %>% 
          fromJSON() %>% 
          pluck("_embedded", "externalDb") %>% 
          {
            if (!is.null(.)) {
              select(
                ., 
                taxrefId, 
                externalId, 
                externalDbName
              )
            }
          }
        
      }
    )
    alldbid <- do.call(rbind, alldbid)
    return(alldbid)
  }
)

external_databases_ids <- alldb_ids %>% 
  sapply(
    function(tb) {
      tb <- tb %>% 
        filter(
          !duplicated(tb)
        )
    },
    simplify = F, 
    USE.NAMES = T
  ) %>% 
  sapply(
    reshape2::dcast, 
    taxrefId ~ externalDbName,
    value.var = "externalId", 
    simplify = FALSE, 
    USE.NAMES = TRUE
  ) %>% 
  sapply(
    function(tb) {
      tb <- tb %>% 
        select(CD_NOM = taxrefId, everything())
    }, 
    simplify = FALSE, 
    USE.NAMES = TRUE
  )

rosette <- mapply(
  left_join, 
  taxref_ids, 
  external_databases_ids,
  SIMPLIFY = F, 
  USE.NAMES = T
)

jacimf2 <- mapply(
  function(occ, txr) {
    sn_match_cdnom <- txr$CD_NOM[
      match(
        occ$scientificName, 
        txr$scientificName_original
      )
    ]
    sn_match_cdref <- txr$CD_REF[
      match(
        occ$scientificName, 
        txr$scientificName_original
      )
    ]
    
    sn_match_aphia <- txr$WoRMS[
      match(
        occ$scientificName, 
        txr$scientificName_original
      )
    ]
    
    occ$CD_NOM <- sn_match_cdnom
    occ$CD_REF <- sn_match_cdref
    if (
      !is.null(sn_match_aphia)
    ) {  
      occ$aphiaID <- as.numeric(sn_match_aphia) 
    }
    else {
      occ$aphiaID <- NA
    }
    
    
    return(occ)
  },
  jacimf, 
  rosette,
  SIMPLIFY = FALSE, 
  USE.NAMES = TRUE
)

jacim_worms <- lapply(
  list(jacimf2$mollsc), 
  function(tb) {
    tb <- tb %>% 
      filter(is.na(aphiaID))
    if (
      length(
        tb %>% 
        pull("scientificName") %>% 
        unique()
      ) > 0
    ) {
      
      my_vec <- tb %>% 
        pull("scientificName") %>% 
        unique()
      my_vec <- my_vec[!grepl("Gen.|sp.|\\(", my_vec)]
      
      # out <- lapply(
      #   my_vec, 
      #   function(x) worrms::wm_records_name(x)
      # )
      # out <- do.call(rbind, out)
      
      out <- worrms::wm_records_names(my_vec)
      
      return(out)
    }
  }
)

jacimw <- lapply(
  jacim_worms, 
  function(l) {
    if (length(l) > 0) {
      l <- do.call(rbind, l)
      names(l)[which(names(l) %in% "scientificname")] <- "scientificName"
      return(l)
    }
  }
)

jacimf_illed <- mapply(
  #columns receiver, common, provider
  function(tb1, tb2, col_rcv, col_cmn, col_prv) { 
    if (length(tb2) > 0) {
      tb3 <- left_join(tb1, tb2, by = col_cmn)
      tb1[is.na(tb1[[col_rcv]]), col_rcv] <- tb3[is.na(tb1[[col_rcv]]), col_prv]
    }
    return(tb1)
  },
  jacimf2, 
  jacimw, 
  "aphiaID", 
  "scientificName", 
  "valid_AphiaID", 
  USE.NAMES = TRUE, 
  SIMPLIFY = FALSE
)


jacim_filling_worms <- mapply(
  function(tb, TB) {
    tb_list <- split(tb, as.factor(tb$scientificName))
    tb_list_corrected <- lapply(
      seq_along(tb_list), 
      function(i) {
        minitb <- tb_list[[i]]
        if (
          unique(minitb$author) == "n. sp."
        ) {
          minitb <- minitb %>%
            mutate(
              aphiaID = max(TB$aphiaID, na.rm = T) + 1000 + i
            )
        }
        return(minitb)
      }
    )
    return(do.call(rbind, tb_list_corrected))
  },
  jacimf_illed, 
  jacim_final, 
  SIMPLIFY = F, 
  USE.NAMES = T
)
lapply(jacim_filling_worms, View)

wm_records_name("Charybdis (Charybdis) hellerii")
jacim_filling_worms$crusta[
  jacim_filling_worms$crusta$scientificName == 
    "Charybdis (Charybdis) hellerii",
  "aphiaID"
] <- 107382

jacim_filling_worms2 <- mapply(
  function(tb, TB) {
    tb_list <- split(tb, as.factor(tb$scientificName))
    tb_list_corrected <- lapply(
      seq_along(tb_list), 
      function(i) {
        minitb <- tb_list[[i]]
        if(
          is.na(unique(minitb$aphiaID))
        ) {
          minitb <- minitb %>%
            mutate(
              aphiaID = max(TB$aphiaID, na.rm = T) + 5000 + i
            )
        }
        return(minitb)
      }
    )
    return(do.call(rbind, tb_list_corrected))
  },
  jacim_filling_worms, 
  jacim_final, 
  SIMPLIFY = F, 
  USE.NAMES = T
)
lapply(jacim_filling_worms2, View)

jacim_Final <- mapply(
  function(tb1, tb2) {
    tb3 <- tb1 %>% rbind(tb2)
    return(tb3)
  }, 
  jacim_final, 
  jacim_filling_worms2,
  SIMPLIFY = F, 
  USE.NAMES = T
)

lapply(jacim_final, dim)
lapply(jacim_Final, dim)












mapply(
  function(x, fn) {
    write.csv(
      x,
      here(
        "data", "tidy", fn %>% 
          paste("2022-10", "invmar", "clean", sep = "_") %>% 
          paste0(".csv")
      ),
      row.names = FALSE
    )
  },
  jacim_final, 
  names(jacim_final),
  SIMPLIFY = FALSE
)
