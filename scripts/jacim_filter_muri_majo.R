# recollection des occurrences ---

species <- lapply(
  Taxa, 
  function(tax) {
    out <- list.files(
      here("data", "raw", "occ", "jacim_2023-01-25_muricidae_majoidea"),
      pattern = paste0(tax, "*.*.csv"), 
      full.names = T
    ) %>% 
      lapply(read.csv, fileEncoding = "UTF-16")
    names(out) <- c("karu2012", "karu2015", "madi2016")
    return(out)
  }
)

species <- list(
  GLP = list(
    muri = do.call(rbind, species$muricidae[1:2]),
    majo = do.call(
      rbind, 
      list(
        species$majoidea[[1]] %>% 
          select(-INVMAR.TAXON.FAMILLE), 
        species$majoidea[[2]]
      )
    )
  ),
  MTQ = list(
    muri = species$muricidae[[3]],
    majo = species$majoidea[[3]] %>% 
      select(-INVMAR.TAXON.FAMILLE)
  )
)

species <- lapply(
  islands,
  function(isl) {
    lapply(
      taxa, 
      function(tax) {
        # names(species[[isl]][[tax]])
        # dim(species[[isl]][[tax]])
        
        if (
          "INVMAR.TAXON.SUPER_FAMILLE" %in% names(species[[isl]][[tax]])
        ) {
          species[[isl]][[tax]] <- species[[isl]][[tax]] %>% 
            add_column(INVMAR.TAXON.FAMILLE = NA)
        } else {
          species[[isl]][[tax]] <- species[[isl]][[tax]] %>% 
            add_column(INVMAR.TAXON.SUPER_FAMILLE = "Muricoidea")
        }
        return(species[[isl]][[tax]])
      }
    )
  }
)


# Formatage de la table ----
jacim <- lapply(
  species, 
  function(x) {
    z <- do.call(rbind, x)
    return(z)
  }
)

jacim <- lapply(
  jacim, 
  function(tb) {
    rownames(tb) <- NULL
    return(tb)
  }
)

strg <- 5
jacimf <- lapply(
  jacim, 
  function(tb) {
    tb <- tb %>%
      filter(
        !grepl("sp.|aff.|cf.", tb$INVMAR.TAXON.TAXON_LIBRE)
      )
    
    maxspace <- lengths(
      regmatches(
        tb$INVMAR.TAXON.TAXON_LIBRE, 
        gregexpr(" ", tb$INVMAR.TAXON.TAXON_LIBRE)
      )
    ) %>% 
      max()
    
    tb <- tb %>% separate(
      INVMAR.TAXON.TAXON_LIBRE,
      c("sp", "gen", paste0("a", 1:(maxspace-2+1))),
      sep = " "
    ) %>% 
      unite(scientificName, sp, gen, sep = " ") %>% 
      unite(author, paste0("a", 1:(maxspace-2+1)), sep = " ", na.rm = TRUE)
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
    
    tb <- tb %>% 
      add_column(
        occurrenceID = NA,
        database = "INVMAR",
        CD_REF = NA, 
        CD_NOM = NA,
        aphiaID = NA, 
        id_itis = NA, 
        country = ifelse(
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
        country, 
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

# Avec synonyme entre parenthèses  ----
jacimf <- lapply(
  jacimf, 
  function(tb) {
    p <- which(grepl("\\(", tb$scientificName))
    # g <- do.call(
    #   rbind, 
    #   strsplit(
    #     tb[p, "scientificName"],
    #     " "
    #   )
    # )[, 1]
    g <- tb[p, "scientificName"]
    s <- do.call(
      rbind, 
      strsplit(
        tb[p, "author"],
        " "
      )
    )[, 1]
    tb[p, "scientificName"] <- paste(g, s)
    tb[p, "author"] <- strsplit(
      tb[p, "author"],
      " "
    ) %>% 
      lapply(function(x) return(x[-1])) %>% 
      lapply(paste, collapse = " ")
    return(tb)
  }
)

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
      out <- worrms::wm_records_names(
        tb %>% 
          pull("scientificName") %>% 
          unique()
      )
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
        country, 
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

# Je constate qu'il y a des aphiaID en synonymie, on rajoute une colonne
# aphiaID_accepted à partir de worms. 
valid_aphia_id <- lapply(
  jacim_final, 
  function(tb) {
    v <- unique(tb[["aphiaID"]])
    v_chunks <- split(
      v, cut(seq_along(v), ceiling(length(v)/50), labels = FALSE)
    )
    # v_seq <- cut(v, breaks = floor(seq_along(v)/50))
    w <- do.call(rbind, lapply(v_chunks, worrms::wm_record)) %>% 
      select(
        aphiaID = AphiaID, 
        aphiaID_valid = valid_AphiaID,
        scientificName_valid = valid_name,
        family
      )
    return(w)
  }
)

# remplacement des aphiaID dans jacim_final par les aphiaID valides
# et homogénéisation des noms scientifiques au passage. 
jacim_final <- mapply(
  function(tb, vaid) {
    tb_min <- tb %>% select(scientificName, aphiaID)
    tb_joi <- tb_min %>% left_join(vaid)
    tb_out <- tb %>% 
      mutate(
        aphiaID = tb_joi$aphiaID_valid,
        scientificName = tb_joi$scientificName_valid
      ) %>% 
      add_column(family = tb_joi$family) %>% 
      select(1:5, family, everything())
    return(tb_out)
  },
  jacim_final, 
  valid_aphia_id, 
  SIMPLIFY = F, 
  USE.NAMES = T
)

mapply(
  function(x, fn) {
    write.csv(
      x,
      here(
        "data", "tidy", "occ", 
        tolower(fn) %>% 
          paste("invmar", "clean", sep = "_") %>% 
          paste0(".csv")
      ),
      row.names = FALSE
    )
  },
  jacim_final, 
  names(jacim_final),
  SIMPLIFY = FALSE
)

species <- jacim_final