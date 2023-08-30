# Fonction de mise en forme pour les différentes bases

require(
  tidyverse, 
  reshape2,
  sp
)

nettoyageBase <- function(OCC, base){
  
  if (base == "GBIF") { # GBIF----
    
    occ <- OCC %>% 
      filter(
        taxonRank == "SPECIES" | 
          taxonRank == "SUBSPECIES" | 
          taxonRank == "FORM" |
          taxonRank == "VARIETY"
      ) %>% 
      filter(
        !basisOfRecord == "FOSSIL_SPECIMEN"
      ) %>% 
      add_column(
        aphiaID = NA, 
        CD_NOM = NA, 
        CD_REF = NA, 
        database = base, 
        expedition = NA,
        method = "NA", 
        collectStation = NA
      ) %>% 
      select(
        occurrenceID, 
        database, 
        CD_NOM, 
        CD_REF, 
        aphiaID, 
        scientificName = species, 
        author = verbatimScientificNameAuthorship, 
        decimalLatitude, 
        decimalLongitude,
        coordinateUncertaintyInMeters,
        eventDate, 
        country = countryCode, 
        individualCount, 
        citation = recordedBy, 
        expedition,
        basisOfRecord, 
        institutionCode, 
        collectionCode, 
        catalogNumber,
        method,
        collectStation
      )
    
    cat(
      base, 
      "\nDimensions avec duplicata\n", 
      dim(occ), 
      "\n"
    )
    
    occ <- occ %>% filter(!duplicated(occ))
    
    cat(
      "Dimensions sans duplicata\n", 
      dim(occ), 
      "\n"
    )
    
    occ[occ == ""] <- NA
    occ[occ == "PRESENT"] <- NA
    occ$decimalLongitude <- as.numeric(occ$decimalLongitude)
    occ$eventDate <- gsub("T", " ", occ$eventDate)
    
  }
  else if (base == "OBIS") { # OBIS ----
    
    occ <- OCC %>% 
      # filter(
      #   country == "French Antilles"  | 
      #     country == "Guadeloupe"     |
      #     country == "Martinique"     | 
      #     country == "St. Barthelemy" |
      #     country == "St. Martin"
      # ) %>% 
      add_column(
        CD_NOM = NA, 
        CD_REF = NA, 
        database = base,
        expedition = NA, 
        method = "NA", 
        collectStation = NA
      ) %>% 
      select(
        occurrenceID, 
        database,
        CD_NOM, 
        CD_REF, 
        aphiaID, 
        scientificName,
        author = scientificNameAuthorship,
        decimalLatitude, 
        decimalLongitude, 
        coordinateUncertaintyInMeters, 
        eventDate, 
        country, 
        individualCount, 
        citation = bibliographicCitation, 
        expedition,
        basisOfRecord,
        institutionCode, 
        collectionCode, 
        catalogNumber,
        method, 
        collectStation
      ) %>% 
      separate(
        scientificName, 
        c("Gen", "sp")
      ) %>% 
      filter(
        !is.na(sp)
      ) %>% 
      unite(
        scientificName, 
        Gen, 
        sp, 
        sep = " "
      )
    
    cat(
      base, 
      "\nDimensions avec duplicata\n", 
      dim(occ), 
      "\n"
    )
    
    occ <- occ %>% filter(!duplicated(occ))
    
    cat(
      "Dimensions sans duplicata\n", 
      dim(occ), 
      "\n"
    )
    
    occ[occ == ""] <- NA
    
    occ <- occ %>% 
      separate(
        eventDate, 
        c("eventDate", "time"),
        sep = "T"
      ) %>% 
      separate(
        eventDate, 
        c("eventDate", "date2"),
        sep = "/"
      )
    occ$eventDate[!is.na(occ$date2)] <- occ$date2[!is.na(occ$date2)]
    
    occ$eventDate[which(occ$time %in% "10:00:00")] <- paste(
      occ$eventDate[which(occ$time %in% "10:00:00")],
      "10:00:00", sep = " "
    )
    
    occ <- occ %>% select(!c(time, date2))
    
  }
  else if (base == "OOBS") { # OpenObs ----
    
    names(OCC) <- tolower(names(OCC))
    occ <- OCC %>% 
      filter(
        rangtaxo == "species"
      ) %>% 
      add_column(
        aphiaID = NA, 
        basisOfRecord = NA, 
        institutionCode = NA,
        collectionCode = NA, 
        catalogNumber = NA ,
        individualCount = NA, 
        database = base,
        expedition = NA, 
        method = "NA", 
        collectStation = NA
      ) %>% 
      select(
        occurrenceID = idsinpocctax, 
        database,
        CD_NOM = cdnom, 
        CD_REF = cdref, 
        aphiaID, 
        scientificName = espece, 
        author = nomscientifiqueref, 
        decimalLatitude = latitude, 
        decimalLongitude = longitude,
        coordinateUncertaintyInMeters = precisiongeometrie, 
        eventDate = dateobservation, 
        country = region, 
        individualCount, 
        citation = referencebiblio, 
        expedition,
        basisOfRecord, 
        institutionCode, 
        collectionCode, 
        catalogNumber, 
        method, 
        collectStation
      )
    
    cat(
      base, 
      "\nDimensions avec duplicata\n", 
      dim(occ), 
      "\n"
    )
    
    occ <- occ %>% filter(!duplicated(occ))
    
    cat(
      "Dimensions sans duplicata\n", 
      dim(occ), 
      "\n"
    )
    occ[occ == ""] <- NA
    
    occ$author <- sapply(
      occ[,"author"], 
      function(x) {
        x <- paste(strsplit(x, " ")[[1]][-c(1:2)], collapse = " ")
      }
    )
    
    # parenthèse auteur
    pospov <- which(grepl("\\(", occ$author))
    pospfm <- which(grepl("\\)", occ$author))
    ov <- rep("(", nrow(occ))
    fm <- rep(")", nrow(occ))
    ov[pospov] <- ""
    fm[pospfm] <- ""
    occ$author <- paste0(ov, occ$author, fm)
    
  }
  else if (base == "INAT") { # iNaturalist ----
    occ <- OCC %>% 
      add_column(
        database = "INAT",
        CD_NOM = NA, 
        CD_REF = NA, 
        aphiaID = NA, 
        author = NA, 
        individualCount = 1, 
        expedition = NA, 
        basisOfRecord = "humanobservation", 
        institutionCode = NA, 
        collectionCode = NA, 
        catalogNumber = NA,
        method = "NA", 
        collectStation = NA
      ) %>% 
      select(
        occurrenceID = url, 
        database, 
        CD_NOM, 
        CD_REF, 
        aphiaID, 
        scientificName = scientific_name, 
        author, 
        decimalLatitude = latitude, 
        decimalLongitude = longitude,
        coordinateUncertaintyInMeters = positional_accuracy,
        eventDate = datetime, 
        country = place_guess, 
        individualCount, 
        citation = user_login, 
        expedition,
        basisOfRecord, 
        institutionCode, 
        collectionCode, 
        catalogNumber,
        method, 
        collectStation
      )
    
    occ$eventDate <- substr(
      occ$eventDate, 
      1, 
      10
    )
    
    cat(
      base, 
      "\nDimensions avec duplicata\n", 
      dim(occ), 
      "\n"
    )
    
    occ <- occ %>% filter(!duplicated(occ))
    
    cat(
      "Dimensions sans duplicata\n", 
      dim(occ), 
      "\n"
    )
  }
  else if (base == "INVMAR") { # Invmar ----
    
    OCC[OCC == ""] <- NA
    
    OCC$scientificName <- paste(
      OCC$TAXON.GENRE, 
      OCC$TAXON.ESPECE, 
      sep = " "
    )
    
    for (i in 1:nrow(OCC)) {
      if(is.na(OCC$LOT.CODE2D[i])) {
        OCC$LOT.CODE2D[i] <- paste(
          OCC$LOT.NUM_MNHN_ACCRO[i], 
          OCC$LOT.NUM_MNHN_ANNEE[i], 
          OCC$LOT.NUM_MNHN_NUM[i], 
          sep = "-"
        )
      }
    }
    
    # complétion des données de localisation
    # conversion des lonlat texte en décimal
    
    invmarLonlatConvert <- function(df, lonlat) {
      str.txt <- paste0("STATION.", lonlat, "_TEXT")
      str.1 <- paste0("STATION.", lonlat, "_1")
      str.2 <- paste0("STATION.", lonlat, "_2")
      
      df[,str.1] <- gsub(pattern = ",", replacement = ".", x = df[,str.1])
      df[,str.2] <- gsub(pattern = ",", replacement = ".", x = df[,str.2])
      df[,str.txt] <- gsub(pattern = ",", replacement = ".", x = df[,str.txt])
      
      df[,str.txt] <- gsub(pattern = "’", replacement = "'", x = df[,str.txt])
      df[,str.txt] <- gsub(pattern = "º", replacement = "°", x = df[,str.txt])
      
      df.min <- df[is.na(df[,str.1]) & 
                     is.na(df[,str.2]) & 
                     !is.na(df[,str.txt]),]
      
      res <- as.numeric(sp::char2dms(from = df.min[,str.txt], 
                                     chd = "°", 
                                     chm = "'"))
      return(res)
    } 
    
    OCC[
      is.na(OCC$STATION.LAT_1) & 
        is.na(OCC$STATION.LAT_2) & 
        !is.na(OCC$STATION.LAT_TEXT), 
      "STATION.LAT_2"
    ] <- invmarLonlatConvert(OCC, "LAT")
    
    OCC[
      is.na(OCC$STATION.LONG_1) & 
        is.na(OCC$STATION.LONG_2) & 
        !is.na(OCC$STATION.LONG_TEXT), 
      "STATION.LONG_2"
    ] <- invmarLonlatConvert(OCC, "LONG")
    
    occ <- OCC %>% 
      filter(!grepl("sp\\.",  TAXON.ESPECE)) %>% 
      filter(!grepl("Gen\\.", TAXON.GENRE)) %>%
      filter(!grepl("aff\\.", TAXON.ESPECE)) %>% 
      filter(!grepl("cf\\.",  TAXON.ESPECE)) %>% 
      filter(!grepl("sp2",    TAXON.ESPECE)) %>%
      add_column(
        database = "INVMAR", 
        CD_NOM = NA,
        CD_REF = NA, 
        coordinateUncertaintyInMeters = NA, 
        basisOfRecord = "PRESERVED SPECIMEN",
        institutionCode = "MNHN"
      ) %>% 
      select(
        occurrenceID = MEDIATHEQUE.MEDIA_MEDIA_UID,
        database, 
        CD_NOM,
        CD_REF, 
        aphiaID = TAXON.ID_WORMS,
        scientificName, 
        author = TAXON.TAXON_LIBRE, 
        decimalLatitude = STATION.LAT_2, 
        decimalLongitude = STATION.LONG_2,
        coordinateUncertaintyInMeters,
        eventDate = STATION.DATE_FIN, 
        country = PROVENANCE.NOM, 
        individualCount = LOT.NB_SPECIMEN, 
        citation = DETERMIN.DETERMINATEUR,
        expedition = CAMPAGNE.ACRONYME,
        basisOfRecord, 
        institutionCode, 
        collectionCode = LOT.NUM_MNHN_ACCRO, 
        catalogNumber = LOT.CODE2D, 
        collectStation = INVMAR.STATION.NUM_STATION
      ) %>% 
      add_column(
        method = substr(collectStation, 1, 2)
      )
    
    # temps
    posna <- which(is.na(occ$eventDate))
    occ <- occ %>% 
      separate(
        eventDate, 
        c("jour", "mois", "anne"), 
        sep = "/"
      ) %>% 
      separate(
        anne, 
        c("anne", "time"), 
        sep = " "
      ) %>% 
      separate(
        time, 
        c("heur", "minu")
      ) %>% 
      add_column(seco = "00") %>% 
      select(1:9, anne, mois, jour, heur, minu, seco, everything()) %>% 
      unite("date", anne, mois, jour, sep = "-") %>%
      unite("temps", heur, minu, seco, sep = ":") %>% 
      unite("eventDate", date, temps, sep = " ")
    
    occ$eventDate[posna] <- NA
    
    # nom de l'auteur
    posdbl <- which(grepl("&", occ$author))
    svg <- occ
    
    for (i in 1:nrow(occ)) {
      if (i %in% posdbl) {
        occ$author[i] <- paste(
          tail(strsplit(occ$author[i], " ")[[1]], n = 4), collapse = " "
        )
      }
      else {
        occ$author[i] <- paste(
          tail(strsplit(occ$author[i], " ")[[1]], n = 2), collapse = " "
        )
      }
    }
    
    posmul <- which(grepl("&", substr(occ$author, 1, 1)))
    
    for (i in 1:nrow(occ)) {
      if (i %in% posmul) {
        occ$author[i] <- paste(
          tail(strsplit(svg$author[i], " ")[[1]], n = 6), collapse = " "
        )
      }
    }
    
    for (i in 1:nrow(occ)) {
      if (i %in% posmul) {
        vec <- strsplit(occ$author[i], " ")[[1]][1]
        if (!str_detect(vec, "[[:upper:]]")) {
          occ$author[i] <- paste(
            tail(strsplit(svg$author[i], " ")[[1]], n = 5), collapse = " "
          )
        }
      }
    }
  }
  return(occ %>% as_tibble())
}
