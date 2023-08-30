# summary for datapaper

# Madibenthos
lapply(
  names(phyla), 
  \(tax) {
    tb <- phyla[[tax]]
    spcmn <- tb %>% 
      filter(INVMAR.CAMPAGNE.NOM == "MADIBENTHOS") %>% 
      select(INVMAR.LOT.NB_SPECIMEN)
    spcmn[is.na(spcmn)] <- 1
    paste(sum(spcmn), "specimens of", tax, "have been collected")
  }
)
# [1] "15419 specimens of crusta have been collected"
# [1] "10020 specimens of mollsc have been collected"

q <- split(species$MTQ, f = species$MTQ$family == "Muricidae")
names(q) <- c("Majoidea", "Muricidae")
lapply(
  names(q), 
  \(tax) {
    tb <- q[[tax]]
    spcmn <- tb$individualCount
    spcmn[is.na(spcmn)] <- 1
    paste(sum(spcmn), "specimens of", tax, "have been collected")
    paste(length(unique(tb$aphiaID)), "species of", tax, "have been collected")
  }
)

# [1] "1848 specimens of Majoidea have been collected"
# [1] "60 species of Majoidea have been collected"
# [1] "1255 specimens of Muricidae have been collected"
# [1] "55 species of Muricidae have been collected"