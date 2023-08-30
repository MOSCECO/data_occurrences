database <- "gbif"

mapply(
  function(tax_key, taxname) {
    
    query <- sprintf(
      '{
  "creator": "igregman2",
  "notificationAddresses": [
    "gregoire.maniel4@mnhn.fr"
  ],
  "sendNotification": true,
  "format": "SIMPLE_CSV",
  "predicate": {
    "type": "and",
    "predicates": [
      {
        "type": "equals",
        "key": "TAXON_KEY",
        "value": "%s"
      },
      {
        "type": "equals",
        "key": "HAS_COORDINATE",
        "value": "true"
      }
    ]
  }
}', 
      tax_key
    )
    
    file_name <- paste(
      "query", "gbif", "api", tax_key, gsub(" ", "_", taxname), sep = "_"
    ) %>% 
      paste0(".json")
    
    write(query, here(path_query, file_name))
    
  },
  gbif_keys, 
  taxa_bn
)