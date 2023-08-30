db_info <- databases_infos[[database]]

mapply(
  function(query, outfn, bn) {
    
    if(
      !(TRUE %in% grepl(bn, list.files(here(path_gbif, superfamily, "occ"))))
    ) {
    # if(
    #   bn %in% c(
    #     "Claremontiella_nodulosa",
    #     "Phyllonotus_pomum", 
    #     "Mithraculus_forceps", 
    #     "Amphithrax_hemphilli"
    #   ) 
    # ) {
      POST(
        url    = db_info$download_url, 
        config = authenticate(user, pwd), 
        add_headers("Content-Type: application/json"),
        body   = here(path_gbif, superfamily, "query", query) %>%
          upload_file(),
        encode = 'json'
      ) %>% 
        content(as = "text") -> request_key
      
      request_url <- paste(
        db_info$download_url, 
        request_key, 
        sep = "/"
      )
      
      request_status <- "Error"
      while(request_status != "Success") {
        request_status <- GET(url = request_url) %>% 
          http_status() %>% 
          pluck("category")
      }
      
      response <- GET(url = request_url)
      download_url <- response$url
      
      temp <- tempfile()
      download.file(download_url, temp)
      datum <- read.csv(
        unz(
          temp, 
          paste(request_key, db_info$output_file_extension, sep = ".")
        ), 
        sep = db_info$output_file_separator
      )
      
      datum %>% write.csv(
        here(
          path_gbif, superfamily, "occ", paste(outfn, "csv", sep = ".")
        ),
        row.names = FALSE, 
      )
      unlink(temp)
    }
  },
  queries, 
  outputf, 
  binom.names,
  SIMPLIFY = F
)