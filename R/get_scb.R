# Download SCB data
# Jonas Björnerstedt
#
# This function downloads queries saved as JSON files
# returning a data_frame
#
# For more info, see:
#
#   http://www.github.com/bjornerstedt/scbapi
#

get_scb <- function(fname, download_data = TRUE, save_data = FALSE, url = NA) {
  library(httr)
  library(jsonlite)
  if(download_data) {
    query <- fromJSON(readLines(str_c(fname, ".json")))
    if(is.na(url)) {
      if( is.null(query$url) )
        stop("url has to be specified in json input")
    }
    data <- content(POST(query$url, body = readLines(str_c(fname, ".json")), encode = "json"), "text")
    if(save_data)
      writeLines(data, str_c(fname, "_data.json"))
    scb <- fromJSON(data)
  } else
    scb <- fromJSON(readLines(str_c(fname, "_data.json")))

  indata <- scb$dataset$value
  lab <- unlist(scb$dataset$dimension$Tid$category$label)
  if(length(lab) != length(indata))
    indata <- matrix(indata, nrow = length(lab))
  data.frame(time = lab, x = indata )
}
