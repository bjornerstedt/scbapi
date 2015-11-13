# Download SCB data
#
# This function downloads queries saved as JSON files
# returning a data_frame
#
# For more info, see
#
#   http://www.github.com/bjornerstedt/scbapi
#

get_scb <- function(fname, download_data = TRUE, url = NA) {
  library(httr)
  library(jsonlite)
  if(download_data) {
    if(is.na(url)) {
      dataurl <- fromJSON(readLines(str_c(fname, ".json")))$url
      if( is.null(dataurl) )
        stop("url has to be specified in json input")
    }
    data <- content(POST(dataurl, body = readLines(str_c(fname, ".json")), encode = "json"), "text")
    writeLines(data, str_c(fname, "_data.json"))
    scb <- fromJSON(data)
  } else
    scb <- fromJSON(readLines(str_c(fname, "_data.json")))
  scb
}
