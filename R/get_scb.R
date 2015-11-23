#' Download SCB data
#'
#' \code{get_scb} downloads a JSON query to SCB as a data_frame
#'
#' For more info, see:
#'
#'  http://www.github.com/bjornerstedt/scbapi
#'
#' @param fname File name of JSON query
#' @param download_data Boolean indicating whether downloaded or saved data should be used
#' @param save_data Boolean indicating whether downloaded data should be saved to a JSON data file
#' @param url Url string if not a url field in the JSON query
#' @param colnames String array of column names.
#'
#' @return A data_frame with time as the first column
#' @export
#'
#' @examples df <- get_scb("gdpreal.json")
#' df <- get_scb("sick1", colnames = c("time", "sm", "sw"))
get_scb <- function(fname, download_data = TRUE, save_data = FALSE, url = NA, colnames = NULL) {
  fn <- str_match(fname,"(.*).json")[2]
  if(!is.na(fn))
    fname <- fn
  if(download_data) {
    query <- fromJSON(str_c(fname, ".json"))
    if(is.na(url)) {
      if( is.null(query$url) )
        stop("url has to be specified in json input")
    }
    data <- content(POST(query$url, body = readLines(str_c(fname, ".json")), encode = "json"), "text")
#    data <- content(POST(query$url, body = toJSON(query, pretty=TRUE), encode = "json"), "text")
    if(save_data)
      writeLines(data, str_c(fname, "_data.json"))
    scb <- fromJSON(data)
  } else
    scb <- fromJSON(readLines(str_c(fname, "_data.json")))

  indata <- scb$dataset$value
  lab <- unlist(scb$dataset$dimension$Tid$category$label)
  if(length(lab) != length(indata))
    indata <- matrix(indata, nrow = length(lab))
  df <- bind_cols(data_frame(time = lab ), x = as.data.frame(indata) )
  if(!is.null(colnames))
    colnames(df) <- colnames
  df
}
