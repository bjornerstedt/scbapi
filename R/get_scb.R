#' Download SCB data
#'
#' \code{get_scb} downloads a JSON query to SCB as a data_frame
#'
#' For more info, see:
#'
#'  \url{http://www.github.com/bjornerstedt/scbapi}
#'
#' @param file File name of JSON query
#' @param query JSON search query as string.
#' @param url Url string if not a url field in the JSON query
#' @param download_data Boolean indicating whether downloaded or saved data should be used
#' @param save_data Boolean indicating whether downloaded data should be saved to a JSON data file
#'
#' @return A data_frame with time as the first column
#' @export
#'
#' @examples df <- get_scb("gdpreal.json")
#' df <- get_scb("sick1", colnames = c("time", "sm", "sw"))
#'

get_scb <- function(file = NA, query = NA, download_data = TRUE, save_data = FALSE,
    url = NA, returnList = FALSE) {
    if(!is.na(file)) {
        fn <- stringr::str_match(file,"(.*).json")[2]
        if(!is.na(fn))
            file <- fn
        if(is.na(file.info(stringr::str_c(file, ".json"))[1,1]) )
            stop("Could not find query file")
        query = readr::read_file(stringr::str_c(file, ".json"))
    } else if(is.na(query)) {
        stop("Either file or query has to be specified")
    }
    if(download_data) {
        queryobj = jsonlite::fromJSON(query)
        if(is.na(url)) {
            if( is.null(queryobj$url) )
                stop("url has to be specified in json input")
        }
        # Make sure that response format is json-stat rather than px
        url = if(is.na(url)) queryobj$url else url
        queryobj$response$format = "json-stat"
        querystring = jsonlite::toJSON(queryobj, pretty=TRUE, auto_unbox = TRUE)
        data = httr::content(httr::POST(url, body = querystring, encode = "json"), "text")
        if(save_data){
            writeLines(data, stringr::str_c(file, "_data.json"))
        }
        scb <- jsonlite::fromJSON(data)
    } else {
        if( is.na(file.info(stringr::str_c(file, "_data.json"))[1,1]) )
            stop("Could not find data file")
        scb <- jsonlite::fromJSON(readLines(stringr::str_c(file, "_data.json")))
    }

    if(returnList)
        return(scb)
    else
        return(list_to_df_old(scb))
}

list_to_df <- function(scb) {
    cnames = scb[["dimension"]][["id"]]
    sizes = scb[["dimension"]][["size"]]

    # INCLUDE?
    # How are time and metric properties special?: Use key or label?
    metric = scb[["dimension"]][["role"]][["metric"]]
    scb[["dimension"]][["role"]][["time"]]

    # sizes = sizes[cnames!=metric]

    cnames[cnames==metric] = scb[["dimension"]][["ContentsCode"]][["label"]]

    # With two columns, the right column has to be copied n times where n is
    # the number of elements in the left column
    # Each element in the left column has to be repeated the number of times = length of right col
    df = tibble::tibble(.rows = length( scb[["value"]]))
    for (i in 1:length(sizes)) {
        # i=2
        after = if(i < length(sizes))  Reduce(`*`, sizes[(i+1):length(sizes)]) else 1
        before = if(i > 1)  Reduce(`*`, sizes[1:(i-1)]) else 1
        # Names instead of label
        # nn = names(scb$dimension[[cn[1]]][["category"]][["label"]])
        nn = scb$dimension[[cnames[i]]][["category"]][["label"]]
        df[[cnames[i]]] = rep(nn, times = before, each = after)
    }
    df[["values"]] = scb[["value"]]
    return(df)
}

list_to_df_old <- function(scb) {
    cats = list()
  for (i in length(scb$dataset$dimension$size):1) {
      if (scb$dataset$dimension$size[i] > 1) {
          cats[[scb$dataset$dimension$id[[i]] ]] =
              as.character( scb$dataset$dimension[[i]]$category$label )
      }
  }
  df = expand.grid(cats, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  df$value = scb$dataset$value
  return(df)
}
