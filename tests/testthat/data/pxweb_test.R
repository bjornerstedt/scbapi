library(pxweb)
parse_json_query <- function(query){
    parsed_query <- RJSONIO::fromJSON(query)
    stopifnot(length(parsed_query) == 2)
    dims <- list()
    for(i in seq_along(parsed_query[[1]])){ 
        dims[[parsed_query[[1]][[i]]$code]] <- parsed_query[[1]][[i]]$selection$values
    }
    dims
}

get_pxweb_contentcode <- function(url){
    metadata <- get_pxweb_metadata(url)
    unlist(lapply(metadata$variables$variables, function(X) if(X$code == "ContentsCode") return(X$values)))
}

query_url <- "http://api.scb.se/OV0104/v1/doris/en/ssd/START/NV/NV1701/NV1701A/NV1701T12TotAr"

query <- '
{
    "query": [
    {
    "code": "Region",
    "selection": {
    "filter": "item",
    "values": [
    "00"
    ]
    }
    },
    {
    "code": "Anlaggning",
    "selection": {
    "filter": "item",
    "values": [
    "S",
    "V"
    ]
    }
    },
    {
    "code": "Tid",
    "selection": {
    "filter": "item",
    "values": [
    "2010",
    "2011",
    "2012"
    ]
    }
    }
    ],
    "response": {
    "format": "px"
    }
}
'

dims <-  get_pxweb_data(url = query_url, dims = c(parse_json_query(query), 
    ContentsCode = get_pxweb_contentcode(query_url)),
    clean = TRUE)

d <- interactive_pxweb(api = "api.scb.se")

pxweb_test_data <- 
    get_pxweb_data(url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/PR/PR0101/PR0101E/Basbeloppet", 
        dims = list(ContentsCode = c('PR0101A1'), 
            Tid = c('*')),
        clean = TRUE)