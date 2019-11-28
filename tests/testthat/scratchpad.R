library(scbapi)

# Population
scb <- get_scb("data/pop", download_data = FALSE, returnList = TRUE)
pop <- get_scb("data/popdetail", returnList = TRUE, save_data = TRUE)

# Real GDP
scb <- get_scb("data/gdpreal", download_data = FALSE, returnList = TRUE)

scb <- get_scb("data/gdpreal2", returnList = TRUE)

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
    # nn = names(scb$dimension[[cnames[i]]][["category"]][["label"]])
    nn = unlist(scb[["dimension"]][[cnames[i]]][["category"]][["label"]])
    df[[cnames[i]]] = rep(nn, times = before, each = after)
  }
  df[["values"]] = scb[["value"]]
df

setwd("~/GitHub/scbapi/tests/testthat")
df <- scb_data("data/cpi1_data.json")
df <- scb_data("data/house_age.json")
