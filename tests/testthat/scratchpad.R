library(scbapi)

# Population
pop <- get_scb("data/pop", download_data = FALSE)
pop <- get_scb("data/popdetail", returnList = TRUE, save_data = TRUE)

# Real GDP
gdptab <- get_scb("data/gdpreal", download_data = FALSE)
gdptab <- get_scb("data/gdpreal2", returnList = TRUE)


query = '
{
  "query": [],
  "response": {
    "format": "px"
  },
  "url": "http://api.scb.se/OV0104/v1/doris/sv/ssd/START/PR/PR0101/PR0101A/KPIFastM"
}
'

