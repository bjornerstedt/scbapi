library(scbapi)

query = '
{
  "query": [],
  "response": {
    "format": "px"
  }
}'

url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/START/PR/PR0101/PR0101A/KPIFastM"

df = get_scb(query = query, url = url)
saveRDS(df, file = "filename.rds")
