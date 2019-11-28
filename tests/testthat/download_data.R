library(scbapi)

query = '
{
  "query": [
    {
      "code": "Franvaroorsak",
      "selection": {
        "filter": "item",
        "values": [
          "FRSJUK"
        ]
      }
    },
    {
      "code": "Kon",
      "selection": {
        "filter": "item",
        "values": [
          "1",
          "2"
        ]
      }
    },
    {
      "code": "Alder",
      "selection": {
        "filter": "item",
        "values": [
          "tot16-64"
        ]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "000004AW"
        ]
      }
    }
  ],
  "response": {
    "format": "px"
  }
}'

url = "http://api.scb.se/OV0104/v1/doris/en/ssd/START/AM/AM0401/AM0401K/NAKUFranvOrsakNM"

df = get_scb(query = query, url = url, save_data = TRUE)
# saveRDS(df, file = "filename.rds")
