# scbapi
Downloading data from SCB using JSON is rather simple in R. Here is a function that creates a `data_frame` from a json query file, as generated by the SCB site. 

## Using

1. Select data, for example [CPI](http://www.statistikdatabasen.scb.se/pxweb/en/ssd/START__PR__PR0101__PR0101A/KPIFastM/)
2. Click on the link: [API for this table]() located below the resulting table output
3. Save the JSON code to a file with extension json, for example "cpi.json"
4. Copy and paste the url in your code as shown below: `cpiurl <- "http://api.scb.se/ ..."`
5. Get the data_frame by invoking `get_scb()`. Note that the .json extension is not provided.

```
library(scbapi)
cpiurl <- "http://api.scb.se/OV0104/v1/doris/en/ssd/START/PR/PR0101/PR0101A/KPIFastM"
df <- get_scb("cpi", cpiurl)
```
The data frame generated is essentially the transpose of the table shown on screen in the search result. Variable names of your choice have to be set manually, for example with `names(df) <- c("time", "cpi")`. 

### Installing

To use, download the function get_url.R, or install the package using devtools:

```
install.packages("devtools")
devtools::install_github("bjornerstedt/scbapi")
```

## Further features

The url can be stored in the downloaded json-file by adding them, comma separated, as fields at the bottom:

```
{
    "query": [
... 
...
       ],
    "response": {
        "format": "json-stat"
    },
    "url": "http://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101A/BefolkningNy",
    "weburl": "http://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BE__BE0101__BE0101A/BefolkningNy"
}
```
Here I have added the field `url` for the json search. The advantage of this approach is that the json file now contains all the information necessary to retreive the data:

```
library(scbapi)
df <- get_scb("cpi")
```

It is a little unfortunate that SCB does not put the url in the json-query. If they did, one could just download a file and execute the short command above to get the data as a data frame. 
