# library(scbapi)
library(tidyr)
library(dplyr)
library(stringr)
library(jsonlite)
library(httr)

# Population
pop <- get_scb("pop", FALSE)

# Real GDP
gdp <- get_scb("gdpreal", colnames = c("date", "gdp"))
