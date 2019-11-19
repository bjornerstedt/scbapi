# devtools::install_github("bjornerstedt/scbapi")
library(scbapi)
library(tidyr)
library(dplyr)
library(stringr)
library(jsonlite)
library(httr)
source("drugs_methods.R")

# Set to TRUE to download and save data as json files
# If FALSE, it uses the saved data files instead
get_data <- TRUE

# switch(Sys.info()[['sysname']],
#    Windows= {setwd("C:/Users/n13017/Dropbox/Projects/Drugs/original_data/scb") },
#    Darwin = {setwd("~/Dropbox/Projects/Drugs/original_data/scb")})

# Population
pop <- get_scb("pop", get_data) %>%
  group_by(Tid, Kon) %>%
  summarise(value = sum(value)) %>%
  spread(Kon, value) %>%
  select(year=Tid, popwomen = kvinnor, popmen = män) %>%
  ungroup() %>%
  mutate(year = as.numeric(year))

# CPI
cpi <- get_scb( "cpi1", get_data) %>%
  bind_rows(get_scb("cpi2", get_data) ) %>%
  separate(Tid, c("year", "month"), sep = "M", convert = TRUE)%>%
  rename(cpi = value)

# Real GDP
gdp <- get_scb("gdpreal", get_data, returnList = TRUE) %>%
  rename(gdp = value) %>%
  separate(Tid, c("year", "quarter"), sep = "K", convert = TRUE) %>%
  na.omit

# Sick leave, pre 2005

sick1 <- get_scb("sick1", get_data)
sick2 <- get_scb("sick2", get_data) %>%
  mutate(
    value = 10 * value
  )
sick <- bind_rows(sick1, sick2) %>%
  separate(Tid, c("year", "month"), sep = "M", convert = TRUE) %>%
  spread(Kon, value) %>%
  rename(sw = kvinnor, sm = män)

################## Combine Data ####################

scbdata <- cpi %>%
    mutate(quarter = (month - 1) %/% 3 + 1) %>%
    left_join(gdp, by=c("year","quarter")) %>%
    left_join(pop, by="year") %>%
    left_join(sick, by=c("year", "month")) %>%
    select(-quarter)

saveRDS(scbdata, get_path("original_data/scbdata.Rds") )
