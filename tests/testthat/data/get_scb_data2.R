# devtools::install_github("bjornerstedt/scbapi")
library(scbapi)
library(tidyr)
library(dplyr)
library(stringr)
library(jsonlite)
library(httr)
# Set to TRUE to download and save data as json files
# If FALSE, it uses the saved data files instead
get_data <- TRUE

# Population
pop <- get_scb("pop", get_data) %>% 
    mutate(
        year = as.numeric(time),
        popmen = rowSums(.[, 1:4 * 2 -1]),
        popwomen = rowSums(.[, 1:4 * 2 ])
    )  %>% 
    select(-matches("V"), -time)

# CPI
cpi <- get_scb( "cpi1", get_data) %>% 
    bind_rows(get_scb("cpi2", get_data) ) %>% 
    rename(cpi = indata) %>% 
    separate(time, c("year", "month"), sep = "M", convert = TRUE)

# Real GDP
gdp <- get_scb("gdpreal", get_data) %>% 
    rename(GDPreal = indata) %>% 
    separate(time, c("year", "quarter"), sep = "K", convert = TRUE) %>% 
    na.omit

# Sick leave, pre 2005
sick1 <- get_scb("sick1", get_data, colnames = c("sm", "sw", "time"))

# sick leave from 2005
sick2 <- get_scb("sick2", get_data, colnames = c("sm", "sw", "time")) %>% 
    mutate(
        sm = 10 * sm,
        sw = 10 * sw
    )
sick <- bind_rows(sick1, sick2) %>% 
    separate(time, c("year", "month"), sep = "M", convert = TRUE)  

hexp <- get_scb("health_expend_qu") %>%
    rename(healthexp = indata) %>%
    separate(time, c("year", "quarter"), sep = "K", convert = TRUE)

################## Combine Data ####################

scbdata <- cpi %>% 
    mutate(quarter = (month - 1) %/% 3 + 1) %>% 
    left_join(gdp, by=c("year","quarter")) %>% 
    left_join(pop, by="year") %>% 
    left_join(sick, by=c("year","month")) %>% 
    left_join(hexp, by=c("year","quarter")) %>%
    select(-quarter) %>% 
    filter( year>= 1995)

save(scbdata, file = "../scbdata.Rdata")
saveRDS(scbdata, file = "../scbdata.Rds")
