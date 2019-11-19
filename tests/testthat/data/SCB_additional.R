# devtools::install_github("bjornerstedt/scbapi")
library(scbapi)
library(tidyr)
library(dplyr)
library(stringr)
library(jsonlite)
library(httr)

# Population
hexp <- get_scb("health_expend_qu") %>% 
    separate(time, c("year", "quarter"), sep = "K", convert = TRUE) %>% 
    mutate(
        month_1 = (quarter - 1) * 3 + 1,
        month_2 = (quarter - 1) * 3 + 2,
        month_3 = (quarter - 1) * 3 + 3
    ) %>% 
    gather(mn,month, matches("month_")) %>% 
    select(year, month, hexp=indata) %>% 
    arrange(year,month) %>% 
    filter(year >= 1995) %>% 
    mutate(
        date = ymd(year*10000+month*100+01)
    )


saveRDS(hexp, file = "../scb_additional.Rds")
