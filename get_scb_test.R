library(tidyr)
library(dplyr)
library(stringr)

# Population
scb <- get_scb("pop")
indata <- scb$dataset$value
lab <- as.numeric(unlist(scb$dataset$dimension$Tid$category$label))
d<- matrix(indata, nrow = length(lab))
pop <- data.frame(year = lab )

pop$popwomen <- rowSums(d[ ,1:4 * 2])
pop$popmen <- rowSums(d[ ,1:4 * 2 - 1])

# Real GDP
scb <- get_scb("gdpreal")
indata <- scb$dataset$value
lab <- unlist(scb$dataset$dimension$Tid$category$label)
gdp <- data_frame(yq = lab, gdp=indata ) %>%
    separate(yq, c("year", "quarter"), sep = "K", convert = TRUE) %>%
    na.omit

