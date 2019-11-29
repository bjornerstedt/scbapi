library(tidyverse)

test_that("population data", {
  housing <- scb_data("data/house_age_stat.json")
  expect_equal(
    housing %>% filter(Hustyp == "FLERBOST", Tid == 2018) %>% spread(Byggnadsperiod, values) %>% filter(Region == "0114") %>% pull(`2001-2010`),
    582
  )
  expect_equal(housing[[1,"values"]], 290)
  expect_error(scb_data("data/house_age.json"), "Error: This is a SCB JSON file, not a JSON-data file.")
})
