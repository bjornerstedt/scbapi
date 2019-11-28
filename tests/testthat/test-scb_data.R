
test_that("population data", {
  housing <- scb_data("data/house_age_stat.json")
  expect_equal(housing[[1,"values"]], 290)

})
