test_that("population data", {
  pop = get_scb("data/pop", download_data = FALSE)
  expect_equal(pop[[1,4]], 1833641)
  expect_equal(pop[[1,4]], 1833643, tolerance=2, scale = 1)
  expect_error(get_scb(), "Either file or query has to be specified")

})

test_that("GDP data", {
  gdp = get_scb("data/gdpreal", download_data = FALSE, returnList = TRUE)
  expect_equal(gdp[["value"]][5], 492939)
  expect_error(get_scb("asdf"), "Could not find query file")
  expect_error(get_scb("data/gdpreal2", download_data = FALSE), "Could not find data file")
})

test_that("Pop detail", {
    # data/pop_distr does not exist:
    expect_error(get_scb("data/pop_distr"), "SCB Server error: 404 - File or directory not found.")

  expect_equal(get_scb("data/popdetail")[[1,"values"]], 3959115)
})

test_that("Standalone", {
    query = '
{
  "query": [],
  "response": {
    "format": "px"
  }
}'
url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/START/PR/PR0101/PR0101A/KPIFastM"

df = get_scb(query = query, url = url)
expect_equal(df[[1,2]], 95.3)
})

