testthat::test_that("rnextbus url returns", {
  #skip_on_cran()

  response = httr::GET(nextbus_url())
  testthat::expect_that(response$status_code, equals(200))
})

testthat::test_that("rnextbus agency list returns", {
  #skip_on_cran()

  agencies = nb_list_agencies()
  testthat::expect_that(names(agencies),
                        equals(c("tag", "title", "regionTitle", "shortTitle")))

})

testthat::test_that("List of routes in an agency", {
  #skip_on_cran()

  agency = "sf-muni"
  route_list = nb_list_routes(agency)
  testthat::expect_that(route_list, is_a("character"))

})

testthat::test_that("Route configuration", {
  #skip_on_cran()

  agency = "sf-muni"
  route = "1"
  route_config = nb_route_config(agency, route)
  testthat::expect_that(route_config, is_a("list"))
  testthat::expect_that(route_config[[1]][[1]], equals("1"))

})

testthat::test_that("Route predictions", {
  #skip_on_cran()

  agency = "sf-muni"
  route = "1"
  stop = "NULL"
  predictions = nb_predictions(agency, route, stop)

})

testthat::test_that("Schedule information", {
  #skip_on_cran()

  agency = "sf-muni"
  route = "1"
  schedule = nb_schedule(agency, route)
  testthat::expect_that()

})

testthat::test_that("Agency route messages", {
  #skip_on_cran()

  agency = "sf-muni"
  route = "1"
  message = nb_messages(agency, route)
  testthat::expect_that()

})

testthat::test_that("Vehicle locations", {
  #skip_on_cran()

  agency = "sf-muni"
  route = "1"
  location = nb_vehicle_locations(agency, route)
  testthat::expect_that(names(location$vehicle),
                        equals("id", "routeTag",
                               "dirTag", "lat", "lon",
                               "secsSinceReport",
                               "predictable",
                               "heading", "speedKmHr"))

  t = as.POSIXct(as.numeric(location$lastTime$time)/1000,
                 origin = "1970-01-01", tz = "UTC")
  testthat::expect_that(t, is_a("POSIXct"))
  testthat::expect_gt(t, t > "2016-01-01")

})
