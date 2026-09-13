library(testthat)
library(sf)

make_square <- function() {
  sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(10, 0), c(10, 10), c(0, 10), c(0, 0)))),
      crs = 3857
    )
  )
}

test_that("extract_geometric_features returns an sf object with requested columns", {
  x <- make_square()
  y <- extract_geometric_features(x, verbose = FALSE, use_parallel = FALSE, make_valid = TRUE)

  expect_s3_class(y, "sf")
  expect_equal(nrow(y), nrow(x))
  expect_true("area_m2" %in% names(y))
  expect_true("perimeter_m" %in% names(y))
  expect_true("centroid_x" %in% names(y))
  expect_true("bbox_width" %in% names(y))
  expect_true("rectangularity" %in% names(y))
  expect_true("num_vertices" %in% names(y))
  expect_true("convexity" %in% names(y))
})

test_that("square metric expectations are stable", {
  x <- make_square()
  y <- extract_geometric_features(x, verbose = FALSE, use_parallel = FALSE, make_valid = TRUE)

  expect_equal(as.numeric(y$area_m2), 100, tolerance = 1e-6)
  expect_equal(as.numeric(y$perimeter_m), 40, tolerance = 1e-6)
  expect_equal(as.numeric(y$bbox_width), 10, tolerance = 1e-6)
  expect_equal(as.numeric(y$bbox_height), 10, tolerance = 1e-6)
  expect_equal(as.numeric(y$rectangularity), 1, tolerance = 1e-6)
})
