library(testthat)

make_square <- function() {
  sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(10, 0), c(10, 10), c(0, 10), c(0, 0)))),
      crs = 3857
    )
  )
}

make_rotated_rectangle <- function(angle_degrees) {
  angle <- angle_degrees * pi / 180
  coordinates <- rbind(
    c(-5, -2), c(5, -2), c(5, 2), c(-5, 2), c(-5, -2)
  )
  rotation <- matrix(
    c(cos(angle), sin(angle), -sin(angle), cos(angle)),
    nrow = 2
  )
  coordinates <- coordinates %*% rotation
  sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(sf::st_polygon(list(coordinates)), crs = 3857)
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

test_that("elongation and rectangularity are invariant to map rotation", {
  horizontal <- extract_geometric_features(
    make_rotated_rectangle(0),
    verbose = FALSE,
    use_parallel = FALSE
  )
  diagonal <- extract_geometric_features(
    make_rotated_rectangle(45),
    verbose = FALSE,
    use_parallel = FALSE
  )

  expect_equal(as.numeric(diagonal$elongation), 2.5, tolerance = 1e-6)
  expect_equal(as.numeric(diagonal$rectangularity), 1, tolerance = 1e-6)
  expect_equal(as.numeric(diagonal$aspect_ratio), 0.4, tolerance = 1e-6)
  expect_equal(diagonal$elongation, horizontal$elongation, tolerance = 1e-6)
  expect_equal(diagonal$rectangularity, horizontal$rectangularity, tolerance = 1e-6)
  expect_equal(diagonal$bbox_width, horizontal$bbox_width, tolerance = 1e-6)
  expect_equal(diagonal$bbox_height, horizontal$bbox_height, tolerance = 1e-6)
})

test_that("make_valid repairs invalid geometries before measuring", {
  invalid <- sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(0, 0), c(2, 2), c(0, 2), c(2, 0), c(0, 0)
      ))),
      crs = 3857
    )
  )

  expect_false(sf::st_is_valid(invalid))
  repaired <- extract_geometric_features(invalid, verbose = FALSE, make_valid = TRUE)

  expect_true(all(sf::st_is_valid(repaired)))
  expect_equal(as.numeric(repaired$area_m2), 2, tolerance = 1e-6)
})

test_that("attribute-based morphology metrics are derived from explicit columns", {
  x <- make_square()
  x$building_height <- 10
  x$storeys <- 2
  x$parcel_area <- 200

  y <- extract_geometric_features(
    x,
    verbose = FALSE,
    height_col = "building_height",
    floors_col = "storeys",
    plot_area_col = "parcel_area"
  )

  expect_equal(as.numeric(y$gfa_m2), 200, tolerance = 1e-6)
  expect_equal(as.numeric(y$fsi), 1, tolerance = 1e-6)
  expect_equal(as.numeric(y$coverage_ratio), 0.5, tolerance = 1e-6)
  expect_equal(as.numeric(y$volume_m3), 1000, tolerance = 1e-6)
  expect_equal(as.numeric(y$floor_height_m), 5, tolerance = 1e-6)
  expect_true(all(y$orientation_degrees >= 0 & y$orientation_degrees < 180))
})

test_that("morphology column validation identifies unusable inputs", {
  x <- data.frame(height = as.numeric(c(NA, NA)), floors = c(2, 3))
  report <- validate_morphology_columns(x, height_col = "height", floors_col = "floors")

  expect_true(all(report$present))
  expect_true(all(report$numeric))
  expect_equal(report$missing_values[report$metric == "height_m"], 2)
  expect_false(report$usable[report$metric == "height_m"])
  expect_true(report$usable[report$metric == "floors"])
})

test_that("requested morphology columns must be numeric", {
  x <- make_square()
  x$height <- "ten"

  expect_error(
    extract_geometric_features(x, verbose = FALSE, height_col = "height"),
    "must exist, be numeric"
  )
})

test_that("bundled example data supports morphology extraction", {
  path <- system.file("extdata", "central_gbg.geojson", package = "geomorphR")
  if (!nzchar(path)) path <- testthat::test_path("..", "..", "inst", "extdata", "central_gbg.geojson")
  buildings <- sf::st_read(path, quiet = TRUE)

  expect_true(all(c("height_m", "floor_count", "orientation_degrees", "gfa_m2", "plot_area_m2") %in% names(buildings)))
  features <- extract_geometric_features(
    buildings,
    verbose = FALSE,
    height_col = "height_m",
    floors_col = "floor_count",
    orientation_col = "orientation_degrees",
    gfa_col = "gfa_m2",
    plot_area_col = "plot_area_m2"
  )

  expect_equal(nrow(features), nrow(buildings))
  expect_true(all(is.finite(features$fsi)))
  expect_true(all(features$orientation_degrees >= 0 & features$orientation_degrees < 180))
})
