make_comparison_layer <- function(boxes, years = NULL, crs = 3857) {
  geometries <- lapply(boxes, function(box) {
    sf::st_polygon(list(rbind(
      c(box[1], box[2]), c(box[3], box[2]), c(box[3], box[4]),
      c(box[1], box[4]), c(box[1], box[2])
    )))
  })
  result <- sf::st_sf(
    id = seq_along(geometries),
    geometry = sf::st_sfc(geometries, crs = crs)
  )
  if (!is.null(years)) result$Year_of_Co <- years
  result
}

test_that("compare_footprints reads and compares Shapefile and GeoPackage inputs", {
  earlier <- make_comparison_layer(
    list(c(0, 0, 10, 10), c(20, 0, 30, 10)),
    years = c(2000, 2000)
  )
  later <- make_comparison_layer(
    list(c(0, 0, 9, 10), c(40, 0, 50, 10)),
    years = c(2000, 2010)
  )

  shp_path <- tempfile(fileext = ".shp")
  gpkg_path <- tempfile(fileext = ".gpkg")
  sf::st_write(earlier, shp_path, quiet = TRUE)
  sf::st_write(later, gpkg_path, layer = "later_period", quiet = TRUE)

  result <- compare_footprints(
    shp_path,
    gpkg_path,
    layer2 = "later_period",
    verbose = FALSE
  )

  expect_s3_class(result$new_constructions, "sf")
  expect_equal(nrow(result$new_constructions), 1L)
  expect_equal(nrow(result$clean_constructions), 1L)
  expect_equal(nrow(result$demolitions), 1L)
  expect_equal(result$new_constructions$construction_type, "clean_construction")
  expect_equal(result$gdf2_enriched$construction_type, c("similar_building", "clean_construction"))
  expect_equal(result$gdf1_enriched$is_demolished, c(FALSE, TRUE))
})

test_that("compare_footprints reads named layers from one GeoPackage", {
  gpkg_path <- tempfile(fileext = ".gpkg")
  earlier <- make_comparison_layer(list(c(0, 0, 10, 10)))
  later <- make_comparison_layer(list(c(1, 0, 11, 10)))
  sf::st_write(earlier, gpkg_path, layer = "earlier", quiet = TRUE)
  sf::st_write(later, gpkg_path, layer = "later", append = TRUE, quiet = TRUE)

  result <- compare_footprints(
    gpkg_path,
    gpkg_path,
    layer1 = "earlier",
    layer2 = "later",
    verbose = FALSE
  )

  expect_equal(nrow(result$new_constructions), 0L)
  expect_equal(result$gdf2_enriched$construction_type, "similar_building")
})

test_that("compare_footprints compares geometry when the optional year field is absent", {
  earlier <- make_comparison_layer(list(c(0, 0, 10, 10)))
  later <- make_comparison_layer(list(c(0, 0, 10, 10)))
  earlier_path <- tempfile(fileext = ".gpkg")
  later_path <- tempfile(fileext = ".gpkg")
  sf::st_write(earlier, earlier_path, quiet = TRUE)
  sf::st_write(later, later_path, quiet = TRUE)

  result <- compare_footprints(earlier_path, later_path, verbose = FALSE)

  expect_false(result$gdf2_enriched$year_construction_changed)
  expect_false(result$gdf1_enriched$is_demolished)
})

test_that("compare_footprints transforms CRS and identifies year-based replacement", {
  earlier <- make_comparison_layer(
    list(c(0, 0, 10, 10)),
    years = 2000
  )
  later <- sf::st_transform(earlier, 4326)
  later$Year_of_Co <- 2010
  earlier_path <- tempfile(fileext = ".gpkg")
  later_path <- tempfile(fileext = ".gpkg")
  sf::st_write(earlier, earlier_path, quiet = TRUE)
  sf::st_write(later, later_path, quiet = TRUE)

  result <- compare_footprints(earlier_path, later_path, verbose = FALSE)

  expect_equal(nrow(result$new_constructions), 1L)
  expect_equal(result$new_constructions$construction_type, "replaced_building")
  expect_true(result$gdf2_enriched$year_construction_changed)
  expect_true(result$gdf1_enriched$is_demolished)
  expect_true(sf::st_crs(result$gdf2_enriched) == sf::st_crs(earlier))
})

test_that("compare_footprints clips both layers to a boundary", {
  earlier <- make_comparison_layer(list(c(0, 0, 10, 10), c(20, 0, 30, 10)))
  later <- make_comparison_layer(list(c(0, 0, 10, 10), c(20, 0, 30, 10)))
  boundary <- make_comparison_layer(list(c(-1, -1, 11, 11)))
  earlier_path <- tempfile(fileext = ".gpkg")
  later_path <- tempfile(fileext = ".gpkg")
  boundary_path <- tempfile(fileext = ".gpkg")
  sf::st_write(earlier, earlier_path, quiet = TRUE)
  sf::st_write(later, later_path, quiet = TRUE)
  sf::st_write(boundary, boundary_path, quiet = TRUE)

  result <- compare_footprints(
    earlier_path,
    later_path,
    boundary_path = boundary_path,
    verbose = FALSE
  )

  expect_equal(nrow(result$gdf1_enriched), 1L)
  expect_equal(nrow(result$gdf2_enriched), 1L)
  expect_equal(nrow(result$new_constructions), 0L)
})

test_that("bundled synthetic GeoPackage, Shapefile, and boundary examples are usable", {
  example_dir <- system.file(
    "extdata", "footprint_comparison",
    package = "geomorphR"
  )
  gpkg_path <- file.path(example_dir, "geopack", "historical.gpkg")
  boundary_path <- file.path(example_dir, "boundary", "study_area.shp")
  shp_dir <- file.path(example_dir, "shp")

  expect_true(file.exists(gpkg_path))
  expect_true(file.exists(boundary_path))
  expect_true(file.exists(system.file(
    "examples", "compare_footprints.R",
    package = "geomorphR"
  )))
  expect_setequal(sf::st_layers(gpkg_path)[["name"]], c("old", "new"))

  gpkg_result <- compare_footprints(
    gpkg_path,
    gpkg_path,
    layer1 = "old",
    layer2 = "new",
    boundary_path = boundary_path,
    verbose = FALSE
  )
  expect_equal(nrow(gpkg_result$gdf1_enriched), 3L)
  expect_equal(nrow(gpkg_result$gdf2_enriched), 3L)
  expect_equal(nrow(gpkg_result$new_constructions), 2L)
  expect_equal(nrow(gpkg_result$demolitions), 2L)

  shp_result <- compare_footprints(
    file.path(shp_dir, "old.shp"),
    file.path(shp_dir, "new.shp"),
    boundary_path = boundary_path,
    verbose = FALSE
  )
  expect_s3_class(shp_result$new_constructions, "sf")
  expect_equal(nrow(shp_result$gdf1_enriched), 3L)
  expect_equal(nrow(shp_result$gdf2_enriched), 3L)
})

test_that("compare_footprints validates files, layers, CRS, and thresholds", {
  gpkg_path <- tempfile(fileext = ".gpkg")
  layer <- make_comparison_layer(list(c(0, 0, 1, 1)))
  sf::st_write(layer, gpkg_path, quiet = TRUE)

  expect_error(compare_footprints("missing.shp", "also_missing.shp"), "does not exist")
  expect_error(
    compare_footprints(gpkg_path, gpkg_path, layer1 = ""),
    "layer must be"
  )
  expect_error(
    compare_footprints("missing.shp", "also_missing.shp", similarity_threshold = 2),
    "must be between 0 and 1"
  )
})
