library(geomorphR)
library(sf)

example_dir <- system.file(
  "extdata", "footprint_comparison",
  package = "geomorphR"
)
historical_gpkg <- file.path(example_dir, "geopack", "historical.gpkg")
boundary_shp <- file.path(example_dir, "boundary", "study_area.shp")
shapefile_dir <- file.path(example_dir, "shp")

# Compare the old and new layers stored in a single GeoPackage.
gpkg_result <- compare_footprints(
  file1 = historical_gpkg,
  file2 = historical_gpkg,
  layer1 = "old",
  layer2 = "new",
  boundary_path = boundary_shp,
  period1 = "old",
  period2 = "new",
  verbose = FALSE
)

# The same comparison using the supplied Shapefile examples.
shapefile_result <- compare_footprints(
  file1 = file.path(shapefile_dir, "old.shp"),
  file2 = file.path(shapefile_dir, "new.shp"),
  boundary_path = boundary_shp,
  period1 = "old",
  period2 = "new",
  verbose = FALSE
)

# Run all requested thresholds and write five named result layers per run.
similarity_thresholds <- c(0.25, 0.5, 1)
overlap_thresholds <- c(25, 50, 100)
output_gpkg <- file.path(getwd(), "footprint_comparison_sensitivity.gpkg")
if (file.exists(output_gpkg)) {
  stop("Refusing to overwrite existing output: ", output_gpkg)
}

first_layer <- TRUE
for (similarity in similarity_thresholds) {
  for (overlap in overlap_thresholds) {
    result <- compare_footprints(
      file1 = historical_gpkg,
      file2 = historical_gpkg,
      layer1 = "old",
      layer2 = "new",
      boundary_path = boundary_shp,
      similarity_threshold = similarity,
      overlap_demolition_threshold = overlap,
      period1 = "old",
      period2 = "new",
      verbose = FALSE
    )

    for (result_name in names(result)) {
      layer <- result[[result_name]]
      layer$similarity_threshold <- rep(similarity, nrow(layer))
      layer$overlap_demolition_threshold <- rep(overlap, nrow(layer))
      layer_name <- sprintf(
        "sim_%03d_ov_%03d_%s",
        round(similarity * 100),
        overlap,
        result_name
      )
      sf::st_write(
        layer,
        output_gpkg,
        layer = layer_name,
        append = !first_layer,
        quiet = TRUE
      )
      first_layer <- FALSE
    }
  }
}

message("Wrote comparison layers to: ", output_gpkg)
