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

# Run all requested threshold combinations and write five named result layers
# per run. Overlap thresholds are specified as fractions here and converted to
# percentages for compare_footprints().
similarity_thresholds <- c(0.25, 0.5, 0.75, 1)
overlap_thresholds <- c(0.25, 0.5, 0.75, 1)
output_gpkg <- file.path(getwd(), "footprint_comparison_sensitivity.gpkg")
if (file.exists(output_gpkg)) {
  stop("Refusing to overwrite existing output: ", output_gpkg)
}

first_layer <- TRUE
summary_rows <- list()
for (similarity in similarity_thresholds) {
  for (overlap_fraction in overlap_thresholds) {
    overlap_percent <- overlap_fraction * 100
    result <- compare_footprints(
      file1 = historical_gpkg,
      file2 = historical_gpkg,
      layer1 = "old",
      layer2 = "new",
      boundary_path = boundary_shp,
      similarity_threshold = similarity,
      overlap_demolition_threshold = overlap_percent,
      period1 = "old",
      period2 = "new",
      verbose = FALSE
    )

    summary_rows[[length(summary_rows) + 1L]] <- data.frame(
      similarity_threshold = similarity,
      overlap_threshold = overlap_fraction,
      new_constructions = nrow(result$new_constructions),
      new_with_overlap = sum(
        result$gdf2_enriched$construction_type == "new_with_overlap"
      ),
      demolitions = nrow(result$demolitions),
      potential_demolitions = sum(
        result$gdf1_enriched$potential_demolition,
        na.rm = TRUE
      )
    )

    for (result_name in names(result)) {
      layer <- result[[result_name]]
      layer$similarity_threshold <- rep(similarity, nrow(layer))
      layer$overlap_demolition_threshold <- rep(overlap_percent, nrow(layer))
      layer_name <- sprintf(
        "sim_%03d_ov_%03d_%s",
        round(similarity * 100),
        round(overlap_percent),
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

message("Results by threshold combination:")
print(do.call(rbind, summary_rows), row.names = FALSE)
message("Wrote comparison layers to: ", output_gpkg)
