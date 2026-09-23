# geomorphR

A focused R package for extracting geometric and morphological features from polygon and multipolygon building footprints represented in `sf` objects.

## Overview

The objective is to provide a clean, transparent, reproducible API to compute raw geometric and morphology descriptors that can feed further spatial or machine-learning workflows.

## Installation

Install directly from GitHub with:

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

remotes::install_github("YOUR_USERNAME/geomorphR")
```

## Example

```r
library(sf)
library(geomorphR)

geojson_path <- system.file("extdata", "central_gbg.geojson", package = "geomorphR")
buildings <- sf::st_read(geojson_path)
features <- extract_geometric_features(
  buildings,
  height_col = "height_m",
  floors_col = "floor_count",
  orientation_col = "orientation_degrees",
  gfa_col = "gfa_m2",
  plot_area_col = "plot_area_m2",
  verbose = FALSE,
  use_parallel = FALSE
)
```

## Building morphology attributes

Attribute-based metrics are opt-in because datasets use different field names. Pass the source columns explicitly:

```r
features <- extract_geometric_features(
  buildings,
  height_col = "height_m",
  floors_col = "floor_count",
  gfa_col = "gfa_m2",
  plot_area_col = "plot_area_m2",
  verbose = FALSE
)
```

The function adds building volume, floor height, GFA, FSI, coverage ratio, height-to-width ratio, and orientation in degrees. If no orientation column is supplied, orientation is estimated from the minimum rotated rectangle. Use `validate_morphology_columns()` to inspect whether source fields exist and are numeric before extraction.
