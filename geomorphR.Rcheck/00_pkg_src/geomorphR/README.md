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
features <- extract_geometric_features(buildings, verbose = FALSE, use_parallel = FALSE)
```
