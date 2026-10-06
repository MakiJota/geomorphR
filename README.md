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

Use `validate_morphology_columns()` to inspect whether source fields exist and are numeric before extraction. Orientation is estimated from the minimum rotated rectangle unless an orientation column is supplied.

## Calculated properties

The function always adds these geometric descriptors. Units follow the input CRS; names ending in `_m` or `_m2` correspond to meters only when the CRS uses meters.

| Property | Description |
|:--|:--|
| `area_m2` | Footprint area, in squared input CRS units. |
| `perimeter_m` | Length of the footprint boundary, in input CRS units. |
| `centroid_x` | X coordinate of the footprint centroid. |
| `centroid_y` | Y coordinate of the footprint centroid. |
| `compactness` | Circular compactness (`4 * pi * area / perimeter^2`); a circle has a value of 1. |
| `perimeter_area_ratio` | Perimeter divided by the square root of area, relating boundary length to footprint size. |
| `shape_index` | Perimeter relative to a circle with the same area; a circle has a value of 1. |
| `bbox_width` | Width of the axis-aligned bounding box. |
| `bbox_height` | Height of the axis-aligned bounding box. |
| `bbox_area` | Area of the axis-aligned bounding box. |
| `elongation` | Longer bounding-box side divided by the shorter side; 1 indicates a square box. |
| `rectangularity` | Footprint area divided by bounding-box area; values closer to 1 fill the box more completely. |
| `aspect_ratio` | Bounding-box height divided by bounding-box width. |
| `has_hole` | Whether the footprint contains one or more interior rings. |
| `num_holes` | Number of interior rings in the footprint. |
| `num_vertices` | Number of coordinate rows across all rings, including closing coordinates. |
| `num_parts` | Number of polygon parts in the footprint. |
| `vertices_per_area` | Number of vertices divided by the square root of footprint area. |
| `convex_area_m2` | Area of the footprint's convex hull. |
| `convexity` | Footprint area divided by convex-hull area; values closer to 1 indicate a more convex shape. |
| `equiv_radius_m` | Radius of a circle with the same area as the footprint. |
| `convex_perimeter_m` | Perimeter of the footprint's convex hull. |
| `fractal_dim_proxy` | Shape-complexity proxy using the logarithms of perimeter and area; compare using consistent CRS units. |
| `geometric_complexity_score` | Heuristic combining vertex count, compactness, and hole count; larger values generally indicate more geometric complexity. |
| `orientation_degrees` | Direction of the longest edge of the minimum rotated rectangle, from 0 to less than 180 degrees; an input orientation column overrides this estimate. |

The following properties are generated only when the corresponding source columns are specified in `extract_geometric_features()`. Existing source attributes are retained; the standardized `height_m`, `floors`, `gfa_m2`, and `plot_area_m2` columns are not duplicated when already present.

| Property | Description | Source column(s) |
|:--|:--|:--|
| `height_m` | Building height, copied from the selected height field when `height_m` is not already present. | `height_col` |
| `volume_m3` | Footprint area multiplied by building height. | `height_col` |
| `height_to_width` | Building height divided by bounding-box width. | `height_col` |
| `floors` | Floor/storey count, copied from the selected field when `floors` is not already present. | `floors_col` |
| `floor_height_m` | Building height divided by floor/storey count. | `height_col` and `floors_col` |
| `gfa_m2` | Gross floor area from `gfa_col`, or footprint area multiplied by floor count when only `floors_col` is supplied. | `gfa_col` or `floors_col` |
| `plot_area_m2` | Plot/parcel area, copied from the selected field when `plot_area_m2` is not already present. | `plot_area_col` |
| `coverage_ratio` | Footprint area divided by plot/parcel area. | `plot_area_col` |
| `fsi` | Gross floor area divided by plot/parcel area. | `plot_area_col` and `gfa_col` or `floors_col` |
