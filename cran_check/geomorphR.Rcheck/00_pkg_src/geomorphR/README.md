# geomorphR

A focused R package for extracting geometric and morphological features and
comparing changes in polygon and multipolygon building footprints using `sf`.

## Overview

The package provides reproducible geometric descriptors for individual
footprints and a temporal comparison workflow for identifying new construction
and demolition. The resulting `sf` objects can be used in spatial analysis and
downstream statistical or machine-learning workflows.

## Installation

Install the released package from CRAN:

```r
install.packages("geomorphR")
```

To install the latest development version from GitHub:

```r
remotes::install_github("MakiJota/geomorphR")
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

## Comparing footprints across periods

`compare_footprints()` compares polygon footprints from two spatial files using
intersection-over-union (IoU). It returns new constructions, potential demolitions,
and enriched input layers. Inputs may be Shapefiles, GeoPackages, or other formats
supported by `sf`. Specify `layer1` and `layer2` when reading GeoPackages with
multiple layers. An optional `boundary_path` clips both periods to a polygon
boundary; differing input CRSs are transformed to the CRS of the earlier layer.

```r
example_dir <- system.file(
  "extdata", "footprint_comparison",
  package = "geomorphR"
)
historical_gpkg <- file.path(example_dir, "geopack", "historical.gpkg")
boundary_shp <- file.path(example_dir, "boundary", "study_area.shp")

comparison <- compare_footprints(
  file1 = historical_gpkg,
  file2 = historical_gpkg,
  layer1 = "old",
  layer2 = "new",
  boundary_path = boundary_shp
)

new_buildings <- comparison$new_constructions
demolished_buildings <- comparison$demolitions
```

The function returns its results as `sf` objects without writing files. Use
`sf::st_write()` to save a result in a format and layer of your choice.
Runnable examples for both the GeoPackage and Shapefile inputs are installed at
`system.file("examples", "compare_footprints.R", package = "geomorphR")`.
The input files are organized under
`system.file("extdata", "footprint_comparison", package = "geomorphR")` in
separate `geopack`, `shp`, and `boundary` folders.

### Comparing threshold combinations

`similarity_threshold` is the minimum IoU used to treat overlapping footprints
as the same building. A lower value is more permissive; a value of `1` requires
identical footprints for a match. `overlap_demolition_threshold` sets the
percentage below which an identified demolition is marked as a potential
demolition. Raising it can mark more demolitions as potential.

This example runs all 16 combinations of similarity thresholds `0.25`, `0.5`,
`0.75`, and `1` with overlap thresholds `0.25`, `0.5`, `0.75`, and `1`. The
overlap values are proportions here and are converted to `25%`, `50%`, `75%`,
and `100%`, respectively, because `overlap_demolition_threshold` is expressed
as a percentage. The example prints these result counts and writes five result
types for each run as named layers in one GeoPackage:

```r
source(system.file(
  "examples", "compare_footprints.R",
  package = "geomorphR"
))
```

The run on the bundled synthetic example produced the following counts. A
`new_with_overlap` feature is a later footprint that overlaps an earlier one
but falls below the similarity threshold; `potential_demolitions` counts
earlier features marked as demolished whose overlap is below the selected
overlap threshold.

| Similarity threshold | Overlap threshold | New constructions | New with overlap | Demolitions | Potential demolitions |
|:--:|:--:|--:|--:|--:|--:|
| 0.25 | 0.25 | 2 | 0 | 2 | 1 |
| 0.25 | 0.50 | 2 | 0 | 2 | 1 |
| 0.25 | 0.75 | 2 | 0 | 2 | 1 |
| 0.25 | 1.00 | 2 | 0 | 2 | 2 |
| 0.50 | 0.25 | 2 | 0 | 2 | 1 |
| 0.50 | 0.50 | 2 | 0 | 2 | 1 |
| 0.50 | 0.75 | 2 | 0 | 2 | 1 |
| 0.50 | 1.00 | 2 | 0 | 2 | 2 |
| 0.75 | 0.25 | 2 | 0 | 2 | 1 |
| 0.75 | 0.50 | 2 | 0 | 2 | 1 |
| 0.75 | 0.75 | 2 | 0 | 2 | 1 |
| 0.75 | 1.00 | 2 | 0 | 2 | 2 |
| 1.00 | 0.25 | 3 | 1 | 3 | 1 |
| 1.00 | 0.50 | 3 | 1 | 3 | 1 |
| 1.00 | 0.75 | 3 | 1 | 3 | 1 |
| 1.00 | 1.00 | 3 | 1 | 3 | 2 |

On this example, raising the similarity threshold from `0.75` to `1` makes the
slightly changed footprint fail the match, so it is counted as one additional
new construction and one additional demolition. The construction year change
also identifies a separate replacement at every similarity setting. Raising
the overlap threshold does not change which features are new or demolished;
it only marks more demolitions as potential. At `100%`, the example marks two
potential demolitions rather than one because both have less than complete
overlap with their successors. These results illustrate the thresholds on this
small synthetic dataset and should not be treated as real-world estimates.

Each run produces five result layers, so the output GeoPackage contains 80
layers. The bundled input files are small synthetic polygon examples created
for this package; they are intended to demonstrate the workflow, not to
represent real city data. Generated comparison outputs are not bundled; run
the example script to create them locally.

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
| `bbox_width` | Longer side of the minimum rotated bounding rectangle. |
| `bbox_height` | Shorter side of the minimum rotated bounding rectangle. |
| `bbox_area` | Area of the minimum rotated bounding rectangle. |
| `elongation` | Longer side divided by shorter side of the minimum rotated bounding rectangle; 1 indicates a square box and the value is invariant to map rotation. |
| `rectangularity` | Footprint area divided by minimum rotated bounding-rectangle area; values closer to 1 fill the box more completely and the value is invariant to map rotation. |
| `aspect_ratio` | Shorter side divided by longer side of the minimum rotated bounding rectangle; values range from 0 to 1. |
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

### Interpreting and comparing metrics

Several output columns are mathematically related rather than independent
measurements. For example, `shape_index` is `1 / sqrt(compactness)`,
`perimeter_area_ratio` is a rescaling of `shape_index`, `vertices_per_area`
combines `num_vertices` and `area_m2`, and `convexity` is
`area_m2 / convex_area_m2`. Avoid giving these related columns repeated weight
in clustering or other multivariate analyses; choose metrics based on the
question or standardize/select a non-redundant set. `shape_index` here increases
with irregularity, so compare formulas rather than names when using results
from other packages.

Vertex-based measures such as `num_vertices` and
`geometric_complexity_score` also depend on how the source outlines were
digitized, not only on building shape. Treat them cautiously when input data
mixes manually traced and model-generated footprints.

The following properties are generated only when the corresponding source columns are specified in `extract_geometric_features()`. Existing source attributes are retained; the standardized `height_m`, `floors`, `gfa_m2`, and `plot_area_m2` columns are not duplicated when already present.

| Property | Description | Source column(s) |
|:--|:--|:--|
| `height_m` | Building height, copied from the selected height field when `height_m` is not already present. | `height_col` |
| `volume_m3` | Footprint area multiplied by building height. | `height_col` |
| `height_to_width` | Building height divided by the longer side of the minimum rotated bounding rectangle. | `height_col` |
| `floors` | Floor/storey count, copied from the selected field when `floors` is not already present. | `floors_col` |
| `floor_height_m` | Building height divided by floor/storey count. | `height_col` and `floors_col` |
| `gfa_m2` | Gross floor area from `gfa_col`, or footprint area multiplied by floor count when only `floors_col` is supplied. | `gfa_col` or `floors_col` |
| `plot_area_m2` | Plot/parcel area, copied from the selected field when `plot_area_m2` is not already present. | `plot_area_col` |
| `coverage_ratio` | Footprint area divided by plot/parcel area. | `plot_area_col` |
| `fsi` | Gross floor area divided by plot/parcel area. | `plot_area_col` and `gfa_col` or `floors_col` |
