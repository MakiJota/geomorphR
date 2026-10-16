# geomorphR 0.2.1

## Bug fixes and performance

* Calculate bounding-box dimensions, elongation, rectangularity, and aspect
  ratio from the minimum rotated rectangle, so these metrics no longer change
  when the same footprint is rotated relative to the map axes.
* Avoid repairing already-valid geometries and vectorize convex-hull
  calculations in the default non-parallel extraction path.
* Clarify metric dependencies, shape-index interpretation, and the source
  sensitivity of vertex-based measures in the README.

# geomorphR 0.2.0

## New functionality

* Add `compare_footprints()` to compare polygon footprints across two vector
  files, including named GeoPackage layers, optional boundary clipping,
  coordinate reference system transformation, and new-construction and
  demolition classifications.
* Include small, synthetic GeoPackage and Shapefile examples with a boundary,
  plus a runnable threshold-sensitivity example script.

## Other changes

* Expand the package README and DESCRIPTION to cover footprint comparison.
