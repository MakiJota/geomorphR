# Footprint comparison sample data

The example data are organized by format:

- `geopack/historical.gpkg` contains synthetic `old` and `new` polygon layers.
- `shp/old.shp` and `shp/new.shp` contain the same synthetic footprints.
  Keep each `.shp` together with its `.shx`, `.dbf`, and `.prj` sidecar files.
- `boundary/study_area.shp` is a synthetic polygon boundary, with its
  required Shapefile sidecar files.

All examples use WGS 84 / Pseudo-Mercator (EPSG:3857) and are small,
programmatically generated rectangles designed only to demonstrate the package
workflow. They do not contain data copied from the Gothenburg historical
building dataset.

The real Gothenburg project is described by the Spatial Morphology Group at
Chalmers in the
[Time model Gothenburg project](https://smog.chalmers.se/projects/time-model-gothenburg-1960-2015/).
Its
[SND catalogue record](https://doi.org/10.5878/ke11-je22) states that the
building layers moved to a
[separate record](https://doi.org/10.5878/t8s9-6y15) and were unpublished due
to licensing restrictions on their source dataset. Those real data and
comparison outputs derived from them are not included in this package.

The synthetic examples are included under the package license in `LICENSE`.
