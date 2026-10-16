# Rotation test output

Run from the package root with:

```r
Rscript response/rotation_test.R
```

The script rotates the bundled `central_gbg.geojson` footprints through a full
360-degree turn in 2-degree steps (180 frames). It leaves out the duplicate
360-degree endpoint because that is the same image as 0 degrees; the loop
seam therefore advances only 2 degrees, from 358 back to 0. A fixed square map
extent sized to contain the farthest rotated bounding-box corner keeps the
zoom, map position, and plot layout stable throughout the animation.
It compares the old axis-aligned bounding-box formulas with the patched
minimum-rotated-rectangle metrics and saves:

- `rotation_invariance.gif`: map view of rectangularity at each rotation.
- `rotation_metrics.csv`: median metric values and rank correlations by angle.
- `rotation_test_summary.txt`: 30-degree rank-correlation comparison and scope.

This is a local reproduction of the rotation test method, not a reproduction of
the report's Dorcol result. The bundled data has 246 footprints; the report
used 2,005 Dorcol footprints.
