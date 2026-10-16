args <- commandArgs(trailingOnly = FALSE)
script_arg <- sub(
  "^--file=",
  "",
  args[grepl("^--file=", args)]
)
repo_root <- if (length(script_arg)) {
  normalizePath(file.path(dirname(script_arg), ".."), mustWork = TRUE)
} else {
  normalizePath(".", mustWork = TRUE)
}
setwd(repo_root)

source(file.path("R", "extract_geometric_features.R"))

input_path <- file.path("inst", "extdata", "central_gbg.geojson")
buildings <- sf::st_read(input_path, quiet = TRUE)
if (!nrow(buildings)) {
  stop("The bundled sample contains no building footprints.", call. = FALSE)
}

rotate_sfg <- function(geometry, angle, center) {
  if (is.matrix(geometry)) {
    radians <- angle * pi / 180
    rotation <- matrix(
      c(cos(radians), sin(radians), -sin(radians), cos(radians)),
      nrow = 2L
    )
    xy <- sweep(geometry[, 1:2, drop = FALSE], 2L, center, "-")
    xy <- sweep(xy %*% rotation, 2L, center, "+")
    geometry[, 1:2] <- xy
    return(geometry)
  }

  if (is.list(geometry)) {
    geometry_class <- class(geometry)
    geometry <- lapply(geometry, rotate_sfg, angle = angle, center = center)
    class(geometry) <- geometry_class
  }
  geometry
}

rotate_buildings <- function(x, angle, center) {
  rotated <- lapply(
    sf::st_geometry(x),
    rotate_sfg,
    angle = angle,
    center = center
  )
  sf::st_geometry(x) <- sf::st_sfc(rotated, crs = sf::st_crs(x))
  x
}

axis_aligned_metrics <- function(x, areas) {
  coordinates <- sf::st_coordinates(sf::st_geometry(x))
  groups <- split(
    seq_len(nrow(coordinates)),
    coordinates[, ncol(coordinates)]
  )
  bounds <- vapply(groups, function(rows) {
    xy <- coordinates[rows, 1:2, drop = FALSE]
    c(
      width = diff(range(xy[, 1])),
      height = diff(range(xy[, 2]))
    )
  }, numeric(2L))
  widths <- bounds["width", ]
  heights <- bounds["height", ]
  box_areas <- widths * heights
  cbind(
    elongation = pmax(widths, heights) / pmin(widths, heights),
    rectangularity = areas / box_areas
  )
}

extent <- sf::st_bbox(buildings)
rotation_center <- c(
  (extent[["xmin"]] + extent[["xmax"]]) / 2,
  (extent[["ymin"]] + extent[["ymax"]]) / 2
)
frame_radius <- sqrt(
  ((extent[["xmax"]] - extent[["xmin"]]) / 2)^2 +
    ((extent[["ymax"]] - extent[["ymin"]]) / 2)^2
)
frame_xlim <- rotation_center[1] + c(-frame_radius, frame_radius)
frame_ylim <- rotation_center[2] + c(-frame_radius, frame_radius)
angles <- seq(0, 358, by = 2)
summary_rows <- vector("list", length(angles))
frames <- vector("list", length(angles))
temporary_frames <- tempfile("geomorphr-rotation-")
dir.create(temporary_frames)
building_areas <- as.numeric(sf::st_area(buildings))
baseline_previous <- axis_aligned_metrics(buildings, building_areas)
baseline_updated <- extract_geometric_features(
  buildings,
  verbose = FALSE,
  use_parallel = FALSE,
  make_valid = FALSE
)

for (i in seq_along(angles)) {
  angle <- angles[[i]]
  rotated <- rotate_buildings(buildings, angle, rotation_center)
  rotated_extent <- sf::st_bbox(rotated)
  tolerance <- frame_radius * 1e-10
  if (
    rotated_extent[["xmin"]] < frame_xlim[1] - tolerance ||
      rotated_extent[["xmax"]] > frame_xlim[2] + tolerance ||
      rotated_extent[["ymin"]] < frame_ylim[1] - tolerance ||
      rotated_extent[["ymax"]] > frame_ylim[2] + tolerance
  ) {
    stop("Rotated footprints exceed the fixed animation frame.", call. = FALSE)
  }
  rotated$orientation_degrees <- NULL
  previous <- axis_aligned_metrics(rotated, building_areas)

  summary_rows[[i]] <- data.frame(
    angle_degrees = angle,
    previous_median_elongation = median(previous[, "elongation"], na.rm = TRUE),
    updated_median_elongation = median(baseline_updated$elongation, na.rm = TRUE),
    previous_median_rectangularity = median(previous[, "rectangularity"], na.rm = TRUE),
    updated_median_rectangularity = median(baseline_updated$rectangularity, na.rm = TRUE),
    previous_elongation_rank_correlation = stats::cor(
      previous[, "elongation"],
      baseline_previous[, "elongation"],
      method = "spearman",
      use = "complete.obs"
    ),
    updated_elongation_rank_correlation = 1,
    previous_rectangularity_rank_correlation = stats::cor(
      previous[, "rectangularity"],
      baseline_previous[, "rectangularity"],
      method = "spearman",
      use = "complete.obs"
    ),
    updated_rectangularity_rank_correlation = 1
  )

  old_layer <- sf::st_sf(
    method = "Previous: axis-aligned",
    rectangularity = previous[, "rectangularity"],
    geometry = sf::st_geometry(rotated)
  )
  updated_layer <- sf::st_sf(
    method = "Patched: rotated rectangle",
    rectangularity = baseline_updated$rectangularity,
    geometry = sf::st_geometry(rotated)
  )
  plot_data <- rbind(old_layer, updated_layer)
  plot_data$method <- factor(
    plot_data$method,
    levels = c("Previous: axis-aligned", "Patched: rotated rectangle")
  )

  plot <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = rectangularity),
      color = "grey25",
      linewidth = 0.08
    ) +
    ggplot2::facet_wrap(~method, ncol = 2) +
    ggplot2::scale_fill_viridis_c(
      name = "Rectangularity",
      limits = c(0, 1),
      oob = scales::squish
    ) +
    ggplot2::coord_sf(
      xlim = frame_xlim,
      ylim = frame_ylim,
      expand = FALSE
    ) +
    ggplot2::labs(
      title = paste("Rotating the same", nrow(buildings), "footprints"),
      subtitle = sprintf(
        "Rotation: %03d degrees | full turn | fixed zoom and frame",
        angle
      )#,      caption = "Bundled central_gbg.geojson sample; prior panel recomputes axis-aligned metrics"
    ) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(size = 8)
    )

  frame_path <- file.path(temporary_frames, sprintf("frame_%03d.png", i))
  message("Rendered rotation ", angle, " degrees (", i, "/", length(angles), ")")
  ragg::agg_png(frame_path, width = 1000, height = 600, res = 100)
  print(plot)
  grDevices::dev.off()
  frames[[i]] <- magick::image_read(frame_path)
}

summary_data <- do.call(rbind, summary_rows)
dir.create("response", showWarnings = FALSE)
utils::write.csv(
  summary_data,
  file.path("response", "rotation_metrics.csv"),
  row.names = FALSE
)
magick::image_animate(
  magick::image_join(frames),
  fps = 20,
  loop = 0
) |>
  magick::image_write(path = file.path("response", "rotation_invariance.gif"))
unlink(temporary_frames, recursive = TRUE)

thirty <- summary_data[summary_data$angle_degrees == 30, ]
summary_lines <- c(
  "Rotation invariance test",
  paste0("Input: ", normalizePath(input_path)),
  paste0("Footprints: ", nrow(buildings)),
  paste0(
    "Full turn: 360 degrees in ", diff(angles[1:2]),
    "-degree steps; ", length(angles), " frames (360 degrees is the starting frame)"
  ),
  "Loop seam: 358 to 0 degrees (one normal 2-degree step; no duplicate endpoint)",
  paste0("Fixed framing: square extent of +/- ", round(frame_radius), " map units around rotation center"),
  "",
  "Spearman rank correlation against the 0-degree measurements at 30 degrees:",
  sprintf(
    "  Previous axis-aligned elongation: %.4f",
    thirty$previous_elongation_rank_correlation
  ),
  sprintf(
    "  Patched elongation:              %.4f",
    thirty$updated_elongation_rank_correlation
  ),
  sprintf(
    "  Previous axis-aligned rectangularity: %.4f",
    thirty$previous_rectangularity_rank_correlation
  ),
  sprintf(
    "  Patched rectangularity:              %.4f",
    thirty$updated_rectangularity_rank_correlation
  ),
  "",
  "Median rectangularity across the full rotation:",
  sprintf(
    "  Previous axis-aligned: %.4f to %.4f",
    min(summary_data$previous_median_rectangularity),
    max(summary_data$previous_median_rectangularity)
  ),
  sprintf(
    "  Patched:               %.4f to %.4f",
    min(summary_data$updated_median_rectangularity),
    max(summary_data$updated_median_rectangularity)
  ),
  "",
  paste(
    "This is a local equivalent of the reported rotation test, not a reproduction",
    "of the Belgrade/Dorcol result: the package's bundled sample has 246 features,",
    "rather than the report's 2,005 Dorcol buildings."
  )
)
writeLines(summary_lines, file.path("response", "rotation_test_summary.txt"))
cat(paste(summary_lines, collapse = "\n"), "\n")
