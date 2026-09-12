#' Extract geometric and morphological descriptors from polygonal building footprints
#'
#' Computes a reproducible set of polygon geometry descriptors for an sf
#' object containing POLYGON or MULTIPOLYGON geometries. The function
#' appends new feature columns and preserves the original geometry.
#'
#' @param buildings_sf An sf object with POLYGON or MULTIPOLYGON geometries.
#' @param verbose Logical; print progress messages when TRUE.
#' @param use_parallel Logical; optional future-based parallel computation.
#' @param n_cores Integer or NULL; number of workers used by future.apply.
#' @param make_valid Logical; call sf::st_make_valid() before extraction.
#' @return An sf object with appended geometry features.
#' @export
extract_geometric_features <- function(buildings_sf,
                                        verbose = TRUE,
                                        use_parallel = FALSE,
                                        n_cores = NULL,
                                        make_valid = TRUE) {
  if (!inherits(buildings_sf, "sf")) {
    stop("buildings_sf must be an sf object.", call. = FALSE)
  }

  if (!"geometry" %in% names(buildings_sf)) {
    stop("buildings_sf must contain a geometry column.", call. = FALSE)
  }

  geom_types <- as.character(sf::st_geometry_type(buildings_sf))
  if (!all(geom_types %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("buildings_sf must contain only POLYGON or MULTIPOLYGON geometries.", call. = FALSE)
  }

  if (is.na(sf::st_crs(buildings_sf))) {
    warning("The input object has an undefined CRS. Units are assumed to be the source CRS units.", call. = FALSE)
  }

  if (make_valid) {
    buildings_sf <- sf::st_make_valid(buildings_sf)
  }

  if (use_parallel) {
    if (is.null(n_cores)) {
      n_cores <- max(1, parallel::detectCores() - 1)
    }
    future::plan(future::multisession, workers = n_cores)
    if (verbose) message("Using parallel processing with ", n_cores, " workers")
  }

  area <- as.numeric(sf::st_area(buildings_sf))
  perimeter <- as.numeric(sf::st_perimeter(buildings_sf))
  centroid_xy <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(buildings_sf)))

  # compactness, perimeter area ratio and shape index
  compactness <- (4 * pi * area) / (perimeter^2)
  perimeter_area_ratio <- perimeter / sqrt(area)
  shape_index <- perimeter / (2 * sqrt(pi * area))

  bbox_features <- list()
  if (use_parallel) {
    progressr::with_progress({
      p <- progressr::progressor(steps = nrow(buildings_sf))
      bbox_features <- future.apply::future_lapply(seq_len(nrow(buildings_sf)), function(i) {
        p()
        geom <- buildings_sf$geometry[i]
        bbox <- sf::st_bbox(geom)
        width <- bbox["xmax"] - bbox["xmin"]
        height <- bbox["ymax"] - bbox["ymin"]
        area_bbox <- width * height

        data.frame(
          bbox_width = as.numeric(width),
          bbox_height = as.numeric(height),
          bbox_area = as.numeric(area_bbox),
          elongation = as.numeric(max(width, height) / min(width, height)),
          rectangularity = as.numeric(sf::st_area(geom) / area_bbox),
          stringsAsFactors = FALSE
        )
      }, future.seed = TRUE)
    })
  } else {
    bbox_features <- lapply(seq_len(nrow(buildings_sf)), function(i) {
      geom <- buildings_sf$geometry[i]
      bbox <- sf::st_bbox(geom)
      width <- bbox["xmax"] - bbox["xmin"]
      height <- bbox["ymax"] - bbox["ymin"]
      area_bbox <- width * height

      data.frame(
        bbox_width = as.numeric(width),
        bbox_height = as.numeric(height),
        bbox_area = as.numeric(area_bbox),
        elongation = as.numeric(max(width, height) / min(width, height)),
        rectangularity = as.numeric(sf::st_area(geom) / area_bbox),
        stringsAsFactors = FALSE
      )
    })
  }

  bbox_df <- dplyr::bind_rows(bbox_features)

  # topology descriptors
  has_hole <- logical(nrow(buildings_sf))
  num_holes <- integer(nrow(buildings_sf))
  num_vertices <- integer(nrow(buildings_sf))
  num_parts <- integer(nrow(buildings_sf))
  vertices_per_area <- numeric(nrow(buildings_sf))

  for (i in seq_len(nrow(buildings_sf))) {
    geom <- buildings_sf$geometry[i]
    geom_class <- class(geom)[2]

    if (geom_class == "MULTIPOLYGON") {
      geom_parts <- geom
      num_parts[i] <- length(geom_parts)
      num_holes[i] <- sum(vapply(geom_parts, function(poly) length(poly) - 1L, integer(1)))
    } else {
      geom_parts <- geom
      num_parts[i] <- 1L
      num_holes[i] <- length(geom_parts) - 1L
    }

    has_hole[i] <- num_holes[i] > 0L

    # count vertices via the nested coordinate matrices
    if (geom_class == "MULTIPOLYGON") {
      vertex_count <- 0L
      for (p in seq_along(geom)) {
        rings <- geom[[p]]
        for (r in seq_along(rings)) {
          vertex_count <- vertex_count + nrow(rings[[r]])
        }
      }
    } else {
      vertex_count <- 0L
      rings <- geom
      for (r in seq_along(rings)) {
        vertex_count <- vertex_count + nrow(rings[[r]])
      }
    }

    num_vertices[i] <- vertex_count
    vertices_per_area[i] <- vertex_count / sqrt(area[i])
  }

  # convexity descriptors
  convexity_features <- list()
  if (use_parallel) {
    progressr::with_progress({
      p <- progressr::progressor(steps = nrow(buildings_sf))
      convexity_features <- future.apply::future_lapply(seq_len(nrow(buildings_sf)), function(i) {
        p()
        geom <- buildings_sf$geometry[i]
        area_i <- as.numeric(sf::st_area(geom))
        hull <- sf::st_convex_hull(geom)
        hull_area <- as.numeric(sf::st_area(hull))
        hull_perimeter <- as.numeric(sf::st_perimeter(hull))

        data.frame(
          convex_area_m2 = hull_area,
          convexity = area_i / hull_area,
          equiv_radius_m = sqrt(area_i / pi),
          convex_perimeter_m = hull_perimeter,
          stringsAsFactors = FALSE
        )
      }, future.seed = TRUE)
    })
  } else {
    convexity_features <- lapply(seq_len(nrow(buildings_sf)), function(i) {
      geom <- buildings_sf$geometry[i]
      area_i <- as.numeric(sf::st_area(geom))
      hull <- sf::st_convex_hull(geom)
      hull_area <- as.numeric(sf::st_area(hull))
      hull_perimeter <- as.numeric(sf::st_perimeter(hull))

      data.frame(
        convex_area_m2 = hull_area,
        convexity = area_i / hull_area,
        equiv_radius_m = sqrt(area_i / pi),
        convex_perimeter_m = hull_perimeter,
        stringsAsFactors = FALSE
      )
    })
  }

  convexity_df <- dplyr::bind_rows(convexity_features)

  # assemble the full ordered feature frame without depending on dplyr's global variable semantics
  feature_frame <- data.frame(
    area_m2 = area,
    perimeter_m = perimeter,
    centroid_x = centroid_xy[, 1],
    centroid_y = centroid_xy[, 2],
    compactness = compactness,
    perimeter_area_ratio = perimeter_area_ratio,
    shape_index = shape_index,
    bbox_width = bbox_df$bbox_width,
    bbox_height = bbox_df$bbox_height,
    bbox_area = bbox_df$bbox_area,
    elongation = bbox_df$elongation,
    rectangularity = bbox_df$rectangularity,
    aspect_ratio = bbox_df$bbox_height / bbox_df$bbox_width,
    has_hole = has_hole,
    num_holes = num_holes,
    num_vertices = num_vertices,
    num_parts = num_parts,
    vertices_per_area = vertices_per_area,
    convex_area_m2 = convexity_df$convex_area_m2,
    convexity = convexity_df$convexity,
    equiv_radius_m = convexity_df$equiv_radius_m,
    convex_perimeter_m = convexity_df$convex_perimeter_m,
    fractal_dim_proxy = log(perimeter) / log(area),
    geometric_complexity_score = (num_vertices / 10) * (1 / compactness) * (1 + num_holes),
    stringsAsFactors = FALSE
  )

  buildings_sf <- dplyr::bind_cols(buildings_sf, feature_frame)

  return(buildings_sf)
}
