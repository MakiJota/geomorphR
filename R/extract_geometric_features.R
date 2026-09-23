geomorphr_safe_st_perimeter <- function(geom) {
  perimeter_value <- tryCatch(
    as.numeric(sf::st_perimeter(geom)),
    error = function(e) NA_real_
  )

  if (!is.na(perimeter_value[1])) {
    return(perimeter_value)
  }

  as.numeric(sf::st_length(sf::st_boundary(geom)))
}

#' Validate source columns used by morphology metrics
#'
#' @param buildings_sf An sf object or data frame containing source columns.
#' @param height_col Character scalar naming building height.
#' @param floors_col Character scalar naming the number of floors/storeys.
#' @param orientation_col Character scalar naming building orientation in degrees.
#' @param gfa_col Character scalar naming gross floor area.
#' @param plot_area_col Character scalar naming plot/parcel area.
#' @return A data frame with one row per requested field and validation status.
#' @export
validate_morphology_columns <- function(buildings_sf,
                                        height_col = NULL,
                                        floors_col = NULL,
                                        orientation_col = NULL,
                                        gfa_col = NULL,
                                        plot_area_col = NULL) {
  if (!is.data.frame(buildings_sf)) {
    stop("buildings_sf must be an sf object or data frame.", call. = FALSE)
  }

  requested <- c(
    height_m = height_col,
    floors = floors_col,
    orientation_degrees = orientation_col,
    gfa_m2 = gfa_col,
    plot_area_m2 = plot_area_col
  )
  requested <- requested[!vapply(requested, is.null, logical(1))]

  if (!length(requested)) {
    return(data.frame(
      metric = character(), column = character(), present = logical(),
      numeric = logical(), missing_values = integer(), usable = logical(),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    metric = names(requested),
    column = unname(requested),
    present = unname(requested) %in% names(buildings_sf),
    numeric = vapply(unname(requested), function(column) {
      column %in% names(buildings_sf) && is.numeric(buildings_sf[[column]])
    }, logical(1)),
    missing_values = vapply(unname(requested), function(column) {
      if (!column %in% names(buildings_sf)) return(NA_integer_)
      sum(is.na(buildings_sf[[column]]))
    }, integer(1)),
    usable = vapply(unname(requested), function(column) {
      column %in% names(buildings_sf) && is.numeric(buildings_sf[[column]]) &&
        any(!is.na(buildings_sf[[column]]))
    }, logical(1)),
    stringsAsFactors = FALSE
  )
}

geomorphr_orientation <- function(geom) {
  rectangle <- sf::st_minimum_rotated_rectangle(geom)
  coordinates <- sf::st_coordinates(rectangle)
  if (nrow(coordinates) < 2L) return(NA_real_)

  if (nrow(coordinates) > 1L && all(coordinates[1, 1:2] == coordinates[nrow(coordinates), 1:2])) {
    coordinates <- coordinates[-nrow(coordinates), , drop = FALSE]
  }
  edges <- coordinates[, 1:2, drop = FALSE]
  next_edges <- coordinates[c(2:nrow(coordinates), 1), 1:2, drop = FALSE]
  lengths <- sqrt(rowSums((next_edges - edges)^2))
  edge <- which.max(lengths)
  angle <- atan2(next_edges[edge, 2] - edges[edge, 2],
                 next_edges[edge, 1] - edges[edge, 1]) * 180 / pi
  angle %% 180
}

#' Extract geometric and morphological descriptors from polygonal building footprints
#'
#' Computes footprint descriptors and optional building morphology metrics for
#' an sf object containing POLYGON or MULTIPOLYGON geometries.
#'
#' @param buildings_sf An sf object with POLYGON or MULTIPOLYGON geometries.
#' @param verbose Logical; print progress messages when TRUE.
#' @param use_parallel Logical; optional future-based parallel computation.
#' @param n_cores Integer or NULL; number of workers used by future.apply.
#' @param make_valid Logical; call sf::st_make_valid() before extraction.
#' @param height_col Character scalar naming building height in source units.
#' @param floors_col Character scalar naming the number of floors/storeys.
#' @param orientation_col Optional character scalar naming building orientation in degrees.
#' @param gfa_col Optional character scalar naming gross floor area.
#' @param plot_area_col Optional character scalar naming plot/parcel area.
#' @return An sf object with appended geometry and morphology features.
#' @export
extract_geometric_features <- function(buildings_sf,
                                        verbose = TRUE,
                                        use_parallel = FALSE,
                                        n_cores = NULL,
                                        make_valid = TRUE,
                                        height_col = NULL,
                                        floors_col = NULL,
                                        orientation_col = NULL,
                                        gfa_col = NULL,
                                        plot_area_col = NULL) {
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

  column_report <- validate_morphology_columns(
    buildings_sf,
    height_col = height_col,
    floors_col = floors_col,
    orientation_col = orientation_col,
    gfa_col = gfa_col,
    plot_area_col = plot_area_col
  )
  invalid_columns <- column_report$metric[!column_report$usable]
  if (length(invalid_columns)) {
    stop(
      "Requested morphology columns must exist, be numeric, and contain at least one non-missing value: ",
      paste(invalid_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (use_parallel) {
    if (is.null(n_cores)) {
      n_cores <- max(1, parallel::detectCores() - 1)
    }
    future::plan(future::multisession, workers = n_cores)
    if (verbose) message("Using parallel processing with ", n_cores, " workers")
  }

  area <- as.numeric(sf::st_area(buildings_sf))
  perimeter <- geomorphr_safe_st_perimeter(buildings_sf)
  centroid_xy <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(buildings_sf)))

  # compactness, perimeter area ratio and shape index
  compactness <- (4 * pi * area) / (perimeter^2)
  perimeter_area_ratio <- perimeter / sqrt(area)
  shape_index <- perimeter / (2 * sqrt(pi * area))

  geom_list <- sf::st_geometry(buildings_sf)

  bbox_features <- list()
  if (use_parallel) {
    progressr::with_progress({
      p <- progressr::progressor(steps = nrow(buildings_sf))
      bbox_features <- future.apply::future_lapply(seq_len(nrow(buildings_sf)), function(i) {
        p()
        geom <- geom_list[[i]]
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
      geom <- geom_list[[i]]
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
    geom <- geom_list[[i]]
    geom_class <- class(geom)[2]

    if (geom_class == "MULTIPOLYGON") {
      num_parts[i] <- length(geom)
      num_holes[i] <- sum(vapply(geom, function(poly) length(poly) - 1L, integer(1)))

      vertex_count <- 0L
      for (p in seq_along(geom)) {
        rings <- geom[[p]]
        for (r in seq_along(rings)) {
          vertex_count <- vertex_count + nrow(rings[[r]])
        }
      }
    } else {
      num_parts[i] <- 1L
      num_holes[i] <- length(geom) - 1L

      vertex_count <- 0L
      rings <- geom
      for (r in seq_along(rings)) {
        vertex_count <- vertex_count + nrow(rings[[r]])
      }
    }

    has_hole[i] <- num_holes[i] > 0L
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
        geom <- geom_list[[i]]
        area_i <- as.numeric(sf::st_area(geom))
        hull <- sf::st_convex_hull(geom)
        hull_area <- as.numeric(sf::st_area(hull))
        hull_perimeter <- geomorphr_safe_st_perimeter(hull)

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
      geom <- geom_list[[i]]
      area_i <- as.numeric(sf::st_area(geom))
      hull <- sf::st_convex_hull(geom)
      hull_area <- as.numeric(sf::st_area(hull))
      hull_perimeter <- geomorphr_safe_st_perimeter(hull)

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

  feature_frame$orientation_degrees <- vapply(geom_list, geomorphr_orientation, numeric(1))
  if (!is.null(orientation_col)) {
    feature_frame$orientation_degrees <- as.numeric(buildings_sf[[orientation_col]]) %% 180
  }
  if (!is.null(height_col)) {
    height <- as.numeric(buildings_sf[[height_col]])
    if (!"height_m" %in% names(buildings_sf)) feature_frame$height_m <- height
    feature_frame$volume_m3 <- height * area
    feature_frame$height_to_width <- height / pmax(feature_frame$bbox_width, .Machine$double.eps)
  }
  if (!is.null(floors_col)) {
    floors <- as.numeric(buildings_sf[[floors_col]])
    if (!"floors" %in% names(buildings_sf)) feature_frame$floors <- floors
    if (!is.null(height_col)) feature_frame$floor_height_m <- height / floors
  }
  if (!is.null(gfa_col) || !is.null(floors_col)) {
    gfa <- if (!is.null(gfa_col)) {
      as.numeric(buildings_sf[[gfa_col]])
    } else {
      area * as.numeric(buildings_sf[[floors_col]])
    }
    if (!"gfa_m2" %in% names(buildings_sf)) feature_frame$gfa_m2 <- gfa
  }
  if (!is.null(plot_area_col)) {
    plot_area <- as.numeric(buildings_sf[[plot_area_col]])
    if (!"plot_area_m2" %in% names(buildings_sf)) feature_frame$plot_area_m2 <- plot_area
    feature_frame$coverage_ratio <- area / plot_area
    if (!is.null(gfa_col) || !is.null(floors_col)) {
      feature_frame$fsi <- gfa / plot_area
    }
  }

  buildings_sf <- dplyr::bind_cols(buildings_sf, feature_frame)

  return(buildings_sf)
}
