geomorphr_compare_read <- function(path, layer, argument) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop(argument, " must be a single non-empty file path.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop(argument, " does not exist: ", path, call. = FALSE)
  }
  if (!is.null(layer) &&
      (!is.character(layer) || length(layer) != 1L || is.na(layer) || !nzchar(layer))) {
    stop(argument, " layer must be NULL or a single non-empty layer name.", call. = FALSE)
  }

  tryCatch(
    if (is.null(layer)) {
      sf::st_read(path, quiet = TRUE)
    } else {
      sf::st_read(path, layer = layer, quiet = TRUE)
    },
    error = function(error) {
      layer_text <- if (is.null(layer)) "" else paste0(" (layer '", layer, "')")
      stop(
        "Could not read ", argument, layer_text, " from '", path, "': ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )
}

geomorphr_compare_validate <- function(x, argument, make_valid) {
  if (!inherits(x, "sf")) {
    stop(argument, " must contain an sf layer.", call. = FALSE)
  }
  if (nrow(x) == 0L) {
    stop(argument, " must contain at least one feature.", call. = FALSE)
  }

  geometry <- sf::st_geometry(x)
  if (any(sf::st_is_empty(geometry))) {
    stop(argument, " must not contain empty geometries.", call. = FALSE)
  }

  geometry_types <- as.character(sf::st_geometry_type(x))
  if (!all(geometry_types %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop(
      argument, " must contain only POLYGON or MULTIPOLYGON geometries.",
      call. = FALSE
    )
  }

  validity <- sf::st_is_valid(x)
  invalid <- is.na(validity) | !validity
  if (any(invalid)) {
    if (!make_valid) {
      stop(
        argument, " contains invalid geometries; set make_valid = TRUE to repair them.",
        call. = FALSE
      )
    }
    n_before_repair <- nrow(x)
    row_key <- make.unique(c(names(x), ".geomorphr_row"))[ncol(x) + 1L]
    x[[row_key]] <- seq_len(nrow(x))
    x <- sf::st_make_valid(x)
    if (any(sf::st_is_empty(x))) {
      stop(
        "Repairing invalid geometries in ", argument, " produced empty geometries.",
        call. = FALSE
      )
    }
    geometry_types <- as.character(sf::st_geometry_type(x))
    if (!all(geometry_types %in% c("POLYGON", "MULTIPOLYGON"))) {
      x <- sf::st_collection_extract(x, "POLYGON", warn = FALSE)
      geometry_types <- as.character(sf::st_geometry_type(x))
      non_polygon <- !geometry_types %in% c("POLYGON", "MULTIPOLYGON")
      if (any(non_polygon)) x <- x[!non_polygon, ]
      if (nrow(x) < n_before_repair) {
        warning(
          n_before_repair - nrow(x), " feature(s) in ", argument,
          " had no polygonal geometry after repair and were dropped.",
          call. = FALSE
        )
      }
    }
    x <- x[order(x[[row_key]]), ]
    x[[row_key]] <- NULL
  }

  x
}

geomorphr_compare_clip <- function(x, boundary, argument) {
  boundary_geometry <- sf::st_union(sf::st_geometry(boundary))
  inside <- lengths(sf::st_intersects(x, boundary_geometry)) > 0L
  x <- x[inside, ]
  if (nrow(x) == 0L) {
    warning(argument, " has no features inside the boundary.", call. = FALSE)
    return(x)
  }
  clipped_geometry <- sf::st_intersection(sf::st_geometry(x), boundary_geometry)
  sf::st_geometry(x) <- clipped_geometry
  clipped <- x
  clipped
}

geomorphr_compare_pair <- function(target, candidates, target_index, candidate_indices,
                                   target_years, candidate_years,
                                   year_change_threshold) {
  if (!length(candidate_indices)) {
    return(list(
      max_iou = 0,
      overlap_pct = 0,
      year_changed = FALSE
    ))
  }

  target_geometry <- sf::st_geometry(target)[target_index]
  target_area <- as.numeric(sf::st_area(target_geometry))
  best_iou <- 0
  best_overlap <- 0
  best_candidate <- NA_integer_

  for (candidate_index in candidate_indices) {
    candidate_geometry <- sf::st_geometry(candidates)[candidate_index]
    candidate_area <- as.numeric(sf::st_area(candidate_geometry))
    intersection <- sf::st_intersection(target_geometry, candidate_geometry)
    intersection_area <- if (length(intersection)) {
      sum(as.numeric(sf::st_area(intersection)))
    } else {
      0
    }
    union_area <- target_area + candidate_area - intersection_area
    iou <- if (union_area > 0) intersection_area / union_area else 0

    if (iou > best_iou || is.na(best_candidate)) {
      best_iou <- min(max(iou, 0), 1)
      best_overlap <- if (target_area > 0) {
        min(max(intersection_area / target_area * 100, 0), 100)
      } else {
        0
      }
      best_candidate <- candidate_index
    }
  }

  year_changed <- FALSE
  if (!is.null(target_years) && !is.null(candidate_years)) {
    target_year <- target_years[target_index]
    candidate_year <- candidate_years[best_candidate]
    if (!is.na(target_year) && !is.na(candidate_year)) {
      year_changed <- abs(target_year - candidate_year) > year_change_threshold
    }
  }

  list(max_iou = best_iou, overlap_pct = best_overlap, year_changed = year_changed)
}

#' Compare polygon footprints across two spatial files
#'
#' Reads polygon layers from two vector files and compares their footprints
#' using intersection-over-union (IoU). Inputs may be Shapefiles, GeoPackages,
#' or other formats supported by `sf::st_read()`. For a GeoPackage containing
#' multiple layers, pass the desired layer names with `layer1` and `layer2`.
#'
#' @param file1,file2 Character paths to the earlier and later spatial files.
#' @param layer1,layer2 Optional layer names to read from `file1` and `file2`.
#'   These are useful when either file is a GeoPackage with multiple layers.
#' @param boundary_path Optional path to a polygon boundary file used to clip
#'   both input layers before comparison.
#' @param boundary_layer Optional layer name when `boundary_path` contains
#'   multiple layers.
#' @param similarity_threshold Numeric IoU threshold between 0 and 1 for treating
#'   footprints as the same building. Defaults to 0.7.
#' @param overlap_demolition_threshold Numeric percentage threshold between 0 and 100
#'   below which an identified demolition is marked as a potential demolition.
#' @param year_col Optional name of a numeric construction-year column present
#'   in both layers. Defaults to `"Year_of_Co"`; year-based replacement
#'   detection is skipped if this column is absent from both layers.
#' @param year_change_threshold Numeric construction-year difference that must
#'   be exceeded to mark a building as replaced. Defaults to 2 years.
#' @param replacement_iou_threshold Numeric IoU threshold between 0 and 1 used with
#'   `year_change_threshold` to identify replacement demolitions. Defaults to
#'   0.5.
#' @param period1,period2 Optional labels for the compared periods. By default,
#'   the input file names are used.
#' @param is_last_period Logical; if `TRUE`, demolition results are not
#'   calculated for the first layer.
#' @param make_valid Logical; repair invalid input geometries before comparing.
#' @param verbose Logical; report progress and CRS transformations.
#'
#' @return A list of `sf` objects: `new_constructions`,
#'   `clean_constructions`, `demolitions`, `gdf1_enriched`, and
#'   `gdf2_enriched`. The enriched layers contain the comparison attributes;
#'   results are returned in memory and are not written to disk.
#' @export
compare_footprints <- function(
    file1,
    file2,
    layer1 = NULL,
    layer2 = NULL,
    boundary_path = NULL,
    boundary_layer = NULL,
    similarity_threshold = 0.7,
    overlap_demolition_threshold = 20,
    year_col = "Year_of_Co",
    year_change_threshold = 2,
    replacement_iou_threshold = 0.5,
    period1 = NULL,
    period2 = NULL,
    is_last_period = FALSE,
    make_valid = TRUE,
    verbose = TRUE
) {
  thresholds <- c(
    similarity_threshold = similarity_threshold,
    overlap_demolition_threshold = overlap_demolition_threshold,
    year_change_threshold = year_change_threshold,
    replacement_iou_threshold = replacement_iou_threshold
  )
  if (!is.numeric(thresholds) || anyNA(thresholds) || any(!is.finite(thresholds))) {
    stop("Comparison thresholds must be finite numeric values.", call. = FALSE)
  }
  if (similarity_threshold < 0 || similarity_threshold > 1 ||
      replacement_iou_threshold < 0 || replacement_iou_threshold > 1) {
    stop(
      "similarity_threshold and replacement_iou_threshold must be between 0 and 1.",
      call. = FALSE
    )
  }
  if (overlap_demolition_threshold < 0 || overlap_demolition_threshold > 100) {
    stop("overlap_demolition_threshold must be between 0 and 100.", call. = FALSE)
  }
  if (year_change_threshold < 0) {
    stop("year_change_threshold must be non-negative.", call. = FALSE)
  }
  if (!is.logical(is_last_period) || length(is_last_period) != 1L ||
      is.na(is_last_period) || !is.logical(make_valid) || length(make_valid) != 1L ||
      is.na(make_valid) || !is.logical(verbose) || length(verbose) != 1L ||
      is.na(verbose)) {
    stop("is_last_period, make_valid, and verbose must be single logical values.",
         call. = FALSE)
  }
  if (!is.null(year_col) &&
      (!is.character(year_col) || length(year_col) != 1L || is.na(year_col) ||
       !nzchar(year_col))) {
    stop("year_col must be NULL or a single non-empty column name.", call. = FALSE)
  }
  if (is.null(boundary_path) && !is.null(boundary_layer)) {
    stop("boundary_layer can only be supplied with boundary_path.", call. = FALSE)
  }

  earlier <- geomorphr_compare_read(file1, layer1, "file1")
  later <- geomorphr_compare_read(file2, layer2, "file2")
  earlier <- geomorphr_compare_validate(earlier, "file1", make_valid)
  later <- geomorphr_compare_validate(later, "file2", make_valid)

  earlier_crs <- sf::st_crs(earlier)
  later_crs <- sf::st_crs(later)
  if (is.na(earlier_crs) != is.na(later_crs)) {
    stop(
      "Both input layers must have a defined CRS, or both must have no CRS.",
      call. = FALSE
    )
  }
  if (is.na(earlier_crs) && is.na(later_crs)) {
    warning(
      "Both input layers have undefined CRS; coordinates are compared as-is.",
      call. = FALSE
    )
  } else if (earlier_crs != later_crs) {
    if (verbose) {
      message("Transforming file2 to the CRS of file1.")
    }
    later <- sf::st_transform(later, earlier_crs)
  }

  if (!is.null(boundary_path)) {
    boundary <- geomorphr_compare_read(boundary_path, boundary_layer, "boundary_path")
    boundary <- geomorphr_compare_validate(boundary, "boundary_path", make_valid)
    boundary_crs <- sf::st_crs(boundary)
    if (is.na(earlier_crs) || is.na(boundary_crs)) {
      stop(
        "Input layers and boundary must have defined CRSs when clipping is requested.",
        call. = FALSE
      )
    }
    if (boundary_crs != earlier_crs) {
      if (verbose) {
        message("Transforming boundary to the CRS of file1.")
      }
      boundary <- sf::st_transform(boundary, earlier_crs)
    }
    earlier <- geomorphr_compare_clip(earlier, boundary, "file1")
    later <- geomorphr_compare_clip(later, boundary, "file2")
  }

  use_years <- !is.null(year_col) &&
    year_col %in% names(earlier) && year_col %in% names(later)
  if (!is.null(year_col) && xor(year_col %in% names(earlier), year_col %in% names(later))) {
    stop(
      "'", year_col, "' must be present in both input layers or neither.",
      call. = FALSE
    )
  }
  earlier_years <- later_years <- NULL
  if (use_years) {
    if (!is.numeric(earlier[[year_col]]) || !is.numeric(later[[year_col]])) {
      stop("The year column must be numeric in both input layers.", call. = FALSE)
    }
    earlier_years <- earlier[[year_col]]
    later_years <- later[[year_col]]
  } else if (verbose && !is.null(year_col)) {
    message("Column '", year_col, "' not found; comparing geometries only.")
  }

  if (is.null(period1)) period1 <- basename(file1)
  if (is.null(period2)) period2 <- basename(file2)
  if (!is.character(period1) || length(period1) != 1L || is.na(period1) ||
      !is.character(period2) || length(period2) != 1L || is.na(period2)) {
    stop("period1 and period2 must be NULL or single character labels.", call. = FALSE)
  }

  earlier_candidates <- sf::st_intersects(earlier, later)
  later_candidates <- sf::st_intersects(later, earlier)

  if (verbose) message("Comparing ", period1, " to ", period2, ".")

  later_results <- lapply(seq_len(nrow(later)), function(i) {
    pair <- geomorphr_compare_pair(
      later, earlier, i, later_candidates[[i]],
      later_years, earlier_years, year_change_threshold
    )
    if (!length(later_candidates[[i]])) {
      return(c(pair, list(
        is_new = TRUE,
        is_clean = TRUE,
        match_type = "clean_construction"
      )))
    }
    if (pair$year_changed) {
      return(c(pair, list(
        is_new = TRUE,
        is_clean = FALSE,
        match_type = "replaced_building"
      )))
    }
    if (pair$max_iou >= similarity_threshold) {
      return(c(pair, list(
        is_new = FALSE,
        is_clean = FALSE,
        match_type = "similar_building"
      )))
    }
    c(pair, list(
      is_new = TRUE,
      is_clean = FALSE,
      match_type = "new_with_overlap"
    ))
  })

  later$is_new_construction <- vapply(later_results, `[[`, logical(1), "is_new")
  later$is_clean_construction <- vapply(later_results, `[[`, logical(1), "is_clean")
  later$overlap_with_previous_pct <- round(
    vapply(later_results, `[[`, numeric(1), "overlap_pct"), 2
  )
  later$construction_type <- vapply(later_results, `[[`, character(1), "match_type")
  later$year_construction_changed <- vapply(
    later_results, `[[`, logical(1), "year_changed"
  )
  later$period <- paste(period1, period2, sep = "-")

  earlier$period <- paste(period1, period2, sep = "-")
  if (is_last_period) {
    earlier$is_demolished <- NA
    earlier$overlap_with_next_pct <- NA_real_
    earlier$potential_demolition <- NA
    earlier$year_construction_changed <- NA
    demolitions <- earlier[0, ]
  } else {
    earlier_results <- lapply(seq_len(nrow(earlier)), function(i) {
      pair <- geomorphr_compare_pair(
        earlier, later, i, earlier_candidates[[i]],
        earlier_years, later_years, year_change_threshold
      )
      is_demolished <- if (pair$year_changed &&
                           pair$max_iou >= replacement_iou_threshold) {
        TRUE
      } else {
        pair$max_iou < similarity_threshold
      }
      c(pair, list(is_demolished = is_demolished))
    })
    earlier$is_demolished <- vapply(
      earlier_results, `[[`, logical(1), "is_demolished"
    )
    earlier$overlap_with_next_pct <- round(
      vapply(earlier_results, `[[`, numeric(1), "overlap_pct"), 2
    )
    earlier$potential_demolition <- earlier$overlap_with_next_pct <
      overlap_demolition_threshold & earlier$is_demolished
    earlier$year_construction_changed <- vapply(
      earlier_results, `[[`, logical(1), "year_changed"
    )
    demolitions <- earlier[earlier$is_demolished, ]
  }

  list(
    new_constructions = later[later$is_new_construction, ],
    clean_constructions = later[later$is_new_construction &
                                   later$is_clean_construction, ],
    demolitions = demolitions,
    gdf1_enriched = earlier,
    gdf2_enriched = later
  )
}
