
# Load required libraries
library(sf)
library(dplyr)
library(purrr)
library(tidyr)
library(future.apply)  # For parallel processing
library(progressr)     # For progress bars

################################################################################
# SAFE CONFIGURATION - ADJUST THESE FOR YOUR SYSTEM
################################################################################

SAFE_CONFIG <- list(
  # Memory management
  max_memory_mb = 4000,        # Stop processing if memory exceeds this
  chunk_size = 5000,           # Process buildings in chunks of this size
  
  # Parallel processing (set to FALSE for safest operation)
  use_parallel = FALSE,        # Change to FALSE for sequential processing
  max_cores = 4,              # Max cores even if more available
  
  # Checkpointing (saves progress to recover from crashes)
  use_checkpoints = TRUE,      # Enable checkpoint saves
  checkpoint_folder = "02_outputs/checkpoints",
  
  # Memory cleanup
  gc_interval = 100,           # Run garbage collection every N buildings
  verbose = TRUE               # Print detailed progress messages
)

################################################################################
# UTILITY FUNCTIONS FOR SAFE PROCESSING
################################################################################

get_memory_usage <- function() {
  #' Get current memory usage in MB
  gc_result <- gc(verbose = FALSE)
  total_mb <- sum(gc_result[, 6])
  return(total_mb)
}

check_memory <- function(max_memory_mb = SAFE_CONFIG$max_memory_mb) {
  #' Check if memory usage exceeds threshold
  current_mb <- get_memory_usage()
  if (current_mb > max_memory_mb) {
    cat("WARNING: Memory usage (", round(current_mb), "MB) exceeds threshold (", 
        max_memory_mb, "MB)\n", sep = "")
    return(FALSE)
  }
  return(TRUE)
}

cleanup_memory <- function(force = FALSE) {
  #' Aggressively clean up memory
  if (force) {
    cat("  Forcing aggressive memory cleanup...\n")
    rm(list = ls(envir = globalenv())[!ls(envir = globalenv()) %in% 
                                      c("SAFE_CONFIG", "load_municipality_boundary", 
                                        "load_building_shapefile", "fix_shapefile_field_names",
                                        "calculate_iou", "calculate_overlap_percentage",
                                        "compare_timeperiods_enhanced_SAFE",
                                        "compare_timeperiods_chunked",
                                        "compare_two_periods_SAFE",
                                        "get_memory_usage", "check_memory", "cleanup_memory",
                                        "save_checkpoint", "load_checkpoint", "has_checkpoint")],
       envir = globalenv())
  }
  gc(verbose = FALSE)
  if (SAFE_CONFIG$verbose) {
    cat("  Memory after cleanup:", round(get_memory_usage()), "MB\n")
  }
}

save_checkpoint <- function(result, period_name) {
  #' Save processing result to checkpoint file for recovery
  if (!SAFE_CONFIG$use_checkpoints) return(invisible(NULL))
  
  checkpoint_file <- file.path(
    SAFE_CONFIG$checkpoint_folder,
    paste0(period_name, "_checkpoint.rds")
  )
  dir.create(SAFE_CONFIG$checkpoint_folder, recursive = TRUE, showWarnings = FALSE)
  
  tryCatch({
    saveRDS(result, checkpoint_file)
    cat("  [CHECKPOINT] Saved:", period_name, "\n")
  }, error = function(e) {
    cat("  [CHECKPOINT ERROR]:", conditionMessage(e), "\n")
  })
}

load_checkpoint <- function(period_name) {
  #' Load a previous checkpoint if available
  if (!SAFE_CONFIG$use_checkpoints) return(NULL)
  
  checkpoint_file <- file.path(
    SAFE_CONFIG$checkpoint_folder,
    paste0(period_name, "_checkpoint.rds")
  )
  
  if (file.exists(checkpoint_file)) {
    tryCatch({
      result <- readRDS(checkpoint_file)
      cat("  [CHECKPOINT RESTORED] Loaded:", period_name, "\n")
      return(result)
    }, error = function(e) {
      cat("  [CHECKPOINT ERROR] Could not load:", conditionMessage(e), "\n")
      return(NULL)
    })
  }
  
  return(NULL)
}

has_checkpoint <- function(period_name) {
  #' Check if a checkpoint exists
  if (!SAFE_CONFIG$use_checkpoints) return(FALSE)
  
  checkpoint_file <- file.path(
    SAFE_CONFIG$checkpoint_folder,
    paste0(period_name, "_checkpoint.rds")
  )
  return(file.exists(checkpoint_file))
}

################################################################################
# SPATIAL ANALYSIS FUNCTIONS (from original script)
################################################################################

calculate_iou <- function(geom1, geom2) {
  #' Calculate Intersection over Union (IoU) for two geometries
  tryCatch({
    intersection_area <- st_area(st_intersection(geom1, geom2))
    union_area <- st_area(st_union(geom1, geom2))
    
    if (as.numeric(union_area) == 0) return(0)
    iou <- as.numeric(intersection_area) / as.numeric(union_area)
    return(min(max(iou, 0), 1))  # Clamp to [0, 1]
  }, error = function(e) {
    return(0)
  })
}

calculate_overlap_percentage <- function(geom1, geom2) {
  #' Calculate overlap as percentage of geom1's area
  tryCatch({
    intersection_area <- st_area(st_intersection(geom1, geom2))
    area1 <- st_area(geom1)
    
    if (as.numeric(area1) == 0) return(0)
    overlap_pct <- (as.numeric(intersection_area) / as.numeric(area1)) * 100
    return(min(max(overlap_pct, 0), 100))  # Clamp to [0, 100]
  }, error = function(e) {
    return(0)
  })
}

fix_shapefile_field_names <- function(sf_object) {
  #' Truncate field names to 10 characters for shapefile compatibility
  names_original <- names(sf_object)
  names_shortened <- substr(names_original, 1, 10)
  
  # Handle duplicates
  names_shortened <- make.unique(names_shortened, sep = "")
  
  names(sf_object) <- names_shortened
  return(sf_object)
}

load_municipality_boundary <- function(boundary_path, verbose = FALSE) {
  #' Load municipality boundary for clipping
  if (!file.exists(boundary_path)) {
    stop("Boundary file not found:", boundary_path)
  }
  
  if (verbose) {
    cat("Reading layer from:", boundary_path, "\n")
  }
  
  boundary <- st_read(boundary_path, quiet = !verbose)
  return(boundary)
}

load_building_shapefile <- function(input_folder, year, boundary = NULL, verbose = FALSE) {
  #' Load and optionally clip building shapefile
  filename <- file.path(input_folder, paste0("B", year, ".shp"))
  
  if (!file.exists(filename)) {
    stop("Shapefile not found:", filename)
  }
  
  if (verbose) {
    cat("  Loaded", year, ":")
  }
  
  gdf <- tryCatch({
    st_read(filename, quiet = !verbose)
  }, error = function(e) {
    stop("Error reading", filename, ":", conditionMessage(e))
  })
  
  # Count initial buildings
  n_initial <- nrow(gdf)
  if (verbose) cat(n_initial, "buildings\n")
  
  # Fix invalid geometries
  if (any(!st_is_valid(gdf))) {
    if (verbose) cat(" \n (fixing", sum(!st_is_valid(gdf)), "invalid geometries...)")
    gdf <- st_make_valid(gdf)
  }
  
  # Reproject if needed
  if (st_crs(gdf) != st_crs(boundary)) {
    if (verbose) cat(" (reprojecting...)")
    gdf <- st_transform(gdf, st_crs(boundary))
  }
  
  # Clip to boundary
  if (!is.null(boundary)) {
    if (verbose) cat(" (clipping...)")
    gdf <- st_intersection(gdf, boundary)
  }
  
  n_final <- nrow(gdf)
  if (verbose) cat(" -> ", n_final, "buildings within boundary\n", sep = "")
  
  return(gdf)
}

################################################################################
# CHUNKED PROCESSING FOR LARGE DATASETS
################################################################################

has_year_field <- function(sf_obj) {
  #' Check if sf object has Year_of_Co (construction year) field
  return("Year_of_Co" %in% names(sf_obj))
}

compare_timeperiods_chunked <- function(
    gdf1, gdf2, year1, year2,
    similarity_threshold = 0.7,
    overlap_demolition_threshold = 20,
    chunk_size = SAFE_CONFIG$chunk_size,
    use_parallel = SAFE_CONFIG$use_parallel,
    is_first_period = FALSE,
    is_last_period = FALSE,
    verbose = SAFE_CONFIG$verbose
) {
  #' Compare buildings between two periods using chunked processing
  #' This is much safer for large datasets as it processes in manageable chunks
  
  if (verbose) {
    cat("Comparing", year1, "vs", year2, "using chunked processing\n")
    cat("  Chunk size:", chunk_size, "buildings\n")
    
    # Check if Year_of_Co field is available for enhanced detection
    if (has_year_field(gdf1) && has_year_field(gdf2)) {
      cat("  ✓ Using Year_of_Co (construction year) for enhanced detection\n")
      cat("    Buildings with Year_of_Co changing >2 years = REPLACEMENT (demolition + new construction)\n")
    } else {
      cat("  ⓘ Year_of_Co field not found - using geometry-only comparison\n")
    }
  }
  
  # Pre-compute spatial indices once
  intersects_new <- st_intersects(gdf2, gdf1)
  intersects_demol <- st_intersects(gdf1, gdf2)
  
  # ========== NEW CONSTRUCTIONS ==========
  cat("  Analyzing", year2, "buildings for new constructions...\n")
  
  analyze_new_building <- function(i) {
    target_geom <- gdf2$geometry[i]
    candidates_idx <- intersects_new[[i]]
    target_year <- gdf2$Year_of_Co[i]
    
    if (length(candidates_idx) == 0) {
      return(list(is_new = TRUE, is_clean = TRUE, max_overlap_pct = 0, 
                 match_type = "clean_construction", year_changed = FALSE))
    }
    
    max_iou <- 0
    max_overlap <- 0
    best_candidate_year <- NA
    best_candidate_idx <- NA
    
    for (j in candidates_idx) {
      iou <- calculate_iou(target_geom, gdf1$geometry[j])
      if (iou > max_iou) {
        max_iou <- iou
        max_overlap <- calculate_overlap_percentage(target_geom, gdf1$geometry[j])
        best_candidate_year <- gdf1$Year_of_Co[j]
        best_candidate_idx <- j
      }
    }
    
    # Check if Year_of_Co changed significantly 
    # This indicates building replacement, not just modification
    year_changed <- FALSE
    if (!is.na(target_year) && !is.na(best_candidate_year)) {
      year_diff <- abs(target_year - best_candidate_year)
      if (year_diff > 0) {  # 
        year_changed <- TRUE
      }
      if (year_diff < 0) {  # 
        print("maquina de timepo!?!!?!")
      }
    }
    
    # If Year_of_Co changed significantly, treat as new even if IoU is high
    if (year_changed) {
      list(is_new = TRUE, is_clean = FALSE, max_overlap_pct = max_overlap,
           match_type = "replaced_building", year_changed = TRUE)
    } else if (max_iou >= similarity_threshold) {
      # Same construction year + high IoU = same building, minor modifications
      list(is_new = FALSE, is_clean = FALSE, max_overlap_pct = max_overlap,
           match_type = "similar_building", year_changed = FALSE)
    } else {
      # Different building
      list(is_new = TRUE, is_clean = FALSE, max_overlap_pct = max_overlap,
           match_type = "new_with_overlap", year_changed = FALSE)
    }
  }
  
  # Process in chunks
  n_buildings <- nrow(gdf2)
  chunks <- seq(1, n_buildings, by = chunk_size)
  new_results <- vector("list", n_buildings)
  
  for (chunk_start in chunks) {
    chunk_end <- min(chunk_start + chunk_size - 1, n_buildings)
    chunk_indices <- chunk_start:chunk_end
    
    if (verbose) {
      cat("    Processing chunk:", chunk_start, "-", chunk_end, "/", n_buildings, "\n")
    }
    
    if (use_parallel && chunk_size > 1000) {
      # Use parallel for large chunks only
      with_progress({
        p <- progressor(along = chunk_indices)
        chunk_results <- future_lapply(chunk_indices, function(i) {
          p()
          analyze_new_building(i)
        }, future.seed = TRUE)
      })
      new_results[chunk_indices] <- chunk_results
    } else {
      # Sequential processing
      for (i in chunk_indices) {
        if ((i - chunk_start) %% 100 == 0 && verbose) {
          cat("      Progress:", i, "/", n_buildings, "\r")
        }
        new_results[[i]] <- analyze_new_building(i)
      }
    }
    
    # Memory check and cleanup between chunks
    if (!check_memory()) {
      cat("    WARNING: Memory limit approaching! Cleaning up...\n")
      cleanup_memory(force = TRUE)
    }
    
    if (verbose && (chunk_end %% SAFE_CONFIG$gc_interval == 0)) {
      gc(verbose = FALSE)
    }
  }
  cat("\n")
  
  # Extract results
  is_new <- sapply(new_results, function(x) x$is_new)
  is_clean <- sapply(new_results, function(x) x$is_clean)
  max_overlap_previous <- sapply(new_results, function(x) x$max_overlap_pct)
  construction_type <- sapply(new_results, function(x) x$match_type)
  year_changed_new <- sapply(new_results, function(x) x$year_changed)
  
  gdf2_enriched <- gdf2
  gdf2_enriched$is_new_construction <- is_new
  gdf2_enriched$is_clean_construction <- is_clean
  gdf2_enriched$overlap_with_previous_pct <- round(max_overlap_previous, 2)
  gdf2_enriched$construction_type <- construction_type
  gdf2_enriched$year_construction_changed <- year_changed_new
  gdf2_enriched$period <- paste0(year1, "-", year2)
  
  new_constructions <- gdf2_enriched[is_new, ]
  clean_constructions <- gdf2_enriched[is_new & is_clean, ]
  
  cat("  Found", sum(is_new), "new constructions (", sum(is_clean), "clean)\n")
  
  # ========== DEMOLITIONS ==========
  gdf1_enriched <- gdf1
  gdf1_enriched$period <- paste0(year1, "-", year2)
  
  if (!is_last_period) {
    cat("  Analyzing", year1, "buildings for demolitions...\n")
    
    analyze_demolition <- function(i) {
      target_geom <- gdf1$geometry[i]
      candidates_idx <- intersects_demol[[i]]
      target_year <- gdf1$Year_of_Co[i]
      
      if (length(candidates_idx) == 0) {
        return(list(is_demolished = TRUE, overlap_with_next_pct = 0, year_changed = FALSE))
      }
      
      max_iou <- 0
      max_overlap <- 0
      best_candidate_year <- NA
      
      for (j in candidates_idx) {
        iou <- calculate_iou(target_geom, gdf2$geometry[j])
        if (iou > max_iou) {
          max_iou <- iou
          max_overlap <- calculate_overlap_percentage(target_geom, gdf2$geometry[j])
          best_candidate_year <- gdf2$Year_of_Co[j]
        }
      }
      
      # Check if Year_of_Co changed significantly
      year_changed <- FALSE
      if (!is.na(target_year) && !is.na(best_candidate_year)) {
        year_diff <- abs(target_year - best_candidate_year)
        if (year_diff > 2) {  # More than 2 years difference = likely replacement
          year_changed <- TRUE
        }
      }
      
      # If Year_of_Co changed significantly, mark as demolished even if IoU high
      if (year_changed && max_iou >= 0.5) {  # Some overlap + year change = replacement
        list(is_demolished = TRUE, overlap_with_next_pct = max_overlap, year_changed = TRUE)
      } else if (max_iou >= similarity_threshold) {
        # Same year + high IoU = not demolished
        list(is_demolished = FALSE, overlap_with_next_pct = max_overlap, year_changed = FALSE)
      } else {
        # Low IoU = demolished
        list(is_demolished = TRUE, overlap_with_next_pct = max_overlap, year_changed = FALSE)
      }
    }
    
    # Process demolitions in chunks
    n_buildings <- nrow(gdf1)
    chunks <- seq(1, n_buildings, by = chunk_size)
    demol_results <- vector("list", n_buildings)
    
    for (chunk_start in chunks) {
      chunk_end <- min(chunk_start + chunk_size - 1, n_buildings)
      chunk_indices <- chunk_start:chunk_end
      
      if (verbose) {
        cat("    Processing chunk:", chunk_start, "-", chunk_end, "/", n_buildings, "\n")
      }
      
      if (use_parallel && chunk_size > 1000) {
        with_progress({
          p <- progressor(along = chunk_indices)
          chunk_results <- future_lapply(chunk_indices, function(i) {
            p()
            analyze_demolition(i)
          }, future.seed = TRUE)
        })
        demol_results[chunk_indices] <- chunk_results
      } else {
        for (i in chunk_indices) {
          if ((i - chunk_start) %% 100 == 0 && verbose) {
            cat("      Progress:", i, "/", n_buildings, "\r")
          }
          demol_results[[i]] <- analyze_demolition(i)
        }
      }
      
      # Memory check and cleanup
      if (!check_memory()) {
        cat("    WARNING: Memory limit approaching! Cleaning up...\n")
        cleanup_memory(force = TRUE)
      }
      
      if (verbose && (chunk_end %% SAFE_CONFIG$gc_interval == 0)) {
        gc(verbose = FALSE)
      }
    }
    cat("\n")
    
    # Extract demolition results
    is_demolished <- sapply(demol_results, function(x) x$is_demolished)
    overlap_next <- sapply(demol_results, function(x) x$overlap_with_next_pct)
    year_changed_demol <- sapply(demol_results, function(x) x$year_changed)
    
    gdf1_enriched$is_demolished <- is_demolished
    gdf1_enriched$overlap_with_next_pct <- round(overlap_next, 2)
    gdf1_enriched$potential_demolition <- (overlap_next < overlap_demolition_threshold) & is_demolished
    gdf1_enriched$year_construction_changed <- year_changed_demol
    
    demolitions <- gdf1_enriched[is_demolished, ]
    
    cat("  Found", sum(is_demolished), "demolitions\n")
  } else {
    # Last period - no demolitions calculated
    gdf1_enriched$is_demolished <- NA
    gdf1_enriched$overlap_with_next_pct <- NA
    gdf1_enriched$potential_demolition <- NA
    gdf1_enriched$year_construction_changed <- NA
    demolitions <- gdf1_enriched[numeric(0), ]
  }
  
  # Clean up spatial indices to free memory
  rm(intersects_new, intersects_demol)
  gc(verbose = FALSE)
  
  return(list(
    new_constructions = new_constructions,
    clean_constructions = clean_constructions,
    demolitions = demolitions,
    gdf1_enriched = gdf1_enriched,
    gdf2_enriched = gdf2_enriched
  ))
}

################################################################################
# MAIN COMPARISON FUNCTION
################################################################################

compare_two_periods_SAFE <- function(
    input_folder,
    year1,
    year2,
    boundary_path = "00_data/gis/gbg_muni.shp",
    similarity_threshold = 0.7,
    overlap_demolition_threshold = 20,
    output_folder = "02_outputs/new_data_new",
    save_results = TRUE,
    use_parallel = SAFE_CONFIG$use_parallel,
    n_cores = NULL,
    is_first_period = FALSE,
    is_last_period = FALSE,
    ignore_checkpoint = FALSE
) {
  #' Safely compare buildings between two time periods
  
  period_name <- paste0(year1, "_", year2)
  
  # Check for existing checkpoint (unless ignoring)
  if (!ignore_checkpoint && has_checkpoint(period_name)) {
    cat("\n### Processing Period", year1, "-", year2, "###\n")
    result <- load_checkpoint(period_name)
    if (!is.null(result)) {
      cat("  [CHECKPOINT LOADED] Skipping reprocessing\n")
      return(result)
    }
  }
  
  cat("\n", rep("=", 70), "\n", sep = "")
  cat("COMPARING:", year1, "vs", year2, "\n")
  cat(rep("=", 70), "\n", sep = "")
  cat("Use parallel:", use_parallel, "\n")
  cat("Memory limit:", SAFE_CONFIG$max_memory_mb, "MB\n")
  cat("Chunk size:", SAFE_CONFIG$chunk_size, "buildings\n\n")
  
  # Setup parallel processing if enabled
  if (use_parallel) {
    if (is.null(n_cores)) {
      n_cores <- min(SAFE_CONFIG$max_cores, max(1, parallel::detectCores() - 1))
    }
    cat("Using", n_cores, "cores for parallel processing\n\n")
    plan(multisession, workers = n_cores)
  } else {
    cat("Using sequential (non-parallel) processing\n\n")
    plan(sequential)
  }
  
  # Load data
  cat("Loading municipality boundary...\n")
  boundary <- load_municipality_boundary(boundary_path, verbose = TRUE)
  
  cat("\nLoading buildings...\n")
  gdf1 <- load_building_shapefile(input_folder, year1, boundary = boundary, verbose = TRUE)
  gdf2 <- load_building_shapefile(input_folder, year2, boundary = boundary, verbose = TRUE)
  
  # Run comparison with chunking
  cat("\n")
  result <- compare_timeperiods_chunked(
    gdf1, gdf2, year1, year2,
    similarity_threshold = similarity_threshold,
    overlap_demolition_threshold = overlap_demolition_threshold,
    chunk_size = SAFE_CONFIG$chunk_size,
    use_parallel = use_parallel,
    is_first_period = is_first_period,
    is_last_period = is_last_period,
    verbose = SAFE_CONFIG$verbose
  )
  
  # Save checkpoint
  save_checkpoint(result, period_name)
  
  # Save results to disk
  if (save_results) {
    cat("\nSaving results...\n")
    
    output_folder <- file.path(output_folder, period_name)
    dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
    
    # Save new constructions
    if (nrow(result$new_constructions) > 0) {
      filepath <- file.path(output_folder, "new_buildings.shp")
      new_to_save <- fix_shapefile_field_names(result$new_constructions)
      # Don't use st_make_valid - can convert POLYGON to LINESTRING
      # Only fix if needed
      if (!all(st_is_valid(new_to_save))) {
        new_to_save <- st_make_valid(new_to_save)
      }
      
      # Filter to only keep POLYGON geometries (avoid LINESTRING, MULTIPOLYGON issues)
      geom_types <- st_geometry_type(new_to_save)
      polygon_mask <- geom_types == "POLYGON"
      if (!all(polygon_mask)) {
        cat("  WARNING: Filtering out", sum(!polygon_mask), "non-POLYGON geometries\n")
        new_to_save <- new_to_save[polygon_mask, ]
      }
      
      if (nrow(new_to_save) > 0) {
        tryCatch({
          st_write(new_to_save, filepath, delete_dsn = TRUE, quiet = TRUE)
          cat("  ✓ Saved new constructions:", nrow(new_to_save), "buildings\n")
        }, error = function(e) {
          cat("  ✗ Error saving new constructions:", conditionMessage(e), "\n")
        })
      } else {
        cat("  ✓ No valid POLYGON geometries to save for new constructions\n")
      }
    }
    
    # Save demolitions
    if (nrow(result$demolitions) > 0) {
      filepath <- file.path(output_folder, "demolished_buildings.shp")
      demol_to_save <- fix_shapefile_field_names(result$demolitions)
      
      # Don't use st_make_valid - can convert POLYGON to LINESTRING
      # Only fix if needed
      if (!all(st_is_valid(demol_to_save))) {
        demol_to_save <- st_make_valid(demol_to_save)
      }
      
      # Filter to only keep POLYGON geometries (avoid LINESTRING, MULTIPOLYGON issues)
      geom_types <- st_geometry_type(demol_to_save)
      polygon_mask <- geom_types == "POLYGON"
      if (!all(polygon_mask)) {
        cat("  WARNING: Filtering out", sum(!polygon_mask), "non-POLYGON geometries\n")
        demol_to_save <- demol_to_save[polygon_mask, ]
      }
      
      if (nrow(demol_to_save) > 0) {
        tryCatch({
          st_write(demol_to_save, filepath, delete_dsn = TRUE, quiet = TRUE)
          cat("  ✓ Saved demolitions:", nrow(demol_to_save), "buildings\n")
        }, error = function(e) {
          cat("  ✗ Error saving demolitions:", conditionMessage(e), "\n")
        })
      } else {
        cat("  ✓ No valid POLYGON geometries to save for demolitions\n")
      }
    }
    
    # Save enriched period layers
    if (!is.null(result$gdf1_enriched) && nrow(result$gdf1_enriched) > 0) {
      filepath <- file.path(output_folder, paste0("B", year1, "_enriched.shp"))
      enriched1_to_save <- fix_shapefile_field_names(result$gdf1_enriched)
      
      if (!all(st_is_valid(enriched1_to_save))) {
        enriched1_to_save <- st_make_valid(enriched1_to_save)
      }
      
      geom_types <- st_geometry_type(enriched1_to_save)
      polygon_mask <- geom_types == "POLYGON"
      if (!all(polygon_mask)) {
        enriched1_to_save <- enriched1_to_save[polygon_mask, ]
      }
      
      if (nrow(enriched1_to_save) > 0) {
        tryCatch({
          st_write(enriched1_to_save, filepath, delete_dsn = TRUE, quiet = TRUE)
          cat("  ✓ Saved enriched layer B", year1, ":", nrow(enriched1_to_save), "buildings\n", sep = "")
        }, error = function(e) {
          cat("  ✗ Error saving enriched B", year1, ":", conditionMessage(e), "\n", sep = "")
        })
      }
    }
    
    if (!is.null(result$gdf2_enriched) && nrow(result$gdf2_enriched) > 0) {
      filepath <- file.path(output_folder, paste0("B", year2, "_enriched.shp"))
      enriched2_to_save <- fix_shapefile_field_names(result$gdf2_enriched)
      
      if (!all(st_is_valid(enriched2_to_save))) {
        enriched2_to_save <- st_make_valid(enriched2_to_save)
      }
      
      geom_types <- st_geometry_type(enriched2_to_save)
      polygon_mask <- geom_types == "POLYGON"
      if (!all(polygon_mask)) {
        enriched2_to_save <- enriched2_to_save[polygon_mask, ]
      }
      
      if (nrow(enriched2_to_save) > 0) {
        tryCatch({
          st_write(enriched2_to_save, filepath, delete_dsn = TRUE, quiet = TRUE)
          cat("  ✓ Saved enriched layer B", year2, ":", nrow(enriched2_to_save), "buildings\n", sep = "")
        }, error = function(e) {
          cat("  ✗ Error saving enriched B", year2, ":", conditionMessage(e), "\n", sep = "")
        })
      }
    }
  }
  
  # Clean up and return
  rm(gdf1, gdf2, boundary)
  cleanup_memory()
  
  return(result)
}

