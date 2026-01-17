# ============================================================
# Fixed-N tree planting functions (uniform over pedestrian area)
# ============================================================
#
# These functions use crowns sampled from the existing-crowns library,
# updates per-tile canopy rasters, and appends new-tree-points GeoJSONs.
#
# What changes vs. the percent-cover version:
#   1) Per gridcell: plant exactly n_trees (or as many as feasible) uniformly
#      over the *plantable raster cells* in that gridcell.
#   2) AOI-wide: allocate total trees across gridcells proportional to
#      *pedestrian area*, then iteratively re-allocate leftovers so we plant all
#      trees while remaining as uniform as possible over pedestrian area.
#
# Requirements / Assumptions:
# - You already have these objects/functions in scope, same as your current script:
#   aws_http, bucket, baseline_folder, scenario_folder, scenario
#   tile_grid, buffered_tile_grid  (sf polygons with tile_name)
#   load_and_merge(paths)          -> SpatRaster
#   rast_retry(path)               -> SpatRaster with retry logic
#   write_s3(obj, "bucket/key")    -> writes SpatRaster or sf to S3
# - crowns: sf with at least columns: height, tile, treeID, geometry
# - trees:  sf point layer of existing trees with geometry
# - tree_structure: list with $tree_heights and $weights for sampling heights
# - min_dist: minimum distance between planted trees (in CRS units; typically meters)
#
# Notes:
# - Planting is uniform over raster cell centers of the plantable raster.
# - AOI-wide uniformity is defined by the allocation weights (pedestrian area),
#   subject to feasibility constraints of plantable area + min_dist.

# ----------------------------
# 1) Plant N trees in one gridcell (uniform over plantable cells)
# ----------------------------
plant_in_gridcell_n <- function(grid_index,
                                     aoi_grid,
                                     n_trees,
                                     min_dist,
                                     trees,
                                     crowns,
                                     tree_structure,
                                     max_attempts_multiplier = 20L) {
  n_trees <- as.integer(n_trees)
  if (is.na(n_trees) || n_trees <= 0) {
    return(list(
      grid_index = grid_index,
      n_requested = n_trees,
      n_planted = 0L,
      pts = NULL
    ))
  }
  
  # ---- gridcell selection ----
  gridcell <- aoi_grid[aoi_grid$ID == grid_index, ]
  if (nrow(gridcell) == 0) {
    return(list(
      grid_index = grid_index,
      n_requested = n_trees,
      n_planted = 0L,
      pts = NULL
    ))
  }
  
  message("Gridcell ", grid_index, " (request n=", n_trees, ")")
  
  # Buffer gridcell by 10m (to include nearby existing trees / adjacent tile effects)
  gridcell_buffered <- sf::st_buffer(gridcell, dist = 10)
  
  # Identify intersecting tiles (buffered vs unbuffered)
  buffered_tile_names <- sf::st_intersection(gridcell_buffered, buffered_tile_grid)$tile_name
  buffered_tile_names <- unique(as.character(buffered_tile_names))
  
  unbuffered_tile_names <- sf::st_intersection(gridcell, tile_grid)$tile_name
  unbuffered_tile_names <- unique(as.character(unbuffered_tile_names))
  
  if (length(unbuffered_tile_names) == 0) {
    return(list(
      grid_index = grid_index,
      n_requested = n_trees,
      n_planted = 0L,
      pts = NULL
    ))
  }
  
  # ---- existing trees (force XY-only to satisfy RANN::nn2) ----
  existing_tree_points <- sf::st_filter(trees, gridcell_buffered)
  existing_tree_coords <- sf::st_coordinates(existing_tree_points)
  if (!is.null(existing_tree_coords) && nrow(existing_tree_coords) > 0) {
    # Keep only X/Y columns even if st_coordinates returns X/Y/Z or X/Y/L1 etc.
    existing_tree_coords <- existing_tree_coords[, c("X", "Y"), drop = FALSE]
  } else {
    existing_tree_coords <- matrix(numeric(0), ncol = 2)
    colnames(existing_tree_coords) <- c("X", "Y")
  }
  
  # ---- existing canopy height raster for gridcell ----
  canopy_paths <- glue::glue(
    "{aws_http}/{scenario_folder}/{buffered_tile_names}/raster_files/tree_canopy.tif"
  )
  canopy_height_existing <- load_and_merge(canopy_paths)
  # work on buffered gridcell so crowns can spill across tile edges
  canopy_height_gridcell <- terra::crop(canopy_height_existing, terra::vect(gridcell_buffered))
  
  # ---- plantable area raster for gridcell (placement constraint) ----
  plantable_paths <- glue::glue(
    "{aws_http}/{scenario_folder}/{unbuffered_tile_names}/ccl_layers/plantable-areas__trees__{scenario}.tif"
  )
  plantable_area <- load_and_merge(plantable_paths) |>
    terra::crop(terra::vect(gridcell)) |>
    terra::subst(from = 0, to = NA)
  
  # Candidate cells are all non-NA plantable raster cells
  plant_vals <- terra::values(plantable_area, mat = FALSE)
  avail_cells <- which(!is.na(plant_vals))
  rm(plant_vals)
  
  if (length(avail_cells) == 0) {
    message("  No plantable cells in this gridcell; skipping.")
    return(list(
      grid_index = grid_index,
      n_requested = n_trees,
      n_planted = 0L,
      pts = NULL
    ))
  }
  
  # ---- crown lookup optimization ----
  crowns_by_h <- split(crowns, crowns$height)
  
  # ---- cache rasters per crown-source tile (existing crown rasters, canopy rasters) ----
  tile_cache <- new.env(parent = emptyenv())
  
  get_tile_rasters <- function(tile) {
    key <- as.character(tile)
    if (exists(key, envir = tile_cache, inherits = FALSE)) {
      return(get(key, envir = tile_cache, inherits = FALSE))
    }
    crowns_r <- rast_retry(glue::glue(
      "{aws_http}/{baseline_folder}/{tile}/ccl_layers/existing-tree-crowns__baseline__baseline.tif"
    ))
    height_r <- rast_retry(glue::glue(
      "{aws_http}/{baseline_folder}/{tile}/raster_files/cif_tree_canopy.tif"
    ))
    val <- list(crowns_r = crowns_r, height_r = height_r)
    assign(key, val, envir = tile_cache)
    val
  }
  
  # ---- collect outputs without rbind-in-loop ----
  new_pts_list <- list()
  new_h_list <- numeric(0)
  new_type_list <- character(0)
  
  planted <- 0L
  attempts <- 0L
  max_attempts <- max(500L, as.integer(max_attempts_multiplier) * n_trees)
  
  while (planted < n_trees && length(avail_cells) > 0 && attempts < max_attempts) {
    attempts <- attempts + 1L
    
    # Uniform sample over remaining available cells (no replacement)
    j <- sample.int(length(avail_cells), 1L)
    cell <- avail_cells[[j]]
    avail_cells[[j]] <- avail_cells[[length(avail_cells)]]
    avail_cells <- avail_cells[-length(avail_cells)]
    
    # Candidate coordinate (cell center)
    coord <- terra::xyFromCell(plantable_area, cell)
    coord <- matrix(coord, nrow = 1)
    colnames(coord) <- c("X", "Y")
    
    # Min distance check against existing (and already-planted) coords
    if (nrow(existing_tree_coords) > 0) {
      d <- RANN::nn2(existing_tree_coords, query = coord, k = 1)$nn.dists[1]
      if (is.finite(d) && d <= min_dist) next
    }
    
    # Sample height class
    tree_height_class <- sample(
      tree_structure$tree_heights,
      size = 1,
      prob = tree_structure$weights
    )
    
    crowns_h <- crowns_by_h[[as.character(tree_height_class)]]
    if (is.null(crowns_h) || nrow(crowns_h) == 0) next
    
    # Sample a single reference crown geometry
    crown_geom <- crowns_h[sample.int(nrow(crowns_h), 1L), ]
    
    # Record new point
    new_pts_list[[length(new_pts_list) + 1L]] <- sf::st_point(as.numeric(coord[1, ]))
    new_h_list <- c(new_h_list, as.numeric(tree_height_class))
    new_type_list <- c(new_type_list, "new")
    
    # Update existing coords for future min_dist checks (keep XY only)
    existing_tree_coords <- rbind(existing_tree_coords, coord[, c("X", "Y"), drop = FALSE])
    
    # ---- Shift and rasterize crown into gridcell canopy height ----
    tr <- get_tile_rasters(crown_geom$tile)
    
    crown_pixels <- terra::crop(tr$crowns_r, terra::vect(crown_geom))
    crown_pixels <- crown_pixels == crown_geom$treeID
    crown_pixels <- terra::subst(crown_pixels, from = 0, to = NA)
    
    height_pixels <- terra::crop(tr$height_r, terra::vect(crown_geom))
    tree_pixels <- terra::mask(height_pixels, crown_pixels)
    
    px_cells <- which(!is.na(terra::values(tree_pixels, mat = FALSE)))
    if (length(px_cells) == 0) next
    
    px_coords <- terra::xyFromCell(tree_pixels, px_cells)
    crown_centroid <- colMeans(px_coords)
    
    pt_coords <- coord[1, ]
    dx <- pt_coords[1] - crown_centroid[1]
    dy <- pt_coords[2] - crown_centroid[2]
    
    shifted <- terra::shift(tree_pixels, dx = dx, dy = dy) |>
      terra::resample(canopy_height_gridcell, method = "near")
    
    if (terra::hasValues(shifted)) {
      canopy_height_gridcell <- max(canopy_height_gridcell, shifted, na.rm = TRUE)
    }
    
    planted <- planted + 1L
  }
  
  if (length(new_pts_list) == 0) {
    return(list(
      grid_index = grid_index,
      n_requested = n_trees,
      n_planted = 0L,
      pts = NULL
    ))
  }
  
  new_tree_pts <- sf::st_sf(
    height = new_h_list,
    type = new_type_list,
    geometry = sf::st_sfc(new_pts_list, crs = sf::st_crs(gridcell))
  )
  
  # ---- Update per-tile canopy rasters in scenario folder ----
  update_paths <- glue::glue(
    "{aws_http}/{scenario_folder}/{buffered_tile_names}/raster_files/tree_canopy.tif"
  )
  
  for (p in update_paths) {
    r <- rast_retry(p)
    
    canopy_aligned <- canopy_height_gridcell |>
      terra::extend(r) |>
      terra::crop(r)
    
    updated_canopy <- max(r, canopy_aligned, na.rm = TRUE)
    
    # Convert HTTP path to s3:// style bucket/key for your write_s3()
    updated_key <- stringr::str_replace(
      p,
      "https://wri-cities-tcm.s3.us-east-1.amazonaws.com",
      "wri-cities-tcm"
    )
    write_s3(updated_canopy, updated_key)
  }
  
  # ---- Append per-tile new-tree-points geojson ----
  for (t in buffered_tile_names) {
    tile_poly <- buffered_tile_grid |> dplyr::filter(tile_name == t)
    pts <- new_tree_pts |> sf::st_filter(tile_poly)
    if (is.null(pts) || nrow(pts) == 0) next
    
    path_http <- glue::glue(
      "{aws_http}/{scenario_folder}/{t}/ccl_layers/new-tree-points__trees__{scenario}.geojson"
    )
    existing <- tryCatch(sf::st_read(path_http, quiet = TRUE), error = function(e) NULL)
    
    combined <- if (is.null(existing) || nrow(existing) == 0) pts else rbind(existing, pts)
    
    write_s3(
      combined,
      glue::glue("{bucket}/{scenario_folder}/{t}/ccl_layers/new-tree-points__trees__{scenario}.geojson")
    )
  }
  
  list(
    grid_index = grid_index,
    n_requested = n_trees,
    n_planted = as.integer(nrow(new_tree_pts)),
    pts = new_tree_pts
  )
}

# ----------------------------
# 2) Plant TOTAL N trees uniformly over pedestrian area (AOI-wide)
#     with leftover redistribution until all trees are planted.
# ----------------------------
plant_trees_uniform_over_ped_area <- function(
    n_total,
    aoi_grid,
    min_dist,
    trees,
    crowns,
    tree_structure,
    max_rounds = 20L,
    chunk_size = 500L,
    success_bias = TRUE
) {
  stopifnot(is.numeric(n_total), length(n_total) == 1, n_total >= 0)
  n_total <- as.integer(n_total)
  if (n_total == 0) return(NULL)
  
  ids <- aoi_grid$ID
  
  # ---- Compute pedestrian-area weights per gridcell ----
  ped_sizes <- vapply(ids, function(grid_index) {
    gridcell <- aoi_grid[aoi_grid$ID == grid_index, ]
    if (nrow(gridcell) == 0) return(0)
    
    unbuffered_tile_names <- sf::st_intersection(gridcell, tile_grid)$tile_name
    unbuffered_tile_names <- unique(as.character(unbuffered_tile_names))
    if (length(unbuffered_tile_names) == 0) return(0)
    
    ped_area_paths <- glue::glue(
      "{aws_http}/{baseline_folder}/{unbuffered_tile_names}/ccl_layers/pedestrian-areas__baseline__baseline.tif"
    )
    ped_area <- load_and_merge(ped_area_paths) |>
      terra::crop(terra::vect(gridcell)) |>
      terra::subst(from = 0, to = NA)
    
    # Total area = sum(cellSize for non-NA ped cells)
    cell_area <- terra::cellSize(ped_area)
    sz <- terra::global(cell_area * ped_area, "sum", na.rm = TRUE)[1, 1]
    ifelse(is.na(sz), 0, sz)
  }, numeric(1))
  
  total_ped <- sum(ped_sizes)
  if (total_ped <= 0) stop("No pedestrian area found in AOI grid; cannot allocate trees.")
  
  ped_probs <- ped_sizes / total_ped
  
  # ---- Safe planter wrapper ----
  safe_planter <- purrr::possibly(
    ~ plant_in_gridcell_n(
      grid_index = .x$grid_index,
      aoi_grid = aoi_grid,
      n_trees = .x$n_trees,
      min_dist = min_dist,
      trees = trees,
      crowns = crowns,
      tree_structure = tree_structure
    ),
    otherwise = NULL
  )
  
  # ---- Initial ideal allocation (multinomial ensures exact total) ----
  n_by_cell <- as.integer(rmultinom(1, size = n_total, prob = ped_probs))
  
  remaining <- n_total
  all_pts <- list()
  
  # Track “eligibility” for leftovers (cells that can still accept trees)
  viable <- rep(TRUE, length(ids))
  names(viable) <- as.character(ids)
  
  # Track last-round success to bias leftovers away from failing cells
  last_planted <- rep(0L, length(ids))
  names(last_planted) <- as.character(ids)
  
  # ---- Iterative planting rounds with leftover redistribution ----
  for (round in seq_len(as.integer(max_rounds))) {
    message("Uniform ped-area planting: round ", round, "/", max_rounds,
            " (remaining=", remaining, ")")
    
    df <- tibble::tibble(grid_index = ids, n_trees = n_by_cell) |>
      dplyr::filter(.data$n_trees > 0)
    
    if (nrow(df) == 0) break
    
    res <- purrr::map(df |> split(seq_len(nrow(df))), safe_planter) |>
      purrr::compact()
    
    if (length(res) == 0) {
      warning("No results returned from planting calls; stopping.")
      break
    }
    
    planted_this_round <- 0L
    
    # Update per-cell accounting + collect points
    for (r in res) {
      gi <- as.character(r$grid_index)
      last_planted[gi] <- as.integer(r$n_planted)
      viable[gi] <- (as.integer(r$n_planted) > 0L)
      
      planted_this_round <- planted_this_round + as.integer(r$n_planted)
      
      if (!is.null(r$pts) && nrow(r$pts) > 0) {
        all_pts[[length(all_pts) + 1L]] <- r$pts
      }
    }
    
    remaining <- remaining - planted_this_round
    
    message("  Planted this round: ", planted_this_round,
            " | Remaining: ", remaining)
    
    if (remaining <= 0) {
      remaining <- 0L
      break
    }
    
    if (planted_this_round == 0L) {
      warning("No progress in this round. Remaining trees may be infeasible given constraints.")
      break
    }
    
    # ---- Redistribute leftovers ----
    # Eligible cells: those that planted >0 in the most recent round
    eligible_ids <- names(viable)[viable]
    if (length(eligible_ids) == 0) {
      warning("No eligible gridcells left to receive leftover trees.")
      break
    }
    
    elig_idx <- match(eligible_ids, as.character(ids))
    w <- ped_probs[elig_idx]
    
    # Optional success bias: prefer cells that actually planted trees recently
    # (prevents repeatedly allocating leftovers to marginal / failing cells)
    if (isTRUE(success_bias)) {
      w <- w * (1 + pmax(0L, last_planted[eligible_ids]))
    }
    
    # Normalize
    w_sum <- sum(w)
    if (!is.finite(w_sum) || w_sum <= 0) {
      warning("Eligible weight sum is zero/invalid; cannot reallocate leftovers.")
      break
    }
    w <- w / w_sum
    
    # Reallocate in chunks for smoother convergence
    add <- min(as.integer(remaining), as.integer(chunk_size))
    add_by_elig <- as.integer(rmultinom(1, size = add, prob = w))
    
    # Build next-round requests:
    # - set all to zero
    # - assign only to eligible cells for this round
    n_by_cell <- integer(length(ids))
    for (k in seq_along(eligible_ids)) {
      idk <- eligible_ids[k]
      n_by_cell[which(as.character(ids) == idk)] <- add_by_elig[k]
    }
  }
  
  if (length(all_pts) == 0) return(NULL)
  
  out <- dplyr::bind_rows(all_pts) |>
    dplyr::select(height, type)
  
  if (remaining > 0) {
    warning(
      "Planted ", (n_total - remaining), " of ", n_total,
      " trees. ", remaining, " could not be placed (constraints too strict)."
    )
  }
  
  out
}
