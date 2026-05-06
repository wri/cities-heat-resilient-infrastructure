library(terra)
library(tidyverse)
library(sf)
library(here)
library(glue)

source(here("tiling-scripts", "utils.R"))

aws_http <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com"

# ── helpers ───────────────────────────────────────────────────────────────────

# Categories 8, 9, 10 = high risk UTCI. Area-weighted percentage.
calc_binary_hot_area_pct <- function(cat_rast, area_mask_rast = NULL, area_mask_vect = NULL) {
  hot_rast <- ifel(cat_rast %in% c(8, 9, 10), 1, 0)
  px_area  <- cellSize(hot_rast)
  
  if (!is.null(area_mask_vect)) {
    hot_masked     <- mask(hot_rast, area_mask_vect)
    px_area_masked <- mask(px_area,  area_mask_vect)
  } else if (!is.null(area_mask_rast)) {
    hot_masked     <- mask(hot_rast, area_mask_rast, maskvalues = 0)
    mask_binary    <- ifel(area_mask_rast != 0, 1, NA)
    px_area_masked <- mask(px_area, mask_binary)
  } else {
    hot_masked     <- hot_rast
    px_area_masked <- px_area
  }
  
  global(px_area_masked * hot_masked, "sum", na.rm = TRUE)[1, 1] /
    global(px_area_masked,            "sum", na.rm = TRUE)[1, 1] * 100
}

mean_utci_masked <- function(utci_rast, mask_rast = NULL) {
  if (!is.null(mask_rast)) utci_rast <- mask(utci_rast, mask_rast, maskvalues = 0)
  mean(values(subst(utci_rast, 0, NA)), na.rm = TRUE)
}

shade_pct_masked <- function(shade_rast, mask_rast = NULL) {
  if (!is.null(mask_rast)) shade_rast <- mask(shade_rast, mask_rast, maskvalues = 0)
  mean(values(shade_rast), na.rm = TRUE) * 100
}

shade_dist_masked <- function(dist_rast, mask_rast = NULL) {
  if (!is.null(mask_rast)) dist_rast <- mask(dist_rast, mask_rast, maskvalues = 0)
  mean(values(dist_rast), na.rm = TRUE)
}

# Returns list(pct, max, mean) for UTCI-decreasing pixels only.
impact_stats <- function(diff_rast) {
  negative_rast <- ifel(diff_rast >= 0, NA, diff_rast)
  list(
    pct  = global(diff_rast < 0,  "mean", na.rm = TRUE)[1, 1] * 100,
    max  = global(negative_rast, "min",  na.rm = TRUE)[1, 1],
    mean = global(negative_rast, "mean", na.rm = TRUE)[1, 1]
  )
}

load_tree_achievable <- function(city, open_urban_aws_http) {
  op_path  <- glue("{open_urban_aws_http}/opportunity-layers/opportunity__stats.parquet")
  csv_path <- glue("{open_urban_aws_http}/scenarios/street-trees/{city}-street-tree-pct-1km-grid.csv")
  
  target_coverage <- NA_real_
  
  op_data <- tryCatch(st_read_parquet(op_path), error = function(...) NULL)
  if (!is.null(op_data) && "street_tree_pct_existing" %in% names(op_data)) {
    target_coverage <- quantile(op_data$street_tree_pct_existing, 0.9, names = FALSE, na.rm = TRUE) / 100
  }
  
  if (!is.finite(target_coverage)) {
    csv_data <- tryCatch(read_csv(csv_path, show_col_types = FALSE), error = function(...) NULL)
    if (!is.null(csv_data) && "pct-tree" %in% names(csv_data)) {
      target_coverage <- quantile(csv_data$`pct-tree`, 0.9, names = FALSE, na.rm = TRUE)
    }
  }
  
  if (!is.finite(target_coverage)) stop("Opportunity layers must be generated")
  target_coverage
}

load_baseline_tree_points <- function(tiles_aoi, baseline_folder, aoi) {
  map_dfr(tiles_aoi, function(t) {
    utci    <- terra::rast(glue("{aws_http}/{baseline_folder}/{t}/ccl_layers/utci-1200__baseline__baseline.tif"))
    bbox_sf <- st_as_sf(st_as_sfc(st_bbox(utci)))
    st_read(glue("{aws_http}/{baseline_folder}/{t}/ccl_layers/tree-points__baseline__baseline.geojson"),
            quiet = TRUE) |> st_filter(bbox_sf)
  }) |> st_filter(aoi)
}

achievable_albedo_rast <- function(city, baseline_albedo, buildings, baseline_folder, tiles_aoi) {
  if (str_detect(city, "USA")) {
    lulc <- load_and_merge(glue("{aws_http}/{baseline_folder}/{tiles_aoi}/raster_files/cif_open_urban.tif"))
    rcl  <- matrix(c(602, 1, 610, 1, 612, 1, 622, 1,
                     600, 2, 601, 2, 611, 2, 620, 2, 621, 2),
                   ncol = 2, byrow = TRUE)
    lulc_rc       <- classify(lulc, rcl, others = 0)
    buildings_low  <- lulc_rc == 1
    buildings_high <- lulc_rc == 2
    alb_low  <- quantile(values(mask(baseline_albedo, buildings_low)),  0.9, na.rm = TRUE)
    alb_high <- quantile(values(mask(baseline_albedo, buildings_high)), 0.9, na.rm = TRUE)
    rast <- ifel(buildings_low  & baseline_albedo < alb_low,  alb_low,  baseline_albedo) |>
      ifel(buildings_high & baseline_albedo < alb_high, alb_high, baseline_albedo)
    list(rast = rast, target = NA_real_, target_low = alb_low, target_high = alb_high)
  } else {
    alb_target <- quantile(values(mask(baseline_albedo, buildings)), 0.9, na.rm = TRUE)
    rast <- ifel(buildings & baseline_albedo < alb_target, alb_target, baseline_albedo)
    list(rast = rast, target = alb_target, target_low = NA_real_, target_high = NA_real_)
  }
}


# ── baseline ──────────────────────────────────────────────────────────────────

calc_baseline_metrics <- function(city, aoi_name, tiles_aoi) {
  
  baseline_folder <- glue("city_projects/{city}/{aoi_name}/scenarios/baseline/baseline")
  
  met_data <- read_csv(glue("{aws_http}/{baseline_folder}/metadata/met_files/met_era5_hottest_days.csv"), skip = 2)
  date <- met_data |> slice(1) |> mutate(date = glue("{Year}_{Month}_{Day}")) |> pull(date)
  
  aoi <- st_read(glue("{aws_http}/{baseline_folder}/aoi__baseline__baseline.geojson"))
  
  ped_area_rast <- load_and_merge(
    glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/pedestrian-areas__baseline__baseline.tif")
  )
  aoi <- aoi |> st_transform(st_crs(ped_area_rast))
  ped_area_rast <- ped_area_rast |> crop(aoi) |> mask(aoi)
  
  px_area         <- cellSize(ped_area_rast)
  pedestrian_area <- global(px_area * ped_area_rast, "sum", na.rm = TRUE)[1, 1]
  
  nonbuild_area_rast <- load_and_merge(
    glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/non-building-areas__baseline__baseline.tif")
  ) |> crop(aoi) |> mask(aoi)
  nonbuilding_area <- global(px_area * nonbuild_area_rast, "sum", na.rm = TRUE)[1, 1]
  
  park_area <- tryCatch({
    parks <- st_read(
      glue("{aws_http}/city_projects/{city}/{aoi_name}/scenarios/shade-structures/all-parks/parks__shade-structures__all-parks.geojson"),
      quiet = TRUE
    )
    sum(parks$area_sqm, na.rm = TRUE)
  }, error = function(...) NA_real_)
  
  # Use 1200 raster as crop template (all timestamps share the same extent)
  template_rast  <- load_and_merge(
    glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/utci-1200__baseline__baseline.tif")
  ) |> crop(aoi) |> mask(aoi)
  ped_crop       <- ped_area_rast     |> crop(template_rast)
  nonbuild_crop  <- nonbuild_area_rast |> crop(template_rast)
  
  results <- tibble()
  
  for (time in c("1200", "1500", "1800")) {
    
    baseline_shade_rast <- load_and_merge(
      glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/shade-{time}__baseline__baseline.tif")
    ) |> crop(aoi) |> mask(aoi) > 0
    
    baseline_utci_rast <- load_and_merge(
      glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/utci-{time}__baseline__baseline.tif")
    ) |> crop(aoi) |> mask(aoi)
    
    baseline_shade_dist_rast <- load_and_merge(
      glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/shade-distance-{time}__baseline__baseline.tif")
    ) |> crop(aoi) |> mask(aoi)
    
    baseline_utci_cat_rast <- load_and_merge(
      glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/utci-cat-{time}__baseline__baseline.tif")
    ) |> crop(aoi) |> mask(aoi)
    
    metrics <- tibble(
      indicators_id = c(
        glue("mean_utci_{time}_baseline_aoi"),
        glue("shade_cover_{time}_baseline_aoi"),
        glue("mean_distance_shade_cover_{time}_baseline_aoi"),
        glue("high_risk_utci_pct__{time}_baseline_aoi"),
        
        glue("mean_utci_{time}_baseline_pedestrian"),
        glue("shade_cover_{time}_baseline_pedestrian"),
        glue("mean_distance_shade_cover_{time}_baseline_pedestrian"),
        glue("high_risk_utci_pct__{time}_baseline_pedestrian"),
        
        glue("mean_utci_{time}_baseline_nonbuilding_areas"),
        glue("shade_cover_{time}_baseline_nonbuilding_areas"),
        glue("mean_distance_shade_cover_{time}_baseline_nonbuilding_areas"),
        glue("high_risk_utci_pct__{time}_baseline_nonbuilding_areas")
      ),
      value = c(
        mean_utci_masked(baseline_utci_rast),
        shade_pct_masked(baseline_shade_rast),
        shade_dist_masked(baseline_shade_dist_rast),
        calc_binary_hot_area_pct(baseline_utci_cat_rast),
        
        mean_utci_masked(baseline_utci_rast, ped_crop),
        shade_pct_masked(baseline_shade_rast, ped_crop),
        shade_dist_masked(baseline_shade_dist_rast, ped_crop),
        calc_binary_hot_area_pct(baseline_utci_cat_rast, area_mask_rast = ped_crop),
        
        mean_utci_masked(baseline_utci_rast, nonbuild_crop),
        shade_pct_masked(baseline_shade_rast, nonbuild_crop),
        shade_dist_masked(baseline_shade_dist_rast, nonbuild_crop),
        calc_binary_hot_area_pct(baseline_utci_cat_rast, area_mask_rast = nonbuild_crop)
      )
    )
    
    results <- bind_rows(results, metrics)
  }
  
  # Tree cover raster (use template_rast for crop)
  baseline_tree_rast <- load_and_merge(
    glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/tree-cover__baseline__baseline.tif")
  ) |> subst(from = NA, 0) |> crop(template_rast) |> mask(aoi)
  
  baseline_tree_pct_aoi       <- mean(values(baseline_tree_rast, na.rm = TRUE))
  baseline_tree_pct_pedestrian <- mean(values(mask(baseline_tree_rast, ped_crop, maskvalues = 0), na.rm = TRUE))
  
  baseline_tree_points <- load_baseline_tree_points(tiles_aoi, baseline_folder, aoi)
  pedestrian_vals <- terra::extract(ped_crop, vect(baseline_tree_points))
  baseline_tree_points$pedestrian <- pedestrian_vals[[2]]
  
  tree_metrics <- tibble(
    indicators_id = c(
      "tree_cover_baseline_aoi",
      "tree_cover_baseline_pedestrian",
      "tree_n_baseline_aoi",
      "tree_n_baseline_pedestrian",
      "pedestrian_area",
      "nonbuilding_area",
      "park_area"
    ),
    value = c(
      baseline_tree_pct_aoi * 100,
      baseline_tree_pct_pedestrian * 100,
      nrow(baseline_tree_points),
      nrow(baseline_tree_points |> filter(pedestrian == 1)),
      pedestrian_area,
      nonbuilding_area,
      park_area
    )
  )
  
  bind_rows(results, tree_metrics) |>
    mutate(
      application_id       = "ccl",
      cities_id            = city,
      areas_of_interest_id = str_replace(aoi_name, "-", "_"),
      interventions_id     = "baseline",
      scenarios_id         = "baseline__baseline",
      date                 = date,
      value                = round(value, 2)
    ) |>
    write_csv(here("misc", "metrics-testing", glue("{city}-{aoi_name}-metrics__baseline__baseline.csv")))
}


# ── street trees ──────────────────────────────────────────────────────────────

calc_street_tree_metrics <- function(city, aoi_name, tiles_aoi, infra, scenario) {
  
  baseline_folder     <- glue("city_projects/{city}/{aoi_name}/scenarios/baseline/baseline")
  scenario_folder     <- glue("city_projects/{city}/{aoi_name}/scenarios/{infra}/{scenario}")
  open_urban_aws_http <- glue("{aws_http}/OpenUrban/{city}")
  
  met_data <- read_csv(glue("{aws_http}/{baseline_folder}/metadata/met_files/met_era5_hottest_days.csv"), skip = 2)
  date <- met_data |> slice(1) |> mutate(date = glue("{Year}_{Month}_{Day}")) |> pull(date)
  
  # baseline_mean_air_temp_1500 comes from the baseline met file.
  # Trees do not have a reduced_temps.csv, so scenario air temp = baseline.
  baseline_mean_air_temp_1500 <- met_data |> filter(Hour == 15) |> pull(Temperature)
  # scenario_mean_air_temp_1500 <- baseline_mean_air_temp_1500
  # change_mean_air_temp_1500   <- 0
  
  aoi <- st_read(glue("{aws_http}/{baseline_folder}/aoi__baseline__baseline.geojson"))
  
  ped_area_rast <- load_and_merge(
    glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/pedestrian-areas__baseline__baseline.tif")
  )
  aoi <- aoi |> st_transform(st_crs(ped_area_rast))
  ped_area_rast <- ped_area_rast |> crop(aoi) |> mask(aoi)
  
  px_area         <- cellSize(ped_area_rast)
  pedestrian_area <- global(px_area * ped_area_rast, "sum", na.rm = TRUE)[1, 1]
  
  nonbuild_area_rast <- load_and_merge(
    glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/non-building-areas__baseline__baseline.tif")
  ) |> crop(aoi) |> mask(aoi)
  
  template_rast <- load_and_merge(
    glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/utci-1200__baseline__baseline.tif")
  ) |> crop(aoi) |> mask(aoi)
  ped_crop      <- ped_area_rast     |> crop(template_rast)
  nonbuild_crop <- nonbuild_area_rast |> crop(template_rast)
  
  results <- tibble()
  
  for (time in c("1200", "1500", "1800")) {
    
    baseline_shade_rast <- load_and_merge(
      glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/shade-{time}__baseline__baseline.tif")
    ) |> crop(aoi) |> mask(aoi) > 0
    
    scenario_shade_rast <- load_and_merge(
      glue("{aws_http}/{scenario_folder}/{tiles_aoi}/ccl_layers/shade-{time}__{infra}__{scenario}.tif")
    ) |> crop(aoi) |> mask(aoi) > 0
    
    baseline_utci_rast <- load_and_merge(
      glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/utci-{time}__baseline__baseline.tif")
    ) |> crop(aoi) |> mask(aoi)
    
    scenario_utci_rast <- load_and_merge(
      glue("{aws_http}/{scenario_folder}/{tiles_aoi}/ccl_layers/utci-{time}__{infra}__{scenario}.tif")
    ) |> crop(aoi) |> mask(aoi)
    
    baseline_shade_dist_rast <- load_and_merge(
      glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/shade-distance-{time}__baseline__baseline.tif")
    ) |> crop(aoi) |> mask(aoi)
    
    scenario_shade_dist_rast <- load_and_merge(
      glue("{aws_http}/{scenario_folder}/{tiles_aoi}/ccl_layers/shade-distance-{time}__{infra}__{scenario}.tif")
    ) |> mosaic(baseline_shade_dist_rast, fun = "first") |> crop(aoi) |> mask(aoi)
    
    utci_diff_rast <- load_and_merge(
      glue("{aws_http}/{scenario_folder}/{tiles_aoi}/ccl_layers/utci-{time}__{infra}__{scenario}__vs-baseline.tif")
    ) |> crop(aoi) |> mask(aoi)
    
    imp <- impact_stats(utci_diff_rast)
    
    baseline_utci_cat_rast <- load_and_merge(
      glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/utci-cat-{time}__baseline__baseline.tif")
    ) |> crop(aoi) |> crop(aoi) |> mask(aoi)
    
    scenario_utci_cat_rast <- load_and_merge(
      glue("{aws_http}/{scenario_folder}/{tiles_aoi}/ccl_layers/utci-cat-{time}__{infra}__{scenario}.tif")
    ) |> crop(aoi) |> mask(aoi)
    
    # AOI metrics
    mean_utci_bl_aoi  <- mean_utci_masked(baseline_utci_rast)
    mean_utci_sc_aoi  <- mean_utci_masked(scenario_utci_rast)
    
    # Pedestrian metrics
    mean_utci_bl_ped  <- mean_utci_masked(baseline_utci_rast, ped_crop)
    mean_utci_sc_ped  <- mean_utci_masked(scenario_utci_rast, ped_crop)
    
    # Non-building metrics
    mean_utci_bl_nb   <- mean_utci_masked(baseline_utci_rast, nonbuild_crop)
    mean_utci_sc_nb   <- mean_utci_masked(scenario_utci_rast, nonbuild_crop)
    
    metrics <- tibble(
      indicators_id = c(
        glue("mean_utci_{time}_baseline_aoi"),
        glue("mean_utci_{time}_scenario_aoi"),
        glue("mean_utci_{time}_change_aoi"),
        glue("shade_cover_{time}_baseline_aoi"),
        glue("shade_cover_{time}_scenario_aoi"),
        glue("shade_cover_{time}_change_aoi"),
        glue("mean_distance_shade_cover_{time}_baseline_aoi"),
        glue("mean_distance_shade_cover_{time}_scenario_aoi"),
        glue("mean_distance_shade_cover_{time}_change_aoi"),
        
        glue("mean_utci_{time}_baseline_pedestrian"),
        glue("mean_utci_{time}_scenario_pedestrian"),
        glue("mean_utci_{time}_change_pedestrian"),
        glue("shade_cover_{time}_baseline_pedestrian"),
        glue("shade_cover_{time}_scenario_pedestrian"),
        glue("shade_cover_{time}_change_pedestrian"),
        glue("mean_distance_shade_cover_{time}_baseline_pedestrian"),
        glue("mean_distance_shade_cover_{time}_scenario_pedestrian"),
        glue("mean_distance_shade_cover_{time}_change_pedestrian"),
        glue("high_risk_utci_pct__{time}_baseline_pedestrian"),
        glue("high_risk_utci_pct__{time}_scenario_pedestrian"),
        glue("high_risk_utci_pct__{time}_change_pedestrian"),
        
        glue("mean_utci_{time}_baseline_nonbuilding_areas"),
        glue("mean_utci_{time}_scenario_nonbuilding_areas"),
        glue("mean_utci_{time}_change_nonbuilding_areas"),
        glue("shade_cover_{time}_baseline_nonbuilding_areas"),
        glue("shade_cover_{time}_scenario_nonbuilding_areas"),
        glue("shade_cover_{time}_change_nonbuilding_areas"),
        glue("mean_distance_shade_cover_{time}_baseline_nonbuilding_areas"),
        glue("mean_distance_shade_cover_{time}_scenario_nonbuilding_areas"),
        glue("mean_distance_shade_cover_{time}_change_nonbuilding_areas"),
        
        glue("max_utci_{time}_change_impact_areas"),
        glue("mean_utci_{time}_change_impact_areas"),
        glue("utci_reduction_{time}_impact_area")
      ),
      value = c(
        mean_utci_bl_aoi,
        mean_utci_sc_aoi,
        mean_utci_sc_aoi - mean_utci_bl_aoi,
        shade_pct_masked(baseline_shade_rast),
        shade_pct_masked(scenario_shade_rast),
        shade_pct_masked(scenario_shade_rast) - shade_pct_masked(baseline_shade_rast),
        shade_dist_masked(baseline_shade_dist_rast),
        shade_dist_masked(scenario_shade_dist_rast),
        shade_dist_masked(scenario_shade_dist_rast) - shade_dist_masked(baseline_shade_dist_rast),
        
        mean_utci_bl_ped,
        mean_utci_sc_ped,
        mean_utci_sc_ped - mean_utci_bl_ped,
        shade_pct_masked(baseline_shade_rast, ped_crop),
        shade_pct_masked(scenario_shade_rast, ped_crop),
        shade_pct_masked(scenario_shade_rast, ped_crop) - shade_pct_masked(baseline_shade_rast, ped_crop),
        shade_dist_masked(baseline_shade_dist_rast, ped_crop),
        shade_dist_masked(scenario_shade_dist_rast, ped_crop),
        shade_dist_masked(scenario_shade_dist_rast, ped_crop) - shade_dist_masked(baseline_shade_dist_rast, ped_crop),
        calc_binary_hot_area_pct(baseline_utci_cat_rast, area_mask_rast = ped_crop),
        calc_binary_hot_area_pct(scenario_utci_cat_rast, area_mask_rast = ped_crop),
        calc_binary_hot_area_pct(scenario_utci_cat_rast, area_mask_rast = ped_crop) -
          calc_binary_hot_area_pct(baseline_utci_cat_rast, area_mask_rast = ped_crop),
        
        mean_utci_bl_nb,
        mean_utci_sc_nb,
        mean_utci_sc_nb - mean_utci_bl_nb,
        shade_pct_masked(baseline_shade_rast, nonbuild_crop),
        shade_pct_masked(scenario_shade_rast, nonbuild_crop),
        shade_pct_masked(scenario_shade_rast, nonbuild_crop) - shade_pct_masked(baseline_shade_rast, nonbuild_crop),
        shade_dist_masked(baseline_shade_dist_rast, nonbuild_crop),
        shade_dist_masked(scenario_shade_dist_rast, nonbuild_crop),
        shade_dist_masked(scenario_shade_dist_rast, nonbuild_crop) - shade_dist_masked(baseline_shade_dist_rast, nonbuild_crop),
        
        imp$max,
        imp$mean,
        imp$pct
      )
    )
    
    results <- bind_rows(results, metrics)
  }
  
  # Trees
  baseline_tree_rast <- load_and_merge(
    glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/tree-cover__baseline__baseline.tif")
  ) |> subst(from = NA, 0) |> crop(template_rast) |> mask(aoi)
  
  scenario_tree_rast <- load_and_merge(
    glue("{aws_http}/{scenario_folder}/{tiles_aoi}/ccl_layers/tree-cover__{infra}__{scenario}.tif")
  ) |> subst(from = NA, 0) |> crop(template_rast) |> mask(aoi)
  
  tree_cover_bl_aoi  <- mean(values(baseline_tree_rast, na.rm = TRUE))
  tree_cover_sc_aoi  <- mean(values(scenario_tree_rast, na.rm = TRUE))
  tree_cover_bl_ped  <- mean(values(mask(baseline_tree_rast, ped_crop, maskvalues = 0), na.rm = TRUE))
  tree_cover_sc_ped  <- mean(values(mask(scenario_tree_rast, ped_crop, maskvalues = 0), na.rm = TRUE))
  
  baseline_tree_points <- load_baseline_tree_points(tiles_aoi, baseline_folder, aoi)
  pedestrian_vals <- terra::extract(ped_crop, vect(baseline_tree_points))
  baseline_tree_points$pedestrian <- pedestrian_vals[[2]]
  tree_n_baseline_pedestrian <- nrow(baseline_tree_points |> filter(pedestrian == 1))
  
  scenario_tree_points    <- st_read(glue("{aws_http}/{scenario_folder}/new-tree-points__{infra}__{scenario}.geojson"), quiet = TRUE)
  tree_n_scenario_pedestrian <- nrow(scenario_tree_points)
  
  target_coverage            <- load_tree_achievable(city, open_urban_aws_http)
  tree_cover_achievable_ped  <- target_coverage * 100
  
  tree_metrics <- tibble(
    indicators_id = c(
      "tree_cover_baseline_aoi",
      "tree_cover_scenario_aoi",
      "tree_cover_change_aoi",
      "tree_cover_baseline_pedestrian",
      "tree_cover_scenario_pedestrian",
      "tree_cover_change_pedestrian",
      "tree_n_baseline_pedestrian",
      "tree_n_change_pedestrian",
      "tree_n_scenario_pedestrian",
      "tree_cover_achievable_pedestrian",
      "tree_cover_progress",
      "baseline_mean_air_temp_1500",
      # "scenario_mean_air_temp_1500",
      # "change_mean_air_temp_1500",
      "mean_air_temp_1500_change_impact_areas",
      "air_temp_reduction_1500_impact_area",
      "pedestrian_area"
    ),
    value = c(
      tree_cover_bl_aoi * 100,
      tree_cover_sc_aoi * 100,
      (tree_cover_sc_aoi - tree_cover_bl_aoi) * 100,
      tree_cover_bl_ped * 100,
      tree_cover_sc_ped * 100,
      (tree_cover_sc_ped - tree_cover_bl_ped) * 100,
      tree_n_baseline_pedestrian,
      tree_n_scenario_pedestrian,
      tree_n_scenario_pedestrian + tree_n_baseline_pedestrian,
      tree_cover_achievable_ped,
      (tree_cover_sc_ped - tree_cover_bl_ped) / (target_coverage - tree_cover_bl_ped) * 100,
      baseline_mean_air_temp_1500,
      # scenario_mean_air_temp_1500,
      # change_mean_air_temp_1500,
      -3.3 * (tree_cover_sc_aoi - tree_cover_bl_aoi),
      100,
      pedestrian_area
    )
  )
  
  bind_rows(results, tree_metrics) |>
    mutate(
      application_id       = "ccl",
      cities_id            = city,
      areas_of_interest_id = str_replace(aoi_name, "-", "_"),
      interventions_id     = infra,
      scenarios_id         = glue("{infra}__{scenario}"),
      date                 = date,
      value                = round(value, 2)
    ) |>
    write_csv(here("misc", "metrics-testing", glue("{city}-{aoi_name}-metrics__{infra}__{scenario}.csv")))
}


# ── cool roofs ────────────────────────────────────────────────────────────────

calc_cool_roofs_metrics <- function(city, aoi_name, tiles_aoi, infra, scenario) {
  
  baseline_folder <- glue("city_projects/{city}/{aoi_name}/scenarios/baseline/baseline")
  scenario_folder <- glue("city_projects/{city}/{aoi_name}/scenarios/{infra}/{scenario}")
  
  baseline_met_data <- read_csv(glue("{aws_http}/{baseline_folder}/metadata/met_files/met_era5_hottest_days.csv"), skip = 2)
  scenario_met_data <- read_csv(glue("{aws_http}/{scenario_folder}/metadata/met_files/reduced_temps.csv"), skip = 2)
  
  date <- baseline_met_data |> slice(1) |> mutate(date = glue("{Year}_{Month}_{Day}")) |> pull(date)
  
  baseline_albedo <- load_and_merge(
    glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/albedo__baseline__baseline.tif")
  )
  scenario_albedo <- load_and_merge(
    glue("{aws_http}/{scenario_folder}/{tiles_aoi}/ccl_layers/albedo__{infra}__{scenario}.tif")
  )
  buildings <- load_and_merge(
    glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/building-areas__baseline__baseline.tif")
  ) |> subst(0, NA)
  
  aoi <- st_read(glue("{aws_http}/{baseline_folder}/aoi__baseline__baseline.geojson")) |>
    st_transform(st_crs(baseline_albedo))
  
  baseline_albedo <- baseline_albedo |> crop(aoi) |> mask(aoi)
  scenario_albedo <- scenario_albedo |> crop(aoi) |> mask(aoi)
  buildings       <- buildings       |> crop(aoi) |> mask(aoi)
  
  ach <- achievable_albedo_rast(city, baseline_albedo, buildings, baseline_folder, tiles_aoi)
  ach_albedo      <- ach$rast        |> crop(aoi) |> mask(aoi)
  
  nonbuild_area_rast <- load_and_merge(
    glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/non-building-areas__baseline__baseline.tif")
  ) |> crop(aoi) |> mask(aoi)
  
  px_area          <- cellSize(baseline_albedo)
  nonbuild_area    <- global(px_area * nonbuild_area_rast, "sum", na.rm = TRUE)[1, 1]
  nonbuild_crop    <- nonbuild_area_rast |> crop(baseline_albedo)
  
  # Cool roof vectors
  buildings_vect <- map_df(
    glue("{aws_http}/{scenario_folder}/{tiles_aoi}/ccl_layers/buildings__{infra}__{scenario}.geojson"),
    ~ st_read(.x, quiet = TRUE)
  ) |> distinct(id, .keep_all = TRUE)
  
  cool_roofs_baseline <- buildings_vect |> filter(median_alb >= cool_roof_alb)
  cool_roofs_scenario <- buildings_vect |> filter(median_alb >= cool_roof_alb | updated)
  
  baseline_roof_alb <- mean(buildings_vect$median_alb, na.rm = TRUE)
  scenario_roof_alb <- buildings_vect |>
    mutate(updated_alb = case_when(updated ~ cool_roof_alb, !updated ~ median_alb)) |>
    summarize(mean = mean(updated_alb, na.rm = TRUE)) |>
    pull(mean)
  
  baseline_cool_roof_area  <- sum(cool_roofs_baseline$area_m2)
  scenario_cool_roof_area  <- sum(cool_roofs_scenario$area_m2)
  technical_cool_roof_area <- sum(buildings_vect$area_m2)
  
  alb_target_val      <- ach$target
  alb_target_low_val  <- ach$target_low
  alb_target_high_val <- ach$target_high
  
  baseline_reflectivity  <- mean(values(baseline_albedo), na.rm = TRUE) 
  scenario_reflectivity  <- mean(values(scenario_albedo), na.rm = TRUE) 
  achievable_reflectivity <- mean(values(ach_albedo), na.rm = TRUE) 
  
  baseline_mean_air_temp_1200 <- baseline_met_data |> filter(Hour == 12) |> pull(Temperature)
  baseline_mean_air_temp_1500 <- baseline_met_data |> filter(Hour == 15) |> pull(Temperature)
  baseline_mean_air_temp_1800 <- baseline_met_data |> filter(Hour == 18) |> pull(Temperature)
  scenario_mean_air_temp_1200 <- scenario_met_data |> filter(Hour == 12) |> pull(Temperature)
  scenario_mean_air_temp_1500 <- scenario_met_data |> filter(Hour == 15) |> pull(Temperature)
  scenario_mean_air_temp_1800 <- scenario_met_data |> filter(Hour == 18) |> pull(Temperature)
  
  scalar_metrics <- tibble(
    indicators_id = c(
      "achievable_cool_roof_reflectivity",
      "achievable_cool_roof_reflectivity_high",
      "achievable_cool_roof_reflectivity_low",
      "achievable_reflectivity",
      "air_temp_reduction_1500_impact_area",
      "baseline_cool_roof_area",
      "baseline_mean_air_temp_1200",
      "baseline_mean_air_temp_1500",
      "baseline_mean_air_temp_1800",
      "baseline_reflectivity",
      "baseline_roof_reflectivity",
      "change_cool_roof_area",
      "change_mean_air_temp_1200",
      "change_mean_air_temp_1500",
      "change_mean_air_temp_1800",
      "change_reflectivity",
      "change_roof_reflectivity",
      "progress_cool_roofs",
      "progress_reflectivity",
      "scenario_cool_roof_area",
      "scenario_mean_air_temp_1200",
      "scenario_mean_air_temp_1500",
      "scenario_mean_air_temp_1800",
      "scenario_reflectivity",
      "scenario_roof_reflectivity",
      "technical_cool_roof_area",
      "nonbuilding_area",
      "mean_air_temp_1500_change_impact_areas"
    ),
    value = c(
      alb_target_val,
      alb_target_high_val,
      alb_target_low_val,
      achievable_reflectivity,
      100,
      baseline_cool_roof_area,
      baseline_mean_air_temp_1200,
      baseline_mean_air_temp_1500,
      baseline_mean_air_temp_1800,
      baseline_reflectivity,
      baseline_roof_alb * 100,
      scenario_cool_roof_area - baseline_cool_roof_area,
      scenario_mean_air_temp_1200 - baseline_mean_air_temp_1200,
      scenario_mean_air_temp_1500 - baseline_mean_air_temp_1500,
      scenario_mean_air_temp_1800 - baseline_mean_air_temp_1800,
      scenario_reflectivity - baseline_reflectivity,
      (scenario_roof_alb - baseline_roof_alb) * 100,
      (scenario_cool_roof_area - baseline_cool_roof_area) /
        (technical_cool_roof_area - baseline_cool_roof_area) * 100,
      (scenario_reflectivity - baseline_reflectivity) /
        (achievable_reflectivity - baseline_reflectivity) * 100,
      scenario_cool_roof_area,
      scenario_mean_air_temp_1200,
      scenario_mean_air_temp_1500,
      scenario_mean_air_temp_1800,
      scenario_reflectivity,
      scenario_roof_alb * 100,
      technical_cool_roof_area,
      nonbuild_area,
      scenario_mean_air_temp_1500 - baseline_mean_air_temp_1500
    )
  )
  
  utci_metrics <- tibble()
  
  for (time in c("1200", "1500", "1800")) {
    
    baseline_utci_rast <- load_and_merge(
      glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/utci-{time}__baseline__baseline.tif")
    ) |> crop(aoi) |> mask(aoi)
    
    scenario_utci_rast <- load_and_merge(
      glue("{aws_http}/{scenario_folder}/{tiles_aoi}/ccl_layers/utci-{time}__{infra}__{scenario}.tif")
    ) |> crop(aoi) |> mask(aoi)
    
    utci_diff_rast <- load_and_merge(
      glue("{aws_http}/{scenario_folder}/{tiles_aoi}/ccl_layers/utci-{time}__{infra}__{scenario}__vs-baseline.tif")
    ) |> crop(aoi) |> mask(aoi)
    
    baseline_utci_cat_rast <- load_and_merge(
      glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/utci-cat-{time}__baseline__baseline.tif")
    ) |> crop(aoi) |> mask(aoi)
    
    scenario_utci_cat_rast <- load_and_merge(
      glue("{aws_http}/{scenario_folder}/{tiles_aoi}/ccl_layers/utci-cat-{time}__{infra}__{scenario}.tif")
    ) |> crop(aoi) |> mask(aoi)
    
    imp <- impact_stats(utci_diff_rast)
    
    bl_nb <- mean_utci_masked(baseline_utci_rast, nonbuild_crop)
    sc_nb <- mean_utci_masked(scenario_utci_rast, nonbuild_crop)
    
    bl_hr_nb <- calc_binary_hot_area_pct(baseline_utci_cat_rast, area_mask_rast = nonbuild_crop)
    sc_hr_nb <- calc_binary_hot_area_pct(scenario_utci_cat_rast, area_mask_rast = nonbuild_crop)
    
    metrics <- tibble(
      indicators_id = c(
        glue("mean_utci_{time}_baseline_nonbuilding_areas"),
        glue("mean_utci_{time}_scenario_nonbuilding_areas"),
        glue("mean_utci_{time}_change_nonbuilding_areas"),
        glue("high_risk_utci_pct__{time}_baseline_nonbuilding_areas"),
        glue("high_risk_utci_pct__{time}_scenario_nonbuilding_areas"),
        glue("high_risk_utci_pct__{time}_change_nonbuilding_areas"),
        glue("max_utci_{time}_change_impact_areas"),
        glue("mean_utci_{time}_change_impact_areas"),
        glue("utci_reduction_{time}_impact_area")
      ),
      value = c(
        bl_nb,
        sc_nb,
        sc_nb - bl_nb,
        bl_hr_nb,
        sc_hr_nb,
        sc_hr_nb - bl_hr_nb,
        imp$max,
        imp$mean,
        imp$pct
      )
    )
    
    utci_metrics <- bind_rows(utci_metrics, metrics)
  }
  
  bind_rows(scalar_metrics, utci_metrics) |>
    mutate(
      date                 = date,
      application_id       = "ccl",
      cities_id            = city,
      areas_of_interest_id = aoi_name,
      interventions_id     = "cool_roofs",
      scenarios_id         = paste("cool_roofs", str_replace(scenario, "-", "_"), sep = "_"),
      value                = round(value, 2)
    ) |>
    write_csv(here("misc", "metrics-testing", glue("{city}-{aoi_name}-metrics__{infra}__{scenario}.csv")))
}


# ── shade structures ──────────────────────────────────────────────────────────

calc_shade_structures_metrics <- function(city, aoi_name, tiles_aoi, infra, scenario) {
  
  library(geoarrow)
  library(sfarrow)
  
  baseline_folder <- glue("city_projects/{city}/{aoi_name}/scenarios/baseline/baseline")
  scenario_folder <- glue("city_projects/{city}/{aoi_name}/scenarios/{infra}/{scenario}")
  
  aoi <- st_read(glue("{aws_http}/{baseline_folder}/aoi__baseline__baseline.geojson"))
  
  parks           <- st_read(glue("{aws_http}/{scenario_folder}/parks__{infra}__{scenario}.geojson")) |> 
    st_filter(aoi)
  park_area       <- sum(parks$area_sqm)
  shade_structures <- st_read(glue("{aws_http}/{scenario_folder}/structures__{infra}__{scenario}.geojson"))
  
  scenario_tiles <- list_tiles(glue("s3://wri-cities-tcm/city_projects/{city}/{aoi_name}/scenarios/{infra}/{scenario}/"))
  
  met_data <- read_csv(glue("{aws_http}/{baseline_folder}/metadata/met_files/met_era5_hottest_days.csv"), skip = 2)
  date <- met_data |> slice(1) |> mutate(date = glue("{Year}_{Month}_{Day}")) |> pull(date)
  
  results <- tibble()
  
  for (time in c("1200", "1500", "1800")) {
    
    baseline_utci_rast <- load_and_merge(
      glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/utci-{time}__baseline__baseline.tif")
    ) |> crop(aoi) |> mask(aoi)
    
    scenario_utci_rast <- load_and_merge(
      glue("{aws_http}/{scenario_folder}/{scenario_tiles}/ccl_layers/utci-{time}__{infra}__{scenario}.tif")
    ) |> mosaic(baseline_utci_rast, fun = "first") |> 
      crop(aoi) |> mask(aoi)
    
    baseline_shade_rast <- load_and_merge(
      glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/shade-{time}__baseline__baseline.tif")
    ) > 0
    baseline_shade_rast <- baseline_shade_rast |> 
      crop(aoi) |> mask(aoi)
    
    scenario_shade_rast <- load_and_merge(
      glue("{aws_http}/{scenario_folder}/{scenario_tiles}/ccl_layers/shade-{time}__{infra}__{scenario}.tif")
    ) > 0 
    scenario_shade_rast <- mosaic(scenario_shade_rast, baseline_shade_rast, fun = "first") |> 
      crop(aoi) |> mask(aoi)
    
    baseline_shade_dist_rast <- load_and_merge(
      glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/shade-distance-{time}__baseline__baseline.tif")
    ) |> crop(aoi) |> mask(aoi)
    
    scenario_shade_dist_rast <- load_and_merge(
      glue("{aws_http}/{scenario_folder}/{scenario_tiles}/ccl_layers/shade-distance-{time}__{infra}__{scenario}.tif")
    ) |> mosaic(baseline_shade_dist_rast, fun = "first") |> 
      crop(aoi) |> mask(aoi)
    
    utci_diff_rast <- load_and_merge(
      glue("{aws_http}/{scenario_folder}/{scenario_tiles}/ccl_layers/utci-{time}__{infra}__{scenario}__vs-baseline.tif")
    ) |> extend(baseline_shade_dist_rast, fill = 0) |> 
      crop(aoi) |> mask(aoi) 
    
    baseline_utci_cat_rast <- load_and_merge(
      glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/utci-cat-{time}__baseline__baseline.tif")
    ) |> crop(aoi) |> mask(aoi)
    
    scenario_utci_cat_rast <- load_and_merge(
      glue("{aws_http}/{scenario_folder}/{scenario_tiles}/ccl_layers/utci-cat-{time}__{infra}__{scenario}.tif")
    ) |> mosaic(baseline_utci_cat_rast, fun = "first") |> 
      crop(aoi) |> mask(aoi)
    
    imp <- impact_stats(utci_diff_rast)
    
    bl_shade_parks <- mean(values(baseline_shade_rast |> mask(parks)), na.rm = TRUE)
    sc_shade_parks <- mean(values(scenario_shade_rast |> mask(parks)), na.rm = TRUE)
    
    bl_utci_parks  <- mean(values(baseline_utci_rast |> mask(parks)), na.rm = TRUE)
    sc_utci_parks  <- mean(values(scenario_utci_rast |> mask(parks)), na.rm = TRUE)
    
    bl_dist_parks  <- mean(values(baseline_shade_dist_rast |> mask(parks)), na.rm = TRUE)
    sc_dist_parks  <- mean(values(scenario_shade_dist_rast |> mask(parks)), na.rm = TRUE)
    
    ach_dist_parks <- exactextractr::exact_extract(baseline_shade_dist_rast, parks, "mean", force_df = TRUE) |>
      quantile(0.1, na.rm = TRUE)
    
    bl_hr_parks <- calc_binary_hot_area_pct(baseline_utci_cat_rast, area_mask_vect = parks)
    sc_hr_parks <- calc_binary_hot_area_pct(scenario_utci_cat_rast, area_mask_vect = parks)
    
    metrics <- tibble(
      indicators_id = c(
        glue("baseline_mean_utci_parks_{time}"),
        glue("scenario_mean_utci_parks_{time}"),
        glue("change_mean_utci_parks_{time}"),
        glue("baseline_park_shade_cover_{time}"),
        glue("scenario_park_shade_cover_{time}"),
        glue("change_park_shade_cover_{time}"),
        glue("mean_distance_shade_cover_{time}_baseline_parks"),
        glue("mean_distance_shade_cover_{time}_scenario_parks"),
        glue("mean_distance_shade_cover_{time}_change_parks"),
        glue("mean_distance_shade_cover_{time}_achievable_parks"),
        glue("mean_distance_shade_cover_{time}_progress_parks"),
        glue("high_risk_utci_pct__{time}_baseline_parks"),
        glue("high_risk_utci_pct__{time}_scenario_parks"),
        glue("high_risk_utci_pct__{time}_change_parks"),
        glue("max_utci_{time}_change_impact_areas"),
        glue("mean_utci_{time}_change_impact_areas"),
        glue("utci_reduction_{time}_impact_area")
      ),
      value = c(
        bl_utci_parks,
        sc_utci_parks,
        sc_utci_parks - bl_utci_parks,
        bl_shade_parks * 100,
        sc_shade_parks * 100,
        (sc_shade_parks - bl_shade_parks) * 100,
        bl_dist_parks,
        sc_dist_parks,
        sc_dist_parks - bl_dist_parks,
        ach_dist_parks,
        (sc_dist_parks - bl_dist_parks) / (ach_dist_parks - bl_dist_parks) * 100,
        bl_hr_parks,
        sc_hr_parks,
        sc_hr_parks - bl_hr_parks,
        imp$max,
        imp$mean,
        imp$pct
      )
    )
    
    results <- bind_rows(results, metrics)
  }
  
  bind_rows(
    results,
    tibble(
      indicators_id = c("new_shade_structures", "park_area"),
      value         = c(nrow(shade_structures), park_area)
    )
  ) |>
    mutate(
      date                 = date,
      application_id       = "ccl",
      cities_id            = city,
      areas_of_interest_id = aoi_name,
      interventions_id     = infra,
      scenarios_id         = scenario,
      value                = round(value, 2)
    ) |>
    write_csv(here("misc", "metrics-testing", glue("{city}-{aoi_name}-metrics__{infra}__{scenario}.csv")))
}


# ── cool roofs + trees (combo) ────────────────────────────────────────────────

calc_cool_roofs_trees_metrics <- function(city, aoi_name, tiles_aoi, infra, scenario) {
  
  baseline_folder     <- glue("city_projects/{city}/{aoi_name}/scenarios/baseline/baseline")
  scenario_folder     <- glue("city_projects/{city}/{aoi_name}/scenarios/{infra}/{scenario}")
  open_urban_aws_http <- glue("{aws_http}/OpenUrban/{city}")
  
  baseline_met_data <- read_csv(glue("{aws_http}/{baseline_folder}/metadata/met_files/met_era5_hottest_days.csv"), skip = 2)
  scenario_met_data <- read_csv(glue("{aws_http}/{scenario_folder}/metadata/met_files/reduced_temps.csv"), skip = 2)
  
  date <- baseline_met_data |> slice(1) |> mutate(date = glue("{Year}_{Month}_{Day}")) |> pull(date)
  
  aoi <- st_read(glue("{aws_http}/{baseline_folder}/aoi__baseline__baseline.geojson"))
  
  ped_area_rast <- load_and_merge(
    glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/pedestrian-areas__baseline__baseline.tif")
  )
  aoi <- aoi |> st_transform(st_crs(ped_area_rast))
  ped_area_rast <- ped_area_rast |> crop(aoi) |> mask(aoi)
  
  px_area         <- cellSize(ped_area_rast)
  pedestrian_area <- global(px_area * ped_area_rast, "sum", na.rm = TRUE)[1, 1]
  
  nonbuild_area_rast <- load_and_merge(
    glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/non-building-areas__baseline__baseline.tif")
  ) |> crop(aoi) |> mask(aoi)
  nonbuild_area <- global(px_area * nonbuild_area_rast, "sum", na.rm = TRUE)[1, 1]
  
  template_rast <- load_and_merge(
    glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/utci-1200__baseline__baseline.tif")
  ) |> crop(aoi) |> mask(aoi)
  ped_crop      <- ped_area_rast     |> crop(template_rast)
  nonbuild_crop <- nonbuild_area_rast |> crop(template_rast)
  
  results <- tibble()
  
  for (time in c("1200", "1500", "1800")) {
    
    baseline_shade_rast <- load_and_merge(
      glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/shade-{time}__baseline__baseline.tif")
    ) |> crop(aoi) |> mask(aoi) > 0
    
    scenario_shade_rast <- load_and_merge(
      glue("{aws_http}/{scenario_folder}/{tiles_aoi}/ccl_layers/shade-{time}__{infra}__{scenario}.tif")
    ) |> crop(aoi) |> mask(aoi) > 0
    
    baseline_utci_rast <- load_and_merge(
      glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/utci-{time}__baseline__baseline.tif")
    ) |> crop(aoi) |> mask(aoi)
    
    scenario_utci_rast <- load_and_merge(
      glue("{aws_http}/{scenario_folder}/{tiles_aoi}/ccl_layers/utci-{time}__{infra}__{scenario}.tif")
    ) |> crop(aoi) |> mask(aoi)
    
    baseline_shade_dist_rast <- load_and_merge(
      glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/shade-distance-{time}__baseline__baseline.tif")
    ) |> crop(aoi) |> mask(aoi)
    
    scenario_shade_dist_rast <- load_and_merge(
      glue("{aws_http}/{scenario_folder}/{tiles_aoi}/ccl_layers/shade-distance-{time}__{infra}__{scenario}.tif")
    ) |> mosaic(baseline_shade_dist_rast, fun = "first") |> crop(aoi) |> mask(aoi)
    
    utci_diff_rast <- load_and_merge(
      glue("{aws_http}/{scenario_folder}/{tiles_aoi}/ccl_layers/utci-{time}__{infra}__{scenario}__vs-baseline.tif")
    ) |> crop(aoi) |> mask(aoi)
    
    baseline_utci_cat_rast <- load_and_merge(
      glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/utci-cat-{time}__baseline__baseline.tif")
    ) |> crop(aoi) |> mask(aoi)
    
    scenario_utci_cat_rast <- load_and_merge(
      glue("{aws_http}/{scenario_folder}/{tiles_aoi}/ccl_layers/utci-cat-{time}__{infra}__{scenario}.tif")
    ) |> crop(aoi) |> mask(aoi)
    
    imp <- impact_stats(utci_diff_rast)
    
    bl_aoi <- mean_utci_masked(baseline_utci_rast)
    sc_aoi <- mean_utci_masked(scenario_utci_rast)
    bl_ped <- mean_utci_masked(baseline_utci_rast, ped_crop)
    sc_ped <- mean_utci_masked(scenario_utci_rast, ped_crop)
    bl_nb  <- mean_utci_masked(baseline_utci_rast, nonbuild_crop)
    sc_nb  <- mean_utci_masked(scenario_utci_rast, nonbuild_crop)
    
    bl_hr_nb <- calc_binary_hot_area_pct(baseline_utci_cat_rast, area_mask_rast = nonbuild_crop)
    sc_hr_nb <- calc_binary_hot_area_pct(scenario_utci_cat_rast, area_mask_rast = nonbuild_crop)
    
    metrics <- tibble(
      indicators_id = c(
        glue("mean_utci_{time}_baseline_aoi"),
        glue("mean_utci_{time}_scenario_aoi"),
        glue("mean_utci_{time}_change_aoi"),
        glue("shade_cover_{time}_baseline_aoi"),
        glue("shade_cover_{time}_scenario_aoi"),
        glue("shade_cover_{time}_change_aoi"),
        glue("mean_distance_shade_cover_{time}_baseline_aoi"),
        glue("mean_distance_shade_cover_{time}_scenario_aoi"),
        glue("mean_distance_shade_cover_{time}_change_aoi"),
        
        glue("mean_utci_{time}_baseline_pedestrian"),
        glue("mean_utci_{time}_scenario_pedestrian"),
        glue("mean_utci_{time}_change_pedestrian"),
        glue("shade_cover_{time}_baseline_pedestrian"),
        glue("shade_cover_{time}_scenario_pedestrian"),
        glue("shade_cover_{time}_change_pedestrian"),
        glue("mean_distance_shade_cover_{time}_baseline_pedestrian"),
        glue("mean_distance_shade_cover_{time}_scenario_pedestrian"),
        glue("mean_distance_shade_cover_{time}_change_pedestrian"),
        
        glue("mean_utci_{time}_baseline_nonbuilding_areas"),
        glue("mean_utci_{time}_scenario_nonbuilding_areas"),
        glue("mean_utci_{time}_change_nonbuilding_areas"),
        glue("shade_cover_{time}_baseline_nonbuilding_areas"),
        glue("shade_cover_{time}_scenario_nonbuilding_areas"),
        glue("shade_cover_{time}_change_nonbuilding_areas"),
        glue("mean_distance_shade_cover_{time}_baseline_nonbuilding_areas"),
        glue("mean_distance_shade_cover_{time}_scenario_nonbuilding_areas"),
        glue("mean_distance_shade_cover_{time}_change_nonbuilding_areas"),
        glue("high_risk_utci_pct__{time}_baseline_nonbuilding_areas"),
        glue("high_risk_utci_pct__{time}_scenario_nonbuilding_areas"),
        glue("high_risk_utci_pct__{time}_change_nonbuilding_areas"),
        
        glue("max_utci_{time}_change_impact_areas"),
        glue("mean_utci_{time}_change_impact_areas"),
        glue("utci_reduction_{time}_impact_area")
      ),
      value = c(
        bl_aoi,
        sc_aoi,
        sc_aoi - bl_aoi,
        shade_pct_masked(baseline_shade_rast),
        shade_pct_masked(scenario_shade_rast),
        shade_pct_masked(scenario_shade_rast) - shade_pct_masked(baseline_shade_rast),
        shade_dist_masked(baseline_shade_dist_rast),
        shade_dist_masked(scenario_shade_dist_rast),
        shade_dist_masked(scenario_shade_dist_rast) - shade_dist_masked(baseline_shade_dist_rast),
        
        bl_ped,
        sc_ped,
        sc_ped - bl_ped,
        shade_pct_masked(baseline_shade_rast, ped_crop),
        shade_pct_masked(scenario_shade_rast, ped_crop),
        shade_pct_masked(scenario_shade_rast, ped_crop) - shade_pct_masked(baseline_shade_rast, ped_crop),
        shade_dist_masked(baseline_shade_dist_rast, ped_crop),
        shade_dist_masked(scenario_shade_dist_rast, ped_crop),
        shade_dist_masked(scenario_shade_dist_rast, ped_crop) - shade_dist_masked(baseline_shade_dist_rast, ped_crop),
        
        bl_nb,
        sc_nb,
        sc_nb - bl_nb,
        shade_pct_masked(baseline_shade_rast, nonbuild_crop),
        shade_pct_masked(scenario_shade_rast, nonbuild_crop),
        shade_pct_masked(scenario_shade_rast, nonbuild_crop) - shade_pct_masked(baseline_shade_rast, nonbuild_crop),
        shade_dist_masked(baseline_shade_dist_rast, nonbuild_crop),
        shade_dist_masked(scenario_shade_dist_rast, nonbuild_crop),
        shade_dist_masked(scenario_shade_dist_rast, nonbuild_crop) - shade_dist_masked(baseline_shade_dist_rast, nonbuild_crop),
        bl_hr_nb,
        sc_hr_nb,
        sc_hr_nb - bl_hr_nb,
        
        imp$max,
        imp$mean,
        imp$pct
      )
    )
    
    results <- bind_rows(results, metrics)
  }
  
  # Trees
  baseline_tree_rast <- load_and_merge(
    glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/tree-cover__baseline__baseline.tif")
  ) |> subst(from = NA, 0) |> crop(template_rast) |> mask(aoi)
  
  scenario_tree_rast <- load_and_merge(
    glue("{aws_http}/{scenario_folder}/{tiles_aoi}/ccl_layers/tree-cover__{infra}__{scenario}.tif")
  ) |> subst(from = NA, 0) |> crop(template_rast) |> mask(aoi)
  
  tree_cover_bl_aoi  <- mean(values(baseline_tree_rast, na.rm = TRUE))
  tree_cover_sc_aoi  <- mean(values(scenario_tree_rast, na.rm = TRUE))
  tree_cover_bl_ped  <- mean(values(mask(baseline_tree_rast, ped_crop, maskvalues = 0), na.rm = TRUE))
  tree_cover_sc_ped  <- mean(values(mask(scenario_tree_rast, ped_crop, maskvalues = 0), na.rm = TRUE))
  
  baseline_tree_points <- load_baseline_tree_points(tiles_aoi, baseline_folder, aoi)
  pedestrian_vals <- terra::extract(ped_crop, vect(baseline_tree_points))
  baseline_tree_points$pedestrian <- pedestrian_vals[[2]]
  tree_n_baseline_pedestrian <- nrow(baseline_tree_points |> filter(pedestrian == 1))
  
  scenario_tree_points       <- st_read(glue("{aws_http}/{scenario_folder}/new-tree-points__{infra}__{scenario}.geojson"), quiet = TRUE)
  tree_n_scenario_pedestrian <- nrow(scenario_tree_points)
  
  target_coverage           <- load_tree_achievable(city, open_urban_aws_http)
  tree_cover_achievable_ped <- target_coverage * 100
  
  # Cool roof layers
  baseline_albedo <- load_and_merge(
    glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/albedo__baseline__baseline.tif")
  )
  scenario_albedo <- load_and_merge(
    glue("{aws_http}/{scenario_folder}/{tiles_aoi}/ccl_layers/albedo__{infra}__{scenario}.tif")
  )
  buildings <- load_and_merge(
    glue("{aws_http}/{baseline_folder}/{tiles_aoi}/ccl_layers/building-areas__baseline__baseline.tif")
  ) |> subst(0, NA)
  
  baseline_albedo <- baseline_albedo |> crop(aoi) |> mask(aoi)
  scenario_albedo <- scenario_albedo |> crop(aoi) |> mask(aoi)
  buildings       <- buildings       |> crop(aoi) |> mask(aoi)
  
  ach <- achievable_albedo_rast(city, baseline_albedo, buildings, baseline_folder, tiles_aoi)
  ach_albedo      <- ach$rast        |> crop(aoi) |> mask(aoi)
  
  buildings_vect <- map_df(
    glue("{aws_http}/{scenario_folder}/{tiles_aoi}/ccl_layers/buildings__{infra}__{scenario}.geojson"),
    ~ st_read(.x, quiet = TRUE)
  ) |> distinct(id, .keep_all = TRUE)
  
  cool_roofs_baseline  <- buildings_vect |> filter(median_alb >= cool_roof_alb)
  cool_roofs_scenario  <- buildings_vect |> filter(median_alb >= cool_roof_alb | updated)
  baseline_roof_alb    <- mean(buildings_vect$median_alb, na.rm = TRUE)
  scenario_roof_alb    <- buildings_vect |>
    mutate(updated_alb = case_when(updated ~ cool_roof_alb, !updated ~ median_alb)) |>
    summarize(mean = mean(updated_alb, na.rm = TRUE)) |> pull(mean)
  
  baseline_cool_roof_area  <- sum(cool_roofs_baseline$area_m2)
  scenario_cool_roof_area  <- sum(cool_roofs_scenario$area_m2)
  technical_cool_roof_area <- sum(buildings_vect$area_m2)
  
  alb_target_val      <- ach$target
  alb_target_low_val  <- ach$target_low
  alb_target_high_val <- ach$target_high
  
  baseline_reflectivity   <- mean(values(baseline_albedo), na.rm = TRUE)
  scenario_reflectivity   <- mean(values(scenario_albedo), na.rm = TRUE)
  achievable_reflectivity <- mean(values(ach_albedo),      na.rm = TRUE)
  
  baseline_mean_air_temp_1200 <- baseline_met_data |> filter(Hour == 12) |> pull(Temperature)
  baseline_mean_air_temp_1500 <- baseline_met_data |> filter(Hour == 15) |> pull(Temperature)
  baseline_mean_air_temp_1800 <- baseline_met_data |> filter(Hour == 18) |> pull(Temperature)
  scenario_mean_air_temp_1200 <- scenario_met_data |> filter(Hour == 12) |> pull(Temperature)
  scenario_mean_air_temp_1500 <- scenario_met_data |> filter(Hour == 15) |> pull(Temperature)
  scenario_mean_air_temp_1800 <- scenario_met_data |> filter(Hour == 18) |> pull(Temperature)
  
  tree_cover_change_aoi <- tree_cover_sc_aoi - tree_cover_bl_aoi 
  
  scalar_metrics <- tibble(
    indicators_id = c(
      "tree_cover_baseline_aoi",
      "tree_cover_scenario_aoi",
      "tree_cover_change_aoi",
      "tree_cover_baseline_pedestrian",
      "tree_cover_scenario_pedestrian",
      "tree_cover_change_pedestrian",
      "tree_n_baseline_pedestrian",
      "tree_n_change_pedestrian",
      "tree_n_scenario_pedestrian",
      "tree_cover_achievable_pedestrian",
      "tree_cover_progress",
      "pedestrian_area",
      "baseline_cool_roof_area",
      "scenario_cool_roof_area",
      "technical_cool_roof_area",
      "change_cool_roof_area",
      "achievable_cool_roof_reflectivity",
      "achievable_cool_roof_reflectivity_low",
      "achievable_cool_roof_reflectivity_high",
      "baseline_reflectivity",
      "scenario_reflectivity",
      "achievable_reflectivity",
      "change_reflectivity",
      "baseline_roof_reflectivity",
      "scenario_roof_reflectivity",
      "change_roof_reflectivity",
      "progress_reflectivity",
      "progress_cool_roofs",
      "baseline_mean_air_temp_1200",
      "baseline_mean_air_temp_1500",
      "baseline_mean_air_temp_1800",
      "scenario_mean_air_temp_1200",
      "scenario_mean_air_temp_1500",
      "scenario_mean_air_temp_1800",
      "change_mean_air_temp_1200",
      "change_mean_air_temp_1500",
      "change_mean_air_temp_1800",
      "mean_air_temp_1500_change_impact_areas",
      "air_temp_reduction_1500_impact_area",
      "nonbuilding_area"
    ),
    value = c(
      tree_cover_bl_aoi * 100,
      tree_cover_sc_aoi * 100,
      tree_cover_change_aoi * 100,
      tree_cover_bl_ped * 100,
      tree_cover_sc_ped * 100,
      (tree_cover_sc_ped - tree_cover_bl_ped) * 100,
      tree_n_baseline_pedestrian,
      tree_n_scenario_pedestrian,
      tree_n_scenario_pedestrian + tree_n_baseline_pedestrian,
      tree_cover_achievable_ped,
      (tree_cover_sc_ped - tree_cover_bl_ped) / (target_coverage - tree_cover_bl_ped) * 100,
      pedestrian_area,
      baseline_cool_roof_area,
      scenario_cool_roof_area,
      technical_cool_roof_area,
      scenario_cool_roof_area - baseline_cool_roof_area,
      alb_target_val,
      alb_target_low_val,
      alb_target_high_val,
      baseline_reflectivity,
      scenario_reflectivity,
      achievable_reflectivity,
      scenario_reflectivity - baseline_reflectivity,
      baseline_roof_alb * 100,
      scenario_roof_alb * 100,
      (scenario_roof_alb - baseline_roof_alb) * 100,
      (scenario_reflectivity - baseline_reflectivity) /
        (achievable_reflectivity - baseline_reflectivity) * 100,
      (scenario_cool_roof_area - baseline_cool_roof_area) /
        (technical_cool_roof_area - baseline_cool_roof_area) * 100,
      baseline_mean_air_temp_1200,
      baseline_mean_air_temp_1500,
      baseline_mean_air_temp_1800,
      scenario_mean_air_temp_1200,
      scenario_mean_air_temp_1500,
      scenario_mean_air_temp_1800,
      scenario_mean_air_temp_1200 - baseline_mean_air_temp_1200,
      scenario_mean_air_temp_1500 - baseline_mean_air_temp_1500,
      scenario_mean_air_temp_1800 - baseline_mean_air_temp_1800,
      tree_cover_change_aoi * (-3.3) + (scenario_mean_air_temp_1500 - baseline_mean_air_temp_1500),
      100,
      nonbuild_area
    )
  )
  
  bind_rows(results, scalar_metrics) |>
    mutate(
      date                 = date,
      application_id       = "ccl",
      cities_id            = city,
      areas_of_interest_id = aoi_name,
      interventions_id     = infra,
      scenarios_id         = glue("{infra}__{scenario}"),
      value                = round(value, 2)
    ) |>
    write_csv(here("misc", "metrics-testing", glue("{city}-{aoi_name}-metrics__{infra}__{scenario}.csv")))
}
