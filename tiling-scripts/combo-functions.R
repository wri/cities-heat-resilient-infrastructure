update_albedo_with_trees <- function(cool_roofs, new_trees, tree_alb_iqr) {
  
  # Get cell indices where new_trees is "on": non-NA and != 0
  nt_vals <- values(new_trees, mat = FALSE)
  on_idx  <- which(!is.na(nt_vals) & nt_vals != 0L)
  if (!length(on_idx)) return(cool_roofs)  # nothing to update
  
  # Sample one tree albedo per target cell (with replacement)
  sampled <- sample(tree_alb_iqr, size = length(on_idx), replace = TRUE)
  
  # Write into cool_roofs
  cr_vals <- values(cool_roofs, mat = FALSE)
  cr_vals[on_idx] <- sampled
  setValues(cool_roofs, cr_vals)
}

source(here("tiling-scripts", "utils.R"))

cool_roof_tree_combo <- function(city,
                                 aoi_name,
                                 aws_http,
                                 city_folder,
                                 baseline_folder,
                                 infra,
                                 scenario,
                                 tiles_aoi,
                                 buffered_tile_grid) {
  
  # Sort infra in alphabetical order  
  combo <- tibble(
    infra = str_split(infra, "_"),
    scenario = str_split(scenario, "_")
  ) %>%
    unnest(cols = everything())|> 
    arrange(infra) 
  
  tree_scenario <- combo |> 
    filter(infra == "trees")
  
  cool_roof_scenario <- combo |> 
    filter(infra == "cool-roofs")
  
  tree_scenario_folder <- file.path(city_folder, "scenarios", 
                                    tree_scenario$infra, tree_scenario$scenario)
  cool_roof_scenario_folder <- file.path(city_folder, "scenarios", 
                                         cool_roof_scenario$infra, cool_roof_scenario$scenario)
  
  infra_combined <- combo %>% 
    pull(infra) %>% 
    str_c(collapse = "_")
  
  scenario_combined <- combo %>% 
    pull(scenario) %>% 
    str_c(collapse = "_")
  
  combo_scenario_folder <- glue("city_projects/{city}/{aoi_name}/scenarios/{infra_combined}/{scenario_combined}")
  
  # Copy new tree points
  system2("aws", c("s3", "cp", 
                   glue("s3://wri-cities-tcm/{tree_scenario_folder}/new-tree-points__{tree_scenario$infra}__{tree_scenario$scenario}.geojson"), 
                   glue("s3://wri-cities-tcm/{combo_scenario_folder}/new-tree-points__{infra_combined}__{scenario_combined}.geojson")))
  
  
  for (tile in tiles_aoi){
    
    baseline_albedo <- rast_retry(glue("{aws_http}/{baseline_folder}/{tile}/ccl_layers/albedo__baseline__baseline.tif"))
    cool_roof_albedo <- rast_retry(glue("{aws_http}/{cool_roof_scenario_folder}/{tile}/ccl_layers/albedo__{cool_roof_scenario$infra}__{cool_roof_scenario$scenario}.tif"))
    
    baseline_tree_cover <- rast_retry(glue("{aws_http}/{baseline_folder}/{tile}/ccl_layers/tree-cover__baseline__baseline.tif"))
    new_tree_cover <- rast_retry(glue("{aws_http}/{tree_scenario_folder}/{tile}/ccl_layers/new-tree-cover__{tree_scenario$infra}__{tree_scenario$scenario}.tif"))
    
    # Get albedo range for existing trees
    tree_albedo <- mask(baseline_albedo, baseline_tree_cover, maskvalues = 0) %>% 
      values(na.rm = T)
    
    q <- quantile(tree_albedo, probs = c(0.25, 0.75), na.rm = TRUE)
    tree_alb_iqr <- tree_albedo[tree_albedo >= q[1] & tree_albedo <= q[2]]
    
    updated_albedo <- update_albedo_with_trees(cool_roof_albedo, new_tree_cover, tree_alb_iqr)
    diff_albedo <- updated_albedo - baseline_albedo
    
    ensure_s3_prefix("wri-cities-tcm", glue("{combo_scenario_folder}/{tile}/ccl_layers"))
    write_s3(updated_albedo, glue("wri-cities-tcm/{combo_scenario_folder}/{tile}/ccl_layers/albedo__{infra_combined}__{scenario_combined}.tif"))
    write_s3(diff_albedo, glue("wri-cities-tcm/{combo_scenario_folder}/{tile}/ccl_layers/albedo__{infra_combined}__{scenario_combined}__vs-baseline.tif"))
    
    # Copy tree scenario ccl layers
    tree_ccl_layers <- c(glue("new-tree-cover__{tree_scenario$infra}__{tree_scenario$scenario}.tif"),
                         glue("new-tree-points__{tree_scenario$infra}__{tree_scenario$scenario}.geojson"),
                         glue("plantable-areas__{tree_scenario$infra}__{tree_scenario$scenario}.tif"),
                         glue("tree-cover__{tree_scenario$infra}__{tree_scenario$scenario}.tif"))
    
    tree_ccl_layers_paths <- glue("s3://wri-cities-tcm/{tree_scenario_folder}/{tile}/ccl_layers/{tree_ccl_layers}")
    
    output_ccl_layers <- c(glue("new-tree-cover__{infra_combined}__{scenario_combined}.tif"),
                           glue("new-tree-points__{infra_combined}__{scenario_combined}.geojson"),
                           glue("plantable-areas__{infra_combined}__{scenario_combined}.tif"),
                           glue("tree-cover__{infra_combined}__{scenario_combined}.tif"))
    
    output_ccl_layers_paths <- glue("s3://wri-cities-tcm/{combo_scenario_folder}/{tile}/ccl_layers/{output_ccl_layers}")
    
    purrr::walk2(
      tree_ccl_layers_paths,
      output_ccl_layers_paths,
      \(src, dst) {
        status <- system2("aws", c("s3", "cp", src, dst))
        if (status != 0) {
          stop("Copy failed: ", src, " -> ", dst)
        }
      }
    )
    
    # Copy buildings
    system2("aws", c("s3", "cp",
                     glue("s3://wri-cities-tcm/{cool_roof_scenario_folder}/{tile}/ccl_layers/buildings__{cool_roof_scenario$infra}__{cool_roof_scenario$scenario}.geojson"),
                     glue("s3://wri-cities-tcm/{combo_scenario_folder}/{tile}/ccl_layers/buildings__{infra_combined}__{scenario_combined}.geojson")))

    print(glue("Saved tile {tile}"))
  }
  
  
  
  
}