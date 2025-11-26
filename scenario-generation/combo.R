street_trees <- rast("~/Documents/github/cities-heat-resilient-infrastructure/data/ZAF-Cape_Town-test/scenarios/street-trees/achievable-90pctl/scenario-tree-canopy-height.tif")
baseline_trees <- rast("~/Documents/github/cities-heat-resilient-infrastructure/data/ZAF-Cape_Town-test/cif_tree_canopy.tif")
new_trees <- street_trees - baseline_trees
baseline_tree_cover <- rast("~/Documents/github/cities-heat-resilient-infrastructure/data/ZAF-Cape_Town-test/scenarios/baseline/tree_cover_baseline.tif")

baseline_alb <- rast("~/Documents/github/cities-heat-resilient-infrastructure/data/ZAF-Cape_Town-test/cif_albedo_cloud_masked.tif")
cool_roofs <- rast("~/Documents/github/cities-heat-resilient-infrastructure/data/ZAF-Cape_Town-test/scenarios/cool-roofs/all-buildings/albedo_cool_roofs_achievable.tif")

tree_alb <- mask(baseline_alb, baseline_tree_cover, maskvalues = 0) %>% 
  values(na.rm = T)

q <- quantile(tree_alb, probs = c(0.25, 0.75), na.rm = TRUE)
tree_alb_iqr <- tree_alb[tree_alb >= q[1] & tree_alb <= q[2]]

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

# Use:
cool_roofs_updated <- update_albedo_with_trees(cool_roofs, new_trees, tree_alb_iqr)

writeRaster(cool_roofs_updated, 
            here("data", city_folder, "scenarios", 
                 "trees_cool-roofs__pedestrian-achievable-90pctl_all-buildings", 
                 "albedo__trees_cool-roofs__pedestrian-achievable-90pctl_all-buildings.tif"))
