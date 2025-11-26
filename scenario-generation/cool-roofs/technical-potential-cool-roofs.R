cool-roofs-potential <- function(aoi, lulc, albedo, city_folder, potential, alb_target = 0.62){
 
  baseline_path <- here("data", city_folder, "scenarios", "baseline")
  scenario_path <- here("data", city_folder, "scenarios", "cool-roofs", glue("{potential}-potential"))
  
  # Create directory
  dir.create(scenario_path, showWarnings = FALSE)
  
  # albedo
  albedo <- rast(here(baseline_path, "albedo_baseline.tif"))
  
  updated_alb <- ifel(lulc >= 600 & lulc < 700 & albedo < alb_target, alb_target, albedo)
  
  writeRaster(updated_alb, here(scenario_path, glue("albedo__cool-roofs__{potential}-potential.tif")))
  
}