dat <- tribble(
  ~ city, ~ aoi_name,
  # "ZAF-Cape_Town",	"business_district",
  # "BRA-Rio_de_Janeiro",	"low_emission_zone",
  # "MEX-Monterrey",	"mitras_centro",
  # # BRA-Teresina,	accelerator_area_big,
  # "ARG-Buenos_Aires",	"cildenez_padre_rodolfo_ricciardelli",
  # "ZAF-Johannesburg",	"jukskei-river",
  # # IND-Bhopal,	tt-nagar,
  # # MEX-Hermosillo,	urban_extent,
  # "BRA-Campinas",	"accelerator_area",
  # "BRA-Florianopolis",	"accelerator_area",
  # "BRA-Fortaleza",	"accelerator_area",
  # "BRA-Recife",	"accelerator_area",
  # "ZAF-Durban", "inner_city_lap",
  "ZAF-Durban", "corridors_of_excellence"
)


library(dplyr)
library(purrr)
library(glue)
library(terra)
library(tibble)


times <- c("1200", "1500", "1800")

process_pair <- function(city, aoi_name,
                         scenario = "custom-n_trees_uniform") {
  
  baseline_folder <- glue("city_projects/{city}/{aoi_name}/scenarios/baseline/baseline")
  scenario_folder <- glue("city_projects/{city}/{aoi_name}/scenarios/trees/{scenario}")
  
  # --- BASELINE: distance rasters (overwrite)
  base_tiles <- list_tiles(glue("s3://wri-cities-tcm/{baseline_folder}/"))
  for (t in base_tiles) {
    message("\n[baseline] ", city, " | ", aoi_name, " | ", t)
    for (time in times) {
      try({
        src <- glue("{aws_http}/{baseline_folder}/{t}/ccl_layers/shade-{time}__baseline__baseline.tif")
        dst <- glue("{bucket}/{baseline_folder}/{t}/ccl_layers/shade-distance-{time}__baseline__baseline.tif")
        
        d <- distance(subst(rast(src) > 0, 0, NA))
        write_s3(d, dst)
      }, silent = FALSE)
    }
  }
  
  # --- TREES: distance + diff vs baseline (overwrite)
  scen_tiles <- list_tiles(glue("s3://wri-cities-tcm/{scenario_folder}/"))
  for (t in scen_tiles) {
    message("\n[trees] ", city, " | ", aoi_name, " | ", t)
    for (time in times) {
      try({
        scen_src <- glue("{aws_http}/{scenario_folder}/{t}/ccl_layers/shade-{time}__trees__{scenario}.tif")
        scen_dst <- glue("{bucket}/{scenario_folder}/{t}/ccl_layers/shade-distance-{time}__trees__{scenario}.tif")
        
        base_src <- glue("{aws_http}/{baseline_folder}/{t}/ccl_layers/shade-{time}__baseline__baseline.tif")
        diff_dst <- glue("{bucket}/{scenario_folder}/{t}/ccl_layers/shade-{time}__trees__{scenario}__vs-baseline.tif")
        
        scen_bin <- rast(scen_src) > 0
        base_bin <- rast(base_src) > 0
        
        write_s3(distance(subst(scen_bin, 0, NA)), scen_dst)
        write_s3(scen_bin - base_bin, diff_dst)
      }, silent = FALSE)
    }
  }
  
  invisible(NULL)
}

# run for all city/aoi pairs
pwalk(dat, process_pair)
