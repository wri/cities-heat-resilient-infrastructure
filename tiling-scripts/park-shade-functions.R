library(here)
library(terra)
library(sf)
library(dplyr)
library(purrr)
library(readr)
library(glue)
library(geoarrow)
library(sfarrow)
library(tidyverse)
library(exactextractr)

source(here("tiling-scripts", "utils.R"))

# Make scenario folder
ensure_s3_prefix(bucket, scenario_folder)

# Functions ---------------------------------------------------------------

# source(here("tiling-scripts", "utils.R"))
source(here("scenario-generation", "park-shade-structures", "shade-generating-functions.R"))

generate_shade_structures <- function(
    park_idx,
    park_suitable_area_vectors,
    buffered_tile_grid,
    bucket,
    aws_http,
    baseline_folder,
    scenario_folder,
    structure_size = 5,
    shade_pct = 0.25,
    spacing = 5,
    min_shade_area = 25,
    max_dist_to_shade = 50
) {
  
  park <- park_suitable_area_vectors %>%
    dplyr::filter(park_id == park_idx)
  
  message(glue::glue("Park {park$park_id}"))
  
  tile_ids <- park %>%
    dplyr::pull(buffered_tile_names) %>%
    unlist()
  
  # Find shadow file
  stamp <- find_shadow_stamp(bucket, baseline_folder, tile_ids[[1]])
  
  shade_paths <- glue::glue(
    "{aws_http}/{baseline_folder}/{tile_ids}/tcm_results/met_era5_hottest_days/Shadow_{stamp}_1200D.tif"
  )
  shade_rast <- load_and_merge(shade_paths)
  
  tree_paths <- glue::glue("{aws_http}/{baseline_folder}/{tile_ids}/raster_files/cif_tree_canopy.tif")
  tree_rast <- load_and_merge(tree_paths) >= 3
  
  # Shade raster convention: Shadow == 0 means shaded 
  shaded   <- shade_rast < 1
  unshaded <- (shade_rast >= 1) * 1  # 1 = unshaded, 0 = shaded
  
  # dist_to_shade <- terra::distance(terra::subst(shaded, 0, NA)) |>
  #   terra::mask(terra::vect(park))
  # dist_to_shade <- terra::global(dist_to_shade, "max", na.rm = TRUE)[, 1]
  
  park <- park %>%
    dplyr::mutate(
      shaded_pct   = exact_extract(shaded, geometry, "mean"))
  
  if (park$area_sqm > 4046.86) {
    shade_structures <- shade_dist_area(
      park = park,
      unshaded_raster = unshaded,
      min_shade_area = min_shade_area,
      max_dist_to_shade = max_dist_to_shade,
      structure_size = structure_size,
      spacing = spacing
    )
  } else {
    shade_structures <- generate_squares_in_valid_area(
      park = park,
      unshaded_raster = unshaded,
      structure_size = structure_size,
      shade_pct = shade_pct,
      spacing = spacing
    )
  }
  
  if (is.null(shade_structures) || nrow(shade_structures) == 0) {
    # return an empty sf in the right CRS
    return(shade_structures)
  }
  
  structure_height <- 8 / 3.281
  shade_structures <- shade_structures %>% dplyr::mutate(height = structure_height)
  
  shade_structures_rast <- shade_structures %>%
    terra::rasterize(shade_rast, field = "height", background = 0)
  
  for (t in tile_ids) {
    
    tile <- buffered_tile_grid %>% dplyr::filter(tile_name == t)
    
    x <- shade_structures_rast %>% 
      crop(tile) %>% 
      extend(tile, fill = 0)
    
    ensure_s3_prefix(bucket, glue::glue("{scenario_folder}/{t}/ccl_layers"))
    
    url_existing <- glue::glue(
      "{aws_http}/{scenario_folder}/{t}/ccl_layers/structures-as-trees.tif"
    )
    
    existing <- tryCatch(terra::rast(url_existing), error = function(e) NULL)
    
    if (!is.null(existing)) {
      x <- terra::mosaic(x, existing, fun = "max")
    }
    
    write_s3(x, glue::glue("{bucket}/{scenario_folder}/{t}/ccl_layers/structures-as-trees.tif"))
    message(glue::glue("{t} shade raster saved"))
  }
  
  return(list(park = park, shade_structures = shade_structures))
}


run_shade_scenario <- function(bucket, 
                               aws_http, 
                               open_urban_aws_http,
                               baseline_folder, 
                               scenario_folder,
                               city, aoi, 
                               buffered_tile_grid, tile_grid) {
  
  parks <- st_read(glue("{open_urban_aws_http}/open_space/open_space_all.geojson"), quiet = TRUE) %>%
    st_filter(aoi, .predicate = sf::st_within)
  
  park_vectors <- parks %>%
    dplyr::filter(leisure != "pitch") %>%
    sf::st_union() %>%
    sf::st_sf() %>%
    sf::st_cast("POLYGON") %>%
    dplyr::mutate(
      area_sqm = as.numeric(units::set_units(sf::st_area(geometry), "m^2")),
      park_id  = dplyr::row_number()
    )
  
  pitch_vectors <- parks %>%
    dplyr::filter(leisure == "pitch") %>%
    sf::st_union() %>%
    sf::st_sf()
  
  if (nrow(pitch_vectors) > 0) {
    park_suitable_area_vectors <- park_vectors %>%
      sf::st_difference(pitch_vectors) %>%
      dplyr::mutate(area_sqm_suitable = as.numeric(units::set_units(sf::st_area(geometry), "m^2")))
  } else {
    park_suitable_area_vectors <- park_vectors
  }
  
  park_suitable_area_vectors <- park_suitable_area_vectors %>%
    {
      bt  <- sf::st_transform(buffered_tile_grid, sf::st_crs(.))
      ubt <- sf::st_transform(tile_grid,        sf::st_crs(.))
      
      dplyr::mutate(
        .,
        buffered_tile_names   = sf::st_intersects(., bt)  %>% purrr::map(\(idx) unique(bt$tile_name[idx])),
        unbuffered_tile_names = sf::st_intersects(., ubt) %>% purrr::map(\(idx) unique(ubt$tile_name[idx]))
      )
    } %>%
    dplyr::filter(lengths(unbuffered_tile_names) > 0)
  
  results <- purrr::map(
    park_suitable_area_vectors$park_id,
    ~ generate_shade_structures(
      park_idx = .x,
      park_suitable_area_vectors = park_suitable_area_vectors,
      buffered_tile_grid = buffered_tile_grid,
      bucket = bucket,
      aws_http = aws_http,
      baseline_folder = baseline_folder,
      scenario_folder = scenario_folder
    )
  )
  
  all_parks <- purrr::map(results, "park") %>% 
    dplyr::bind_rows() %>% 
    select(- buffered_tile_names, - unbuffered_tile_names)
  
  shade_structures_all_parks <- purrr::map(results, "shade_structures") %>%
    purrr::compact() %>%
    dplyr::bind_rows()
  
  write_s3(all_parks, glue::glue("{bucket}/{scenario_folder}/parks__shade-structures__all-parks.geojson"))
  
  if (nrow(shade_structures_all_parks) > 0) {
    write_s3(shade_structures_all_parks, glue::glue("{bucket}/{scenario_folder}/structures__shade-structures__all-parks.geojson"))
  }
  
  # invisible(list(
  #   parks = all_parks,
  #   structures = shade_structures_all_parks
  # ))
}

