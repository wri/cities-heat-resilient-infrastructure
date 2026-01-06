library(here)
library(terra)
library(tidyverse)
library(glue)
library(lubridate)

system("aws sso login --profile cities-data-dev")
Sys.setenv(AWS_PROFILE = "cities-data-dev",
           AWS_DEFAULT_REGION = "us-east-1",
           AWS_SDK_LOAD_CONFIG = "1")

s3 <- paws::s3()
bucket <- "wri-cities-tcm"
aws_http <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com"

# Process baseline tile data ----------------------------------------------
city <- "BRA-Teresina"

aoi_name <- "urban_extent"
city_folder <- glue("city_projects/{city}/{aoi_name}")

infra_id <- "baseline"
scenario_id <- "baseline"

baseline_folder <- glue("{city_folder}/scenarios/{infra_id}/{scenario_id}")

source(here("tiling-scripts", "utils.R"))
tiles <- list_tiles(glue("s3://wri-cities-tcm/{baseline_folder}"))

if (aoi_name == "urban_extent"){
  met <- read_csv(glue("{aws_http}/{baseline_folder}/metadata/met_files/met_era5_hottest_days.csv"),
                  skip = 2)
} else {
  met <- read_csv(glue("{aws_http}/{baseline_folder}/met_era5_hottest_days.csv"),
                  skip = 2)
}


year <- met$Year %>% unique()
month <- met$Month %>% as.integer() %>% str_pad(2, pad = "0") %>% unique()
day <- met$Day %>% as.integer() %>% str_pad(2, pad = "0") %>% unique()

date_ymd <- glue("{year}-{month}-{day}")
date_doy <- yday(date_ymd)

date <- glue("{year}_{date_doy}")

missing_tmrt <- tibble(
  tile = character(),
  name = character()
)

source(here("utils", "utci.R"))

tile_stats <- tibble(
  tile   = character(),
  time   = integer(),
  min    = double(),
  mean   = double(),
  median = double(),
  max    = double()
)

for (t in tiles) {
  message("Tile: ", t)
  
  for (time in c(1200, 1500, 1800)) {
    
    tmrt_name <- glue("Tmrt_{date}_{time}D")
    tmrt_path <- glue("{aws_http}/{baseline_folder}/{t}/tcm_results/met_era5_hottest_days/{tmrt_name}.tif")
    
    # Try to read the raster
    mrt <- try(rast(tmrt_path)[[1]], silent = TRUE)
    
    # If it failed, record and skip to next iteration
    if (inherits(mrt, "try-error")) {
      missing_tmrt <- bind_rows(
        missing_tmrt,
        tibble(
          tile = t,
          name = tmrt_name
        )
      )
      next
    }
    
    utci <- create_utci(mrt, time, met)
    
    stats_df <- global(
      utci,
      fun = function(x, ...) {
        c(
          min    = min(x, ...),
          mean   = mean(x, ...),
          median = median(x, ...),
          max    = max(x, ...)
        )
      },
      na.rm = TRUE
    )
    
    # Append to running stats tibble
    tile_stats <- bind_rows(
      tile_stats,
      tibble(
        tile   = t,
        time   = time,
        min    = stats_df$min,
        mean   = stats_df$mean,
        median = stats_df$median,
        max    = stats_df$max
      )
    )
    
    # Otherwise, proceed as usual
    
    out_path <- glue("{bucket}/{baseline_folder}/{t}/tcm_results/met_era5_hottest_days/UTCI_{date}_{time}D.tif")
    write_s3(utci, out_path)
    
    ensure_s3_prefix(bucket, prefix = glue("{baseline_folder}/{t}/ccl_layers/"))
    out_path2 <- glue("{bucket}/{baseline_folder}/{t}/ccl_layers/utci-{time}__{infra_id}__{scenario_id}.tif")
    write_s3(utci, out_path2)
    
  }
}

write_s3(tile_stats, glue("{bucket}/{baseline_folder}/utci-tile-statistics.csv"))

# Get list of tiles without UTCI ------------------------------------------

# s3_key_exists <- function(bucket, key) {
#   tryCatch({
#     s3$head_object(Bucket = bucket, Key = key)
#     TRUE
#   }, error = function(e) FALSE)
# }
# 
# check_utci_for_tile <- function(tile) {
#   base <- glue("{baseline_folder}/{tile}/tcm_results/met_era5_hottest_days/")
#   keys <- c("utci-1200.tif", "utci-1500.tif", "utci-1800.tif")
#   full <- glue("{base}/{keys}")
#   has  <- map_lgl(full, ~ s3_key_exists(bucket, .x))
#   tibble(
#     tile = tile,
#     has_1200 = has[1],
#     has_1500 = has[2],
#     has_1800 = has[3],
#     has_all  = all(has)
#   )
# }
# 
# utci_index <- map_dfr(tiles, check_utci_for_tile)
# 
# tiles_without <- utci_index %>%
#   filter(!has_all) %>%
#   pull(tile)
# 
