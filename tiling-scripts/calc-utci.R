system("aws sso login --profile cities-data-dev")

s3 <- paws::s3()
bucket <- "wri-cities-tcm"
aws_http <- "https://wri-cities-tcm.s3.us-east-1.amazonaws.com"

# Process baseline tile data ----------------------------------------------
city <- "ZAF-Cape_Town"
aoi_name <- "urban_extent"
city_folder <- glue("city_projects/{city}/{aoi_name}")

baseline_name <- "wholecity_start"
baseline_folder <- glue("{city_folder}/scenarios/baseline/{baseline_name}")

# Get tile ids
# prefix helper: ensure it ends with a single slash
norm_prefix <- function(x) sub("/+$", "/", paste0(sub("^/+", "", x), "/"))

list_tiles <- function(bucket, prefix) {
  prefix <- norm_prefix(prefix)
  tiles <- character()
  token <- NULL
  
  repeat {
    resp <- s3$list_objects_v2(
      Bucket = bucket,
      Prefix = prefix,
      Delimiter = "/",                  # <- needed to get folder-like prefixes
      ContinuationToken = token
    )
    
    # 1) Folders directly under prefix (CommonPrefixes)
    if (!is.null(resp$CommonPrefixes)) {
      from_prefixes <- vapply(resp$CommonPrefixes, `[[`, character(1), "Prefix") |>
        basename() |>                     # e.g., "tile_00383/"
        str_remove("/$") |>
        str_extract("tile_\\d{5}")
      tiles <- c(tiles, from_prefixes)
    }
    
    # 2) Also parse any object keys returned on this page (in case tiles are deeper)
    if (!is.null(resp$Contents)) {
      from_keys <- vapply(resp$Contents, `[[`, character(1), "Key") |>
        str_extract("tile_\\d{5}")
      tiles <- c(tiles, from_keys)
    }
    
    if (isTRUE(resp$IsTruncated)) {
      token <- resp$NextContinuationToken
    } else break
  }
  
  tiles <- tiles[!is.na(tiles)] |> unique() |> sort()
  tiles
}

tiles <- list_tiles(bucket, baseline_folder)

source(here("utils", "utci.R"))

met <- read_csv(glue("{aws_http}/{baseline_folder}/metadata/met_files/met_era5_hottest_days.csv"),
                skip = 2)

year <- met$Year %>% unique()
month <- met$Month %>% as.integer() %>% str_pad(2, pad = "0") %>% unique()
day <- met$Day %>% unique()

date_ymd <- glue("{year}-{month}-{day}")
date_doy <- ymd(date_ymd)

date <- glue("{met$Year}_{date_doy}")

for (t in tiles_without){
  print(t)
  for (time in c(1200, 1500, 1800)){

    mrt <- rast(glue("{aws_http}/{baseline_folder}/{t}/tcm_results/met_era5_hottest_days/Tmrt_2022_22_{time}D.tif"))
    utci <- create_utci(mrt, time, met)
    write_s3(utci, glue("{bucket}/{baseline_folder}/{t}/tcm_results/met_era5_hottest_days/utci-{time}.tif"))
  }
 

}


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
