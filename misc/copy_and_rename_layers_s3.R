library(tidyverse)
library(here)

base <- tribble(
  ~ city, ~aoi,
  "ZAF-Cape_Town", "business_district",
  "MEX-Monterrey", "mitras_centro",
  "BRA-Rio_de_Janeiro", "low_emission_zone",
  "IND-Bhopal", "tt_nagar",
  "ZAF-Johannesburg", "jukskei-river"
)

base <- base %>% 
  mutate(src_folder = paste("s3://wri-cities-heat", city, "scenarios/aoi", aoi, sep = "/"),
         tgt_folder = paste("s3://wri-cities-tcm/city_projects", city, aoi, "scenarios", sep = "/"))

paths <- read_csv(here("misc", "layer_names_rename.csv")) %>% 
  mutate(old_path = file.path(`old infraID`, paste(old_layer_name, extension, sep = ".")),
         new_path = case_when(extension == "csv" ~ file.path("metrics", paste(`NEW NAME`, extension, sep = ".")),
                              extension != "csv" ~ file.path(infraID, scenarioID, "tile_00001", paste(`NEW NAME`, extension, sep = ".")))) %>% 
  crossing(base) %>%
  # build the full source path
  mutate(src_path = file.path(src_folder, old_path),
         tgt_path = file.path(tgt_folder, new_path)) %>% 
  filter(!(city %in% c("BRA-Rio_de_Janeiro", "MEX-Monterrey") &
             `old infraID` == "street-trees-achievable-cool-roofs-all")) %>% 
  filter(extension == "csv") 


# Copy files --------------------------------------------------------------

# install.packages("paws") # if needed
library(paws)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(tibble)

s3 <- paws::s3()

parse_s3 <- function(uri) {
  stopifnot(startsWith(uri, "s3://"))
  rest <- sub("^s3://", "", uri)
  bucket <- sub("/.*$", "", rest)
  key <- sub("^[^/]+/", "", rest)
  list(bucket = bucket, key = key)
}

s3_exists <- function(bucket, key) {
  tryCatch({
    s3$head_object(Bucket = bucket, Key = key)
    TRUE
  }, error = function(e) {
    # Safely grab fields; coalesce NULL/length-0 to NA
    code <- tryCatch(e$response$Error$Code, error = function(...) NA_character_)
    if (is.null(code) || length(code) == 0) code <- NA_character_
    
    status <- tryCatch(e$response$ResponseMetadata$HTTPStatusCode, error = function(...) NA_integer_)
    if (is.null(status) || length(status) == 0) status <- NA_integer_
    
    msg <- conditionMessage(e)
    
    # Return FALSE only for clear "not found" signals
    not_found <- (!is.na(code)   && code %in% c("404", "NoSuchKey", "NotFound")) ||
      (!is.na(status) && status == 404) ||
      grepl("404", msg, fixed = TRUE) ||
      grepl("Not Found", msg, fixed = TRUE) ||
      grepl("NoSuchKey", msg, fixed = TRUE)
    
    if (isTRUE(not_found)) {
      return(FALSE)
    }
    
    # Otherwise, rethrow (likely permission or other real error)
    stop(e)
  })
}


s3_copy_safe <- function(src_bucket, src_key, tgt_bucket, tgt_key, dry_run = FALSE) {
  if (dry_run) {
    message(sprintf("[DRY-RUN] copy s3://%s/%s -> s3://%s/%s", src_bucket, src_key, tgt_bucket, tgt_key))
    return(list(ok = TRUE, dry_run = TRUE))
  }
  # If your keys might have spaces or unicode, URL-encode CopySource:
  copy_source <- paste0(src_bucket, "/", src_key)  # encode if needed
  s3$copy_object(Bucket = tgt_bucket, Key = tgt_key, CopySource = copy_source)
  list(ok = TRUE, dry_run = FALSE)
}

# Main driver
s3_copy_from_df <- function(df, dry_run = FALSE, overwrite = TRUE) {
  stopifnot(all(c("src_path","tgt_path") %in% names(df)))
  
  df2 <- df %>%
    mutate(
      src = map(src_path, parse_s3),
      tgt = map(tgt_path, parse_s3),
      src_bucket = map_chr(src, "bucket"),
      src_key    = map_chr(src, "key"),
      tgt_bucket = map_chr(tgt, "bucket"),
      tgt_key    = map_chr(tgt, "key")
    ) %>%
    select(-src, -tgt)
  
  df2 <- df2 %>%
    mutate(
      src_exists = pmap_lgl(list(src_bucket, src_key), ~ s3_exists(..1, ..2)),
      tgt_exists = pmap_lgl(list(tgt_bucket, tgt_key), ~ s3_exists(..1, ..2))
    )
  
  missing_src <- df2 %>% filter(!src_exists) %>%
    select(src_path, tgt_path, src_bucket, src_key)
  
  to_copy <- df2 %>% filter(src_exists) %>%
    { if (overwrite) . else filter(., !tgt_exists) }
  
  copy_log <-
    if (nrow(to_copy) == 0) {
      tibble(src_path = character(), tgt_path = character(),
             ok = logical(), dry_run = logical(),
             skipped_existing = logical())
    } else {
      to_copy %>%
        mutate(
          result = pmap(
            list(src_bucket, src_key, tgt_bucket, tgt_key),
            ~ s3_copy_safe(..1, ..2, ..3, ..4, dry_run = dry_run)
          ),
          ok = map_lgl(result, "ok"),
          dry_run_flag = map_lgl(result, "dry_run"),
          skipped_existing = FALSE
        ) %>%
        select(src_path, tgt_path, ok, dry_run = dry_run_flag, skipped_existing)
    }
  
  # If overwrite = FALSE, also report the ones we intentionally skipped
  skipped_existing <- df2 %>%
    filter(src_exists, tgt_exists, !overwrite) %>%
    transmute(src_path, tgt_path, ok = NA, dry_run = dry_run, skipped_existing = TRUE)
  
  list(
    copied_log = bind_rows(copy_log, skipped_existing),
    missing_src = missing_src
  )
}

system("aws sso login --profile cities-data-dev")
Sys.setenv(AWS_PROFILE = "cities-data-dev",
           AWS_DEFAULT_REGION = "us-east-1",
           AWS_SDK_LOAD_CONFIG = "1")

res <- s3_copy_from_df(paths, dry_run = FALSE, overwrite = TRUE)

# What was copied (or skipped if overwrite=FALSE)
copied <- res$copied_log

# Which sources didn’t exist and need creating
missing <- res$missing_src
