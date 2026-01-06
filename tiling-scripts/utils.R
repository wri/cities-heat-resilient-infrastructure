# Write to s3 
write_s3 <- function(obj, file_path) {
  s3_uri <- glue("s3://{file_path}")
  
  # parse bucket/key
  p   <- sub("^s3://", "", s3_uri)
  bkt <- sub("/.*", "", p)
  key <- sub("^[^/]+/", "", p)
  ext <- tolower(tools::file_ext(key))
  tmp <- tempfile(fileext = paste0(".", ext))
  
  # write to a local temp file
  if (inherits(obj, "sf") || inherits(obj, "sfc")) {
    st_write(obj, tmp, delete_dsn = TRUE, quiet = TRUE)
    ctype <- if (ext %in% c("geojson","json")) "application/geo+json" else "application/octet-stream"
    
  } else if (inherits(obj, "SpatRaster")) {
    writeRaster(obj, tmp, overwrite = TRUE)
    ctype <- if (ext %in% c("tif","tiff")) "image/tiff" else "application/octet-stream"
    
  } else if (inherits(obj, c("RasterLayer", "RasterStack", "RasterBrick"))) {
    # for objects from the {raster} package
    raster::writeRaster(obj, tmp, overwrite = TRUE)
    ctype <- if (ext %in% c("tif","tiff")) "image/tiff" else "application/octet-stream"
    
  } else if (ext == "csv") {                  
    write_csv(obj, file = tmp)
    ctype <- "application/octet-stream"
    
  } else {
    stop("Unsupported extension: ", ext)
  }
  
  # upload
  raw <- readBin(tmp, "raw", file.info(tmp)$size)
  s3$put_object(Bucket = bkt, Key = key, Body = raw, ContentType = ctype)
  unlink(tmp)
  invisible(s3_uri)
}

# Make sure an S3 prefix exists
ensure_s3_prefix <- function(bucket, prefix){
  s3 <- paws::s3()
  # normalize: single trailing slash, no leading slash
  prefix <- str_replace_all(prefix, "/+", "/")
  prefix <- str_remove(prefix, "^/")
  if (!str_ends(prefix, "/")) prefix <- paste0(prefix, "/")
  # create zero-byte object to materialize the "folder"
  s3$put_object(
    Bucket = bucket,
    Key    = prefix,
    Body   = raw(0)  # zero bytes
  )
  invisible(prefix)
}

# Copy files in S3
s3_copy_vec <- function(from, to,
                            from_bucket,
                            to_bucket,
                            overwrite = TRUE,
                            quiet = FALSE,
                            dryrun = FALSE) {
  stopifnot(length(from) == length(to))
  
  norm_key <- function(k) sub("^/+", "", k)  # remove leading /
  mk_s3 <- function(bucket, key) {
    sprintf("s3://%s/%s", sub("^s3://", "", sub("/+$", "", bucket)), norm_key(key))
  }
  
  results <- vector("list", length(from))
  
  for (i in seq_along(from)) {
    src <- mk_s3(from_bucket, from[i])
    dst <- mk_s3(to_bucket,   to[i])
    
    # aws s3 cp behavior:
    # - By default it WILL overwrite.
    # - To avoid overwriting, we do a HEAD check first via `aws s3api head-object`.
    if (!overwrite) {
      head_cmd <- c("aws", "s3api", "head-object",
                    "--bucket", sub("^s3://", "", sub("/+$", "", to_bucket)),
                    "--key", norm_key(to[i]))
      head_out <- suppressWarnings(system2(head_cmd[1], head_cmd[-1],
                                           stdout = TRUE, stderr = TRUE))
      if (!quiet) message("HEAD ", dst)
      if (attr(head_out, "status") == 0) {
        if (!quiet) message("SKIP (exists): ", dst)
        results[[i]] <- list(ok = TRUE, skipped = TRUE, src = src, dst = dst, msg = "exists")
        next
      }
    }
    
    cmd <- c("aws", "s3", "cp", src, dst)
    if (dryrun) cmd <- c(cmd, "--dryrun")
    if (quiet)  cmd <- c(cmd, "--only-show-errors")
    
    if (!quiet) message("CP ", src, " -> ", dst)
    
    out <- suppressWarnings(system2(cmd[1], cmd[-1], stdout = TRUE, stderr = TRUE))
    status <- attr(out, "status"); if (is.null(status)) status <- 0
    
    results[[i]] <- list(
      ok = (status == 0),
      skipped = FALSE,
      src = src,
      dst = dst,
      status = status,
      output = paste(out, collapse = "\n")
    )
    
    if (!quiet && status != 0) {
      message("ERROR (status ", status, "):\n", results[[i]]$output)
    }
  }
  
  # Return a simple logical vector invisibly + attach details
  ok <- vapply(results, function(x) isTRUE(x$ok), logical(1))
  attr(ok, "details") <- results
  ok
}



# Function to load and merge rasters from a list of paths
load_and_merge <- function(paths) {
  if (length(paths) == 0) {
    stop("No raster paths provided in plantable_paths")
  }
  
  rasters <- lapply(paths, rast)
  
  if (length(rasters) == 1) {
    return(rasters[[1]])   # just return the single raster
  } else {
    return(do.call(merge, c(rasters)))  # merge multiple rasters
  }
}

# Get tile ids
# prefix helper: ensure it ends with a single slash
norm_prefix <- function(x) sub("/+$", "/", paste0(sub("^/+", "", x), "/"))

# list_tiles <- function(bucket, prefix) {
#   prefix <- norm_prefix(prefix)
#   tiles <- character()
#   token <- NULL
#   
#   repeat {
#     resp <- s3$list_objects_v2(
#       Bucket = bucket,
#       Prefix = prefix,
#       Delimiter = "/",                  # <- needed to get folder-like prefixes
#       ContinuationToken = token
#     )
#     
#     # 1) Folders directly under prefix (CommonPrefixes)
#     if (!is.null(resp$CommonPrefixes)) {
#       from_prefixes <- vapply(resp$CommonPrefixes, `[[`, character(1), "Prefix") |>
#         basename() |>                     # e.g., "tile_00383/"
#         str_remove("/$") |>
#         str_extract("tile_\\d{5}")
#       tiles <- c(tiles, from_prefixes)
#     }
#     
#     # 2) Also parse any object keys returned on this page (in case tiles are deeper)
#     if (!is.null(resp$Contents)) {
#       from_keys <- vapply(resp$Contents, `[[`, character(1), "Key") |>
#         str_extract("tile_\\d{5}")
#       tiles <- c(tiles, from_keys)
#     }
#     
#     if (isTRUE(resp$IsTruncated)) {
#       token <- resp$NextContinuationToken
#     } else break
#   }
#   
#   tiles <- tiles[!is.na(tiles)] |> unique() |> sort()
#   tiles
# }

list_tiles <- function(folder_s3_url, profile = "cities-data-dev") {
  # Normalize: ensure trailing slash so `aws s3 ls` treats it as a "folder"
  folder_s3_url <- sub("/?$", "/", folder_s3_url)
  
  lines <- system2(
    "aws",
    c("s3", "ls", folder_s3_url, "--profile", profile),
    stdout = TRUE,
    stderr = TRUE
  )
  
  tile_lines <- grep("^[[:space:]]*PRE[[:space:]]+tile_[0-9]+/", lines, value = TRUE)
  sub("^[[:space:]]*PRE[[:space:]]+(tile_[0-9]+)/$", "\\1", tile_lines)
}

# Find the yyyy_dd stamp used in Shadow_yyyy_dd_1200D.tif
find_shadow_stamp <- function(bucket, baseline_folder, tile_id, profile = "cities-data-dev") {
  # Folder containing the Shadow rasters for a tile
  s3_dir <- sprintf("s3://%s/%s/%s/tcm_results/met_era5_hottest_days/", bucket, baseline_folder, tile_id)
  
  # List files in the folder
  lines <- system2(
    "aws",
    c("s3", "ls", s3_dir, "--profile", profile),
    stdout = TRUE,
    stderr = TRUE
  )
  
  # Find a matching filename
  m <- regmatches(
    lines,
    regexec("Shadow_(\\d{4}_\\d{1,3})_1200D\\.tif", lines)
  )
  
  stamps <- vapply(m, function(x) if (length(x) >= 2) x[2] else NA_character_, character(1))
  stamps <- unique(stats::na.omit(stamps))
  
  if (length(stamps) == 0) {
    stop("No Shadow_*_1200D.tif files found in: ", s3_dir)
  }
  if (length(stamps) > 1) {
    stop("Multiple yyyy_dd stamps found in ", s3_dir, ": ", paste(stamps, collapse = ", "))
  }
  
  stamps[[1]]
}

