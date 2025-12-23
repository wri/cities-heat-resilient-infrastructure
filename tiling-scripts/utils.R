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
s3_copy_vec <- function(from, to, bucket, overwrite = TRUE) {
  
  norm <- function(k) sub("^/+","", sub("/+$","", k))  # strip leading/trailing "/"
  vapply(seq_along(from), function(i) {
    src <- norm(from[i]); dst <- norm(to[i])
    if (!overwrite) {
      exists <- tryCatch({ s3$head_object(Bucket = bucket, Key = dst); TRUE },
                         error = function(e) FALSE)
      if (exists) return(FALSE)
    }
    tryCatch({
      s3$copy_object(
        Bucket = bucket,
        Key = dst,
        CopySource = paste0("/", bucket, "/", src),
        MetadataDirective = "COPY"
      )
      TRUE
    }, error = function(e) FALSE)
  }, logical(1))
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
