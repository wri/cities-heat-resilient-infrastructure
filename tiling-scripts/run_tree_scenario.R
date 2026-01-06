# tiling-scripts/run_tree_scenario.R
# Runner helpers for tree (street) scenarios.
#
# Assumes you have already set:
# - bucket, aws_http
# - city_folder, open_urban_aws_http
# - baseline_folder, scenario_folder
# - tiles (character vector of tile IDs, e.g. "tile_00001")
#
# And that you have sourced:
# - tiling-scripts/utils.R   (for write_s3(), ensure_s3_prefix(), list_tiles(), etc.)
# - tiling-scripts/trees-functions.R (for baseline_processing(), etc.)

run_tree_scenario <- function(
  tiles,
  bucket,
  aws_http,
  baseline_folder,
  scenario_folder,
  city_folder,
  open_urban_aws_http,
  parallel = FALSE,
  workers = max(1, future::availableCores() - 1)
) {

  # Make required objects visible to functions that were written expecting globals.
  # (baseline_processing() in trees-functions.R uses these names.)
  list2env(
    list(
      bucket = bucket,
      aws_http = aws_http,
      baseline_folder = baseline_folder,
      scenario_folder = scenario_folder,
      city_folder = city_folder,
      open_urban_aws_http = open_urban_aws_http
    ),
    envir = .GlobalEnv
  )

  if (length(tiles) == 0) {
    message("No tiles provided; nothing to do.")
    return(invisible(list()))
  }

  if (isTRUE(parallel)) {
    # Parallel execution (optional)
    if (!requireNamespace("future", quietly = TRUE) ||
        !requireNamespace("future.apply", quietly = TRUE)) {
      stop("parallel=TRUE requires the 'future' and 'future.apply' packages.")
    }
    if (!requireNamespace("progressr", quietly = TRUE)) {
      # still run parallel, just without nice progress
      future::plan(future::multisession, workers = workers)
      on.exit(future::plan(future::sequential), add = TRUE)

      res <- future.apply::future_lapply(tiles, baseline_processing)
      return(invisible(res))
    }

    # parallel + progress
    future::plan(future::multisession, workers = workers)
    on.exit(future::plan(future::sequential), add = TRUE)

    progressr::with_progress({
      p <- progressr::progressor(along = tiles)
      res <- future.apply::future_lapply(
        tiles,
        function(t) {
          p(t)
          baseline_processing(t)
        }
      )
      res
    }) -> res

    return(invisible(res))
  }

  # Serial execution (default) — simplest + most robust
  res <- lapply(tiles, baseline_processing)
  invisible(res)
}
