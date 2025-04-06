# Process trees -----------------------------------------------------------

process_trees <- function(tree_raster, save_files = TRUE){
  
  tree_height <- tree_raster
  
  # Get tree height and area for local maxima
  # locate trees
  ttops <- locate_trees(tree_height, lmf(3)) 
  
  # segment crowns
  crowns <- dalponte2016(tree_height, ttops)()
  names(crowns) <- "treeID"
  
  writeRaster(crowns, here(scenario_path, "existing-tree-crowns.tif"))
  
  # crown vectors
  crown_vectors <- crowns %>% 
    as.polygons() %>% 
    st_as_sf() %>% 
    left_join(st_drop_geometry(ttops), by = "treeID") %>% 
    rename(height = Z) 
  
  ttops <- ttops %>% 
    rename(height = Z) %>% 
    st_zm(drop = TRUE, what = "ZM")
  
  # Probabilities for tree height classes
  tree_structure <- tibble(
    tree_classes = c("small", "medium", "large"),
    tree_heights = c(ceiling(quantile(crown_vectors$height, 0.25)),
                     ceiling(quantile(crown_vectors$height, 0.50)),
                     ceiling(quantile(crown_vectors$height, 0.75))),
    weights = c(0.25, 0.50, 0.25)
  )
  
  # Save tree data
  if (save_files){
    save(ttops, crown_vectors, tree_structure, 
         file = here(scenario_path, "tree-vars.RData"))
  }
  
}
