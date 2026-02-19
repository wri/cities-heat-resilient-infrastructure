library(terra)
library(tidyverse)

utci_risk_cat <- function(utci_rast){
  
  # Risk categories
  rcl <- matrix(c(
    -Inf,  -40, 1,   # extreme cold stress
    -40,   -27, 2,   # very strong cold stress
    -27,   -13, 3,   # strong cold stress
    -13,     0, 4,   # moderate cold stress
    0,     9, 5,   # slight cold stress
    9,    26, 6,   # no thermal stress
    26,    32, 7,   # moderate heat stress
    32,    38, 8,   # strong heat stress
    38,    46, 9,   # very strong heat stress
    46,   Inf,10    # extreme heat stress
  ), ncol = 3, byrow = TRUE)
  
  utci_levels <- data.frame(
    value = 1:10,
    category = c(
      "extreme cold stress",
      "very strong cold stress",
      "strong cold stress",
      "moderate cold stress",
      "slight cold stress",
      "no thermal stress",
      "moderate heat stress",
      "strong heat stress",
      "very strong heat stress",
      "extreme heat stress"
    )
  )
  
  utci_cat <- classify(utci_rast, rcl, right = FALSE)
  levels(utci_cat) <- utci_levels
  
  return(utci_cat)
  
}

