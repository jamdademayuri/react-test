# Main entry point for the Shiny app
# Load libraries
tryCatch({
  required_pkgs <- c(
    "sparklyr", "dplyr", "data.table", "purrr", "tibble",
    "writexl", "ggplot2", "rlang", "survival", "survminer",
    "ggpubr", "tidyr", "DT", "shinythemes", "shiny", "shinyjs"
  )
  
  # Install missing packages only
  missing_pkgs <- setdiff(required_pkgs, rownames(installed.packages()))
  if (length(missing_pkgs) > 0) {
    install.packages(missing_pkgs)
  }
  
  # Load all packages
  invisible(lapply(required_pkgs, library, character.only = TRUE))
  
}, error = function(e) {
  message("An error occurred: ", e$message)
})

tryCatch({
  source("functions.R")
  source("userinterface.R")
  if (!exists("userinterface")) stop("userinterface.R must define a variable 'ui'")
  source("backend.R")
  if (!exists("backend")) stop("backend.R must define a variable 'server'")
  shinyApp(ui = ui, server = server)
}, error = function(e) {
  message("An error occurred while starting the Shiny app:")
  message(e$message)
})