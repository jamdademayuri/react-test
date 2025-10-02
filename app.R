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
  source("global.R")
  source("ui.R")
  if (!exists("ui")) stop("ui.R must define a variable 'ui'")
  source("server.R")
  if (!exists("server")) stop("server.R must define a variable 'server'")
  shinyApp(ui = ui, server = server)
}, error = function(e) {
  message("An error occurred while starting the Shiny app:")
  message(e$message)
})