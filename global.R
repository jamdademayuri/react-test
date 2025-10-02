# global.R
library(dplyr)
library(purrr)
library(tibble)
library(sparklyr)
library(DT)
library(ggplot2)
library(rlang)
library(survival)
library(survminer)
library(tidyr)
library(writexl)

# Increase maximum file upload size in Shiny
options(shiny.maxRequestSize = 50 * 1024^2) # 50 MB

# Entities and covariates to ignore in analysis
ignored_entities   <- c("Metadata", "program", "project")
ignored_covariates <- c("id", "dst_id", "dst_name", "created_datetime", "updated_datetime")


# Connect to Spark if not already connected
connect_spark_if_needed <- function(sc_current) {
  tryCatch({
    if (!is.null(sc_current)) {
      ok <- tryCatch(spark_connection_is_open(sc_current), error = function(e) FALSE)
      if (ok) return(sc_current)
    }
    spark_connect(
      master     = "local",
      spark_home = "/opt/spark-3.5.0",
      packages   = c("org.apache.spark:spark-avro_2.12:3.5.0")
    )
  }, error = function(e) {
    message("Error connecting to Spark: ", e$message)
    NULL
  })
}

# Read AVRO file into Spark, split by entity types into separate data frames
prepare_dfs_from_avro <- function(sc_conn, path, output = NULL) {
  tryCatch({
    # Read AVRO into Spark
    sdf <- spark_read_avro(
      sc_conn,
      name = paste0("avro_", as.integer(Sys.time())),
      path = path,
      overwrite = TRUE
    )
    
    # Collect a few rows locally to check column names
    df_sample <- sdf %>% head(5) %>% collect()
    required_cols <- c("id", "name", "object", "relations")
    
    # If any required column is missing, show error in UI and stop
    if (!all(required_cols %in% names(df_sample))) {
      if (!is.null(output)) {
        output$filePath <- renderText({
          "Error: Invalid file format. Required columns: id, name, object, relations"
        })
      }
      stop("Invalid file format")  # Stop further processing
    }
    
    # If valid, split by entity type
    entity_types <- sdf %>% distinct(name) %>% collect() %>% pull(name)
    dfs <- map(entity_types, ~ sdf %>% filter(name == .x)) %>% set_names(entity_types)
    
    list(spark_df = sdf, dfs = dfs, entity_types = entity_types)
    
  }, error = function(e) {
    message("Error preparing DFS from AVRO: ", e$message)
    if (!is.null(output)) {
      output$filePath <- renderText({
        paste("Error:", e$message)
      })
    }
    list(spark_df = NULL, dfs = list(), entity_types = character(0))
  })
}


#  Flatten nested entity object into a clean data frame
flatten_entity_df <- function(entity_sdf, output = NULL) {
  tryCatch({
    if (is.null(entity_sdf)) return(tibble::tibble())

    sel_try <- tryCatch({
      entity_sdf %>% dplyr::select(id, name, object, relations)
    }, error = function(e) {
      entity_sdf
    })

    df_local <- tryCatch(sel_try %>% sparklyr::collect(), error = function(e) {
      sel_try %>% dplyr::collect()
    })

    if (nrow(df_local) == 0) return(tibble::tibble())

    # Ensure list-columns exist so pmap won't fail
    if (!"object" %in% names(df_local)) df_local$object <- rep(list(list()), nrow(df_local))
    if (!"relations" %in% names(df_local)) df_local$relations <- rep(list(list()), nrow(df_local))

    # Build rows in R without repeated df[i,] slicing
    rows_list <- purrr::pmap(
      list(
        id = df_local$id,
        top_name = df_local$name,
        obj_raw = df_local$object,
        rel_raw = df_local$relations
      ),
      function(id, top_name, obj_raw, rel_raw) {
        obj <- if (length(obj_raw) > 0 && !is.null(obj_raw[[1]])) obj_raw[[1]] else list()

        if (!is.null(obj$state) && is.list(obj$state) && !is.null(obj$state$member0)) {
          obj$state <- obj$state$member0
        }

        if (!is.null(obj$name)) {
          obj[[paste0(top_name, "_name")]] <- obj$name
          obj$name <- NULL
        }

        obj_flat <- purrr::map(obj, function(x) {
          if (is.null(x)) return(NA_character_)
          if (is.list(x)) {
            x_un <- unlist(x, use.names = FALSE)
            if (length(x_un) == 0) return(NA_character_)
            return(paste(as.character(x_un), collapse = ";"))
          } else if (length(x) > 1) {
            return(paste(as.character(x), collapse = ";"))
          } else {
            return(as.character(x))
          }
        })
        dst_id <- NA_character_
        dst_name <- NA_character_
        if (length(rel_raw) > 0 && !is.null(rel_raw[[1]])) {
          rel <- rel_raw[[1]]
          if (!is.null(rel$dst_id))   dst_id   <- as.character(rel$dst_id)
          if (!is.null(rel$dst_name)) dst_name <- as.character(rel$dst_name)
        }

        out_list <- c(
          list(id = id, name = top_name),
          obj_flat,
          list(dst_id = dst_id, dst_name = dst_name)
        )

        tibble::as_tibble(out_list)
      }
    )

    final_df <- dplyr::bind_rows(rows_list)

    clean_string <- function(x) {
      x <- gsub("_2d", "-", x, fixed = TRUE)
      x <- gsub("_2f", "/", x, fixed = TRUE)
      x <- gsub("_20", " ", x, fixed = TRUE)
      x <- gsub(" _", " ", x, fixed = TRUE)
      x <- gsub(" +", " ", x)
      x <- gsub("-_", "-", x, fixed = TRUE)
      x <- gsub("- ", "-", x, fixed = TRUE)
      x <- gsub("_28_", "(", x, fixed = TRUE)
      x <- gsub("_2e_", ".", x, fixed = TRUE)
      x <- gsub("_29_", ")", x, fixed = TRUE)
      x <- gsub("/_", "_", x, fixed = TRUE)
      trimws(x)
    }

    # Apply cleaning only to character columns
    final_df <- final_df %>%
      dplyr::mutate(dplyr::across(where(is.character), clean_string))

    # Optionally show status in UI (preserve original behaviour)
    if (!is.null(output)) {
      try({
        output$errorMsg <- renderText({
          paste0("Flattened ", nrow(final_df), " records for ", paste(unique(df_local$name), collapse = ", "))
        })
      }, silent = TRUE)
    }

    final_df
  }, error = function(e) {
    message("Error flattening entity: ", e$message)
    tibble::tibble()
  })
}

# UI for a single covariate selection card
covariateCardUI <- function(id, choices = NULL) {
  tryCatch({
    ns <- NS(id)
    tags$div(
      id = id,
      class = "card",
      style = "background:#fff;border-radius:14px;box-shadow:0 6px 18px rgba(0,0,0,.06);padding:14px;margin-bottom:12px;",
      div(
        style = "display:flex;justify-content:space-between;align-items:center;",
        h4(paste("Covariate", gsub("\\D", "", id)), style = "margin:0;"),
        actionButton(ns("delete"), "Remove", class = "btn btn-sm btn-outline-danger")
      ),
      selectInput(
        ns("col"),
        "Select Covariate",
        choices = c("", choices %||% character(0)),
        selected = "",
        width = "100%"
      ),
      uiOutput(ns("vals_ui")) # Placeholder for dynamic value selection
    )
  }, error = function(e) {
    message("Error creating covariate card UI: ", e$message)
    NULL
  })
}

# Server logic for a single covariate selection card
covariateCardServer <- function(id, data_reactive, removeCallback) {
  tryCatch({
    moduleServer(id, function(input, output, session) {
      
      # Update covariate choices whenever data changes
      observeEvent(data_reactive(), {
        df <- data_reactive()
        if (is.null(df)) return()
        choices <- setdiff(names(df), ignored_covariates)
        prev <- isolate(input$col) %||% ""
        last <- session$userData$.lastChoices %||% character(0)
        
        if (!identical(sort(choices), sort(last))) {
          session$userData$.lastChoices <- choices
          selected <- if (nzchar(prev) && prev %in% choices) prev else ""
          updateSelectInput(session, "col", choices = choices, selected = selected)
        }
      }, ignoreInit = TRUE)
      
      # Render UI for selecting statistics or categorical values
      output$vals_ui <- renderUI({
        req(input$col, nzchar(input$col), data_reactive())
        col_data <- data_reactive()[[input$col]]
        suppressWarnings({
          numeric_like <- !all(is.na(as.numeric(col_data)))
        })
        if (numeric_like) {
          checkboxGroupInput(session$ns("vals"), "Summary Statistic(s):",
                             choices = c("Mean", "Mode"),
                             selected = character(0),
                             inline = TRUE)
        } else {
          vals <- sort(unique(col_data))
          checkboxGroupInput(session$ns("vals"), "Values to show (%):",
                             choices = vals,
                             selected = character(0))
        }
      })
      
      # Remove this covariate card when delete button is pressed
      observeEvent(input$delete, {
        removeCallback(id)
      })
    })
  }, error = function(e) {
    message("Error in covariate card server: ", e$message)
  })
}