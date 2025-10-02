# server.R
library(shiny)
library(shinythemes)
library(shinyjs)
library(dplyr)
library(purrr)
library(tibble)
library(sparklyr)
library(DT)
library(ggplot2)
library(survival)
library(lubridate)
library(survminer)
library(readxl)
library(utils)
library(writexl)

if (!exists("ignored_entities"))   ignored_entities   <- character(0)
if (!exists("ignored_covariates")) ignored_covariates <- character(0)
if (!exists("%||%")) `%||%` <- function(a, b) if (!is.null(a)) a else b

server <- function(input, output, session) {
  uploaded_subject_ids_val <- reactiveVal(NULL)
  uploaded_subject_file_name_val <- reactiveVal(NULL)
  
  # Initial render of the upload input
  output$upload_excel_ui <- renderUI({
    fileInput("upload_excel", "Choose Excel / CSV / TXT", 
              accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv"))
  })
  
  
  # Holds any uploaded Subject IDs parsed from file (xlsx/csv/tsv/txt)
  uploaded_subject_ids_val <- reactiveVal(NULL)
  # Holds the basename (sans extension) of the uploaded ID file for labeling
  uploaded_subject_file_name_val <- reactiveVal(NULL)
  
  # Core reactive state for the app
  rv <- reactiveValues(
    sc = NULL,            
    path = NULL,
    dfs = NULL,
    entity_types = character(0),
    selected_entities = character(0),
    flattened = list(),
    final_df = NULL,
    prep_done = FALSE,
    dist_done = FALSE,
    os_done = FALSE,
    efs_done = FALSE,
    dist_data = NULL,
    os_fit = NULL,
    efs_fit = NULL,
    table1_done = FALSE
  )
  
  # Session utilities
  observeEvent(input$reset_btn, { session$reload() })
  output$progress_steps <- renderUI({
    tags$div(
      class = "muted",
      tags$div(
        class = paste("step-badge", if (!is.null(rv$dfs)) "done"),
        "File Loaded"
      ),
      tags$div(
        class = paste("step-badge", if (length(rv$selected_entities) > 0) "done"),
        "Entities Chosen"
      ),
      tags$div(
        class = paste("step-badge", if (rv$prep_done) "done"),
        "Prepare Data"
      ),
      tags$div(
        class = paste("step-badge", if (rv$dist_done) "done", if (!rv$prep_done) "locked"),
        "Distribution"
      ),
      # CHANGED: OS shows locked only if prep not done (no dependency on Distribution)
      tags$div(
        class = paste("step-badge", if (rv$os_done) "done", if (!rv$prep_done) "locked"),
        "Overall Survival"
      ),
      # CHANGED: EFS shows locked only if prep not done (no dependency on OS)
      tags$div(
        class = paste("step-badge", if (rv$efs_done) "done", if (!rv$prep_done) "locked"),
        "Event-Free Survival"
      ),
      # Keep Summary label; unlocked after prep for consistency
      tags$div(
        class = paste("step-badge", if (rv$efs_done) "done", if (!rv$prep_done) "locked"),
        "Summary Table"
      )
    )
  })
  
  observe({
    toggleState("dist_submit", rv$prep_done)
    # CHANGED: Enable survival buttons right after prep (no Distribution/OS dependency)
    toggleState("overallSurvivalBtn", rv$prep_done)
    toggleState("eventFreeSurvivalBtn", rv$prep_done)
    toggleState("apply_filters", rv$prep_done)
    # CHANGED: Allow navigating to Survival from Distribution area after prep
    toggleState("go_surv_from_dist", rv$prep_done)
    # Keep existing chaining for these nav buttons as before
    toggleState("go_efs", rv$prep_done)
    toggleState("go_table1",rv$prep_done)
  })
  
  # Upload AVRO 
  observeEvent(input$avroFile, {
    req(input$avroFile)
    
    output$errorMsg <- renderText("")
    
    rv$path <- {
      tgt <- file.path("/home/rstudio", input$avroFile$name)
      success <- file.copy(input$avroFile$datapath, tgt, overwrite = TRUE)
      
      output$filePath <- renderText({
        if (success) paste("File saved to:", tgt) else "Error: Failed to save uploaded file."
      })
      
      if (!success) return(NULL)
      tgt
    }
    
    if (is.null(rv$path)) return()
    
    tryCatch({
      rv$sc <- connect_spark_if_needed(rv$sc)
    }, error = function(e) {
      output$filePath <- renderText(paste("Error connecting to Spark:", conditionMessage(e)))
      return()
    })
    
    tryCatch({
      prep <- prepare_dfs_from_avro(rv$sc, rv$path)
      if (is.null(prep$spark_df)) {
        output$filePath <- renderText("Error: Invalid file format. Required columns: id, name, object, relations")
        return()
      }
      
      rv$dfs <- prep$dfs
      rv$entity_types <- prep$entity_types
      
      # Reset downstream state on new upload
      rv$prep_done <- FALSE
      rv$dist_done <- FALSE
      rv$os_done   <- FALSE
      rv$efs_done  <- FALSE
      rv$final_df  <- NULL
      rv$flattened <- list()
      
      # Clear any previously uploaded Subject IDs and filename
      uploaded_subject_ids_val(NULL)
      uploaded_subject_file_name_val(NULL)
      
    }, error = function(e) {
      output$filePath <- renderText(paste("Error reading AVRO:", conditionMessage(e)))
      return()
    })
    
    others <- setdiff(
      rv$entity_types,
      c("subject", "person", "timing", "survival_characteristic", ignored_entities)
    )
    output$entity_picker <- renderUI({
      if (length(others) == 0) {
        div(class = "muted", "No optional entities found.")
      } else {
        checkboxGroupInput(
          "selected_entities",
          "Optional entities to include:",
          choices  = sort(others),
          selected = rv$selected_entities,
          width    = "100%"
        )
      }
    })
  })
  
  observe({
    rv$selected_entities <- input$selected_entities %||% character(0)
  })

  
  observeEvent(input$selected_entities, {
    others <- setdiff(
      rv$entity_types,
      c("subject", "person", "timing", "survival_characteristic", ignored_entities)
    )
  })
  
  #  PREPARE: Flatten required + selected entities; merge 
  observeEvent(input$prep_btn, {
    req(rv$dfs)
    output$errorMsg <- renderText("")
    
    withProgress(message = "Preparing data", value = 0, {
      needed <- c("subject", "person")
      if (!all(needed %in% names(rv$dfs))) {
        output$errorMsg <- renderText("Required entities 'subject' and 'person' not found.")
        return()
      }
      
      incProgress(0.1, detail = "Flattening subject & person")
      subject_df <- flatten_entity_df(rv$dfs[["subject"]], output)
      person_df  <- flatten_entity_df(rv$dfs[["person"]], output)
      rv$flattened[["subject"]] <- subject_df
      rv$flattened[["person"]]  <- person_df
      
      for (sp in c("timing", "survival_characteristic")) if (sp %in% names(rv$dfs)) {
        incProgress(0.12, detail = paste("Flattening", sp))
        rv$flattened[[sp]] <- flatten_entity_df(rv$dfs[[sp]], output)
      }
      
      merged_df <- subject_df %>%
        left_join(person_df, by = c("dst_id" = "id"), suffix = c("_subject", "_person"))
      
      sel <- rv$selected_entities
      if (length(sel) > 0) {
        step <- 0.6 / length(sel)
        for (i in seq_along(sel)) {
          ent <- sel[i]
          incProgress(0.25 + i * step, detail = paste("Flattening", ent))
          flat_df <- flatten_entity_df(rv$dfs[[ent]], output)
          rv$flattened[[ent]] <- flat_df
          merged_df <- merged_df %>%
            left_join(flat_df, by = c("id" = "dst_id"), suffix = c("", paste0("_", ent)))
        }
      }
      
      rv$final_df <- merged_df
      try(write_xlsx(merged_df, "final_merged.xlsx"), silent = TRUE)
      incProgress(1, detail = "Done")
    })
    
    # Columns to ignore by prefix
    ignored_prefixes <- c("id", "dst_id", "dst_name", "created_datetime", "updated_datetime")
    
    # Keep only columns that do NOT start with ignored prefixes
    dist_columns <- names(rv$final_df)[
      !vapply(names(rv$final_df), function(col) any(startsWith(col, ignored_prefixes)), logical(1))
    ]
    
    # Initialize vectors
    cat_cols <- c()
    num_cols <- c()
    
    # Loop through columns to classify
    for (col in dist_columns) {
      vec <- rv$final_df[[col]]
      
      # Flatten list columns if any
      if (is.list(vec)) vec <- sapply(vec, function(x) if (length(x) > 1) paste0(x, collapse=";") else as.character(x))
      vec_chr <- as.character(vec)
      
      # Determine if numeric-like
      numeric_prop <- sum(!is.na(suppressWarnings(as.numeric(vec_chr)))) / max(1, sum(nzchar(vec_chr)))
      if (numeric_prop >= 0.9) {
        num_cols <- c(num_cols, col)
      } else {
        cat_cols <- c(cat_cols, col)
      }
    }
    
    # Update dropdown with categorical columns
    updateSelectInput(session, "dist_col", choices = sort(cat_cols))
    
    
    
    rv$prep_done <- TRUE
    showNotification("Data prepared. You can now run Distribution, Survival, EFS, or Table 1.", type = "message")
    updateTabsetPanel(session, "mainTabs", selected = "Distribution")
  })
  
  output$status <- renderText({
    if (is.null(rv$final_df)) return("Status: waiting for data preparation ...")
    paste0("Merged dataset rows: ", nrow(rv$final_df), " | columns: ", ncol(rv$final_df))
  })
  
  # Distribution
  observeEvent(input$dist_submit, {
    req(rv$final_df, input$dist_col)
    tryCatch({
      column_name <- input$dist_col
      df <- rv$final_df
      validate(need(column_name %in% names(df), "Selected column not found."))
      validate(need(is.character(df[[column_name]]) || is.factor(df[[column_name]]),
                    "Only categorical columns are allowed for distribution."))
      
      df_unique <- df %>% select(id, !!sym(column_name)) %>% distinct()
      value_percentages <- df_unique %>%
        count(!!sym(column_name)) %>%
        mutate(Percentage = round((n / sum(n)) * 100, 2),
               Label = paste0(Percentage, "%"))
      
      rv$dist_data <- value_percentages
      
      output$percentagePlot <- renderPlot({
        ggplot(
          value_percentages,
          aes(x = reorder(!!sym(column_name), -Percentage),
              y = Percentage,
              fill = !!sym(column_name))
        ) +
          geom_bar(stat = "identity", width = 0.7) +
          geom_text(aes(label = Label), vjust = -0.5, size = 4) +
          labs(
            title = paste("Distribution of", column_name),
            x = column_name,
            y = "Percentage (%)"
          ) +
          coord_flip() + 
          theme_minimal(base_size = 14) +
          theme(legend.position = "none")
      }, res = 120)
      
      output$dist_msg <- renderText("Distribution ready. Download or click 'Next' to continue.")
      rv$dist_done <- TRUE
      
    }, error = function(e) {
      output$percentagePlot <- renderPlot({ NULL })
      output$errorMsg <- renderText(paste("Error:", conditionMessage(e)))
    })
  })
  
  output$download_dist_xlsx <- downloadHandler(
    filename = function() paste0("value_percentages_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(rv$dist_data)
      export_df <- rv$dist_data %>%
        mutate(across(1, ~ ifelse(is.na(.), "N/A", as.character(.)))) %>% 
        select(1, Percentage)
      write_xlsx(export_df, file)
    }
  )
  
  output$download_dist_png <- downloadHandler(
    filename = function() paste0("distribution_", input$dist_col %||% "column", "_", Sys.Date(), ".png"),
    content  = function(file) {
      req(rv$dist_data, input$dist_col)
      gp <- ggplot(
        rv$dist_data,
        aes(x = reorder(!!sym(input$dist_col), -Percentage),
            y = Percentage,
            fill = !!sym(input$dist_col))
      ) +
        geom_bar(stat = "identity", width = 0.7) +
        geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.5, size = 4) +
        labs(
          title = paste("Distribution of", input$dist_col),
          x = input$dist_col,
          y = "Percentage (%)"
        ) +
        coord_flip() + 
        theme_minimal(base_size = 14) +
        theme(legend.position = "none")
      ggsave(file, gp, width = 10, height = 6, dpi = 150)
    }
  )
  
  observeEvent(input$go_surv_from_dist, {
    req(rv$final_df)
    updateTabsetPanel(session, "mainTabs", selected = "Survival")
  })
  
  # Utilities for datetime detection/parsing
  parse_datetime_vec <- function(x) {
    x_char <- as.character(x)
    res <- tryCatch(ymd_hms(x_char, tz = "UTC"),
                    error = function(e) rep(as.POSIXct(NA), length(x_char)))
    if (all(is.na(res))) {
      orders <- c("Ymd HMS", "YmdHMS", "Y-m-dT%H:%M:%OS", "Y-m-d H:M:S", "Y-m-d")
      res2 <- tryCatch(parse_date_time(x_char, orders = orders, tz = "UTC"),
                       error = function(e) rep(as.POSIXct(NA), length(x_char)))
      if (!all(is.na(res2))) res <- res2
    }
    if (all(is.na(res))) {
      res3 <- tryCatch(as_datetime(x_char, tz = "UTC"),
                       error = function(e) rep(as.POSIXct(NA), length(x_char)))
      if (!all(is.na(res3))) res <- res3
    }
    res
  }
  
  find_datetime_col <- function(df) {
    nm <- names(df)
    if (length(nm) == 0) return(NULL)
    prefs <- c(
      "create_date", "created_date", "created_datetime", "create_datetime",
      "create_date_", "create_dateentity", "create"
    )
    patt1 <- paste0(prefs, collapse = "|")
    m1 <- nm[grepl(patt1, nm, ignore.case = TRUE)]
    if (length(m1) > 0) return(m1[1])
    m2 <- nm[grepl("date|time|timestamp|datetime", nm, ignore.case = TRUE)]
    if (length(m2) > 0) return(m2[1])
    for (col in nm) {
      vals <- as.character(head(na.omit(df[[col]]), 30))
      if (length(vals) < 3) next
      iso_hits <- sum(grepl("^\\d{4}-\\d{2}-\\d{2}T", vals) |
                        grepl("^\\d{4}-\\d{2}-\\d{2} ", vals))
      if ((iso_hits / length(vals)) >= 0.6) return(col)
    }
    NULL
  }
  
  ensure_datetime_col <- function(df, col) {
    if (is.null(col)) return(list(df = df, col = NULL))
    if (inherits(df[[col]], "POSIXt")) return(list(df = df, col = col))
    parsed <- parse_datetime_vec(df[[col]])
    df[[col]] <- parsed
    list(df = df, col = col)
  }
  
  # Build survival data (OS)
  survival_df <- reactive({
    req(rv$flattened[["timing"]], rv$flattened[["survival_characteristic"]])
    timing_df    <- rv$flattened[["timing"]]
    survchar_df  <- rv$flattened[["survival_characteristic"]]
    
    dt_timing <- find_datetime_col(timing_df)
    dt_surv   <- find_datetime_col(survchar_df)
    timing_res <- ensure_datetime_col(timing_df, dt_timing)
    timing_df  <- timing_res$df
    dt_timing  <- timing_res$col
    surv_res   <- ensure_datetime_col(survchar_df, dt_surv)
    survchar_df <- surv_res$df
    dt_surv     <- surv_res$col
    
    diagnosis_age_df <- NULL
    if (!is.null(dt_timing) && any(!is.na(timing_df[[dt_timing]]))) {
      diagnosis_age_df <- timing_df %>%
        filter(disease_phase == "Initial Diagnosis") %>%
        group_by(dst_id) %>%
        slice_max(order_by = .data[[dt_timing]], n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        select(dst_id, age_at_disease_phase)
    } else {
      if ("age_at_disease_phase" %in% names(timing_df)) {
        timing_df <- timing_df %>%
          mutate(age_at_disease_phase = suppressWarnings(as.numeric(age_at_disease_phase)))
        diagnosis_age_df <- timing_df %>%
          filter(disease_phase == "Initial Diagnosis") %>%
          group_by(dst_id) %>%
          slice_max(order_by = age_at_disease_phase, n = 1, with_ties = FALSE) %>%
          ungroup() %>%
          select(dst_id, age_at_disease_phase)
        output$errorMsg <- renderText(
          "Warning: no create-date found in timing; using age_at_disease_phase to pick record."
        )
      } else {
        diagnosis_age_df <- timing_df %>%
          filter(disease_phase == "Initial Diagnosis") %>%
          distinct(dst_id, .keep_all = TRUE) %>%
          select(dst_id, age_at_disease_phase)
        output$errorMsg <- renderText(
          "Warning: no create-date or age_at_disease_phase available; using first diagnosis record per subject."
        )
      }
    }
    
    if (!is.null(dt_surv) && any(!is.na(survchar_df[[dt_surv]]))) {
      surv_rows <- survchar_df %>%
        group_by(dst_id) %>%
        slice_max(order_by = .data[[dt_surv]], n = 1, with_ties = FALSE) %>%
        ungroup()
    } else if ("age_at_lkss" %in% names(survchar_df) &&
               any(!is.na(suppressWarnings(as.numeric(survchar_df$age_at_lkss))))) {
      survchar_df <- survchar_df %>%
        mutate(age_at_lkss = suppressWarnings(as.numeric(age_at_lkss)))
      surv_rows <- survchar_df %>%
        group_by(dst_id) %>%
        slice_max(order_by = age_at_lkss, n = 1, with_ties = FALSE) %>%
        ungroup()
      output$errorMsg <- renderText(
        "Warning: no create-date found in survival_characteristic; using max age_at_lkss per subject."
      )
    } else {
      surv_rows <- survchar_df %>% group_by(dst_id) %>% slice(1) %>% ungroup()
      output$errorMsg <- renderText(
        "Warning: no create-date or age_at_lkss found; using first survival_characteristic row per subject."
      )
    }
    
    survival_data_local <- surv_rows %>%
      inner_join(diagnosis_age_df, by = "dst_id") %>%
      mutate(
        age_at_lkss          = suppressWarnings(as.numeric(age_at_lkss)),
        age_at_disease_phase = suppressWarnings(as.numeric(age_at_disease_phase)),
        time_years           = (age_at_lkss - age_at_disease_phase) / 365.25,
        event                = ifelse(tolower(as.character(lkss)) == "dead", 1L, 0L)
      ) %>%
      filter(!is.na(time_years), !is.na(event), time_years >= 0)
    
    survival_data_local <- survival_data_local %>%
      group_by(dst_id) %>%
      slice_max(order_by = time_years, n = 1, with_ties = FALSE) %>%
      ungroup()
    
    validate(need(nrow(survival_data_local) > 0, "No valid rows for Overall Survival."))
    survival_data_local
  })
  
  # Build EFS data
  efs_df <- reactive({
    req(rv$flattened[["subject"]], rv$flattened[["timing"]])
    subject_df <- rv$flattened[["subject"]]
    timing_df  <- rv$flattened[["timing"]]
    
    dt_timing  <- find_datetime_col(timing_df)
    dt_subject <- find_datetime_col(subject_df)
    timing_res  <- ensure_datetime_col(timing_df, dt_timing)
    timing_df   <- timing_res$df
    dt_timing   <- timing_res$col
    subject_res <- ensure_datetime_col(subject_df, dt_subject)
    subject_df  <- subject_res$df
    dt_subject  <- subject_res$col
    
    if (!is.null(dt_timing) && any(!is.na(timing_df[[dt_timing]]))) {
      diagnosis_age_df <- timing_df %>%
        filter(disease_phase == "Initial Diagnosis") %>%
        group_by(dst_id) %>%
        slice_max(order_by = .data[[dt_timing]], n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        select(dst_id, age_at_disease_phase)
    } else if ("age_at_disease_phase" %in% names(timing_df)) {
      timing_df <- timing_df %>%
        mutate(age_at_disease_phase = suppressWarnings(as.numeric(age_at_disease_phase)))
      diagnosis_age_df <- timing_df %>%
        filter(disease_phase == "Initial Diagnosis") %>%
        group_by(dst_id) %>%
        slice_max(order_by = age_at_disease_phase, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        select(dst_id, age_at_disease_phase)
      output$errorMsg <- renderText(
        "Warning: no create-date in timing; using age_at_disease_phase for diagnosis."
      )
    } else {
      diagnosis_age_df <- timing_df %>%
        filter(disease_phase == "Initial Diagnosis") %>%
        distinct(dst_id, .keep_all = TRUE) %>%
        select(dst_id, age_at_disease_phase)
      output$errorMsg <- renderText(
        "Warning: no create-date or age_at_disease_phase available; using first diagnosis record."
      )
    }
    
    if (!is.null(dt_subject) && any(!is.na(subject_df[[dt_subject]]))) {
      subject_rows <- subject_df %>%
        group_by(id) %>%
        slice_max(order_by = .data[[dt_subject]], n = 1, with_ties = FALSE) %>%
        ungroup()
    } else {
      subject_rows <- subject_df %>% group_by(id) %>% slice(1) %>% ungroup()
      output$errorMsg <- renderText("Warning: no create-date in subject; using first subject row for each id.")
    }
    
    efs_data_local <- subject_rows %>%
      inner_join(diagnosis_age_df, by = c("id" = "dst_id")) %>%
      mutate(
        age_at_censor_status  = suppressWarnings(as.numeric(age_at_censor_status)),
        age_at_disease_phase  = suppressWarnings(as.numeric(age_at_disease_phase)),
        time_years            = (age_at_censor_status - age_at_disease_phase) / 365.25,
        event                 = ifelse(as.character(censor_status) == "Subject has had one or more events", 1L, 0L)
      ) %>%
      filter(!is.na(time_years), !is.na(event), time_years >= 0) %>%
      group_by(id) %>%
      slice_max(order_by = time_years, n = 1, with_ties = FALSE) %>%
      ungroup()
    
    validate(need(nrow(efs_data_local) > 0, "No valid rows for Event-Free Survival."))
    efs_data_local
  })
  
  survival_df_clean <- reactive({
    df <- survival_df()
    df <- df %>% group_by(dst_id) %>% slice_max(order_by = time_years, n = 1, with_ties = FALSE) %>% ungroup()
    validate(need(nrow(df) > 0, "No valid rows after deduplication."))
    df
  })
  
  efs_df_clean <- reactive({
    df <- efs_df()
    df <- df %>% group_by(id) %>% slice_max(order_by = time_years, n = 1, with_ties = FALSE) %>% ungroup()
    validate(need(nrow(df) > 0, "No valid rows after deduplication."))
    df
  })
  
  observeEvent(input$overallSurvivalBtn, {
    tryCatch({
      req(survival_df_clean())
      rv$os_fit <- survfit(Surv(time_years, event) ~ 1, data = survival_df_clean())
      rv$os_done <- TRUE
      showNotification("Overall Survival fitted — plot will render below.", type = "message")
    }, error = function(e) {
      output$errorMsg <- renderText(paste("ERROR preparing Overall Survival:", conditionMessage(e)))
    })
  })
  
  observeEvent(input$eventFreeSurvivalBtn, {
    tryCatch({
      req(efs_df_clean())
      rv$efs_fit <- survfit(Surv(time_years, event) ~ 1, data = efs_df_clean())
      rv$efs_done <- TRUE
      showNotification("Event-Free Survival fitted — plot will render below.", type = "message")
    }, error = function(e) {
      output$errorMsg <- renderText(paste("ERROR preparing Event-Free Survival:", conditionMessage(e)))
    })
  })
  
  output$overallSurvivalPlot <- renderPlot({
    req(rv$os_fit)
  
    # check median survival
    med <- suppressWarnings(summary(rv$os_fit)$table["median"])
    surv.median.opt <- if (!is.na(med) && med > 0) "hv" else NULL

    gp <- ggsurvplot(
      rv$os_fit,
      data = survival_df_clean(),
      conf.int = TRUE,
      risk.table = TRUE,
      risk.table.col = "black",
      risk.table.height = 0.25,
      surv.median.line = surv.median.opt,
      xlab = "Time Since Diagnosis (Years)",
      ylab = "Overall Survival Probability",
      title = "Kaplan-Meier Overall Survival Curve",
      palette = "Dark2",
      ggtheme = theme_minimal(base_size = 14)
    )
    print(gp)
  }, res = 120)
  
  output$eventFreeSurvivalPlot <- renderPlot({
    req(rv$efs_fit)

    # check median survival
    med <- suppressWarnings(summary(rv$efs_fit)$table["median"])
    surv.median.opt <- if (!is.na(med) && med > 0) "hv" else NULL
    
    gp <- ggsurvplot(
      rv$efs_fit,
      data = efs_df_clean(),
      conf.int = TRUE,
      risk.table = TRUE,
      risk.table.col = "black",
      risk.table.height = 0.25,
      surv.median.line = surv.median.opt,
      xlab = "Time Since Diagnosis (Years)",
      ylab = "Event-Free Survival Probability",
      title = "Kaplan-Meier Event-Free Survival Curve",
      palette = "Dark2",
      ggtheme = theme_minimal(base_size = 14)
    )
    print(gp)
  }, res = 120)
  
  observeEvent(input$go_table1, {
    updateTabsetPanel(session, "mainTabs", selected = "Table1")
  })
  
# Overall Survival (OS) with Uploaded IDs
observeEvent(input$overallSurvivalSubjectBtn, {
  ids <- uploaded_subject_ids_val()
  
  if (is.null(ids) || length(ids) == 0) {
    showNotification("⚠️ Please upload Subject IDs before running Overall Survival.", type = "warning")
    return()
  }
  
  tryCatch({
    df <- survival_df_clean()
    df_sub <- df %>% filter(dst_id %in% ids)
    validate(need(nrow(df_sub) > 0, "No matching Subject IDs found in OS data."))
    
    surv_obj <- survfit(Surv(time_years, event) ~ 1, data = df_sub)
    rv$os_fit <- surv_obj
    rv$os_done <- TRUE
  }, error = function(e) {
    output$errorMsg <- renderText(paste("ERROR preparing Overall Survival (IDs):", conditionMessage(e)))
  })
})

# Event-Free Survival (EFS) with Uploaded IDs
observeEvent(input$eventFreeSurvivalSubjectBtn, {
  ids <- uploaded_subject_ids_val()
  
  if (is.null(ids) || length(ids) == 0) {
    showNotification("⚠️ Please upload Subject IDs before running Event-Free Survival.", type = "warning")
    return()
  }
  
  tryCatch({
    df <- efs_df_clean()
    df_sub <- df %>% filter(id %in% ids)
    validate(need(nrow(df_sub) > 0, "No matching Subject IDs found in EFS data."))
    
    surv_obj <- survfit(Surv(time_years, event) ~ 1, data = df_sub)
    rv$efs_fit <- surv_obj
    rv$efs_done <- TRUE
  }, error = function(e) {
    output$errorMsg <- renderText(paste("ERROR preparing EFS (IDs):", conditionMessage(e)))
  })
})

# Clear Uploaded IDs (reset both uploaders)
observeEvent(input$clear_ids_btn, {
  # Reset stored IDs + filename
  uploaded_subject_ids_val(NULL)
  uploaded_subject_file_name_val(NULL)
  rv$uploaded_subject_ids_val <- list()
  
  # Reset the Excel/CSV/TXT uploader
  output$upload_excel_ui <- renderUI({
    fileInput("upload_excel", "Choose Excel / CSV / TXT", 
              accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv"))
  })
  
  # Reset the Subject IDs uploader
  output$upload_ids_ui <- renderUI({
    fileInput(
      "upload_subject_ids",
      "Upload Subject IDs (CSV/TXT, one ID per line):",
      accept = c(".csv", ".txt")
    )
  })
  
  showNotification("Uploaded Subject IDs have been cleared.", type = "message")
})

# Download OS Plot as PNG
output$download_os_png <- downloadHandler(
  filename = function() {
    paste0("overall_survival_", Sys.Date(), ".png")
  },
  content = function(file) {
    req(rv$os_fit)
    gp <- ggsurvplot(
      rv$os_fit,
      data = survival_df_clean(),
      conf.int = TRUE,
      risk.table = TRUE,
      risk.table.col = "black",
      risk.table.height = 0.25,
      surv.median.line = "hv",
      xlab = "Time Since Diagnosis (Years)",
      ylab = "Overall Survival Probability",
      title = "Kaplan-Meier Overall Survival Curve",
      palette = "Dark2",
      ggtheme = theme_minimal(base_size = 14)
    )
    ggsave(file, plot = gp$plot, device = "png", width = 8, height = 6, dpi = 300)
  }
)

# Download EFS Plot as PNG
output$download_efs_png <- downloadHandler(
  filename = function() {
    paste0("event_free_survival_", Sys.Date(), ".png")
  },
  content = function(file) {
    req(rv$efs_fit)
    gp <- ggsurvplot(
      rv$efs_fit,
      data = efs_df_clean(),
      conf.int = TRUE,
      risk.table = TRUE,
      risk.table.col = "black",
      risk.table.height = 0.25,
      surv.median.line = "hv",
      xlab = "Time Since Diagnosis (Years)",
      ylab = "Event-Free Survival Probability",
      title = "Kaplan-Meier Event-Free Survival Curve",
      palette = "Dark2",
      ggtheme = theme_minimal(base_size = 14)
    )
    ggsave(file, plot = gp$plot, device = "png", width = 8, height = 6, dpi = 300)
  }
)
  
  # TABLE 1 — UI cards & grouping handling
  card_ids <- reactiveVal(character(0))
  counter  <- reactiveVal(0)
  
  observeEvent(input$add_covariate, {
    req(rv$final_df)
    n <- counter() + 1
    counter(n)
    new_id <- paste0("filter", n)
    try(suppressWarnings(removeUI(selector = "#no_cards_msg", immediate = TRUE)), silent = TRUE)
    ignored_prefixes <- c("id", "dst_id", "dst_name")
    init_choices <- names(rv$final_df)[!vapply(
      names(rv$final_df),
      function(col) any(startsWith(as.character(col), ignored_prefixes)),
      logical(1)
    )]
    
    insertUI(
      selector = "#filter_container",
      where = "beforeEnd",
      ui = covariateCardUI(new_id, choices = init_choices)
    )
    covariateCardServer(
      new_id,
      reactive({ rv$final_df }),
      function(id) {
        try(suppressWarnings(removeUI(selector = paste0("#", id), immediate = TRUE)), silent = TRUE)
        card_ids(setdiff(card_ids(), id))
      }
    )
    card_ids(c(card_ids(), new_id))
  })
  
  # Robust uploaded ID parsing: watch the upload input and populate reactiveVal
  observeEvent(input$upload_excel, {
    f <- input$upload_excel
    uploaded_subject_ids_val(NULL)
    uploaded_subject_file_name_val(NULL)
    if (is.null(f)) return()
    
    path <- f$datapath
    ext  <- tolower(tools::file_ext(f$name %||% path))
    ids <- NULL
    
    try({
      if (ext %in% c("xlsx", "xls")) {
        df <- suppressWarnings(readxl::read_excel(path, col_names = TRUE))
        if (ncol(df) > 0) {
          cn <- names(df)
          pick <- which(tolower(cn) %in% c("id", "subject_id", "subjectid"))
          col <- if (length(pick) >= 1) cn[pick[1]] else cn[1]
          ids <- as.character(df[[col]])
        }
      } else if (ext %in% c("txt")) {
        lines <- readLines(path, warn = FALSE)
        ids <- trimws(lines)
        ids <- ids[nzchar(ids)]
      } else if (ext %in% c("csv")) {
        tmp <- read.csv(path, stringsAsFactors = FALSE)
        if (ncol(tmp) >= 1) ids <- as.character(tmp[[1]])
      } else if (ext %in% c("tsv")) {
        tmp <- read.delim(path, stringsAsFactors = FALSE)
        if (ncol(tmp) >= 1) ids <- as.character(tmp[[1]])
      }
    }, silent = TRUE)
    
    ids <- ids %||% character(0)
    ids <- unique(trimws(as.character(ids)))
    ids <- ids[nzchar(ids)]
    
    if (length(ids) == 0) {
      uploaded_subject_ids_val(NULL)
      uploaded_subject_file_name_val(NULL)
      showNotification("Uploaded ID file parsed but no IDs found.", type = "warning")
    } else {
      uploaded_subject_ids_val(ids)
      # Store basename (sans extension) for column labeling
      bn <- tools::file_path_sans_ext(basename(f$name))
      uploaded_subject_file_name_val(bn)
      showNotification(paste("Parsed", length(ids), "unique IDs from upload."), type = "message")
    }
  }, ignoreInit = TRUE)
  
  uploaded_subject_ids <- reactive({ uploaded_subject_ids_val() })
  
  observeEvent(uploaded_subject_ids(), {
    if (!is.null(uploaded_subject_ids())) {
      showNotification(
        "Subject ID file detected — Table 1 will use two groups (Uploaded IDs vs Others).",
        type = "message"
      )
    }
  }, ignoreInit = TRUE)
  
  # Build grouping info based on uploaded IDs; fallback to single-group
  group_info <- reactive({
    req(rv$final_df)
    unique_ids <- rv$final_df %>% pull(id) %>% as.character() %>% unique()
    up_ids <- uploaded_subject_ids()
    fname  <- uploaded_subject_file_name_val()
    
    if (!is.null(up_ids) && length(up_ids) > 0) {
      g1 <- intersect(unique_ids, up_ids)
      g2 <- setdiff(unique_ids, g1)
      if (length(g1) == 0 || length(g2) == 0) {
        list(mode = "single", all = unique_ids, g1 = character(0), g2 = character(0),
             g1_label = NULL, g2_label = "Everything else")
      } else {
        g1_label <- if (!is.null(fname) && nzchar(fname)) fname else "Group 1"
        g2_label <- "Everything else"
        list(mode = "two", all = unique_ids, g1 = g1, g2 = g2,
             g1_label = g1_label, g2_label = g2_label)
      }
    } else {
      list(mode = "single", all = unique_ids, g1 = character(0), g2 = character(0),
           g1_label = NULL, g2_label = "Everything else")
    }
  })
  
  # Helper formatters
  fmt_p <- function(p) {
    if (is.null(p) || is.na(p)) return("—")
    if (p < 0.001) "<0.001" else sprintf("%.3f", p)
  }
  
  fmt_mean_sd <- function(x) {
    x <- x[!is.na(x)]
    if (!length(x)) return("—")
    paste0(round(mean(x), 2), " (", round(stats::sd(x), 2), ")")
  }
  
  n_pct <- function(n, d) {
    if (is.na(d) || d == 0) return("0 (0%)")
    paste0(n, " (", round(100 * n / d, 2), "%)")
  }
  
  # Clear all cards + reset grouping only (do NOT delete flattened data)
  observeEvent(input$clear_cards, {
    try(suppressWarnings(removeUI(selector = "#filter_container .card", multiple = TRUE, immediate = TRUE)), silent = TRUE)
    card_ids(character(0))
    counter(0)
    session$userData$lastChoices <- NULL
    
    try(suppressWarnings(removeUI(selector = "#no_cards_msg", immediate = TRUE)), silent = TRUE)
    insertUI(
      selector = "#filter_container",
      where = "afterBegin",
      ui = tags$div(id = "no_cards_msg", class = "muted", "No covariates added. Click 'Add Covariate' to begin.")
    )
    
    # Clear stored uploaded subject IDs and filename
    uploaded_subject_ids_val(NULL)
    uploaded_subject_file_name_val(NULL)
    
    rv$table1_done <- FALSE
    
    output$status <- renderText({
      paste0(
        "Status: data loaded, but grouping cleared. Table 1 will now use single-group mode (",
        nrow(rv$final_df %||% tibble())[1],
        " rows)."
      )
    })
    output$errorMsg <- renderText("")
    
    showNotification(
      "Grouping cleared. Operations will now default to single-group until a new Subject ID file is uploaded.",
      type = "message"
    )
  })
  
  # Core: build Table 1 according to grouping mode
  table1_summary <- eventReactive(input$apply_filters, {
    req(rv$final_df)
    gi <- group_info()
    df <- rv$final_df
    
    active_cards <- card_ids()
    active_cards <- purrr::keep(active_cards, function(idc) {
      sel_col <- isolate(input[[paste0(idc, "-col")]])
      !is.null(sel_col) && nzchar(sel_col) && (sel_col %in% names(df))
    })
    
    results <- list()
    
    if (identical(gi$mode, "two")) {
      col1_name <- gi$g1_label %||% "Group 1"
      col2_name <- gi$g2_label %||% "Everything else"
      
      # Header counts for two groups (dynamically named)
      hdr <- tibble(Covariate = "Count")
      hdr[[col1_name]] <- paste0(length(gi$g1), " (Count)")
      hdr[[col2_name]] <- paste0(length(gi$g2), " (Count)")
      hdr$P_value <- ""
      results[[length(results) + 1]] <- hdr
      
      for (idc in active_cards) {
        col  <- isolate(input[[paste0(idc, "-col")]])
        vals <- isolate(input[[paste0(idc, "-vals")]])
        if (is.null(col) || !nzchar(col) || !(col %in% names(df))) next
        
        df_unique <- df %>%
          select(id, all_of(col)) %>%
          distinct() %>%
          mutate(.grp = case_when(
            id %in% gi$g1 ~ "G1",
            id %in% gi$g2 ~ "G2",
            TRUE          ~ NA_character_
          ))
        
        col_raw <- df_unique[[col]]
        if (is.list(col_raw)) {
          col_chr <- sapply(
            col_raw,
            function(x) {
              if (is.null(x)) return(NA_character_)
              if (length(x) > 1) paste0(x, collapse = ";")
              else as.character(x)
            },
            USE.NAMES = FALSE
          )
        } else {
          col_chr <- as.character(col_raw)
        }
        
        non_blank   <- !is.na(col_chr) & nzchar(trimws(col_chr))
        n_non_blank <- sum(non_blank)
        col_num     <- suppressWarnings(as.numeric(col_chr))
        numeric_prop <- ifelse(n_non_blank == 0, 0, sum(!is.na(col_num) & non_blank) / n_non_blank)
        numeric_like <- numeric_prop >= 0.90
        
        if (numeric_like) {
          g1_vec <- col_num[df_unique$.grp == "G1" & !is.na(col_num)]
          g2_vec <- col_num[df_unique$.grp == "G2" & !is.na(col_num)]
          
          p_val <- NA_real_
          if (length(g1_vec) > 0 && length(g2_vec) > 0) {
            p_val <- tryCatch({
              suppressWarnings(stats::wilcox.test(g1_vec, g2_vec)$p.value)
            }, error = function(e) NA_real_)
            if (is.na(p_val)) {
              p_val <- tryCatch({
                suppressWarnings(stats::t.test(g1_vec, g2_vec)$p.value)
              }, error = function(e) NA_real_)
            }
          }
          
          row_main <- tibble(Covariate = col)
          row_main[[col1_name]] <- ""
          row_main[[col2_name]] <- ""
          row_main$P_value <- fmt_p(p_val)
          results[[length(results) + 1]] <- row_main
          
          if (!is.null(vals) && length(vals) > 0) {
            if ("Mean" %in% vals) {
              r <- tibble(Covariate = "   Mean")
              r[[col1_name]] <- as.character(ifelse(length(g1_vec) > 0, round(mean(g1_vec, na.rm = TRUE), 2), NA))
              r[[col2_name]] <- as.character(ifelse(length(g2_vec) > 0, round(mean(g2_vec, na.rm = TRUE), 2), NA))
              r$P_value <- ""
              results[[length(results) + 1]] <- r
            }
            if ("Mode" %in% vals) {
              mode_of <- function(v) {
                v <- v[!is.na(v)]
                if (!length(v)) return(NA_character_)
                tb <- sort(table(v), decreasing = TRUE)
                names(tb)[1]
              }
              r <- tibble(Covariate = "   Mode")
              r[[col1_name]] <- as.character(mode_of(g1_vec))
              r[[col2_name]] <- as.character(mode_of(g2_vec))
              r$P_value <- ""
              results[[length(results) + 1]] <- r
            }
          }
          
          g1_tot <- sum(df_unique$.grp == "G1")
          g2_tot <- sum(df_unique$.grp == "G2")
          g1_mis <- sum(df_unique$.grp == "G1" & is.na(col_num))
          g2_mis <- sum(df_unique$.grp == "G2" & is.na(col_num))
          rmis <- tibble(Covariate = "   Missing")
          rmis[[col1_name]] <- n_pct(g1_mis, g1_tot)
          rmis[[col2_name]] <- n_pct(g2_mis, g2_tot)
          rmis$P_value <- ""
          results[[length(results) + 1]] <- rmis
          
        } else {
          test_df <- df_unique %>% filter(!is.na(.grp), !is.na(.data[[col]]))
          tab <- table(as.character(test_df[[col]]), test_df$.grp)
          p_val <- NA_real_
          if (length(dim(tab)) == 2 && all(dim(tab) >= c(1, 2))) {
            p_val <- tryCatch({
              if (all(dim(tab) == c(2, 2))) {
                if (any(tab < 5)) fisher.test(tab)$p.value
                else suppressWarnings(chisq.test(tab, correct = FALSE)$p.value)
              } else {
                if (any(tab < 5)) suppressWarnings(chisq.test(tab, simulate.p.value = TRUE, B = 5000)$p.value)
                else suppressWarnings(chisq.test(tab, correct = FALSE)$p.value)
              }
            }, error = function(e) NA_real_)
          }
          
          row_cat <- tibble(Covariate = col)
          row_cat[[col1_name]] <- ""
          row_cat[[col2_name]] <- ""
          row_cat$P_value <- fmt_p(p_val)
          results[[length(results) + 1]] <- row_cat
          
          show_levels <- vals
          if (is.null(show_levels) || length(show_levels) == 0) show_levels <- character(0)
          
          g1_den <- sum(df_unique$.grp == "G1" & !is.na(df_unique[[col]]) &
                          nzchar(as.character(df_unique[[col]])))
          g2_den <- sum(df_unique$.grp == "G2" & !is.na(df_unique[[col]]) &
                          nzchar(as.character(df_unique[[col]])))
          
          for (lvl in show_levels) {
            g1_n <- sum(df_unique$.grp == "G1" & as.character(df_unique[[col]]) == as.character(lvl), na.rm = TRUE)
            g2_n <- sum(df_unique$.grp == "G2" & as.character(df_unique[[col]]) == as.character(lvl), na.rm = TRUE)
            r <- tibble(Covariate = paste0("   ", lvl))
            r[[col1_name]] <- n_pct(g1_n, g1_den)
            r[[col2_name]] <- n_pct(g2_n, g2_den)
            r$P_value <- ""
            results[[length(results) + 1]] <- r
          }
          
          g1_mis <- sum(df_unique$.grp == "G1" & (is.na(df_unique[[col]]) | df_unique[[col]] %in% c("None", "")))
          g2_mis <- sum(df_unique$.grp == "G2" & (is.na(df_unique[[col]]) | df_unique[[col]] %in% c("None", "")))
          g1_tot <- sum(df_unique$.grp == "G1")
          g2_tot <- sum(df_unique$.grp == "G2")
          rmiss <- tibble(Covariate = "   Missing")
          rmiss[[col1_name]] <- n_pct(g1_mis, g1_tot)
          rmiss[[col2_name]] <- n_pct(g2_mis, g2_tot)
          rmiss$P_value <- ""
          results[[length(results) + 1]] <- rmiss
        }
      }
      
      out <- bind_rows(results) %>% mutate(across(everything(), ~ as.character(.x)))
      out
      
    } else {
      # SINGLE-GROUP: All subjects; rename column to "Everything else"
      col_all_name <- "Everything"
      hdr <- tibble(Covariate = "Count")
      hdr[[col_all_name]] <- paste0(length(gi$all), " (Count)")
      results[[length(results) + 1]] <- hdr
      
      for (idc in active_cards) {
        col  <- isolate(input[[paste0(idc, "-col")]])
        vals <- isolate(input[[paste0(idc, "-vals")]])
        if (is.null(col) || !nzchar(col) || !(col %in% names(df))) next
        
        df_unique <- df %>% select(id, all_of(col)) %>% distinct()
        
        col_raw <- df_unique[[col]]
        if (is.list(col_raw)) {
          col_chr <- sapply(
            col_raw,
            function(x) {
              if (is.null(x)) return(NA_character_)
              if (length(x) > 1) paste0(x, collapse = ";")
              else as.character(x)
            },
            USE.NAMES = FALSE
          )
        } else {
          col_chr <- as.character(col_raw)
        }
        
        non_blank   <- !is.na(col_chr) & nzchar(trimws(col_chr))
        n_non_blank <- sum(non_blank)
        col_num     <- suppressWarnings(as.numeric(col_chr))
        numeric_prop <- ifelse(n_non_blank == 0, 0, sum(!is.na(col_num) & non_blank) / n_non_blank)
        numeric_like <- numeric_prop >= 0.90
        
        if (numeric_like) {
          vec <- col_num[!is.na(col_num)]
          results[[length(results) + 1]] <- tibble(Covariate = col, !!col_all_name := "")
          
          if (!is.null(vals) && length(vals) > 0) {
            if ("Mean" %in% vals) {
              results[[length(results) + 1]] <- tibble(
                Covariate = "   Mean",
                !!col_all_name := as.character(ifelse(length(vec) > 0, round(mean(vec, na.rm = TRUE), 2), NA))
              )
            }
            if ("Mode" %in% vals) {
              mode_of <- function(v) {
                v <- v[!is.na(v)]
                if (!length(v)) return(NA_character_)
                tb <- sort(table(v), decreasing = TRUE)
                names(tb)[1]
              }
              results[[length(results) + 1]] <- tibble(
                Covariate = "   Mode",
                !!col_all_name := as.character(mode_of(vec))
              )
            }
          }
          
          mis <- sum(is.na(col_num))
          tot <- nrow(df_unique)
          results[[length(results) + 1]] <- tibble(
            Covariate = "   Missing",
            !!col_all_name := n_pct(mis, tot)
          )
          
        } else {
          results[[length(results) + 1]] <- tibble(Covariate = col, !!col_all_name := "")
          show_levels <- vals
          if (is.null(show_levels) || length(show_levels) == 0) show_levels <- character(0)
          
          den <- sum(!is.na(df_unique[[col]]) & nzchar(as.character(df_unique[[col]])))
          for (lvl in show_levels) {
            n <- sum(as.character(df_unique[[col]]) == as.character(lvl), na.rm = TRUE)
            results[[length(results) + 1]] <- tibble(
              Covariate = paste0("   ", lvl),
              !!col_all_name := n_pct(n, den)
            )
          }
          
          mis <- sum(is.na(df_unique[[col]]) | df_unique[[col]] %in% c("None", ""))
          tot <- nrow(df_unique)
          results[[length(results) + 1]] <- tibble(
            Covariate = "   Missing",
            !!col_all_name := n_pct(mis, tot)
          )
        }
      }
      
      out <- bind_rows(results) %>% mutate(across(everything(), ~ as.character(.x)))
      out
    }
  })
  
  output$table1_ui <- renderUI({
    req(input$apply_filters)
    tagList(h4("Table 1:"), DTOutput("filtered_table"))
  })
  
  output$filtered_table <- renderDT({
    req(table1_summary())
    df <- table1_summary()
    df$Covariate <- as.character(df$Covariate)
    col_defs <- list(list(className = "dt-left", targets = 0))
    if (ncol(df) > 1) {
      col_defs <- c(col_defs, list(list(className = "dt-center", targets = 1:(ncol(df) - 1))))
    }
    
    dt <- datatable(
      df,
      options = list(pageLength = 20, scrollX = TRUE, columnDefs = col_defs),
      rownames = FALSE,
      escape = FALSE
    )
    
    top_level <- unique(df$Covariate[!grepl("^\\s", df$Covariate)])
    indented  <- unique(df$Covariate[grepl("^\\s", df$Covariate)])
    
    if (length(top_level) > 0) {
      dt <- dt %>% formatStyle("Covariate", target = "row",
                               fontWeight = styleEqual(top_level, rep("bold", length(top_level))))
    }
    if (length(indented) > 0) {
      dt <- dt %>% formatStyle("Covariate", target = "row",
                               textIndent = styleEqual(indented, rep("20px", length(indented))))
    }
    dt
  })
  
  output$download_table1_xlsx <- downloadHandler(
    filename = function() paste0("table1_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(table1_summary())
      write_xlsx(table1_summary(), file)
    }
  )
}