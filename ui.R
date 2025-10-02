# ui.R
library(shiny)
library(shinythemes)
library(shinyjs)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  useShinyjs(),

  tags$head(
    tags$style(HTML("
      :root{
        --bg:#f6fbfd;
        --card:#ffffff;
        --brand:#0ea5e9;
        --brand-dark:#0284c7;
        --accent:#16a34a;
        --muted:#64748b;
        --glass: rgba(2,6,23,0.04);
        --success: #10b981;
      }

      html, body { 
        background: var(--bg); 
        color: #0f172a; 
        font-family: 'Inter','Segoe UI',Roboto,sans-serif;
      }
      .container-fluid { padding-left:20px; padding-right:20px; }

      /* Header */
      .app-title { 
        display:flex; align-items:center; justify-content:space-between; 
        gap:12px; margin-bottom:16px; padding-bottom:8px; 
        border-bottom:2px solid rgba(2,132,199,0.08);
      }
      .brand { font-weight:800; letter-spacing:.5px; font-size:20px; }
      .subtitle { color:var(--muted); font-size:14px; font-weight:500; }

      /* Card look */
      .panel-soft {
        background: var(--card); border-radius:14px; padding:20px;
        box-shadow: 0 6px 20px rgba(2,6,23,0.08);
        transition: transform 0.08s ease, box-shadow 0.12s ease;
      }

      /* Sidebar */
      .sticky-sidebar { position: sticky; top:16px; }

      /* Buttons */
      .btn { 
        font-weight:700; border-radius:10px; 
        padding:8px 16px; transition: all 0.15s ease; 
      }
      .btn-primary { background: var(--brand); border:none; color: #fff; }
      .btn-primary:hover { background: var(--brand-dark); transform: scale(1.03); }
      .btn-success { background: var(--accent); border:none; color:#fff; }
      .btn-success:hover { background:#059669; transform: scale(1.03); }
      .btn-info { background: var(--brand-dark); border:none; color:#fff; }
      .btn-info:hover { background:#0369a1; transform: scale(1.03); }

      .btn-outline-secondary {
        background: #fff;
        border: 2px solid #e2e8f0;   
        color: #0f172a;
        border-radius: 8px;          
        padding: 8px 16px;           
        font-weight: 500;
        transition: all 0.2s ease-in-out;
        box-shadow: 0 1px 2px rgba(0,0,0,0.05);
      }
      .btn-outline-secondary:hover {
        background: #f0f9ff; 
        border-color: var(--brand); 
        color: #0f172a;
        box-shadow: 0 3px 6px rgba(0,0,0,0.1);
      }

      /* Muted text */
      .muted { color:var(--muted); font-size:13px; line-height:1.5; }

      /* Progress steps */
      .mini-progress { display:flex; flex-direction:column; gap:10px; margin-top:10px; }
      .mini-step {
        display:flex; align-items:center; gap:12px;
        padding:8px 10px; border-radius:10px;
        background: #f8fafc; border: 1px solid rgba(2,6,23,0.05);
        transition: background 0.15s ease;
      }
      .mini-step:hover { background: rgba(14,165,233,0.05); }
      .mini-icon {
        width:30px; height:30px; border-radius:999px;
        display:flex; align-items:center; justify-content:center;
        font-weight:800; font-size:14px; color:#fff;
        box-shadow: 0 2px 6px rgba(0,0,0,0.12);
      }
      .mini-label { font-weight:700; font-size:13px; color:#0f172a; }
      .mini-icon.pending { background: #cbd5e1; color:#0f172a; }
      .mini-icon.active { background: linear-gradient(90deg,var(--brand),var(--brand-dark)); }
      .mini-icon.done { background: var(--success); }

      /* Next button for Survival -> Table1 */
      .next-to-table {
        font-size: 15px;
        padding: 10px 18px;
        border-radius: 10px;
        box-shadow: 0 8px 22px rgba(2,6,23,0.08);
      }
      @media (max-width: 767px) {
        .next-to-table { width: 100%; display:block; }
      }

      /* Footer */
      .footer { margin-top:16px; font-size:12px; color:var(--muted); text-align:center; }

      /* Plot containers */
      .shiny-plot-output { 
        background: var(--card); border-radius:10px; padding:8px; 
        border:1px solid rgba(2,6,23,0.04); 
      }

      hr {
        border: none;
        border-top: 1px solid #e2e8f0;
        margin: 12px 0;
      }

      .step-badge { 
        display:inline-flex; align-items:center; gap:8px; 
        padding:8px 12px; border-radius:999px; 
        background:#f1f5f9; margin-right:8px; 
      }
      .step-badge.done { background:#d1fae5; }
      .step-badge.locked { background:#eee; opacity:.7; }

      /* Tab bar */
      .nav-tabs {
        border-bottom: 2px solid #e2e8f0;  
        background: #f8fafc;               
        padding: 6px 12px;                 
        border-radius: 8px 8px 0 0;        
      }
    "))
  ),

  titlePanel(
    div(
      span("Avro Insights", class = "brand"),
      br(),
      span("Upload → Select → Explore → Analyze → Summarize", class = "muted")
    )
  ),

  # Layout
  fluidRow(
    # Sidebar
    column(
      width = 3,
      div(
        class = "sticky-sidebar",
        wellPanel(
          # Step 1
          h4("1) Upload AVRO", style = "margin-top:0;"),
          fileInput("avroFile", "Choose AVRO File:", accept = ".avro"),
          textOutput("filePath"),
          hr(),

          # Step 2
          h4("2) Upload Subject IDs (optional)"),
          uiOutput("upload_excel_ui"),
          p(class = "muted no-space",
            "• TXT: one ID per line", br(),
            "• CSV/Excel: must contain a column named 'SubjectID'"
          ),
          actionButton("clear_ids_btn", "Clear IDs", class = "btn btn-outline-secondary", width = "100%"),
          hr(),

          # Step 3
          h4("3) Choose Entities (optional)"),
          p(class = "muted", "We always include 'subject', 'person', 'timing', and 'survival_characteristic'. Select additional entities to join if needed."),
          uiOutput("entity_picker"),
          hr(),

          # Step 4
          h4("4) Prepare Data"),
          actionButton("prep_btn", "Prepare (Flatten Selected Only)", class = "btn btn-primary", width = "100%"),
          br(), br(),
          actionButton("reset_btn", "Start Over", class = "btn btn-outline-secondary", width = "100%"),
          hr(),

          # Progress
          h5("Progress"),
          htmlOutput("progress_steps"),
          div(class = "footer", "D4CG · AVRO File Analysis Tool")
        )
      )
    ),

    # Main content
    column(
      width = 9,
      tabsetPanel(
        id = "mainTabs",

        # Distribution
        tabPanel("Distribution", value = "Distribution",
          br(),
          wellPanel(
            h4("Percentage Distribution"),
            fluidRow(
              column(6, selectInput("dist_col", "Select Column", choices = NULL, width = "100%")),
              column(6, style = "text-align:right;",
                actionButton("dist_submit", "Generate Distribution", class = "btn btn-info"),
                actionButton("go_surv_from_dist", "Next: Survival →", class = "btn btn-success")
              )
            ),
            br(), plotOutput("percentagePlot", height = "520px"),
            br(),
            fluidRow(
              column(6, downloadButton("download_dist_xlsx", "Download Percentages (.xlsx)", class = "btn btn-outline-secondary")),
              column(6, downloadButton("download_dist_png", "Download Plot (.png)", class = "btn btn-outline-secondary"))
            ),
            textOutput("dist_msg")
          )
        ),

        # Survival
        tabPanel("Survival", value = "Survival",
          br(),
          wellPanel(
            h4("Overall Survival (OS)"),
            p(class = "muted", "Uses timing + survival_characteristic. Optionally restrict to uploaded IDs."),
            fluidRow(
              column(6, actionButton("overallSurvivalBtn", "Show OS (All Data)", class = "btn btn-info")),
              column(6, style = "text-align:right;", actionButton("overallSurvivalSubjectBtn", "Show OS (Using IDs)", class = "btn btn-success"))
            ),
            br(), plotOutput("overallSurvivalPlot", height = "820px"),
            br(), downloadButton("download_os_png", "Download OS Plot (.png)", class = "btn btn-outline-secondary")
          ),
          br(),
          wellPanel(
            h4("Event-Free Survival (EFS)"),
            p(class = "muted", "Uses subject + timing. Optionally restrict to uploaded IDs."),
            fluidRow(
              column(6, actionButton("eventFreeSurvivalBtn", "Show EFS (All Data)", class = "btn btn-info")),
              column(6, style = "text-align:right;", actionButton("eventFreeSurvivalSubjectBtn", "Show EFS (Using IDs)", class = "btn btn-success"))
            ),
            br(), plotOutput("eventFreeSurvivalPlot", height = "820px"),
            br(), downloadButton("download_efs_png", "Download EFS Plot (.png)", class = "btn btn-outline-secondary")
          ),
          br(),
          div(style = "display:flex; justify-content:flex-end; align-items:center; gap:10px;",
            actionButton("go_table1", "Next: Summary Table →", class = "btn btn-success next-to-table")
          )
        ),

        # Table 1
        tabPanel("Summary Table", value = "Table1",
          br(),
          wellPanel(
            h4("Covariate Filters"),
            span(class = "muted", "Apply covariate filters. If you upload Subject IDs, a grouping will be created between the uploaded IDs and the remaining data."),
            br(), br(),
            div(id = "filter_container", div(id = "no_cards_msg", class = "muted", "No covariates added. Click 'Add Covariate' to begin.")),
            br(),
            actionButton("add_covariate", "Add Covariate", class = "btn btn-success"),
            actionButton("apply_filters", "Generate Table 1", class = "btn btn-info", style = "margin-left:6px;"),
            actionButton("clear_cards", "Clear All", class = "btn btn-outline-secondary", style = "margin-left:6px;")
          ),
          br(), uiOutput("table1_ui"),
          br(), downloadButton("download_table1_xlsx", "Download Table 1 (.xlsx)", class = "btn btn-outline-secondary")
        )
      ),
      br(), verbatimTextOutput("status"),
      verbatimTextOutput("errorMsg")
    )
  )
)