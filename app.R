# ==================================================================
# DE Venn Explorer: An interactive tool for visualizing and comparing differential gene expression overlaps.
# Version (2.0) Enhanced - Added gene direction filter, Euler, Sankey diagrams.
# By Dinuka Adasooriya, Yonsei University College of Dentistry, Seoul, Korea
# ==================================================================

#install.packages(c(
#  "shiny", "shinyjs", "colourpicker", "VennDiagram", "ggvenn",
#  "dplyr", "DT", "shinyWidgets", "readxl", "openxlsx", "grid",
# "UpSetR", "eulerr", "ggplot2", "showtext", "networkD3", "htmlwidgets"
#))

# List of required packages
library(shiny)
library(shinyjs)
library(colourpicker)
library(VennDiagram)
library(ggvenn)
library(dplyr)
library(DT)
library(shinyWidgets)
library(readxl)
library(openxlsx)
library(tools)
library(grid)
library(UpSetR)
library(eulerr)
library(ggplot2)
library(showtext)
library(networkD3)

# Set maximum file upload size to 50MB
options(shiny.maxRequestSize = 50 * 1024^2)

# ---- UI ----
ui <- fluidPage(
  titlePanel(
    windowTitle = "DE Venn Explorer v2.0",
    title = div(
      "DE Venn Explorer",
      tags$small("v2.0", style = "margin-left: 10px; font-size: 60%; color: #777;")
    )
  ),
  tags$h5("An interactive tool for visualizing and comparing differential gene expression overlaps."),

  # Enhanced CSS
  tags$head(
    tags$style(HTML("
      .selectize-dropdown {
        z-index: 10000;
      }
      #edit_panel {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 8px;
        padding: 20px;
        margin-top: 15px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }
      .well {
        background-color: #ffffff;
        border: 1px solid #e9ecef;
        border-radius: 6px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.05);
        margin-bottom: 15px;
      }
      .btn-primary {
        background-color: #0073C2;
        border-color: #0073C2;
        font-weight: 500;
      }
      .btn-primary:hover {
        background-color: #005a94;
        border-color: #005a94;
      }
      .btn-info {
        background-color: #17a2b8;
        border-color: #17a2b8;
      }
      h4 {
        color: #2c3e50;
        font-weight: 600;
        margin-top: 15px;
      }
      h5 {
        color: #34495e;
        font-weight: 500;
      }
      footer {
        margin-top: 40px;
        padding-top: 20px;
        border-top: 2px solid #e9ecef;
        color: #6c757d;
        font-size: 13px;
        text-align: center;
      }
      .shiny-notification {
        position: fixed;
        top: 60px;
        right: 20px;
        width: 400px;
      }
      hr {
        border-top: 1px solid #dee2e6;
        margin: 25px 0;
      }
      .form-control:focus {
        border-color: #0073C2;
        box-shadow: 0 0 0 0.2rem rgba(0, 115, 194, 0.25);
      }
      .alert-warning {
        background-color: #fff3cd;
        border-color: #ffc107;
        color: #856404;
        padding: 10px;
        border-radius: 4px;
        margin-top: 10px;
      }
      /* Utility class to align buttons with input labels */
      .align-btn-with-input {
        margin-top: 25px;
      }
    "))
  ),
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("📊 Data Input Settings"),
      numericInput("num_datasets", "Number of datasets (2–5):",
        value = 2, min = 2, max = 5, step = 1
      ),

      # Safety check message
      div(
        id = "dataset_count_warning", style = "display: none;",
        class = "alert alert-warning",
        icon("exclamation-triangle"),
        " Please enter a valid number between 2 and 5"
      ),
      hr(),
      h4("📥 Data Input Method"),
      radioButtons("input_method",
        "Choose input method:",
        choices = c(
          "Upload DE result files" = "file",
          "Paste gene IDs/names" = "paste"
        ),
        selected = "file"
      ),
      helpText(
        icon("info-circle"),
        strong("Upload files:"), " Upload full differential expression result files (CSV/TSV/Excel) with statistics. Note: For .xls files, save as .xlsx to avoid errors.",
        br(),
        strong("Paste genes:"), " Directly paste lists of significant gene IDs or names (one per line, no commas)."
      ),
      hr(),

      # File upload section
      conditionalPanel(
        condition = "input.input_method == 'file'",
        h4("📤 Upload DE Result Files"),
        helpText(
          "Recommended: Use .xlsx or .csv formats. If you have .xls files and encounter errors, ",
          "please save them as .xlsx or .csv in Excel first."
        ),
        uiOutput("file_upload_ui"),
        hr(),
        h4("🔧 Column Name Mapping"),
        selectizeInput("gene_col", "Gene ID Column Name:",
          choices = NULL,
          options = list(
            create = TRUE,
            placeholder = "Type or select column name"
          )
        ),
        selectizeInput("gene_name_col", "Gene Name Column (optional):",
          choices = NULL,
          options = list(
            create = TRUE,
            placeholder = "Type or select column name"
          )
        ),
        selectizeInput("padj_col", "Adjusted P-value Column Name:",
          choices = NULL,
          options = list(
            create = TRUE,
            placeholder = "Type or select column name"
          )
        ),
        selectizeInput("lfc_col", "Log2 Fold Change Column Name:",
          choices = NULL,
          options = list(
            create = TRUE,
            placeholder = "Type or select column name"
          )
        ),
        hr(),
        h4("⚙️ Filter Settings"),
        numericInput("padj_cutoff", "Adjusted P-value Cutoff:",
          value = 0.05, min = 0, max = 1, step = 0.01
        ),
        numericInput("lfc_cutoff", "Absolute Log2FC Cutoff:",
          value = 1, min = 0, step = 0.1
        ),
        checkboxInput("use_lfc", "Apply Log2FC filter", value = TRUE),
        hr(),
        h4("🎯 Gene Direction Filter"),
        radioButtons("gene_direction",
          "Include genes:",
          choices = c(
            "All significant genes" = "all",
            "Upregulated only (Log2FC > 0)" = "up",
            "Downregulated only (Log2FC < 0)" = "down"
          ),
          selected = "all"
        ),
        helpText(
          icon("info-circle"),
          "Filter genes based on their expression direction. Only applies when Log2FC filter is enabled."
        )
      ),

      # Paste genes section
      conditionalPanel(
        condition = "input.input_method == 'paste'",
        h4("📋 Paste Gene Lists"),
        helpText(
          "Enter one gene ID or name per line. These should be your ",
          strong("already filtered significant genes"), " from differential expression analysis."
        ),
        uiOutput("paste_genes_ui"),
        actionButton("clear_paste", "Clear All Pasted Data", icon = icon("eraser"), class = "btn-warning btn-sm btn-block")
      ),
      hr(),
      actionButton("generate", "Generate Diagram",
        class = "btn-primary btn-block",
        icon = icon("chart-area")
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "📊 Visualization",
          br(),
          # Layout for Select and Downloads in one row
          fluidRow(
            column(
              width = 4,
              selectInput("diagram_type", "Select Diagram Type:",
                choices = c(
                  "Venn Diagram" = "venn",
                  "UpSet Plot" = "upset",
                  "Euler Diagram" = "euler",
                  "Sankey Diagram" = "sankey"
                ),
                selected = "venn"
              )
            ),
            column(
              width = 8,
              class = "align-btn-with-input",
              div(
                style = "display: inline-block; vertical-align: top;",
                downloadButton("download_plot", "Download (PNG)", class = "btn-default")
              ),
              div(
                style = "display: inline-block; vertical-align: top; margin-left: 5px;",
                downloadButton("download_svg", "Download (SVG)", class = "btn-default")
              )
            )
          ),
          fluidRow(
            column(
              width = 8,
              conditionalPanel(
                condition = "input.diagram_type != 'sankey'",
                plotOutput("main_plot", height = "600px")
              ),
              conditionalPanel(
                condition = "input.diagram_type == 'sankey'",
                sankeyNetworkOutput("sankey_plot", height = "600px")
              )
            ),
            column(
              width = 4,
              h4("🎨 Diagram Options"),
              actionButton("edit_diagram_btn",
                "Show / Hide Diagram Options",
                icon = icon("paint-brush"),
                class = "btn-info btn-sm btn-block"
              ),
              shinyjs::hidden(
                div(
                  id = "edit_panel",
                  h5("Font Sizes"),
                  numericInput("label_font_size", "Label Font Size:",
                    value = 1.2, min = 0.1, max = 5, step = 0.1
                  ),
                  numericInput("number_font_size", "Number Font Size:",
                    value = 1.5, min = 0.1, max = 5, step = 0.1
                  ),
                  helpText("For 2–3 sets: set_name_size / text_size (ggvenn). For 4–5 sets: cat.cex / cex (VennDiagram)."),
                  hr(),
                  h5("Set Labels"),
                  uiOutput("label_inputs"),
                  hr(),

                  # Color options - conditional based on diagram type
                  conditionalPanel(
                    condition = "input.diagram_type == 'venn' || input.diagram_type == 'euler'",
                    h5("Set Colors"),
                    uiOutput("color_inputs"),
                    hr()
                  ),
                  conditionalPanel(
                    condition = "input.diagram_type == 'upset'",
                    h5("UpSet Plot Colors"),
                    colourInput("upset_main_bar_color", "Main Bar Color:", value = "#0073C2FF"),
                    colourInput("upset_sets_bar_color", "Sets Bar Color:", value = "#EFC000FF"),
                    colourInput("upset_matrix_color", "Matrix Dot Color:", value = "#404040"),
                    hr()
                  ),
                  conditionalPanel(
                    condition = "input.diagram_type == 'sankey'",
                    h5("Sankey Diagram Colors"),
                    uiOutput("sankey_color_inputs"),
                    colourInput("sankey_link_color", "Link Opacity Color:", value = "#CCCCCC"),
                    hr()
                  ),
                  conditionalPanel(
                    condition = "input.diagram_type == 'venn'",
                    checkboxInput("show_percent",
                      "Show percentage values (for 2–3 set diagrams)",
                      value = TRUE
                    )
                  )
                )
              )
            )
          )
        ),
        tabPanel(
          "📋 Overlap Summary",
          h4("Number of Genes per Set"),
          tableOutput("summary_table"),
          hr(),
          h4("Intersection Counts"),
          tableOutput("intersection_table"),
          hr(),
          downloadButton("download_summary_txt", "Download Summary (TXT)", class = "btn-default")
        ),
        tabPanel(
          "🔍 Gene Lists",
          br(),
          fluidRow(
            column(
              width = 4,
              selectInput("overlap_select", "Select Overlap:", choices = NULL)
            ),
            column(
              width = 8,
              class = "align-btn-with-input",
              downloadButton("download_overlaps", "Download Gene List (CSV)", class = "btn-default")
            )
          ),
          DTOutput("gene_table")
        ),
        tabPanel(
          "ℹ️ Data Input Info",
          verbatimTextOutput("file_info")
        ),
        tabPanel(
          "ℹ️ About",
          h3("DE Venn Explorer v2.0"),
          p("An interactive tool for visualizing and comparing differential gene expression overlaps."),
          br(),
          p(strong("Developer:"), "Dinuka Adasooriya"),
          p(
            "Division of Anatomy and Developmental Biology, Department of Oral Biology, BK21 FOUR Project,",
            "Yonsei University College of Dentistry, Seoul, Korea"
          ),
          p(strong("Email:"), "dinuka90@yuhs.ac"),
          br(),
          h4("Availability"),
          p(
            "Source code:",
            tags$a(
              href = "https://github.com/Dinuka0001/DE_Venn_Explorer.git",
              "GitHub repository",
              target = "_blank"
            )
          ),
          p(
            "Online app:",
            tags$a(
              href = "https://dinuka-de-venn-explorer.share.connect.posit.cloud",
              "DE Venn Explorer on Posit Cloud",
              target = "_blank"
            )
          ),
          br(),
          h4("R packages used"),
          verbatimTextOutput("references")
        )
      )
    )
  ),
  tags$footer(
    "DE Venn Explorer v2.0 | An interactive tool for visualizing and comparing differential gene expression overlaps",
    tags$br(),
    "© Dinuka Adasooriya | Yonsei University College of Dentistry"
  )
)

# ---- Server ----
server <- function(input, output, session) {
  # Enable custom fonts for plots
  showtext_auto()

  # Reactive values to store data
  gene_lists <- reactiveVal(NULL)
  gene_names_data <- reactiveVal(NULL)
  all_overlaps <- reactiveVal(NULL)

  # Diagram settings
  diagram_settings <- reactiveValues(
    labels = NULL,
    colors = NULL
  )

  # Validate number of datasets input
  observe({
    if (is.na(input$num_datasets) || is.null(input$num_datasets)) {
      shinyjs::show("dataset_count_warning")
      shinyjs::disable("generate")
    } else if (input$num_datasets < 2 || input$num_datasets > 5) {
      shinyjs::show("dataset_count_warning")
      shinyjs::disable("generate")
    } else {
      shinyjs::hide("dataset_count_warning")
      shinyjs::enable("generate")
    }
  })

  # Toggle diagram edit panel
  observeEvent(input$edit_diagram_btn, {
    shinyjs::toggle("edit_panel")
  })

  # ---- Dynamic upload UI ----
  output$file_upload_ui <- renderUI({
    req(input$num_datasets)
    if (is.na(input$num_datasets) || input$num_datasets < 2 || input$num_datasets > 5) {
      return(NULL)
    }

    n <- input$num_datasets

    tagList(
      lapply(1:n, function(i) {
        wellPanel(
          h5(paste("📁 Dataset", i)),
          fileInput(
            paste0("file", i),
            label = "Upload CSV / TSV / TXT / Excel",
            multiple = FALSE,
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv", ".tsv", ".txt", ".xlsx", ".xls"
            )
          ),
          selectInput(
            paste0("sep", i), "Column separator (for text files):",
            choices = c(
              "Auto" = "auto",
              "Comma (,)" = ",",
              "Tab (\\t)" = "\t",
              "Semicolon (;)" = ";"
            ),
            selected = "auto"
          ),
          uiOutput(paste0("sheet_ui", i))
        )
      })
    )
  })

  # ---- Dynamic paste genes UI ----
  output$paste_genes_ui <- renderUI({
    req(input$num_datasets)
    if (is.na(input$num_datasets) || input$num_datasets < 2 || input$num_datasets > 5) {
      return(NULL)
    }

    n <- input$num_datasets

    tagList(
      lapply(1:n, function(i) {
        wellPanel(
          textInput(
            paste0("dataset_name", i),
            label = paste("Dataset", i, "name:"),
            value = paste("Dataset", i),
            placeholder = "Enter a name for this dataset"
          ),
          textAreaInput(
            paste0("genes", i),
            label = paste("Gene IDs/Names (one per line):"),
            value = "",
            height = "150px",
            placeholder = "GENE1\nGENE2\nGENE3\n..."
          ),
          helpText("Genes entered:", textOutput(paste0("gene_count", i), inline = TRUE))
        )
      })
    )
  })

  # Gene count displays for paste method
  max_datasets <- 5
  for (ii in 1:max_datasets) {
    local({
      i <- ii
      output[[paste0("gene_count", i)]] <- renderText({
        genes_text <- input[[paste0("genes", i)]]
        if (is.null(genes_text) || genes_text == "") {
          return("0")
        }
        genes <- strsplit(genes_text, "\n")[[1]]
        genes <- trimws(genes)
        genes <- genes[genes != ""]
        as.character(length(genes))
      })
    })
  }

  # ---- Clear pasted data ----
  observeEvent(input$clear_paste, {
    req(input$num_datasets)
    n <- input$num_datasets

    for (i in 1:n) {
      updateTextAreaInput(session, paste0("genes", i), value = "")
      updateTextInput(session, paste0("dataset_name", i), value = paste("Dataset", i))
    }

    showNotification("Pasted data has been cleared.",
      type = "message", duration = 3
    )
  })

  # ---- Sheet selection UI ----
  max_files <- 5
  for (ii in 1:max_files) {
    local({
      i <- ii
      output[[paste0("sheet_ui", i)]] <- renderUI({
        file <- input[[paste0("file", i)]]
        if (is.null(file)) {
          return(NULL)
        }

        ext <- tolower(file_ext(file$name))
        if (ext %in% c("xlsx", "xls")) {
          sheets <- tryCatch(
            {
              tmp_file <- tempfile(fileext = paste0(".", ext))
              file.copy(file$datapath, tmp_file, overwrite = TRUE)
              sheet_names <- excel_sheets(tmp_file)
              unlink(tmp_file)
              sheet_names
            },
            error = function(e) {
              showNotification(paste("Error reading Excel sheets:", e$message),
                type = "error", duration = 5
              )
              NULL
            }
          )

          if (is.null(sheets) || length(sheets) == 0) {
            return(NULL)
          }

          selectInput(
            paste0("sheet", i), "Excel sheet:",
            choices = sheets,
            selected = sheets[1]
          )
        } else {
          NULL
        }
      })
    })
  }

  # ---- Helper: read a single uploaded file into data.frame ----
  read_uploaded_file <- function(file_input, sep_input, sheet_input = NULL) {
    req(file_input)

    tryCatch(
      {
        ext <- tolower(file_ext(file_input$name))

        if (ext %in% c("xlsx", "xls")) {
          tmp_file <- tempfile(fileext = paste0(".", ext))
          success <- file.copy(file_input$datapath, tmp_file, overwrite = TRUE)

          if (!success) {
            stop("Failed to copy uploaded file to temporary location")
          }

          if (!file.exists(tmp_file)) {
            stop("Temporary file does not exist")
          }

          sheet_to_read <- if (is.null(sheet_input) || sheet_input == "") 1 else sheet_input

          df <- NULL

          if (is.null(df) && ext == "xlsx") {
            df <- tryCatch(
              {
                read_excel(
                  path = tmp_file,
                  sheet = sheet_to_read,
                  .name_repair = "minimal"
                )
              },
              error = function(e) {
                message("readxl failed: ", e$message)
                NULL
              }
            )
          }

          if (is.null(df)) {
            df <- tryCatch(
              {
                if (ext == "xlsx") {
                  openxlsx::read.xlsx(
                    xlsxFile = tmp_file,
                    sheet = sheet_to_read,
                    check.names = FALSE
                  )
                } else {
                  NULL
                }
              },
              error = function(e) {
                message("openxlsx failed: ", e$message)
                NULL
              }
            )
          }

          if (is.null(df) && ext == "xls") {
            df <- tryCatch(
              {
                read_excel(
                  path = tmp_file,
                  sheet = sheet_to_read,
                  .name_repair = "minimal"
                )
              },
              error = function(e) {
                message("Could not read .xls file: ", e$message)
                stop(paste0(
                  "Unable to read .xls file. This may be due to:\n",
                  "1. The file is corrupted or in an unsupported format\n",
                  "2. The file is actually in a different format (try .xlsx or .csv)\n\n",
                  "Solutions:\n",
                  "- Open the file in Excel and save as .xlsx format\n",
                  "- Export the file as CSV format\n",
                  "- Ensure the file is not password protected\n\n",
                  "Original error: ", e$message
                ))
              }
            )
          }

          if (is.null(df)) {
            stop("All methods to read Excel file failed. Please convert to .xlsx or .csv format.")
          }

          df <- as.data.frame(df, stringsAsFactors = FALSE)

          if (file.exists(tmp_file)) {
            unlink(tmp_file)
          }

          df <- df[rowSums(is.na(df)) != ncol(df), ]
          df <- df[, colSums(is.na(df)) != nrow(df), drop = FALSE]
        } else {
          sep <- sep_input
          if (is.null(sep) || sep == "auto") {
            first_line <- readLines(file_input$datapath, n = 1)
            if (grepl("\t", first_line)) {
              sep <- "\t"
            } else if (grepl(";", first_line)) {
              sep <- ";"
            } else {
              sep <- ","
            }
          }

          df <- read.table(
            file = file_input$datapath,
            sep = sep,
            header = TRUE,
            stringsAsFactors = FALSE,
            check.names = FALSE,
            quote = "\"",
            comment.char = "",
            fill = TRUE,
            na.strings = c("", "NA", "N/A", "null", "NULL")
          )
        }

        colnames(df) <- trimws(colnames(df))

        if (nrow(df) == 0 || ncol(df) == 0) {
          stop("File appears to be empty or improperly formatted")
        }

        return(df)
      },
      error = function(e) {
        message("Error in read_uploaded_file: ", e$message)
        stop(e$message)
      }
    )
  }

  # ---- Auto-detect column names & Clear Data on Upload ----
  observeEvent(
    {
      file_triggers <- lapply(1:5, function(i) input[[paste0("file", i)]])
      sheet_triggers <- lapply(1:5, function(i) input[[paste0("sheet", i)]])
      list(file_triggers, sheet_triggers, input$input_method, input$num_datasets)
    },
    {
      if (is.null(input$input_method) || input$input_method != "file") {
        return(NULL)
      }

      if (is.na(input$num_datasets) || input$num_datasets < 2 || input$num_datasets > 5) {
        return(NULL)
      }

      gene_lists(NULL)
      gene_names_data(NULL)
      all_overlaps(NULL)
      updateSelectInput(session, "overlap_select", choices = character(0))

      n <- input$num_datasets
      first_index <- NULL

      for (i in 1:n) {
        if (!is.null(input[[paste0("file", i)]])) {
          first_index <- i
          break
        }
      }

      if (is.null(first_index)) {
        updateSelectizeInput(session, "gene_col", choices = character(0), selected = character(0))
        updateSelectizeInput(session, "gene_name_col", choices = character(0), selected = character(0))
        updateSelectizeInput(session, "padj_col", choices = character(0), selected = character(0))
        updateSelectizeInput(session, "lfc_col", choices = character(0), selected = character(0))
        return(NULL)
      }

      file <- input[[paste0("file", first_index)]]
      sep <- input[[paste0("sep", first_index)]]
      sheet_input <- input[[paste0("sheet", first_index)]]

      df <- tryCatch(
        {
          read_uploaded_file(file, sep, sheet_input)
        },
        error = function(e) {
          showNotification(paste("Error reading file for column detection:", e$message),
            type = "error", duration = 6
          )
          NULL
        }
      )

      if (is.null(df)) {
        return(NULL)
      }

      col_names <- colnames(df)

      gene_id_defaults <- c("gene_id", "gene", "Gene_ID", "GeneID", "ID", "ensembl_gene_id", "ensembl", "ENSEMBL")
      gene_name_defaults <- c("gene_name", "gene_symbol", "GeneName", "Symbol", "Name", "external_gene_name", "SYMBOL", "symbol")
      padj_defaults <- c("padj", "adj.P.Val", "FDR", "p.adjust", "pvalue_adj", "P.Value.adj", "adjusted_pvalue", "qvalue")
      lfc_defaults <- c("log2FoldChange", "logFC", "log2FC", "LFC", "FC", "fold_change", "foldChange")

      find_best_match <- function(cols, defaults) {
        for (default in defaults) {
          matches <- grep(paste0("^", default, "$"), cols, ignore.case = TRUE, value = TRUE)
          if (length(matches) > 0) {
            return(matches[1])
          }
        }
        for (default in defaults) {
          matches <- grep(default, cols, ignore.case = TRUE, value = TRUE)
          if (length(matches) > 0) {
            return(matches[1])
          }
        }
        return(NULL)
      }

      gene_id_match <- find_best_match(col_names, gene_id_defaults)
      gene_name_match <- find_best_match(col_names, gene_name_defaults)
      padj_match <- find_best_match(col_names, padj_defaults)
      lfc_match <- find_best_match(col_names, lfc_defaults)

      updateSelectizeInput(
        session, "gene_col",
        choices = col_names,
        selected = if (!is.null(gene_id_match)) gene_id_match else col_names[1],
        server = TRUE
      )

      updateSelectizeInput(
        session, "gene_name_col",
        choices = c("", col_names),
        selected = if (!is.null(gene_name_match)) gene_name_match else "",
        server = TRUE
      )

      updateSelectizeInput(
        session, "padj_col",
        choices = col_names,
        selected = if (!is.null(padj_match)) padj_match else col_names[1],
        server = TRUE
      )

      updateSelectizeInput(
        session, "lfc_col",
        choices = col_names,
        selected = if (!is.null(lfc_match)) lfc_match else col_names[1],
        server = TRUE
      )

      showNotification("✓ Files changed. Data reset and columns detected.",
        type = "message", duration = 3
      )
    }
  )

  # ---- Calculate overlaps ----
  calculate_overlaps <- function(gene_lists) {
    n <- length(gene_lists)
    list_names <- names(gene_lists)
    overlaps <- list()

    for (i in 1:n) {
      overlaps[[list_names[i]]] <- gene_lists[[i]]
    }

    if (n >= 2) {
      for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
          name <- paste(list_names[i], "∩", list_names[j])
          overlaps[[name]] <- intersect(gene_lists[[i]], gene_lists[[j]])
        }
      }
    }

    if (n >= 3) {
      for (i in 1:(n - 2)) {
        for (j in (i + 1):(n - 1)) {
          for (k in (j + 1):n) {
            name <- paste(
              list_names[i], "∩",
              list_names[j], "∩",
              list_names[k]
            )
            overlaps[[name]] <- Reduce(intersect, gene_lists[c(i, j, k)])
          }
        }
      }
    }

    if (n >= 4) {
      for (i in 1:(n - 3)) {
        for (j in (i + 1):(n - 2)) {
          for (k in (j + 1):(n - 1)) {
            for (l in (k + 1):n) {
              name <- paste(
                list_names[i], "∩",
                list_names[j], "∩",
                list_names[k], "∩",
                list_names[l]
              )
              overlaps[[name]] <- Reduce(intersect, gene_lists[c(i, j, k, l)])
            }
          }
        }
      }
    }

    if (n == 5) {
      name <- paste(list_names, collapse = " ∩ ")
      overlaps[[name]] <- Reduce(intersect, gene_lists)
    }

    overlaps
  }

  # ---- Generate analysis on click ----
  observeEvent(input$generate, {
    if (is.na(input$num_datasets) || input$num_datasets < 2 || input$num_datasets > 5) {
      showNotification("⚠️ Please enter a valid number of datasets (2-5).",
        type = "warning", duration = 5
      )
      return(NULL)
    }

    n_datasets <- input$num_datasets

    if (input$input_method == "file") {
      # FILE UPLOAD METHOD
      req(input$gene_col, input$padj_col)

      uploaded_files <- lapply(1:n_datasets, function(i) input[[paste0("file", i)]])
      if (any(vapply(uploaded_files, is.null, logical(1)))) {
        showNotification("⚠️ Please upload all selected files.", type = "warning", duration = 5)
        return(NULL)
      }

      if (input$use_lfc && (is.null(input$lfc_col) || input$lfc_col == "")) {
        showNotification("⚠️ Please specify Log2FC column when Log2FC filter is enabled.",
          type = "warning", duration = 5
        )
        return(NULL)
      }

      lists <- list()
      gene_names_map <- list()

      withProgress(message = "Processing files...", value = 0, {
        for (i in 1:n_datasets) {
          incProgress(1 / n_datasets, detail = paste("Processing dataset", i))

          file <- input[[paste0("file", i)]]
          sep_i <- input[[paste0("sep", i)]]
          sheet_i <- input[[paste0("sheet", i)]]

          tryCatch(
            {
              df <- read_uploaded_file(file, sep_i, sheet_i)

              if (is.null(df)) {
                next
              }

              if (!input$gene_col %in% colnames(df)) {
                showNotification(
                  paste(
                    "⚠️ Gene ID column '", input$gene_col,
                    "' not found in file:", file$name
                  ),
                  type = "warning", duration = 5
                )
                next
              }

              if (!input$padj_col %in% colnames(df)) {
                showNotification(
                  paste(
                    "⚠️ P-adj column '", input$padj_col,
                    "' not found in file:", file$name
                  ),
                  type = "warning", duration = 5
                )
                next
              }

              if (input$use_lfc && !input$lfc_col %in% colnames(df)) {
                showNotification(
                  paste(
                    "⚠️ Log2FC column '", input$lfc_col,
                    "' not found in file:", file$name
                  ),
                  type = "warning", duration = 5
                )
                next
              }

              dataset_name <- file_path_sans_ext(basename(file$name))

              # Apply filters with proper type conversion
              if (input$use_lfc) {
                sig_df <- df %>%
                  mutate(
                    padj_numeric = suppressWarnings(as.numeric(as.character(.data[[input$padj_col]]))),
                    lfc_numeric = suppressWarnings(as.numeric(as.character(.data[[input$lfc_col]])))
                  ) %>%
                  filter(
                    !is.na(padj_numeric),
                    !is.na(lfc_numeric),
                    padj_numeric < input$padj_cutoff,
                    abs(lfc_numeric) > input$lfc_cutoff
                  )

                # Apply gene direction filter
                if (input$gene_direction == "up") {
                  sig_df <- sig_df %>% filter(lfc_numeric > 0)
                } else if (input$gene_direction == "down") {
                  sig_df <- sig_df %>% filter(lfc_numeric < 0)
                }
              } else {
                sig_df <- df %>%
                  mutate(
                    padj_numeric = suppressWarnings(as.numeric(as.character(.data[[input$padj_col]])))
                  ) %>%
                  filter(
                    !is.na(padj_numeric),
                    padj_numeric < input$padj_cutoff
                  )
              }

              sig_genes <- unique(as.character(sig_df[[input$gene_col]]))
              sig_genes <- sig_genes[!is.na(sig_genes) & sig_genes != ""]

              if (length(sig_genes) == 0) {
                showNotification(paste("⚠️ No significant genes found in:", file$name),
                  type = "warning", duration = 5
                )
              }

              lists[[dataset_name]] <- sig_genes

              if (!is.null(input$gene_name_col) &&
                input$gene_name_col != "" &&
                input$gene_name_col %in% colnames(sig_df)) {
                gene_name_map <- setNames(
                  as.character(sig_df[[input$gene_name_col]]),
                  sig_genes
                )
                gene_names_map[[dataset_name]] <- gene_name_map
              }
            },
            error = function(e) {
              showNotification(
                paste("❌ Error processing file:", file$name, "\nDetails:", e$message),
                type = "error",
                duration = 10
              )
            }
          )
        }
      })

      gene_names_data(gene_names_map)
    } else {
      # PASTE GENES METHOD
      lists <- list()

      withProgress(message = "Processing gene lists...", value = 0, {
        for (i in 1:n_datasets) {
          incProgress(1 / n_datasets, detail = paste("Processing dataset", i))

          genes_text <- input[[paste0("genes", i)]]
          dataset_name <- input[[paste0("dataset_name", i)]]

          if (is.null(genes_text) || genes_text == "") {
            showNotification(paste("⚠️ Dataset", i, "is empty. Please paste gene IDs."),
              type = "warning", duration = 5
            )
            next
          }

          if (is.null(dataset_name) || dataset_name == "") {
            dataset_name <- paste("Dataset", i)
          }

          genes <- strsplit(genes_text, "\n")[[1]]
          genes <- trimws(genes)
          genes <- genes[genes != ""]
          genes <- unique(genes)

          if (length(genes) == 0) {
            showNotification(paste("⚠️ No genes found in Dataset", i),
              type = "warning", duration = 5
            )
            next
          }

          lists[[dataset_name]] <- genes
        }
      })

      gene_names_data(NULL)
    }

    if (length(lists) == 0) {
      showNotification("❌ No valid data to analyze. Please check your input.",
        type = "error", duration = 10
      )
      return(NULL)
    }

    if (!is.null(names(lists)) && any(duplicated(names(lists)))) {
      names(lists) <- make.unique(names(lists))
      showNotification("⚠️ Duplicate dataset names detected – names were made unique automatically.",
        type = "warning", duration = 5
      )
    }

    if (length(lists) < 2) {
      showNotification("❌ At least 2 valid datasets are required for diagram.",
        type = "error", duration = 10
      )
      return(NULL)
    }

    gene_lists(lists)

    overlaps <- calculate_overlaps(lists)
    all_overlaps(overlaps)

    updateSelectInput(session, "overlap_select",
      choices = names(overlaps)
    )

    n <- length(lists)
    diagram_settings$labels <- names(lists)
    diagram_settings$colors <- c(
      "#0073C2FF", "#EFC000FF", "#868686FF",
      "#CD534CFF", "#7AA6DCFF"
    )[1:n]

    updateNumericInput(session, "label_font_size", value = 1.0)
    updateNumericInput(session, "number_font_size", value = 1.5)

    showNotification("✅ Analysis complete!", type = "message", duration = 3)
  })

  # ---- Dynamic UI: labels ----
  output$label_inputs <- renderUI({
    req(diagram_settings$labels)
    labels <- diagram_settings$labels

    tagList(
      lapply(seq_along(labels), function(i) {
        textInput(
          paste0("label_", i),
          label = paste("Set", i, "label:"),
          value = labels[i]
        )
      })
    )
  })

  # ---- Dynamic UI: colors ----
  output$color_inputs <- renderUI({
    req(diagram_settings$colors, diagram_settings$labels)
    colors <- diagram_settings$colors
    labels <- diagram_settings$labels

    tagList(
      lapply(seq_along(colors), function(i) {
        colourInput(
          paste0("color_", i),
          label = paste("Set", i, "color:", labels[i]),
          value = colors[i]
        )
      })
    )
  })

  # ---- Dynamic UI: Sankey colors ----
  output$sankey_color_inputs <- renderUI({
    req(diagram_settings$colors, diagram_settings$labels)
    colors <- diagram_settings$colors
    labels <- diagram_settings$labels

    tagList(
      lapply(seq_along(colors), function(i) {
        colourInput(
          paste0("sankey_color_", i),
          label = paste("Node", i, "color:", labels[i]),
          value = colors[i]
        )
      })
    )
  })

  # ---- Helper: gather current plot settings ----
  `%||%` <- function(x, y) if (is.null(x)) y else x

  get_plot_settings <- function() {
    req(gene_lists())
    lists <- gene_lists()
    n <- length(lists)

    plot_labels <- sapply(seq_along(diagram_settings$labels), function(i) {
      input[[paste0("label_", i)]] %||% diagram_settings$labels[i]
    })

    plot_colors <- sapply(seq_along(diagram_settings$colors), function(i) {
      input[[paste0("color_", i)]] %||% diagram_settings$colors[i]
    })

    plot_labels <- head(c(plot_labels, names(lists)), n)
    plot_colors <- head(c(plot_colors, diagram_settings$colors), n)

    plot_label_size <- input$label_font_size %||% 1.2
    plot_number_size <- input$number_font_size %||% 1.5
    show_percent <- input$show_percent %||% TRUE

    # Removed font family selection logic as requested

    if (length(plot_labels) == n) {
      names(lists) <- plot_labels
    }

    list(
      lists        = lists,
      n            = n,
      labels       = plot_labels,
      colors       = plot_colors,
      label_size   = plot_label_size,
      number_size  = plot_number_size,
      show_percent = show_percent
    )
  }

  # ---- Main plot output ----
  output$main_plot <- renderPlot({
    req(gene_lists(), input$diagram_type)
    settings <- get_plot_settings()

    tryCatch(
      {
        if (input$diagram_type == "venn") {
          # VENN DIAGRAM
          if (settings$n == 2 || settings$n == 3) {
            p <- ggvenn(
              settings$lists,
              fill_color      = settings$colors,
              stroke_size     = 0.5,
              set_name_size   = settings$label_size * 4,
              text_size       = settings$number_size * 2.6,
              show_percentage = settings$show_percent
            )
            print(p)
          } else {
            venn.plot <- venn.diagram(
              x               = settings$lists,
              filename        = NULL,
              fill            = settings$colors,
              alpha           = 0.5,
              cex             = settings$number_size,
              cat.cex         = settings$label_size,
              cat.default.pos = "outer",
              margin          = 0.1,
              disable.logging = TRUE
            )
            grid::grid.draw(venn.plot)
          }
        } else if (input$diagram_type == "upset") {
          # UPSET PLOT
          lists <- settings$lists
          all_genes <- unique(unlist(lists))

          mat <- sapply(lists, function(v) as.integer(all_genes %in% v))
          mat <- as.data.frame(mat)
          rownames(mat) <- all_genes

          # Get UpSet colors
          upset_main <- input$upset_main_bar_color %||% "#0073C2FF"
          upset_sets <- input$upset_sets_bar_color %||% "#EFC000FF"
          upset_matrix <- input$upset_matrix_color %||% "#404040"

          UpSetR::upset(
            mat,
            nsets = settings$n,
            nintersects = NA,
            order.by = "freq",
            sets.x.label = "Set Size",
            mainbar.y.label = "Intersection Size",
            main.bar.color = upset_main,
            sets.bar.color = upset_sets,
            matrix.color = upset_matrix,
            text.scale = c(1.3, 1.3, 1, 1, 1.5, 1.2)
          )
        } else if (input$diagram_type == "euler") {
          # EULER DIAGRAM
          lists <- settings$lists

          # Create combinations for euler
          euler_fit <- euler(lists)

          plot(euler_fit,
            fills = list(fill = settings$colors, alpha = 0.5),
            labels = list(
              fontsize = settings$label_size * 10
            ),
            quantities = list(
              fontsize = settings$number_size * 8
            ),
            legend = list(labels = settings$labels)
          )
        }
      },
      error = function(e) {
        showNotification(paste("Error generating diagram:", e$message),
          type = "error", duration = 10
        )
        plot.new()
        text(0.5, 0.5, paste("Error generating diagram\n", e$message),
          cex = 1.2, col = "red"
        )
      }
    )
  })

  # ---- Sankey diagram output ----
  output$sankey_plot <- renderSankeyNetwork({
    req(all_overlaps(), input$diagram_type)
    if (input$diagram_type != "sankey") {
      return(NULL)
    }

    tryCatch(
      {
        overlaps <- all_overlaps()
        lists <- gene_lists()
        settings <- get_plot_settings()

        # Get Sankey-specific colors
        sankey_colors <- sapply(seq_along(settings$labels), function(i) {
          input[[paste0("sankey_color_", i)]] %||% settings$colors[i]
        })

        # Build nodes
        nodes <- data.frame(name = names(lists), stringsAsFactors = FALSE)
        links <- data.frame(source = integer(), target = integer(), value = integer())

        # For each pairwise intersection, create a link
        for (i in 1:(length(lists) - 1)) {
          for (j in (i + 1):length(lists)) {
            intersection_name <- paste(names(lists)[i], "∩", names(lists)[j])
            if (intersection_name %in% names(overlaps)) {
              link_value <- length(overlaps[[intersection_name]])
              if (link_value > 0) {
                links <- rbind(links, data.frame(
                  source = i - 1, # 0-indexed for networkD3
                  target = j - 1,
                  value = link_value
                ))
              }
            }
          }
        }

        if (nrow(links) == 0) {
          return(NULL)
        }

        # Create color scale for nodes
        color_js <- paste0(
          "d3.scaleOrdinal().domain([",
          paste0('"', nodes$name, '"', collapse = ","),
          "]).range([",
          paste0('"', sankey_colors, '"', collapse = ","),
          "])"
        )

        sankeyNetwork(
          Links = links,
          Nodes = nodes,
          Source = "source",
          Target = "target",
          Value = "value",
          NodeID = "name",
          NodeGroup = "name", # use node name as group so colours apply
          fontSize = 14,
          nodeWidth = 30,
          height = 600,
          width = 800,
          colourScale = htmlwidgets::JS(color_js), # wrap JS string
          sinksRight = TRUE,
          iterations = 32
        )
      },
      error = function(e) {
        showNotification(paste("Error generating Sankey diagram:", e$message),
          type = "error", duration = 10
        )
        NULL
      }
    )
  })

  # ---- Summary table ----
  output$summary_table <- renderTable(
    {
      req(gene_lists())
      lists <- gene_lists()

      data.frame(
        Dataset = names(lists),
        `Number of Genes` = sapply(lists, length),
        check.names = FALSE
      )
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE
  )

  # ---- Intersection table ----
  output$intersection_table <- renderTable(
    {
      req(all_overlaps())
      overlaps <- all_overlaps()
      intersection_names <- if (!is.null(overlaps)) names(overlaps)[grepl("∩", names(overlaps))] else character(0)

      intersection_df <- if (length(intersection_names) > 0) {
        data.frame(
          Intersection = intersection_names,
          Count        = sapply(overlaps[intersection_names], length),
          check.names  = FALSE
        )
      } else {
        data.frame(Intersection = "No intersections found", Count = NA)
      }

      intersection_df
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE
  )

  # ---- Gene table for selected overlap ----
  output$gene_table <- renderDT({
    req(all_overlaps(), input$overlap_select)

    genes <- all_overlaps()[[input$overlap_select]]

    if (length(genes) == 0) {
      return(datatable(
        data.frame(Message = "No genes in this overlap"),
        options = list(dom = "t")
      ))
    }

    gene_df <- data.frame(Gene_ID = genes, stringsAsFactors = FALSE)
    gene_names_map <- gene_names_data()

    if (!is.null(gene_names_map) && length(gene_names_map) > 0) {
      tryCatch(
        {
          all_names <- c()
          for (dataset in names(gene_names_map)) {
            if (!is.null(gene_names_map[[dataset]]) &&
              length(gene_names_map[[dataset]]) > 0) {
              all_names <- c(all_names, gene_names_map[[dataset]])
            }
          }

          if (length(all_names) > 0) {
            gene_df$Gene_Name <- vapply(genes, function(g) {
              tryCatch(
                {
                  if (g %in% names(all_names)) {
                    name <- all_names[[g]]
                    if (!is.null(name) && !is.na(name) &&
                      name != "" && name != "NA") {
                      return(as.character(name))
                    }
                  }
                  "N/A"
                },
                error = function(e) {
                  "N/A"
                }
              )
            }, character(1))
          }
        },
        error = function(e) {
          message("Error adding gene names: ", e$message)
        }
      )
    }

    datatable(
      gene_df,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel")
      ),
      rownames = FALSE,
      class = "cell-border stripe"
    )
  })

  # ---- File info ----
  output$file_info <- renderPrint({
    if (is.na(input$num_datasets) || input$num_datasets < 2 || input$num_datasets > 5) {
      cat("⚠️ Please enter a valid number of datasets (2-5)\n")
      return(NULL)
    }

    n <- input$num_datasets

    cat("╔═══════════════════════════════════════════\n")
    cat("  DATA INPUT INFORMATION\n")
    cat("╚═══════════════════════════════════════════\n\n")

    cat("Number of datasets specified:", n, "\n")
    cat("Input method:", ifelse(input$input_method == "file",
      "Upload DE result files",
      "Paste gene IDs/names"
    ), "\n\n")

    if (input$input_method == "file") {
      cat("Uploaded Files:\n")
      cat("───────────────────────────────────────────\n")
      for (i in 1:n) {
        file <- input[[paste0("file", i)]]
        if (!is.null(file)) {
          cat(sprintf("  Dataset %d: %s\n", i, file$name))
          cat(sprintf("    Size: %.2f KB\n", file$size / 1024))
          sheet <- input[[paste0("sheet", i)]]
          if (!is.null(sheet) && sheet != "") {
            cat(sprintf("    Sheet: %s\n", sheet))
          }
        } else {
          cat(sprintf("  Dataset %d: (not uploaded)\n", i))
        }
      }

      cat("\n╔═══════════════════════════════════════════\n")
      cat("  COLUMN MAPPINGS\n")
      cat("╚═══════════════════════════════════════════\n\n")
      cat("  Gene ID column:    ", input$gene_col %||% "Not set", "\n")
      cat(
        "  Gene Name column:  ",
        ifelse(is.null(input$gene_name_col) || input$gene_name_col == "",
          "Not specified", input$gene_name_col
        ), "\n"
      )
      cat("  P-adj column:      ", input$padj_col %||% "Not set", "\n")
      cat("  Log2FC column:     ", input$lfc_col %||% "Not set", "\n")

      cat("\n╔═══════════════════════════════════════════\n")
      cat("  FILTER SETTINGS\n")
      cat("╚═══════════════════════════════════════════\n\n")
      cat("  P-adj cutoff:       ", input$padj_cutoff, "\n")
      cat("  Log2FC cutoff:      ", input$lfc_cutoff, "\n")
      cat("  Apply Log2FC filter:", input$use_lfc, "\n")
      cat(
        "  Gene direction:     ",
        switch(input$gene_direction,
          "all" = "All significant genes",
          "up" = "Upregulated only",
          "down" = "Downregulated only"
        ), "\n"
      )
    } else {
      cat("Pasted Gene Lists:\n")
      cat("───────────────────────────────────────────\n")
      for (i in 1:n) {
        dataset_name <- input[[paste0("dataset_name", i)]]
        genes_text <- input[[paste0("genes", i)]]

        if (is.null(dataset_name) || dataset_name == "") {
          dataset_name <- paste("Dataset", i)
        }

        gene_count <- 0
        if (!is.null(genes_text) && genes_text != "") {
          genes <- strsplit(genes_text, "\n")[[1]]
          genes <- trimws(genes)
          genes <- genes[genes != ""]
          gene_count <- length(unique(genes))
        }

        cat(sprintf("  %s: %d genes\n", dataset_name, gene_count))
      }
    }

    if (!is.null(gene_lists())) {
      cat("\n╔═══════════════════════════════════════════\n")
      cat("  ANALYSIS RESULTS\n")
      cat("╚═══════════════════════════════════════════\n\n")
      lists <- gene_lists()
      for (name in names(lists)) {
        cat(sprintf("  %s: %d genes\n", name, length(lists[[name]])))
      }
    }
  })

  # ---- References in About ----
  output$references <- renderPrint({
    cat(
      'Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J, McPherson J, Dipert A, Borges B (2025).
_shiny: Web Application Framework for R_. R package version 1.11.1.

Attali D (2021). _shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds_.
R package version 2.1.0.

Attali D (2023). _colourpicker: A Colour Picker Tool for Shiny and for Selecting Colours in Plots_.
R package version 1.3.0.

Chen H (2022). _VennDiagram: Generate High-Resolution Venn and Euler Plots_.
R package version 1.7.3.

Yan L (2025). _ggvenn: Draw Venn Diagram by ggplot2_. R package version 0.1.19.

Wickham H, François R, Henry L, Müller K, Vaughan D (2023). _dplyr: A Grammar of Data Manipulation_.
R package version 1.1.4.

Xie Y, Cheng J, Tan X, Aden-Buie G (2025). _DT: A Wrapper of the JavaScript Library DataTables_.
R package version 0.34.0.

Perrier V, Meyer F, Granjon D (2025). _shinyWidgets: Custom Inputs Widgets for Shiny_.
R package version 0.9.0.

Wickham H, Bryan J (2025). _readxl: Read Excel Files_. R package version 1.4.5.

Schauberger P, Walker A (2025). _openxlsx: Read, Write and Edit xlsx Files_.
R package version 4.2.8.1.

Conway JR, Lex A, Gehlenborg N (2017). "UpSetR: an R package for the visualization of intersecting sets and their properties."
_Bioinformatics_, 33(18), 2938-2940.

Larsson J (2024). _eulerr: Area-Proportional Euler and Venn Diagrams with Ellipses_.
R package version 7.0.2.

Allaire JJ, Gandrud C, Russell K, Yetman CJ (2017). _networkD3: D3 JavaScript Network Graphs from R_.
R package version 0.4.

Qiu Y, details. aotisSfAf (2024). _showtext: Using Fonts More Easily in R Graphs_.
R package version 0.9-7,
 
R Core Team (2025). _R: A Language and Environment for Statistical Computing_.
R Foundation for Statistical Computing, Vienna, Austria.
'
    )
  })

  # ---- Helper: Render and draw the plot ----
  draw_plot <- function(settings, diagram_type) {
    if (is.null(settings)) {
      stop("Plot settings are not available.")
    }

    if (diagram_type == "venn") {
      if (settings$n == 2 || settings$n == 3) {
        p <- ggvenn(
          settings$lists,
          fill_color      = settings$colors,
          stroke_size     = 0.5,
          set_name_size   = settings$label_size * 4,
          text_size       = settings$number_size * 2.6,
          show_percentage = settings$show_percent
        )
        print(p)
      } else {
        venn.plot <- venn.diagram(
          x               = settings$lists,
          filename        = NULL,
          fill            = settings$colors,
          alpha           = 0.5,
          cex             = settings$number_size,
          cat.cex         = settings$label_size,
          cat.default.pos = "outer",
          margin          = 0.1
        )
        grid.draw(venn.plot)
      }
    } else if (diagram_type == "upset") {
      lists <- settings$lists
      all_genes <- unique(unlist(lists))

      mat <- sapply(lists, function(v) as.integer(all_genes %in% v))
      mat <- as.data.frame(mat)
      rownames(mat) <- all_genes

      # Get UpSet colors
      upset_main <- input$upset_main_bar_color %||% "#0073C2FF"
      upset_sets <- input$upset_sets_bar_color %||% "#EFC000FF"
      upset_matrix <- input$upset_matrix_color %||% "#404040"

      UpSetR::upset(
        mat,
        nsets = settings$n,
        nintersects = NA,
        order.by = "freq",
        sets.x.label = "Set Size",
        mainbar.y.label = "Intersection Size",
        main.bar.color = upset_main,
        sets.bar.color = upset_sets,
        matrix.color = upset_matrix,
        text.scale = c(1.3, 1.3, 1, 1, 1.5, 1.2)
      )
    } else if (diagram_type == "euler") {
      lists <- settings$lists
      euler_fit <- euler(lists)

      plot(euler_fit,
        fills = list(fill = settings$colors, alpha = 0.5),
        labels = list(
          fontsize = settings$label_size * 10
        ),
        quantities = list(
          fontsize = settings$number_size * 8
        ),
        legend = list(labels = settings$labels)
      )
    }
  }

  # ---- Download plot (PNG) ----
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0(input$diagram_type, "_diagram_", Sys.Date(), ".png")
    },
    content = function(file) {
      if (input$diagram_type == "sankey") {
        showNotification("⚠️ Sankey diagrams cannot be downloaded as PNG. Use browser screenshot instead.",
          type = "warning", duration = 5
        )
        return(NULL)
      }

      tryCatch(
        {
          png(file, width = 800, height = 800, res = 120)
          showtext_opts(dpi = 120) # Match DPI for font rendering
          on.exit(if (dev.cur() > 1) dev.off(), add = TRUE)
          settings <- get_plot_settings()
          draw_plot(settings, input$diagram_type)
        },
        error = function(e) {
          showNotification(paste("Error saving plot:", e$message),
            type = "error", duration = 10
          )
        }
      )
    }
  )

  # ---- Download plot (SVG) ----
  output$download_svg <- downloadHandler(
    filename = function() {
      paste0(input$diagram_type, "_diagram_", Sys.Date(), ".svg")
    },
    content = function(file) {
      if (input$diagram_type == "sankey") {
        showNotification("⚠️ Sankey diagrams cannot be downloaded as SVG. Use browser screenshot instead.",
          type = "warning", duration = 5
        )
        return(NULL)
      }

      tryCatch(
        {
          svg(file, width = 8, height = 8)
          showtext_opts(dpi = 96) # Standard SVG DPI
          on.exit(if (dev.cur() > 1) dev.off(), add = TRUE)
          settings <- get_plot_settings()
          draw_plot(settings, input$diagram_type)
        },
        error = function(e) {
          showNotification(paste("Error saving SVG:", e$message),
            type = "error", duration = 10
          )
        }
      )
    }
  )

  # ---- Download Summary (TXT) ----
  output$download_summary_txt <- downloadHandler(
    filename = function() {
      paste0("overlap_summary_", Sys.Date(), ".txt")
    },
    content = function(file) {
      if (is.null(gene_lists()) || length(gene_lists()) == 0) {
        showNotification("⚠️ No summary data to download.", type = "warning", duration = 5)
        return(NULL)
      }

      tryCatch(
        {
          summary_df <- data.frame(
            Dataset = names(gene_lists()),
            `Number of Genes` = sapply(gene_lists(), length),
            check.names = FALSE
          )

          overlaps <- all_overlaps()
          intersection_names <- if (!is.null(overlaps)) names(overlaps)[grepl("∩", names(overlaps))] else character(0)

          intersection_df <- if (length(intersection_names) > 0) {
            data.frame(
              Intersection = intersection_names,
              Count        = sapply(overlaps[intersection_names], length),
              check.names  = FALSE
            )
          } else {
            data.frame(Intersection = "No intersections found", Count = NA)
          }

          writeLines(c(
            "Overlap Summary",
            "===============",
            "\nNumber of Genes per Set:", capture.output(print(summary_df, row.names = FALSE)),
            "\nIntersection Counts:", capture.output(print(intersection_df, row.names = FALSE))
          ), file)
        },
        error = function(e) {
          showNotification(paste("❌ Error creating summary file:", e$message), type = "error", duration = 10)
        }
      )
    }
  )

  # ---- Download overlaps (CSV) ----
  output$download_overlaps <- downloadHandler(
    filename = function() {
      paste0("gene_overlaps_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(all_overlaps())

      tryCatch(
        {
          overlaps <- all_overlaps()
          gene_names_map <- gene_names_data()

          all_names <- c()
          if (!is.null(gene_names_map) && length(gene_names_map) > 0) {
            for (dataset in names(gene_names_map)) {
              if (!is.null(gene_names_map[[dataset]]) &&
                length(gene_names_map[[dataset]]) > 0) {
                all_names <- c(all_names, gene_names_map[[dataset]])
              }
            }
          }

          out_list <- lapply(names(overlaps), function(name) {
            genes <- overlaps[[name]]
            if (length(genes) == 0) {
              return(NULL)
            }
            df <- data.frame(
              Overlap = name,
              Gene_ID = genes,
              stringsAsFactors = FALSE
            )

            if (length(all_names) > 0) {
              df$Gene_Name <- vapply(df$Gene_ID, function(g) {
                if (g %in% names(all_names)) {
                  nm <- all_names[[g]]
                  if (!is.null(nm) && !is.na(nm) &&
                    nm != "" && nm != "NA") {
                    return(as.character(nm))
                  }
                }
                "N/A"
              }, character(1))
            } else {
              df$Gene_Name <- NA_character_
            }
            df
          })

          out_list_nonnull <- Filter(Negate(is.null), out_list)
          if (length(out_list_nonnull) == 0) {
            out_df <- data.frame(Overlap = character(0), Gene_ID = character(0), Gene_Name = character(0), stringsAsFactors = FALSE)
          } else {
            out_df <- do.call(rbind, out_list_nonnull)
          }
          write.csv(out_df, file, row.names = FALSE)
        },
        error = function(e) {
          showNotification(paste("Error creating CSV:", e$message),
            type = "error", duration = 10
          )
        }
      )
    }
  )
}

shinyApp(ui = ui, server = server)
