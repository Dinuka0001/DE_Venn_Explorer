# DE Venn Explorer: An interactive tool for visualizing and comparing differential gene expression overlaps.
# Version (1.0) - Enhanced with improved error handling and aesthetics
# By Dinuka Adasooriya, Yonsei University College of Dentistry, Seoul, Korea

# ---- Load Required Packages ----
# Note: Packages are installed by the server based on manifest.json. 
# We only need to load them here.

library(shiny)
library(shinyjs)       # For showing/hiding the edit panel and other controls
library(colourpicker)  # For the color input
library(VennDiagram)   # For 4-5 set Venn diagrams
library(ggvenn)        # For 2-3 set Venn diagrams
library(dplyr)         # For data manipulation
library(DT)            # For interactive data tables
library(shinyWidgets)  # For modern UI elements
library(readxl)        # For reading Excel files (.xlsx)
library(openxlsx)      # Alternative for Excel files
library(tools)         # For file extension handling
library(grid)          # For grid.draw in VennDiagram

# Set maximum file upload size to 50MB
options(shiny.maxRequestSize = 50 * 1024^2)

# Set maximum file upload size to 50MB
options(shiny.maxRequestSize = 50 * 1024^2)

# ---- UI ----
ui <- fluidPage(
  titlePanel("DE Venn Explorer"),
  
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
        width: 350px;
      }
      hr {
        border-top: 1px solid #dee2e6;
        margin: 20px 0;
      }
      .form-control:focus {
        border-color: #0073C2;
        box-shadow: 0 0 0 0.2rem rgba(0, 115, 194, 0.25);
      }
    "))
  ),
  
  useShinyjs(),
  
  sidebarLayout(
    sidebarPanel(
      h4("📤 Upload DE Results"),
      
      helpText(
        icon("info-circle"),
        "Recommended: Use .xlsx or .csv formats. If you have .xls files and encounter errors, ",
        "please save them as .xlsx or .csv in Excel first."
      ),
      
      numericInput("num_files", "Number of files (2–5):",
                   value = 2, min = 2, max = 5, step = 1),
      
      # Safety check message
      div(id = "file_count_warning", style = "display: none;",
          class = "alert alert-danger",
          icon("exclamation-triangle"),
          " Please enter a valid number of files (2-5)"
      ),
      
      # Dynamic upload UI
      uiOutput("file_upload_ui"),
      
      hr(),
      h4("🔧 Column Name Mapping"),
      
      selectizeInput("gene_col", "Gene ID Column Name:",
                     choices = NULL,
                     options = list(
                       create = TRUE,
                       placeholder = "Type or select column name"
                     )),
      
      selectizeInput("gene_name_col", "Gene Name Column (optional):",
                     choices = NULL,
                     options = list(
                       create = TRUE,
                       placeholder = "Type or select column name"
                     )),
      
      selectizeInput("padj_col", "Adjusted P-value Column Name:",
                     choices = NULL,
                     options = list(
                       create = TRUE,
                       placeholder = "Type or select column name"
                     )),
      
      selectizeInput("lfc_col", "Log2 Fold Change Column Name:",
                     choices = NULL,
                     options = list(
                       create = TRUE,
                       placeholder = "Type or select column name"
                     )),
      
      hr(),
      h4("⚙️ Filter Settings"),
      numericInput("padj_cutoff", "Adjusted P-value Cutoff:",
                   value = 0.05, min = 0, max = 1, step = 0.01),
      numericInput("lfc_cutoff", "Absolute Log2FC Cutoff:",
                   value = 1, min = 0, step = 0.1),
      checkboxInput("use_lfc", "Apply Log2FC filter", value = TRUE),
      
      hr(),
      actionButton("generate", "Generate Venn Diagram", 
                   class = "btn-primary btn-block", 
                   icon = icon("chart-area")),
      br(), br(),
      downloadButton("download_plot", "Download Diagram (PNG)", 
                     class = "btn-block"),
      downloadButton("download_svg", "Download Diagram (SVG)", 
                     class = "btn-block"),
      downloadButton("download_overlaps", "Download Overlaps (CSV)", 
                     class = "btn-block")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "📊 Venn Diagram",
          fluidRow(
            column(
              width = 8,
              plotOutput("venn_plot", height = "600px")
            ),
            column(
              width = 4,
              h4("🎨 Diagram Options"),
              actionButton("edit_diagram_btn",
                           "Show / Hide Diagram Options",
                           icon = icon("paint-brush"),
                           class = "btn-info btn-sm btn-block"),
              
              shinyjs::hidden(
                div(
                  id = "edit_panel",
                  h5("Font Sizes"),
                  numericInput("label_font_size", "Label Font Size:",
                               value = 1.2, min = 0.1, max = 5, step = 0.1),
                  numericInput("number_font_size", "Number Font Size:",
                               value = 1.5, min = 0.1, max = 5, step = 0.1),
                  helpText("For 2–3 sets: set_name_size / text_size (ggvenn). 
For 4–5 sets: cat.cex / cex (VennDiagram)."),
                  
                  hr(),
                  h5("Set Labels"),
                  uiOutput("label_inputs"),
                  
                  hr(),
                  h5("Set Colors"),
                  uiOutput("color_inputs"),
                  
                  hr(),
                  checkboxInput("show_percent",
                                "Show percentage values (for 2–3 set diagrams)",
                                value = TRUE)
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
          tableOutput("intersection_table")
        ),
        tabPanel(
          "📑 Gene Lists",
          selectInput("overlap_select", "Select Overlap:", choices = NULL),
          DTOutput("gene_table")
        ),
        tabPanel(
          "ℹ️ Uploaded Data Info",
          verbatimTextOutput("file_info")
        ),
        tabPanel(
          "ℹ️ About",
          h3("DE Venn Explorer"),
          p("An interactive tool for visualizing and comparing differential gene expression overlaps."),
          br(),
          p(strong("Developer:"), "Dinuka Adasooriya"),
          p("Division of Anatomy and Developmental Biology, Department of Oral Biology, BK21 FOUR Project,",
            "Yonsei University College of Dentistry, Seoul, Korea"),
          p(strong("Email:"), "dinuka90@yuhs.ac"),
          br(),
          h4("R packages used"),
          verbatimTextOutput("packages_versions")
        )
      )
    )
  ),
  
  footer = tags$footer(
    "DE Venn Explorer v2.1 | An interactive tool for visualizing and comparing differential gene expression overlaps",
    tags$br(),
    "© Dinuka Adasooriya | Yonsei University College of Dentistry"
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # Reactive values to store data
  gene_lists      <- reactiveVal(NULL)
  gene_names_data <- reactiveVal(NULL)
  all_overlaps    <- reactiveVal(NULL)
  
  # Diagram settings
  diagram_settings <- reactiveValues(
    labels = NULL,
    colors = NULL
  )
  
  # Toggle diagram edit panel
  observeEvent(input$edit_diagram_btn, {
    shinyjs::toggle("edit_panel")
  })
  
  # ---- Dynamic upload UI ----
  output$file_upload_ui <- renderUI({
    req(input$num_files)
    n <- input$num_files
    
    tagList(
      lapply(1:n, function(i) {
        wellPanel(
          h5(paste("📁 File", i)),
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
              "Auto"      = "auto",
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
  
  # ---- Sheet selection UI ----
  max_files <- 5
  for (ii in 1:max_files) {
    local({
      i <- ii
      output[[paste0("sheet_ui", i)]] <- renderUI({
        file <- input[[paste0("file", i)]]
        if (is.null(file)) return(NULL)
        
        ext <- tolower(file_ext(file$name))
        if (ext %in% c("xlsx", "xls")) {
          sheets <- tryCatch({
            # Enhanced Excel reading with proper file handling
            tmp_file <- tempfile(fileext = paste0(".", ext))
            file.copy(file$datapath, tmp_file, overwrite = TRUE)
            sheet_names <- excel_sheets(tmp_file)
            unlink(tmp_file)
            sheet_names
          }, error = function(e) {
            showNotification(paste("Error reading Excel sheets:", e$message), 
                             type = "error", duration = 5)
            NULL
          })
          
          if (is.null(sheets) || length(sheets) == 0) return(NULL)
          
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
    
    tryCatch({
      ext <- tolower(file_ext(file_input$name))
      
      if (ext %in% c("xlsx", "xls")) {
        # Create a proper temp file with the correct extension
        tmp_file <- tempfile(fileext = paste0(".", ext))
        
        # Copy the uploaded file to temp location
        success <- file.copy(file_input$datapath, tmp_file, overwrite = TRUE)
        
        if (!success) {
          stop("Failed to copy uploaded file to temporary location")
        }
        
        # Ensure the file exists and is readable
        if (!file.exists(tmp_file)) {
          stop("Temporary file does not exist")
        }
        
        sheet_to_read <- if (is.null(sheet_input) || sheet_input == "") 1 else sheet_input
        
        # Try multiple methods to read Excel files
        df <- NULL
        
        # Method 1: Try readxl (works best for .xlsx files)
        if (is.null(df) && ext == "xlsx") {
          df <- tryCatch({
            read_excel(
              path  = tmp_file,
              sheet = sheet_to_read,
              .name_repair = "minimal"
            )
          }, error = function(e) {
            message("readxl failed: ", e$message)
            NULL
          })
        }
        
        # Method 2: Try openxlsx (good for both .xlsx and some .xls)
        if (is.null(df)) {
          df <- tryCatch({
            if (ext == "xlsx") {
              openxlsx::read.xlsx(
                xlsxFile = tmp_file,
                sheet = sheet_to_read,
                check.names = FALSE
              )
            } else {
              NULL
            }
          }, error = function(e) {
            message("openxlsx failed: ", e$message)
            NULL
          })
        }
        
        # Method 3: For .xls files, try converting to .xlsx first
        if (is.null(df) && ext == "xls") {
          df <- tryCatch({
            # Try reading with readxl anyway (sometimes works)
            read_excel(
              path  = tmp_file,
              sheet = sheet_to_read,
              .name_repair = "minimal"
            )
          }, error = function(e) {
            message("Could not read .xls file: ", e$message)
            
            # Provide helpful error message
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
          })
        }
        
        if (is.null(df)) {
          stop("All methods to read Excel file failed. Please convert to .xlsx or .csv format.")
        }
        
        df <- as.data.frame(df, stringsAsFactors = FALSE)
        
        # Clean up temp file
        if (file.exists(tmp_file)) {
          unlink(tmp_file)
        }
        
        # Remove completely empty rows and columns
        df <- df[rowSums(is.na(df)) != ncol(df), ]
        df <- df[, colSums(is.na(df)) != nrow(df), drop = FALSE]
        
      } else {
        # Text-based files
        sep <- sep_input
        if (is.null(sep) || sep == "auto") {
          # Auto-detect separator
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
          file   = file_input$datapath,
          sep    = sep,
          header = TRUE,
          stringsAsFactors = FALSE,
          check.names = FALSE,
          quote = "\"",
          comment.char = "",
          fill = TRUE,
          na.strings = c("", "NA", "N/A", "null", "NULL")
        )
      }
      
      # Clean column names - remove leading/trailing whitespace
      colnames(df) <- trimws(colnames(df))
      
      # Ensure we have data
      if (nrow(df) == 0 || ncol(df) == 0) {
        stop("File appears to be empty or improperly formatted")
      }
      
      return(df)
      
    }, error = function(e) {
      # Return NULL and let the calling function handle the error message
      message("Error in read_uploaded_file: ", e$message)
      stop(e$message)
    })
  }
  
  # ---- Auto-detect column names & Clear Data on Upload ----
  observeEvent({
    file_triggers <- lapply(1:input$num_files, function(i) input[[paste0("file", i)]])
    sheet_triggers <- lapply(1:input$num_files, function(i) input[[paste0("sheet", i)]])
    list(file_triggers, sheet_triggers)
  }, {
    
    req(input$num_files)
    
    # Clear old data
    gene_lists(NULL)
    gene_names_data(NULL)
    all_overlaps(NULL)
    updateSelectInput(session, "overlap_select", choices = character(0))
    
    # Detect columns from first available file
    n <- input$num_files
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
    sep  <- input[[paste0("sep", first_index)]]
    sheet_input <- input[[paste0("sheet", first_index)]]
    
    df <- read_uploaded_file(file, sep, sheet_input)
    
    if (is.null(df)) return(NULL)
    
    col_names <- colnames(df)
    
    # Enhanced column detection patterns
    gene_id_defaults   <- c("gene_id", "gene", "Gene_ID", "GeneID", "ID", "ensembl_gene_id", "ensembl", "ENSEMBL")
    gene_name_defaults <- c("gene_name", "gene_symbol", "GeneName", "Symbol", "Name", "external_gene_name", "SYMBOL", "symbol")
    padj_defaults      <- c("padj", "adj.P.Val", "FDR", "p.adjust", "pvalue_adj", "P.Value.adj", "adjusted_pvalue", "qvalue")
    lfc_defaults       <- c("log2FoldChange", "logFC", "log2FC", "LFC", "FC", "fold_change", "foldChange")
    
    find_best_match <- function(cols, defaults) {
      for (default in defaults) {
        matches <- grep(paste0("^", default, "$"), cols, ignore.case = TRUE, value = TRUE)
        if (length(matches) > 0) return(matches[1])
      }
      for (default in defaults) {
        matches <- grep(default, cols, ignore.case = TRUE, value = TRUE)
        if (length(matches) > 0) return(matches[1])
      }
      return(NULL)
    }
    
    gene_id_match   <- find_best_match(col_names, gene_id_defaults)
    gene_name_match <- find_best_match(col_names, gene_name_defaults)
    padj_match      <- find_best_match(col_names, padj_defaults)
    lfc_match       <- find_best_match(col_names, lfc_defaults)
    
    updateSelectizeInput(
      session, "gene_col",
      choices  = col_names,
      selected = if (!is.null(gene_id_match)) gene_id_match else col_names[1],
      server   = TRUE
    )
    
    updateSelectizeInput(
      session, "gene_name_col",
      choices  = c("", col_names),
      selected = if (!is.null(gene_name_match)) gene_name_match else "",
      server   = TRUE
    )
    
    updateSelectizeInput(
      session, "padj_col",
      choices  = col_names,
      selected = if (!is.null(padj_match)) padj_match else col_names[1],
      server   = TRUE
    )
    
    updateSelectizeInput(
      session, "lfc_col",
      choices  = col_names,
      selected = if (!is.null(lfc_match)) lfc_match else col_names[1],
      server   = TRUE
    )
    
    showNotification("✓ Files changed. Data reset and columns detected.", 
                     type = "message", duration = 3)
  })
  
  # ---- Calculate overlaps ----
  calculate_overlaps <- function(gene_lists) {
    n <- length(gene_lists)
    list_names <- names(gene_lists)
    overlaps <- list()
    
    # Individual sets
    for (i in 1:n) {
      overlaps[[list_names[i]]] <- gene_lists[[i]]
    }
    
    # Pairwise intersections
    if (n >= 2) {
      for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
          name <- paste(list_names[i], "∩", list_names[j])
          overlaps[[name]] <- intersect(gene_lists[[i]], gene_lists[[j]])
        }
      }
    }
    
    # Three-way intersections
    if (n >= 3) {
      for (i in 1:(n - 2)) {
        for (j in (i + 1):(n - 1)) {
          for (k in (j + 1):n) {
            name <- paste(list_names[i], "∩",
                          list_names[j], "∩",
                          list_names[k])
            overlaps[[name]] <- Reduce(intersect, gene_lists[c(i, j, k)])
          }
        }
      }
    }
    
    # Four-way intersections
    if (n >= 4) {
      for (i in 1:(n - 3)) {
        for (j in (i + 1):(n - 2)) {
          for (k in (j + 1):(n - 1)) {
            for (l in (k + 1):n) {
              name <- paste(list_names[i], "∩",
                            list_names[j], "∩",
                            list_names[k], "∩",
                            list_names[l])
              overlaps[[name]] <- Reduce(intersect, gene_lists[c(i, j, k, l)])
            }
          }
        }
      }
    }
    
    # Five-way intersection
    if (n == 5) {
      name <- paste(list_names, collapse = " ∩ ")
      overlaps[[name]] <- Reduce(intersect, gene_lists)
    }
    
    overlaps
  }
  
  # ---- Generate analysis on click ----
  observeEvent(input$generate, {
    req(input$num_files, input$gene_col, input$padj_col)
    
    n_files <- input$num_files
    
    # Validate all files are uploaded
    uploaded_files <- lapply(1:n_files, function(i) input[[paste0("file", i)]])
    if (any(vapply(uploaded_files, is.null, logical(1)))) {
      showNotification("⚠️ Please upload all selected files.", type = "warning", duration = 5)
      return(NULL)
    }
    
    if (n_files < 2 || n_files > 5) {
      showNotification("⚠️ Number of files must be between 2 and 5.", type = "warning", duration = 5)
      return(NULL)
    }
    
    # Validate required columns
    if (input$use_lfc && (is.null(input$lfc_col) || input$lfc_col == "")) {
      showNotification("⚠️ Please specify Log2FC column when Log2FC filter is enabled.", 
                       type = "warning", duration = 5)
      return(NULL)
    }
    
    lists <- list()
    gene_names_map <- list()
    
    withProgress(message = 'Processing files...', value = 0, {
      
      for (i in 1:n_files) {
        incProgress(1/n_files, detail = paste("Processing file", i))
        
        file  <- input[[paste0("file", i)]]
        sep_i <- input[[paste0("sep", i)]]
        sheet_i <- input[[paste0("sheet", i)]]
        
        tryCatch({
          df <- read_uploaded_file(file, sep_i, sheet_i)
          
          if (is.null(df)) {
            next
          }
          
          # Validate required columns exist
          if (!input$gene_col %in% colnames(df)) {
            showNotification(paste("⚠️ Gene ID column '", input$gene_col, 
                                   "' not found in file:", file$name), 
                             type = "warning", duration = 5)
            next
          }
          
          if (!input$padj_col %in% colnames(df)) {
            showNotification(paste("⚠️ P-adj column '", input$padj_col, 
                                   "' not found in file:", file$name), 
                             type = "warning", duration = 5)
            next
          }
          
          if (input$use_lfc && !input$lfc_col %in% colnames(df)) {
            showNotification(paste("⚠️ Log2FC column '", input$lfc_col, 
                                   "' not found in file:", file$name), 
                             type = "warning", duration = 5)
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
          
          # Extract significant genes
          sig_genes <- unique(as.character(sig_df[[input$gene_col]]))
          sig_genes <- sig_genes[!is.na(sig_genes) & sig_genes != ""]
          
          if (length(sig_genes) == 0) {
            showNotification(paste("⚠️ No significant genes found in:", file$name), 
                             type = "warning", duration = 5)
          }
          
          lists[[dataset_name]] <- sig_genes
          
          # Extract gene names if available
          if (!is.null(input$gene_name_col) &&
              input$gene_name_col != "" &&
              input$gene_name_col %in% colnames(sig_df)) {
            
            gene_name_map <- setNames(
              as.character(sig_df[[input$gene_name_col]]),
              sig_genes
            )
            gene_names_map[[dataset_name]] <- gene_name_map
          }
          
        }, error = function(e) {
          showNotification(
            paste("❌ Error processing file:", file$name, "\nDetails:", e$message),
            type = "error",
            duration = 10
          )
        })
      }
    })
    
    # Check if we have valid data
    if (length(lists) == 0) {
      showNotification("❌ No valid data to analyze. Please check your files and settings.", 
                       type = "error", duration = 10)
      return(NULL)
    }
    
    if (length(lists) < 2) {
      showNotification("❌ At least 2 valid datasets are required for Venn diagram.", 
                       type = "error", duration = 10)
      return(NULL)
    }
    
    gene_lists(lists)
    gene_names_data(gene_names_map)
    
    overlaps <- calculate_overlaps(lists)
    all_overlaps(overlaps)
    
    updateSelectInput(session, "overlap_select",
                      choices = names(overlaps))
    
    # Default diagram settings
    n <- length(lists)
    diagram_settings$labels <- names(lists)
    diagram_settings$colors <- c(
      "#0073C2FF", "#EFC000FF", "#868686FF",
      "#CD534CFF", "#7AA6DCFF"
    )[1:n]
    
    # Reset font sizes
    updateNumericInput(session, "label_font_size", value = 1.2)
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
    
    plot_label_size  <- input$label_font_size %||% 1.2
    plot_number_size <- input$number_font_size %||% 1.5
    show_percent     <- input$show_percent %||% TRUE
    
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
  
  # ---- Plot ----
  output$venn_plot <- renderPlot({
    req(gene_lists())
    
    tryCatch({
      settings <- get_plot_settings()
      
      if (settings$n == 2 || settings$n == 3) {
        ggvenn(
          settings$lists,
          fill_color     = settings$colors,
          stroke_size    = 0.5,
          set_name_size  = settings$label_size * 4,
          text_size      = settings$number_size * 2.6,
          show_percentage = settings$show_percent
        )
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
    }, error = function(e) {
      showNotification(paste("Error generating plot:", e$message), 
                       type = "error", duration = 10)
      plot.new()
      text(0.5, 0.5, "Error generating Venn diagram\nCheck your data and settings", 
           cex = 1.2, col = "red")
    })
  })
  
  # ---- Summary table ----
  output$summary_table <- renderTable({
    req(gene_lists())
    lists <- gene_lists()
    
    data.frame(
      Dataset = names(lists),
      `Number of Significant Genes` = sapply(lists, length),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # ---- Intersection table ----
  output$intersection_table <- renderTable({
    req(all_overlaps())
    overlaps <- all_overlaps()
    intersection_names <- names(overlaps)[grepl("∩", names(overlaps))]
    
    if (length(intersection_names) == 0) {
      return(data.frame(Message = "No intersections calculated yet"))
    }
    
    data.frame(
      Intersection = intersection_names,
      Count        = sapply(overlaps[intersection_names], length),
      check.names  = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # ---- Gene table for selected overlap ----
  output$gene_table <- renderDT({
    req(all_overlaps(), input$overlap_select)
    
    genes <- all_overlaps()[[input$overlap_select]]
    
    if (length(genes) == 0) {
      return(datatable(
        data.frame(Message = "No genes in this overlap"),
        options = list(dom = 't')
      ))
    }
    
    gene_df <- data.frame(Gene_ID = genes, stringsAsFactors = FALSE)
    gene_names_map <- gene_names_data()
    
    if (!is.null(gene_names_map) && length(gene_names_map) > 0) {
      tryCatch({
        all_names <- c()
        for (dataset in names(gene_names_map)) {
          if (!is.null(gene_names_map[[dataset]]) &&
              length(gene_names_map[[dataset]]) > 0) {
            all_names <- c(all_names, gene_names_map[[dataset]])
          }
        }
        
        if (length(all_names) > 0) {
          gene_df$Gene_Name <- vapply(genes, function(g) {
            tryCatch({
              if (g %in% names(all_names)) {
                name <- all_names[[g]]
                if (!is.null(name) && !is.na(name) &&
                    name != "" && name != "NA") {
                  return(as.character(name))
                }
              }
              "N/A"
            }, error = function(e) {
              "N/A"
            })
          }, character(1))
        }
      }, error = function(e) {
        message("Error adding gene names: ", e$message)
      })
    }
    
    datatable(
      gene_df, 
      options = list(
        pageLength = 25, 
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    )
  })
  
  # ---- File info ----
  output$file_info <- renderPrint({
    req(input$num_files)
    n <- input$num_files
    
    cat("═══════════════════════════════════════════\n")
    cat("  FILE UPLOAD INFORMATION\n")
    cat("═══════════════════════════════════════════\n\n")
    
    cat("Number of files specified:", n, "\n\n")
    
    cat("Uploaded Files:\n")
    cat("───────────────────────────────────────────\n")
    for (i in 1:n) {
      file <- input[[paste0("file", i)]]
      if (!is.null(file)) {
        cat(sprintf("  File %d: %s\n", i, file$name))
        cat(sprintf("    Size: %.2f KB\n", file$size / 1024))
        sheet <- input[[paste0("sheet", i)]]
        if (!is.null(sheet) && sheet != "") {
          cat(sprintf("    Sheet: %s\n", sheet))
        }
      } else {
        cat(sprintf("  File %d: (not uploaded)\n", i))
      }
    }
    
    cat("\n═══════════════════════════════════════════\n")
    cat("  COLUMN MAPPINGS\n")
    cat("═══════════════════════════════════════════\n\n")
    cat("  Gene ID column:    ", input$gene_col %||% "Not set", "\n")
    cat("  Gene Name column:  ", 
        ifelse(is.null(input$gene_name_col) || input$gene_name_col == "", 
               "Not specified", input$gene_name_col), "\n")
    cat("  P-adj column:      ", input$padj_col %||% "Not set", "\n")
    cat("  Log2FC column:     ", input$lfc_col %||% "Not set", "\n")
    
    cat("\n═══════════════════════════════════════════\n")
    cat("  FILTER SETTINGS\n")
    cat("═══════════════════════════════════════════\n\n")
    cat("  P-adj cutoff:       ", input$padj_cutoff, "\n")
    cat("  Log2FC cutoff:      ", input$lfc_cutoff, "\n")
    cat("  Apply Log2FC filter:", input$use_lfc, "\n")
    
    if (!is.null(gene_lists())) {
      cat("\n═══════════════════════════════════════════\n")
      cat("  ANALYSIS RESULTS\n")
      cat("═══════════════════════════════════════════\n\n")
      lists <- gene_lists()
      for (name in names(lists)) {
        cat(sprintf("  %s: %d genes\n", name, length(lists[[name]])))
      }
    }
  })
  
  # ---- Packages versions in About ----
  output$packages_versions <- renderPrint({
    pkgs <- required_pkgs
    vers <- sapply(pkgs, function(p) as.character(packageVersion(p)))
    
    cat("═══════════════════════════════════════════\n")
    cat("  PACKAGE VERSIONS\n")
    cat("═══════════════════════════════════════════\n\n")
    
    for (i in seq_along(pkgs)) {
      cat(sprintf("  %-20s : %s\n", pkgs[i], vers[i]))
    }
  })
  
  # ---- Download plot (PNG) ----
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("venn_diagram_", Sys.Date(), ".png")
    },
    content = function(file) {
      tryCatch({
        png(file, width = 800, height = 800, res = 120)
        
        settings <- get_plot_settings()
        
        if (settings$n == 2 || settings$n == 3) {
          print(
            ggvenn(
              settings$lists,
              fill_color      = settings$colors,
              stroke_size     = 0.5,
              set_name_size   = settings$label_size * 4,
              text_size       = settings$number_size * 2.6,
              show_percentage = settings$show_percent
            )
          )
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
        
        dev.off()
      }, error = function(e) {
        showNotification(paste("Error saving plot:", e$message), 
                         type = "error", duration = 10)
      })
    }
  )
  
  # ---- Download plot (SVG) ----
  output$download_svg <- downloadHandler(
    filename = function() {
      paste0("venn_diagram_", Sys.Date(), ".svg")
    },
    content = function(file) {
      tryCatch({
        svg(file, width = 8, height = 8)
        
        settings <- get_plot_settings()
        
        if (settings$n == 2 || settings$n == 3) {
          print(
            ggvenn(
              settings$lists,
              fill_color      = settings$colors,
              stroke_size     = 0.5,
              set_name_size   = settings$label_size * 4,
              text_size       = settings$number_size * 2.6,
              show_percentage = settings$show_percent
            )
          )
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
        
        dev.off()
      }, error = function(e) {
        showNotification(paste("Error saving SVG:", e$message), 
                         type = "error", duration = 10)
      })
    }
  )
  
  # ---- Download overlaps (CSV) ----
  output$download_overlaps <- downloadHandler(
    filename = function() {
      paste0("gene_overlaps_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(all_overlaps())
      
      tryCatch({
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
        
        out_df <- do.call(rbind, out_list)
        write.csv(out_df, file, row.names = FALSE)
        
      }, error = function(e) {
        showNotification(paste("Error creating CSV:", e$message), 
                         type = "error", duration = 10)
      })
    }
  )
}


shinyApp(ui = ui, server = server)
