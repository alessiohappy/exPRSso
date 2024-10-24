# app.R
library(shiny)
library(shinythemes)
library(plotly)
library(DT)
library(data.table)
library(dplyr)
library(tidyr)

# UI
ui <- fluidPage(options(shiny.maxRequestSize = 500 * 1024^2),
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
            .container-fluid { max-width: 1200px; }
            .well { background-color: #f8f9fa; }
            .btn-primary { background-color: #3498db; }
            .btn-primary:hover { background-color: #2980b9; }
            .status { padding: 10px; border-radius: 4px; margin-top: 10px; }
            .status-error { background-color: #ffebee; color: #c62828; }
            .status-success { background-color: #e8f5e9; color: #2e7d32; }
            .file-input { margin-bottom: 15px; }
            .results-section { margin-top: 30px; }
        "))
  ),
  
  # Titolo
  titlePanel("exPRSso"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # File inputs
      tags$div(class = "file-input",
               fileInput("mapFile", "Upload MAP File",
                         accept = c(".map"),
                         placeholder = "No file selected"
               )
      ),
      
      tags$div(class = "file-input",
               fileInput("pedFile", "Upload PED File",
                         accept = c(".ped"),
                         placeholder = "No file selected"
               )
      ),
      
      tags$div(class = "file-input",
               fileInput("statsFile", "Upload PRS summary",
                         accept = c(".txt"),
                         placeholder = "No file selected"
               )
      ),
      
      # Calculate button
      actionButton("calculateBtn", "Calculate PRS",
                   class = "btn-primary btn-block",
                   style = "margin-top: 20px;"
      ),
      
      # Download button (inizialmente nascosto)
      conditionalPanel(
        condition = "output.calculationComplete",
        downloadButton("downloadBtn", "Download Results (CSV)",
                       class = "btn-success btn-block",
                       style = "margin-top: 10px;"
        )
      ),
      
      # Status messages
      uiOutput("statusMsg")
    ),
    
    mainPanel(
      width = 9,
      # Tabs per i risultati
      tabsetPanel(
        tabPanel("Results",
                 conditionalPanel(
                   condition = "output.calculationComplete",
                   tags$div(class = "results-section",
                            plotlyOutput("prsPlot", height = "500px"),
                            tags$hr(),
                            DTOutput("resultsTable")
                   )
                 )
        ),
        tabPanel("Data Summary",
                 conditionalPanel(
                   condition = "output.calculationComplete",
                   verbatimTextOutput("dataSummary")
                 )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values per memorizzare i dati
  rv <- reactiveValues(
    scores = NULL,
    calculation_status = NULL
  )
  
  # Funzione per leggere il file MAP
  read_map_file <- function(file) {
    tryCatch({
      map_data <- fread(file$datapath, 
                        col.names = c("CHR", "SNP", "cM", "BP"))
      # Rimuovi duplicati
      map_data <- unique(map_data, by = "SNP")
      return(map_data)
    }, error = function(e) {
      stop("Error reading MAP file: ", e$message)
    })
  }
  
  # Funzione per leggere il file PED
  read_ped_file <- function(file, snp_names) {
    tryCatch({
      # Crea i nomi delle colonne
      col_names <- c("FID", "IID", "IDPa", "IDMa", "Sex", "Pheno", snp_names)
      
      # Leggi il file PED
      ped_data <- fread(file$datapath, 
                        col.names = col_names)
      return(ped_data)
    }, error = function(e) {
      stop("Error reading PED file: ", e$message)
    })
  }
  
  # Funzione per leggere il file delle statistiche
  read_stats_file <- function(file) {
    tryCatch({
      stats_data <- fread(file$datapath,
                          col.names = c("SNP", "EA", "OR"))
      return(stats_data)
    }, error = function(e) {
      stop("Error reading statistics file: ", e$message)
    })
  }
  
  # Funzione per calcolare il PRS
  calculate_prs <- function(ped_data, map_data, stats_data) {
    # Simula il calcolo del PRS (sostituire con il vero calcolo)
    n_samples <- nrow(ped_data)
    
    scores <- data.frame(
      IID = ped_data$IID,
      unweighted = rnorm(n_samples, mean = 5, sd = 1),
      weighted = rnorm(n_samples, mean = 10, sd = 2)
    )
    
    return(scores)
  }
  
  # Observer per il bottone di calcolo
  observeEvent(input$calculateBtn, {
    req(input$mapFile, input$pedFile, input$statsFile)
    
    # Reset status
    rv$calculation_status <- NULL
    
    withProgress(message = 'Calculating PRS...', value = 0, {
      tryCatch({
        # Leggi i file
        incProgress(0.2, detail = "Reading MAP file...")
        map_data <- read_map_file(input$mapFile)
        
        incProgress(0.2, detail = "Reading statistics file...")
        stats_data <- read_stats_file(input$statsFile)
        
        incProgress(0.2, detail = "Reading PED file...")
        snp_names <- rep(map_data$SNP, each = 2)
        ped_data <- read_ped_file(input$pedFile, snp_names)
        
        incProgress(0.2, detail = "Calculating scores...")
        rv$scores <- calculate_prs(ped_data, map_data, stats_data)
        
        incProgress(0.2, detail = "Finalizing...")
        rv$calculation_status <- "success"
        
      }, error = function(e) {
        rv$calculation_status <- paste("Error:", e$message)
      })
    })
  })
  
  # Output per i messaggi di stato
  output$statusMsg <- renderUI({
    if (is.null(rv$calculation_status)) return(NULL)
    
    if (rv$calculation_status == "success") {
      div(class = "status status-success",
          "PRS calculation completed successfully!")
    } else {
      div(class = "status status-error",
          rv$calculation_status)
    }
  })
  
  # Flag per indicare se il calcolo è completato
  output$calculationComplete <- reactive({
    return(!is.null(rv$scores) && rv$calculation_status == "success")
  })
  outputOptions(output, 'calculationComplete', suspendWhenHidden = FALSE)
  
  # Plot dei risultati
  output$prsPlot <- renderPlotly({
    req(rv$scores)
    
    plot_ly() %>%
      add_histogram(x = ~rv$scores$unweighted,
                    name = "Unweighted PRS",
                    opacity = 0.75) %>%
      add_histogram(x = ~rv$scores$weighted,
                    name = "Weighted PRS",
                    opacity = 0.75) %>%
      layout(barmode = "overlay",
             title = "PRS Score Distribution",
             xaxis = list(title = "Score"),
             yaxis = list(title = "Count"))
  })
  
  # Tabella dei risultati
  output$resultsTable <- renderDT({
    req(rv$scores)
    datatable(rv$scores,
              options = list(pageLength = 10,
                             scrollX = TRUE),
              rownames = FALSE)
  })
  
  # Summary dei dati
  output$dataSummary <- renderPrint({
    req(rv$scores)
    summary(rv$scores[, c("unweighted", "weighted")])
  })
  
  # Download handler
  output$downloadBtn <- downloadHandler(
    filename = function() {
      paste("prs_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$scores, file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
