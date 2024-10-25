# app.R
library(tidyverse)
library(R6)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(DT)

# uploading limits
options(shiny.maxRequestSize=600*1024^2) #600MB

# Define exPRSso class (R6Class object)
exPRSso <- R6::R6Class("exPRSso",
  public = list(
    # Class fields
    map_path = NULL,
    ped_path = NULL,
    risk_alleles_path = NULL,
    map_data = NULL,
    ped_data = NULL,
    risk_alleles = NULL,
    dosage_matrix = NULL,
    prs_scores = NULL,
    
    # Initialize
    initialize = function(map_path, ped_path, risk_alleles_path) {
      self$map_path <- map_path
      self$ped_path <- ped_path
      self$risk_alleles_path <- risk_alleles_path
    },
    
    # Read data
    read_data = function() {
      # Read MAP file
      self$map_data <- read.table(
        self$map_path,
        col.names = c("CHR", "SNP", "cM", "BP"),
        stringsAsFactors = FALSE
      )
      
      # Create colnames for PED file
      snp_list <- self$map_data$SNP
      ped_columns <- c("FID", "IID", "IDPa", "IDMa", "Sex", "Pheno")
      snp_columns <- paste0(rep(snp_list, each = 2), "_", 0:1)
      ped_columns <- c(ped_columns, snp_columns)
      
      # Read PED file
      self$ped_data <- read.table(
        self$ped_path,
        col.names = ped_columns,
        stringsAsFactors = FALSE
      )
      
      # Filter valid individuals
      self$ped_data <- self$ped_data[self$ped_data$FID > 0, ]
      
      # Read risk alleles
      self$risk_alleles <- read.table(
        self$risk_alleles_path,
        col.names = c("SNP", "EA", "OR"),
        sep = "\t",
        stringsAsFactors = FALSE
      )
      
      invisible(self)
    },
    
    # Calculate allelic dosage
    calculate_allelic_dosage = function() {
      # 1: risk allele dictionary
      risk_dict <- setNames(self$risk_alleles$EA, self$risk_alleles$SNP)
      
      # 2: Extract genetic data
      genetic_data <- self$ped_data[, 7:ncol(self$ped_data)]
      
      # 3: Initialize dosage matrix
      dosage_matrix <- matrix(0,
        nrow = nrow(genetic_data),
        ncol = length(unique(self$map_data$SNP)))
      colnames(dosage_matrix) <- unique(self$map_data$SNP)
      
      # Calculate dosages
      for (snp in unique(self$map_data$SNP)) {
        if (snp %in% names(risk_dict)) {
          col1 <- paste0(snp, "_0")
          col2 <- paste0(snp, "_1")
          
          matches <- (genetic_data[[col1]] == risk_dict[snp]) +
            (genetic_data[[col2]] == risk_dict[snp])
          
          dosage_matrix[, snp] <- matches}
      }
      
      self$dosage_matrix <- as.data.frame(dosage_matrix)
      invisible(self)
    },
    
    # Calculate PRS
    calculate_prs = function() {
      # Unweighted score
      unweighted <- rowSums(self$dosage_matrix)
      
      # Weighted score
      or_dict <- setNames(self$risk_alleles$OR, self$risk_alleles$SNP)
      weighted_matrix <- sweep(
        self$dosage_matrix,
        2,
        or_dict[colnames(self$dosage_matrix)],
        "*"
      )
      weighted <- rowSums(weighted_matrix)
      
      # Combine scores
      self$prs_scores <- data.frame(
        IID = self$ped_data$IID,
        unweighted = unweighted,
        weighted = weighted
      )
      
      invisible(self)
    },
    
    # Plot distributions
    plot_distributions = function(save_path = NULL) {
      # Boxplot
      boxplot <- ggplot(
        tidyr::pivot_longer(
          self$prs_scores,
          cols = c("unweighted", "weighted")
        ),
        aes(x = name, y = value)
      ) +
        geom_boxplot() +
        theme_minimal() +
        labs(x = "Score Type", y = "Value")
      
      if (!is.null(save_path)) {
        ggsave(
          file.path(save_path, "prs_boxplot.png"),
          boxplot,
          width = 8,
          height = 6,
          dpi = 400
        )
      }
      
      # Density plots
      for (score_type in c("unweighted", "weighted")) {
        density_plot <- ggplot(
          self$prs_scores,
          aes_string(x = score_type)
        ) +
          geom_density(fill = "grey80") +
          theme_minimal() +
          labs(
            x = paste(tools::toTitleCase(score_type), "score"),
            y = "Density"
          )
        
        if (!is.null(save_path)) {
          ggsave(
            file.path(save_path, paste0("prs_", score_type, "_distribution.png")),
            density_plot,
            width = 10,
            height = 8,
            dpi = 600)}
        }
    },
    
    # Save scores
    save_scores = function(output_path) {
      if (!is.null(self$prs_scores)) {
        write.csv(
          self$prs_scores,
          output_path,
          row.names = FALSE
        )
        cat("PRS scores saved to", output_path, "\n")
      } else {
        cat("Please calculate PRS scores first using calculate_prs()\n")}
    }
  )
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "exPRSso"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Data", tabName = "upload", icon = icon("upload")), # Tab1: Upload
      menuItem("Results", tabName = "results", icon = icon("chart-line")), # Tab2: Results
      menuItem("Download", tabName = "download", icon = icon("download")) # Tab3: Download
    )
  ),
  
  dashboardBody(
    tabItems(
      # Upload tab
      tabItem(
        tabName = "upload",
        fluidRow(
          box(
            title = "Upload Files",
            width = 12,
            fileInput("map_file", "Choose MAP File",
                      accept = c(".map")),
            fileInput("ped_file", "Choose PED File",
                      accept = c(".ped")),
            fileInput("risk_file", "Choose Risk Alleles File",
                      accept = c(".txt")),
            actionButton("calculate", "Calculate PRS", 
                         class = "btn-primary")
          )
        )
      ),
      
      # Results tab
      tabItem(
        tabName = "results",
        fluidRow(
          box(
            title = "PRS Distributions",
            width = 12,
            plotOutput("boxplot")
          )
        ),
        fluidRow(
          box(
            title = "Unweighted PRS Distribution",
            width = 6,
            plotOutput("unweighted_dist")
          ),
          box(
            title = "Weighted PRS Distribution",
            width = 6,
            plotOutput("weighted_dist")
          )
        ),
        fluidRow(
          box(
            title = "PRS Scores",
            width = 12,
            DTOutput("scores_table")
          )
        )
      ),
      
      # Download tab
      tabItem(
        tabName = "download",
        fluidRow(
          box(
            title = "Download Results",
            width = 12,
            downloadButton("download_scores", "Download PRS Scores (CSV)"),
            downloadButton("download_plots", "Download Plots (ZIP)")
          )
        )
      )
    )
  )
)

# SERVER
server <- function(input, output, session) {
  # Reactive values 
  rv <- reactiveValues(
    calculator = NULL,
    calculation_done = FALSE
  )
  
  # PRS computation manager
  observeEvent(input$calculate, {
    req(input$map_file, input$ped_file, input$risk_file)
    
    withProgress(message = 'Calculating PRS...', value = 0, {
      # Temporarly save the uploaded files
      map_path <- input$map_file$datapath
      ped_path <- input$ped_file$datapath
      risk_path <- input$risk_file$datapath
      
      # New calculator instance
      rv$calculator <- exPRSso$new(map_path, ped_path, risk_path)
      
      incProgress(0.25, detail = "Reading data...")
      rv$calculator$read_data()
      
      incProgress(0.25, detail = "Calculating allelic dosage...")
      rv$calculator$calculate_allelic_dosage()
      
      incProgress(0.25, detail = "Calculating PRS...")
      rv$calculator$calculate_prs()
      
      incProgress(0.25, detail = "Done!")
      rv$calculation_done <- TRUE
    })
  })
  
  # Plots
  output$boxplot <- renderPlot({
    req(rv$calculation_done)
    boxplot_data <- tidyr::pivot_longer(
      rv$calculator$prs_scores,
      cols = c("unweighted", "weighted")
    )
    ggplot(boxplot_data, aes(x = name, y = value)) +
      geom_boxplot() +
      theme_minimal() +
      labs(x = "Score Type", y = "Value", title = "PRS Score Distribution")
  })
  
  output$unweighted_dist <- renderPlot({
    req(rv$calculation_done)
    ggplot(rv$calculator$prs_scores, aes(x = unweighted)) +
      geom_density(fill = "grey80") +
      theme_minimal() +
      labs(x = "Unweighted score", y = "Density")
  })
  
  output$weighted_dist <- renderPlot({
    req(rv$calculation_done)
    ggplot(rv$calculator$prs_scores, aes(x = weighted)) +
      geom_density(fill = "grey80") +
      theme_minimal() +
      labs(x = "Weighted score", y = "Density")
  })
  
  # Results table
  output$scores_table <- renderDT({
    req(rv$calculation_done)
    datatable(rv$calculator$prs_scores,
              options = list(pageLength = 10))
  })
  
  # Download handlers
  output$download_scores <- downloadHandler(
    filename = function() {
      paste("prs_scores_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$calculator$prs_scores, file, row.names = FALSE)
    }
  )
  
  output$download_plots <- downloadHandler(
    filename = function() {
      paste("prs_plots_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      # Temporary directory for the plots
      temp_dir <- tempdir()
      plots_dir <- file.path(temp_dir, "plots")
      dir.create(plots_dir)
      
      # Plots: save
      rv$calculator$plot_distributions(plots_dir)
      
      # zip if you want
      zip(file, files = list.files(plots_dir, full.names = TRUE))
    }
  )
}

# Start shinyapp
shinyApp(ui = ui, server = server)
