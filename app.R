library(shiny)
library(tidyverse)
library(ggrepel)
library(gridExtra)
library(glue)
library(tictoc)
library(patchwork)
library(grid)
library(googlesheets4)
library(xlsx)
library(lubridate)
library(shinyFiles)
library(purrr)
library(readxl)
library(readtext)
library(readtext)
gs4_deauth()
source("functions.R")
source("readMED.R")
source("signal_assessment.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MED Import & Signal QC"),
    sidebarLayout(
        sidebarPanel(
            # for MED import:
            shinyDirButton("medFolder", "Choose MED folder", "Upload MED folder"),
            helpText("All MED files in the folder will be processed automatically."),
            
            fileInput("varMapAA", "AA MED→R mapping (.xlsx)", accept = ".xlsx"),
            helpText("If left empty, will use 'defaults/RK_ActiveAvoidance_MEDtoR_variableNames.xlsx'"),
            
            fileInput("varMapShock", "Shock MED→R mapping (.xlsx)", accept = ".xlsx"),
            helpText("If left empty, will use 'defaults/RK_Shock.NoEscape_MEDtoR_variableNames.xlsx'"),
            
            actionButton("importMed", "Import MED"),
            hr(),
            
            # for signal QC:
            textInput("synapseURL", "Synapse evaluation sheet URL",
                      value = "https://docs.google.com/spreadsheets/d/1FNJXXd2jJ7Pbq_uwR8aN11q5FHH3kqWftUo_nug5vdM/edit?usp=sharing"),
            textInput("setupURL", "Setup metadata sheet URL",
                      value = "https://docs.google.com/spreadsheets/d/1VPx4tm8clOViS7U8v_KSBg8S7oL2NFnLAkSvROfv-6o/edit?usp=sharing"),
            textInput("operantRds", "Operant_Data.Rds path", value = "Combined_Operant_Data.Rds"),
            helpText("'Combined' will process both Active Avoidance and Inescapable Shock"),
            textInput("metaRds", "meta_Data.Rds path", value = "Combined_meta_Data.Rds"),
            helpText("'Combined' will process both Active Avoidance and Inescapable Shock"),
            numericInput("minDays", "Min keep-days", 0, min = 0),
            actionButton("qcSignal", "Assess Signal")
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("MED result", 
                         tableOutput("medMeta"), 
                         verbatimTextOutput("medOutSummary")
                ),
                tabPanel("Signal QC", 
                         tableOutput("signalTbl")
                )
            )
        )
    )
)

# Define server
server <- function(input, output, session) {
    roots <- c(
        R = "R:/Basic_Sciences/Phys/Lerner_Lab_tnl2633/Baran",
        C = "C:/"
    )
    shinyDirChoose(input, "medFolder", roots = roots, session = session)
    
    medResult <- eventReactive(input$importMed, {
        req(input$medFolder)
        
        # Get the base directory where the app is running
        app_dir <- getwd()
        
        # Default mapping files should be in the 'defaults' directory relative to the app
        default_aa_map <- file.path(app_dir, "defaults", "RK_ActiveAvoidance_MEDtoR_variableNames.xlsx")
        default_shock_map <- file.path(app_dir, "defaults", "RK_Shock.NoEscape_MEDtoR_variableNames.xlsx")
        
        # Get the MED files directory
        med_base_dir <- parseDirPath(roots, input$medFolder)
        if(length(med_base_dir) == 0) {
            stop("Please select a valid directory")
        }
        
        # Set up the actual MED file directories - don't add MedPC again
        aa_dir <- file.path(med_base_dir, "ActiveAvoidance")
        shock_dir <- file.path(med_base_dir, "ShockNoEscape")
        
        # Use uploaded mapping files if provided, otherwise use defaults
        var_map_aa_path <- if (!is.null(input$varMapAA)) {
            input$varMapAA$datapath
        } else {
            if (!file.exists(default_aa_map)) {
                stop("Default Active Avoidance mapping file not found at: ", default_aa_map, 
                     "\nPlease upload a mapping file or place the default file in the correct location.")
            }
            default_aa_map
        }
        
        var_map_shock_path <- if (!is.null(input$varMapShock)) {
            input$varMapShock$datapath
        } else {
            if (!file.exists(default_shock_map)) {
                stop("Default Shock NoEscape mapping file not found at: ", default_shock_map,
                     "\nPlease upload a mapping file or place the default file in the correct location.")
            }
            default_shock_map
        }
        
        # Print directories for debugging
        message("Processing directories:")
        message("AA dir: ", aa_dir)
        message("Shock dir: ", shock_dir)
        
        # Validate directories exist
        if (!dir.exists(aa_dir)) {
            stop("Active Avoidance directory not found at: ", aa_dir)
        }
        if (!dir.exists(shock_dir)) {
            stop("Shock NoEscape directory not found at: ", shock_dir)
        }
        
        withProgress(message = 'Processing files...', {
            result <- process_and_combine(
                parent_medpc_dir = med_base_dir,
                var_map_aa      = var_map_aa_path,
                var_map_shock   = var_map_shock_path,
                days_aa         = 7,
                days_shock      = 1
            )
        })
        result
    })
    
    # Now medResult() is a list with $operant, $meta and $settings
    output$medMeta <- renderTable({
        req(medResult())
        medResult()$meta
    })
    output$medOutSummary <- renderPrint({
        req(medResult())
        str(medResult()$operant)
    })
    
    signalTbl <- eventReactive(input$qcSignal, {
        assess_signal_quality(
            synapse_eval_link   = input$synapseURL,
            setup_metadata_link = input$setupURL,
            operant_data_rds    = input$operantRds,
            meta_data_rds       = input$metaRds,
            min_days            = input$minDays
        )
    })
    output$signalTbl <- renderTable({ req(signalTbl()); signalTbl() })
}

# Run the application 
shinyApp(ui = ui, server = server)
