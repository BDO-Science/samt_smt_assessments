library(shiny)
library(tidyverse)
library(here)
library(kableExtra)
library(flextable)
library(gt)
library(shinyjs)

# Source your salmon code
project <- here()
source(here(project, 'source_code/salmon_code.R'), echo = FALSE)

# Define UI
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  
  titlePanel("Assessment for Delta Operations on ESA and CESA-listed Salmonids"),
  
  sidebarLayout(
    sidebarPanel(
      h4("BDO Science Division"),
      p(strong("Report Date:"), textOutput("report_date", inline = TRUE)),
      hr(),
      actionButton("refresh", "Refresh Data", class = "btn-primary"),
      hr(),
      h5("Table of Contents"),
      tags$ul(
        tags$li(actionLink("link_exec", "Executive Summary")),
        tags$li(actionLink("link_ops", "Operational Conditions")),
        tags$li(actionLink("link_wr", "Natural Winter-run Chinook")),
        tags$li(actionLink("link_hwr", "Hatchery Winter-run")),
        tags$li(actionLink("link_sh", "Central Valley Steelhead")),
        tags$li(actionLink("link_loss", "Loss Thresholds")),
        tags$li(actionLink("link_refs", "References")),
        tags$li(actionLink("link_append", "Appendix"))
      )
    ),
    
    mainPanel(
      # Executive Summary
      h3(id = "exec_summary", "Executive Summary"),
      p("PRELIMINARY DATA"),
      hr(),
      
      # Operational and Regulatory Conditions
      h3(id = "ops_conditions", "Operational and Regulatory Conditions"),
      p("See weekly Outlook."),
      p(em("add hydrology graph(s) here?")),
      hr(),
      
      # Natural Winter-run Chinook
      h3(id = "wr_section", "Natural Winter-run Chinook"),
      uiOutput("wr_summary_text"),
      br(),
      h4("Timing Summary"),
      tableOutput("wr_timing_table"),
      
      h4("DCC Gate Closures"),
      p("Content for DCC gate closures..."),
      
      h4("Early Season Migration"),
      uiOutput("early_season_text"),
      hr(),
      
      # Hatchery Winter-run Chinook
      h3(id = "hwr_section", "Hatchery Winter-run Chinook"),
      uiOutput("hatchery_wr_text"),
      hr(),
      
      # Central Valley Steelhead
      h3(id = "sh_section", "Central Valley Steelhead"),
      uiOutput("sh_summary_text"),
      br(),
      h4("Timing Summary"),
      tableOutput("sh_timing_table"),
      hr(),
      
      # Loss Thresholds
      h3(id = "loss_thresholds", "Annual Loss Thresholds"),
      p("Content for annual loss thresholds..."),
      
      h3("Weekly Distributed Loss Threshold"),
      p("Content for weekly distributed loss threshold..."),
      hr(),
      
      # References
      h3(id = "references", "References"),
      p("References content..."),
      hr(),
      
      # Appendix
      h3(id = "appendix", "Appendix"),
      p(em("Dumping ground for graphs/tables like tillotson, STARs etc."))
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive value for report date
  report_date <- reactiveVal(Sys.Date())
  
  # Update date when refresh button is clicked
  observeEvent(input$refresh, {
    report_date(Sys.Date())
    # Re-source the data
    source(here(project, 'source_code/salmon_code.R'), echo = FALSE)
  })
  
  # Display report date
  output$report_date <- renderText({
    format(report_date(), "%B %d, %Y")
  })
  
  # Winter-run summary text
  output$wr_summary_text <- renderUI({
    yesterday <- format(report_date() - 1, "%b %d")
    
    delta_entry <- if_else(is.na(delta_entry_wr), '0%', delta_entry_wr)
    delta_exit <- if_else(is.na(delta_exit_wr), '0%', delta_exit_wr)
    salvage <- if_else(is.na(salvage_wr), '0%', salvage_wr)
    
    HTML(paste0(
      "Historically, as of ", yesterday, ", ",
      "<strong>", delta_entry, "</strong> of length-at-date (LAD) winter-run have entered the delta based on Knights Landing RST catch, ",
      "<strong>", delta_exit, "</strong> have exited the delta based on Chipps Island Trawl Catch, and ",
      "<strong>", salvage, "</strong> of DNA confirmed winter-run have been salvaged."
    ))
  })
  
  # Winter-run timing table
  output$wr_timing_table <- renderTable({
    wr_natural_timing
  }, caption = "Average Percent of annual emigrating population for unclipped LAD winter-run captured at the following locations and LAD and DNA confirmed winter-run salvaged at SWP and CVP Delta facilities for the past 10 years.",
  caption.placement = "top")
  
  # Early season text
  output$early_season_text <- renderUI({
    HTML(early_season_text)
  })
  
  # Hatchery winter-run text
  output$hatchery_wr_text <- renderUI({
    HTML(paste0(
      "As of ", format(report_date(), "%B %d, %Y"), 
      " 500,000 hatchery winter-run have been released into the Sacramento River on February 15, 2025 ",
      "and 50,000 have been released into Battle Creek on March 21, 2025. ",
      "JPEs are 80,000 and 10,000 for the respective releases with annual loss thresholds of ",
      "150 fish for the Sacramento Release and 10 fish for the Battle Creek Release."
    ))
  })
  
  # Steelhead summary text
  output$sh_summary_text <- renderUI({
    yesterday <- format(report_date() - 1, "%b %d")
    
    delta_entry <- if_else(is.na(delta_entry_sh), '0%', delta_entry_sh)
    delta_exit <- if_else(is.na(delta_exit_sh), '0%', delta_exit_sh)
    salvage <- if_else(is.na(salvage_sh), '0%', salvage_sh)
    
    HTML(paste0(
      "Historically, as of ", yesterday, ", ",
      "<strong>", delta_entry, "</strong> of CCV steelhead have entered the delta based on Knights Landing RST catch, ",
      "<strong>", delta_exit, "</strong> have exited the delta based on Chipps Island Trawl Catch, and ",
      "<strong>", salvage, "</strong> have been salvaged."
    ))
  })
  
  # Steelhead timing table
  output$sh_timing_table <- renderTable({
    sh_natural_timing
  }, caption = "Average Percent, with 95%CI, of annual emigrating population for unclipped CCV steelhead captured at the following locations and salvaged at SWP and CVP Delta facilities for the past 10 years.",
  caption.placement = "top")
  
  # Navigation links
  observeEvent(input$link_exec, {
    shinyjs::runjs("document.getElementById('exec_summary').scrollIntoView({behavior: 'smooth'});")
  })
  
  observeEvent(input$link_ops, {
    shinyjs::runjs("document.getElementById('ops_conditions').scrollIntoView({behavior: 'smooth'});")
  })
  
  observeEvent(input$link_wr, {
    shinyjs::runjs("document.getElementById('wr_section').scrollIntoView({behavior: 'smooth'});")
  })
  
  observeEvent(input$link_hwr, {
    shinyjs::runjs("document.getElementById('hwr_section').scrollIntoView({behavior: 'smooth'});")
  })
  
  observeEvent(input$link_sh, {
    shinyjs::runjs("document.getElementById('sh_section').scrollIntoView({behavior: 'smooth'});")
  })
  
  observeEvent(input$link_loss, {
    shinyjs::runjs("document.getElementById('loss_thresholds').scrollIntoView({behavior: 'smooth'});")
  })
  
  observeEvent(input$link_refs, {
    shinyjs::runjs("document.getElementById('references').scrollIntoView({behavior: 'smooth'});")
  })
  
  observeEvent(input$link_append, {
    shinyjs::runjs("document.getElementById('appendix').scrollIntoView({behavior: 'smooth'});")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)