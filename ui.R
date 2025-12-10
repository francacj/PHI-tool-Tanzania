pacman::p_load(
  shiny,
  DT,
  shinyjs
)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .large-input input {
        height: 60px !important;
      }
      .table-actions { display: flex; gap: 8px; justify-content: flex-end; margin-bottom: 8px; }
      #annuler_modif {
        background-color: #a8bacc;
        color: white;
        border-color: #a8bacc;
      }
      table.dataTable tbody td { vertical-align: top; }
      .cell-fixed {
        max-height: 78px;
        overflow-y: auto;
        display: block;
        white-space: normal;
      }
      .info-icon {
        display: inline-block; margin-left:6px; width:18px; height:18px;
        line-height:18px; text-align:center; border-radius:50%;
        border:1px solid #17a2b8; color:#17a2b8; font-weight:700;
        font-family: Arial, sans-serif; cursor:help; font-size:12px;
      }
      .info-icon:hover { background:#e8f7fb; }
    "))
  ),
  titlePanel("Event Reporting Tool"),
  tags$script(HTML('Shiny.addCustomMessageHandler("show_export_count", function(message) {
    alert("Number of selected events: " + message);
  });')),
  sidebarLayout(
    sidebarPanel(
      div(style = "display: flex; gap: 10px; margin-top: 20px;",
          actionButton("edit", label = "Load for Editing", icon = icon("edit"), class = "btn btn-warning"),
          actionButton("delete", label = "Delete", icon = icon("trash"), class = "btn btn-danger")
      ),
      
      div(class = "large-input", textInput("title", "Event Title", width = "100%")),
      div(class = "large-input", textInput("regions", "Regions (Countries / regions mentioned)", width = "100%")),
      selectInput("category", "Alert Category", 
                  choices = c("", "National and International Alerts", "National Alerts", "International Alerts"),
                  selected = ""),
      selectInput("event_type", "Event Type", 
                  choices = c("", "Human", "Animal", "Environment", "Human and Animal", "Other"),
                  selected = ""),
      div(
        class = "large-input",
        textAreaInput(
          inputId = "sources",
          label = HTML(
            'Sources <span class="info-icon" title="Enter a source per line in the format: Name (link). Example: WHO (https://www.who.int)
ECDC (https://www.ecdc.europa.eu)">i</span>'
          ),
          width = "100%",
          rows = 4,
          placeholder = "One source per line:\nWHO (https://www.who.int)\nECDC (https://www.ecdc.europa.eu)"
        )
      ),
      
      dateInput("date", "Date of last data update", value = Sys.Date(), format = "yyyy-mm-dd", language = "en"),
      textAreaInput("situation", "Epidemiological Situation", rows = 10),
      textAreaInput("risk_assessment", "Risk Assessment", rows = 10),
      textAreaInput("measures", "Measures Taken", rows = 10),
      selectInput("health_relevance", "Public Health Relevance", 
                  choices = c("", "Very Low", "Low", "Moderate", "High"),
                  selected = ""),
      selectInput("travelers_relevance", "Relevance for Travelers", 
                  choices = c("", "Very Low", "Low", "Moderate", "High"),
                  selected = ""),
      div(class = "large-input", textInput("comments", "Comments", width = "100%")),
      tags$div(style = "background-color: #ffffcc; padding: 10px; border-radius: 5px;",
               textAreaInput("summary", "Disease Summary", rows = 10)),
      div(class = "large-input", textInput("other", "Other", width = "100%")),
      selectInput("status", "Event Status", choices = c("active", "completed")),
      
      tags$div(style = "margin-top: 20px;",
               h4("Add New Event"),
               actionButton("clear_mask", label = "Clear Mask", icon = icon("eraser"), class = "btn btn-default"),
               actionButton("save", label = "Add Event", icon = icon("plus"), class = "btn btn-success")
      ),
      
      div(style = "margin-top: 20px;",
          h4("Update / Save Event as New"),
          actionButton("update", label = "Save Event Modifications", icon = icon("refresh"), class = "btn btn-primary"),
          br(), br(),
          actionButton("save_new", label = "Save as New Event (Update of Previous Event)", icon = icon("copy"), class = "btn btn-info")
      ),
      
      div(style = "margin-top: 20px;",
          h4("Export Events"),
          actionButton("export_excel", label = "Export to Excel", icon = icon("file-excel"), class = "btn btn-outline-success", 
                       onclick = 'Shiny.setInputValue("count_selected_excel", Math.random())'),
          actionButton("export_word", label = "Generate Word Report", icon = icon("file-word"), class = "btn btn-outline-primary",
                       onclick = 'Shiny.setInputValue("count_selected_word", Math.random())')
      ),
    ),
    
    mainPanel(
      dateRangeInput("filter_date", "Filter by Date", start = Sys.Date() - 30, end = Sys.Date()),
      actionButton("reset_filter_date", "Reset Date Filter", icon = icon("undo")),
      
      fluidRow(
        column(
          width = 4,
          selectInput(
            inputId = "search_field",
            label   = "Search Field",
            choices = c(
              "All Fields"           = "all",
              "Title"                 = "Title",
              "Sources"               = "Sources",
              "Situation"             = "Situation",
              "Measures"              = "Measures",
              "Summary"               = "Summary",
              "Risk Assessment"       = "Risk_Assessment",
              "Comments"              = "Comments",
              "Other"                 = "Other",
              "Regions"               = "Regions",
              "EIOS_id"               = "EIOS_id"
            ),
            selected = "all"
          )
        ),
        column(
          width = 4,
          textInput(
            inputId = "search_text",
            label   = "Search Term",
            value   = ""
          )
        )
      ),
      
      div(class = "table-actions",
          actionButton("select_all", "Select All Events", icon = icon("check-double"),
                       class = "btn btn-outline-secondary btn-sm"),
          actionButton("clear_all",  "Deselect All Events", icon = icon("ban"),
                       class = "btn btn-outline-secondary btn-sm")
      ),
      
      tabsetPanel(id = "tabs",
                  tabPanel("Active",
                           DTOutput("active_events_table")
                  ),
                  tabPanel("Completed",
                           DTOutput("completed_events_table")
                  ),
                  tabPanel("EIOS",
                           DTOutput("eios_table")
                  )
      )
      
    )
  )
)
