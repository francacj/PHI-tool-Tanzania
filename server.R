pacman::p_load(
  shiny,
  shinyjs,
  DT,
  officer,
  flextable,
  readr,
  dplyr,
  writexl,
  magick,
  lubridate,
  stringr,
  renv
)

# ---- Link to store data ----
data_file <- "S:/Projekte/ZIG1_PHIRA/6_tool-tanzania/events_data.csv"

server <- function(input, output, session) {
  useShinyjs()
  
  # Define the data source (CSV)
  data <- reactiveVal({
    tryCatch({
      df <- readr::read_csv(
        data_file, show_col_types = FALSE,
        col_types = cols(
          Version = col_double(),
          Date = col_date(),
          Last_Modified = col_datetime()
        )
      )
      
      if (!"Images" %in% names(df)) df$Images <- NA_character_
      df
    }, error = function(e) {
      showModal(modalDialog("Error reading the CSV file: ", e$message))
      data.frame()
    })
  })
  
  # --- Active Events: Filter and Sort ---
  active_events_filtered <- reactive({
    df <- data()
    if (nrow(df) == 0) return(df)
    df <- dplyr::filter(df, Status == "active")
    if (!is.null(input$filter_date[1]) && !is.null(input$filter_date[2])) {
      df <- dplyr::filter(df, Date >= input$filter_date[1], Date <= input$filter_date[2])
    }
    df <- dplyr::arrange(df, dplyr::desc(Date), dplyr::desc(Last_Modified))
    filter_by_text(df)
  })
  
  # --- Completed Events: Filter and Sort ---
  completed_events_filtered <- reactive({
    df <- data()
    if (nrow(df) == 0) return(df)
    df <- dplyr::filter(df, Status == "completed")
    if (!is.null(input$filter_date[1]) && !is.null(input$filter_date[2])) {
      df <- dplyr::filter(df, Date >= input$filter_date[1], Date <= input$filter_date[2])
    }
    df <- dplyr::arrange(df, dplyr::desc(Date), dplyr::desc(Last_Modified))
    filter_by_text(df)
  })
  
  # --- Generic Filter by Text ---
  filter_by_text <- function(df) {
    term <- input$search_text
    field <- input$search_field
    
    if (is.null(term) || !nzchar(trimws(term)) || nrow(df) == 0) {
      return(df)
    }
    
    term <- trimws(term)
    
    cols_all <- c("Title", "Sources", "Situation", "Other", "Regions", "EIOS_id")
    
    if (identical(field, "all")) {
      cols_exist <- intersect(cols_all, colnames(df))
      if (length(cols_exist) == 0) return(df)
      idx <- apply(
        as.data.frame(df[, cols_exist, drop = FALSE], stringsAsFactors = FALSE),
        1,
        function(line) {
          any(grepl(term, line, ignore.case = TRUE))
        }
      )
      df[idx, , drop = FALSE]
    } else {
      if (!field %in% colnames(df)) return(df)
      df[grepl(term, df[[field]], ignore.case = TRUE), , drop = FALSE]
    }
  }
  
  # --- Add a New Event ---
  observeEvent(input$save, {
    new_id <- sprintf("EVT_%s_%03d", format(Sys.Date(), "%Y%m%d"),
                      sum(grepl(format(Sys.Date(), "%Y%m%d"), data()$ID_Event)) + 1)
    
    new_event <- data.frame(
      ID_Event = new_id,
      Update = "New event",
      Version = 1,
      Date = input$date,
      Last_Modified = force_tz(Sys.time(), "Africa/Casablanca"),
      Title = input$title,
      Event_Type = input$event_type,
      Category = input$category,
      Regions = input$regions,
      Sources = input$sources,
      Situation = input$situation,
      Risk_Assessment = input$risk_assessment,
      Measures = input$measures,
      Relevance_Health = input$health_relevance,
      Relevance_Travelers = input$travelers_relevance,
      Comments = input$comments,
      Summary = input$summary,
      Other = input$other,
      Status = input$status,
      EIOS_id = NA_character_,
      stringsAsFactors = FALSE
    )
    
    data(bind_rows(data(), new_event))
    
    tryCatch({
      write_csv(data(), data_file)
    }, error = function(e) {
      showModal(modalDialog("Error while saving: ", e$message))
    })
    
    showModal(modalDialog(
      title = "Success",
      "The event has been added successfully.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    
    reset_fields()
  })
  
  # --- Reset Input Fields ---
  reset_fields <- function() {
    updateTextInput(session, "title", value = "")
    updateSelectInput(session, "event_type", selected = "")
    updateSelectInput(session, "category", selected = "")
    updateTextInput(session, "regions", value = "")
    updateTextInput(session, "sources", value = "")
    updateDateInput(session, "date", value = Sys.Date())
    updateTextAreaInput(session, "situation", value = "")
    updateTextAreaInput(session, "risk_assessment", value = "")
    updateTextAreaInput(session, "measures", value = "")
    updateSelectInput(session, "health_relevance", selected = "")
    updateSelectInput(session, "travelers_relevance", selected = "")
    updateTextInput(session, "comments", value = "")
    updateTextAreaInput(session, "summary", value = "")
    updateTextInput(session, "other", value = "")
    updateSelectInput(session, "status", selected = "active")
  }
  
  # --- Edit Event ---
  observeEvent(input$edit, {
    tab <- input$tabs
    df_full <- data()
    
    if (identical(tab, "Active")) {
      selection <- input$active_events_table_rows_selected
      df_view <- active_events_filtered()
    } else if (identical(tab, "Completed")) {
      selection <- input$completed_events_table_rows_selected
      df_view <- completed_events_filtered()
    } else {
      showModal(modalDialog("No active tab detected."))
      return()
    }
    
    if (length(selection) != 1) {
      showModal(modalDialog("Please select exactly one (1) event in the current tab."))
      return()
    }
    
    event <- df_view[selection, ]
    
    idx <- which(df_full$ID_Event == event$ID_Event)
    if (length(idx) != 1) {
      showModal(modalDialog("Unable to find the selected event in the complete dataset."))
      return()
    }
    
    # Fill form fields with event data
    updateTextInput(session, "title", value = event$Title)
    updateSelectInput(session, "event_type", selected = event$Event_Type)
    updateSelectInput(session, "category", selected = event$Category)
    updateTextInput(session, "regions", selected = event$Regions)
    updateTextInput(session, "sources", value = event$Sources)
    updateDateInput(session, "date", value = as.Date(event$Date))
    updateTextAreaInput(session, "situation", value = event$Situation)
    updateTextAreaInput(session, "risk_assessment", value = event$Risk_Assessment)
    updateTextAreaInput(session, "measures", value = event$Measures)
    updateSelectInput(session, "health_relevance", selected = event$Relevance_Health)
    updateSelectInput(session, "travelers_relevance", selected = event$Relevance_Travelers)
    updateTextInput(session, "comments", value = event$Comments)
    updateTextAreaInput(session, "summary", value = event$Summary)
    updateTextInput(session, "other", value = event$Other)
    
    session$userData$edit_index <- idx
  })
  
  # --- Update Event ---
  observeEvent(input$update, {
    idx <- session$userData$edit_index
    if (is.null(idx)) return()
    
    df <- data()
    
    if (idx > nrow(df)) {
      showModal(modalDialog("Error: the item to update no longer exists."))
      return()
    }
    
    df[idx, ] <- df[idx, ] %>%
      mutate(
        Title = input$title,
        Event_Type = input$event_type,
        Category = input$category,
        Regions = input$regions,
        Sources = input$sources,
        Date = input$date,
        Situation = input$situation,
        Risk_Assessment = input$risk_assessment,
        Measures = input$measures,
        Relevance_Health = input$health_relevance,
        Relevance_Travelers = input$travelers_relevance,
        Comments = input$comments,
        Summary = input$summary,
        Other = input$other,
        Status = input$status,
        Last_Modified = force_tz(Sys.time(), "Africa/Casablanca")
      )
    
    data(df)
    write_csv(df, data_file)
    session$userData$edit_index <- NULL
  })
  
  # --- Delete Event ---
  observeEvent(input$delete, {
    selected_rows <- input$active_events_table_rows_selected
    if (length(selected_rows) == 0) {
      showModal(modalDialog("No event selected.", easyClose = TRUE))
      return()
    }
    
    showModal(modalDialog(
      title = "Confirmation",
      paste("Are you sure you want to delete", length(selected_rows), "event(s)?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete", "Confirm", class = "btn btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete, {
    selected_rows <- input$active_events_table_rows_selected
    df <- data()
    
    # Deleting selected events
    df_new <- df[-selected_rows, ]
    data(df_new)
    
    # Update the file
    tryCatch({
      write_csv(df_new, data_file)
      showModal(modalDialog(
        title = "Success",
        paste(length(selected_rows), "event(s) have been deleted."),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }, error = function(e) {
      showModal(modalDialog("Error while deleting:", e$message))
    })
  })
  
  # --- Export Data to Excel ---
  observeEvent(input$export_excel, {
    dir.create("excel_outputs", showWarnings = FALSE)
    
    data_to_export <- active_events_filtered()
    selected_rows <- input$active_events_table_rows_selected
    if (length(selected_rows) > 0) {
      data_to_export <- data_to_export[selected_rows, ]
    }
    
    if (nrow(data_to_export) == 0) {
      showModal(modalDialog("No selected or active events to export."))
      return()
    }
    
    data_to_export <- data_to_export %>%
      mutate(Last_Modified = format(Last_Modified, "%Y-%m-%d %H:%M:%S"))
    
    filename <- sprintf("excel_outputs/events_%s.xlsx", format(Sys.Date(), "%Y-%m-%d"))
    tryCatch({
      writexl::write_xlsx(data_to_export, filename)
      showModal(modalDialog(
        title = "Success",
        paste("Excel file created successfully:", filename),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }, error = function(e) {
      showModal(modalDialog("Error while exporting:", e$message))
    })
  })
}
