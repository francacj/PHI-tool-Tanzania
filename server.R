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
fichier_donnees <- "events_data.csv"


server <- function(input, output, session) {
  useShinyjs()

  # Reactive file reader: when events_data.csv changes (any client or external edit),
  # all clients see the update. Uses cheap mtime check every 2s, re-reads only when changed.
  donnees <- reactivePoll(
    intervalMillis = 2000L,
    checkFunc = function() {
      fi <- file.info(fichier_donnees)
      if (is.na(fi$mtime)) return(0)
      paste(fi$mtime, fi$size, sep = "_")
    },
    valueFunc = function() {
      tryCatch({
        df_lu <- readr::read_csv(
          fichier_donnees, show_col_types = FALSE,
          col_types = cols(
            Version = col_double(),
            Date = col_date(),
            Last_Modified = col_character()
          )
        )
        if ("Last_Modified" %in% names(df_lu)) {
          df_lu$Last_Modified <- lubridate::ymd_hms(df_lu$Last_Modified, tz = "Africa/Dar_es_Salaam")
        }
        if (!"Images" %in% names(df_lu)) df_lu$Images <- NA_character_
        df_lu
      }, error = function(e) {
        data.frame()
      })
    }
  )

  # --- Button: table zoom (show/hide the sidebar) ---
  observeEvent(input$zoom_table, {
    shinyjs::toggleClass(selector = "body", class = "zoom-dt")
    shinyjs::runjs("setTimeout(function(){ $(window).trigger('resize'); }, 150);")
  }, ignoreInit = TRUE)


  # --- EIOS data in memory (EIOS tab) ---
  eios_donnees <- reactiveVal(
    data.frame(
      Sources   = character(),
      Title     = character(),
      Situation = character(),
      Other     = character(),
      Regions   = character(),
      EIOS_id   = character(),
      stringsAsFactors = FALSE
    )
  )


  # --- Folders & utilities for uploaded images ---
  dir.create("Graphs_uploaded", showWarnings = FALSE)

  # Function: get current Event_ID (editing takes priority, otherwise selection in the active tab)
  id_evenement_courant <- function() {
    df_full <- donnees()

    # 1) if an event is loaded for editing, use that row
    if (!is.null(session$userData$indice_edition)) {
      idx <- session$userData$indice_edition
      if (is.finite(idx) && idx <= nrow(df_full)) {
        return(df_full$Event_ID[idx])
      }
    }

    # 2) otherwise, use the selection from the active tab table
    onglet <- input$onglets
    if (identical(onglet, "Active")) {
      sel <- input$tableau_evenements_actifs_rows_selected
      if (length(sel) == 1) return(donnees_actifs_filtrees()$Event_ID[sel])
    } else if (identical(onglet, "Closed")) {
      sel <- input$tableau_evenements_termines_rows_selected
      if (length(sel) == 1) return(donnees_termines_filtrees()$Event_ID[sel])
    }

    return(NA_character_)
  }

  # Function: next image_N index for a given Event_ID
  prochain_index_image <- function(id_evt) {
    dir_evt <- file.path("Graphs_uploaded", id_evt)
    if (!dir.exists(dir_evt)) return(1L)
    exist <- list.files(dir_evt, pattern = "^image_[0-9]+\\.", full.names = FALSE)
    if (length(exist) == 0) return(1L)
    nums <- suppressWarnings(as.integer(gsub("^image_([0-9]+)\\..*$", "\\1", exist)))
    max(nums, na.rm = TRUE) + 1L
  }

  # Function: compute height to keep aspect ratio (target width in inches)
  taille_img <- function(path, largeur_cible = 6) {
    info <- tryCatch(magick::image_read(path), error = function(e) NULL)
    if (is.null(info)) return(list(w = largeur_cible, h = 4.5))
    dim <- magick::image_info(info)
    h <- (dim$height / dim$width) * largeur_cible
    # Keep a reasonable minimum height
    if (!is.finite(h) || h <= 0) h <- 4.5
    list(w = largeur_cible, h = h)
  }




  # --- Active: filter + sort from newest to oldest ---
  donnees_actifs_filtrees <- reactive({
    df <- donnees()
    if (nrow(df) == 0) return(df)
    df <- dplyr::filter(df, Status == "active")
    if (!is.null(input$filtre_date[1]) && !is.null(input$filtre_date[2])) {
      df <- dplyr::filter(df, Date >= input$filtre_date[1], Date <= input$filtre_date[2])
    }
    df <- dplyr::arrange(df, dplyr::desc(Date), dplyr::desc(Last_Modified))
    filtrer_par_texte(df)
  })

  # --- Closed: filter + sort from newest to oldest ---
  donnees_termines_filtrees <- reactive({
    df <- donnees()
    if (nrow(df) == 0) return(df)
    df <- dplyr::filter(df, Status == "closed")
    if (!is.null(input$filtre_date[1]) && !is.null(input$filtre_date[2])) {
      df <- dplyr::filter(df, Date >= input$filtre_date[1], Date <= input$filtre_date[2])
    }
    df <- dplyr::arrange(df, dplyr::desc(Date), dplyr::desc(Last_Modified))
    filtrer_par_texte(df)
  })

  # --- Generic text filter (specific field or all fields) ---
  filtrer_par_texte <- function(df) {
    terme <- input$texte_filtre
    champ <- input$champ_filtre

    if (is.null(terme) || !nzchar(trimws(terme)) || nrow(df) == 0) {
      return(df)
    }

    terme <- trimws(terme)

    # Columns to consider for "all fields"
    cols_tous <- c("Title","Sources","Situation","Other","Regions","EIOS_id")

    if (identical(champ, "tous")) {
      # Search across multiple columns
      cols_existe <- intersect(cols_tous, colnames(df))
      if (length(cols_existe) == 0) return(df)
      idx <- apply(
        as.data.frame(df[, cols_existe, drop = FALSE], stringsAsFactors = FALSE),
        1,
        function(ligne) {
          any(grepl(terme, ligne, ignore.case = TRUE))
        }
      )
      df[idx, , drop = FALSE]
    } else {
      # Search within a single column (if it exists)
      if (!champ %in% colnames(df)) return(df)
      df[grepl(terme, df[[champ]], ignore.case = TRUE), , drop = FALSE]
    }
  }




  session$userData$indice_edition <- NULL
  filtre_date_active <- reactiveVal(TRUE)

  observeEvent(input$enregistrer, {
    nouvelle_id <- sprintf("EVT_%s_%03d", format(Sys.Date(), "%Y%m%d"),
                           sum(grepl(format(Sys.Date(), "%Y%m%d"), donnees()$Event_ID)) + 1)

    nouvel_evenement <- data.frame(
      Event_ID = nouvelle_id,
      Update = "New event",
      Version = 1,
      Date = input$date,
      Last_Modified = lubridate::now(tzone = "Africa/Dar_es_Salaam"),
      Title = input$titre,
      Event_Type = input$type_evenement,
      Alert_Category = input$categorie,
      Regions = input$regions,
      Sources = input$sources,
      Situation = input$situation,
      Risk_Assessment = input$evaluation_risque,
      Measures = input$mesures,
      Public_Health_Relevance = input$pertinence_sante,
      Traveler_Relevance = input$pertinence_voyageurs,
      Comments = input$commentaires,
      Summary = input$resume,
      Other = input$autre,
      Status = input$statut,
      EIOS_id   = NA_character_,
      stringsAsFactors = FALSE
    )


    tryCatch({
      write_csv(bind_rows(donnees(), nouvel_evenement), fichier_donnees)
    }, error = function(e) {
      showModal(modalDialog("Error while saving: ", e$message))
    })

    showModal(modalDialog(
      title = "Success",
      "The event was added successfully.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))

    updateTextInput(session, "titre", value = "")
    updateSelectInput(session, "type_evenement", selected = "")
    updateSelectInput(session, "categorie", selected = "")
    updateTextInput(session, "regions", value = "")
    updateTextInput(session, "sources", value = "")
    updateDateInput(session, "date", value = Sys.Date())
    updateDateRangeInput(session, "periode", start = Sys.Date() - 7, end = Sys.Date())
    updateTextAreaInput(session, "situation", value = "")
    updateTextAreaInput(session, "evaluation_risque", value = "")
    updateTextAreaInput(session, "mesures", value = "")
    updateSelectInput(session, "pertinence_sante", selected = "")
    updateSelectInput(session, "pertinence_voyageurs", selected = "")
    updateTextInput(session, "commentaires", value = "")
    updateTextAreaInput(session, "resume", value = "")
    updateTextInput(session, "autre", value = "")
    updateSelectInput(session, "statut", selected = "active")
  })

  observeEvent(input$effacer_masque, {
    updateTextInput(session, "titre", value = "")
    updateSelectInput(session, "type_evenement", selected = "")
    updateSelectInput(session, "categorie", selected = "")
    updateTextInput(session, "regions", value = "")
    updateTextInput(session, "sources", value = "")
    updateDateInput(session, "date", value = Sys.Date())
    updateDateRangeInput(session, "periode", start = Sys.Date() - 7, end = Sys.Date())
    updateTextAreaInput(session, "situation", value = "")
    updateTextAreaInput(session, "evaluation_risque", value = "")
    updateTextAreaInput(session, "mesures", value = "")
    updateSelectInput(session, "pertinence_sante", selected = "")
    updateSelectInput(session, "pertinence_voyageurs", selected = "")
    updateTextInput(session, "commentaires", value = "")
    updateTextAreaInput(session, "resume", value = "")
    updateTextInput(session, "autre", value = "")
    updateSelectInput(session, "statut", selected = "active")
  })

  observeEvent(input$enregistrer_nouveau, {
    indice <- session$userData$indice_edition

    if (is.null(indice)) {
      showModal(modalDialog("Please load an event to edit first using the 'Load for editing' button."))
      return()
    }

    donnees_actuelles <- donnees()
    evenement_original <- donnees_actuelles[indice, ]

    nouvelle_id <- sprintf("EVT_%s_%03d", format(Sys.Date(), "%Y%m%d"),
                           sum(grepl(format(Sys.Date(), "%Y%m%d"), donnees_actuelles$Event_ID)) + 1)


    original_id <- evenement_original$Event_ID[1]

    nouvel_evenement <- evenement_original %>%
      mutate(
        Event_ID = nouvelle_id,
        Update = paste0("Update of event: ", original_id),
        Version = evenement_original$Version + 1,
        Last_Modified = lubridate::with_tz(Sys.time(), "Africa/Dar_es_Salaam"),
        Title = input$titre,
        Event_Type = input$type_evenement,
        Alert_Category = input$categorie,
        Regions = input$regions,
        Sources = input$sources,
        Date = input$date,
        Situation = input$situation,
        Risk_Assessment = input$evaluation_risque,
        Measures = input$mesures,
        Public_Health_Relevance = input$pertinence_sante,
        Traveler_Relevance = input$pertinence_voyageurs,
        Comments = input$commentaires,
        Summary = input$resume,
        Other = input$autre,
        Status = input$statut
      ) %>%
      select(Event_ID, Update, everything())


    tryCatch({
      write_csv(bind_rows(donnees_actuelles, nouvel_evenement), fichier_donnees)
    }, error = function(e) {
      showModal(modalDialog("Error while saving: ", e$message))
    })

    showModal(modalDialog(
      title = "Success",
      "The new event was saved successfully.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$count_selected_excel, {
    selected <- input$tableau_evenements_actifs_rows_selected
    session$sendCustomMessage("show_export_count", length(selected))
  })

  observeEvent(input$count_selected_word, {
    selected <- input$tableau_evenements_actifs_rows_selected
    session$sendCustomMessage("show_export_count", length(selected))
  })

  observeEvent(input$mettre_a_jour, {
    indice <- session$userData$indice_edition
    if (is.null(indice)) return()

    donnees_actuelles <- donnees()

    if (indice > nrow(donnees_actuelles)) {
      showModal(modalDialog("Error: the item to update no longer exists."))
      return()
    }

    donnees_actuelles[indice, ] <- donnees_actuelles[indice, ] %>%
      mutate(
        Title = input$titre,
        Event_Type = input$type_evenement,
        Alert_Category = input$categorie,
        Regions = input$regions,
        Sources = input$sources,
        Date = input$date,
        Situation = input$situation,
        Risk_Assessment = input$evaluation_risque,
        Measures = input$mesures,
        Public_Health_Relevance = input$pertinence_sante,
        Traveler_Relevance = input$pertinence_voyageurs,
        Comments = input$commentaires,
        Summary = input$resume,
        Other = input$autre,
        Status = input$statut,
        Last_Modified = lubridate::now(tzone = "Africa/Dar_es_Salaam")
      )

    write_csv(donnees_actuelles, fichier_donnees)
    session$userData$indice_edition <- NULL
  })

  observeEvent(input$editer, {
    onglet <- input$onglets
    df_full <- donnees()

    # Determine which tab is active and which row is selected
    if (identical(onglet, "Active")) {
      selection <- input$tableau_evenements_actifs_rows_selected
      df_view   <- donnees_actifs_filtrees()
    } else if (identical(onglet, "Closed")) {
      selection <- input$tableau_evenements_termines_rows_selected
      df_view   <- donnees_termines_filtrees()
    } else {
      showModal(modalDialog("No active tab detected."))
      return()
    }

    if (length(selection) != 1) {
      showModal(modalDialog("Please select exactly one (1) event in the current tab."))
      return()
    }

    # Get the event from the visible (filtered) tab
    evenement <- df_view[selection, ]

    # Find the global index in the full dataset by Event_ID
    idx <- which(df_full$Event_ID == evenement$Event_ID)
    if (length(idx) != 1) {
      showModal(modalDialog("Unable to find the selected event in the full dataset."))
      return()
    }

    # Fill the form fields
    updateTextInput(session, "titre", value = evenement$Title)
    updateSelectInput(session, "type_evenement",
                      selected = ifelse(is.na(evenement$Event_Type) || evenement$Event_Type == "", "", evenement$Event_Type))
    updateSelectInput(session, "categorie",
                      selected = ifelse(is.na(evenement$Alert_Category) || evenement$Alert_Category == "", "", evenement$Alert_Category))
    updateSelectInput(session, "regions", selected = evenement$Regions)
    updateTextInput(session, "sources", value = evenement$Sources)
    updateSelectInput(session, "statut", selected = evenement$Status)
    updateDateInput(session, "date", value = as.Date(evenement$Date))
    updateTextAreaInput(session, "situation",          value = evenement$Situation)
    updateTextAreaInput(session, "evaluation_risque",  value = evenement$Risk_Assessment)
    updateTextAreaInput(session, "mesures",            value = evenement$Measures)
    updateSelectInput(session, "pertinence_sante",
                      selected = ifelse(is.na(evenement$Public_Health_Relevance) || evenement$Public_Health_Relevance == "", "", evenement$Public_Health_Relevance))
    updateSelectInput(session, "pertinence_voyageurs",
                      selected = ifelse(is.na(evenement$Traveler_Relevance) || evenement$Traveler_Relevance == "", "", evenement$Traveler_Relevance))
    updateTextInput(session, "commentaires", value = evenement$Comments)
    updateTextAreaInput(session, "resume",    value = evenement$Summary)
    updateTextInput(session, "autre",         value = evenement$Other)

    # Marker for "Save changes" / "Delete" / "Save as update"
    session$userData$indice_edition <- idx
  })


  # Delete events
  observeEvent(input$supprimer, {
    selected_rows <- input$tableau_evenements_actifs_rows_selected
    if (length(selected_rows) == 0) {
      showModal(modalDialog("No event selected.", easyClose = TRUE))
      return()
    }

    # Confirmation dialog
    showModal(modalDialog(
      title = "Confirmation",
      paste("Are you sure you want to delete", length(selected_rows), "event(s)?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete", "Confirm", class = "btn btn-danger")
      )
    ))
  })

  # Confirmation after modal dialog (delete)
  observeEvent(input$confirm_delete, {
    selected_rows <- input$tableau_evenements_actifs_rows_selected
    df <- donnees()

    # Delete selected events
    df_new <- df[-selected_rows, ]

    tryCatch({
      write_csv(df_new, fichier_donnees)
      showModal(modalDialog(
        title = "Success",
        paste(length(selected_rows), "event(s) were deleted."),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }, error = function(e) {
      showModal(modalDialog("Error while deleting: ", e$message))
    })
  })


  # ---- active ----
  output$tableau_evenements_actifs <- DT::renderDT({
    df <- donnees_actifs_filtrees() %>%
      dplyr::mutate(across(where(is.character), ~{
        x <- .
        x[is.na(x)] <- ""
        sprintf('<div class="cell-fixed" title="%s">%s</div>', x, x)
      }))


    cols <- colnames(df)
    idx  <- function(nm) which(cols == nm) - 1L  # DataTables: 0-based

    DT::datatable(
      transform(df, Last_Modified = format(Last_Modified, "%Y-%m-%d %H:%M:%S %Z")),
      selection = "multiple",
      editable  = TRUE,
      escape    = FALSE,
      rownames  = FALSE,        # important: aligns column indices
      options   = list(
        scrollX    = TRUE,      # allows widths to apply
        autoWidth  = TRUE,
        pageLength = 15,
        columnDefs = list(
          list(targets = idx("Sources"),         width = "250px"),
          list(targets = idx("Situation"),       width = "500px"),
          list(targets = idx("Risk_Assessment"), width = "500px"),
          list(targets = idx("Measures"),        width = "500px")
        )
      )
    )
  })

  # ---- closed ----
  output$tableau_evenements_termines <- DT::renderDT({
    df <- donnees_termines_filtrees() %>%
      dplyr::mutate(across(where(is.character), ~{
        x <- .
        x[is.na(x)] <- ""
        sprintf('<div class="cell-fixed" title="%s">%s</div>', x, x)
      }))


    cols <- colnames(df)
    idx  <- function(nm) which(cols == nm) - 1L  # DataTables: 0-based

    DT::datatable(
      df,
      selection = "multiple",
      editable  = TRUE,
      escape    = FALSE,
      rownames  = FALSE,        # important: aligns column indices
      options   = list(
        scrollX    = TRUE,      # allows widths to apply
        autoWidth  = TRUE,
        pageLength = 15,
        columnDefs = list(
          list(targets = idx("Sources"),         width = "250px"),
          list(targets = idx("Situation"),       width = "500px"),
          list(targets = idx("Risk_Assessment"), width = "500px"),
          list(targets = idx("Measures"),        width = "500px")
        )
      )
    )
  })


  # ---- EIOS (table) ----
  output$tableau_eios <- DT::renderDT({
    df <- eios_donnees() %>%
      dplyr::mutate(across(where(is.character), ~{
        x <- .
        x[is.na(x)] <- ""
        sprintf('<div class="cell-fixed" title="%s">%s</div>', x, x)
      }))

    cols <- colnames(df)
    idx  <- function(nm) which(cols == nm) - 1L  # DataTables: 0-based

    DT::datatable(
      df,
      selection = "multiple",
      editable  = FALSE,
      escape    = FALSE,
      rownames  = FALSE,
      options   = list(
        scrollX    = TRUE,
        autoWidth  = TRUE,
        pageLength = 15
      )
    )
  })


  # --- Proxies for both tables ---
  proxy_actifs   <- DT::dataTableProxy("tableau_evenements_actifs")
  proxy_termines <- DT::dataTableProxy("tableau_evenements_termines")

  # --- Helper: select/unselect ---
  .select_all <- function(proxy, nrows) {
    if (is.finite(nrows) && nrows > 0) DT::selectRows(proxy, 1:nrows) else DT::selectRows(proxy, NULL)
  }
  .clear_all <- function(proxy) DT::selectRows(proxy, NULL)

  # --- Buttons impacting the active tab ---
  observeEvent(input$select_all, {
    onglet <- input$onglets
    if (identical(onglet, "Active")) {
      .select_all(proxy_actifs, nrow(donnees_actifs_filtrees()))
    } else if (identical(onglet, "Closed")) {
      .select_all(proxy_termines, nrow(donnees_termines_filtrees()))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$clear_all, {
    onglet <- input$onglets
    if (identical(onglet, "Active")) {
      .clear_all(proxy_actifs)
    } else if (identical(onglet, "Closed")) {
      .clear_all(proxy_termines)
    }
  }, ignoreInit = TRUE)


  # Date filter
  observeEvent(input$reset_filtre_date, {
    df <- donnees()
    # Find earliest date in the full dataset
    if (!is.null(df) && nrow(df) > 0 && "Date" %in% names(df)) {
      min_date <- suppressWarnings(min(df$Date, na.rm = TRUE))
      if (is.infinite(suppressWarnings(as.numeric(min_date))) || is.na(min_date)) {
        min_date <- Sys.Date() - 30
      }
    } else {
      min_date <- Sys.Date() - 30
    }
    # Reset date filter in the UI: from first event to today
    updateDateRangeInput(session, "filtre_date", start = as.Date(min_date), end = Sys.Date())
  })



  # Excel -------------------------------------------------------------------

  dir.create("excel_outputs", showWarnings = FALSE)

  # --- Excel: export ---
  observeEvent(input$exporter_excel, {
    dir.create("excel_outputs", showWarnings = FALSE)

    # Current view (active filtered + sorted) to align selection & export
    data_view <- donnees_actifs_filtrees()
    sel <- input$tableau_evenements_actifs_rows_selected
    export_data <- if (!is.null(sel) && length(sel) > 0) data_view[sel, ] else data_view

    if (nrow(export_data) == 0) {
      showModal(modalDialog("No selected or active events."))
      return()
    }

    export_data <- export_data %>%
      mutate(Last_Modified = format(Last_Modified, "%Y-%m-%d %H:%M:%S"))

    date_today <- format(Sys.Date(), "%Y-%m-%d")
    existing_files <- list.files("excel_outputs", pattern = paste0("^evenements_", date_today, "_\\d+\\.xlsx$"))
    existing_nums <- as.integer(gsub(paste0("evenements_", date_today, "_|\\.xlsx"), "", existing_files))
    next_num <- ifelse(length(existing_nums) == 0, 1, max(existing_nums, na.rm = TRUE) + 1)
    filename <- sprintf("excel_outputs/evenements_%s_%d.xlsx", date_today, next_num)

    tryCatch({
      writexl::write_xlsx(export_data, filename)
      showModal(modalDialog(
        title = "Success",
        paste("The Excel file was created successfully:", filename),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }, error = function(e) {
      showModal(modalDialog("Error during export: ", e$message))
    })
  })

  # ----------------------- EIOS

  # --- Import an EIOS (CSV) file and populate the EIOS tab ---
  observeEvent(input$upload_eios, {
    if (is.null(input$upload_eios$datapath)) return()
    path <- input$upload_eios$datapath

    # Read EIOS CSV, keep only required columns
    df_raw <- tryCatch(
      readr::read_csv(path, show_col_types = FALSE),
      error = function(e) { showModal(modalDialog("Error reading EIOS CSV: ", e$message)); return(NULL) }
    )
    if (is.null(df_raw)) return()

    # Check required columns
    req_cols <- c("externalLink","title","summary","categories","mentionedCountries","id")
    if (!all(req_cols %in% names(df_raw))) {
      manq <- paste(setdiff(req_cols, names(df_raw)), collapse = ", ")
      showModal(modalDialog(paste("Missing columns in the EIOS CSV:", manq)))
      return()
    }

    # Map to the EIOS tab schema
    df_eios <- df_raw %>%
      dplyr::transmute(
        Sources   = as.character(.data$externalLink),
        Title     = as.character(.data$originalTitle),
        Situation = as.character(.data$translatedDescription),
        Other     = as.character(.data$categories),
        Regions   = as.character(.data$mentionedCountries),
        EIOS_id   = as.character(.data$id)
      )

    eios_donnees(df_eios)
  })


  # --- Copy EIOS selection to "Active" (creates new events) ---
  observeEvent(input$copier_vers_actifs, {
    df_eios <- eios_donnees()
    sel <- input$tableau_eios_rows_selected
    if (is.null(sel) || length(sel) == 0) return()

    # Current selection
    selection <- df_eios[sel, , drop = FALSE]
    if (nrow(selection) == 0) return()

    df_all <- donnees()

    # Generate new events with unique ID and active status
    to_add <- lapply(seq_len(nrow(selection)), function(i) {
      # Event_ID in the existing format
      nouvelle_id <- sprintf("EVT_%s_%03d", format(Sys.Date(), "%Y%m%d"),
                             sum(grepl(format(Sys.Date(), "%Y%m%d"), df_all$Event_ID)) + i)

      data.frame(
        Event_ID                = nouvelle_id,
        Update                  = "new event from EIOS",
        Version                 = 1,
        Date                    = Sys.Date(),
        Last_Modified           = lubridate::now(tzone = "Africa/Dar_es_Salaam"),
        Title                   = selection$Title[i],
        Event_Type              = NA_character_,
        Alert_Category          = NA_character_,
        Regions                 = selection$Regions[i],
        Sources                 = selection$Sources[i],
        Situation               = selection$Situation[i],
        Risk_Assessment         = NA_character_,
        Measures                = NA_character_,
        Public_Health_Relevance = NA_character_,
        Traveler_Relevance      = NA_character_,
        Comments                = NA_character_,
        Summary                 = NA_character_,
        Other                   = selection$Other[i],
        Status                  = "active",
        EIOS_id                 = selection$EIOS_id[i],
        Images                  = if ("Images" %in% names(df_all)) NA_character_ else NULL,
        stringsAsFactors        = FALSE
      )
    })

    # Merge and save
    df_new <- dplyr::bind_rows(df_all, dplyr::bind_rows(to_add))
    tryCatch({
      write_csv(df_new, fichier_donnees)
    }, error = function(e) {
      showModal(modalDialog("Error while saving CSV: ", e$message))
    })
  })


  # Word report ------------------------------------------------------------

  # --- Word: export ---
  observeEvent(input$exporter_word, {
    dir.create("report_word_outputs", showWarnings = FALSE)

    # Current view (active filtered + sorted)
    data_view <- donnees_actifs_filtrees()
    sel <- input$tableau_evenements_actifs_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      showModal(modalDialog(
        title = "No event selected",
        "Please select at least one event in the Active table before creating a Word report.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }
    export_data <- if (!is.null(sel) && length(sel) > 0) data_view[sel, ] else data_view

    if (nrow(export_data) == 0) {
      showModal(modalDialog("No selected or active events."))
      return()
    }

    # Filename
    rapport_filename <- generer_nom_fichier()

    # Document generation
    doc <- officer::read_docx("template_report/template_word.docx")
    doc <- ajouter_en_tete(doc)

    # Push content down so it starts around the middle of the first page
    for (i in 1:0) doc <- officer::body_add_par(doc, "", style = "t_standard")
    doc <- ajouter_sommaire(doc, export_data, NULL)

    # Page break: events start on page 2
    doc <- officer::body_add_break(doc)
    doc <- ajouter_evenements(doc, export_data, NULL)


    tryCatch({
      print(doc, target = rapport_filename)
      showModal(modalDialog(
        title = "Success",
        paste("The Word report was generated successfully:", rapport_filename),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }, error = function(e) {
      showModal(modalDialog("Error while generating the Word report: ", e$message))
    })
  })

  # --- Upload charts/images and attach them to an event ---
  observeEvent(input$televerser_graphs, {
    fichiers <- input$televerser_graphs
    if (is.null(fichiers) || nrow(fichiers) == 0) {
      showModal(modalDialog("Please choose one or more image files."))
      return()
    }

    id_evt <- id_evenement_courant()
    if (is.na(id_evt) || is.null(id_evt)) {
      showModal(modalDialog("Please select an event (in the table) or load it for editing first."))
      return()
    }

    # Prepare event folder
    dir_evt <- file.path("Graphs_uploaded", id_evt)
    dir.create(dir_evt, showWarnings = FALSE, recursive = TRUE)

    # Copy files + naming image_1, image_2, ...
    idx <- prochain_index_image(id_evt)
    chemins_relatifs <- character(0)

    for (i in seq_len(nrow(fichiers))) {
      src  <- fichiers$datapath[i]
      ext  <- tools::file_ext(fichiers$name[i])
      if (identical(ext, "")) ext <- "png"
      dest_name <- sprintf("image_%d.%s", idx, tolower(ext))
      dest_path <- file.path(dir_evt, dest_name)
      ok <- file.copy(from = src, to = dest_path, overwrite = FALSE)
      if (isTRUE(ok)) {
        chemins_relatifs <- c(chemins_relatifs, dest_path)  # relative path in the project
        idx <- idx + 1L
      }
    }

    if (length(chemins_relatifs) == 0) {
      showModal(modalDialog("No file was copied."))
      return()
    }

    # Save/append paths in the 'Images' column for the corresponding row
    df <- donnees()
    pos <- which(df$Event_ID == id_evt)
    if (length(pos) == 1) {
      exist <- df$Images[pos]
      if (is.null(exist) || is.na(exist)) exist <- ""
      ajout <- paste(chemins_relatifs, collapse = ";")
      df$Images[pos] <- if (nzchar(exist)) paste(exist, ajout, sep = ";") else ajout

      tryCatch(write_csv(df, fichier_donnees),
               error = function(e) showModal(modalDialog("Error while saving image paths: ", e$message)))
    }

    showModal(modalDialog(
      title = "Success",
      paste0(length(chemins_relatifs), " file(s) linked to event ", id_evt, "."),
      easyClose = TRUE, footer = modalButton("Close")
    ))
  })

  # File naming
  generer_nom_fichier <- function() {
    date_today <- format(Sys.Date(), "%Y-%m-%d")
    existing_files <- list.files("report_word_outputs", pattern = paste0("^report_", date_today, "_\\d+\\.docx$"))
    existing_nums <- as.integer(gsub(paste0("report_", date_today, "_|\\.docx"), "", existing_files))
    next_num <- ifelse(length(existing_nums) == 0, 1, max(existing_nums, na.rm = TRUE) + 1)
    sprintf("report_word_outputs/report_%s_%d.docx", date_today, next_num)
  }


  # =========== page 1 ============

  ajouter_en_tete <- function(doc) {
    # --- BM_DATE ---
    month_en <- c("January","February","March","April","May","June",
                  "July","August","September","October","November","December")
    today <- Sys.Date()
    month_name <- month_en[as.integer(format(today, "%m"))]

    date_value <- paste0(
      format(today, "%d"), " ", month_name, " ", format(today, "%Y")
    )

    doc <- officer::body_replace_text_at_bkm(
      doc, bookmark = "BM_DATE", value = date_value
    )

    doc
  }




  # ============== page 2 =================

  # -------- Event list ----------
  ajouter_sommaire <- function(doc, data, styles) {
    cat_map <- list(
      "International and national alerts" = "International and National Public Health Alerts and Emergencies",
      "National alerts"                   = "National Public Health Alerts and Emergencies",
      "International alerts"              = "International Public Health Alerts and Emergencies"
    )
    categories <- names(cat_map)

    doc <- doc %>% officer::body_add_par("EVENT LIST", style = "t_heading_1")

    compteur <- 1

    # Category sections (print only if there are events)
    for (cat in categories) {
      cat_data <- data %>% dplyr::filter(Alert_Category == cat)
      if (nrow(cat_data) == 0) next

      doc <- doc %>% officer::body_add_par(cat_map[[cat]], style = "t_heading_2")

      for (i in seq_len(nrow(cat_data))) {
        titre <- as.character(cat_data$Title[i])
        if (!is.na(titre) && nzchar(trimws(titre))) {
          doc <- doc %>% officer::body_add_par(
            paste0(compteur, ". ", titre),
            style = "t_heading_4"
          )
          compteur <- compteur + 1
        }
      }
    }

    # Also include events without category (typical for EIOS)
    other_data <- data %>%
      dplyr::filter(is.na(Alert_Category) | Alert_Category == "" | !(Alert_Category %in% categories))

    if (nrow(other_data) > 0) {
      doc <- doc %>% officer::body_add_par("Uncategorized", style = "t_heading_2")

      for (i in seq_len(nrow(other_data))) {
        titre <- as.character(other_data$Title[i])
        if (!is.na(titre) && nzchar(trimws(titre))) {
          doc <- doc %>% officer::body_add_par(
            paste0(compteur, ". ", titre),
            style = "t_heading_4"
          )
          compteur <- compteur + 1
        }
      }
    }

    doc
  }


 # Word: add events ------------------------------------------------------
  ajouter_evenements <- function(doc, data, styles) {
    cat_map <- list(
      "International and national alerts" = "International and National Public Health Alerts and Emergencies",
      "National alerts"                   = "National Public Health Alerts and Emergencies",
      "International alerts"              = "International Public Health Alerts and Emergencies"
    )
    categories <- names(cat_map)

    compteur <- 1

    # Simple formatting
    p_std   <- officer::fp_par(line_spacing = 0.7)
    run_std <- officer::fp_text()
    run_b   <- officer::fp_text(bold = TRUE)

    # Columns that should NOT be printed in the Word report
    colonnes_exclues <- c("Event_ID", "Version", "Last_Modified", "EIOS_id", "Images")

    # Helper to print one event
    .print_event <- function(doc, row, compteur) {
      titre <- if ("Title" %in% names(row)) as.character(row$Title) else ""
      if (is.na(titre) || !nzchar(trimws(titre))) titre <- "(No title)"

      doc <- doc %>% officer::body_add_par(paste0(compteur, ". ", titre), style = "t_heading_1")

      for (champ in names(row)) {
        if (champ %in% colonnes_exclues) next

        val <- row[[champ]]
        val_txt <- as.character(val)

        if (!is.null(val) && !is.na(val) && nzchar(trimws(val_txt))) {
          bloc <- officer::fpar(
            officer::ftext(paste0(champ, " : "), run_b),
            officer::ftext(val_txt, run_std),
            fp_p = p_std
          )
          doc <- officer::body_add_fpar(doc, value = bloc, style = "t_standard")
        }
      }

      # Images linked to the event (if present in the 'Images' column)
      if ("Images" %in% names(row)) {
        imgs_raw <- as.character(row$Images)
        if (!is.na(imgs_raw) && nzchar(trimws(imgs_raw))) {
          chemins <- strsplit(imgs_raw, ";")[[1]]
          chemins <- trimws(chemins)
          chemins <- chemins[chemins != ""]
          for (che in chemins) {
            if (file.exists(che)) {
              s <- taille_img(che, 3)
              doc <- officer::body_add_img(doc, src = che, width = s$w, height = s$h)
              doc <- officer::body_add_par(doc, "", style = "t_standard")
            }
          }
        }
      }

      list(doc = doc, compteur = compteur + 1)
    }

    # Category sections
    for (cat in categories) {
      cat_data <- data %>% dplyr::filter(Alert_Category == cat)
      if (nrow(cat_data) == 0) next

      doc <- doc %>% officer::body_add_par(cat_map[[cat]], style = "t_heading_2")

      for (i in seq_len(nrow(cat_data))) {
        res <- .print_event(doc, cat_data[i, , drop = FALSE], compteur)
        doc <- res$doc
        compteur <- res$compteur
      }
    }

    # Uncategorized / other
    other_data <- data %>%
      dplyr::filter(is.na(Alert_Category) | Alert_Category == "" | !(Alert_Category %in% categories))

    if (nrow(other_data) > 0) {
      doc <- doc %>% officer::body_add_par("Uncategorized", style = "t_heading_2")

      for (i in seq_len(nrow(other_data))) {
        res <- .print_event(doc, other_data[i, , drop = FALSE], compteur)
        doc <- res$doc
        compteur <- res$compteur
      }
    }
    doc
  }
}
