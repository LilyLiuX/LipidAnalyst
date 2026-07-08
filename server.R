server <- function(input, output,session) {
  output$nav_ui0 <- renderUI({
    
    nav_buttons(next_id = "next0")
  })
  ## ---- Lipidomics Data Upload & Preview ----
  # load example
  
  originalData <- reactiveVal(NULL)
  
  # Reactive for llipidomics data
  observe({
    req(input$lipidomics_file,input$lipidROW)
    ext <- tools::file_ext(input$lipidomics_file$name)
    df <- NULL
    # ---------- Step 1: read the file ----------
    if (ext %in% c("csv", "CSV")) {
      df <- read.csv(input$lipidomics_file$datapath,
                     header = F,
                     check.names = FALSE,
                     stringsAsFactors = FALSE)
    } 
    else if (ext %in% c("tsv", "TSV")) {
      df <- read.delim(input$lipidomics_file$datapath,
                       header = F,
                       check.names = FALSE,
                       stringsAsFactors = FALSE)
    }
    else if (ext %in% c("xls", "xlsx", "XLS", "XLSX")) {
      df <- suppressMessages(
        readxl::read_excel(input$lipidomics_file$datapath, 
                       col_names = FALSE,
                       sheet = 1))
      df <- as.data.frame(df)
    }
    else {
      showNotification("Unsupported file type", type = "error")
      return(NULL)
    }
    # ---------- Step 2: validate df ----------
    if (is.null(df) || ncol(df) < 2 || nrow(df) < 2) {
      showNotification("Error: Uploaded file is empty or it only has one column. ", type = "error")
      return(NULL)
    }
    
    # ---------- Step 3: transpose df if necessary --------
    if (input$lipidROW == "lipids on the rows") {
              df <- as.data.frame(t(df))
              colnames(df) <- df[1,]
              df <- df[-1, , drop = FALSE]
              # ---------- Step 4: check duplicates ------------
              if (any(duplicated(df[[1]]))) {
                showNotification("Error: 
                                 Please ensure all sample names are unique. 
                                 Make sure you use the corrected setting of 
                                 'Lipids on the column' or 'Lipids on the row', 
                                 and upload the file again.", 
                                 type = "error",duration = 15)
                return(NULL)
              }
              rownames(df) <- df[[1]]
              df <- df[, -1, drop = FALSE]
    } 
    else{
      colnames(df) <- df[1,]
      df <- df[-1, , drop = FALSE]
      if (any(duplicated(df[,1]))) {
        showNotification("Error: 
                                 Please ensure all sample names are unique. 
                                 Make sure you use the corrected setting of 
                                 'Lipids on the column' or 'Lipids on the row', 
                                  and upload the file again.", 
                         type = "error",duration = 15)
        return(NULL)
      }
      rownames(df) <- df[[1]]
      df <- df[, -1, drop = FALSE]
    }
  
    
    # if the df has NaN, NA, " ", or ""
    # treating them as missing value
    df[is.na(df) | df == " " | df == ""|df == "NaN"|df == "N/A"] <- NA
     
    
    df[] <- lapply(df, function(col) {
      col <- trimws(as.character(col))
      suppressWarnings(as.numeric(col))
    })
    
    # sort columns alphabetically by colnames
    df <- df[, order(colnames(df)), drop = FALSE]
    originalData(df)   # <-- store in reactiveVal
  })
  
  observeEvent(input$load_example_l, {
    example_path <- "www/Lipidomics_Data.xlsx"
    df <- readxl::read_excel(example_path, sheet = 1)
    df <- as.data.frame(df)
    rownames(df) <- df[[1]]  # set first column as rownames
    df <- df[ , -1,drop =F]           # remove first column
    # if the df has NaN, NA, " ", or ""
    # treating them as missing value
    df[is.na(df) | df == " " | df == ""|df == "NaN"|df == "N/A"] <- NA
    
    # Try to convert each column to numeric, if possible
    df[] <- lapply(df, function(col) {
      # Remove leading/trailing spaces
      col <- trimws(col)
      # Convert to numeric if possible, else keep as is
      suppressWarnings(as.numeric(col))
    })
    df <- df[, order(colnames(df)), drop = FALSE]
    showNotification("Example Lipidomics data loaded.", type = "message")
    originalData(df)  
  })
  
  observeEvent(input$help_btn_upload1, {
    showModal(modalDialog(
      title = "Help Information",
      tagList(
        p("This section lets you upload your lipidomics expression data in CSV/TSV/XLS/XLSX format."),
        p("If the uploaded file is an Excel workbook, only the first sheet will be processed."),
        p("Data file includes lipids and sample ID only."),
        p("Choose whether your data has lipids on rows or columns."),
        p("After uploading, you can preview the data and check validation results here."),
        p("Missing values in the sheet may appear in different forms, such as NA, NaN, N/A, an empty string, or just a blank cell. ")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Validation message for lipidomics file
  output$validation <- renderUI({
    req(input$lipidomics_file)
    ext <- tools::file_ext(input$lipidomics_file$name)
    if (!ext %in% c("csv", "CSV","tsv", "TSV","xls", "xlsx", "XLS", "XLSX")) {
      HTML('<span style="color:red;"> ❌ Please upload a valid file and make sure it is in CSV/XLS/XLSX format.</span>')
    } else {
      HTML('<span style="color:green;">Lipidomics Data successfully uploaded.</span>')
    }
    req(originalData())
    df <- originalData()
    data_part <- df[, , drop = FALSE]  
    is_numeric_matrix <- all(sapply(data_part, is.numeric))
    if (!is_numeric_matrix) {

      non_numeric_cols <- names(data_part)[!sapply(data_part, is.numeric)]
      if (length(non_numeric_cols) > 0) {
        return(HTML(paste("<span style='color:red;'>Non-numeric columns detected: ",
                          paste(non_numeric_cols, collapse = ", "), 
                          ". Please ensure all values except row and column names are numeric.</span>")))
      }
    }

  })
  
  output$Summary <- renderUI({
    req( originalData())
    df <- originalData()
    # if (isTRUE(input$lipidROW == "lipids on the rows")) {
    #   df <- as.data.frame(t(df))
    # }

    HTML(sprintf('<p style="font-size:18px; color:#2E8B57; font-weight:bold;">
                 Data has %d samples and %d lipids.</p>', nrow(df), ncol(df)))
  })
  
  
  output$dataPreview <- DT::renderDataTable({
    req(originalData())
    df <- originalData()
    if (isTRUE(input$lipidROW == "lipids on the rows")) {
      df <- as.data.frame(t(df))
    }
    datatable(df, options = list(pageLength = 10), rownames = TRUE)
  })
  
  output$nav_ui1 <- renderUI({
    next_id <- NULL
    if (!is.null(originalData())) {
      next_id <- "next1"
    }
    nav_buttons(next_id = next_id,prev_id = "prev1")
  })
  
  output$download_example_l <- downloadHandler(
    filename = function() {
      "Lipidomics_Data.xlsx"   # the name the user will see
    },
    content = function(file) {
      # copy from your www/ folder to the download target
      file.copy("www/Lipidomics_Data.xlsx", file)
    }
  )
  
  
  ##
  ## ---- Metadata Upload & Preview ----
  
  metadataFile <- reactiveVal(NULL)
  
  # Reactive for metadata file
  observeEvent(input$metadata_file,{
    ext <- tools::file_ext(input$metadata_file$name)
    if (ext %in% c("csv", "CSV")) {
      metadata <- read.csv(input$metadata_file$datapath, header = TRUE, 
                           check.names = FALSE)
    }     
    else if (ext %in% c("tsv", "TSV")) {
      metadata <- read.delim(input$metadata_file$datapath, header = TRUE, 
                       check.names = FALSE)
    }
    
    else if (ext %in% c("xls", "xlsx", "XLS", "XLSX")) {
      metadata <- readxl::read_excel(input$metadata_file$datapath, sheet = 1)
      metadata <- as.data.frame(metadata)
    }
    # if the first column has duplication, raise warning and return NULL
    if (any(duplicated(metadata[[1]]))) {
      showNotification("Warning: The first column has duplicate values. Please ensure there are no duplicates in sample names or lipid names.", type = "error")
      return(NULL)
    }
    rownames(metadata) <- metadata[[1]]  # set first column as rownames
    # if there is any empty colnames, replace it with "NA_column"
    colnames(metadata)[colnames(metadata) == ""] <- "NA_column"
    metadata <- metadata[ , -1, drop = F] 
    metadataFile(metadata)
  })
  
  observeEvent(input$load_example_m, {
    example_path <- "www/Metadata_Group_Info.xlsx"
    df <- readxl::read_excel(example_path, sheet = 1)
    df <- as.data.frame(df)
    rownames(df) <- df[[1]]  # set first column as rownames
    df <- df[ , -1,drop =F]           # remove first column
    showNotification("Example Metadata data loaded.", type = "message")
    metadataFile(df)  
  })
  
  observeEvent(input$help_btn_upload2, {
    showModal(modalDialog(
      title = "Help Information",
      tagList(
        p("This section lets you upload the metadata in CSV/TSV/XLS/XLSX format."),
        p("Data file includes sample ID, grouping variables, and other factors. Please ensure sample name is on the ",tags$b("first"), " column."),
        p("After uploading, you can preview the data and check validation results here."),
        p("Grouping variables are variables that help categorize your samples into different groups for analysis, such as treatment vs. control."),
        p("Grouping variables are essential for downstream analysis. Please select the appropriate grouping variable from the dropdown menu after uploading the metadata.")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observe({
    req(metadataFile())
    updateSelectInput(session, "define_group",
                      choices = colnames(metadataFile()),
                      selected = colnames(metadataFile())[1])
  })
  metadataWithGroup <- reactive({
    req(metadataFile(),nzchar(input$define_group))
    df <- metadataFile()
    req(input$define_group %in% colnames(metadataFile()))
    # check if every sample has its own group
    if (length(unique(df[[input$define_group]])) == nrow(df)) {
      showNotification(
        "Warning: Each sample has its own group. Please check the selected group column.",
        type = "warning"
      )
    }

    df[[input$define_group]] <- as.factor(df[[input$define_group]])
    df <- df[order(df[[input$define_group]]), , drop = FALSE]
    return(df)
  })
  
  
  ngroup <- reactiveVal(NULL)
  
  observeEvent(metadataWithGroup(),{
    req(nzchar(input$define_group))
    groups<- unique(metadataWithGroup()[[input$define_group]])
    ngroup(length(groups))
  })
  

  # Validation message for metadata
  output$metadata_validation <- renderUI({
    req(metadataFile())
    
    # Invalid file extension
    if (!is.null(input$metadata_file)){
      ext <- tools::file_ext(input$metadata_file$name)
      if (!ext %in% c("csv", "CSV","tsv", "TSV","xls", "xlsx", "XLS", "XLSX")) {
        return(HTML('<span style="color:red;">❌ Please upload a valid CSV file for metadata.</span>'))
      }
    }
    
    # Extension is valid, check rownames
   if (!identical(sort(rownames(metadataFile())), sort(rownames(originalData()))))
     {
      # 
      return(HTML('<span style="color:red;">❌ Please ensure sample names of metadata are consistent with the lipidomics data.</span>'))
    }
    
    # Everything is OK
    HTML('<span style="color:green;">Metadata successfully uploaded.</span>')
  })
  
  # Metadata preview table
  output$metadataPreview <- DT::renderDataTable({
    req(metadataWithGroup())
    datatable(metadataWithGroup(), options = list(pageLength = 10), rownames = TRUE)
  })
  
  output$nav_ui2 <- renderUI({
    next_id <- NULL
    if (!is.null(metadataFile())) {
      next_id <- "next2"
    }
    
    nav_buttons(
      prev_id = "prev2",
      next_id = next_id
    )
  })
  
  Lipid_column_data_reseq <- reactive({
    req(originalData(),metadataWithGroup())
    df <- originalData()
    meta <- metadataWithGroup()
    sample_order <- rownames(meta)
    df <- df[sample_order, , drop = FALSE]
    return(df)
    })
  
  output$download_example_m <- downloadHandler(
    filename = function() {
      "Metadata_Group_Info.xlsx"   # the name the user will see
    },
    content = function(file) {
      # copy from your www/ folder to the download target
      file.copy("www/Metadata_Group_Info.xlsx", file)
    }
  )
  
  ## ---- Internal Standard Upload & Preview ----
  
  internalStandardFile <- reactiveVal(NULL)
  validation_pass <- reactiveVal(FALSE)
  # Reactive for internal standard file
  observeEvent(input$internal_standard_file,{
    ext <- tools::file_ext(input$internal_standard_file$name)
    if (ext %in% c("csv", "CSV")) {
      is_data <- read.csv(input$internal_standard_file$datapath, header = F, 
                          check.names = FALSE)
    } 
    else if (ext %in% c("tsv", "TSV")) {
      is_data <- read.delim(input$internal_standard_file$datapath, header = F,
                       check.names = FALSE)
    }
    else if (ext %in% c("xls", "xlsx", "XLS", "XLSX")) {
      is_data <- readxl::read_excel(input$internal_standard_file$datapath,
                            col_name = F,
                            sheet = 1)
    }
    
    if (input$lipidROW_internal == "lipids on the rows") {
      is_data <- as.data.frame(t(is_data))
      if (any(duplicated(is_data[[1]]))) {
        showNotification("Warning: The first column has duplicate values. Please ensure there are no duplicates in sample names or lipid names.", type = "error")
        return(NULL)
      }
      colnames(is_data) <- is_data[1,]
      is_data <- is_data[-1, , drop = FALSE]
      rownames(is_data) <- is_data[[1]]  # set first column as rownames
      is_data <- is_data[ , -1,drop =F] 
    } else {
      # if the first column has duplication, raise warning and return NULL
      if (any(duplicated(is_data[[1]]))) {
        showNotification("Warning: The first column has duplicate values. Please ensure there are no duplicates in sample names or lipid names.", type = "error")
        return(NULL)
      }
      colnames(is_data) <- is_data[1,]
      is_data <- is_data[-1, , drop = FALSE]
      rownames(is_data) <- is_data[[1]]  # set first column as rownames
      is_data <- is_data[ , -1,drop =F] 
    }
    is_data[is.na(is_data) | is_data == " " | is_data == ""|is_data == "NaN"|is_data == "N/A"] <- NA
    is_data <- as.data.frame(is_data)
    
    is_data <- is_data[, order(colnames(is_data)), drop = FALSE]
    meta <- metadataWithGroup()
    if (!is.null(meta)) {
      sample_order <- rownames(meta)
      is_data <- is_data[sample_order, , drop = FALSE]
    }
    # Try to convert each column to numeric, if possible
    is_data[] <- lapply(is_data, function(col) {
      suppressWarnings(as.numeric(col))
    })
    internalStandardFile(is_data)
  })
  
  
  observeEvent(input$load_example_i, {
    example_path <- "www/Internal_Standard.xlsx"
    df <- readxl::read_excel(example_path, sheet = 1)
    df <- as.data.frame(df)
    rownames(df) <- df[[1]]  # set first column as rownames
    df <- df[ , -1,drop =F]           # remove first column
    # if the df has NaN, NA, " ", or ""
    # treating them as missing value
    df[is.na(df) | df == " " | df == ""|df == "NaN"|df == "N/A"] <- NA
    
    # Try to convert each column to numeric, if possible
    df[] <- lapply(df, function(col) {
      # Remove leading/trailing spaces
      col <- trimws(col)
      # Convert to numeric if possible, else keep as is
      suppressWarnings(as.numeric(col))
    })
    showNotification("Example Internal Standard data loaded.", type = "message")
    internalStandardFile(df)  
  })
  
  observeEvent(input$help_btn_upload3, {
    showModal(modalDialog(
      title = "Help Information",
      tagList(
        p("This section lets you upload the internal standard data in CSV/TSV/XLS/XLSX format."),
        p("Data file includes sample ID and correspnding internal standards."),
        p("After uploading, you can preview the data and check validation results here.")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Validation message for internal standard file
  output$internal_standard_validation <- renderUI({
    req(internalStandardFile())
    mode <- input$lipidROW_internal  # declare rely
    
    if (!is.null(input$internal_standard_file)){
      ext <- tools::file_ext(input$internal_standard_file$name) 
      if (!ext %in% c("csv", "CSV","tsv", "TSV","xls", "xlsx", "XLS", "XLSX")) {
        validation_pass(FALSE)
        return(HTML('<span style="color:red;">❌ lease upload a valid CSV/XLS/XLSX file for internal standard.</span>'))
      } 
    }
    if (!suppressWarnings(setequal(rownames(internalStandardFile()),rownames(originalData())))) {
      validation_pass(FALSE)
      return(HTML('<span style="color:red;">❌ Please ensure sample names of 
                  internal standard are consistent with the lipidomics data.</span>'))
    }
    validation_pass(T)
    HTML('<span style="color:green;">Internal Standard file successfully uploaded.</span>')
  })
  
  parsed_standards <- eventReactive(validation_pass(), {
    req(isTRUE(validation_pass()), internalStandardFile(), !input$skip_upload_internal)
    internal_standard_data <- internalStandardFile()
    colnames(internal_standard_data) <- clean_names(colnames(internal_standard_data))
    parse_name(colnames(internal_standard_data),IS=T)
  })
  
  duplicated_lipid_classes <- reactive({
    parsed <- parsed_standards()
    duplicated_classes <- parsed$Lipid.class[duplicated(parsed$Lipid.class) | duplicated(parsed$Lipid.class,
                                                                                         fromLast = TRUE)]
    parsed[parsed$Lipid.class %in% duplicated_classes, ]
  })
  
  output$duplicate_selector_ui <- renderUI({
    req(validation_pass(),!input$skip_upload_internal)
    dupls <- duplicated_lipid_classes()
    if (nrow(dupls) == 0 || is.null(dupls)) return(NULL)
    # else(give warning of duplicate internal standards found!)
    warning_msg <- tags$div(
      style = "color: red; font-weight: bold; margin-bottom: 10px;",
      "Warning: Duplicate internal standards found. Please select one representative per class."
    )
    selectors <- lapply(unique(dupls$Lipid.class), function(lipid_class) {
      choices <- dupls$Name[dupls$Lipid.class == lipid_class]
      selectInput(inputId = paste0("1select_", lipid_class),
                  label = paste("Select representative for class:", lipid_class),
                  choices = choices,
                  selected = choices[1])
    })
    
    # Return warning + selectors as a tagList
    tagList(warning_msg, selectors)
  })
  
  resolved_standards_val <- reactiveVal(NULL)
  
  observeEvent({
    # Build a reactive list of all select input values for duplicated classes
    dupls <- duplicated_lipid_classes()
    lipid_classes <- unique(dupls$Lipid.class)
    list(dupls, lapply(lipid_classes, function(class) input[[paste0("1select_", class)]]))
  }, {
    dupls <- duplicated_lipid_classes()
    if (nrow(dupls) == 0 || is.null(dupls)) {
      resolved_standards_val(parsed_standards())
      return()
    }
    
    lipid_classes <- unique(dupls$Lipid.class)
    req(all(paste0("1select_", lipid_classes) %in% names(reactiveValuesToList(input))))
    
    selected <- sapply(lipid_classes, function(class) input[[paste0("1select_", class)]], simplify = FALSE)
    selected_df <- data.frame(parse_name(clean_names(unlist(selected)),IS = T))
    nondupls <- parsed_standards()[!(parsed_standards()$Lipid.class %in% names(selected)), ]
    final <- rbind(nondupls, selected_df)
    resolved_standards_val(final)
  })
  
  resolved_standards <- reactive({ 
    req(resolved_standards_val())
    resolved_standards_val()
    })
  
  internalStandardFile_clean <- reactiveVal(NULL)
  
  observeEvent(resolved_standards(),{
    req(internalStandardFile(),!input$skip_upload_internal)
     
    original_df <- internalStandardFile()
    colnames(original_df) <- clean_names(colnames(original_df))  # Clean column names
    # keep the same name as the resolved_standards
    
    trim_df <- original_df[,  colnames(original_df) %in% resolved_standards()$Name, drop = FALSE]
    meta <- metadataWithGroup()
    if (!is.null(meta)) trim_df <- trim_df[rownames(meta), , drop = FALSE]
    
    internalStandardFile_clean(trim_df)
  })
  
  # Internal Standard preview table
  output$InternalStandardPreview <- DT::renderDataTable({
    req(internalStandardFile_clean())
    is_data <- internalStandardFile_clean()
    if (identical(input$lipidROW_internal, "lipids on the rows")) {
      is_data <- as.data.frame(t(is_data))
    }
   
    datatable(is_data, options = list(pageLength = 10), rownames = TRUE)
  })
  
  output$nav_ui3 <- renderUI({
    next_id <- NULL
    if ((!is.null(internalStandardFile_clean())) | (input$skip_upload_internal)) {
      next_id <- "next3"}
    nav_buttons(prev_id = "prev3", next_id = next_id)
  })
  
  output$download_example_i <- downloadHandler(
    filename = function() {
      "Internal_Standard.xlsx"   # the name the user will see
    },
    content = function(file) {
      # copy from your www/ folder to the download target
      file.copy("www/Internal_Standard.xlsx", file)
    }
  )
  observeEvent(input$skip_upload_internal, {
    if (isTRUE(input$skip_upload_internal)) {
      internalStandardFile_clean(NULL)
      showNotification("Skip uploading Internal Standard File.", type = "warning")
    } else {
      # 重新计算
      internalStandardFile_clean(internalStandardFile())
    }
  })
  
  ## ---- Data filter ----
  abundance_cutoff <- reactive({
    req( Lipid_column_data_reseq())  # Ensure input is ready
    req(input$enable_abundance_filter)
    df <-  Lipid_column_data_reseq()
    abundance_func <- if (input$abundance_stat == "Mean") mean else median
    
    abundance_scores <- apply(
      df,
      2,
      abundance_func,
      na.rm = TRUE
    )
    
    quantile(
      abundance_scores,
      probs = input$abundance_percentile / 100,
      na.rm = TRUE
    )
  })
  
  output$abundance_cutoff_text <- renderText({
    req(abundance_cutoff())
    
    stat_label <- tolower(input$abundance_stat)
    cutoff_val <- abundance_cutoff()
    
    paste0(
      "Bottom ",
      input$abundance_percentile,
      "% features correspond to ",
      stat_label,
      " peak area ≤ ",
      signif(cutoff_val, 4)
    )
  })
  
  
  filtered_data <- reactiveVal(NULL)
  
  observeEvent(input$run_data_filtering, {
    req( Lipid_column_data_reseq())  # Ensure input is ready
    df <-  Lipid_column_data_reseq()
    removed_features_quality = NULL
    removed_features_abundance = NULL
    removed_features_variance =NULL
    
    
    numeric_cols <- names(df)
    df <- drop_all_na_columns_df(df, numeric_cols)
    
    
    
    if (all(!c(input$enable_quality_filter,
               input$enable_abundance_filter,
               input$enable_variance_filter))){
      showNotification("Filtering is skipped.", type = "warning")
      df <- df
 
    }
    else{
      # Step 1: Low Quality Filter (based on missing rate)
      if (isTRUE(input$enable_quality_filter)) {
        missing_rates <- colMeans(is.na(df))
        cutoff_missing <- input$missing_percentile / 100
        keep_cols <- missing_rates < cutoff_missing
        removed_features_quality <- colnames(df)[!keep_cols]
        df <- df[, keep_cols, drop = FALSE]
      }
      
      # Step 2: Low Abundance Filter (mean/median + percentile cutoff)
      if (isTRUE(input$enable_abundance_filter)) {
        abundance_func <- if (input$abundance_stat == "Mean") mean else median
        abundance_scores <- apply(df, 2, abundance_func, na.rm = TRUE)
        cutoff_abundance <- quantile(abundance_scores, probs = input$abundance_percentile / 100, na.rm = TRUE)
        keep_cols <- abundance_scores > cutoff_abundance
        removed_features_abundance <- colnames(df)[!keep_cols]
        df <- df[, keep_cols, drop = FALSE]
      }
      
      # Step 3: Low Variance Filter
      if (isTRUE(input$enable_variance_filter) ){
        var_scores <- switch(input$variance_method,
                             "IQR" = apply(df, 2, function(x) IQR(x, na.rm = TRUE)),
                             "SD" = apply(df, 2, sd, na.rm = TRUE),
                             "MAD" = apply(df, 2, mad, na.rm = TRUE),
                             "RSD" = apply(df, 2, function(x) {
                               mu <- mean(x, na.rm = TRUE)
                               if (mu == 0) return(0)
                               sd(x, na.rm = TRUE) / mu
                             }),
                             "MAD_RSD" = apply(df, 2, function(x) {
                               med <- median(x, na.rm = TRUE)
                               if (med == 0) return(0)
                               mad(x, constant = 1, na.rm = TRUE) / med
                             }))
        cutoff_var <- quantile(var_scores, probs = input$variance_percentile / 100, na.rm = TRUE)
        keep_cols <- var_scores > cutoff_var
        keep_cols[is.na(keep_cols)] <- FALSE  # avoid NA issues
        removed_features_variance <- colnames(df)[!keep_cols]
        df <- df[, keep_cols, drop = FALSE]
      }
      message_parts <- c()
      
      if (length(removed_features_quality) > 0) {
        if (length(removed_features_quality) <= 10){
          message_parts <- c(message_parts, paste0("Low quality features: ", paste(removed_features_quality, collapse = ", ")))
        }
        else{
          message_parts <- c(message_parts, paste0("Low quality features: ", paste(removed_features_quality[1:10], collapse = ", "),"..."))
        }
      }
      if (length(removed_features_abundance) > 0) {
        if (length(removed_features_abundance) <= 10){
          message_parts <- c(message_parts, paste0("Low abundance features: ", paste(removed_features_abundance, collapse = ", ")))
        }else{
          message_parts <- c(message_parts, paste0("Low abundance features: ", paste(removed_features_abundance[1:10], collapse = ", "),"..."))
        }
      }
      if (length(removed_features_variance) > 0) {
        if (length(removed_features_variance)<=10){
          message_parts <- c(message_parts, paste0("Low variance features: ", paste(removed_features_variance, collapse = ", ")))
        }
        else{
          message_parts <- c(message_parts, paste0("Low variance features: ", paste(removed_features_variance[1:10], collapse = ", "),"..."))
        }
      }
      
      # Combine with <br> for HTML line breaks if using `shiny::showNotification`
      message <- paste(message_parts, collapse = "<br>")
      
      showNotification("Filtering completed.", type = "message")
      if (nzchar(trimws(gsub("<br>", "", message)))) {
        showNotification(HTML(message), type = "message", duration = 10)
      }
    }
    original_rownames <- rownames(df)  # Save rownames
    rownames(df) <- original_rownames
    
    # Set result
    filtered_data(df)

  })
  
  output$summary_filtered_data <- renderUI({
    req( filtered_data())
    df <- filtered_data()
    
    HTML(sprintf('<p style="font-size:18px; color:#2E8B57; font-weight:bold;">
                 Data has %d samples and %d lipids.</p>', nrow(df), ncol(df)))
  })
  
  # Output preview
  output$filtered_data_preview <- DT::renderDataTable({
    req(filtered_data())
    DT::datatable(filtered_data(),
                  options = list(scrollX = TRUE, pageLength = 10)
                  )
  })

  
  output$download_filtered <- downloadHandler(
    filename = function() { paste0("Filtered_data_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(filtered_data(), file, row.names = TRUE) }
  )
  
  output$nav_ui4 <- renderUI({
    next_id <- NULL
    if (!is.null(filtered_data())) {
      next_id <- "next4"}
    nav_buttons(prev_id = "prev4", next_id = next_id)
  })
  
  ## ---- Imputation in Data PreProcessing: Impute Panel ----
  # create a reactive flag

  
  show_general_imputation <- reactiveVal(FALSE)
  
  # When click "Impute group-level" → show next box
  observeEvent(input$impute_action0, {
    show_general_imputation(TRUE)
  })
  
  # When user decides to skip group-level → also show next box
  observeEvent(input$skip_imputation0, {
    # Only show when skip == TRUE
    if (isTRUE(input$skip_imputation0)) {
      show_general_imputation(TRUE)
    }
    else {
    show_general_imputation(FALSE)
  }
  })
  output$show_general_imputation <- reactive({
    show_general_imputation()
  })
  outputOptions(output, "show_general_imputation", suspendWhenHidden = FALSE)
  
  # Process the lipidomics data based on the input for data format
  observeEvent(input$missing_heatmap_modal, {
    showModal(modalDialog(
      title = "Missing Value Heatmap",
      size = "xl",
      p("Missing value heatmap visualizes the pattern of missing data across samples and features in your lipidomics dataset. 
                                           Each cell in the heatmap represents a data point, with colors indicating whether the value is present or missing.
                                           This visualization helps to identify systematic missingness, assess data quality, and guide decisions on imputation strategies."),
      p("Red color indicates missing values. Hover over the heatmap to see details for each feature and sample. 
                                           Only top 40 fetaures with the most missing values are shown in the heatmap."),
      checkboxInput('show_feature',"Show the feature name in the plot", TRUE),
      withSpinner(plotlyOutput("MissingHM", height="500px",width="100%")),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  output$MissingHM <- plotly::renderPlotly({
    req(filtered_data())  # make sure filtered_data() is available
    
    # Check for missing values
    if (!anyNA(filtered_data())) {
      showNotification("No missing values detected!", type = "message")
      p <- plotly::plot_ly(
        x = 0,
        y = 0,
        type = "scatter",
        mode = "markers",
        marker = list(opacity = 0)
      ) %>%
        layout(
          annotations = list(
            x = 0.5,
            y = 0.5,
            text = "No missing values detected!",
            showarrow = FALSE,
            font = list(size = 15),
            xref = "paper",
            yref = "paper"
          ),
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )
      return(p)  # optionally return NULL to skip plotting
    }
    else{
    # If missing values exist, generate the heatmap
    plot_missing_heatmap_fast(filtered_data(), show_feature = input$show_feature)
    }
  })
  
  ## ---- Group level missingness ---- 
  imputed_df_0 <- reactiveVal(NULL)
  observe({
    if (isTRUE(input$skip_imputation0)) {
      shinyjs::disable("impute_action0")
      showNotification("Skipping Group-level Missingness Imputation.", type = "warning")
      imputed_df_0(filtered_data())
    } else {
      shinyjs::enable("impute_action0")
      imputed_df_0(NULL)
    }
  })
  missing_group_lipids <- reactiveVal(NULL)

  observe({
    req(filtered_data(),  metadataWithGroup())

    df <- filtered_data()
    met <-  metadataWithGroup()
    group_var <- input$define_group
    threshold_val <- input$group_missing_threshold

    group_missing <- detect_group_missing_threshold(
      df = df, metadata = met, group_var = group_var, threshold = threshold_val/100
    )
    missing_group_lipids(names(group_missing))

    output$group_missing_message <- renderUI({
      if (length(group_missing) == 0) {
        HTML("<p style='color:green; font-weight:bold;'>No lipids exceed the group-level missingness threshold.</p>")
      } else {
        # create a detailed list message
        message_lines <- paste0(
          "<ul>",
          paste0(
            sapply(names(group_missing), function(lipid) {
              groups <- paste(group_missing[[lipid]], collapse = ", ")
              paste0("<li><b>", lipid, "</b>: ≥", threshold_val, "% missing in Group: <b>", groups, "</b></li>")
            }),
            collapse = ""
          ),
          "</ul>"
        )

        HTML(paste0(
          "<p style='color:#b30000; font-weight:bold;'>",
          length(group_missing), " lipids have ≥", threshold_val, "% missingness in at least one group:</p>",
          message_lines
        ))
      }
    })
  })
  
    
  # impute the lipid with group missingness
  observeEvent(input$impute_action0, {
      req(filtered_data())
      
      # separate the column with group missingness and without group missingness
      df <- filtered_data()
      min <- min(df, na.rm = TRUE)
      lipids_to_impute <- missing_group_lipids()
      df_to_impute <- df[, lipids_to_impute, drop = FALSE]
      feature_min <- sapply(df[, lipids_to_impute, drop = FALSE], function(x) {
        min(x, na.rm = TRUE)
      })
      df_no_impute <- df[, !(colnames(df) %in% lipids_to_impute), drop = FALSE]

      for (i in seq_along(lipids_to_impute)) {
        lipid <- lipids_to_impute[i]
        if (input$impute_method_group=="LoD 1/5 minimum value"){
          lod <- feature_min[i] / 5
        }
        else if(input$impute_method_group=="LoD 1/2 minimum value"){
          lod <- feature_min[i] / 2
        }
        col_vec <- df_to_impute[[lipid]]
        col_vec[is.na(col_vec)] <- lod
        df_to_impute[[lipid]] <- col_vec
      }
      # combine columns back again
      combined_df <- cbind(df_to_impute, df_no_impute)

      # reorder them using alphabetical order
      combined_df <- combined_df[, order(colnames(combined_df)), drop = FALSE]
      imputed_df_0(combined_df)
      showNotification("Group-level Missingness Imputation completed.", type = "message")
    })

  ## ---- Normal missingness ---- 
  output$Missing_checks <- renderUI({
    req(imputed_df_0())
    na_num <- sum(is.na(imputed_df_0()))
    if (na_num == 0) {
      HTML('<span style="color:green; font-size: 16px;">After group level missing is handled, no missing values found.</span>')
    }
    else{
      message <- paste0('<span style="color:red;"> After group level missing is handled, found ',na_num,' missing values. Check the missing values heatmap for more information. </span>')
      HTML(message)
    }
  })
  imputedData <- reactiveVal(NULL)
  imputedInternalStandard <- reactiveVal(NULL)
  observe({
    if (isTRUE(input$skip_imputation)) {
      shinyjs::disable("impute_action")
      imputedData(imputed_df_0())  # Skip imputation, use original data
      imputedInternalStandard(internalStandardFile_clean())  # Use original internal standard data
      showNotification("Skipping Imputation. Please only skip imputation when the dataset do not have missing values.", 
                       type = "error")
    } else {
      shinyjs::enable("impute_action")
      imputedData(NULL)  # Reset imputed data
      imputedInternalStandard(NULL)  # Reset imputed internal standard data
    }
  })


  
  observeEvent(input$impute_action, {
    req(imputed_df_0())  
    data_to_impute <- imputed_df_0()
    method <- input$impute_method
    k <- ifelse(method == "knn", input$knn_k, 5)
    
    imputed_result <- impute_missing_values(data_to_impute, method = method, k = k)
    imputedData(imputed_result) 
    
    if (!is.null(internalStandardFile_clean())){
    imputed_is_data <- impute_missing_values(internalStandardFile_clean(), method = method, k = k)
    
    imputedInternalStandard(imputed_is_data)  # Update the reactive value
    }
  })
  output$summary_imputedData <- renderUI({
    req( imputedData())
    df <- imputedData()
    
    HTML(sprintf('<p style="font-size:18px; color:#2E8B57; font-weight:bold;">
                 Data has %d samples and %d lipids.</p>', nrow(df), ncol(df)))
  })
  output$imputedDataPreview <- DT::renderDataTable({
    req(imputedData())  
    datatable(imputedData(), options = list(pageLength = 10), rownames = TRUE)
  })
  
  # observeEvent(input$impute_internal_standard_action, {
  #   req(internalStandardFile_clean())  # Ensure the internal standard data exists
  #   imputedInternalStandard(internalStandardFile_clean())
  #   if (input$impute_internal_standard_action) {
  #     method <- input$impute_method
  #     k <- ifelse(method == "knn", input$knn_k, 5)
  #     
  #     imputed_is_data <- impute_missing_values(internalStandardFile_clean(), method = method, k = k)
  #     
  #     imputedInternalStandard(imputed_is_data)  # Update the reactive value
  #   }
  #   else {
  #     imputedInternalStandard(internalStandardFile_clean())  # Reset if checkbox is unchecked
  #   }
  # })
  
  # # Render the imputed internal standard data preview
  # output$InternalStandardPreview_imputed <- DT::renderDataTable({
  #   req(imputedInternalStandard())  # Ensure internal standard data is available
  #   # Display imputed internal standard data
  #   datatable(imputedInternalStandard(), options = list(pageLength = 10), rownames = TRUE)
  # })
  output$download_imputed_data <- downloadHandler(
    filename = function() { paste0("imputed_lipid_data_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(imputedData(), file, row.names = TRUE) }
  )
  
  # output$download_imputed_is <- downloadHandler(
  #   filename = function() { paste0("imputed_internal_standard_", Sys.Date(), ".csv") },
  #   content = function(file) { write.csv(imputedInternalStandard(), file, row.names = TRUE) }
  # )
  
  output$nav_ui5 <- renderUI({
    next_id <- NULL
    if ((!is.null(imputedData()) & !is.null(imputedInternalStandard())) || (!is.null(imputedData()) & input$skip_upload_internal)){
      next_id <- "next5"}
    nav_buttons(prev_id = "prev5", next_id = next_id)
  })
  
  ## ---- Combine Duplicated Lipids ----  

  # ---------- V1: parse table ----------
  # Reactive expression to clean lipid names and parse lipid information
  parsedData <- reactive({
    req(imputedData())  # Ensure processed data is available
    
    # Clean lipid names
    cleanlipidname <- clean_names(colnames(imputedData()))  # Clean column names
    
    # Parse lipid names (ensure parse_name function is correctly defined)
    parsedTable <- parse_name(cleanlipidname)  # Store parsed results
    
    return(list(parsedTable = parsedTable, cleanlipidname = cleanlipidname))  # Return both
  })
  
  # store a modifiable table
  parsed_table <- reactiveVal()
  parsed_table_base <- reactiveVal()
  
  # initialize when parsing is done
  observeEvent(parsedData(), {
    req(parsedData())
    parsed_table(parsedData()$parsedTable)
    parsed_table_base(parsedData()$parsedTable)
  })
  processedRAW <- reactive({
    req(imputedData(), parsedData())
    df <- imputedData()
    colnames(df) <- parsedData()$cleanlipidname
    return(df)
  })
  
  dup_message <- reactiveVal("")
  
  observeEvent(processedRAW(), {
    req(parsed_table_base())
    dup_message(detect_duplication(parsed_table_base()))
  }, ignoreNULL = TRUE)
  
  output$duplicate_lipid_names <- renderUI({
    msg <- dup_message()
    msg <- if (is.null(msg) || is.na(msg)) "" else trimws(as.character(msg))
    
    
    if (!nzchar(msg)) {
      HTML("<span style='color:green;'>No duplicate lipid names detected.</span>")
    } else {
      HTML(paste0("<span style='color:black;'>", msg, "</span>"))
    }
  })

  
  combine_rules <- reactiveValues(count = 0)
  
  observe({
    req(parsed_table_base(),processedRAW(),input$skip_combine == FALSE)
    if (is.null(combine_rules$ids)) combine_rules$ids <- c()  # in case it hasn't been defined
    if (length(combine_rules$ids) == 0) {
      first_id <- 1
      combine_rules$ids <- c(first_id)
      insert_combine_rule_ui(first_id,parsed_table_base())
    }
  })
  observeEvent(input$add_combine_rule, {
    req(parsed_table_base())
    new_id <- if (length(combine_rules$ids) == 0) 1 else max(combine_rules$ids) + 1
    combine_rules$ids <- c(combine_rules$ids, new_id)
    insert_combine_rule_ui(new_id,parsed_table_base())
  })
  
  
  
  observe({
    lapply(combine_rules$ids, function(id) {
      observeEvent(input[[paste0("remove_rule_", id)]], {
        # Remove the UI
        removeUI(selector = paste0("#combine_rule_", id))
        
        # Remove the id from tracking
        combine_rules$ids <- setdiff(combine_rules$ids, id)
      }, ignoreInit = TRUE)
    })
  })
  
  observeEvent(input$CombineButton, {
    all_rules <- lapply(combine_rules$ids, function(i) {
      list(
        class = input[[paste0("lipid_class_", i)]],
        method = input[[paste0("combine_method_", i)]]
      )
    })
    
    
  })
  
  
  
  combined_result <- eventReactive(input$CombineButton, {
    req(processedRAW())
    dataset <- processedRAW()
    parsed_table_base <- parsed_table_base()
    
    # Get all rules
    all_rules <- lapply(combine_rules$ids, function(i) {
      list(
        class = input[[paste0("lipid_class_", i)]],
        method = input[[paste0("combine_method_", i)]]
      )
    })

    selected_classes <- sapply(all_rules, function(rule) rule$class)
    
    has_all_merge <- "Combine all duplicated lipids based on clean names in the parse table" %in% selected_classes
    specific_classes <- setdiff(selected_classes, 
                                c("Combine all duplicated lipids based on clean names in the parse table"))
    
    
    if (has_all_merge) {
      # Warn and ignore specific class rules
      if (length(specific_classes) > 0) {
        showNotification("Warning: 'Combine all duplicated lipids based on clean names in the parse table' selected. Other class-specific rules will be ignored.", type = "warning")
      }
      
      colnames(dataset) <- parsed_table_base$Clean.Name

      
      result <- combine_duplicated_lipids(
        dataset,
        merged_function_string = all_rules[[which(selected_classes == "Combine all duplicated lipids based on clean names in the parse table")]]$method
      )
      return(result)
    }
    else{
    combined_parts <- list()
    messages <- c()
    
    for (rule in all_rules) {
      class <- rule$class
      method <- rule$method
      
      
      temp_result <- swap_clean_name(lipid_class = class, dataset = dataset, lipid_df_samples = parsed_table_base)
      temp_dataset <- temp_result$data
      result <- combine_duplicated_lipids(temp_dataset, method)
      
      temp_dataset <- result$data
      messages <- c(messages, paste0("Class ", class, ": ", result$message))
    }
    
    final_combined <- as.data.frame(temp_dataset)
    return(list(data = final_combined, message = paste(messages, collapse = "\n")))
    }
  })
  
  
  combinedData <- reactive({
    req(combined_result())  # Ensure combined data is available
    combined_result()$data  # Return the combined data
  })
  
  observeEvent(input$CombineButton, {
    req(combined_result())
    Message <- combined_result()$message
    showNotification(Message, type = "message")
  })
  processedData <- reactiveVal(NULL)
  
  observe(if (isTRUE(input$skip_combine)) {
    shinyjs::disable("CombineButton")
    df <- processedRAW()
    processedData(df)  # Use original data if skipping combination
    showNotification("Skipping combination of duplicated lipids.", type = "warning")
  }else{
    shinyjs::enable("CombineButton")
    processedData(combinedData())  # Update processed data with combined result
  })
  
  output$summary_processedData <- renderUI({
    req( processedData())
    df <- processedData()
    
    HTML(sprintf('<p style="font-size:18px; color:#2E8B57; font-weight:bold;">
                 Data has %d samples and %d lipids.</p>', nrow(df), ncol(df)))
  })
  
  output$combinedDataPreview <- DT::renderDataTable({
    req(processedData())  # Only show if combination is done
    # Display combined data
    datatable(processedData(), options = list(pageLength = 10), rownames = TRUE)
  })
  # ---------- V2: parse table ----------

  go_back_df <- reactiveVal(NULL)
  observeEvent(processedData(), {
    
    req(processedData())

    parsed_combined <- parse_name(colnames(processedData()))
    
    # if (isTRUE(input$skip_combine)) {
    #   parsed_combined$Clean.Name <- parsed_combined$Name
    # }
    go_back_df(parsed_combined)
    parsed_table(parsed_combined)
    
  })
  
  output$download_combined_data <- downloadHandler(
    filename = function() { paste0("combined_lipid_data_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(processedData(), file, row.names = TRUE) }
  )
  
  output$nav_ui6 <- renderUI({
    next_id <- NULL
    if ((input$CombineButton > 0)|input$skip_combine) {
      next_id <- "next6"}
    nav_buttons(prev_id = "prev6", next_id = next_id)
  })
  
  ## ---- Parsing Name ----
  
  observeEvent(input$help_btn_parse_upload, {
    showModal(modalDialog(
      title = "Help Information",
      tagList(
        p("Upload your lipid parsing table in CSV format."),
        p("This feature allows you to review and edit the parsed lipid information before using it in future analyses."),
        p("The parsing table should contain the following columns: 
          Name, Lipid.class, Chain1, Chain1.unsaturation, 
          Chain2, Chain2.unsaturation, Chain3, Chain3.unsaturation, Total.carbon, Total.unsaturation, Clean.Name."),
        p("Do not rename, remove, or reorder the required column names"),
        p("Do not rename, or remove the lipid name column 'Name', as they are used to match the expression dataset."),
        p("Missing values may be represented as NA, NaN, N/A, empty strings, or blank cells."),
        p("Do not add or remove lipid entries. Only modify the existing parsing information if needed."),
        p("An example parsing table is shown below."),
        img(
          src = "Parsing_instruction.png",
          width = "100%",
          style = "max-width:900px; border:1px solid #ddd;"
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
 
  # Render the parsed lipid names table
  output$cleantable <- DT::renderDataTable({
    req(parsed_table(),input$parseButton)  # Ensure parsed data is available
    # Display parsed data
    # only show the selected lipid class data
    
    datatable(parsed_table(), 
              editable = TRUE,   
              filter = "top",
              options = list(pageLength = 10), rownames = TRUE)
  })
  
  observeEvent(input$cleantable_cell_edit, {
    info <- input$cleantable_cell_edit
    str(info)  # debug: see what was edited
    
    newdata <-parsed_table()
    newdata[info$row, info$col] <- info$value
    parsed_table(newdata)   # update the reactive dataset
  })
  
  
  observe({
    req(input$new_parse_file)
    ext <- tools::file_ext(input$new_parse_file$name)
    df <- NULL
    # ---------- Step 1: read the file ----------
    if (ext %in% c("csv", "CSV")) {
      df <- read.csv(input$new_parse_file$datapath,
                     header = T,
                     check.names = FALSE,
                     stringsAsFactors = FALSE)
    } 
    else {
      showNotification("Unsupported file type", type = "error")
      return(NULL)
    }
    # ---------- Step 2: validate df ----------
    if (is.null(df) || ncol(df) < 2 || nrow(df) < 2) {
      showNotification("Error: Uploaded file is empty or it only has one column. ", type = "error")
      return(NULL)
    }
    rownames(df) <- df[[1]]
    df <- df[, -1, drop = FALSE]
  
    # if the df has NaN, NA, " ", or ""
    # treating them as missing value
    df[is.na(df) | df == " " | df == ""|df == "NaN"|df == "N/A"] <- NA
    
    # Check same column nam are Name	Lipid.class	Chain1	Chain1.unsaturation	Chain2	Chain2.unsaturation	Chain3	Chain3.unsaturation	Total.carbon	Total.unsaturation	Clean.Name
    expected_cols <- c("Name", "Lipid.class", "Chain1", "Chain1.unsaturation", 
                       "Chain2", "Chain2.unsaturation", "Chain3", "Chain3.unsaturation",
                       "Total.carbon", "Total.unsaturation", "Clean.Name")
    if (!all(expected_cols %in% colnames(df))) {
      showNotification(paste0("Error: Uploaded file must contain the following columns: ", paste(expected_cols, collapse = ", ")), type = "error")
      df <- go_back_df()  # revert to previous table if validation fails
    }
    
    
    cleanlipidname <- colnames(processedData())
    uploaded_names <- clean_names(df$Name)
    
    if (
      length(cleanlipidname) != length(uploaded_names) ||
      !setequal(cleanlipidname, uploaded_names)
    ) {
      
      showNotification(
        "Error: Uploaded file must contain exactly the same lipid names as the processed data (no missing and no extra lipids).",
        type = "error"
      )
      
      df <- go_back_df()
    }

    
    parsed_table(df)   # <-- store in reactiveVal
  })
  
  

  observeEvent(input$cancel_parse_upload, {
    # ---------- Step 3: Go back to the default when cancel ----------
    if (isTRUE(input$cancel_parse_upload)) {
      parsed_table(go_back_df())
    }
    
  })

  
  output$download_cleantable <- downloadHandler(
    filename = function() { paste0("parsed_lipid_names_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(parsed_table(), file, row.names = TRUE) }
  )
  
  output$nav_ui7 <- renderUI({
    next_id <- NULL
    if (!is.null(parsed_table())){
      next_id <- "next7"}
    nav_buttons(prev_id = "prev7", next_id = next_id)
  })
  

  
  ## ---- Data Preview ----
  observeEvent(input$help_pie, {
    showModal(modalDialog(
      title = "Help Information",
      tagList(
        p("The pie plot shows the distribution of lipid classes in the dataset based on the combined lipid data."),
        p("Sum of each lipid class across all samples is calculated to determine their proportions."),
        p('Hover over each section of the pie chart to see the lipid class name, total intensity, and percentage in the dataset.' )
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$help_LCB, {
    showModal(modalDialog(
      title = "Help Information",
      tagList(
        p("The lipid class boxplot visualizes the distribution of lipid class averages across all samples."),
        p("For better visulization, data was log2 transform before the plotting."),  
        p("Log2 transformation won't be applied in the actual data used for downstream analysis.")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$help_PCA, {
    showModal(modalDialog(
      title = "Help Information",
      tagList(
        p("The PCA plot visualizes the principal component analysis of the lipidomics data."),
        p("Group labels are based on the group variables from the metadata users uploaded."),  
        p("The x y axis are scaled principal components (PC1 and PC2) that capture the most variance in the data.")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$help_sample_boxplot, {
    showModal(modalDialog(
      title = "Help Information",
      tagList(
        p("The sample boxplot visualizes the distribution of lipid intensities for each sample."),
        p("Users can use this boxplot to assess the overall intensity distribution and identify potential outliers."),
        p("For better visulization, data was log2 transform before the plotting."),  
        p("Log2 transformation won't be applied in the actual data used for downstream analysis.")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$help_barplot, {
    showModal(modalDialog(
      title = "Help Information",
      tagList(
        p("The bar plot visualizes how many lipids in each lipid class in the dataset."),
        p("Users can use this bar plot to assess the composition of lipid classes in their dataset.")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  observeEvent(input$help_barplot_g, {
    showModal(modalDialog(
      title = "Help Information",
      tagList(
        p("Users can use this bar plot to assess the sample size of each group in their dataset.")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  output$lipid_class_boxplot <- renderPlotly({
    df <-log2(processedData() + 1)
    p <- boxplot_lipid_class(df,parsed_table(),
                             Title = "Boxplot of Lipid Class Averages Across Samples(Log2-Transformed)")
    ggplotly(p)  
    })
  output$sample_boxplot <- renderPlotly({
    df_p <- processedData()
    if (nrow(df_p) >= 80) {
      p <- plotly::plot_ly(
        x = 0,
        y = 0,
        type = "scatter",
        mode = "markers",
        marker = list(opacity = 0)
      ) %>%
        layout(
          annotations = list(
            x = 0.5,
            y = 0.5,
            text = "We don't support boxplot<br>if your expression data has more than 80 samples.",
            showarrow = FALSE,
            font = list(size = 15),
            xref = "paper",
            yref = "paper"
          ),
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )
      return(p)
    }
    else{
    df_p <- as.data.frame(t(df_p))
    df_log <- log2(df_p + 1)
    temp_plot <- create_boxplot(df_log,"Boxplot for samples(Log2-Transformed)")
    ggplotly(temp_plot)
    }
    
  })
  output$pca_plot_preview <- renderPlotly({
    df_p <- processedData()
    group <- metadataWithGroup()[[input$define_group]]
    temp_plot <- create_pca_plot(df_p,group,center_m = T, scale = T)
    ggplotly(temp_plot,tooltip = c("text", "x", "y"))
  })
  
  output$pie_plot <- renderPlotly({
    p <- create_pie_plot(processedData(),parsed_table())
    p
  })
  
  output$group_barplot<- renderPlotly({
    p <- plot_group_distribution(metadataWithGroup(),input$define_group)
    ggplotly(p,tooltip = "text")
  })
  
  output$lipid_barplot <- renderPlotly({
    p <- create_lipid_bar_plot(parsed_table())
    ggplotly(p)
  })
  
  
  output$nav_ui8 <- renderUI({
    nav_buttons(prev_id = "prev8", next_id = "next8")
  })
  
  

  ## ---- Internal Standard Selection ----
  
  # Reactive value to store current selection table
  internal_standard_selections <- reactiveVal()
  
  
  # On initial load: create smart default selections
  observe({
    req(parsed_table(), resolved_standards())
    
    sample_classes <- unique(parsed_table()$Lipid.class)
    IS_classes <- unique(resolved_standards()$Lipid.class)
    
    default_standards <- find_best_match(IS_classes, sample_classes)
    
    internal_standard_selections()  # Ensure the reactive value is initialized
    internal_standard_selections(default_standards)
    
  })
  
  
  # Render editable selection UI
  output$internalStandardSelection <- renderUI({
    req(internal_standard_selections())
    selections <- internal_standard_selections()
    
    verticalLayout(
      # Header row
      fluidRow(
        column(5, strong("Sample lipid class")),
        column(5, strong("Internal standard selected"))
      ),
      tags$hr(style = "margin-top: 2px; margin-bottom: 6px;"),  # Optional separator line
      lapply(seq_along(selections$Lipid_Class_Sample), function(i) {
        choices <- c("", unique(resolved_standards()$Lipid.class))
        selected_value <- selections$Internal_standard[i]
        fluidRow(
          column(5, p(selections$Lipid_Class_Sample[i])),
          column(5, selectInput(
            inputId = paste0("select_", selections$Lipid_Class_Sample[i]),
            label = NULL,
            choices = choices,
            selected =  selected_value 
          ))
        )
      })
    )
  })
  
  observeEvent(input$reset_internal_standard_selection, {
    req(parsed_table(), resolved_standards())
    
    sample_classes <- unique( parsed_table()$Lipid.class)
    IS_classes <- unique(resolved_standards()$Lipid.class)
    
    default_standards <- find_best_match(IS_classes, sample_classes)
    
    internal_standard_selections(default_standards)
    
    for (i in seq_along(default_standards$Lipid_Class_Sample)) {
      lipid_class <- default_standards$Lipid_Class_Sample[i]
      selected_val <- default_standards$Internal_standard[i]
      
      updateSelectInput(
        inputId = paste0("select_", lipid_class),
        selected = selected_val
      )
    }
    
    showNotification("Internal standard selections have been reset to default.", type = "warning")
  })
  
  
  #Save button behavior: capture current selections
  saved_internal_standard <- reactiveVal(NULL)
  
  observeEvent(input$save_internal_standard_selection, {
    req( parsed_table(), resolved_standards())
    
    lipid_classes <- unique( parsed_table()$Lipid.class)
    
    selections <- map_chr(lipid_classes, function(class) {
      input[[paste0("select_", class)]] %||% ""
    })
    
    final_selection <- data.frame(
      Lipid_Class_Sample = lipid_classes,
      Internal_Standard = selections,
      stringsAsFactors = FALSE
    )
    # If some internal standard selections in the final_selection is still "",
    # Trigger a warning notification
    if (any(final_selection$Internal_Standard == "")) {
      showNotification("Warning: Some lipid classes do not have an internal standard selected. 
                       These lipid classes would not be normalized.", type = "error")
    }
    saved_internal_standard(final_selection)
    
    showNotification("Internal standard selections saved successfully.", type = "message")
  })
  
  
  # Reactive value to track if the user clicked "Save Selection"
  saved <- reactiveVal(FALSE)
  
  # Observe the Save button
  observeEvent(input$save_internal_standard_selection, {
    saved(TRUE)
  })
  
  # Render the Run Normalization button conditionally
  output$run_norm_ui <- renderUI({
    if (saved()) {
      actionButton("run_internal_norm", "Run Normalization")
    }
  })
  
  #1 add a column to parsed_table() to store the internal standard 
  # based on the saved_internal_standard
  
  updated_parsed_table <- reactiveVal(NULL)
  observeEvent(saved_internal_standard(), {
    req( parsed_table())
    
    df <-  parsed_table()
    df$Internal_Standard <- NA      
    
    # make the Internal_Standard column the second column
    df <- df[, c(1, ncol(df), 2:(ncol(df) - 1))]
    
    for (i in seq_len(nrow(df))) {
      class <- df$Lipid.class[i]
      match_row <- saved_internal_standard()$Internal_Standard[
        saved_internal_standard()$Lipid_Class_Sample == class
      ]
      if (length(match_row) > 0) {
        df$Internal_Standard[i] <- match_row
      }
    }
    updated_parsed_table(df)
  })
  
  output$updatedParsedTable <- DT::renderDataTable({
    req(updated_parsed_table())
    datatable(updated_parsed_table()[,c(1,2,3)], options = list(pageLength = 10), rownames = TRUE)
  })
  
  
  # ---- Normalization by Internal Standard ----
  normalization_messages <- reactiveVal(NULL)
  internal_normed_matirx <- reactiveVal(NULL)
  
  observeEvent(input$run_internal_norm, {
    req(processedData(), updated_parsed_table(), imputedInternalStandard())
    logs <- character()
    warnings <- character()
    
    # Capture both print and warning messages
    result <- withCallingHandlers({
      logs <- capture.output({
        result <- normalize_by_internal_standard(processedData(), 
                                                 updated_parsed_table(),
                                                 imputedInternalStandard())
        internal_normed_matirx(result)
      })
      result 
    }, warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning") 
    })
    
    # Combine logs and warnings
    combined_logs <- c(logs, if (length(warnings) > 0) c("Warnings:", warnings))
    normalization_messages(paste(combined_logs, collapse = "\n"))
  })
  
  output$normalizationLog <- renderText({
    req(normalization_messages())
    normalization_messages()
  })
  
  observe(if(input$skip_internal) {
    shinyjs::disable("run_internal_norm")
    internal_normed_matirx(processedData())  # Use original data if skipping normalization
    updated_parsed_table( parsed_table()) # Use original parsed table
    showNotification("Skipping normalization by internal standard.", type = "warning")
  }
  else{
    shinyjs::enable("run_internal_norm")
  })
  
  output$summary_internal_normed_matirx <- renderUI({
    req( internal_normed_matirx())
    df <- internal_normed_matirx()
    
    HTML(sprintf('<p style="font-size:18px; color:#2E8B57; font-weight:bold;">
                 Data has %d samples and %d lipids.</p>', nrow(df), ncol(df)))
  })
  
  output$normalizedDataPreview <- DT::renderDataTable({
    req(internal_normed_matirx())
    df <- internal_normed_matirx()
    datatable(df, options = list(pageLength = 10), rownames = TRUE)  %>%
      formatRound(columns = which(sapply(df, is.numeric)), digits = 4)
  })
  
  output$download_updated_parsed <- downloadHandler(
    filename = function() { paste0("updated_parsed_table_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(updated_parsed_table(), file, row.names = TRUE) }
  )
  
  output$download_normalized <- downloadHandler(
    filename = function() { paste0("normalized_by_internal_standard_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(internal_normed_matirx(), file, row.names = TRUE) }
  )
  
  output$nav_ui9 <- renderUI({
    next_id <- NULL
    if (!is.null(internal_normed_matirx()) ) {
      next_id <- "next9"}
    nav_buttons(prev_id = "prev9", next_id = next_id)
  })
  ## ---- Metadata normalization ----
  # create a reactive flag
  show_met_norm2 <- reactiveVal(FALSE)
  
  # When click "Impute group-level" → show next box
  observeEvent(input$run_supplement_normalization, {
    show_met_norm2(TRUE)
  })
  
  # When user decides to skip group-level → also show next box
  observeEvent(input$skip_supp, {
    # Only show when skip == TRUE
    if (isTRUE(input$skip_supp)) {
      show_met_norm2(TRUE)
    }
    else{
      show_met_norm2(F)
    }
  })
  
  output$show_met_norm2 <- reactive({
    show_met_norm2()
  })
  outputOptions(output, "show_met_norm2", suspendWhenHidden = FALSE)
  
  observeEvent(metadataWithGroup(), {
    updateSelectInput(session,
                      "metadata_supplement",
                      choices = colnames(metadataWithGroup()),
                      selected = colnames(metadataWithGroup())[2])
  })
  meta_normed_matrix_1 <- reactiveVal(NULL)
  observe(if (isTRUE(input$skip_supp)){
    meta_normed_matrix_1(internal_normed_matirx())  # Use original data if skipping normalization
    showNotification("Skipping normalization by supplement coefficient.", type = "warning")
  })
  observeEvent(input$run_supplement_normalization, {
    req(input$supplement_coeffient, internal_normed_matirx())
    internal_normed_df <- internal_normed_matirx()
    internal_normed_df <- internal_normed_df / input$supplement_coeffient
    meta_normed_matrix_1(internal_normed_df)
    
    showNotification(paste("Normalization by supplement coefficient", 
                           input$supplement_coeffient, "completed."), 
                     type = "message")
  })
  meta_normed_matrix2 <- reactiveVal(NULL)
  observe(if (isTRUE(input$skip_meta)) {
    meta_normed_matrix2(meta_normed_matrix_1())  # Use original data if skipping normalization
    showNotification("Skipping normalization by metadata.", type = "warning")
  })
  observeEvent(input$run_metadata_normalization, {
    req(metadataWithGroup(), input$metadata_supplement,meta_normed_matrix_1)
    metadata <- metadataWithGroup()
    selected_column <- input$metadata_supplement
    supplement_data <- metadata[[selected_column]]
    # Ensure numeric
    supplement_data <- as.numeric(supplement_data)
    
    # Check if coercion resulted in NA (non-numeric values originally)
    if (any(is.na(supplement_data))) {
      showNotification("Selected metadata column contains NA values.", type = "error")
    } else {
      # Make sure the dimensions match
      norm_matrix <- meta_normed_matrix_1()
      # Normalize each row by corresponding metadata value
      normed <- norm_matrix / supplement_data
      meta_normed_matrix2(normed)
      showNotification(paste("Normalization by", selected_column, "completed."), type = "message")
    }
  })
  
  output$summary_meta_normed_matrix2 <- renderUI({
    req( meta_normed_matrix2())
    df <- meta_normed_matrix2()
    
    HTML(sprintf('<p style="font-size:18px; color:#2E8B57; font-weight:bold;">
                 Data has %d samples and %d lipids.</p>', nrow(df), ncol(df)))
  })
  
  output$MetanormalizedDataPreview <- DT::renderDataTable({
    req(meta_normed_matrix2())
    df <- meta_normed_matrix2()
    datatable(df, options = list(pageLength = 10), rownames = TRUE)  %>%
      formatRound(columns = which(sapply(df, is.numeric)), digits = 4)
    
    })
  output$download_meta_normalized <- downloadHandler(
    filename = function() { paste0("meta_normalized_data_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(meta_normed_matrix2(), file, row.names = TRUE) }
  )
  
  output$nav_ui10 <- renderUI({
    nav_buttons(prev_id = "prev10", 
                next_id = if (!is.null(meta_normed_matrix2())) "next10" else NULL)
  })
  
  ## ---- Normalized plan ----
  normalized_plan_data <- reactiveVal(NULL)


  observeEvent(input$run_plan_normalization, {
    req(meta_normed_matrix2(), updated_parsed_table())

    df <- meta_normed_matrix2()
    if (input$norm1 == "none_1"){
      df1 <- df
    }
    else if (input$norm1 == "sample_sum"){
      df1 <- sample_normalize(df,"sum")
      showNotification("Normalized by sum (samplewise) completed.", type = "message")
    }
    else if (input$norm1 == "sample_mean"){
      df1 <- sample_normalize(df,"mean")
      showNotification("Normalized by mean (samplewise) completed.", type = "message")
    }
    else if (input$norm1 == "sample_median"){
      df1 <- sample_normalize(df,"median")
      showNotification("Normalized by median (samplewise) completed.", type = "message")
    }
    else if (input$norm1 == "lipid_class_sum"){
      df1 <- lipid_class_normalization(df,updated_parsed_table(),"sum")
      showNotification("Normalized by lipid class sum completed.", type = "message")
    }
    else if (input$norm1 == "lipid_class_mean"){
      df1 <- lipid_class_normalization(df,updated_parsed_table(),"mean")
      showNotification("Normalized by lipid class mean completed.", type = "message")
    }
    else if (input$norm1 == "lipid_class_median"){
      df1 <- lipid_class_normalization(df,updated_parsed_table(),"median")
      showNotification("Normalized by lipid class median completed.", type = "message")
    }
    else if (input$norm1 == 'qnorm'){
      df1 <- quantile_normalize(df)
      showNotification("Quantile normalization completed.", type = "message")
    }

    if (input$norm2 == "none_2"){
      df2 <- df1
    }
    else if(input$norm2 == "log_t"){
      if(input$log_base == "ln"){
        df2 <- log(df1)
        showNotification("Nature log transformation completed.", type = "message")
      }
      else if(input$log_base == "log2"){
        df2 <- log2(df1)
        showNotification("Log2 transformation completed.", type = "message")
      }
      else if(input$log_base == "log10"){
        df2 <- log10(df1)
        showNotification("Log10 transformation completed.", type = "message")
      }
    }
    else if (input$norm2 == "logit_t"){
      df2 <-logit_transform(df1)
      showNotification("Logit transformation completed.", type = "message")
      if (input$norm1 != "lipid_class_sum"){
        showNotification("We suggest logit transfromation after lipid class sum normalization ONLY.
                         Otherwise, it might generate NA values in the result", type = "warning")
      }
    }
    else if (input$norm2 == "s_root"){
      if (any(df1 < 0, na.rm = TRUE)) {
        showNotification("df1 contains negative values. sqrt() will return NaN for those entries.", type = "warning")
      }
      df2 <- sqrt(df1)
      showNotification("Square root transformation completed.", type = "message")
    }
    else if (input$norm2 == "c_root"){
      df2 <- cuberoot(df1)
      showNotification("Cubic root transformation completed.", type = "message")
    }
    if (input$norm3 == "mean_centered") {
      df3 <- mean_center(df2)
      showNotification("Mean centered transformation completed.", type = "message")
    } else if (input$norm3 == "auto_scaling") {
      df3 <- auto_scale(df2)
      showNotification("Auto scaling completed.", type = "message")
    } else if (input$norm3 == "pareto_scaling") {
      df3 <- pareto_scale(df2)
      showNotification("Pareto scaling completed.", type = "message")
    } else if (input$norm3 == "range_scaling") {
      df3 <- range_scale(df2)
      showNotification("Range scaling completed.", type = "message")
    } else {
      df3 <- df2
    }
    if(any(is.na(df3))) {
      showNotification("Normalization resulted in NA values. Please check normalization method.", type = "warning")
    }

    normalized_plan_data(df3)

  })

  output$summary_normalized_plan_data <- renderUI({
    req( normalized_plan_data())
    df <- normalized_plan_data()

    HTML(sprintf('<p style="font-size:18px; color:#2E8B57; font-weight:bold;">
                 Data has %d samples and %d lipids.</p>', nrow(df), ncol(df)))
  })

  output$PlanDataPreview <- DT::renderDataTable({
    req(normalized_plan_data(),input$run_plan_normalization)
    df <- as.data.frame(normalized_plan_data())
    datatable(df, options = list(pageLength = 10), rownames = TRUE)  %>%
      formatRound(columns = which(sapply(df, is.numeric)), digits = 4)
  })

  output$download_plan_normalized <- downloadHandler(
    filename = function() {
      paste0("normalized_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(normalized_plan_data())
      write.csv(normalized_plan_data(), file, row.names = TRUE)
    }
  )

  output$nav_ui11 <- renderUI({
    nav_buttons(prev_id = "prev11",
                next_id =if (!is.null(normalized_plan_data())) "next11" else NULL)
  })

  observeEvent(input$help_btn_norm1, {
    showModal(modalDialog(
      title = "Help Information",
      tagList(
        HTML("
        <h4>1. Normalized by Sum (Sample-wise)</h4>
        Each sample is divided by its total signal (sum of all lipids).<br>
        This controls for overall signal intensity differences.<br>
        Formula: x' = x / sum(x)<br><br>

        <h4>2. Normalized by Median (Sample-wise)</h4>
        Each sample is divided by its median value.<br>
        Useful when samples differ mainly by global shifts.<br>
        Formula: x' = x / median(x)<br><br>

        <h4>3. Normalized by Mean (Sample-wise)</h4>
        Each sample is divided by its mean value.<br>
        Similar to median normalization but influenced more by large values.<br>
        Formula: x' = x / mean(x)<br><br>

        <h4>4. Lipid Class Sum Normalization</h4>
        Each lipid is divided by the sum of all lipids within the same class<br>
        (e.g., all PCs normalized together).<br>
        Helps control for class-specific batch effects.<br>
        Formula: x' = x / sum(class)<br><br>

        <h4>5. Lipid Class Median Normalization</h4>
        Each lipid is divided by the median value of its lipid class.<br>
        More robust to extreme lipid values within the class.<br>
        Formula: x' = x / median(class)<br><br>

        <h4>6. Lipid Class Mean Normalization</h4>
        Each lipid is divided by the mean value of its lipid class.<br>
        Useful when class values are approximately symmetric.<br>
        Formula: x' = x / mean(class)<br><br>

        <h4>7. Quantile Normalization</h4>
        Makes all samples have the same distribution by aligning quantiles.<br>
        Very effective for large datasets (> 1000 features).<br>
        Not recommended for small lipidomics datasets.<br><br>

        <i><b>Tip:</b> 1. Sample-wise normalization adjusts each sample independently, while lipid-class normalization adjusts within biological lipid categories.
             2. For lipid class that only contains one lipid, lipid class normalization will not change its value.</i>
             <br>")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  observeEvent(input$help_btn_transform, {
    showModal(modalDialog(
      title = "Help Information",
      tagList(
        HTML("
        <h4>Logit transformation</h4>
             For data in the range [0,1], logit transformation maps values to the entire real line.
             Thus, it is only suggested after lipid class sum normalization.<br>
             If there is data outside [0,1], Min-max normalization would be applied before
             logit transformation.<br>")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$help_btn_scaling, {
    showModal(modalDialog(
      title = "Help Information",
      tagList(
        HTML("<b>Scaling Methods for Lipidomics</b><br><br>
              <b>1. Mean Centering</b><br>
              Subtracts the mean of each lipid across all samples.<br>
              Formula: x' = x - mean(x)<br><br>

              <b>2. Auto-scaling (Unit Variance Scaling, z-score)</b><br>
              Subtracts the mean and divides by the standard deviation.<br>
              All lipids have variance = 1.<br>
              Formula: x' = (x - mean(x)) / sd(x)<br><br>

              <b>3. Pareto Scaling</b><br>
              Similar to auto-scaling, but divides by sqrt(sd) instead of sd.<br>
              Keeps large-variance lipids somewhat larger than small-variance ones.<br>
              Formula: x' = (x - mean(x)) / sqrt(sd(x))<br><br>

              <b>4. Range Scaling (Min–Max)</b><br>
              Rescales each lipid into the range [0,1].<br>
              Formula: x' = (x - min(x)) / (max(x) - min(x))<br><br>

              <i>Tip:</i> Auto-scaling treats all lipids equally, Pareto scaling is a compromise, and Range scaling puts values on a fixed range.")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })


  
  ## ---- Plots Menu ----
  
  ## ---- Boxplot ----
  # Reactive expression for transformed lipidomics data (used for boxplot)
  
  # Unified reactive for data and metadata
  transformedData <- eventReactive(input$Box_action, {
    req(normalized_plan_data(), updated_parsed_table())  # ensure both are ready
    
    df <- normalized_plan_data()
    lipid_meta <- updated_parsed_table()
    
    
    validate(
      need(ncol(df) > 0, "The file appears empty."),
      need(any(sapply(df, is.numeric)), "No numeric columns found.")
    )
    
    list(data = df, metadata = lipid_meta)
  })
  
  # initialize empty containers
  boxplot_lipids     <- reactiveVal(NULL)
  boxplot_samples    <- reactiveVal(NULL)
  boxplot_lipidclass <- reactiveVal(NULL)
  
  # when transformedData changes, update all three
  observeEvent(transformedData(), {
    df <- transformedData()$data
    lipid_meta <- transformedData()$metadata
    
    # Lipids
    if (ncol(df) >= 80) {
        p <- plotly::plot_ly(
          x = 0,
          y = 0,
          type = "scatter",
          mode = "markers",
          marker = list(opacity = 0)
        ) %>%
          layout(
            annotations = list(
              x = 0.5,
              y = 0.5,
              text = "We don't support boxplot<br>if your expression data has more than 80 lipids.",
              showarrow = FALSE,
              font = list(size = 15),
              xref = "paper",
              yref = "paper"
            ),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          )
      
      boxplot_lipids(p)
    } else {
      df_p <- as.data.frame(df)
      boxplot_lipids(create_boxplot(df_p, "Boxplot of Lipids"))
    }
    
    # Samples
    df_p <- as.data.frame(t(df))
    boxplot_samples(create_boxplot(df_p, "Boxplot of Samples"))
    
    # Lipid class
    boxplot_lipidclass(boxplot_lipid_class(df, lipid_meta))
  })
  
  box_plot_obj <- reactive({
    switch(input$boxplot_var,
           "boxplot for Lipids"      = boxplot_lipids(),
           "boxplot for Samples"     = boxplot_samples(),
           "boxplot for Lipid Class" = boxplot_lipidclass())
  })
  
  output$pixel_info_BP <- renderUI({
    req(input$width_BP, input$height_BP, input$DPI_BP)
    pixel_width <- round(input$width_BP * input$DPI_BP)
    pixel_height <- round(input$height_BP * input$DPI_BP)
    
    HTML(paste0(
      "<b>Output pixel dimensions:</b> ",
      pixel_width, " × ", pixel_height, " px",
      " (", round(pixel_width/pixel_height, 2), ":1 ratio)"
    ))
  })
  
  output$boxPlotUI <- renderUI({
    
    width_px  <- input$width_BP  * input$DPI_BP
    height_px <- input$height_BP * input$DPI_BP
    
    withSpinner(plotlyOutput(
      "boxPlot",
      width  = paste0(width_px, "px"),
      height = paste0(height_px, "px")
    ))
  })
    
  output$boxPlot <- plotly::renderPlotly({
    req(box_plot_obj())
    p <- box_plot_obj()
    ggplotly(p)  # Call the function+
  })
  output$download_boxplot <- downloadHandler(
    filename = function() {
      paste0("Boxplot_", gsub(" ", "_", input$boxplot_var), "_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = input$width_BP  * input$DPI_BP, height = input$height_BP  * input$DPI_BP)
      plot_fun <- box_plot_obj()

      print(plot_fun)

      dev.off()
    }
  )
  
  ## ---- Lipid PCA ----
  
  output$pixel_info_PCA <- renderUI({
    req(input$width_PCA, input$height_PCA, input$DPI_PCA)
    pixel_width <- round(input$width_PCA * input$DPI_PCA)
    pixel_height <- round(input$height_PCA * input$DPI_PCA)
    
    HTML(paste0(
      "<b>Output pixel dimensions:</b> ",
      pixel_width, " × ", pixel_height, " px",
      " (", round(pixel_width/pixel_height, 2), ":1 ratio)"
    ))
  })
  
  output$PCAPlotUI <- renderUI({
    
    width_px  <- input$width_PCA  * input$DPI_PCA
    height_px <- input$height_PCA * input$DPI_PCA
    
    withSpinner(plotlyOutput(
      "PCAPlot",
      width  = paste0(width_px, "px"),
      height = paste0(height_px, "px")
    ))
  })
  
  output$PCAPlot <- renderPlotly({
    req(input$generate_pca_plot,normalized_plan_data(), metadataWithGroup())
    df_p <- normalized_plan_data()
    group <- metadataWithGroup()[[input$define_group]]
    
    if(input$pca_3d){
      temp_plot <- create_pca_plot_3d(df_p,group)
      temp_plot
    }
    
    else{
      temp_plot <- create_pca_plot(df_p,group,
                                   width=input$width_PCA * input$DPI_PCA,
                                   height = input$height_PCA * input$DPI_PCA)
      ggplotly(temp_plot,tooltip = c("text", "x", "y"))
    }
    
  })
  
  output$download_pca_plot <- downloadHandler(
    filename = function() {
      base_name <- paste0("PCA_", Sys.Date())
      
      if (isTRUE(input$pca_3d)) {
        paste0(base_name, ".html")
      } else {
        paste0(base_name, ".png")
      }
    },
    
    content = function(file) {
      showModal(modalDialog(
        title = "Preparing download...",
        "Please wait while we prepare files. It might take a while, and thank you for your patience.",
        footer = NULL
      ))
      req(input$generate_pca_plot, normalized_plan_data(), metadataWithGroup())
      
      df_p <- normalized_plan_data()
      group <- metadataWithGroup()[[input$define_group]]
      
      if (isTRUE(input$pca_3d)) {
        # 3D plotly object
        p3d <- create_pca_plot_3d(df_p, group)
        
        htmlwidgets::saveWidget(
          widget = p3d,
          file = file,
          selfcontained = TRUE
        )
        removeModal()
        
      } else {
        # 2D ggplot object
        p2d <- create_pca_plot(df_p, group)
        
        png(
          filename = file,
          width = input$width_PCA,
          height = input$height_PCA,
          units = "in",
          res = input$DPI_PCA
        )
        
        print(p2d)
        
        dev.off()
        removeModal()
      }
    }
  )
  
  ## ---- hirechical clustering and heatmap ----
  heatmap_plot_obj <- reactiveVal(NULL)
  observeEvent(input$generate_heatmaphcl, {
    req(normalized_plan_data(), metadataWithGroup())
    p <- create_hcl_heatmaply(normalized_plan_data(), 
                          metadat = metadataWithGroup(),
                          group_var = input$define_group,
                          parse_table = updated_parsed_table(),
                          cluster_rows = input$cluster_rows,
                          cluster_cols = input$cluster_cols
                        )
    heatmap_plot_obj(p)
  })
  output$HeatmapPlot <- plotly::renderPlotly({
    req(heatmap_plot_obj())
    p <- heatmap_plot_obj()
    p  # Call the function+
  })
  
  output$download_hcl_plot <- downloadHandler(
    filename = function() {
      base_name <- paste0("hierarchical_heatmap", Sys.Date())
      paste0(base_name, ".html")
  
    },
    
    content = function(file) {
      showModal(modalDialog(
        title = "Preparing download...",
        "Please wait while we prepare files. It might take a while, and thank you for your patience.",
        footer = NULL
      ))
      req(heatmap_plot_obj())
      
      p <- heatmap_plot_obj()
      
      htmlwidgets::saveWidget(
        widget = p,
        file = file,
        selfcontained = TRUE
      )
      removeModal()
    }
        
  )
  ## ---- Plot lipid comparison ----
  
  observe({
    req(updated_parsed_table())
    updateSelectInput(session,
                      "plot_lipid_class",
                      choices = unique(updated_parsed_table()$Lipid.class))
  })
  
  observeEvent(list(input$plot_lipid_class, updated_parsed_table_w_mean()), {
    
    req(updated_parsed_table_w_mean())
    df <- updated_parsed_table_w_mean()
    
    # Only update if the input already exists
    if (!is.null(input$tt_unsat)) {
        tmp <- df[df$Lipid.class == input$plot_lipid_class, "Total.unsaturation"]
        
        if (length(tmp) == 0) {
          min_db <- NA
        } else {
          min_db <- min(tmp)
        }
        
        tmp2 <- df[df$Lipid.class == input$plot_lipid_class, "Total.unsaturation"]
        
        if (length(tmp2) == 0) {
          max_db <- NA
          
        } else {
          max_db <- max(tmp2)
        }
        
       
        updateSliderInput(session, "tt_unsat",
                          min = min_db,
                          max = max_db,
                          value = c(min_db, max_db)
        )
        
        updateSliderInput(session, "tt_unsat2",
                          min = min_db,
                          max = max_db,
                          value = c(min_db, max_db)
        )
      
    }
    
    if (!is.null(input$total_c_range)) {
      tmp_c <- df[df$Lipid.class == input$plot_lipid_class, "Total.carbon"]
      
      if (length(tmp_c) == 0) {
        min_c <-NA
      } else {
        min_c <- min(tmp_c)
      }
      
      
      tmp2_c <- df[df$Lipid.class == input$plot_lipid_class, "Total.carbon"]
      
      if (length(tmp2_c) == 0) {
        max_c <-NA
      } else {
        max_c <- max(tmp2_c)
      }
      updateSliderInput(session, "total_c_range",
                          min = min_c,
                          max = max_c,
                          value = c(min_c, max_c)
        )
      updateSliderInput(session, "total_c_range2",
                        min = min_c,
                        max = max_c,
                        value = c(min_c, max_c)
      )
    }
  })
  

  
  lipid_class_plot_obj <- eventReactive(input$comparison_action, {
    req(input$plot_lipid_class, input$define_group,
        updated_parsed_table_w_mean(), metadataWithGroup(), normalized_plan_data())
    
    plot_lipid_comparison(
      input$plot_lipid_class,
      normalized_plan_data(),
      metadataWithGroup(),
      updated_parsed_table_w_mean(),
      group_variable = input$define_group,
      plot_type = input$plot_type_var,
      stats_method = input$stats_m,
      show_jitter = input$show_points,
      double_bond_range  = if (isTRUE(input$enable_db_filter))  input$tt_unsat else NULL,
      total_carbon_range = if (isTRUE(input$enable_c_filter))   input$total_c_range else NULL,
      double_bond_range2  = if (isTRUE(input$enable_db_filter2)) input$tt_unsat2 else NULL,
      total_carbon_range2 = if (isTRUE(input$enable_c_filter2))  input$total_c_range2 else NULL
    )
  }, ignoreInit = TRUE)
  
  output$pixel_info_CP <- renderUI({
    req(input$width_CP, input$height_CP, input$DPI_CP)
    pixel_width <- round(input$width_CP * input$DPI_CP)
    pixel_height <- round(input$height_CP * input$DPI_CP)
    
    HTML(paste0(
      "<b>Output pixel dimensions:</b> ",
      pixel_width, " × ", pixel_height, " px",
      " (", round(pixel_width/pixel_height, 2), ":1 ratio)"
    ))
  })
  
  output$Lipid_class_plotUI <- renderUI({
    
    width_px  <- input$width_CP  * input$DPI_CP
    height_px <- input$height_CP * input$DPI_CP
    
    withSpinner(plotlyOutput(
      "Lipid_class_plot",
      width  = paste0(width_px, "px"),
      height = paste0(height_px, "px")
    ))
  })
  
  output$Lipid_class_plot <- plotly::renderPlotly({
    req(lipid_class_plot_obj())
    p <- lipid_class_plot_obj()
    ggplotly(p,tooltip =  "text")  # Call the function+
  })
  
  output$Lipid_class_download_plot_single <- downloadHandler(
    filename = function() {
      paste0("Comparison_of_", input$plot_lipid_class, "_between_Groups_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(lipid_class_plot_obj())
      png(file, width = input$width_CP  * input$DPI_CP, height = input$height_CP  * input$DPI_CP)
      plot_fun <- lipid_class_plot_obj()
      
      print(plot_fun)
      
      dev.off()
    }
  )
  
  all_lipid_class_plot_obj <- reactiveVal(NULL)

  observeEvent(input$comparison_action, {
    req( input$comparison_action,updated_parsed_table_w_mean(),metadataWithGroup(),normalized_plan_data(),input$define_group)
    plt_list <- list()
    classes <- unique(updated_parsed_table_w_mean()$Lipid.class)
    for (class in classes) {
      plot <- plot_lipid_comparison(class,
                                    normalized_plan_data(),
                                    metadataWithGroup(),
                                    updated_parsed_table_w_mean(),
                                    show_jitter = input$show_points,
                                    group_variable = input$define_group,
                                    plot_type=input$plot_type_var,
                                    stats_method = input$stats_m)
      plt_list[[paste(input$plot_type_var,class,sep = "_")]] <- plot
    }
    all_lipid_class_plot_obj(plt_list)
  })
  
  output$Lipid_class_download_plot <- downloadHandler(
    filename = function() {
      paste0("Comparison of lipid between Groups_",Sys.Date(), ".zip")
    },
    content = function(file) {
      showModal(modalDialog(
        title = "Preparing download...",
        "Please wait while we compress files.",footer = NULL
      ))
      req( updated_parsed_table_w_mean(),metadataWithGroup(),normalized_plan_data(),input$define_group)
      tmpdir <- tempdir()
      plot_paths <- c()

      plt_list <- list()
      classes <- unique(updated_parsed_table_w_mean()$Lipid.class)
      for (class in classes) {
        plot <- plot_lipid_comparison(class,
                                      normalized_plan_data(),
                                      metadataWithGroup(),
                                      updated_parsed_table_w_mean(),
                                      group_variable = input$define_group,
                                      plot_type=input$plot_type_var,
                                      show_jitter = input$show_points,
                                      stats_method = input$stats_m)
        plt_name <- paste(input$plot_type_var,class,sep = "_")
        path <- file.path(tmpdir, paste0(plt_name, ".png"))
        ggsave(path, plot = plot, width = input$width_CP, height = input$height_CP, dpi = input$DPI_CP)
        plot_paths <- c(plot_paths, path)
      }

      zip::zip(zipfile = file, files = plot_paths, mode = "cherry-pick")
      # remove modal when finished
      removeModal()

    }
  )
  
  
  ## ---- individual comparison ----
  observe({
    req(updated_parsed_table())
    updateSelectInput(session,
                      "selected_lipid",
                      choices = unique(updated_parsed_table()$Name))
  })
  
  lipid_indi_plot_obj <- reactive({
    req(input$single_lipid_plot_btn, input$selected_lipid, 
        input$define_group,
        updated_parsed_table_w_mean(),metadataWithGroup(),normalized_plan_data())
    p <- plot_indi_lipid_comparison(input$selected_lipid, 
                                    normalized_plan_data(), 
                                    metadataWithGroup(), 
                                    updated_parsed_table_w_mean(),
                                    group_variable = input$define_group,
                                    plot_type=input$plot_type_var2,
                                    stats_method = input$stats_m2,
                                    show_jitter = input$show_points2
    )
    return(p)
  })
  
  output$single_lipid_plotUI <- renderUI({
    
    width_px  <- input$width_IP  * input$DPI_IP
    height_px <- input$height_IP * input$DPI_IP
    
    withSpinner(plotlyOutput(
      "single_lipid_plot",
      width  = paste0(width_px, "px"),
      height = paste0(height_px, "px")
    ))
  })
  output$single_lipid_plot <- renderPlotly({
    req(lipid_indi_plot_obj())
    ggplotly(lipid_indi_plot_obj(),tooltip = 'text')
  })
  

  output$single_lipid_plot_download <- downloadHandler(
    filename = function() {
      paste0("Comparison_of_", input$selected_lipid, "_between_Groups_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(lipid_indi_plot_obj())
      png(file, width = input$width_IP  * input$DPI_IP, height = input$height_IP  * input$DPI_IP)
      plot_fun <- lipid_indi_plot_obj()
      
      print(plot_fun)
      
      dev.off()
    }
  )
  
  ## ---- Lipid Class Heatmap Plot ----

  observeEvent(updated_parsed_table_w_mean(), {
    updateSelectInput(session,
                      "LC_selection",
                      choices = unique(updated_parsed_table_w_mean()$Lipid.class)
                      )
  })
  
  output$pixel_info_LCH <- renderUI({
    req(input$width_LCH, input$height_LCH, input$DPI_LCH)
    pixel_width <- round(input$width_LCH * input$DPI_LCH)
    pixel_height <- round(input$height_LCH * input$DPI_LCH)
    
    HTML(paste0(
      "<b>Output pixel dimensions:</b> ",
      pixel_width, " × ", pixel_height, " px",
      " (", round(pixel_width/pixel_height, 2), ":1 ratio)"
    ))
  })
  
  all_plots <- reactiveVal()
  
  # Define a reactive value to keep track of current plot index
  current_plot_index <- reactiveVal(1)
  
  # When generate plots, reset the index
  observeEvent(input$generate_LCH_plots, {
    req( updated_parsed_table_w_mean())
    color_scheme <- switch(input$color_code_LCH,
                           "RWB"   = list(low = "blue", mid = "white", high = "red", na = "grey90"),
                           "BGY"       = list(low = "#08306B", mid = "#41B6C4", high = "#FFFFB2", na = "grey90"),
                           "OWB"       = list(low = "#E66101", mid = "white", high = "#0C2C84", na = "grey90"),
                           "greyscale" = list(low = "grey90", mid = "grey70", high = "black", na = "grey95"),
                           "viridis"   = list(low = viridis::viridis(3)[1], mid = viridis::viridis(3)[2], high = viridis::viridis(3)[3], na = "grey90"),
                           "heat"      = list(low = "yellow", mid = "orange", high = "red", na = "grey90")
    )
    plt_list <- list()
    groups <- colnames(updated_parsed_table_w_mean())[(ncol(updated_parsed_table_w_mean())-ngroup()+1):ncol(updated_parsed_table_w_mean())]
    for (group in groups){
      safe_g <- gsub("[^A-Za-z0-9_\\-]+", "_", group)
      plt_list[[safe_g]] <- create_LCHplot_single(
      label_text = input$label_text_checkbox,
      stub = input$LC_selection,
      group_selection = group,
      lipid_df_samples = updated_parsed_table_w_mean(),
      range_min = input$heatmap_min, range_max = input$heatmap_max,
      font_size = input$label_text_size,
      title_size = input$label_title_size,
      text_x_size = input$label_x_size,
      text_y_size = input$label_y_size,
      title_x_size = input$title_x_size,
      title_y_size = input$title_y_size,
      sequence = input$sequence_heatmap,
      legend_title_size = input$legend_title_size,
      legend_text_size = input$legend_text_size,
      color_scheme = color_scheme
    )
    }
    all_plots(plt_list)
    current_plot_index(1)  # reset to first plot
  })
  
  output$plot_ui <- renderUI({
    req(all_plots())
    plot_names <- names(all_plots())
    
    # Split into groups of 2
    plot_groups <- split(plot_names, ceiling(seq_along(plot_names) / 2))

    req(input$width_LCH, input$height_LCH, input$DPI_LCH)
    pixel_width <- input$width_LCH * input$DPI_LCH
    pixel_height <- input$height_LCH * input$DPI_LCH
    
    tagList(
      lapply(plot_groups, function(group) {
        fluidRow(
          lapply(group, function(name) {
            column(
              width = 6,
              div(
                style = paste0(
                  "width:", pixel_width, "px; height:", pixel_height,
                  "px; overflow:hidden; margin: 0 auto;"
                ),
                withSpinner(plotOutput(outputId = name, height = paste0(pixel_height, "px"), width = paste0(pixel_width, "px")))
              ),
              br(),
              div(style = "text-align: center;font-size: 16px;", strong(name)),
              br()
            )
          })
        )
      })
    )
  })
  
  # Render each plot (same as before)
  observe({
    req(all_plots())
    lapply(names(all_plots()), function(name) {
      local({
        plotname <- name
        output[[plotname]] <- renderPlot({
          all_plots()[[plotname]]
        }, res = input$DPI_LCH)
      })
    })
  })
  
  output$download_selection <- downloadHandler(
    filename = function() {
      paste0("differential_mean_lipid_heatmaps_selection_",Sys.Date(), ".zip")
    },
    content = function(file) {
      showModal(modalDialog(
        title = "Preparing download...",
        "Please wait while we compress files.",footer = NULL
      ))
      req( all_plots())
      tmpdir <- tempdir()
      plot_paths <- c()
      for (name in names(all_plots())) {
        plot <- all_plots()[[name]]
        path <- file.path(tmpdir, paste0("Differential Mean Lipid Heatmap_", name, ".png"))
        ggsave(path, plot = plot, width = input$width_LCH, height = input$height_LCH, dpi = input$DPI_LCH)
        plot_paths <- c(plot_paths, path)
      }
      
      zip::zip(zipfile = file, files = plot_paths, mode = "cherry-pick")
      removeModal()
    },
    contentType = "application/zip"
  )

  
  output$download_zip <- downloadHandler(
    filename = function() {
      paste0("differential_mean_lipid_heatmaps_",Sys.Date(), ".zip")
    },
    content = function(file) {
      showModal(modalDialog(
        title = "Preparing download...",
        "Please wait patiently while we compress files. 
        It may take a while to download all the plots for the differential mean lipid heatmap.",
        footer = NULL
      ))
      req( updated_parsed_table_w_mean())
      tmpdir <- tempdir()
      heatmap_dir <- file.path(tmpdir, "differential_mean_lipid_heatmaps")
      plot_paths <- save_LCH_heatmaps_to_dir(
        out_dir   = heatmap_dir,
        parsed_tbl = updated_parsed_table_w_mean(),
        ngroup    = ngroup(),
        input     = input
      )
      
      zip::zip(zipfile = file, files = plot_paths, mode = "cherry-pick")
      removeModal()
    },
    contentType = "application/zip"
  )
  
  
  ## ---- Volcano Plot ----
  observeEvent(metadataWithGroup(), {
    req(metadataWithGroup())
    updateSelectInput(session, "vol_var1",
                      choices =unique(metadataWithGroup()[[input$define_group]]),
                      selected = unique(metadataWithGroup()[[input$define_group]])[1])
    updateSelectInput(session, "vol_var2",
                      choices = unique(metadataWithGroup()[[input$define_group]]),
                      selected = unique(metadataWithGroup()[[input$define_group]])[2])
  })
  volcano_df <- reactiveVal(NULL)
  volcano_plot_obj <- reactive({
    req(input$vol_var1, input$vol_var2, updated_parsed_table_w_mean(), input$generate_volcano_plot)
    lipid_df_samples <- updated_parsed_table_w_mean()
    
    var1 <- input$vol_var1
    var2 <- input$vol_var2
    
    # here, the t test is welch t test in default
    t_test_result <- t_test_lipids(split_data()[[var1]], 
                                   split_data()[[var2]],
                                   variance_p = F)
    
    # fetch the fold change
    var1_meta <- paste0("Mean_group_",var1)
    var2_meta <- paste0("Mean_group_",var2)
    required_cols <- c("Name", var1_meta, var2_meta)
    if (!all(required_cols %in% colnames(lipid_df_samples))) {
      stop("Missing required columns in lipid_df_samples: ", paste(setdiff(required_cols, colnames(lipid_df_samples)), collapse = ", "))
    }
    lipid_meta <- lipid_df_samples[, required_cols]
    lipid_meta$Mean_difference <- lipid_meta[[var2_meta]] - lipid_meta[[var1_meta]]
    
    
    lipid_meta <- merge(lipid_meta, t_test_result, by= "Name", all.x = TRUE)
    
    title <- paste("Volcano Plot for", var2, "vs", var1)
    volcano_df(lipid_meta[order(lipid_meta$Adjusted_P_value), ])  # Sort by adjusted p-value
    # Create a volcano plot
    plot <- plot_volcano (lipid_meta,
                          title,
                          alpha = input$p_value_threshold ,
                          fold_threshold = input$fc_threshold ,adj = input$Adj_y)
    
    return(plot)
  })
  output$volcano_plotUI <- renderUI({
    
    width_px  <- input$width_volcano  * input$DPI_volcano
    height_px <- input$height_volcano * input$DPI_volcano
    
    withSpinner(plotlyOutput(
      "volcanoPlot",
      width  = paste0(width_px, "px"),
      height = paste0(height_px, "px")
    ))
  })
  
  output$volcanoPlot <- plotly::renderPlotly({
    req(volcano_plot_obj())  # if needed
    ggplotly(volcano_plot_obj(), tooltip = "text") 
  })
  
  output$download_volcano_plot <- downloadHandler(
    filename = function() {
      paste0("Volcano_Plot_", input$vol_var1, "_vs_", input$vol_var2, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, 
          width = input$width_volcano  * input$DPI_volcano, 
          height = input$height_volcano * input$DPI_volcano)
      print(volcano_plot_obj())
      dev.off()
    }
  )
  
  output$volcanoDataPreview <- DT::renderDataTable({
    req(volcano_df())
    datatable(volcano_df(), options = list(pageLength = 10), rownames = TRUE)  %>%
      formatRound(columns = which(sapply(volcano_df(), is.numeric)), digits = 4)
  })
  
  output$download_volcano_data <- downloadHandler(
    filename = function() {
      paste0("Ttest_w_Mean_difference_Data_", input$vol_var1, "_vs_", input$vol_var2, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(volcano_df())
      write.csv(volcano_df(), file, row.names = TRUE)
    }
  )
  ## ---- Mean Calculation ----
  updated_parsed_table_w_mean <- reactiveVal(NULL)
  updated_parsed_table_w_mean_std <- reactiveVal(NULL)
  split_data <- reactiveVal(NULL)
  
  observeEvent({
    list(normalized_plan_data(), updated_parsed_table(), metadataWithGroup(), input$define_group)
  }, {
    req(normalized_plan_data(), updated_parsed_table(),metadataWithGroup(),input$define_group)
    split_d <- split_group_labels(normalized_plan_data(),
                                  metadataWithGroup(), 
                                  group_col = input$define_group)
    split_data(split_d)
    dat <- calculate_group_means(split_d, 
                                 updated_parsed_table())
    dat2 <- calculate_group_means_sdv(split_d, 
                                   updated_parsed_table())
    updated_parsed_table_w_mean(dat)
    updated_parsed_table_w_mean_std(dat2)
    })
  
  output$download_mean_calculated <- downloadHandler(
    filename = function() {
      paste0("mean_calculated_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(updated_parsed_table_w_mean_std())
      write.csv(updated_parsed_table_w_mean_std(), file, row.names = TRUE)
    }
  )
  
  output$Mean_Calculated_DataPreview <- DT::renderDataTable({
    req(updated_parsed_table_w_mean_std(),input$run_mean_cal)
    showNotification("Mean calculation completed.", type = "message")
    datatable(updated_parsed_table_w_mean_std(), options = list(pageLength = 10), rownames = TRUE)  %>%
      formatRound(columns = which(sapply(updated_parsed_table_w_mean_std(), is.numeric)), digits = 4)
  })
  
  ## ---- T-test ----
  observeEvent(metadataWithGroup(), {
    req(metadataWithGroup())
    updateSelectInput(session, "ttest_var1",
                      choices =unique(metadataWithGroup()[[input$define_group]]),
                      selected = unique(metadataWithGroup()[[input$define_group]])[1])
    updateSelectInput(session, "ttest_var2",
                      choices = unique(metadataWithGroup()[[input$define_group]]),
                      selected = unique(metadataWithGroup()[[input$define_group]])[2])
  })
  
  ttest_results <- reactiveVal(NULL)
  
  observeEvent(input$run_ttest, {
    req(split_data(), input$ttest_var1, input$ttest_var2)
    
    var1 <- input$ttest_var1
    var2 <- input$ttest_var2
    
    if (var1 == var2) {
      showNotification("Please select two different groups for the t-test.", type = "error")
      return(NULL)
    }
    
    if (input$ttest_method == "student"){variance_p = T}
    else {variance_p = F}
    
    results <- t_test_lipids(split_data()[[var1]], split_data()[[var2]], variance_p)

    results <- append_mean_dataframe(results, updated_parsed_table_w_mean())
    ttest_results(results)
    
    showNotification("T-test completed.", type = "message")
  })
  
  output$Ttest_Results_Preview <- DT::renderDataTable({
    req(ttest_results())
    datatable(ttest_results(), options = list(pageLength = 10), rownames = TRUE)  %>%
      formatRound(columns = which(sapply(ttest_results(), is.numeric)), digits = 4)
  })
  
  output$download_ttest_results <- downloadHandler(
    filename = function() {
      paste0("ttest_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(ttest_results())
      write.csv(ttest_results(), file, row.names = TRUE)
    }
  )
  ## ---- T-test PCA Plot ----
  # Store PCA plot object
  pca_plot_obj_ttest <- reactiveVal(NULL)
  
  observeEvent(input$ttest_pca_checkbox, {
    req(ttest_results(),normalized_plan_data(), metadataWithGroup(),input$define_group)
    if (input$ttest_pca_checkbox>0) {
      # Run PCA on the ttest results
      if(input$ttest_pca_pval == 'pnominal'){
        significant_features <- ttest_results()[!is.na(ttest_results()$P_value) &
                                                  ttest_results()$P_value < 0.05, ]
      }
      else if (input$ttest_pca_pval == 'padj'){
        significant_features <- ttest_results()[!is.na(ttest_results()$Adjusted_P_value) &
                                                  ttest_results()$Adjusted_P_value < 0.05, ]
      }
      # only keep significant features in the normalized_plan_data
      if (nrow(significant_features) <=2) {
        showNotification("Too few significant features (<=2) for PCA.", type = "error")
        pca_plot_obj_ttest(NULL)
        return(NULL)
      }
      significant_features <- significant_features$Name
      # Ensure the subset remains a matrix even with one feature``
      zscore_data <- normalized_plan_data()[,significant_features, drop = FALSE]
      group <- metadataWithGroup()[[input$define_group]]
      temp_plot <- create_pca_plot(zscore_data,group)
      
      pca_plot_obj_ttest(temp_plot)
    } else {
      pca_plot_obj_ttest(NULL)
    }  
  })
  
  # Render the PCA plot
  output$ttestPCAPlot <- renderPlotly({
    req(pca_plot_obj_ttest())
    ggplotly(pca_plot_obj_ttest(),tooltip = c("text", "x", "y"))
  })
  
  # Download handler
  output$download_ttest_PCA_plot <- downloadHandler(
    filename = function() {
      paste0("PCA_plot_significant_features_on_normalized_data", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = pca_plot_obj_ttest(), width = 8, height = 6, dpi = 300)
    }
  )
  
  
  ## ---- Anova ----
  # ReactiveVal to hold ANOVA results
  anova_results <- reactiveVal(NULL)
  
  observeEvent(input$run_anova, {
    req(normalized_plan_data(), metadataWithGroup())
    
    expr_data <- normalized_plan_data()
    metadata <- metadataWithGroup()
    group_col <- metadata[[input$define_group]]
    # validate that group_col has at least 3 levels
    if (length(unique(group_col)) < 3) {
      showNotification("Please select a group with at least 3 levels for ANOVA.", type = "error")
      return(NULL)
    }
    
    expr_df <- as.data.frame(expr_data)
    
    result_df <- run_oneway_anova(expr_df, group_col)
    append_mean_dataframe_df <- append_mean_dataframe(result_df,
                                                      updated_parsed_table_w_mean())
    anova_results(append_mean_dataframe_df)
    showNotification("ANOVA completed successfully!", type = "message")
  })
  # Show a preview table
  output$ANOVA_Results_Preview <- DT::renderDataTable({
    req(anova_results())
    datatable(anova_results(), options = list(pageLength = 10), rownames = TRUE)  %>%
      formatRound(columns = which(sapply(anova_results(), is.numeric)), digits = 4)
  })
  
  # Download handler
  output$download_anova_results <- downloadHandler(
    filename = function() {
      paste0("anova_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(anova_results())
      write.csv(anova_results(), file, row.names = FALSE)
    }
  )
  ## ---- Anova PCA Plot ----
  # Store PCA plot object
  pca_plot_obj2 <- reactiveVal(NULL)
  
  observeEvent(input$anova_pca_checkbox, {
    req(anova_results(),normalized_plan_data(), metadataWithGroup(),input$define_group)
    if (isTRUE(input$anova_pca_checkbox)) {
      # Run PCA on the ANOVA results
      if (input$anova_pca_pval == 'padj'){
        significant_features <- anova_results()[!is.na(anova_results()$Adjusted_P_value) &
                                                  anova_results()$Adjusted_P_value < 0.05, ]$Name
      }
      else if (input$anova_pca_pval == 'pnominal'){
        significant_features <- anova_results()[!is.na(anova_results()$P_value) &
                                                  anova_results()$P_value < 0.05, ]$Name
      }
      # only keep significant features in the normalized_plan_data
      if (length(significant_features) == 0) {
        showNotification("No significant features found for PCA.", type = "warning")
        pca_plot_obj2(NULL)
        return(NULL)
      }
      zscore_data <- normalized_plan_data()[, significant_features, drop = FALSE]
      group <- metadataWithGroup()[[input$define_group]]
      temp_plot <- create_pca_plot(zscore_data,group)
      
      pca_plot_obj2(temp_plot)
    } else {
      pca_plot_obj2(NULL)
    }  
  })
  
  # Render the PCA plot
  output$anovaPCAPlot <- renderPlotly({
    req(pca_plot_obj2())
    ggplotly(pca_plot_obj2(),tooltip = c("text", "x", "y"))
  })
  
  # Download handler
  output$download_anova_PCA_plot <- downloadHandler(
    filename = function() {
      paste0("PCA_plot_significant_features_on_normalized_data", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = pca_plot_obj2(), width = 8, height = 6, dpi = 300)
    }
  )
  
  ## ---- Correlation ----
  corr_results_list <- reactiveVal(list())
  corr_plot_list <- reactiveVal(list())
  
  observeEvent(input$run_corr, {
    req(split_data(), metadataWithGroup(), input$define_group)
    
    groups <- unique(metadataWithGroup()[[input$define_group]])
    res_list <- list()
    plot_list <- list()
    
    for (g in groups) {
      df <- split_data()[[g]]
      if (isTRUE(input$control_thres)){
        result <- correlation_cal_adj(
          df,
          input$control_z,
          input$correlation_thres[1],
          input$correlation_thres[2]
        )}
      else{
        result <- correlation_cal_adj(
          df,
          input$control_z
        )
      }
      result <- as.data.frame(result)
      res_list[[g]] <- result
      
      # 对应的 plotly heatmap
      p <- correlation_heatmap_plotly(result, input$color_setting,g)
      plot_list[[g]] <- p
    }
    
    corr_results_list(res_list)
    corr_plot_list(plot_list)
  })
  
  ##  Output for plots
  output$corr_heatmaps <- renderUI({
    req(corr_plot_list())
    plot_list <- corr_plot_list()
    
    # Dynamically generate plotlyOutput for each group
    tagList(
      lapply(names(plot_list), function(g) {
        withSpinner(plotlyOutput(outputId = paste0("plot_", g),height = "600px", width = "800px"))
      })
    )
  })
  
  # assign renderPlotly for each group
  observe({
    req(corr_plot_list())
    plot_list <- corr_plot_list()
    lapply(names(plot_list), function(g) {
      local({
        group_name <- g
        output[[paste0("plot_", group_name)]] <- plotly::renderPlotly({
          corr_plot_list()[[group_name]]
        })
      })
    })
  })
  
  
  output$download_corr_heatmap <- downloadHandler(
    filename = function() {
      paste0("Correlation_Heatmaps_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(corr_plot_list())
      plot_list <- corr_plot_list()
      showModal(modalDialog(
        title = "Preparing download...",
        "Please wait while we compress files.",footer = NULL
      ))
      tmpdir <- tempdir()
      file_list <- c()
      for (g in names(plot_list)) {
        safe_g <- gsub("[^A-Za-z0-9_\\-]+", "_", g)
        fpath <- file.path(tmpdir, paste0("Corr_results_", safe_g, ".html"))
        htmlwidgets::saveWidget(plot_list[[g]], fpath, selfcontained = TRUE)
        file_list <- c(file_list, fpath)
      }
      zip::zipr(zipfile = file, files = file_list, root = tmpdir)
      removeModal()
    },
    contentType = "application/zip"
  )
  
  output$download_corr_results <- downloadHandler(
    filename = function() {
      paste0("Correlation_results_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(corr_results_list())
      showModal(modalDialog(
        title = "Preparing download...",
        "Please wait while we compress files.",footer = NULL
      ))
      tmpdir <- tempdir()
      file_list <- c()
      for (g in names(corr_results_list())) {
        safe_g <- gsub("[^A-Za-z0-9_\\-]+", "_", g)
        fpath <- file.path(tmpdir, paste0("Corr_results_", safe_g, ".csv"))
        write.csv(cor_mat_to_long(as.matrix(corr_results_list()[[g]])), fpath, row.names = TRUE)
        file_list <- c(file_list, fpath)
      }
      zip::zipr(zipfile = file, files = file_list, root = tmpdir)
      removeModal()
    },
    contentType = "application/zip"
  )
  
  ## ---- DSPC ----
  dspc_results <- reactiveVal(NULL)  # store all results
  selected_subnet <- reactiveVal(NULL) 
  observeEvent(input$help_btn_dspc, {
    showModal(
      modalDialog(
        title = "DSPC Network Analysis Help",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        
        tags$h4("How network clusters are generated"),
        p("For each group, DSPC computes a partial correlation network where edges 
           represent direct molecular interactions after removing indirect effects."),
        p("We then apply ", strong("Leiden clustering"), 
          " to divide the network into subnetworks (clusters) that contain highly 
           interconnected lipids."),
        
        tags$h4("Controlling cluster size"),
        tags$ul(
          tags$li(strong("Maximum nodes per subnetwork:"), 
                  " limits cluster size to maintain interpretability."),
          tags$li(strong("Minimum nodes per subnetwork:"), 
                  " removes very small and uninformative clusters.")
        ),
        p("These settings help focus on meaningful lipid modules instead of a single 
           large, dense network."),
        
        tags$h4("Interpreting the network visualization"),
        tags$ul(
          tags$li("Edge ", strong("width"), " reflects the strength of the partial correlation 
                 (stronger correlations → thicker edges)."),
          tags$li(span(style = "color:#E75480; font-weight:bold;", "Pink edges"),
                  " indicate positive partial correlations."),
          tags$li(span(style = "color:#3FA9F5; font-weight:bold;", "Blue edges"),
                  " indicate negative partial correlations.")
        ),
        
        tags$h4("How to interact with the network"),
        tags$ul(
          tags$li("Click a cluster name to display its network."),
          tags$li("Drag individual nodes to refine the layout."),
          tags$li("Use the highlight feature to emphasize a selected lipid and its neighbors.")
        )
      )
    )
  })
  
  observeEvent(input$run_dspc, {
    req(split_data(), metadataWithGroup(), input$define_group)
    if (any(is.na(normalized_plan_data()))) {
      showNotification("NA values found in normalized data. Please check your data. NA not permitted.", type = "error")
      return(NULL)
    }
    showNotification("Please wait patiently before the DSPC result is ready...",
                     type = "message",duration = 15)
    groups <- unique(metadataWithGroup()[[input$define_group]])
    results <- list()
    n_groups <- length(groups)
    
    withProgress(message = "Running DSPC analysis...", value = 0, {
      
      results <- list()
      
      for (g in groups) {
        
        incProgress(0.4/n_groups, detail = paste("Processing group:", g))
        
        df <- split_data()[[g]]
        dspc_df <- DSPC(df)
        incProgress(0.6/n_groups, detail = paste("Processing group:", g))
        res <- get_subnetworks(
          long_df          = dspc_df,
          max_cluster_size = input$max_cluster_size,
          min_nodes        = input$min_nodes
        )
        
        results[[g]] <- list(
          dspc_table  = dspc_df,
          subnetworks = res$subnetworks,
          summary     = res$summary
        )
        dspc_results(results)
      }
    })
    
    
    showNotification("DSPC subnetworks ready!", type = "message", duration = 10)
  })
  
  output$download_dspc <- downloadHandler(
    filename = function() {
      paste0("DSPC_results_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(dspc_results())
      showModal(modalDialog(
        title = "Preparing download...",
        "Please wait while we compress files.",footer = NULL
      ))
      results <- dspc_results()
      
      tmpdir <- tempdir()
      files <- c()
      
      for (g in names(results)) {
        dspc_df <- results[[g]]$dspc_table
        
        outfile <- file.path(tmpdir, paste0("DSPC_", g, ".csv"))
        write.csv(dspc_df, outfile, row.names = FALSE)
        files <- c(files, outfile)
      }
      
      zip::zipr(zipfile = file, files = files)
      removeModal()
    },
    contentType = "application/zip"
  )
  # === Dynamic UI list of all groups and subnetworks ===
  output$subnetwork_list <- renderUI({
    req(dspc_results())
    
    results <- dspc_results()
    
    tagList(
      lapply(names(results), function(g) {
        
        # group title
        list_block <- tags$div(
          tags$h4(style="margin-top:12px;font-weight:bold;", glue::glue("Group: {g}"))
        )
        
        subs <- names(results[[g]]$subnetworks)
        
        if (length(subs) == 0) {
          list_block <- tagList(list_block, tags$i("No subnetworks (after filtering)"))
        } else {
          sn_buttons <- lapply(subs, function(sn) {
            # get node count
            n_nodes <- nrow(results[[g]]$subnetworks[[sn]]$nodes)
            
            actionLink(
              inputId = paste0( g, "_", sn),
              label   = paste0("• ", sn, "  (", n_nodes, " nodes)"),
              style   = "display:block; margin-left:10px; font-size:15px;"
            )
          })
          
          list_block <- tagList(list_block, sn_buttons)
        }
        
        return(list_block)
      })
    )
  })
  
  
  # === Click listener for any subnetwork ===
  title_dspc <- reactiveVal(NULL)
  observe({
    req(dspc_results())
    results <- dspc_results()
    
    for (g in names(results)) {
      for (sn in names(results[[g]]$subnetworks)) {
        
        btn_id <- paste0(g, "_", sn)
        
        local({
          group_val <- g
          subnet_val <- sn
          id_val <- btn_id
          
          observeEvent(input[[id_val]], {
            selected_subnet(results[[group_val]]$subnetworks[[subnet_val]])
            title_dspc(paste0("Group ",group_val," - ",subnet_val))
          }, ignoreInit = TRUE)
          
        })
      }
    }
  })
  
  
  
  # === Render selected network ===
  output$DSPCPlot <- visNetwork::renderVisNetwork({
    req(selected_subnet(),title_dspc())
    
    title <- title_dspc()
    plot_subnetwork(selected_subnet(),title)
  })
  ## ---- PLS-DA ----
  plsda_results <- reactiveVal(NULL)
  observeEvent(input$run_plsda, {
    req(normalized_plan_data(), metadataWithGroup(), input$define_group)
    showNotification("Please wait patiently before PLS-DA analysis is ready...", type = "message",duration = 10)
    group_col <- metadataWithGroup()[[input$define_group]]
    # check the na values
    if (any(is.na(normalized_plan_data()))) {
      showNotification("NA values found in normalized data. Please check your data. NA not permitted.", type = "error")
      return(NULL)
      
    }
    result <- run_pls_model(
      X = as.matrix(normalized_plan_data()),
      Y = group_col,
      n_perm = input$plsda_n_perm,
      n_cv = input$plsda_n_cv
    )
    if (is.null(result)) {
      showNotification("PLS-DA failed. Please check input data.", type = "error")
      return()
    }
    plsda_results(result)
    showNotification("PLS-DA analysis completed.", type = "message")
  })
  output$PLSDA_ModelSummary <- DT::renderDataTable({
    req(plsda_results())
    
    model <- plsda_results()
    summary_df <- model@summaryDF
    
    df <- data.frame(
      Metric = colnames(summary_df),
      Value  = round(as.numeric(summary_df[1, ]), 3),
      row.names = NULL
    )
    
    DT::datatable(
      df,
      rownames = FALSE,
      options = list(
        dom = "t",
        pageLength = nrow(df)
      )
    )
  })
  
  
  # 1. Overview Plot (base R)
  output$PLSDA_overviewPlot <- renderPlot({
    req(plsda_results())
    plot(plsda_results(), typeVc = "overview")
  })
  
  output$download_plsda_overviewplot <- downloadHandler(
    filename = function() {
      paste0("PLSDA_overview_",Sys.Date(),".png")
    },
    content = function(file) {
      png(file, width = 1800, height = 1600, res = 300)
      plot(plsda_results(), typeVc = "overview")
      dev.off()
    }
  )
  
  
  # 2. Permutation Plot (base R)
  
  output$PLSDA_permutationPlot <- renderPlot({
    req(plsda_results())
    plot(plsda_results(), typeVc = "permutation")
  })
  
  output$download_plsda_permutationplot <- downloadHandler(
    filename = function() {
      paste0("PLSDA_permutation_",Sys.Date(),".png")
      },
    content = function(file) {
      png(file, width = 1800, height = 1600, res = 300)
      plot(plsda_results(), typeVc = "permutation")
      dev.off()
    }
  )
  
  
  # 5. VIP Plot (ggplotly)
  
  output$PLSDA_VIPPlot <- renderPlotly({
    req(plsda_results())
    vip_plot <- plot_opls_vip(plsda_results(), 
                              color_scheme = input$plsda_color_scheme,
                              title ="PLS-DA VIP Plot" )
    ggplotly(vip_plot, tooltip = "text")
  })
  
  output$download_plsda_vipplot <- downloadHandler(
    filename = function() {
      paste0("PLSDA_VIPPlot_",Sys.Date(),".png")
    },
    content = function(file) {
      png(file, width = 1800, height = 1600, res = 300)
      print(plot_opls_vip(plsda_results(), 
                          title ="PLS-DA VIP Plot",
                          color_scheme = input$plsda_color_scheme))
      dev.off()
    }
  )
  
  
  # 6. VIP Table
  output$PLSDA_VIPTable <- DT::renderDataTable({
    req(plsda_results())
    model <- plsda_results()
    
    # VIP scores
    vip <- getVipVn(model)
    
    # p1 loadings (predictive component 1)
    loadings <- model@loadingMN
    
    if (!"p1" %in% colnames(loadings)) {
      showNotification(
        "p1 loading not found in PLS-DA model.",
        type = "warning"
      )
      return(NULL)
    }
    
    p1 <- loadings[names(vip), "p1"]
    
    df <- data.frame(
      Lipid      = names(vip),
      VIP        = as.numeric(vip),
      p1_loading = as.numeric(p1),
      row.names  = NULL
    )
    
    # Order by VIP (primary criterion)
    df <- df[order(-df$VIP), ]
    
    df
  },
  options = list(
    pageLength = 10,
    autoWidth = TRUE
  ))
  
  
  
  output$download_plsda_viptable <- downloadHandler(
    filename = function()  {
      paste0("PLSDA_VIP_Table_",Sys.Date(),".csv.")
    },
    content = function(file) {
      vip <- getVipVn(plsda_results())
      df <- data.frame(Lipid = names(vip), VIP = vip, row.names = NULL)
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  ## ---- OPLS-DA ----
  oplsda_results <- reactiveVal(NULL)
  observeEvent(input$run_oplsda, {
    req(normalized_plan_data(), metadataWithGroup(), input$define_group)
    if (any(is.na(normalized_plan_data()))) {
      showNotification("NA values found in normalized data. Please check your data. NA not permitted.", type = "error")
      return(NULL)
      
    }
    showNotification("Please wait patiently before OPLS-DA analysis is ready...", type = "message",duration = 10)
    group_col <- metadataWithGroup()[[input$define_group]]
    
    result <- run_opls_model(
      X = as.matrix(normalized_plan_data()),
      Y = group_col,
      n_perm = input$oplsda_n_perm,
      n_cv = input$oplsda_n_cv
    )
    if (is.null(result)) {
      showNotification("OPLS-DA failed. Please check input data.", type = "error")
      return()
    }
    oplsda_results(result)
    showNotification("OPLS-DA analysis completed.", type = "message")
  })
  output$OPLSDA_ModelSummary <- DT::renderDataTable({
    req(oplsda_results())
    
    model <- oplsda_results()
    summary_df <- model@summaryDF
    
    df <- data.frame(
      Metric = colnames(summary_df),
      Value  = round(as.numeric(summary_df[1, ]), 3),
      row.names = NULL
    )
    
    DT::datatable(
      df,
      rownames = FALSE,
      options = list(
        dom = "t",
        pageLength = nrow(df)
      )
    )
  })
  
  
  # 1. Overview Plot (base R)
  output$OPLSDA_overviewPlot <- renderPlot({
    req(oplsda_results())
    plot(oplsda_results(), typeVc = "overview")
  })
  
  output$download_oplsda_overviewplot <- downloadHandler(
    filename = function() {
      paste0("OPLSDA_overview_",Sys.Date(),".png")
    },
    content = function(file) {
      png(file, width = 1800, height = 1600, res = 300)
      plot(oplsda_results(), typeVc = "overview")
      dev.off()
    }
  )
  

  # 2. Permutation Plot (base R)

  output$OPLSDA_permutationPlot <- renderPlot({
    req(oplsda_results())
    plot(oplsda_results(), typeVc = "permutation")
  })
  
  output$download_oplsda_permutationplot <- downloadHandler(
    filename = function() {
      paste0("OPLSDA_permutation_",Sys.Date(),".png")
    },
    content = function(file) {
      png(file, width = 1800, height = 1600, res = 300)
      plot(oplsda_results(), typeVc = "permutation")
      dev.off()
    }
  )
  

  # 3. Score Plot (base R)

  output$OPLSDA_ScorePlot <- renderPlot({
    req(oplsda_results())
    plot(oplsda_results(), typeVc = "x-score")
  })
  
  output$download_oplsda_scoreplot <- downloadHandler(
    filename = function() {
      paste0("OPLSDA_score_",Sys.Date(),".png")
    },
    content = function(file) {
      png(file, width = 1800, height = 1600, res = 300)
      plot(oplsda_results(), typeVc = "x-score")
      dev.off()
    }
  )
  
  # 4. Outlier Plot (base R)

  output$OPLSDA_outlierPlot <- renderPlot({
    req(oplsda_results())
    plot(oplsda_results(), typeVc = "outlier")
  })
  
  output$download_oplsda_outlierplot <- downloadHandler(
    filename = function() {
      paste0("OPLSDA_outlier_",Sys.Date(),".png")
    },
    content = function(file) {
      png(file, width = 1800, height = 1600, res = 300)
      plot(oplsda_results(), typeVc = "outlier")
      dev.off()
    }
  )
  

  # 5. VIP Plot (ggplotly)

  output$OPLSDA_VIPPlot <- renderPlotly({
    req(oplsda_results())
    vip_plot <- plot_opls_vip(oplsda_results(), color_scheme = input$oplsda_color_scheme)
    ggplotly(vip_plot, tooltip = "text")
  })
  
  output$download_oplsda_vipplot <- downloadHandler(
    filename = function(){
      paste0("OPLSDA_VIPPlot_",Sys.Date(),".png")
    }, 
    content = function(file) {
      png(file, width = 1800, height = 1600, res = 300)
      print(plot_opls_vip(oplsda_results(), color_scheme = input$oplsda_color_scheme))
      dev.off()
    }
  )
  
  
  # 6. VIP Table
  output$OPLSDA_VIPTable <- DT::renderDataTable({
    req(oplsda_results())
    model <- oplsda_results()
    
    # VIP scores
    vip <- getVipVn(model)
    
    # p1 loadings (predictive component 1)
    loadings <- model@loadingMN
    
    if (!"p1" %in% colnames(loadings)) {
      showNotification(
        "p1 loading not found in OPLS-DA model.",
        type = "warning"
      )
      return(NULL)
    }
    
    p1 <- loadings[names(vip), "p1"]
    
    df <- data.frame(
      Lipid      = names(vip),
      VIP        = as.numeric(vip),
      p1_loading = as.numeric(p1),
      row.names  = NULL
    )
    
    # Order by VIP (primary criterion)
    df <- df[order(-df$VIP), ]
    
    df
  },
  options = list(
    pageLength = 10,
    autoWidth = TRUE
  ))
  

  
  output$download_oplsda_viptable <- downloadHandler(
    filename = function() {
      paste0("OPLSDA_VIP_Table",Sys.Date(),".csv")
    },
    content = function(file) {
      vip <- getVipVn(oplsda_results())
      df <- data.frame(Lipid = names(vip), VIP = vip, row.names = NULL)
      write.csv(df, file, row.names = FALSE)
    }
  )
  ## ---- Random Forest ----
  rf_results <- reactiveVal(NULL)
  observeEvent(input$run_rf, {
    req(normalized_plan_data(), metadataWithGroup(), input$define_group)
    showNotification("Please wait patiently before Random Forest analysis is ready...", type = "message",duration = 10)
    group_col <- metadataWithGroup()[[input$define_group]]
    # check NA in normalized_plan_data 
    if (any(is.na(normalized_plan_data()))) {
      showNotification("NA values found in normalized data. Please check your data. NA not permitted in Random Forest predictors", type = "error")
      return(NULL)
      
    }
    
    result <- run_RF(
      X = normalized_plan_data(),
      Y = group_col,
      n_tree = input$rf_n_trees,
      data_partition = input$data_partition /100,
    )
    if (is.null(result)) {
      showNotification("Random Forest failed. Please check input data.", type = "error")
      return()
    }
    rf_results(result)
    showNotification("Random Forest analysis completed.", type = "message")
  })
  

  output$RF_ModelSummary <- DT::renderDataTable({
    req(rf_results())
    
    cm <- rf_results()$confusion_matrix  
    stats_df <- data.frame(
      Metric = names(cm$overall),
      Value  = sapply(cm$overall, function(x) {
        if (is.na(x)) "NA" else round(as.numeric(x), 4)
      }),
      row.names = NULL
    )
    
    DT::datatable(
      stats_df,
      rownames = FALSE,
      options = list(
        dom = "t",
        pageLength = nrow(stats_df)
      )
    )
  })
  
  
  output$RF_ImportancePlot <- renderPlotly({
    req(rf_results())
    vip_plot <- plot_rf_importance(rf_results()$importance,
                                            color_scheme = input$rf_color_scheme)
    ggplotly(vip_plot, tooltip = "text")
  })
  
  output$download_rf_importanceplot <- downloadHandler(
    filename = function()  {
      paste0("RandomForest_Variable_Importance_", Sys.Date(), ".html")
      },
    content = function(file) {
      
      p <- plot_rf_importance(
        rf_results()$importance,
        top_n = input$rf_top_n,
        color_scheme = input$rf_color_scheme
      )
      if (!inherits(p, "plotly")) p <- plotly::ggplotly(p)
      
      htmlwidgets::saveWidget(p, file = file, selfcontained = TRUE)
    }
  )
  
  
  output$RF_ImportanceTable <- DT::renderDataTable({
    req(rf_results())
    importance_df <- rf_results()$importance
    df <- data.frame(
      Lipid = importance_df$feature,
      MeanDecreaseGini = importance_df$MeanDecreaseGini,
      MeanDecreaseAccuracy = importance_df$MeanDecreaseAccuracy,
      row.names = NULL
    )
    
    # Order by MeanDecreaseGini
    df <- df[order(-df$MeanDecreaseGini), ]
    
    DT::datatable(
      df,
      options = list(
        pageLength = 10,
        autoWidth = TRUE
      )
    )
  })
  
  output$download_rf_importancetable <- downloadHandler(
    filename = function() {
      paste0("RandomForest_VariableImportance_Table_", Sys.Date(),".csv")
      },
    content = function(file) {
      importance_df <- rf_results()$importance
      df <- data.frame(
        Lipid = importance_df$feature,
        MeanDecreaseGini = importance_df$MeanDecreaseGini,
        MeanDecreaseAccuracy = importance_df$MeanDecreaseAccuracy,
        row.names = NULL
      )
      write.csv(df, file, row.names = FALSE)
    }
  )

  
  
  ## ---- Download Page ----
  output$download_zip_data <- downloadHandler(
    filename = function() {
      paste0("LipidAnalyst_data_", Sys.Date(), ".zip")
    },
    content = function(file) {
      showModal(modalDialog(
        title = "Preparing download...",
        "Please wait while we compress files.",
        footer = NULL
      ))
      temp_dir <- tempdir()
      data_dir <- file.path(temp_dir, "data")
      dir.create(data_dir, showWarnings = FALSE)
      
      files <- list(
        filtered_data   = list(path = file.path(data_dir, paste0("Filtered_data_", Sys.Date(), ".csv")), data = filtered_data()),
        imputed_data    = list(path = file.path(data_dir, paste0("imputed_lipid_data_", Sys.Date(), ".csv")), data = imputedData()),
        imputed_is      = list(path = file.path(data_dir, paste0("imputed_internal_standard_", Sys.Date(), ".csv")), data = imputedInternalStandard()),
        cleantable      = list(path = file.path(data_dir, paste0("parsed_lipid_names_", Sys.Date(), ".csv")), data = parsed_table()),
        combined_data   = list(path = file.path(data_dir, paste0("combined_lipid_data_", Sys.Date(), ".csv")), data = combinedData()),
        updated_parsed  = list(path = file.path(data_dir, paste0("updated_parsed_table_", Sys.Date(), ".csv")), data = updated_parsed_table()),
        normalized      = list(path = file.path(data_dir, paste0("normalized_by_internal_standard_", Sys.Date(), ".csv")), data = internal_normed_matirx()),
        meta_normalized = list(path = file.path(data_dir, paste0("meta_normalized_data_", Sys.Date(), ".csv")), data = meta_normed_matrix2()),
        plan_normalized = list(path = file.path(data_dir, paste0("normalized_data_", Sys.Date(), ".csv")), data = normalized_plan_data()),
        mean_calculated = list(path = file.path(data_dir, paste0("mean_calculated_data_", Sys.Date(), ".csv")), data = updated_parsed_table_w_mean())
      )
      
      if (exists("volcano_df") && !is.null(volcano_df())) {
        files$volcano <- list(
          path = file.path(data_dir, paste0("volcano_results_", Sys.Date(), ".csv")),
          data = volcano_df()
        )
      }
      
      if (!is.null(ttest_results())) {
        files$ttest <- list(
          path = file.path(data_dir, paste0("ttest_results_", Sys.Date(), ".csv")),
          data = ttest_results()
        )
      }
      
      if (!is.null(anova_results())) {
        files$anova <- list(
          path = file.path(data_dir, paste0("anova_results_", Sys.Date(), ".csv")),
          data = anova_results()
        )
      }
      
      if (!is.null(plsda_results())) {
        vip <- getVipVn(plsda_results())
        df <- data.frame(Lipid = names(vip), VIP = vip, row.names = NULL)
        if(!is.null(df)){
        files$plsda_vip <- list(
          path = file.path(data_dir, paste0("PLSDA_VIP_Table_", Sys.Date(), ".csv")),
          data = df
        )}
      }
      
      if (!is.null(oplsda_results())) {
        vip <- getVipVn(oplsda_results())
        df <- data.frame(Lipid = names(vip), VIP = vip, row.names = NULL)
        if(!is.null(df)){
          files$oplsda_vip <- list(
            path = file.path(data_dir, paste0("OPLSDA_VIP_Table_", Sys.Date(), ".csv")),
            data = df
          )}
      }
      
      if (!is.null(rf_results())) {
        importance_df <- rf_results()$importance
        df <- data.frame(
          Lipid = importance_df$feature,
          MeanDecreaseGini = importance_df$MeanDecreaseGini,
          MeanDecreaseAccuracy = importance_df$MeanDecreaseAccuracy,
          row.names = NULL
        )
        if(!is.null(df)){
          files$rf_vip <- list(
            path = file.path(data_dir, paste0("RandomForest_VariableImportance_Table_", Sys.Date(), ".csv")),
            data = df
          )}
      }
      
      for (f in files) {
        if (!is.null(f$data)) {
          write.csv(f$data, f$path, row.names = TRUE)
        }
      }
      
      if (!is.null(corr_results_list())) {
        for (g in names(corr_results_list())) {
          corr_mat <- corr_results_list()[[g]]
          
          if (!is.null(corr_mat)) {
            fpath <- file.path(data_dir, paste0("Corr_results_", g, "_", Sys.Date(), ".csv"))
            write.csv(
              cor_mat_to_long(as.matrix(corr_mat)),
              fpath,
              row.names = FALSE
            )
          }
        }
      }
      
      if (!is.null(dspc_results())) {
        for (g in names(dspc_results())) {
          dspc_df <-dspc_results()[[g]]$dspc_table
          
          if (!is.null(dspc_df)) {
            fpath <- file.path(data_dir, paste0("dspc_", g, "_", Sys.Date(), ".csv"))
            write.csv(
              dspc_df,
              fpath,
              row.names = FALSE
            )
          }
        }
      }
      

      
      #  zip
      zip::zipr(file, files = list.files(data_dir, full.names = TRUE, pattern = "\\.csv$"))
      removeModal()
    },
    contentType = "application/zip"
  )
  
  # --------- plots save ----------
  output$download_zip_plots <- downloadHandler(
    filename = function() {
      paste0("LipidAnalyst_plots_", Sys.Date(), ".zip")
    },
    content = function(file) {
      showModal(modalDialog(
        title = "Preparing download...",
        "Please wait while we compress files. It might take a while if you have generated many plots.",
        footer = NULL
      ))
      temp_dir <- tempdir()
      plot_dir <- file.path(temp_dir, "plots")
      if (dir.exists(plot_dir)) {
        unlink(plot_dir, recursive = TRUE, force = TRUE)
      }
      dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

      today <- Sys.Date()



      # --------- save single  ---------
      save_plot_safe(boxplot_lipids(),
                     file.path(plot_dir, paste0("boxplot_lipids_", today, ".png")),
                     width=input$width_BP, height=input$height_BP, dpi=input$DPI_BP)
      
      save_plot_safe(boxplot_samples(),    
                     file.path(plot_dir, paste0("boxplot_samples_", today, ".png")),
                     width=input$width_BP, height=input$height_BP, dpi=input$DPI_BP)
      
      save_plot_safe(boxplot_lipidclass(), 
                     file.path(plot_dir, paste0("boxplot_lipidclass_", today, ".png")),
                     width=input$width_BP, height=input$height_BP, dpi=input$DPI_BP)
      
      save_plot_safe(volcano_plot_obj(),   
                     file.path(plot_dir, paste0("volcano_plot_", today, ".png")),
                     width=input$width_volcano, height=input$height_volcano, dpi=input$DPI_volcano)
      
      save_plot_safe(pca_plot_obj_ttest(),   file.path(plot_dir, paste0("PCA_plot_on_significant_features_from_T_test", today, ".png")))
      save_plot_safe(pca_plot_obj2(),  file.path(plot_dir, paste0("PCA_plot_on_significant_features_from_Anova", today, ".png")))
      save_plot_safe(
        plot_opls_vip(
          plsda_results(),
          title = "PLS-DA VIP Plot",
          color_scheme = input$plsda_color_scheme
        ),
        file.path(plot_dir, paste0("PLSDA_VIPPlot_", today, ".png"))
      )
      
      save_plot_safe(
        plot_opls_vip(
          oplsda_results(),
          color_scheme = input$oplsda_color_scheme
        ),
        file.path(plot_dir, paste0("OPLSDA_VIPPlot_", today, ".png"))
      )
      # --------- Plot save for OPLS/PLS DA ---------
      pls_obj <- plsda_results()
      opls_obj <- oplsda_results()
      
      save_opls_plot_safe(
        pls_obj,
        "overview",
        file.path(plot_dir, paste0("PLSDA_overview_", today, ".png"))
      )
      
      save_opls_plot_safe(
        pls_obj,
        "permutation",
        file.path(plot_dir, paste0("PLSDA_permutation_", today, ".png"))
      )
      
      save_opls_plot_safe(
        opls_obj,
        "overview",
        file.path(plot_dir, paste0("OPLSDA_overview_", today, ".png"))
      )
      
      save_opls_plot_safe(
        opls_obj,
        "permutation",
        file.path(plot_dir, paste0("OPLSDA_permutation_", today, ".png"))
      )
      
      save_opls_plot_safe(
        opls_obj,
        "x-score",
        file.path(plot_dir, paste0("OPLSDA_x-score_", today, ".png"))
      )
      
      save_opls_plot_safe(
        opls_obj,
        "outlier",
        file.path(plot_dir, paste0("OPLSDA_outlier_", today, ".png"))
      )
      # --------- HTML plots ---------
      try({
        p <- heatmap_plot_obj()
        if (!is.null(p)) {
          html_file <- file.path(plot_dir, paste0("hierarchical_heatmap_", today, ".html"))
          
          htmlwidgets::saveWidget(
            widget = p,
            file = html_file,
            selfcontained = T
          )
        }
      }, silent = F)
      
      try({
        plot_list <- corr_plot_list()
        if (!is.null(plot_list)) {
          for (g in names(plot_list)) {
            if (!is.null(plot_list[[g]])){
              safe_g <- gsub("[^A-Za-z0-9_\\-]+", "_", g)
              html_file <- file.path(plot_dir, paste0("Correlation_heatmap_", safe_g,"_", today, ".html"))
              
              htmlwidgets::saveWidget(
                widget = plot_list[[g]],
                file = html_file,
                selfcontained = T
              )
            }
          }
        }
      }, silent = F)
      
      try({
        if (!is.null(rf_results())){
        p <- plot_rf_importance(
          rf_results()$importance,
          top_n = input$rf_top_n,
          color_scheme = input$rf_color_scheme
        )
        if (!inherits(p, "plotly")) p <- plotly::ggplotly(p) 
        html_file <- file.path(plot_dir, paste0("RandomForest_Variable_Importance_", today, ".html"))
        htmlwidgets::saveWidget(p, file = html_file, selfcontained = TRUE)}
      }, silent = F)
      
      # --------- Save plots list ---------
      
      if (input$comparison_action > 0) {
        req(all_lipid_class_plot_obj())
        plot_list <- all_lipid_class_plot_obj()
        plot_names <- names(plot_list)
        all_class_dir <- file.path(plot_dir, "lipid_class_comparison_plots")
        dir.create(all_class_dir, showWarnings = FALSE, recursive = TRUE)
        
        for (i in seq_along(plot_list)) {
          fname <- if (!is.null(plot_names) && plot_names[i] != "") {
            paste0(plot_names[i], "_", today, ".png")
          } else {
            paste0("all_lipid_class_plot_", i, "_", today, ".png")
          }
          save_plot_safe(plot_list[[i]], file.path(all_class_dir, fname))
        }
      }

      # --------- also save LCH heatmaps into this zip ---------
      if (input$generate_LCH_plots >0) {
        req(updated_parsed_table_w_mean())
        heatmap_dir <- file.path(plot_dir, "differential_mean_lipid_heatmaps")
        
        save_LCH_heatmaps_to_dir(
          out_dir    = heatmap_dir,
          parsed_tbl = updated_parsed_table_w_mean(),
          ngroup     = ngroup(),
          input      = input
        )
      } 
        
      # zip
      zip::zipr(zipfile = file, files = "plots", root = temp_dir)
      
      on.exit(removeModal(), add = TRUE)
    },
    contentType = "application/zip"
  )


  ## ---- Dynamic side bar ----
  # Track the current step
  current_step <- reactiveVal(0)
  max_step <- reactiveVal(0)
  
  # Render sidebar dynamically based on current step
  output$dynamic_sidebar <- renderMenu({
    step <- max_step()
    
    sidebarMenu(
      id = "tabs",

      # Step 1
      if (step >= 0) menuItem("Welcome!", tabName = "welcome", icon = icon("hands")),
      if (step >= 1) menuItem("Uploading Files", icon = icon("folder-open"), startExpanded = TRUE,
               # Step 1
               if (step >= 1) menuSubItem("Upload Lipidomics Data", 
                                          tabName = "upload_lipid", 
                                          icon = icon("upload")),
               
               # Step 2
               if (step >= 2) menuSubItem("Upload Metadata", 
                                          tabName = "upload_metadata", 
                                          icon = icon("table")),
               
               # Step 3
               if (step >= 3) menuSubItem("Update Internal Standard", 
                                          tabName = "upload_internal", 
                                          icon = icon("flask"))
      ),
      

      # Step 4–6: Preprocessing
      if (step >= 4) menuItem("Preprocessing", icon = icon("wrench"),
                              startExpanded = TRUE,
                              if (step >= 4) menuSubItem("Data Filtering", tabName = "data_filter"),
                              if (step >= 5) menuSubItem("Missing Value Imputation", tabName = "impute"),
                              if (step >= 6) menuSubItem("Combine and Data integration", tabName = "combine"),
                              if (step >= 7) menuSubItem("Lipid Parsing", tabName = "lipid_parsing"),
                              if (step >= 8) menuSubItem("Data Preview", tabName = "preview")
      ),

      # Step 8–9: Normalization
      if (step >= 9) menuItem("Normalization", icon = icon("balance-scale"),
                              startExpanded = TRUE,
                              if (step >= 9) menuSubItem("Quantification by Internal Standard", tabName = "norm_internal"),
                              if (step >= 10) menuSubItem("Quantification by User Defined Factors", tabName = "norm_supplement"),
                              if (step >= 11) menuSubItem("Normalization and Data Scaling", tabName = "norm_plan")
      ),

      # Step 10+: Show plots, stats, download
      if (step >= 11) menuItem("Plots", tabName = "plots", icon = icon("chart-bar"),
                               startExpanded = TRUE,
                               menuSubItem("Global Distribution Boxplot", tabName = "boxplot"),
                               menuSubItem('PCA plot', tabName = "pca"),
                               menuSubItem('Heatmap and Hierarchical Clustering',tabName = 'heatmaphcl'),
                               menuSubItem("Differential Mean Lipid Heatmap", tabName = "LCH_plot"),
                               menuSubItem("Class Level Lipid Comparison", tabName = "plot_comparison"),
                               menuSubItem("Individual Lipid Comparison", tabName = "indi_comparison"),
                               menuSubItem("Volcano Plots", tabName = "volcano_plot")
      ),

      if (step >= 11) menuItem("Statistical Test", icon = icon("calculator"),
                               startExpanded = TRUE,
                               menuSubItem("Lipidomics Mean Calculator", tabName = "mean_cal"),
                               menuSubItem("T-test", tabName = "ttest"),
                               menuSubItem("One way ANOVA", tabName = "anova"),
                               menuSubItem("Correlation", tabName = "correlation"),
                               menuSubItem("DSPC network", tabName = "DSPC"),
                               menuSubItem("PLS-DA",tabName="plsda"),
                               menuSubItem("OPLS-DA", tabName = "oplsda"),
                               menuSubItem("Random Forest", tabName = "rf")
      ),

      if (step >= 11) menuItem("Download", tabName = "download", icon = icon("download"))
    )
  })
  
 # Define the tab order
tab_sequence <- c(
  "welcome",          # step 0
  "upload_lipid",     # step 1
  "upload_metadata",  # step 2
  "upload_internal",  # step 3
  "data_filter",      # step 4
  "impute",           # step 5
  "combine",          # step 6
  "lipid_parsing",    # step 7
  "preview",          # step 8
  "norm_internal",    # step 9         
  "norm_supplement",  # step 10
  "norm_plan"         # step 11
)
# Step 0
observeEvent(input$next0, {
  current_step(1); max_step(max(max_step(), 1))
  updateTabItems(session, "tabs", "upload_lipid")
})

# Step 1
observeEvent(input$prev1, {
  current_step(0)
  updateTabItems(session, "tabs", "welcome")
})
observeEvent(input$next1, {
  current_step(2); max_step(max(max_step(), 2))
  updateTabItems(session, "tabs", "upload_metadata")
})

# Step 2
observeEvent(input$prev2, {
  current_step(1)
  updateTabItems(session, "tabs", "upload_lipid")
})
observeEvent(input$next2, {
  current_step(3); max_step(max(max_step(), 3))
  updateTabItems(session, "tabs", "upload_internal")
})

# Step 3
observeEvent(input$prev3, {
  current_step(2)
  updateTabItems(session, "tabs", "upload_metadata")
})
observeEvent(input$next3, {
  current_step(4); max_step(max(max_step(), 4))
  updateTabItems(session, "tabs", "data_filter")
})

# Step 4
observeEvent(input$prev4, {
  current_step(3)
  updateTabItems(session, "tabs", "upload_internal")
})
observeEvent(input$next4, {
  current_step(5); max_step(max(max_step(), 5))
  updateTabItems(session, "tabs", "impute")
})

# Step 5
observeEvent(input$prev5, {
  current_step(4)
  updateTabItems(session, "tabs", "data_filter")
})
observeEvent(input$next5, {
  current_step(6); max_step(max(max_step(), 6))
  updateTabItems(session, "tabs", "combine")
})

# Step 6
observeEvent(input$prev6, {
  current_step(5)
  updateTabItems(session, "tabs", "impute")
})
observeEvent(input$next6, {
  current_step(7); max_step(max(max_step(), 7))
  updateTabItems(session, "tabs", "lipid_parsing")
})

# Step 7 combine
observeEvent(input$prev7, {
  current_step(6)
  updateTabItems(session, "tabs", "combine")
})
observeEvent(input$next7, {
  current_step(8); max_step(max(max_step(), 8))
  updateTabItems(session, "tabs", "preview")
})

# Step 8 preview
observeEvent(input$prev8, {
  current_step(7)
  updateTabItems(session, "tabs", "lipid_parsing")
})
observeEvent(input$next8, {
  current_step(9); max_step(max(max_step(), 9))
  updateTabItems(session, "tabs", "norm_internal")
})


# Step 9
observeEvent(input$prev9, {
  current_step(8)
  updateTabItems(session, "tabs", "preview")
})
observeEvent(input$next9, {
  current_step(10); max_step(max(max_step(), 10))
  updateTabItems(session, "tabs", "norm_supplement")
})

# Step 10
observeEvent(input$prev10, {
  current_step(9)
  updateTabItems(session, "tabs", "norm_internal")
})
observeEvent(input$next10, {
  current_step(11); max_step(max(max_step(), 11))
  updateTabItems(session, "tabs", "norm_plan")
})

# Step 11
observeEvent(input$prev11, {
  current_step(10)
  updateTabItems(session, "tabs", "norm_supplement")
})
observeEvent(input$next11, {
  current_step(11); max_step(max(max_step(), 11))
  updateTabItems(session, "tabs", "boxplot")
})
# No next button for step 11 if this is the last step

  ## ---- End of Server Logic ----
}