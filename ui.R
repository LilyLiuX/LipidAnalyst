

ui <-dashboardPage(
  dashboardHeader(
    title = tagList(
      # tags$i(class = "fa-solid fa-magnifying-glass", style = "margin-right: 5px;"),
      "LipidAnalyst"
    ),
    titleWidth = 240  # set header width
  ),
  # ---- Dashboard Sidebar ----
  dashboardSidebar(
    width = 240,
    sidebarMenuOutput("dynamic_sidebar")  # <-- Sidebar will be generated dynamically
  ),
  dashboardBody(

                tags$head(

                  tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),

                  # format global
                  tags$style(HTML("


   
    body, label, input, button, select, .box-title, .sidebar-menu, .content-wrapper {
      font-size: 13.5px !important;    
      line-height: 1.3em !important;
    }

    /* ----------- box title ----------- */
    .box-header .box-title {
      font-size: 15px !important;
      font-weight: 600;
      margin-bottom: 4px;
    }

    /* ----------- button ----------- */
    .btn {
      font-size: 13.5px !important;
      padding: 4px 10px;
      border-radius: 4px;
    }

    /* ----------- Heading font styles  ----------- */
    h1, h2, h3, h4 {
      font-weight: 600;
      margin-top: 0.4em;
      margin-bottom: 0.4em;
    }

    /* ----------- sidebar ----------- */
    .sidebar-menu > li > a {
      font-size: 13px !important;
      padding: 8px 15px !important;
    }

    /* ----------- menuSubItem ----------- */
    .sidebar-menu .treeview-menu > li > a {
      font-size: 12.5px !important;    
      padding: 6px 25px !important;   
      line-height: 1.2em !important;
    }

    /* ----------- Remove extra gap ----------- */
    .content-wrapper {
      padding: 10px 15px;
    }

    .modal-content {
      height: 92vh !important;
    }
    .modal-body {
      height: calc(92vh - 120px) !important; /* subtract header+footer roughly */
      overflow-y: auto !important;
    }
  "))
                ),

                
    tabItems(
      # ---- Welcome ----
      tabItem(tabName = "welcome",
              fluidRow(column(width = 12,
                              scroll_box(title =tags$strong("Welcome to LipidAnalyst!"), status = "primary",
                                         p("LipidAnalyst: A Comprehensive Tool for Lipidomics Data Analysis",
                                           style = "font-size:16px;font-weight:bold;"),
                                         img(src = "LipidAnalyst.png", height = "10%", width = "10%"),
                                         p("LipidAnalyst is an interactive web application designed for the analysis of lipidomics data. 
                                           It provides a user-friendly interface for uploading, processing, normalizing, and analyzing lipidomics datasets.",
                                           style = "font-size:14px;"),
                                         p("Key Features:",
                                         tags$ul(style = "font-size:14px; line-height:1.6;",
                                           tags$li("Data Upload: Easily upload lipidomics data, metadata and internal standards in CSV/TSV/XLS/XLSX format."),
                                           tags$li("Data Filtering: Apply filters to remove low-quality, low-abundance, and low-variance features."),
                                           tags$li("Missing Value Imputation: Choose from various imputation methods to handle missing data."),
                                           tags$li("Lipid Parsing: Automatically parse lipid names into structured components."),
                                           tags$li("Normalization: Normalize data using internal standards or other user-defined methods."),
                                           tags$li("Statistical Analysis: Perform T-tests, ANOVA, and correlation analyses with visualization options."),
                                           tags$li("Interactive Visualizations: Explore your data with Differential Mean Lipid Heatmaps, PCA plots, boxplots, DSPC networks, and more.")
                                         )),
                                         br(),
                                         p(
                                           "For detailed instructions and help, please refer to the ",
                                           a("tutorial document", href = "Tutorial.html", target = "_blank"),
                                           ".",
                                           style = "font-size:14px;"
                                         ),
                                        
                                         
                                         # footnote area
                                         tags$hr(),
                                         tags$div(
                                           style = "font-size: 1.0em; color: gray; text-align: right;",
                                           "Developed in University of Michigan/",tags$br(),
                                            "For support or inquiries, please contact: lipidanalyst-requests@umich.edu"
                                           
                                         ),
                                         width = 12)
              )),
              uiOutput("nav_ui0")
      ),
      
      # ---- upload lipid data ----
      tabItem(tabName = "upload_lipid",
              fluidRow(column(width = 12,
                              scroll_box(title = "Upload Lipidomics Data", status = "primary",
                                         p("Please select the correct data format before uploading the file. 
                                           After adjusting any settings, make sure to reload your data so that
                                           the software uses the correct parameters."),
                                         selectInput("lipidROW", "Data format:",
                                                     choices = c("lipids on the columns", "lipids on the rows"),
                                                     selected = "lipids on the columns"),
                                         fileInput("lipidomics_file", "Lipidomics Data File (CSV/TSV/XLS/XLSX)"),
                                         actionButton("load_example_l", "Load Example Lipidomics Data", icon = icon("file")),
                                         downloadButton("download_example_l", "Download Example Data"),
                                         uiOutput("validation"),
                                         actionButton("help_btn_upload1", "Help", icon = icon("question-circle"))
                              )
              )),
              fluidRow(column(width = 12,  
                              scroll_box(title = "Lipidomics Data Preview", status = "info",
                                         uiOutput("Summary"),
                                         withSpinner(DT::dataTableOutput("dataPreview")),
                                         width = 12)
              )),
              uiOutput("nav_ui1")

      ),
      # ---- upload metadata ----
      tabItem(tabName = "upload_metadata",
              fluidRow(column(width = 12,
                              scroll_box(title = "Upload Metadata (Group information)", status = "primary",
                                         fileInput("metadata_file", "Metadata File  (CSV/TSV/XLS/XLSX)"),
                                         actionButton("load_example_m", "Load Example Metadata", icon = icon("file")),
                                         downloadButton("download_example_m", "Download Example Data"),
                                         selectInput("define_group", "Select Grouping Variable:",
                                                     choices = NULL),  # update dynamically
                                         uiOutput("metadata_validation"),
                                         actionButton("help_btn_upload2", "Help", icon = icon("question-circle"))
                                         )
              )),
              fluidRow(column(width = 12,
                              scroll_box(title = "Metadata Preview", 
                                         status = "info", 
                                         withSpinner(DT::dataTableOutput("metadataPreview")),
                                         width = 12)
              )),
              uiOutput("nav_ui2")
              
      ),
      # ---- upload internal standard ----
      tabItem(tabName = "upload_internal",
              fluidRow(column(width = 12,
                              scroll_box(title = "Upload Internal Standard", status = "primary",
                                         p("Please select the correct data format before uploading the file. 
                                           After adjusting any settings, make sure to reload your data so that 
                                           the software uses the correct parameters."),
                                         checkboxInput("skip_upload_internal", "Skip Internal Standard File", F),
                                         conditionalPanel(
                                           condition = "input.skip_upload_internal == false",
                                           p("Please select the correct data format before uploading the file."),
                                           selectInput("lipidROW_internal", "Data format:",
                                                       choices = c("lipids on the columns", "lipids on the rows"),
                                                       selected = "lipids on the columns"),
                                           fileInput("internal_standard_file", "Internal Standard File  (CSV/TSV/XLS/XLSX)"),
                                           actionButton("load_example_i", "Load Example Internal Standard Information", icon = icon("file")),
                                           downloadButton("download_example_i", "Download Example Data"),
                                           uiOutput("internal_standard_validation"),
                                           uiOutput("duplicate_selector_ui")),
                                         actionButton("help_btn_upload3", "Help", icon = icon("question-circle"))
                              
              ))),
              fluidRow(column(width = 12,
                              scroll_box(title = "Internal Standard Preview", 
                                         status = "info", 
                                         withSpinner(DT::dataTableOutput("InternalStandardPreview")),
                                         width = 12)
              )),
              uiOutput("nav_ui3")
              
      ),
      # ---- data filter ----
      tabItem(
        tabName = "data_filter",
        fluidRow(
          scroll_box(
            title = "Data Filtering Overview", status = "primary", solidHeader = TRUE, width = 12,
            p("Data filtering is a crucial preprocessing step in lipidomics data analysis. 
              It helps to eliminate noise and irrelevant features, ensuring that subsequent analyses focus on meaningful 
          biological variations."),
            p("This section provides options to apply various filters to your lipidomics data. 
              You can choose to enable or disable each filter and set specific thresholds according to your analysis needs.")
          )
        ),
        
        fluidRow(
          box(
            title = "Low Quality Filter", status = "primary", solidHeader = TRUE, width = 12,
            p("Low Quality Filter removes features with a high percentage of missing values across samples."),
            checkboxInput("enable_quality_filter", "Enable Low Quality Filter", TRUE),
              conditionalPanel(
              condition = "input.enable_quality_filter == true",
              sliderInput("missing_percentile", 
                          "Filter bottom features with more than x% missing value(s):", 
                          min = 0, max = 100, value = 50)
            )
          )
        ),
        fluidRow(
          box(
            title = "Low Abundance Filter", status = "primary", solidHeader = TRUE, width = 12,
            p("Low Abundance Filter removes features with a mean lower than a specified percentile across all feature means."),
            checkboxInput("enable_abundance_filter", "Enable Low Abundance Filter", TRUE),
            conditionalPanel(
              condition = "input.enable_abundance_filter == true",
              selectInput("abundance_stat", "Use summary statistic:", choices = c("Mean", "Median")),
              sliderInput("abundance_percentile", "Filter bottom X% features:", min = 0, max = 100, value = 0),
              textOutput("abundance_cutoff_text")
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Low Variance Filter", status = "primary", solidHeader = TRUE, width = 12,
            p("Low Variance Filter removes features with low variability across samples, 
              which would not be useful for distinguishing between different conditions or groups."),
            checkboxInput("enable_variance_filter", "Enable Low Variance Filter", TRUE),
            
            conditionalPanel(
              condition = "input.enable_variance_filter == true",
              radioButtons("variance_method", "Variance method:",
                           choices = c("Interquantile range (IQR)" = "IQR",
                                       "Standard deviation (SD)" = "SD",
                                       "Median absolute deviation (MAD)" = "MAD",
                                       "Relative standard deviation (RSD = SD/mean)" = "RSD",
                                       "Non-parametric RSD (MAD/median)" = "MAD_RSD")),
              sliderInput("variance_percentile", "Filter bottom X% features:", min = 0, max = 100, value = 10)
            )
          )
        ),
        
        fluidRow(column(width = 12,
          
          scroll_box(title = "Filtered Data Preview", status = "info", 
                     actionButton("run_data_filtering", "Apply Filters", icon = icon("filter")),
          br(), br(),
          downloadButton("download_filtered", "Download CSV"),
          uiOutput("summary_filtered_data"),
          withSpinner(DT::dataTableOutput("filtered_data_preview")),
          width = 12
          ))
        ),
        uiOutput("nav_ui4")
      ),

      # ---- imputation ----
      tabItem(tabName = "impute",
              fluidRow(column(width = 12,
                              scroll_box(title = "Missing Value Imputation Guide", status = "info",
                                         p("Missing values are common in lipidomics datasets and can arise from several sources, including:"),
                                         
                                         tags$ul(
                                           tags$li("Technical limitations of the mass spectrometer, such as detection thresholds or signal suppression,"),
                                           tags$li("Variability introduced during sample extraction, handling, or instrument runs,"),
                                           tags$li("True biological absence of a lipid in certain samples or conditions.")
                                         ),
                                         
                                         p("Understanding the pattern of missingness is essential for selecting an appropriate imputation strategy. 
   Different types of missingness can reflect different underlying causes and may require tailored handling 
   to avoid introducing bias into downstream analyses such as statistical testing, or network construction."),
                                         
                                         p("We categorize missingness into two major types:"),
                                         
                                         tags$ul(
                                           tags$li(
                                             tags$b("Group-level missingness:"), 
                                             " A lipid is almost completely missing within one experimental group (e.g., all disease group samples have NA). 
     This often suggests true biological absence or very low abundance. 
     In these cases, methods such as ",
                                             tags$b("Limit of Detection (LoD) imputation"), 
                                             " is generally more appropriate."
                                           ),
                                           tags$li(
                                             tags$b("General missingness:"), 
                                             " Values are sporadically missing across samples but not confined to a single group. 
      This pattern typically reflects technical noise or stochastic signal dropout. 
      For this scenario, ",
                                             tags$b("K-Nearest Neighbors (KNN) imputation(sample-wise)"), 
                                             " is recommended as it leverages similarity among samples to estimate reasonable values."
                                           )
                                         ),
                                         
                                         p("You can view the Missing value heatmap to identify these patterns and decide on the best imputation approach for your data."),
                                         
                                         actionButton("missing_heatmap_modal", "View Missing Value Heatmap", icon = icon("chart-bar")),
                      
                                         width = 12)
              )),
              fluidRow(column(width = 6,
                              scroll_box(
                                title = "Group-level Missingness Summary", status = "primary",
                                p(HTML("Group Level Missing means features that are missing in a large proportion of samples within a specific group. 
                                  This type of missingness can usually mean features are truly absent in the specific group, 
                                  and we suggest to use <b>Limit of Detection (LoD) 1/5 minimum value</b> of the specific feature to impute the missingness here.")), 
                                checkboxInput("skip_imputation0", "Skip imputation of group level missingness", FALSE),
                                conditionalPanel(
                                  condition = "input.skip_imputation0 == false",
                                  numericInput("group_missing_threshold",
                                               "Threshold for group-level missingness (Missing for X % in one group):",
                                               value = 95, min = 5, max = 100, step = 1),
                                  uiOutput("group_missing_message"),
                                  selectInput("impute_method_group", "Imputation Method:",
                                              choices = c("Limit of Detection (LoD) 1/5 minimum value"= "LoD 1/5 minimum value",
                                                          "Limit of Detection (LoD) 1/2 minimum value"= "LoD 1/2 minimum value") ),
                                  conditionalPanel("input.impute_method_group == 'knn-featurewise' | input.impute_method_group == 'knn-samplewise'",
                                                   numericInput("knn_k", "Number of Nearest Neighbours:", value = 5, min = 1)),
                                  actionButton("impute_action0", "Impute the group-level missingness")
                                  ),
                                width = 12
                              )
              ),
              conditionalPanel(
                condition = "output.show_general_imputation == true",
             column(width = 6,
                              scroll_box(title = "Imputation Settings", status = "primary",
                                         p("This panel allows you to handle general missing values in your lipidomics data. 
                                           You can choose to skip imputation (if no missingness) 
                                           or select from various imputation methods to fill in missing data points."),
                                         checkboxInput("skip_imputation", "Skip imputation", FALSE),
                                         uiOutput("Missing_checks"),
                                         conditionalPanel(
                                           condition = "input.skip_imputation == false",
                                           selectInput("impute_method", "Imputation Method:",
                                                       choices = c("KNN-featurewise"="knn-featurewise",
                                                                   "KNN-samplewise"="knn-samplewise",
                                                                   "Median"="median",
                                                                   "Mean"="mean",
                                                                   "Limit of Detection (LoD) 1/5 minimum value" = "LoD 1/5 minimum value",
                                                                   "Limit of Detection (LoD) 1/2 minimum value"= "LoD 1/2 minimum value"), selected = "knn-samplewise"),
                                           conditionalPanel("input.impute_method == 'knn-featurewise' | input.impute_method == 'knn-samplewise'",
                                                            numericInput("knn_k", "Number of Nearest Neighbours:", value = 5, min = 1)),
                                           # 🔽 Explanation for Limit of Detection (LoD) 1/5 minimum value method
                    #                        conditionalPanel("input.impute_method == 'Limit of Detection (LoD) 1/5 minimum value'",
                    #                                         helpText("The 'Limit of Detection (LoD) 1/5 minimum value' method imputes missing values \
                    # by taking the featurewise minimum value \
                    # and multiplies it by 0.2 to simulate a low abundance value. \
                    # This method assumes missing values are due to values being below detection limit.")
                    #                        ),
                                           actionButton("impute_action", "Impute the Data")),
                                           # actionButton("impute_internal_standard_action", 
                                           #               "Impute Internal Standard Missing Values")),
                                         br(),br(),
                                         width = 12)

              ))
             ),
              fluidRow(column(width = 12,
                              scroll_box(title = "Imputed Data Preview", status = "info", 
                                         downloadButton("download_imputed_data", "Download CSV"),
                                         uiOutput("summary_imputedData"),
                                         withSpinner(DT::dataTableOutput("imputedDataPreview")),
                                         width = 12)
              )),
              # fluidRow(column(width = 12,
              #                 scroll_box(title = "Imputed Internal Standard Preview", status = "info", 
              #                            downloadButton("download_imputed_is", "Download CSV"),
              #                            DT::dataTableOutput("InternalStandardPreview_imputed"),
              #                            width = 12)
              # )),
              uiOutput("nav_ui5")
      ),
      # ---- Combine Duplicated lipids ----
      tabItem(tabName = "combine",
              
              fluidRow(column(width = 12,
                              scroll_box(title = 'Combine duplicated lipids or lipids with different adducts',
                                         status = "info",
                                         p("In lipidomics data, it is common to encounter duplicated lipid entries that represent the lipid species with same shorthand 
                                            name but differ in their adduct forms (e.g., [M+H]+, [M+Na]+, [M+K]+). 
                                            Combining these duplicated lipids into a single representative entry can help streamline data analysis and interpretation. 
                                            This process involves selecting criteria to identify and merge these duplicates based on their lipid names and adduct types."),
                                         # text output to show the duplicates name
                                         uiOutput("duplicate_lipid_names"),
                                         width = 12,
                                         height ='400px'
                              )
              )),
              
              fluidRow(column(width = 12,
                              box(title = 'Combine Duplicated lipids',status = "primary",
                                  strong( "If you have duplicated lipids with different adducts, \
                                         you can combine them into one lipid by selecting the criteria below. "),
                                  checkboxInput("skip_combine", "Skip combine duplicated lipids", FALSE),
                                  conditionalPanel(
                                    condition = "input.skip_combine == false",
                                    actionButton("add_combine_rule", "➕ Add combine criteria"),
                                    actionButton("CombineButton", "Combine duplicated lipids in the data.")),
                                  width = 12,
                                  collapsible = TRUE,
                                  collapsed = F,
                                  style = paste0(
                                    "overflow-y: auto;", 
                                    "height: ", "400px")
                              )
              )),
              fluidRow(column(width = 12,
                              scroll_box(title = "Combined Data Preview", status = "info",
                                         downloadButton("download_combined_data", "Download CSV"),
                                         uiOutput("summary_processedData"),
                                         withSpinner(DT::dataTableOutput("combinedDataPreview")),
                                         width = 12)
              )),
              uiOutput("nav_ui6")
      ),
      # ---- parsing ----
      tabItem(tabName = "lipid_parsing",
              fluidRow(column(width = 12,
                              scroll_box(title = "Lipid Parsing Controls", status = "primary",
                                         width =12,
                                         p(HTML(paste0(
                                           "<b>Chain annotation convention</b><br/>",
                                           "The biological meaning of each chain field depends on lipid class:<br/><br/>",
                                           
                                           "<ul>",
                                           "<li><b>Sphingolipids</b> (e.g., SM, Cer, dhCer, LacCer, HexCer): ",
                                           "<code>Chain1</code> = sphingoid base (long-chain base, LCB), ",
                                           "<code>Chain2</code> = N-acyl <b>fatty acid</b> (amide-linked acyl chain), ",
                                           "<code>Chain3</code> = not applicable.</li>",
                                           
                                           "<li><b>Phospholipids</b> (e.g., PC, PE, PG, PI, PS,LPC, LPE, PE(O), PE(P), PA): ",
                                           "<code>Chain1</code> and <code>Chain2</code> refer to fatty acyl chains at Sn1 and Sn2 carbons of the glycerol back bone, 
                                           respectively. <code>Chain3</code> = not applicable. </li>",
                                           
                                           "<li><b>Glycerolipids</b> (e.g., MAG, TAG, DAG): ",
                                           "<code>Chain1</code> to <code>Chain3</code> refer to fatty acyl chains at Sn1 to Sn3 carbons of the glycerol back bone, respectively.",
                                           "For MAG, <code>chain2</code> and <code>chain3</code> are not applicable.</li>",
                                           
                                           "<li><b>Cholesteryl esters</b> (CE): ",
                                           "<code>Chain1</code> = esterified <b>fatty acid</b> chain, ",
                                           "<code>Chain2</code> and <code>Chain3</code> = not applicable.</li>",
                                           "</ul>",
                                           "<span style='color:#666;'>Note: Chain fields are stored uniformly for all lipid classes; ",
                                           "interpretation is lipid-class–dependent.</span>",
                                           "<br/>"
                                         ))),
                                         
                                         p(HTML("The parsing table is <b>editable</b>.
                                                 Double-click a cell to make changes.
                                                 If any lipid information is incorrect,
                                                 you may revise it directly in the table.")),
                                         p(HTML("Use the search box in each column to filter the rows that you want to view or edit.")),
                                         actionButton("parseButton", "Parse Lipid Names"),
                                         
                                         p(HTML("<br>If you want to edit multiple points in the parse table,
                                                we suggest download the parse table as a CSV file, make changes in Excel,
                                                and upload the updated parse table using the file input below.")),
                                         fileInput("new_parse_file", "Updated parse table Data File (CSV)"),
                                         # cancel uploading 
                                         checkboxInput(
                                           "cancel_parse_upload",
                                           " Cancel uploading the updated parse table and keep the original parse table", 
                                           FALSE
                                         )
              ))),
              fluidRow(column(width = 12,
                              scroll_box(title = "Parsed Lipid Names", status = "info", 
                                         downloadButton("download_cleantable", "Download CSV"),
                                         withSpinner(DT::dataTableOutput("cleantable")),
                                         width = 12)
              )),
              uiOutput("nav_ui7")
      ),

      # ---- Data Preview ----
      tabItem(tabName = "preview",
              
              fluidRow(
                box(
                  title = "Lipid Class Pie Plot",
                  width = 6, height = 400, solidHeader = TRUE, status = "primary",
                  actionButton("help_pie", "Help", icon = icon("question-circle")),
                  withSpinner(plotlyOutput("pie_plot", height = "300px"))
                ),
                box(
                  title = "Lipid Class Boxplot",
                  width = 6, height = 400, solidHeader = TRUE, status = "primary",
                  actionButton("help_LCB", "Help", icon = icon("question-circle")),
                  withSpinner(plotlyOutput("lipid_class_boxplot", height = "300px"))
                )
              ),
              
              fluidRow(
                box(
                  title = "PCA Plot",
                  width = 6, height = 400, solidHeader = TRUE, status = "primary",
                  actionButton("help_pca_preview", "Help", icon = icon("question-circle")),
                  withSpinner(plotlyOutput("pca_plot_preview", height = "300px"))
                ),
                box(
                  title = "Sample Boxplot",
                  width = 6, height = 400, solidHeader = TRUE, status = "primary",
                  actionButton("help_sample_boxplot", "Help", icon = icon("question-circle")),
                  withSpinner(plotlyOutput("sample_boxplot", height = "300px"))
                )
              ),
              fluidRow(
                box(title = "Barplot of lipid species", solidHeader = TRUE, status = "primary",
                    actionButton("help_barplot", "Help", icon = icon("question-circle")),
                    withSpinner(plotlyOutput("lipid_barplot", height = "300px")),
                     width = 6,height = 400) ,
                box(title = "Barplot of group distribution", solidHeader = TRUE, status = "primary",
                    actionButton("help_barplot_g", "Help", icon = icon("question-circle")),
                    withSpinner(plotlyOutput("group_barplot", height = "300px")),
                    width = 6,height = 400)                 
              ),
              uiOutput("nav_ui8")
      ),
      
      
      # ---- Normalization: internal standard ----
      tabItem(tabName = "norm_internal",
              
              fluidRow( column(width = 12,
                               scroll_box(title = "Internal Standard Selection", status = "primary",
                                          p("Normalization by internal standards is a widely used technique 
                                              in lipidomics data analysis to correct for technical variability 
                                              and improve the accuracy of quantification. 
                                                Internal standards are compounds that are chemically similar to
                                                the target analytes but are not naturally present in the sample. 
                                                By adding known quantities of internal standards to each sample 
                                                prior to analysis, researchers can account for variations in 
                                                sample preparation, instrument performance, and other factors 
                                                that may affect the measured signal intensity."),
                                          checkboxInput("skip_internal", "Skip normalization by internal standard", FALSE),
                                          conditionalPanel(
                                            condition = "input.skip_internal == false",
                                            uiOutput("internalStandardSelection"),
                                            div(
                                              style = "display: inline-block; margin-right: 15px;",
                                              actionButton("save_internal_standard_selection", "Save Selection")
                                            ),
                                            div(
                                              style = "display: inline-block; margin-right: 15px;",
                                              actionButton("reset_internal_standard_selection", "Reset to Default", icon = icon("undo"))
                                            ),
                                            div(
                                              style = "display: inline-block; margin-right: 15px;",
                                              uiOutput("run_norm_ui")
                                            )),
                                          width =10)
              )),
              fluidRow( column(width = 12,
                               scroll_box(title = "Updated Parsed Table with Internal Standard", 
                                          status = "info", 
                                          downloadButton("download_updated_parsed", "Download CSV"),
                                          withSpinner(DT::dataTableOutput("updatedParsedTable")),
                                          width = 12,collapsed = T)
              )),
              fluidRow( column(width = 12,
                               scroll_box(title = "Normalization Messages", 
                                          status = "info", 
                                          verbatimTextOutput("normalizationLog"),
                                          width = 12,collapsed = T)
              )),
              fluidRow( column(width = 12,
                               scroll_box(title = "Normalized Data Preview", status = "info", 
                                          downloadButton("download_normalized", "Download CSV"),
                                          uiOutput("summary_internal_normed_matrix"),
                                          withSpinner(DT::dataTableOutput("normalizedDataPreview")),
                                          width = 12)
              )),
              uiOutput("nav_ui9")
            
      ),
      # ---- Normalization by Supplement ----
      tabItem(tabName = "norm_supplement", 
              fluidRow(
                column(width = 6,   
                       scroll_box(
                         title = "Normalization by a user-defined constant value ", 
                         status = "primary",
                         p(HTML("Normalization by a constant value involves adjusting the lipid abundances by dividing each data entry by a user-defined constant. <br>
                           This method is useful when there is a known factor that affects all samples equally, such as dilution factor, weight, or volume.<br>
                           By applying this normalization, researchers can standardize the lipid measurements across samples, facilitating more accurate comparisons and analyses.<br>")),
                         checkboxInput("skip_supp", "Skip normalization by supplement coeffient", FALSE),
                         conditionalPanel(
                           condition = "input.skip_supp == false",
                           numericInput("supplement_coeffient", 
                                        "Normalized by a constant value (e.g. Dilution factor, Weight, Volume):", 
                                        value = 2.5),
                           actionButton("run_supplement_normalization", "Run Normalization")),
                         width = 12  # <-- width goes **inside** the scroll_box
                       )
                ),
              conditionalPanel(
                condition = "output.show_met_norm2 == true",
                column(width = 6,
                       scroll_box(
                         title = "Metadata Supplement", 
                         status = "primary",
                         p(HTML("Normalization by metadata involves adjusting lipid abundances based on relevant metadata information. <br>
                           This method helps to account for variations in sample characteristics, experimental conditions, or other factors that may influence lipid measurements, such as cell counts and protein concentration. <br>
                           By normalizing the lipid data using appropriate metadata, researchers can improve the accuracy and reliability of their analyses, leading to more meaningful biological interpretations.<br>")),
                         checkboxInput("skip_meta", "Skip normalization by metadata", FALSE),
                         conditionalPanel(
                           condition = "input.skip_meta == false",
                           selectInput("metadata_supplement", 
                                       "Select a column from the uploaded Metadata as the Supplement Data:", 
                                       choices = NULL),  # dynamic from server
                           actionButton("run_metadata_normalization", "Run Metadata Normalization")),
                         width = 12
                       )
                )
              )),
              
              fluidRow(  
                column(width = 12,
                       scroll_box(
                         title = "Normalized Data Preview", 
                         status = "info", 
                         width = 12,
                         downloadButton("download_meta_normalized", "Download CSV"),
                         uiOutput("summary_meta_normed_matrix2"),
                         withSpinner(DT::dataTableOutput("MetanormalizedDataPreview"))
                       )
                )
              ),
              uiOutput("nav_ui10")
      ),
      # ---- Normalization Plan ----
      tabItem(tabName = "norm_plan",
              fluidRow(
                column(width = 12,
                       scroll_box(
                         title = "Normalization",
                         status = "primary",
                         width = 12,  # <-- width goes **inside** the scroll_box
                         radioButtons(
                           inputId = "norm1",
                           label = "Select a normalization method:",
                           choices = list(
                             'None' = "none_1",
                             "Normalized by sum (samplewise)" = "sample_sum",
                             "Normalized by median(samplewise)" = "sample_median",
                             "Normalized by mean(samplewise)" = "sample_mean",
                             "Lipid class sum normalization" = "lipid_class_sum",
                             "Lipid class median normalization" = "lipid_class_median",
                             "Lipid class mean normalization" = "lipid_class_mean",
                             "Quantile normalization (suggested only for > 1000 features)" = "qnorm"
                           ),
                           selected = "none_1"  # Nothing selected initially
                           #Quantile normalization is a technique that makes the distributions of
                           # values across multiple samples identical by aligning their quantiles,
                           # helping to reduce technical variation and enable fair comparisons.
                         ),
                         actionButton("help_btn_norm1", "Help", icon = icon("question-circle")),
                         conditionalPanel(
                           condition = "input.norm1 == 'qnorm'",
                           helpText("Quantile normalization is a technique that makes the distributions of values across multiple samples
                                    identical by aligning their quantiles, helping to reduce technical variation and enable fair comparisons.")
                         )
                       )
                )),
              fluidRow(
                column(width = 12,
                       scroll_box(
                         title = "Data Transformation",
                         status = "primary",
                         width = 12,
                         radioButtons(
                           inputId = "norm2",
                           label = "Select a data transformation method:",
                           choices = list(
                             'None' = "none_2",
                             "Log transformation" = "log_t",
                             "Logit transformation (suggested only after lipid class sum normalization )" = "logit_t",
                             "Square root" = "s_root",
                             "Cubic root" = "c_root"
                           ),
                           selected = "none_2" # Nothing selected initially
                         ),
                         actionButton("help_btn_transform", "Help", icon = icon("question-circle")),
                         conditionalPanel(
                           condition = "input.norm2 == 'log_t'",
                           selectInput(
                             inputId = "log_base",
                             label = "Select Log Base:",
                             choices = c("Natural Log (e)" = "ln", "Log2" = "log2", "Log10" = "log10"),
                             selected = "ln"
                           )
                         )
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       scroll_box(
                         title = "Data Scaling",
                         status = "primary",
                         width = 12,
                         radioButtons(
                           inputId = "norm3",
                           label = "Select a data scaling method:",
                           choices = list(
                             'None' = "none_3",
                             'Mean centered' = "mean_centered",
                             'Auto scaling' = "auto_scaling",
                             'Pareto scaling' = "pareto_scaling",
                             'Range scaling' = "range_scaling"
                           ),
                           selected = 'none_3'  # Nothing selected initially
                         ),

                         actionButton("help_btn_scaling", "Help", icon = icon("question-circle"))

                       )
                )
              ),

            fluidRow(
              column(width = 12,
                     scroll_box(
                       title = "Normalized Data Preview",
                       status = "info",
                       width = 12,
                       actionButton("run_plan_normalization", "Run Normalization Plan"),
                       downloadButton("download_plan_normalized", "Download CSV"),
                       uiOutput("summary_normalized_plan_data"),
                       withSpinner(DT::dataTableOutput("PlanDataPreview"))
                     )
              )
            ),
            uiOutput("nav_ui11")

      ),
      
      
      # ---- boxplot ----
      tabItem(tabName = "boxplot",
              fluidRow(
                column(width = 12,
                       box(title = "Boxplot Settings", status = "primary",
                          selectInput("boxplot_var", "Boxplot for:",
                                      choices = c("boxplot for Samples","boxplot for Lipid Class","boxplot for Lipids" )),
                          conditionalPanel("input.boxplot_var == 'boxplot for Lipids'",
                                           helpText("We don't support boxplot if your expression data has more than 80 lipids.")),
                          actionButton("Box_action", "Plot the Boxplot"),
                           collapsible = TRUE,
                           collapsed = F,
                           width = 6,
                           style = paste0(
                             "overflow-y: auto;", 
                             "height: ", "200px"
                           )
                           ),
                       scroll_box(title="Pixel size settings for downloading Boxplot", status = "primary",
                                  numericInput("width_BP", "Width (in inches):", value = 10, step = 0.5),
                                  numericInput("height_BP", "Height (in inches):", value = 6, step = 0.5),
                                  sliderInput("DPI_BP","Select the dots per inch (DPI) for downloading plot:",
                                              min = 60, max =600,value=100,step=10),
                                  uiOutput("pixel_info_BP"),
                                  width = 6,collapsed = T
                       ),
                       scroll_box(title = "Boxplot Output", status = "info", 
                                  downloadButton("download_boxplot", "Download Boxplot"),
                                  uiOutput("boxPlotUI"),
                                  width = 12)
                ))
      ),
      # ---- plot_comparison ----
      tabItem(tabName = "plot_comparison",
              fluidRow(
                column(width = 12,
                       scroll_box(title = "Plot Settings", status = "primary",
                                  selectInput("plot_lipid_class", "Select lipid class for comparison:",choice = NULL),
                                  # enable double bond filter
                                  checkboxInput("enable_db_filter", tags$strong("Filter by number of double bonds"), FALSE),
                                  
                                  # show range slider only if checkbox is TRUE
                                  conditionalPanel(
                                    condition = "input.enable_db_filter == true",
                                    sliderInput("tt_unsat", "Select range for number of double bonds:",
                                                min = 0, max = 20, value = c(1, 5))
                                  ),
                                  

                                  # enable total carbon filter
                                  checkboxInput("enable_c_filter",tags$strong( "Filter by total carbon number"), FALSE),
                                  
                                  # show carbon range slider only if checkbox is TRUE
                                  conditionalPanel(
                                    condition = "input.enable_c_filter == true",
                                    sliderInput("total_c_range", "Select range for total carbon number:",
                                                min = 0, max = 100, value = c(20, 40))
                                  ),
                                  
                                  conditionalPanel(
                                    condition = "input.enable_db_filter == true",
                                    checkboxInput("enable_db_filter2", tags$strong("Enable an additional range selection for number of double bonds"), FALSE)
                                  ),
                                  conditionalPanel(
                                    condition = "input.enable_db_filter2 == true",
                                    sliderInput("tt_unsat2", "Select range for number of double bonds:",
                                                min = 0, max = 20, value = c(1, 5))
                                  ),
                                  
                                  conditionalPanel(
                                    condition = "input.enable_c_filter == true",
                                    checkboxInput("enable_c_filter2", tags$strong("Enable an additional range selection for total carbon number"), FALSE)
                                    ),
                                  
                                  # show carbon range slider only if checkbox is TRUE
                                  conditionalPanel(
                                    condition = "input.enable_c_filter2 == true",
                                    sliderInput("total_c_range2", "Select range for total carbon number:",
                                                min = 0, max = 100, value = c(20, 40))
                                  ),
                                  
                                  selectInput("plot_type_var", "Select the plot type:",
                                              choices = c("boxplot","violin plot")),
                                  
                                  selectInput("stats_m", "Select the statistical methods to compare difference (Only for two group comparison):",
                                              choices = list(
                                                "Student's T test (equal variances assumed) " = "student",
                                                "Welch's T test (unequal variances allowed) " = "welch",
                                                "Wilcoxon Test" = "wilcox"
                                              )),
                                  checkboxInput("show_points", "Show individual data points", value = TRUE),
                                  actionButton("comparison_action", "Generate Plot")),
                       scroll_box(title="Pixel size settings for downloading plots", status = "primary",
                                  numericInput("width_CP", "Width (in inches):", value = 10, step = 0.5),
                                  numericInput("height_CP", "Height (in inches):", value = 6, step = 0.5),
                                  sliderInput("DPI_CP","Select the dots per inch (DPI) for downloading plot:",
                                              min = 60, max =600,value=100,step=10),
                                  uiOutput("pixel_info_CP"),
                                  width = 12,collapsed = T
                       ),
                       scroll_box(title = "Plot Output", status = "info", 
                                  downloadButton(
                                    "Lipid_class_download_plot_single",
                                    "Download the Plot for the Selected Lipid Class"
                                  ),
                                  downloadButton("Lipid_class_download_plot", "Download Plots for All Lipid Class"),
                                  uiOutput("Lipid_class_plotUI"),
                                  width = 12)
                ))
      ),
      ##---- individual comparison ----
      tabItem(
        tabName = "indi_comparison",
        fluidRow(
          column(
            width = 12,
            scroll_box(
              title = "Plot Settings", status = "primary",
              selectInput("selected_lipid", "Select a lipid:", choices = NULL),
              selectInput(
                "plot_type_var2", "Select plot type:",
                choices = c("boxplot", "violin plot")
              ),
              selectInput(
                "stats_m2", "Statistical method:",
                choices = list(
                  "Student's T test" = "student",
                  "Welch's T test" = "welch",
                  "Wilcoxon Test" = "wilcox"
                )
              ),
              checkboxInput("show_points2", "Show individual data points", value = TRUE),
              actionButton("single_lipid_plot_btn", "Generate Plot")
            ),
            scroll_box(title="Pixel size settings for downloading plots", status = "primary",
                       numericInput("width_IP", "Width (in inches):", value = 10, step = 0.5),
                       numericInput("height_IP", "Height (in inches):", value = 6, step = 0.5),
                       sliderInput("DPI_IP","Select the dots per inch (DPI) for downloading plot:",
                                   min = 60, max =600,value=100,step=10),
                       uiOutput("pixel_info_IP"),
                       width = 12,collapsed = T
            ),
            scroll_box(
              title = "Plot Output", status = "info",
              downloadButton(
                "single_lipid_plot_download",
                "Download the Plot for the Selected Lipid"
              ),
              uiOutput("single_lipid_plotUI"),
              width = 12
            )
          )
        )
      ),
      # ---- LCH_plot ----
      tabItem(tabName = "LCH_plot",
              fluidRow(
                column(width = 6,
                       scroll_box(title = "Differential Mean Lipid Heatmap Parameter", status = "primary",
                                  selectInput("LC_selection", "Select the lipid class of interest", choices = NULL),
                                  # input the range of heatmap 
                                   checkboxInput("label_text_checkbox", "Show numbers inside the heatmap", value = F),
                                   conditionalPanel(
                                     condition = "input.label_text_checkbox == true",
                                     sliderInput("label_text_size", "Select Label font size for Heatmap:",
                                                 min = 0.1, max = 5 , value = 3, step = 0.1)
                                   ),
                                  numericInput("heatmap_min", "Heatmap range (min):", value = -1, step = 0.1),
                                  numericInput("heatmap_max", "Heatmap range (max):", value = 1, step = 0.1),
                                  selectInput("sequence_heatmap",
                                              "For lipids that has multiple chains, select a criteria to sequence lipids on the x axis",
                                              choice = c("Based on Chain1 carbon number", 
                                                         "Based on total carbon number",
                                                         "Based on Chain2 carbon number"
                                              )),
                                  width = 12),
                         scroll_box(title="Pixel size settings for downloading Heatmap", status = "primary",
                                    numericInput("width_LCH", "Width (in inches):", value = 8, step = 0.5),
                                    numericInput("height_LCH", "Height (in inches):", value = 6, step = 0.5),
                                    sliderInput("DPI_LCH","Select the dots per inch (DPI):",
                                                min = 60, max =600,value=100,step=10),
                                    uiOutput("pixel_info_LCH"),                       
                                    width = 12,collapsed = T
                         )),
                column(width = 6,
                       scroll_box(title ="Font and Color Settings", status ="primary",
                                  selectInput("color_code_LCH", "Select the color code for the plot:",
                                              choices = list(
                                                'Default: Red/Blue' = "RWB",
                                                'Blue/Green/Yellow' = "BGY",
                                                'Orange/Blue' = "OWB",
                                                'Grey Scale' = "greyscale",
                                                'Viridis color'= 'viridis',
                                                "Heat color" = 'heat'
                                              ),
                                              selected = 'RWB'),
                                  sliderInput("label_title_size","Select title font size for Heatmap:",
                                              min = 2, max = 20,
                                              value = 14,
                                              step = 0.5),
                                  
                                  sliderInput("label_x_size","Select x axis label font size for Heatmap:",
                                              min = 1, max = 15,
                                              value = 8,
                                              step = 0.5),
                                  
                                  sliderInput("label_y_size","Select y axis label font size for Heatmap:",
                                              min = 1, max = 15,
                                              value = 8,
                                              step = 0.5),
                                  
                                  sliderInput("title_x_size","Select x axis title font size for Heatmap:",
                                              min = 2, max = 20,
                                              value = 12,
                                              step = 0.5),
                                  
                                  sliderInput("title_y_size","Select y axis title font size for Heatmap:",
                                              min = 2, max = 20,
                                              value = 12,
                                              step = 0.5),
                                  
                                  sliderInput("legend_title_size","Select legend title font size for Heatmap:",
                                              min = 2, max = 20,
                                              value = 12,
                                              step = 0.5),
                                  
                                  sliderInput("legend_text_size","Select legend text font size for Heatmap:",
                                              min = 1, max = 15,
                                              value = 8,
                                              step = 0.5),
                                  width = 12,collapsed = T
                         
                       ),
                       actionButton(
                         "generate_LCH_plots",
                         "Generate Heatmaps",
                         style = "margin-bottom: 12px;"
                       ),
                       
                       downloadButton(
                         "download_selection",
                         "Download the Plots for the Selected Lipid Class",
                         style = "margin-bottom: 12px;"
                       ),
                       
                       downloadButton(
                         "download_zip",
                         "Download Plots of All Lipid Classes Following same requirements as ZIP",
                         style = "margin-bottom: 12px;"
                       )
                       
                       ),
                column(width = 12,
                       scroll_box(title = "Differential Mean Lipid Heatmap Output", status = "info",
                                  uiOutput("plot_ui"),
                                  width = 12
                           )
                ))
      ),
      # ---- volcano plot ----
      tabItem(tabName = "volcano_plot",
              fluidRow(
                column(width = 12,
                       scroll_box(title = "Volcano Plot Settings", status = "primary",
                                  selectInput("vol_var1", "Select Grouping Variable:", choices = NULL),
                                  selectInput("vol_var2", "Select Comparison Variable:", choices = NULL),
                                  numericInput("p_value_threshold", "P value Threshold:", value = 0.05, min = 0, max = 1, step = 0.01),
                                  numericInput("fc_threshold", "Fold Change Threshold:", value = 1, min = 0, step = 0.01),
                                  p("Fold change was defined as the difference in mean lipid abundance between groups rather than a ratio. 
                                    This is because some normalized and transformed values included negative numbers, 
                                    making ratio-based fold change inappropriate."),
                                  checkboxInput("Adj_y", "Adjusted p value on the Y axis", value = F),
                                  actionButton("generate_volcano_plot", "Generate Volcano Plot"),
                                  width = 12),
                       scroll_box(title="Pixel size settings for downloading plots", status = "primary",
                                  numericInput("width_volcano", "Width (in inches):", value = 10, step = 0.5),
                                  numericInput("height_volcano", "Height (in inches):", value = 6, step = 0.5),
                                  sliderInput("DPI_volcano","Select the dots per inch (DPI) for downloading plot:",
                                              min = 60, max =600,value=100,step=10),
                                  uiOutput("pixel_info_volcano"),
                                  width = 12,collapsed = T
                       ),
                       scroll_box(title = "Volcano Plot Output", status = "info",
                                  downloadButton("download_volcano_plot", "Download Volcano Plot (.png)"),
                                  uiOutput("volcano_plotUI"),
                                  width = 12),
                       scroll_box(title = "Volcano Plot Data", status = "info",
                                  downloadButton("download_volcano_data", "Download Volcano Data (.csv)"),
                                  withSpinner(DT::dataTableOutput("volcanoDataPreview")),
                                  width = 12)
                ))
      ),
      # ---- PCA ----
      tabItem(tabName ="pca",
              fluidRow(
                column(width = 12,
                       scroll_box(title = "PCA Plot Settings", status = "primary",
                                  # checkboxInput('scale_data', 'Scale Data', value = TRUE),
                                  checkboxInput("pca_3d", "3D PCA Plot", value = F),
                                  actionButton("generate_pca_plot", "Generate PCA Plot"),
                                  width = 12),
                       scroll_box(title="Pixel size settings for downloading PCA", status = "primary",
                                  numericInput("width_PCA", "Width (in inches):", value = 10, step = 0.5),
                                  numericInput("height_PCA", "Height (in inches):", value = 6, step = 0.5),
                                  sliderInput("DPI_PCA","Select the dots per inch (DPI):",
                                              min = 60, max =600,value=100,step=10),
                                  uiOutput("pixel_info_PCA"),
                                  width = 12,collapsed = T
                       ),
                       scroll_box(title = "PCA Plot Output", status = "info",
                                  downloadButton("download_pca_plot", "Download PCA plot"),
                                  uiOutput("PCAPlotUI"),
                                  width = 12)
                ))
      ),
      # ---- heatmap hirechical ----
      tabItem(tabName = 'heatmaphcl',
              fluidRow(
                column(width = 12,
                       scroll_box(title = "Heatmap Settings", status = "primary",
                                  checkboxInput('cluster_rows', 'Cluster Group Labels', value = TRUE),
                                  checkboxInput('cluster_cols', 'Cluster Lipid Features', value = TRUE),
                                  actionButton("generate_heatmaphcl", "Generate Heatmap"),
                                  width = 12),
                       scroll_box(title = "Heatmap Output", status = "info",
                                  downloadButton("download_hcl_plot", "Download Heatmap"),
                                  withSpinner(plotlyOutput("HeatmapPlot", height = "600px", width = "1200px")),
                                  width = 12)
                )
        
      )),
      
      # ---- Statistical Tests ----
      
      # ---- Mean Calculator ----
      tabItem(tabName = 'mean_cal',
              fluidRow(column(width = 12,
                              scroll_box(
                                title = "Lipidomics Mean Calculator", 
                                status = "primary",
                                width = 12,  # <-- width goes **inside** the scroll_box
                                actionButton("run_mean_cal", "Calculate Mean")
                              )
              )),
              fluidRow(column(width = 12,
                              scroll_box(
                                title = "Mean Calculated Data Preview", 
                                status = "info", 
                                downloadButton("download_mean_calculated", "Download CSV"),
                                withSpinner(DT::dataTableOutput("Mean_Calculated_DataPreview")),
                                width = 12
                              )
              ))
      ),
      # ---- T test ----
      tabItem(tabName = "ttest",
              fluidRow(column(width = 12,
                              scroll_box(
                                title = "T-test Settings", 
                                status = "primary",
                                width = 12,  # <-- width goes **inside** the scroll_box
                                selectInput("ttest_var1", "Select First Group Variable for T-test:",
                                            choices = NULL),  # update dynamically
                                selectInput("ttest_var2", "Select Second Group Variable for T-test:",
                                            choices = NULL),  # update dynamically
                                selectInput("ttest_method", "Select T-test method:",
                                            choices =list(
                                              "Welch’s t-test (unequal variances allowed)"="welch",
                                              "Student’s t-test (equal variances assumed)"="student")),
                                actionButton("run_ttest", "Run T-test")
                              )
              )),
              fluidRow(column(width = 12,
                              scroll_box(
                                title = "T-test Results", 
                                status = "info", 
                                downloadButton("download_ttest_results", "Download CSV"),
                                withSpinner(DT::dataTableOutput("Ttest_Results_Preview")),
                                width = 12
                              )
              )),
              fluidRow(column(width = 12,
                              scroll_box(
                                title = "T-test PCA Plot ", 
                                status = "info",
                                selectInput("ttest_pca_pval", "Select:",
                                            choices =list(
                                              "Significancy based on adjusted p values < 0.05"="padj",
                                              "Significancy based on nominal p values < 0.05"="pnominal")),
                                actionButton("ttest_pca_checkbox", "Run PCA on Significant T-test Features"),
                                withSpinner(plotlyOutput("ttestPCAPlot", height = "600px", width = "800px")),
                                downloadButton("download_ttest_PCA_plot", "Download T-test PCA Plot (.png)"),
                                width = 12
                              )
              ))
      ),
      # ---- anova ----
      tabItem(tabName = 'anova',
              fluidRow(column(width = 12,
                              scroll_box(
                                title = "ANOVA Settings", 
                                status = "primary",
                                width = 12,  # <-- width goes **inside** the scroll_box
                                actionButton("run_anova", "Run ANOVA")
                                
                              )
              )),
              fluidRow(column(width = 12,
                              scroll_box(
                                title = "ANOVA Results", 
                                status = "info", 
                                downloadButton("download_anova_results", "Download CSV"),
                                withSpinner(DT::dataTableOutput("ANOVA_Results_Preview")),
                                width = 12
                              )
              )),
              fluidRow(column(width = 12,
                              scroll_box(
                                title = "ANOVA PCA Plot ", 
                                status = "info", 
                                selectInput("anova_pca_pval", "Select:",
                                            choices =list(
                                              "Significancy based on adjusted p values < 0.05"="padj",
                                              "Significancy based on nominal p values < 0.05"="pnominal")),
                                actionButton("anova_pca_checkbox", "Run PCA on Significant ANOVA Features"),
                                withSpinner(plotlyOutput("anovaPCAPlot", height = "600px", width = "800px")),
                                downloadButton("download_anova_PCA_plot", "Download ANOVA PCA Plot (.png)"),
                                width = 12
                              )
              ))
      ),
      # ---- correlation ----
      tabItem(tabName = 'correlation',
              fluidRow(column(width = 12,
                              scroll_box(
                                title = "Correlation", 
                                status = "primary",
                                width = 12,  # <-- width goes **inside** the scroll_box
                                selectInput("color_setting", "Select the color for the plot:",
                                            choices = list(
                                              'Default: Red/Blue' = "RWB",
                                              'Blue/Green/Yellow' = "BGY",
                                              'Orange/Blue' = "OWB",
                                              'Yellow/Blue' = "YWB",
                                              'Grey Scale' = "greyscale",
                                              'Viridis color'= 'viridis',
                                              "Heat color" = 'heat'
                                            )),
                                checkboxInput("control_thres", 
                                              "Enable correlation threshold", 
                                              value = T),
                                conditionalPanel(
                                  condition = "input.control_thres == true",
                                  sliderInput("correlation_thres", 
                                              "Select the correlation threshold. Correlations within the range (e.g., -0.9 to 0.9) will be filtered out:",
                                              min = -1, max = 1 , value = c(-0.8,0.8), step = 0.01, 
                                              width="50%")),
                                checkboxInput("control_z", 
                                              "Filter correlations that are not significantly different to zero by Fisher z-test", 
                                              value = FALSE),
                                actionButton("run_corr", "Run correlation")
                              )
              )),
              fluidRow(column(width = 12,
                              scroll_box(
                                title = "Correlation heatmap", 
                                status = "info", 
                                downloadButton("download_corr_results", "Download Zip Files for Correlations Table"),
                                downloadButton("download_corr_heatmap", "Download Zip Files for Correlation Heatmap"),
                                uiOutput("corr_heatmaps"),
                                width = 12
                              )
              ))
      ),
      
      # ---- DSPC ----
      tabItem(
        tabName = "DSPC",
        fluidRow(
          column(
            width = 4,
            scroll_box(
              title = "DSPC Network Settings",
              status = "primary",
              width = 12,
              p("DSPC (Debiased Sparse Partial Correlation) is a statistical 
              framework designed to infer sparse and interpretable molecular 
              networks from high-dimensional omics data such as lipidomics.
              It estimates partial correlations while correcting for the bias 
              introduced by regularization (e.g., graphical lasso), allowing 
              more reliable detection of direct molecular relationships."),
              actionButton("help_btn_dspc", "More Information", icon = icon("question-circle")),
              numericInput("max_cluster_size", "Max nodes per cluster", value = 20, min = 5, max = 200),
              numericInput("min_nodes", "Minimum nodes to keep in one cluster", value = 5, min = 2, max = 100),
              
              actionButton("run_dspc", "Generate DSPC Network"),
              downloadButton("download_dspc", "Download DSPC Data (.csv)"),
              br(),
              withSpinner(uiOutput("subnetwork_list"))   # dynamically show cluster selection
            )),
          column(
            width = 8,
            scroll_box(
              title = "DSPC Network Output",
              status = "info",
              solidHeader = TRUE,
              width = 12,
              withSpinner(visNetwork::visNetworkOutput("DSPCPlot", height = "650px"))
            )
          )
        )
      ),
      # ---- PLS - DA ----
      tabItem(
        tabName = "plsda",
        fluidRow(
          column(
            width = 12,
            scroll_box(
              title = "PLS-DA Model Overview",
              status = "primary",
              width = 12,
              p(HTML("Partial Least Squares Discriminant Analysis (PLS-DA) is a supervised multivariate statistical method used to model the relationship between lipidomic data and predefined categorical groups. <br>
                     PLS-DA identifies latent variables that maximize the covariance between the lipid features and the group labels, enabling effective classification and discrimination of samples based on their lipid profiles. <br>
                     This technique is particularly useful for biomarker discovery, as it highlights the most relevant features contributing to group separation while handling complex, high-dimensional datasets typical in lipidomics studies.<br>"))
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            scroll_box(
              title = "PLS-DA Model Settings",
              status = "primary",
              width = 12,
              numericInput("plsda_n_perm", "Number of Permutations:", 
                           value = 20, min = 10, step = 10),
              numericInput("plsda_n_cv", "Number of Cross-validations:", 
                           value = 7, min = 2, step = 1),
              actionButton("run_plsda", "Run PLS-DA Model")
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            scroll_box(
              title = "PLS-DA Model Output",
              status = "primary",
              width = 12,
              # show model summary first
              fluidRow(
                column(width = 6,
                       withSpinner(DT::dataTableOutput("PLSDA_ModelSummary"))
                       )),
              
              # ROW 1 
              fluidRow(
                column(
                  width = 6,
                  withSpinner(plotOutput("PLSDA_overviewPlot", height = "600px")),
                  downloadButton("download_plsda_overviewplot", "Download Overview (.png)")
                ),
                column(
                  width = 6,
                  withSpinner(plotOutput("PLSDA_permutationPlot", height = "600px")),
                  downloadButton("download_plsda_permutationplot", "Download Permutation (.png)")
                )
              ),
              
              tags$hr(),
              
              # ROW 3 
              fluidRow(
                column(
                  width = 6,
                  selectInput("plsda_color_scheme", "Select Color Scheme for VIP Plot:",
                              choices = list(
                                "Viridis" = "viridis",
                                "Cividis" = "cividis",
                                "Plasma" = "plasma",
                                "Magma"  = "magma",
                                "Inferno" = "inferno",
                                "Teal gradient" = "Teal gradient",
                                "Purple Pink Orange gradient" = "Purple Pink Orange gradient",
                                "Mint Teal Blue gradient" = "Mint Teal Blue gradient"
                              )),
                  withSpinner(plotlyOutput("PLSDA_VIPPlot", height = "600px")),
                  downloadButton("download_plsda_vipplot", "Download VIP Plot (.png)")
                ),
                column(
                  width = 6,
                  withSpinner(DT::dataTableOutput("PLSDA_VIPTable")),
                  downloadButton("download_plsda_viptable", "Download VIP Table (.csv)")
                ))
              )))
        

      ),
      
      # ---- OPLS - DA ----
      tabItem(
        tabName = "oplsda",
        fluidRow(
          column(
            width = 12,
            scroll_box(
              title = "OPLS-DA Model Overview",
              status = "primary",
              width = 12,
              tagList(
                p(
                  "Orthogonal Partial Least Squares Discriminant Analysis (OPLS-DA) 
                  is a supervised multivariate statistical method used to identify 
                  differences between predefined groups in complex datasets."
                ),
                
                p(
                  "Unlike PLS-DA, OPLS-DA explicitly separates the variation in the data into:"
                ),
                
                tags$ul(
                  tags$li(
                    strong("Predictive components: "),
                    "directly associated with the group labels."
                  ),
                  tags$li(
                    strong("Orthogonal components: "),
                    "capture systematic variation unrelated to group differences (e.g., technical variation or biological heterogeneity)."
                  )
                )
              )
              
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            scroll_box(
              title = "OPLS-DA Model Settings",
              status = "primary",
              width = 12,
              numericInput("oplsda_n_perm", "Number of Permutations:", 
                           value = 20, min = 10, step = 10),
              numericInput("oplsda_n_cv", "Number of Cross-validations:", 
                           value = 7, min = 2, step = 1),
              # selectInput("oplsda_scale_method", "Select Scaling Method:",
              #             choices = list(
              #               'None' = "none",
              #               'Standard Scaling' = "standard",
              #               'Mean Centered' = "center",
              #               'Pareto Scaling' = "pareto"
              #             ),
                          # selected = "standard"),
              actionButton("run_oplsda", "Run OPLS-DA Model")
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            scroll_box(
              title = "OPLS-DA Model Output",
              status = "primary",
              width = 12,
              # show model summary first
              fluidRow(
                column(width = 6,
                  withSpinner(DT::dataTableOutput("OPLSDA_ModelSummary"))
                              
                              )),
              
              # ROW 1 
              fluidRow(
                column(
                  width = 6,
                  withSpinner(plotOutput("OPLSDA_overviewPlot", height = "600px")),
                  downloadButton("download_oplsda_overviewplot", "Download Overview (.png)")
                ),
                column(
                  width = 6,
                  withSpinner(plotOutput("OPLSDA_permutationPlot", height = "600px")),
                  downloadButton("download_oplsda_permutationplot", "Download Permutation (.png)")
                )
              ),
              
              tags$hr(),
              
              # ROW 2 
              fluidRow(
                column(
                  width = 6,
                  withSpinner(plotOutput("OPLSDA_ScorePlot", height = "600px")),
                  downloadButton("download_oplsda_scoreplot", "Download Score Plot (.png)")
                ),
                column(
                  width = 6,
                  withSpinner(plotOutput("OPLSDA_outlierPlot", height = "600px")),
                  downloadButton("download_oplsda_outlierplot", "Download Outlier Plot (.png)")
                )
              ),
              
              tags$hr(),
              
              # ROW 3 
              fluidRow(
                column(
                  width = 6,
                  selectInput("oplsda_color_scheme", "Select Color Scheme for VIP Plot:",
                              choices = list(
                                "Viridis" = "viridis",
                                "Cividis" = "cividis",
                                "Plasma" = "plasma",
                                "Magma"  = "magma",
                                "Inferno" = "inferno",
                                "Teal gradient" = "Teal gradient",
                                "Purple Pink Orange gradient" = "Purple Pink Orange gradient",
                                "Mint Teal Blue gradient" = "Mint Teal Blue gradient"
                              )),
                  withSpinner(plotlyOutput("OPLSDA_VIPPlot", height = "600px")),
                  downloadButton("download_oplsda_vipplot", "Download VIP Plot (.png)")
                ),
                column(
                  width = 6,
                  withSpinner(DT::dataTableOutput("OPLSDA_VIPTable")),
                  downloadButton("download_oplsda_viptable", "Download VIP Table (.csv)")
                )
              )
              )))),
      # ---- Random Forest ----
      tabItem(tabName ="rf",
              fluidRow(
                column(width = 12,
                       scroll_box(title = "Random Forest Model Settings", status = "primary",
                                  numericInput("rf_n_trees", "Number of Trees:", 
                                               value = 200, min = 50, step = 100),
                                  numericInput("data_partition"," Data Partition (Training Set %):", 
                                               value = 70, min = 50, max = 90, step = 5),
                                  actionButton("run_rf", "Run Random Forest Model"),
                                  width = 12),
                       scroll_box(title = "Random Forest Model Output", status = "info",
                                  width = 12,
                                  # show model summary first
                                  fluidRow(
                                    column(width = 6,
                                           DT::dataTableOutput("RF_ModelSummary")),
                                    column(
                                      width = 6,
                                      withSpinner(DT::dataTableOutput("RF_ImportanceTable")),
                                      downloadButton("download_rf_importancetable", "Download Importance Table (.csv)")
                                    )
                                    ),
                                  
                                  # ROW 1 
                                  fluidRow(
                                    column(
                                      width = 12,
                                      selectInput("rf_color_scheme", "Select Color Scheme for Importance Plot:",
                                                  choices = list(
                                                    "Viridis" = "viridis",
                                                    "Cividis" = "cividis",
                                                    "Plasma" = "plasma",
                                                    "Magma"  = "magma",
                                                    "Inferno" = "inferno",
                                                    "Teal gradient" = "Teal gradient",
                                                    "Purple Pink Orange gradient" = "Purple Pink Orange gradient",
                                                    "Mint Teal Blue gradient" = "Mint Teal Blue gradient"
                                                  )),
                                      numericInput("rf_top_n", "Number of Top Important Features to Display:", 
                                                    value = 20, min = 5, step = 5),
                                      withSpinner(plotlyOutput("RF_ImportancePlot", height = "600px")),
                                      downloadButton("download_rf_importanceplot", "Download Importance Plot (.html)")
                                    )
                                    
                                  )
                       ))
              )),
     # ---- Download ----
      tabItem(tabName = "download",
              h2("Download All Generated Files"),              
              p("Thank you for using our lipidomics data analysis tool! If you have any feedback or encounter any issues, please don't hesitate to contact us."),
              p("For support, please contact: lipidanalyst-requests@umich.edu"),
              p("Click the button below to download all output files as a zip archive."),
              downloadButton("download_zip_data", "Download All Data (ZIP)"),
              downloadButton("download_zip_plots", "Download All Plots (ZIP)")

            
      )
    )
  )
)

