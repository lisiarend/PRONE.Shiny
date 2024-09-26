uploadBody <- function() {
  box_height <- "20em"
  
  exampleDataVector <- c( "Li et al. on hPDLCs (TMT)" = "data/li_et_al.rds",
                          "Vehmas et al. on AROM + mice (LFQ)" = "data/mouse_liver_cytochrome_P450_se.rds"
                        )
  
  exampleDataDescription <- c("Li et al. on hPDLCs (TMT)" = "Li et al.",
                              "Vehmas et al. on AROM + mice (LFQ)" = "Vehmans et al.")

  
  dataExampleBox <- fluidRow(
    width=12,
    style="margin-right: 0px; margin-left: 0px;",
    selectizeInput(
      "exampleData",
      "Choose Example Dataset",
      exampleDataVector,
      width="100%",
      options = list(
        placeHolder = "Select Example Dataset...",
        onInitialize = I("function() {this.setValue(''); }")
      )
    ),
    bsButton("loadDataExample",
             "Load Data",
             icon("database"),
             style = "success",
             block = TRUE,
             disabled = TRUE,
    ),
  )
  
  seFilePopover <- 
    bsPopover(
      id = "seFileInfo",
      title = "A .rds file containing the SummarizedExperiment object.",
      content = "If you downloaded your data once before on the app as SummarizedExperiment, you can easily continue your analysis uploading the .rds file."
    )
  
  dataSeBox <- fluidRow(
    width=12,
    style="margin-right: 0px; margin-left: 0px;",
    fileInput(
      "seFile",
      span("Choose Summarized Experiment File (.rds)", tags$i(class = "fa fa-circle-info", style = "color: rgb(0,166,90)"), id = "seFileInfo"),
      width="100%",
      multiple = FALSE,
      buttonLabel = "Browse...",
      accept = c(".rds"),
      placeholder = "No file selected",
    ),
    seFilePopover,
    bsButton("loadDataSE",
             "Load Data",
             icon("database"),
             style = "success",
             block = TRUE,
             disabled = TRUE,
    ),
  )

  
  metaFilePopover <- 
    bsPopover(
      id = "metaFileInfo",
      title = "A CSV or Txt file with experimental design.",
      content = "The metadata file must contain a column specifying the names of the samples. Additionally, it can contain for example a column representing the batches, the conditions to analyze in DE, etc.</b>"
    )
  
  dataFilePopover <- 
    bsPopover(
      id = "dataFileInfo",
      title = "A CSV or Txt file with the proteomics data.",
      content = "A file containing the RAW intensities of proteins for multiple samples (samples specified in metadata file). For example, the output file proteinGroups.txt of MaxQuant.</b>"
    )
  
  columnNamePopover <- 
    bsPopover(
      id = "columnNameInfo",
      title = "The name of the column of the metadata file containing the samples of the proteomics data file to analyze.",
      content = "All samples of this column of the metadata file need to be in the proteomics data file.</b>"
    )
  
  
  proteinIDsPopover <- 
    bsPopover(
      id = "protIDsInfo",
      title = "The name of the proteomics data file column containing the protein IDs.",
      content = "This column should be unique and not contain any empty cells.</b>"
    )
  
  geneNamesPopover <- 
    bsPopover(
      id = "geneNamesInfo",
      title = "The name of the proteomics data file column containing the gene names.",
      content = "This column should be unique and not contain any empty cells. If it contains any empty cells, we will fill these with the names of the protein IDs. If there are duplicated gene names entries, we will just append the protein groups to make the entries unique.</b>"
    )
  
  refBoolPopover <- 
    bsPopover(
      id = "refBoolInfo",
      title = "Does your proteomics data contains some reference samples?",
      content = "e.g. a TMT experiment can be run by adding an internal reference sample per batch.</b>"
    )
  
  refSamplesPopover <- 
    bsPopover(
      id = "refSamplesInfo",
      title = "Select the samples names of the internal reference samples.",
      content = "e.g. a TMT experiment can be run by adding an internal reference sample per batch. Please select the names of these reference samples. These need to be specified for certain normalization methods.</b>"
    )
  
  batchBoolPopover <- 
    bsPopover(
      id = "batchBoolInfo",
      title = "Was your proteomics data measured in batches??",
      content = "e.g. a TMT experiment can be run in different batches.</b>"
    )
  
  batchColumnPopover <- 
    bsPopover(
      id = "batchColumnInfo",
      title = "Select the column in the metadata that contains the batch labels.",
      content = "The batch effect correction methods need the name of the column representing the batches.</b>"
    )
  
    
  
  dataOwnBox <- fluidRow(
    width=12,
    column(
      width = 6,
      fileInput(
        "metaFile",
        span("Choose Metadata File ", tags$i(class = "fa fa-circle-info", style = "color: rgb(0,166,90)"), id = "metaFileInfo"),
        width="100%",
        multiple = FALSE,
        buttonLabel = "Browse...",
        accept = c(".csv", ".txt"),
        placeholder = "No file selected",
      ),
      metaFilePopover
    ),
    column(
      width = 6,
      fileInput(
        "dataFile",
        span("Choose Proteomics Data File (Raw)", tags$i(class = "fa fa-circle-info", style = "color: rgb(0,166,90)"), id = "dataFileInfo"),
        width = "100%",
        multiple = FALSE,
        accept = c(".csv", ".txt"),
        buttonLabel = "Browse...",
        placeholder = "No file selected",
      ),
      dataFilePopover
    ),
    column(
      width = 6,
      pickerInput(
        "columnName",
        span("Choose Column of Metadata Representing Sample in Data ",tags$i(class = "fa fa-circle-info", style = "color: rgb(0,166,90)"), id = "columnNameInfo"),
        choices = NULL,
        multiple = FALSE,
        width="100%",
      ),
      columnNamePopover
    ),
    column(
      width = 3,
      pickerInput(
        "protIDsColumn",
        span("Select Column Name of Protein IDs ",tags$i(class = "fa fa-circle-info", style = "color: rgb(0,166,90)"), id = "protIDsInfo"),
        choices = NULL,
        multiple = FALSE,
        width="100%",
      ),
      proteinIDsPopover
    ),
    column(
      width = 3,
      pickerInput(
        "geneNamesColumn",
        span("Select Column Name of Gene Names ",tags$i(class = "fa fa-circle-info", style = "color: rgb(0,166,90)"), id = "geneNamesInfo"),
        choices = NULL,
        width="100%",
        multiple = FALSE,
      ),
      geneNamesPopover
    ), 
    column(
      width = 6,
      div(style = "margin-top:20px"),
      materialSwitch(
        "batchBool",
        value = TRUE,
        label = span(HTML("<b>Do You Have Batches?</b>"), tags$i(class = "fa fa-circle-info", style = "color: rgb(0,166,90)"), id = "batchBoolInfo"),
        status = "success",
        inline = TRUE
      ),
      batchBoolPopover
    ),
    column(
      width = 6, 
      pickerInput(
        "batchColumn",
        span("Select the Batch Column", tags$i(class = "fa fa-circle-info", style = "color: rgb(0,166,90)"), id = "batchColumnInfo"),
        choices = NULL,
        width="100%",
        multiple = FALSE
      ),
      batchColumnPopover
    ),
    column(
      width = 6,
      div(style = "margin-top:20px"),
      materialSwitch(
        "refSamplesBool",
        value = TRUE,
        label = span(HTML("<b>Do You Have Reference Samples?</b>"), tags$i(class = "fa fa-circle-info", style = "color: rgb(0,166,90)"), id = "refBoolInfo"),
        status = "success",
        inline = TRUE
      ),
      refBoolPopover
    ),
    column(
      width = 6, 
      pickerInput(
        "refSamples",
        span("Select Reference Samples", tags$i(class = "fa fa-circle-info", style = "color: rgb(0,166,90)"), id = "refSamplesInfo"),
        choices = NULL,
        width="100%",
        multiple = TRUE,
        options = list("actions-box" = TRUE,
                       "size" = 5)
      ),
      refSamplesPopover
    ),
    column(
      width = 12,
      bsButton("loadDataOwn",
               "Load Data",
               icon("database"),
               style = "success",
               block = TRUE,
               disabled = TRUE,
               
      ),
    )
  )

  uploadBody <- tabItem(tabName = "upload",
                        fluidRow(shinydashboard::box(
                          div("Please upload your data or simply use an example dataset."),
                          title = h2("Upload Data"),
                          width = 12
                        )),
                        
                        fluidRow(
                          tabBox(
                            tabPanel(
                              fluidRow(dataOwnBox, 
                                       width = 12,
                                       style="margin-right: 0px; margin-left: 0px;"),
                              value = "dataOwn",
                              title = "Upload Data",
                            ),
                            tabPanel(
                              fluidRow(dataSeBox, 
                                       width = 12,
                                       style="margin-right: 0px; margin-left: 0px;"),
                              value = "dataExample",
                              title = "Upload SE",
                            ),
                            tabPanel(
                              fluidRow(dataExampleBox, 
                                       width = 12,
                                       style="margin-right: 0px; margin-left: 0px;"),
                              value = "dataExample",
                              title = "Example Data",
                            ),
                          ,
                          id = "chooseDataTab",
                          title = "Choose Data",
                          width = 12)
                        ),
                        
                        fluidRow(
                          column(
                            width = 12,
                            shinycssloaders::withSpinner(uiOutput("currentData"))
                          )
                        )
   
  )
  return(uploadBody)
  
}
