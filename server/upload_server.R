
################################## Helper Functions ##################################

checkNullTable <- function(toCheck) {
  if (is.null(toCheck))
    return(data.frame("Nothing" = ""))
  else
    return(toCheck)
}

loadData <- function(data, md, metadata_column, protein_column, gene_column, ref_samples_bool, ref_samples, batch_bool, batch_column){
  # check if all samples of metadata are in data column
  # check names
  samples <- make.names(md[,get(metadata_column),])
  md$Column <- samples
  # check if in colnames of data V1
  if("V1" %in% colnames(data)){
    data$V1 <- NULL
  }
  if(!ref_samples_bool){
    ref_samples <- NULL
  }
  if(!batch_bool){
    batch_column <- NULL
  }
  se <- PRONE.R::load_data(data, md, protein_column = protein_column, gene_column = gene_column, ref_samples = ref_samples, batch_column = batch_column, condition_column = NULL, label_column = NULL)
  return(se)
}

################################## Observer ##################################

observeEvent(input$seFile, {
  if(is.null(input$seFile)){
    updateButton(session, "loadDataSE", label = " Load Data", disabled = TRUE)
  } else if(endsWith(input$seFile$datapath, ".rds")){
    reactiveVals$data$se <- readRDS(input$seFile$datapath)
    updateButton(session, "loadDataSE", label = " Load Data", disabled = FALSE)
  } else {
    updateButton(session, "loadDataSE", label = " Load Data", disabled = TRUE)
  }
}, ignoreNULL = FALSE)

observeEvent(input$metaFile, {
  if(endsWith(input$metaFile$datapath, ".csv") | endsWith(input$metaFile$datapath, ".txt")){
    reactiveVals$data$upload$md <- data.table::fread(input$metaFile$datapath, check.names = TRUE)
    updatePickerInput(session, "columnName", choices = colnames(reactiveVals$data$upload$md), selected = "")
    updatePickerInput(session, "batchColumn", choices = colnames(reactiveVals$data$upload$md), selected = "")
  }
})

observeEvent(input$columnName, {
  md <- reactiveVals$data$upload$md
  column <- input$columnName
  samples <- md[,..column]
  updatePickerInput(session, "refSamples", choices = samples, selected = "")
})

observeEvent(input$dataFile, {
  if(endsWith(input$dataFile$datapath, ".csv") | endsWith(input$dataFile$datapath, "txt")){
    reactiveVals$data$upload$data <- data.table::fread(input$dataFile$datapath, check.names = TRUE)
    updatePickerInput(session, "protIDsColumn", selected = "", choices = colnames(reactiveVals$data$upload$data))
    updatePickerInput(session, "geneNamesColumn", selected = "", choices = c("No Gene Name Column", colnames(reactiveVals$data$upload$data)))
  }
})

observeEvent(input$batchBool,{
  if(input$batchBool){
    shinyjs::show("refSamplesBool")
    shinyjs::show("refSamples")
    shinyjs::show("batchColumn")
  } else {
    updatePickerInput(session, "refSamples", selected = "")
    shinyjs::hide("refSamplesBool")
    shinyjs::hide("refSamples")
    shinyjs::hide("batchColumn")
  }
})

observeEvent(input$refSamplesBool,{
  if(input$refSamplesBool){
    shinyjs::show("refSamples")
  } else {
    shinyjs::hide("refSamples")
    updatePickerInput(session, "refSamples", selected = "")
  }
})

observe({
  md <- !is.null(reactiveVals$data$upload$md)
  data <- !is.null(reactiveVals$data$upload$data)
  column <- !is.null(input$columnName)
  protids <- !is.null(input$protIDsColumn)
  geneNames <- !is.null(input$geneNamesColumn)
  refs <- input$refSamplesBool
  batches <- input$batchBool
  if(batches){
    batches <- !is.null(input$batchColumn)
    if(refs){
      refs <- !is.null(input$refSamples)
    } else {
      refs <- TRUE
    }
  } else {
    batches <- TRUE
    refs <- TRUE
  }
  if(md & data & column & protids & geneNames & refs & batches){
    updateButton(session, "loadDataOwn", disabled = FALSE)
  } else {
    updateButton(session, "loadDataOwn", disabled = TRUE)
  }
})

observeEvent(input$exampleData, {
  if (input$exampleData != ""){
    updateButton(session, "loadDataExample", label = " Load Data", disabled = FALSE)
  } else {
    updateButton(session, "loadDataExample", label = " Load Data", disabled = TRUE)
  }
})

observeEvent(input$loadDataExample, {
  # reset reactiveVals
  ids <- reactiveVal()
  ids(NULL)
  updateButton(session, "loadDataExample", label = " Loading...", disabled = TRUE)
  status <- "warning"
  if(input$exampleData != ""){
    se <- readRDS(input$exampleData)
  }
  reactiveVals$se <- se
  updatePickerInput(session = session, inputId = "filterProteinsColumn", choices = colnames(rowData(se))) # only show those columns that are not the intensities
  updatePickerInput(session = session, inputId = "pomaGroup", choices = colnames(colData(se))[colnames(colData(se)) != "Column"])
  updatePickerInput(session = session, inputId = "removeSamplesColumn", choices = colnames(colData(se)), selected = colnames(colData(se))[1])
  updatePickerInput(session = session, inputId = "removeSamples", choices = colData(se)[[colnames(colData(se))[1]]])
  updatePickerInput(session = session, inputId = "normInput", choices = names(assays(se))[names(assays(se))!= "raw"])
  updatePickerInput(session = session, inputId = "normVisAin", choices = names(assays(se))[names(assays(se)) != "raw"], selected = names(assays(se))[names(assays(se)) != "raw"])
  updatePickerInput(session = session, inputId = "deNormInput", choices = names(assays(se))[names(assays(se)) != "raw"])
  updatePickerInput(session = session, inputId = "deColComparison", choices = colnames(colData(se))[colnames(colData(se)) != "Column"])
  updateSelectizeInput(session = session, inputId = "NAheatmap_color", choices = colnames(colData(se)), selected = colnames(colData(se))[2])
  # check unique columns (for labeling rows of heatmap)
  cols <- sapply(as.data.table(colData(se)), function(x) if(length(unique(x)) == length(x)) return(TRUE) else return(FALSE))
  label_options <- colnames(colData(se))[cols]
  updateSelectizeInput(session = session, inputId = "NAheatmap_label", choices = label_options, selected = label_options[1])
  # only those columns with at least one sample group with more than 1 value
  choices <- colnames(colData(se))[sapply(as.data.table(colData(se)), function(x) {
    if(length(unique(x)) < length(x)) return(TRUE) else FALSE
  })]
  updatePickerInput(session = session, inputId = "normVisCondition", choices = choices)



  reactiveVals$se_filtered <- se
  reactiveVals$curr_NA_thr <- 0
  status <- "success"
  if(status == "success"){
    upload_tab <- which(tab_ids == "upload")
    if (isolate(reactiveVals$max_tab) > upload_tab)
      reactiveVals$max_tab <- upload_tab
    reactiveVals$continue[upload_tab] <- TRUE
  } else if (status=="error"){
    shinyalert::shinyalert("The selected column does not match with the column names of the uploaded data!", type ="error")
  }
  updateButton(session, "loadDataExample", label = " Load Data", disabled = FALSE)

  # check if empty gene names --> if then show ProHarMeD alert
  rowdata <- as.data.table(rowData(se))
  empty_gn <- sum(rowdata$Gene.Names == "")
  na_gn <- sum(is.na(rowdata$Gene.Names))
  if((empty_gn != 0) | (na_gn != 0)){
    proharmed_alert <-  shinyalert(
      inputId = "confirmProharmedExample",
      title = "Empty Gene Names Found !",
      text = div(
        HTML("To remap gene names and filter protein IDs, go check "),
        HTML('<img src="ProHarMeD_Logo.png" width="250px" />'),
      ),
      size = "s",
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = TRUE,
      type = "warning",
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "Visit ProHarMeD",
      confirmButtonCol = "#53A93F",
      cancelButtonText = "Cancel",
      timer = 10000/2,
      imageUrl = "",
      animation = TRUE
    )
  }

})

observeEvent(input$loadDataSE, {
  # reset reactiveVals
  ids <- reactiveVal()
  ids(NULL)
  updateButton(session, "loadDataSE", label = " Loading...", disabled = TRUE)
  status <- "warning"
  if(!is.null(reactiveVals$data$se)){
    se <- reactiveVals$data$se
  }
  reactiveVals$se <- se

  updatePickerInput(session = session, inputId = "filterProteinsColumn", choices = colnames(rowData(se))) # only show those columns that are not the intensities
  updatePickerInput(session = session, inputId = "pomaGroup", choices = colnames(colData(se))[colnames(colData(se)) != "Column"])
  updatePickerInput(session = session, inputId = "removeSamplesColumn", choices = colnames(colData(se)), selected = colnames(colData(se))[1])
  updatePickerInput(session = session, inputId = "removeSamples", choices = colData(se)[[colnames(colData(se))[1]]])
  updatePickerInput(session = session, inputId = "normInput", choices = names(assays(se))[names(assays(se))!= "raw"])
  updatePickerInput(session = session, inputId = "normVisAin", choices = names(assays(se))[names(assays(se)) != "raw"], selected = names(assays(se))[names(assays(se)) != "raw"])
  updatePickerInput(session = session, inputId = "deNormInput", choices = names(assays(se))[names(assays(se)) != "raw"])
  updatePickerInput(session = session, inputId = "deColComparison", choices = colnames(colData(se))[colnames(colData(se)) != "Column"])
  updateSelectizeInput(session = session, inputId = "NAheatmap_color", choices = colnames(colData(se)), selected = colnames(colData(se))[2])
  # check unique columns (for labeling rows of heatmap)
  cols <- sapply(as.data.table(colData(se)), function(x) if(length(unique(x)) == length(x)) return(TRUE) else return(FALSE))
  label_options <- colnames(colData(se))[cols]
  updateSelectizeInput(session = session, inputId = "NAheatmap_label", choices = label_options, selected = label_options[1])
  updatePickerInput(session = session, inputId = "normVisCondition", choices = colnames(colData(se))[colnames(colData(se)) != "Column"])

  reactiveVals$se_filtered <- se
  reactiveVals$curr_NA_thr <- 0
  status <- "success"
  if(status == "success"){
    upload_tab <- which(tab_ids == "upload")
    if (isolate(reactiveVals$max_tab) > upload_tab)
      reactiveVals$max_tab <- upload_tab
    reactiveVals$continue[upload_tab] <- TRUE
  } else if (status=="error"){
    shinyalert::shinyalert("The selected column does not match with the column names of the uploaded data!", type ="error")
  }
  # check if empty gene names --> if then show ProHarMeD alert
  rowdata <- as.data.table(rowData(se))
  empty_gn <- sum(rowdata$Gene.Names == "")
  na_gn <- sum(is.na(rowdata$Gene.Names))
  if((empty_gn != 0) | (na_gn != 0)){
    shinyalert(
      inputId = "confirmProharmedSE",
      title = "Empty Gene Names Found !",
      text = div(
        HTML("To remap gene names and filter protein IDs, go check "),
        HTML('<img src="ProHarMeD_Logo.png" width="250px" />'),
      ),
      size = "s",
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = TRUE,
      type = "warning",
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "Visit ProHarMeD",
      confirmButtonCol = "#53A93F",
      cancelButtonText = "Cancel",
      timer = 10000/2,
      imageUrl = "",
      animation = FALSE
    )
  }
  updateButton(session, "loadDataSE", label = " Load Data", disabled = FALSE)
})

observeEvent(input$loadDataOwn, {
  # reset reactiveVals
  ids <- reactiveVal()
  ids(NULL)
  updateButton(session, "loadDataOwn", label = " Loading...", disabled = TRUE)
  status <- "warning"

  tryCatch(
    {
      se <- loadData(reactiveVals$data$upload$data, reactiveVals$data$upload$md, input$columnName, input$protIDsColumn, input$geneNamesColumn, input$refSamplesBool, input$refSamples, input$batchBool, input$batchColumn)
      reactiveVals$se <- se
      updatePickerInput(session = session, inputId = "filterProteinsColumn", choices = colnames(rowData(se))) # only show those columns that are not the intensities
      updatePickerInput(session = session, inputId = "pomaGroup", choices = colnames(colData(se))[colnames(colData(se)) != "Column"])
      updatePickerInput(session = session, inputId = "removeSamplesColumn", choices = colnames(colData(se)), selected = colnames(colData(se))[1])
      updatePickerInput(session = session, inputId = "removeSamples", choices = colData(se)[[colnames(colData(se))[1]]])
      updatePickerInput(session = session, inputId = "normInput", choices = names(assays(se))[names(assays(se))!= "raw"])
      updatePickerInput(session = session, inputId = "normVisAin", choices = names(assays(se))[names(assays(se)) != "raw"], selected = names(assays(se))[names(assays(se)) != "raw"])
      updatePickerInput(session = session, inputId = "deNormInput", choices = names(assays(se))[names(assays(se)) != "raw"])
      updatePickerInput(session = session, inputId = "deColComparison", choices = colnames(colData(se))[colnames(colData(se)) != "Column"])
      updateSelectizeInput(session = session, inputId = "NAheatmap_color", choices = colnames(colData(se)), selected = colnames(colData(se))[2])
      # check unique columns (for labeling rows of heatmap)
      cols <- sapply(as.data.table(colData(se)), function(x) if(length(unique(x)) == length(x)) return(TRUE) else return(FALSE))
      label_options <- colnames(colData(se))[cols]
      updateSelectizeInput(session = session, inputId = "NAheatmap_label", choices = label_options, selected = label_options[1])
      updatePickerInput(session = session, inputId = "normVisCondition", choices = colnames(colData(se))[colnames(colData(se)) != "Column"])

      reactiveVals$se_filtered <- se
      reactiveVals$curr_NA_thr <- 0
      status <- "success"
    },
    error = function(e){
      status <- "error"
      sendSweetAlert(
        session = session,
        title = "Data Loading Failed !",
        text = e,
        type = "error"
      )
    }
  )

  if(status == "success"){
    upload_tab <- which(tab_ids == "upload")
    if (isolate(reactiveVals$max_tab) > upload_tab)
      reactiveVals$max_tab <- upload_tab
    reactiveVals$continue[upload_tab] <- TRUE
  }
  updateButton(session, "loadDataOwn", label = " Load Data", disabled = FALSE)

  # check if empty gene names --> if then show ProHarMeD alert
  rowdata <- as.data.table(rowData(se))
  empty_gn <- sum(rowdata$Gene.Names == "")
  na_gn <- sum(is.na(rowdata$Gene.Names))
  if((empty_gn != 0) | (na_gn != 0)){
    proharmed_alert <-  shinyalert(
      inputId = "confirmProharmedOwn",
      title = "Empty Gene Names Found !",
      text = div(
        HTML("To remap gene names and filter protein IDs, go check "),
        HTML('<img src="ProHarMeD_Logo.png" width="250px" />'),
      ),
      size = "s",
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = TRUE,
      type = "warning",
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "Visit ProHarMeD",
      confirmButtonCol = "#53A93F",
      cancelButtonText = "Cancel",
      timer = 10000/2,
      imageUrl = "",
      animation = TRUE
    )
  }
})

observeEvent(input$confirmProharmedOwn, {
  if(input$confirmProharmedOwn){
    browseURL("https://proharmed.zbh.uni-hamburg.de")
  }
})

observeEvent(input$confirmProharmedExample, {
  if(input$confirmProharmedExample){
    browseURL("https://proharmed.zbh.uni-hamburg.de")
  }
})

observeEvent(input$confirmProharmedSE, {
  if(input$confirmProharmedSE){
    browseURL("https://proharmed.zbh.uni-hamburg.de")
  }
})


################################## UI Output ##################################


output$metadataTable <- DT::renderDataTable({
  req(reactiveVals$se)
  dt <- DT::datatable(as.data.table(colData(reactiveVals$se)),
                rownames = FALSE,
                options = list(pageLength = 8, searching = TRUE, scrollX = TRUE))
  return(dt)
})

# Output Selected Data Box
output$currentData <- renderInfoBox({
  refSamples <- NULL
  info <- NULL
  se <- NULL
  batchColumn <- NULL
  if(!is.null(reactiveVals$se)){
    se <- reactiveVals$se
    mq <- assays(se)["raw"]
    md <- as.data.table(colData(se))
    refSamples <- metadata(se)$refs
    batchColumn <- metadata(se)$batch
    if(is.null(refSamples)){
      info <- data.frame("Number of Samples" = dim(se)[2],
                         "Number of Proteins" = dim(se)[1]
      )
    } else {
      info <- data.frame("Number of Samples" = dim(se)[2],
                         "Number of Proteins" = dim(se)[1],
                         "Reference Samples" = "Yes"
      )
    }
  }

  status <- "warning"
  # Store Info Table
  overview_current <- NULL
  if(!is.null(info)){
    info_dt <- renderTable(
      checkNullTable(info),
      caption = "Data Overview",
      caption.placement = "top")
    overview_current <- c(list(info_dt))
  }
  # Store NA Overview Table
  if(!is.null(se)){
    NA_overview <- get_NA_overview(se)
    NA_overviewText <- fluidRow(column(
      width = 12,
      renderTable(
        checkNullTable(NA_overview),
        caption = "Overview of Entries",
        caption.placement = "top"
      )
    ))
    overview_current <- c(overview_current, list(NA_overviewText))
  }
  # Store Reference Samples as Table if Reference Samples in Data
  if(!is.null(refSamples)){
    refSamples <- data.frame(Samples = refSamples)
    refSamplesText <- fluidRow(column(
      width = 12,
      renderTable(
        checkNullTable(refSamples),
        caption = "Reference Samples",
        caption.placement = "top"
      )
    ))
    overview_current <- c(overview_current, list(refSamplesText))
  }
  if(!is.null(batchColumn)){
    overview_current <- c(overview_current, list(h5(HTML("<p style=\"color:grey;\">Batch</p>")), batchColumn))
  }
  metadatalist <- NULL
  if(is.null(overview_current)){
    status <- "warning"
    collapsed <- TRUE
  } else {
    status <- "success"
    collapsed <- FALSE
    metadatalist <- list(h5(HTML("<p style=\"color:grey;\">Metadata</p>")),
                         DT::dataTableOutput("metadataTable"))
  }



  shinydashboard::box(
    collapsible = TRUE,
    collapsed = collapsed,
    fluidRow(
      column(4, list(overview_current)),
      column(8, metadatalist),
      style='padding-top:10px;'
    ),
    title = "Selected Data", status = status)
})


