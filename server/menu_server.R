tab_ids <-
  c("welcome",
    "upload",
    "filterProteins",
    "outlierDetection",
    "normalization",
    "de_analysis"
    )

reactiveVals$current_tab <- 1
reactiveVals$max_tab <- 1
reactiveVals$continue <- c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE)


tabs <- list(
  menuItem("Welcome",
           tabName = tab_ids[1],
           icon = icon("house")),
  menuItem("Upload",
           tabName = tab_ids[2],
           icon = icon("circle-play")),
  menuItem(
    "Filter Proteins",
    tabName = tab_ids[3],
    icon = icon("filter")
  ),
  menuItem(
    "Outlier Detection",
    tabName = tab_ids[4],
    icon = icon("magnifying-glass")
  ),
  menuItem(
    "Normalization",
    tabName = tab_ids[5],
    icon = icon("screwdriver-wrench")
  ),
  menuItem(
    "DE Analysis",
    tabName = tab_ids[6],
    icon = icon("object-group")
  )
)

#observeEvent(reactiveVals$max_tab,{
#  # check NA values
#  if((reactiveVals$max_tab == 3) && (!is.null(reactiveVals$se))){
#    tmp <- remove_complete_NA_proteins(reactiveVals$se)
#    if(nrow(tmp[[2]]) > 0){
#      shinyalert(
#        inputId = "confirmCompleteNAs",
#        title = "Protein Groups with NA values for all samples have been identified",
#        text = paste0(nrow(tmp[[2]])," proteins have only NA values. These proteins should be removed for the analysis."),
#        size = "s", 
#        closeOnEsc = TRUE,
#        closeOnClickOutside = TRUE,
#        html = FALSE,
#        type = "warning",
#        showConfirmButton = TRUE,
#        showCancelButton = TRUE,
#        confirmButtonText = "Remove Proteins",
#        confirmButtonCol = "#53A93F",
#        cancelButtonText = "Cancel",
#        timer = 0,
#        imageUrl = "",
#        animation = TRUE,
#        callbackJS = 
#        
#      )
#    }
#  }
#})

observeEvent({
  reactiveVals$current_tab
  reactiveVals$continue
},
{
  if (reactiveVals$current_tab == 1)
    shinyjs::hide("previousTab")
  else if (reactiveVals$current_tab > 1)
    shinyjs::show("previousTab")
  else
    stop("what is the current id?")
  if (reactiveVals$current_tab < length(tab_ids))
    shinyjs::show("nextTab")
  else if (reactiveVals$current_tab == length(tab_ids))
    shinyjs::hide("nextTab")
  #shinyBS::updateButton(session, inputId = "nextTab", icon = icon("times-circle"), style = "warning", disabled = FALSE)
  else
    stop("what is the current id?")
  if (!reactiveVals$continue[reactiveVals$current_tab] &&
      reactiveVals$current_tab == reactiveVals$max_tab)
    shinyjs::disable("nextTab")
  else if (reactiveVals$continue[reactiveVals$current_tab] || reactiveVals$current_tab != reactiveVals$max_tab) {
    shinyjs::enable("nextTab")
  }
})

prevNames <- c(" Previous", " Welcome", " Upload Data", " Filter Proteins", " Outlier Detection", " Normalization")
nextNames <- c(" Upload Data", " Filter Proteins", " Outlier Detection", " Normalization"," DE Analysis")

observeEvent(input$tabs, {
  reactiveVals$current_tab <- match(input$tabs, tab_ids)
  shinyBS::updateButton(session, inputId = "previousTab", label = prevNames[reactiveVals$current_tab])
  shinyBS::updateButton(session, inputId = "nextTab", label = nextNames[reactiveVals$current_tab])
})

observeEvent(input$previousTab, {
  shinyjs::enable("nextTab")
  reactiveVals$current_tab <- reactiveVals$current_tab - 1
  updateTabItems(session, "tabs", tab_ids[reactiveVals$current_tab]) # here we update in case the new current tab is not a new max tab
  shinyjs::runjs("window.scrollTo(0, 0)")
})

observeEvent(input$nextTab, {
  shinyjs::enable("previousTab")
  reactiveVals$current_tab <- reactiveVals$current_tab + 1
  if (reactiveVals$current_tab > reactiveVals$max_tab){
    reactiveVals$max_tab <- reactiveVals$current_tab
  } else 
    updateTabItems(session, "tabs", tab_ids[reactiveVals$current_tab]) # here we update in case the new current tab is not a new max tab
  shinyjs::runjs("window.scrollTo(0, 0)")
})

observeEvent(input$confirmCompleteNAs, {
  if(input$confirmCompleteNAs){
    req(reactiveVals$se)
    tmp <- remove_complete_NA_proteins(reactiveVals$se)
    reactiveVals$se <- tmp[[1]]
    reactiveVals$se_filtered <- tmp[[1]]
  }
})


# grow menu depending on current tab
output$sidebar <- renderUI({
  curr_menu <- sidebarMenu(id = "tabs",
                           tabs[1:reactiveVals$max_tab])
  
  # here we update in case the new current tab is also a new max tab
  updateTabItems(session, "tabs", tab_ids[reactiveVals$max_tab])
  
  return(curr_menu)
})

observeEvent(input$download_data, {
  if(is.null(reactiveVals$se)){
    output <- HTML("No Data Uploaded")
  } else {
    methods <- names(assays(reactiveVals$se))
    output <- fluidRow(
        style = "padding-left: 10px; padding-right: 10px",
        awesomeCheckboxGroup(
          inputId = "download_assays",
          label = "Select Data You Want To Download:", 
          choices = methods,
          selected = methods
        ),
        radioGroupButtons(
          inputId = "download_assays_type",
          label = "Select Type of Data To Download:", 
          choices = c("As SummarizedExperiment", "As CSV"),
          selected = ("As SummarizedExperiment"),
          justified = TRUE
        ),
        downloadButton(
          "downloadData",
          " Download Data ",
          icon = icon("download"),
          style = "success",
          disabled = FALSE,
        ),
    )
  }
  showModal(
    modalDialog(title = "Download Data",
                output,
                easyClose = TRUE,
                footer = modalButton("Close"))
  )
  
})

observeEvent(input$download_assays, {
  if(is.null(input$download_assays)){
    # disable button
    disable("downloadData")
  } else if(length(input$download_assays) == 0){
    # disable
    disable("downloadData")
  } else {
    # enable
    enable("downloadData")
  }
}, ignoreNULL = FALSE)

output$downloadData <- downloadHandler(
  filename = function(){
    ifelse(input$download_assays_type == "As CSV", "Data_CSV.zip", "Data_SE.rds")
  },
  content = function(file){
    waiter_show(id = "app",html = tagList(spinner$logo, 
                                          HTML("<br>Downloading...")), 
                color=spinner$color)
    if(input$download_assays_type == "As CSV"){
      # Cip Directory
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      rowdata <- as.data.table(rowData(reactiveVals$se))
      write.csv(as.data.table(colData(reactiveVals$se)), file.path(temp_directory, "experimental_design.csv"))
      for(ain in input$download_assays){
        dt <- as.data.table(assays(reactiveVals$se)[[ain]])
        dt <- cbind(rowdata[, c("Protein.IDs"), with = FALSE], dt)
        file_name <- paste0(ain, "_data.csv")
        write.csv(dt, file.path(temp_directory, file_name))
      }
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
    } else {
      # SummarizedExperiment
      saveRDS(reactiveVals$se, file)
    }
    waiter_hide(id="app")
  }
)


observeEvent(input$dependencies_modal, {
  showModal(
    modalDialog(title = "Dependencies",
                shinycssloaders::withSpinner(dataTableOutput("licenseTable")),
                easyClose = TRUE,
                footer = modalButton("Close"))
  )
})

output$licenseTable <- renderDataTable({
  deps_table <- sapply(unique(renv::dependencies()$Package), packageDescription, fields="License")
  DT::datatable(data.frame(Package = names(deps_table), License = deps_table),
                rownames = FALSE,
                options = list(pageLength = 10, searching = TRUE))
})


