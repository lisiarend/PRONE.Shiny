filterProteinsBody <- function() {
  box_height <- "30em"
  
  # Popovers
  filterNAPopover <- bsPopover( id = "filterNAInfo", 
                                title = "Filter proteins with too many missing values.", 
                                content = "A threshold of 80% can be applied to remove proteins with missing values in over 20% of samples (must have a value in at least 80% of samples). Before filtering, click Visualize Selection to generate visualizations, then proteins can be permanently filtered.<b>")
  
                                #"The identification of some proteins can be limited to a subset of samples due to biological or analytical factors, 
                                #resulting in missing values in the remaining samples. To address this, a threshold can be applied, such as 80%, which removes all 
                                #proteins with missing values in more than 20% of the samples, meaning they must have a valid value in at least 80% of the samples. 
                                #Prior to the filtering process, it is necessary to click on the visualize selection button to generate the visualizations before any
                                #proteins can be removed. Once the visualizations have been generated, the proteins can then be permanently filtered from the data.<b>")
  
  filterHeatmapPopover <- bsPopover( id = "filterHeatmapInfo", 
                                title = "Heatmap of proteins with missing values.", 
                                content = "To explore the pattern of missing values in the data, a heatmap is generated to depict the presence or absence of values. Only proteins with at least one missing value are included in the visualization.")
  
  filterDensityPopover <- bsPopover( id = "filterDensityInfo", 
                                     title = "Intensity distributions of proteins with and without missing values.", 
                                     content = "Protein containing any missing values are colored in red. Here you can see if missing values are biased to lower intense proteins.")
  
  filterBarPopover <- bsPopover( id = "filterBarInfo", 
                                     title = "Protein identifications overlap", 
                                     content = "The number of proteins quantified in number of samples is plotted to depict the extend of missing values.")
  
  filterColumnPopover <- bsPopover( id = "filterColumnInfo", 
                                 title = "Select a specfic column from the proteomics data and remove proteins with a specific value", 
                                 content = "In MaxQuant, reverse counterparts and contaminants are typically included in the database search. To filter out these proteins, select the specifc column, then choose the value for the column you want to filter.")
  
  
  # Boxes
  
  filterProteinsNABox <- shinydashboard::box(
    width = 12,
    title = span("Filter Proteins Based on Missing Value Pattern", tags$i(class = "fa fa-circle-info", style = "color: rgb(0,166,90)"), id = "filterNAInfo"),
    collapsible = TRUE,
    collapsed = FALSE,
    fluidRow(
      filterNAPopover,
      column(
        width = 12,
        sliderInput(
          "filterProteinsNAThr",
          label = "Select Threshold (Remove Proteins That Have Less Than n% of Valid Values in All Samples)",
          min = 10,
          max = 100,
          step = 5,
          value = 80,
        ),
        div(
          bsButton(
            "filterProteinsNAUpdateButton",
            " Visualize Selection",
            icon = icon("palette"),
            style = "success"
          ),
          bsButton(
            "filterProteinsNAButton",
            " Filter Proteins",
            icon = icon("trash"),
            style = "danger",
            disabled = TRUE,
          ),
          style = {"padding-bottom : 20px;"}
        ),
      )
    )
  )
  
  filterProteinsVisBox <- shinydashboard::box(
    width = 12,
    title = "Visualizations",
    collapsible = TRUE,
    collapsed = FALSE,
    fluidRow(
      filterHeatmapPopover,
      column(
        width = 12,
        tabBox(
          tabPanel(
            fluidRow(
              column(
                1,
                div(
                  dropdownButton(
                    tags$h3("Plot Options"),
                    selectizeInput(
                      "NAheatmap_color",
                      "Color By: ",
                      choices = NULL,
                      multiple = FALSE,
                    ),
                    selectizeInput(
                      "NAheatmap_label",
                      "Label By: ",
                      choices = NULL,
                      multiple = FALSE
                    ),
                    selectizeInput(
                      "NAheatmap_cluster_samples",
                      "Cluster Samples: ",
                      choices = c("Yes" = TRUE, "No" = FALSE),
                      multiple = FALSE,
                      selected = "Yes"
                    ),
                    selectizeInput(
                      "NAheatmap_cluster_proteins",
                      "Cluster Proteins: ",
                      choices = c("Yes" = TRUE, "No" = FALSE),
                      multiple = FALSE,
                      selected = "Yes",
                    ),
                    selectizeInput(
                      "NAheatmap_row_dend",
                      "Row Dendrogram: ",
                      choices = c("Yes" = TRUE, "No" = FALSE),
                      multiple = FALSE,
                      selected = "Yes"
                    ),
                    selectizeInput(
                      "NAheatmap_col_dend",
                      "Column Dendrogram: ",
                      choices = c("Yes" = TRUE, "No" = FALSE),
                      multiple = FALSE,
                      selected = "Yes"
                    ),
                    circle = TRUE,
                    status = "custom",
                    icon = tags$i(class = "fa fa-gear", style = "color: white"),
                    width = "400px",
                    tooltip = tooltipOptions(title = "Click to see plot options")
                  ),
                  style = "position:relative; height: 500px;"
                ),
              ),
              column(
                11,
                shinycssloaders::withSpinner(plotOutput("NA_heatmap_plot", width = "100%", height = "750px")),
                div(
                  uiOutput("NA_heatmap_download"),
                  style = "float: right"
                )
                
              ),
              filterHeatmapPopover
            ),
            value = "NA_heatmap",
            title = span("Heatmap of NAs", tags$i(class = "fa fa-circle-info", style = "color: rgb(0,166,90)"), id = "filterHeatmapInfo")
          ),
          tabPanel(
            shinycssloaders::withSpinner(plotOutput("NA_density_plot",  width = "100%", height = "500px")),
            filterDensityPopover,
            div(
              uiOutput("NA_density_download"),
              style = "float: right; padding-top: 20px;"
            ),
            value = "NA_density",
            title = span("Density of NAs", tags$i(class = "fa fa-circle-info", style = "color: rgb(0,166,90)"), id = "filterDensityInfo")
          ),
          tabPanel(
            shinycssloaders::withSpinner(plotOutput("NA_frequency_plot",  width = "100%", height = "500px")),
            filterBarPopover,
            div(
              uiOutput("NA_frequency_download"),
              style = "float: right; padding-top: 20px;"
            ),
            value = "NA_frequency",
            title = span("Frequency of NAs", tags$i(class = "fa fa-circle-info", style = "color: rgb(0,166,90)"), id = "filterBarInfo"),
          ),
          id = "NA_plots",
          title = "Missing Value Plots",
          width = 12
        )
      )
    )
  )
  
  filterProteinsColumnBox <- shinydashboard::box(
    width = 12,
    title = span("Filter Proteins Based on A Specifc Column", tags$i(class = "fa fa-circle-info", style = "color: rgb(0,166,90)"), id = "filterColumnInfo"),
    collapsible = TRUE,
    collapsed = FALSE,
    fluidRow(height = "50em",
             filterColumnPopover,
             column(
               width = 12,
               pickerInput(
                 "filterProteinsColumn",
                 label = "Select Column You Want to Filter Proteins",
                 choices = NULL,
                 multiple = FALSE
               ),
               pickerInput(
                 "filterProteinsValue",
                 label = "Select the Value of the Column You Want to Filter Proteins",
                 choices = NULL,
                 multiple = FALSE
               ),
               div(
                 bsButton(
                   "filterProteinsColumnButton",
                   " Filter Proteins",
                   icon = icon("trash"),
                   style = "danger",
                 ),
               )
             ))
  )
  
  filteringBody <- tabItem(
    tabName = "filterProteins",
    fluidRow(shinydashboard::box(
      div(HTML(
        "The analysis of proteomics data is challenged by the presence of missing values in protein abundance measurements. The missing values
        can significantly impact the statistical power and functional analysis due to both biological and analytical factors, such as as the absence 
        of certain proteins or low protein levels below the detection limit of the instrument, or sample loss during preparation and low ionization efficiency.
        <br />
        Many statistical methods have limitations in handling missing values or cannot handle them at all. To mitigate these challenges, plots can be used to show the pattern
        and quantity of missing values, and proteins with an excessive number of missing values can be filtered.
        <br />
        Furthermore, users have the option to select a specfic column from the proteomics data and remove proteins with a specific value. In MaxQuant, reverse counterparts and 
        contaminants, which are typically excluded in downstream analysis, are included in the database search and can be removed at the bottom of the page.
        "
      )
      ),
      title = h2("Filter Proteins"),
      width = 12
    ), ),
    fluidRow(filterProteinsNABox,),
    fluidRow(filterProteinsVisBox,),
    fluidRow(filterProteinsColumnBox,),
    
  )
  return(filteringBody)
  
}