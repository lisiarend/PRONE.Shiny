outlierDetectionBody <- function() {
  box_height <- "30em"

  outlierDistancePopover <- bsPopover( id = "outlierDistanceInfo",
                                     title = "Distance measure method to perform MDS (multidimensional scaling).",
                                     content = "The distances between each pair of samples are calculated. Options are euclidean, maximum, manhattan, canberra, and minkowski.")

  outlierTypePopover <- bsPopover( id = "outlierTypeInfo",
                                       title = "Measure type for multivariate dispersion",
                                       content = "One measure of multivariate dispersion for a group of samples is to calculate the average distance (centroid) of group members to the group centroid or spatial median (median) in multivariate space.")

  outlierConditionPopover <- bsPopover( id = "outlierConditionInfo",
                                   title = "Column name of metadata containing the information about the group/condition of the samples.",
                                   content = "POMA is a multivariate outlier detection method, and outlier samples are calculated based on their distance to their group members. This vector should contain at least 2 unique values!")

  outlierCoeffPopover <- bsPopover( id = "outlierCoeffInfo",
                                        title = "Classical univariate outlier detection formula Q3 + x * IQR.",
                                        content = "Select x. Classical value is 1.5. The higher this value, the less sensitive the method is to outliers.")



  outlierMethodBox <- shinydashboard::box(
    width = 6,
    height = box_height,
    title = "Detect Outlier Samples",
    fluidRow(height = box_height,
             column(
               width = 12,
               outlierDistancePopover,
               outlierTypePopover,
               outlierConditionPopover,
               outlierCoeffPopover,
               pickerInput(
                 "pomaMethod",
                 label = span("Select the Distance Measure Method to Perform MDS.", tags$i(class = "fa fa-circle-info", style = "color: rgb(0,166,90)"),  id = "outlierDistanceInfo"),
                 choices = c("euclidean", "maximum", "manhattan", "canberra", "minkowski"),
                 multiple = FALSE,
               ),
               pickerInput(
                 "pomaType",
                 label = span("Select the Type of Outlier Analysis to Perform.", tags$i(class = "fa fa-circle-info", style = "color: rgb(0,166,90)"), id = "outlierTypeInfo"),
                 choices = c("median", "centroid"),
                 multiple = FALSE
               ),
               pickerInput(
                 "pomaGroup",
                 label = span("Select the Condition for Multi-Variate Outlier Detection (Non-unique).", tags$i(class = "fa fa-circle-info", style = "color: rgb(0,166,90)"), id = "outlierConditionInfo"),
                 choices = NULL,
                 multiple = FALSE
               ),
               numericInput(
                 "pomaCoefficient",
                 label = span("Select the x for Q3 + x * IQR", tags$i(class = "fa fa-circle-info", style = "color: rgb(0,166,90)"), id = "outlierCoeffInfo"),
                 value = 1.5,
                 min = 0.25,
                 max = 3,
                 step = 0.25
                ),
             )),
    div(
      bsButton(
        "performOutlierDetectionButton",
        " Perform Outlier Detection ",
        icon = icon("magnifying-glass"),
        style = "success",
      ),
      style = "float: right; position: absolute; bottom: 15px;"
    )
  )

  outlierTableBox <- shinydashboard::box(
    width = 6,
    height = box_height,
    title = "Outlier Samples",
    collapsible = FALSE,
    uiOutput("pomaOutlierSamples"),
    div(
      bsButton(
      "removeOutliersButton",
      " Remove Outlier Samples ",
      icon = icon("trash"),
      style = "danger",
      disabled = TRUE
      ),
      style = "float:right; position: absolute; bottom: 15px"
    )
  )

  outlierVisualizationBox <- shinydashboard::box(
    width = 12,
    title = "Visualizations",
    collapsible = TRUE,
    collapsed = FALSE,
    uiOutput("outlierPlots")
  )


  manualSampleRemovalBox <- shinydashboard::box(
    width = 12,
    title = "Manual Sample Removal",
    collapsible = TRUE,
    collapsed = TRUE,
    fluidRow(column(
      width = 12,
      pickerInput(
        "removeSamplesColumn",
        label = "Select Column of Metadata",
        choices = NULL,
        multiple = FALSE,
      ),
      pickerInput(
        "removeSamples",
        label = "Select Samples You Want to Remove",
        choices = NULL,
        multiple = TRUE,
        options = list(size = 10, "actions-box" = TRUE)
      ),
      div(
        bsButton(
          "removeSamplesManuallyButton",
          " Remove Samples",
          icon = icon("trash"),
          style = "danger",
          disabled = TRUE
        ),
      )

    ))
  )

  outlierDetection <- tabItem(
    tabName = "outlierDetection",
    fluidRow(shinydashboard::box(
      div(
        HTML(
          "Outliers are atypical observations that deviate significantly from the majority of the data points.
          These values can have a significant impact on the results of statistical analysis and can pose a risk
          to the assumptions inherent in many commonly used parametric tests.
          <br />
          POMA is a multivariate outlier detection method that identifies outlier samples through the calculation of euclidean
          distances among samples and their distances to each group centroid in a two-dimensional space.
          The method employs a classical univariate outlier detection formula Q3 + 1.5 IQR based on the computed
          distances to determined multivariate group-specific outliers.
          <br />
          Furthermore, this tab provides the ability to remove samples manually."
        )
      ),
      title = h2("Outlier Detection On Sample Level (POMA)"),
      width = 12
    )),
    fluidRow(outlierMethodBox, outlierTableBox),
    fluidRow(outlierVisualizationBox,),
    fluidRow(manualSampleRemovalBox)
  )
  return(outlierDetection)

}
