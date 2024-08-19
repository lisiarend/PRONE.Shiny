normalizationBody <- function() {
  box_height <- "40em"

  normalizationRawLogPopover <- bsPopover( id = "normalizationRawLogInfo",
                                          title = "Raw of log2-transformed data as input for normalization..",
                                          content = "Normalization is typically performed on raw data, but some normalization techniques require log2-transformed data. Have a look at the table on the right where a suggestion on the data input is showed for each normalization technique. If this suggestion fits your data, use the default settings.")

  normalizationMethodPopover <- bsPopover( id = "normalizationMethodInfo",
                                           title = "Normalization method to perform",
                                           content = "Select the normalization technique you want to perform on the input data.")

  normalizationCombPopover <- bsPopover( id = "normalizationCombInfo",
                                          title = "Input data to use for performing normalization, only needed for combination of methods!",
                                          content = "The selection is refreshed with each normalization process. To perform a combination of normalization techniques, such as IRS on Median-normalized data, first apply Median normalization, and then use the resulting data as input (here) for the subsequent process, in this case IRS.")

  normalizationMethodVisPopover <- bsPopover( id = "normalizationMethodVisInfo",
                                              title = "Normalization methods to visualize",
                                              content = "The visualization of normalization methods performed above can be limited to a specific subset of methods.")

  normalizationPCVPopover <- bsPopover( id = "normalizationPCVInfo",
                                        title = "Intragroup Pooled Coefficient of Variation (PCV)",
                                        content = "The PCV for a sample group is determined as the average CV across all protein groups. A low intragroup variation indicates a high level of similarity among technical replicates, indicating high degree of reproducibility in the analysis.")

  normalizationPEVPopover <- bsPopover( id = "normalizationPEVInfo",
                                        title = "Intragroup Pooled Estimate of Variance (PEV)",
                                        content = "The PEV is defined as the mean of variances of a sample group over all protein groups. A low intragroup variation indicates a high level of similarity among technical replicates, indicating high degree of reproducibility in the analysis.")

  normalizationPMADPopover <- bsPopover( id = "normalizationPMADInfo",
                                         title = "Intragroup Pooled Median Absolute Deviation (PMAD)",
                                         content = "The PMAD for a sample group is defined as the mean MAD over all protein groups. A low intragroup variation indicates a high level of similarity among technical replicates, indicating high degree of reproducibility in the analysis.")

  normalizationCorrPopover <- bsPopover( id = "normalizationCorrInfo",
                                         title = "Intragroup Correlation",
                                         content = "Similarity of replicates in sample groups is determined using the Pearson correlation coefficient. A high intragroup similarity indicates high degree of reproducibility in the analysis ")

  normalizationMethodVisConditionPopover <- bsPopover( id = "normalizationMethodVisConditionInfo",
                                                       title = "Condition to use for intragroup variation calculation",
                                                       content = "The PMAD, PEV, PCV and intragroup correlation measures are calculated using the sample groups which need to be specified..")

  normalizationGlobalPCAPopover <- bsPopover( id = "normalizationGlobalPCAInfo",
                                              title = "PCA of samples from different normalization methods.",
                                              content = "All individual normalized sample intensities will be added to a big SummarizedExperiment object, and a single PCA will be performed on all samples.")


  normalizationSelectionBox <- shinydashboard::box(
    width = 6,
    height = box_height,
    title = "Normalize Data",
    fluidRow(height = box_height,
             column(
               width = 12,
               awesomeRadio(
                 "normRawLog",
                 label = span("Normalize on raw data or log2 transformed data?", tags$i(class = "fa fa-circle-info", style = "color: rgb(0,166,90)"), id = "normalizationRawLogInfo"),
                 choices = c("on default" = "default", "on raw" = TRUE, "on log2" = FALSE),
                 selected = NULL,
                 inline = TRUE
               ),
               pickerInput(
                 "normMethods",
                 label = span("Select the normalization method.", tags$i(class = "fa fa-circle-info", style = "color: rgb(0,166,90)"), id = "normalizationMethodInfo"),
                 choices = list(
                   "Normalization" = c("GlobalMean","GlobalMedian", "Median", "Mean", "Quantile", "VSN",
                       "LoessF", "LoessCyc", "RLR", "RlrMA", "RlrMACyc", "EigenMS", "MAD", "RobNorm", "TMM", "NormicsVSN", "NormicsMedian"),
                   "Batch Effect Correction" = c("IRS", "limBE")
                 ),
                 options = list("actions-box" = TRUE),
                 multiple = TRUE
               ),
               uiOutput("normMethodsCombination"),
               fluidRow(
                 uiOutput("normMethodsParameter")
               ),
               normalizationRawLogPopover,
               normalizationMethodPopover,
               normalizationCombPopover
             ),
    ),
    div(
      bsButton(
        "performNormalization",
        " Perform Normalization ",
        icon = icon("screwdriver-wrench"),
        style = "success",
        disabled = TRUE,
      ),
      style = "float: right; position: absolute; bottom: 15px;"
    )
  )

  normalizationHelpBox <- shinydashboard::box(
    width = 6,
    height = box_height,
    title = "Information on Normalization Methods",
    fluidRow(height = box_height,
             column(
               width = 12,
               div(
                 HTML(
                   "<b>Methods that should be performed on raw data:</b>"
                 ),
                 HTML(
                   "<ul>
                      <li>Global Intensity Normalization (GlobalMean, GlobalMedian)</li>
                      <li>Median and Mean Normalization</li>
                      <li>Normics with Median Normalization (NormicsMedian)</li>
                      <li>Trimmed Mean of M-values Normalization (TMM)</li>
                      <li>Variance Stabilization Normalization (VSN)</li>
                      <li>Normics with VSN Normalization (NormicsVSN)</li>
                      <li>Internal Reference Scaling Normalization (IRS) - to adjust for TMT batches: raw data</li>
                    </ul>"
                ),
                HTML(
                  "<b>Methods that should be performed on log2 transformed data:</b>",
                ),
                HTML(
                  "<ul>
                    <li>Median Absolute Deviation Normalization (MAD)</li>
                    <li>Quantile Normalization</li>
                    <li>Linear Regression Normalization (Rlr, RlrMA, RlrMACyc)</li>
                    <li>Logistic Regression Normalization (LoessF, LoessCyc)</li>
                    <li>EigenMS - surrogate variable analysis (combining ANOVA and SVD)</li>
                    <li>RobNorm - density-power-weight method to estimate and remove sample effect (recommendation for gamma: 0.5 or 1 if sample size > 100, otherwise 0.5 or 0.1)</li>
                    <li>Remove batch effects method of limma package (limBE) - to adjust for TMT batrches </li>
                  </ul>"
                ),
                HTML("<b> NormicsVSN and NormicsMedian take longer than the other methods. You can also perform normalization using the R package and then load your object in the Shiny app!</b>"),
                HTML("<b>Attention !</b> IRS and limBE can only be performed if the experiment was performed in batches. In addition, IRS requires the existence of one reference sample per batch!")
                ),
             ))
  )

  normalizationQualVisBox <- shinydashboard::box(
    width = 12,
    title = "Qualitative Normalization Plots",
    collapsible = TRUE,
    collapsed = FALSE,
    fluidRow(
      column(
        width = 12,
        tabBox(
          tabPanel(
            uiOutput("norm_boxplots_tab"),
            value = "norm_boxplots",
            title = "Boxplots"
          ),
          tabPanel(
            uiOutput("norm_densities_tab"),
            value = "norm_densities",
            title = "Densities"
          ),
          tabPanel(
            uiOutput("norm_PCA_tab"),
            value = "norm_PCA",
            title = "PCA"
          ),
          normalizationGlobalPCAPopover,
          id = "qual_norm_plots",
          title = "Qualitative Normalization Plots",
          width = 12
        )
      )
    )
  )

  normalizationQuanVisBox <- shinydashboard::box(
    width = 12,
    title = "Quantitative Normalization Plots",
    collapsible = TRUE,
    collapsed = FALSE,
    fluidRow(
      column(
        width = 12,
        tabBox(
          tabPanel(
            uiOutput("norm_pev_tab"),
            normalizationPEVPopover,
            value = "norm_pev",
            title = span("Intragroup PEV", tags$i(class = "fa fa-circle-info", style = "color: rgb(0,166,90)"), id = "normalizationPEVInfo"),
          ),
          tabPanel(
            uiOutput("norm_pcv_tab"),
            normalizationPCVPopover,
            value = "norm_pcv",
            title = span("Intragroup PCV", tags$i(class = "fa fa-circle-info", style = "color: rgb(0,166,90)"), id = "normalizationPCVInfo"),
          ),
          tabPanel(
            uiOutput("norm_pmad_tab"),
            normalizationPMADPopover,
            value = "norm_pmad",
            title = span("Intragroup PMAD", tags$i(class = "fa fa-circle-info", style = "color: rgb(0,166,90)"), id = "normalizationPMADInfo"),
          ),
          tabPanel(
            uiOutput("norm_intr_corr_tab"),
            normalizationCorrPopover,
            value = "norm_intr_corr",
            title = span("Intragroup Correlation", tags$i(class = "fa fa-circle-info", style = "color: rgb(0,166,90)"), id = "normalizationCorrInfo"),
          ),

          id = "quan_norm_plots",
          title = "Quantitative Normalization Plots",
          width = 12
        )
      )
    )
  )

  normalizationVisBox <- shinydashboard::box(width = 12,
                                             title = "Visualizations",
                                             fluidRow(
                                               column(
                                                 width = 6,

                                                 pickerInput(
                                                   "normVisAin",
                                                   choices = NULL,
                                                   multiple = TRUE,
                                                   options = list("actions-box" = TRUE),
                                                   label = span("Select the normalization methods you want to visualize.", tags$i(class = "fa fa-circle-info", style = "color: rgb(0,166,90)"), id = "normalizationMethodVisInfo"),
                                                 )
                                               ),
                                               column(
                                                 width = 6,
                                                 pickerInput(
                                                   "normVisCondition",
                                                   choices = NULL,
                                                   multiple = FALSE,
                                                   label = span("Select the condition you want to calculate intragroup variation.", tags$i(class = "fa fa-circle-info", style = "color: rgb(0,166,90)"), id = "normalizationMethodVisConditionInfo"),
                                                 ),
                                               ),
                                               normalizationMethodVisPopover,
                                               normalizationMethodVisConditionPopover,
                                             ),
                                             fluidRow(normalizationQuanVisBox),
                                             fluidRow(normalizationQualVisBox),

  )

  normalization <- tabItem(
    tabName = "normalization",
    fluidRow(shinydashboard::box(
      div(
        HTML(
          "Mass spectrometry (MS) data remain biased as a result of reasons ranging from sample handling to difference caused by the instrumentation.
        Normalization mitigates these biases and increases the comparability between samples. However, there are many different normalization methods and the
        choice of an appropriate normalization method is crucial for the accuracy of downstream analysis and outcomes.
        <br />
        This app offers multiple normalization methods and also allows for the combination of different normalization methods. Classical visualizations,
        such as boxplots, PCA plots, and density plots are generated for each normalization method applied. Additionally, visualizations that depict
        intragroup variations and similarities are provided to assess the performance of individual normalization methods.
        <br />
        It is not necessary to choose a single normalization method at this point, as the subsequent tab allows for the performance of differential
        expression analysis using multiple normalization methods, providing an additional means to evalute the performance of these methods."
        )
      ),
      title = h2("Normalization"),
      width = 12
    )),
    fluidRow(normalizationSelectionBox, normalizationHelpBox),
    fluidRow(normalizationVisBox)
  )
  return(normalization)

}
