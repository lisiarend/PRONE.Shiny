goodbyeBody <-  tabItem(tabName = "goodbye",
                        fluidRow(
                          shinydashboard::box(fluidRow(
                            column(width = 12,
                                   div(
                                     HTML("We sincerely appreciate your trust in our tool to assist you in achieving reliable and accurate proteomics data normalization and performing proteomics data analysis."),

                                     HTML("We are committed to continuously improving PRONE and eagerly anticipate your valuable feedback, which is crucial for driving future enhancements. Therefore, please feel free to write an issue to the GitHub repository."),

                                     HTML("How to cite PRONE?"),
                                     HTML("... coming soon ..."),
                                     style = "font-size: large;"
                                   )
                            ),
                            column(
                              width = 12,
                              img(
                                src = "PRONE_Text_Logo.png",
                                height = "170px",
                                width = "auto"
                              ),
                              style = "vertical-align: middle; text-align:center;"
                            )
                          ),
                          title = h2("Thank you for using PRONE, the PROteomics Normalization Evaluator."),
                          width = 12
                          ),
                          width = 12)

)
