

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # make reactiveValues and server-wide variables
  reactiveVals <- reactiveValues()
  
  reactiveVals$preprocessingShowed <- FALSE
  
  options(spinner.color = "#95AAD3")
  
  spinner <- list(logo = list(spin_loaders(id = 5, color = "#95AAD3")), color="rgb(149, 170, 211, .5)")
  
  # read all server files
  sapply(list.files("server", full.names = TRUE), source, environment())
  
  ## set theme for ggplot ##
  old_theme <- theme_get()
  new_theme <- old_theme + theme(axis.text.x = element_text(size = 12), 
                                 axis.text.y = element_text(size = 12), 
                                 strip.text = element_text(size = 12), 
                                 axis.title.x = element_text(size = 12), 
                                 axis.title.y = element_text(size = 12),
                                 legend.text = element_text(size = 12),
                                 legend.title = element_text(size = 12),
                                 title = element_text(size = 15)
                                 )
  theme_set(new_theme)
  
  shinyBS::updateButton(session, inputId = "previousTab", icon = icon("arrow-left"), style = "success")
  shinyBS::updateButton(session, inputId = "nextTab", icon = icon("arrow-right"), style = "success", disabled = FALSE)
  
  shinyjs::hide("loading")
  waiter_hide()
}