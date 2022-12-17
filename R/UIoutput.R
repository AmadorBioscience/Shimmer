##################################
## Model output tab
UIoutput <- function() {
  
  tagList(
    
    #ryan added here
    actionBttn(
      "do",
      label = "Start simulation",
      icon = icon("play"),
      style = "simple",
      color = "primary",
      size = "m",
      block = TRUE,
      no_outline = TRUE),
    #ryan added here end
    
    hr(),      
    
    fluidRow(
      tablerCard(solidHeader = TRUE, status = "primary",collapsible = F,closable=FALSE, title="User Settings & Output Metrics",width=4, statusSide = "top",
                 
                 
                 headerPanel("Plotting Options"),
                 
                 
                 
                 # AESTHETIC
                 tablerCard(solidHeader = FALSE,title = h4("Aesthetic", align="center"),
                            solidHeader = T,
                            collapsible = T,
                            closable=F,
                            collapsed=T,
                            zoomable = F,
                            width = 12, height = 300,
                            background = 'blue', status = "primary", statusSide = "left",
                            
                            column(8, 
                                   sliderInput("slider1_text", label = h4("Text Size"), min = 0, max = 24, value = 14)),
                            
                            column(8, 
                                   sliderInput("slider2_alpha", label = h4("Transparency"), min = 0, max = 1, value = 0.5))
                            
                 ),
                 
                 
                 
                 
                 
                 
                 tablerCard(solidHeader = FALSE,title = h4("Model Options", align="center"),
                            solidHeader = T,
                            collapsible = T,
                            closable=F,
                            collapsed=T,
                            zoomable = F,
                            width = 12, height = 300,
                            background = 'blue', status = "primary", statusSide = "left",
                            
                            
                            column(8,
                                   selectInput('variability', label="Show Prediction interval", c("10%-90%","5%-95%","25%-75%","No variability")),
                                   selectInput('units', label="Concentration units", c("ug/L","mg/L","nM")),
                                   selectInput('select_output_var', choices=NULL, label="Dependent Variable")
                            )),
                 
                 
                 
                 tablerCard(solidHeader = FALSE,collapsed=T,collapsible = T,title= h4("Add user data") ,width=12, height=400, background = 'blue', status = "primary", statusSide = "left", closable = FALSE,
                            
                            column(8,  HTML('<b>Include user data</b>
                                <br>Column headers should minimally include the following columns: <em>ID,CONCENTRATION,TIME</em><br>
                                <br>A <em>DOSE</em> column can be added to stratify the data.</br>
                                ')),
                            
                            HTML('<br></br>'),
                            column(8,
                                   # Input: Select a file ----
                                   fileInput("user_data_upload", "Choose CSV File",
                                             multiple = TRUE,
                                             accept = c("text/csv",
                                                        "text/comma-separated-values,text/plain",
                                                        ".csv"))
                            ),
                            column(8,
                                   # Input: Select separator from user dataset ----
                                   radioButtons("sep", "Separator",
                                                choices = c(Comma = ",",
                                                            Semicolon = ";"),
                                                selected = ",")
                            )),
                 
                 
                 
                 
                 
                 
                 tablerCard(solidHeader = FALSE,title = h4("User Data Options", align="center"),
                            solidHeader = T,
                            collapsible = T,
                            closable=F,
                            collapsed=T,
                            zoomable = F,
                            width = 12, height = 300,
                            background = 'blue', status = "primary", statusSide = "left",
                            
                            
                            column(8,
                                   HTML('<b>Requires uploaded user data</b>'),
                                   selectInput('plot_user_dv', NULL, label="Dependent Variable"),
                                   selectInput('plot_user_facet', NULL, label="Facet Variable")
                            )
                 ),
                 
                 
                 hr(),                 
                 
                 
                 # Summary Statistics
                 headerPanel("Summary Statistics"),
                 zoomable = FALSE,
                 sliderInput("sumstat_interval_slider", "Interval", value = c (0,100), min = 0, max = 100),
                 tableOutput('sumstattable'),
                 
                 style = "overflow-y:scroll; overflow-x:scroll; max-height: 700px;", div(style = "height:0px;"),
                 
                 
      ),
      
      
      # PK PROFILES
      tablerCard(solidHeader = TRUE, status = "primary",collapsible = F,title="Pharmacokinetic Profile Plots",
                 width=8, statusSide = "top",
                 closable = FALSE,
                 zoomable = FALSE,
                 # overflow = TRUE,
                 
                 div(class='css_plotting_options',radioGroupButtons(
                   inputId = "plotting_options",
                   choiceNames = c("Linear", "Log", "Both"),
                   choiceValues = c ("linear","log", "both"),
                   individual = TRUE
                 )),
                 
                 downloadBttn(
                   "downloadData",
                   label = "Download the simulated dataset (.csv)",
                   style = "bordered",
                   color = "primary",
                   size = "s",
                   block = FALSE,
                   no_outline = TRUE
                 ),
                 
                 
                 plotOutput("PKplot", width="1100px", height="1800px"),
                 style = "overflow-y:scroll; overflow-x:scroll; max-height: 700px;", div(style = "height:0px;"),
                 style = "overflow-x:scroll; overflow-x:scroll; max-height: 700px;", div(style = "height:0px;"),),
    ),
    
  )
  
  
  
  
  
  
  
}


