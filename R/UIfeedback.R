UIfeedback <-  function() {
  tablerCard(
    width=12,title="Please submit feedback", solidHeader = TRUE, statusSide = "left", status = "primary",
    collapsible = FALSE,
    closable = FALSE,
    zoomable = FALSE,
    
    fluidRow(
      column(4,  HTML("What is your name?")),
      column(8,textInput("text1", placeholder="Enter text...",label=NULL))
    ),
    fluidRow(
      column(4,  HTML("How are you liking the UI?")),
      column(8,radioGroupButtons("text2",choices = c(1:10),label=NULL,size = "sm", status = "info", individual = TRUE, justified = TRUE))
    ),
    # Set compound name
    fluidRow(
      column(4,  HTML("What additional models would you like?")),
      column(8,textInput("text3", placeholder="Enter text...",label=NULL))
    ),
    # Add description text to the report
    fluidRow(
      column(4,  HTML("Please submit any additional feedback...")),
      column(8,  textAreaInput("text4", placeholder= "Enter text...", height=150,width = "100%",label=NULL))
    ),
    
    # Start report rendering
    fluidRow(
      column(6,  actionBttn("submit", "Submit",icon = icon("paper-plane"),style = "unite", color = "primary",size = "s", block = FALSE,no_outline = TRUE))
      
    )
    
  )
    
}

