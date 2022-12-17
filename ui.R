###############################################################################|
## shiny app dependent packages & versions -------------------------------------
###############################################################################|
## shiny = 1.7.2
## tablerDash = 0.1.0
## shinyWidgets = 0.7.1
## shinyFeedback = 0.4.0
## shinyBS = 0.61.1
## shinyjs = 2.1.0
## rhandsontable = 0.3.8
## reactable = 0.3.0
## htmlwidgets = 1.5.4
## plotly = 4.10.0
## bs4Dash = 2.1.0
## mrgsolve = 1.0.0
## dplyr = 1.0.9
## purrr = 0.3.4
## data.table = 1.14.2
## DT = 0.23
## ggplot2 = 3.3.6
## flux = 0.3-0.1
## markdown = 1.1
## rmarkdown = 2.14
## ggpubr = 0.4.0
## matrixcalc = 1.0-5
## assertr = 2.8
## tidyr = 1.2.0
## tinytex = 0.40
###############################################################################|
library(shiny)
library(tablerDash)
library(shinyWidgets)
library(shinyFeedback)
library(shinyBS)
library(shinyjs)
library(rhandsontable)
library(reactable)
library(htmlwidgets)
library(plotly)
library(bs4Dash)
library(mrgsolve)
library(dplyr)
library(purrr)
library(data.table)
library(DT)
library(ggplot2)
library(flux)
library(markdown)
library(rmarkdown)
library(ggpubr)
library(matrixcalc)
library(assertr)
library(tidyr)
library(tinytex)


## install required Tex Packages
if (!tinytex::check_installed('draftwatermark')) {
  tinytex::install_tinytex()
  tinytex::tlmgr_install("draftwatermark")
}



## Source all files with R functions to load them into the current environment
file.sources = list.files("./R",full.names = T)
sapply(file.sources, source, .GlobalEnv)

## initialize reactlog???
if (isTRUE(run_app_with_reactlog())) {
  reactlog::reactlog_enable()
  shiny::reactlogReset()
}


ui = tablerDashPage(
  loading_duration = 3,
  title = "Regeneron PMX Simulations",
  navbar = tablerDashNav(
    id = "mymenu",
    src = "https://upload.wikimedia.org/wikipedia/commons/thumb/4/48/Regeneron_logo.svg/2560px-Regeneron_logo.svg.png",
    
    ## Dyanmic simulation step list:  top/left-aligned notification window
    tags$style("#shiny-notification-sim_steps_notification {position:fixed; top: calc(0%); left: calc(0%); width: 200px; border: 2px solid rgba(60, 60, 60, 0.5); line-height: 1.25; max-height: 108px; padding-top: 7px; padding-right: 0px; padding-bottom: 0px; padding-left: 5px; margin: 3px;}"),
    
    ## Loaded model message:  top/center-aligned notification window
    tags$style("#shiny-notification-loaded_model_notification {position:fixed; top: 0; bottom: unset; left: 0; right: 0; max-width: 350px; width = 100%; margin-left: auto; margin-right: auto; line-height: 1.5; border: 2px solid rgba(60, 60, 60, 0.5); padding-top: 5px; padding-right: 15px; padding-bottom: 5px; padding-left: 15px; margin-top: 3px;}"),
    
    ## font-awesome.css tag for calling icons
    tags$style("@import url(https://use.fontawesome.com/releases/v6.0.0/css/all.css);"),
    
    tablerDropdown(
      tablerDropdownItem(
        title = "Notifications",
        status = "success",
        date = Sys.time(),
        "Welcome to the PMX simulation engine (beta version)! 
          This shiny app is still under development. 
          Check confluence for new releases!")),
    
    navMenu = tablerNavMenu(
      tablerNavMenuItem(
        tabName = "profilecard",
        icon("bars"),
        "Dashboard"),
      
      tablerNavMenuItem(
        tabName = "model",
        icon("fa-regular fa-folder-open"),
        "Model Selection"),
      
      tablerNavMenuItem(
        tabName = "siminfo",
        icon("sliders"),
        "Simulation Details"),
      
      tablerNavMenuItem(
        tabName = "output",
        icon("chart-line"),
        "Simulation Output"),
      
      tablerNavMenuItem(
        tabName = "report",
        icon("clipboard"),
        "Report Generation"),
      
      tablerNavMenuItem(
        tabName = "feedback",
        icon("fa-regular fa-comment"),
        "Feedback"),
      
      tablerNavMenuItem(
        tabName = "documentation",
        icon("fa-regular fa-book"),
        "Documentation")
      
    )
  ),
  
  tablerTabItems(
    tablerTabItem(tabName = "profilecard", UIdashboard()),
    tablerTabItem(tabName = "model", UI_model_selection()),
    tablerTabItem(tabName = "siminfo", UI_dose_selection()),
    tablerTabItem(tabName = "output", UIoutput()),
    tablerTabItem(tabName = "report", UIreport()),
    tablerTabItem(tabName = "feedback", UIfeedback()),
    tablerTabItem(tabName = "documentation", UIdocumentation())
  )
)
