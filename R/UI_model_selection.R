#####################################
## Set all model parameters
UI_model_selection <- function() {
  
  tablerCard(solidHeader = TRUE, title="Select Model", width=12, statusSide = "left", status = "primary",
             collapsible = FALSE,
             closable = FALSE,
             zoomable = FALSE,
             
             
             tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
             
             shinyFeedback::useShinyFeedback(),
  
  fluidRow(
    column(3, wellPanel(
      
      ## Model Selection Dropdown List
      h4("Select Model File"),
      selectInput('model_selection_method', label = NULL, choices = c("Select from Model Library", "Upload Custom Model File"), selected = "Select from Model Library"),
      
      conditionalPanel(condition = "input.model_selection_method == 'Select from Model Library'",
                       
                       h4("Select Model from Library"),
                       selectInput('model_selection', label = NULL, choices = modify_modlist(first = "popPK_2cmt_linear-mm_iv-sc", drop = drop_custom_modlist_models()), selected = ""),
                       
      ),
      
      ## Upload model file button
      conditionalPanel(condition = "input.model_selection_method == 'Upload Custom Model File'",
                       
                       h4("Upload Custom Model File"),
                       fileInput("upload_model", label = NULL, buttonLabel = "Upload", multiple = FALSE, accept = ".cpp", placeholder = "Upload mrgsolve '.cpp' model file"),
                       
      ),
      
      br(),
      
      ## Load model file button
      actionButton("load_model", label = "Load Model"),
      
      hr(),
      
      conditionalPanel(condition = "input.load_model != 0", style = "display: none;",
                       
                       ## Update Model Parameters Buttons: Fixed & Random Effects
                       wellPanel(
                         h4("Model Parameters", align = "center"),
                         actionButton("fixed_params_modal_button", "Fixed Effects", class="sim-opts-button", icon = icon("user", class="fa-solid fa-user-lock", lib="font-awesome")),
                         br(),
                         actionButton("random_effects_modal_button", "Random Effects", class="sim-opts-button", icon = icon("users", class="fa fa-users fa-fw", lib="font-awesome")),
                         br(),
                       ),
      )
      
    )),
    
    column(9, wellPanel(
      
      ## Display filename of currently loaded model
      conditionalPanel(
        condition = "input.load_model != 0", style = "display: none;",
        h5("Currently loaded model"),
        verbatimTextOutput("model_name"),
      ),
      
      ## Load Library Model & Display Model Code
      conditionalPanel(condition = "input.load_model != 0",
                       h5("Model Specification File"),
                       verbatimTextOutput("model_code"),
                       tags$head(tags$style("#model_code{color:black; font-size:12px; overflow-y:scroll; max-height: 550px; background: ghostwhite;}"))
      ))),
  ),
  
  
  ## Modal pop-up windows for updating model parameters & simulation settings
  tablerTabItem(tabName = "update_model_parameters",
                
                ## update fixed parameter pop-up window
                uiOutput("fixed_params_modal_uiout"),
                
                ## update random effect matrices pop-up window
                reorder_modal_popup_children(popup_html = 
                bsModal(id = "random_effects_modal_window", title = "Select Simulated Random Effects", trigger = "random_effects_modal_button",
                        size = "large",
                        tags$div(class="user_input_popup",
                                 radioButtons("random_effects_selected", "Variability Options",
                                              c("None", "Interindividual Variability", "Residual Variability", "Both"), "None", inline = TRUE, width = "100%"),
                                 conditionalPanel("input.random_effects_selected == 'Interindividual Variability' || input.random_effects_selected == 'Both'",
                                                  h5("Interindividual Variability - Correlation Matrix"),
                                                  rHandsontableOutput("omega_matrix_hot"),
                                                  br()
                                 ),
                                 conditionalPanel("input.random_effects_selected == 'Residual Variability' || input.random_effects_selected == 'Both'",
                                                  h5("Residual Variability - Correlation Matrix"),
                                                  rHandsontableOutput("sigma_matrix_hot"),
                                                  br()
                                 ),
                                 conditionalPanel("input.random_effects_selected != 'None'",
                                                  br(),
                                                  column(6,
                                                         actionButton("check_valid_re_matrices", "Check Matrices", icon = icon("check-matrix-calc", class="fas fa-calculator", lib="font-awesome")),
                                                  ),
                                                  br(),
                                                  )
                        )
                        )),
  ),  ## tablerTabItem
  
  
  )
}


