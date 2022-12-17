########################################
## Set dosing and simulation info to be used in mrgsolve
UI_dose_selection <- function() {
  
  tablerCard(solidHeader = TRUE, 
             title="Specify Dosing Regimens", 
             width=12, statusSide = "left", status = "primary",
             collapsible = FALSE,
             closable = FALSE,
             zoomable = FALSE,
             
             
             tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
             
             
             ## Define dosing regimen using Rx label syntax 
             wellPanel(
               fluidRow(shinyjs::useShinyjs(),
                 column(5,
                        fluidRow(
                          column(12, 
                                 textInput("rx_input", label = NULL, value = define_default_rx_input()),
                                 tags$style("#rx_input {font-size:13px; height:35px;}"),
                                 # style = "background-color:yellow;", div(style = "height:0px; max-height:0px;"))
                                 div(style = "height:0px; max-height:0px;"))
                        ), 
                        fluidRow(
                          column(12,
                                 fluidRow(
                                   column(4,
                                          actionButton("rx_submit", label = "Submit dosing regimen")
                                   ),
                                   column(4,
                                          actionButton(inputId = "rx_docs_modal_button", label = "Rx Label Syntax Documentation")
                                   )),
                                 # style = "background-color:green;", div(style = "height:5px;"))
                                 div(style = "height:5px;"))
                        ),
                        fluidRow(
                          column(12,
                                 wellPanel(
                                   rhandsontable::rHandsontableOutput("rx_regimen_hot"),
                                 ),
                                 tags$style("#rx_regimen_hot {font-size:13px;}"),
                                 tags$style("#rx_regimen_hot .highlight {font-size:13px;}"),
                                 # style = "background-color:gray; overflow-y:scroll; max-height: 200px;", div(style = "height:0px;")),
                                 div(style = "height:0px;")),
                          ),
                 ), 
                 column(3,
                        ## Plot Rx Dosing Profile
                        plotOutput("rx_dosing_profile", height = "325px"),
                        # style = "background-color:blue;", div(style = "height:0px;")
                        div(style = "height:0px;")
                 ),
                 column(4,
                        ## Download Button
                        downloadBttn(
                          "download_rx_dose_event_data",
                          label = "Download Rx Dosing Event Dataset (.csv)",
                          style = "bordered",
                          color = "primary",
                          size = "xs",
                          block = FALSE,
                          no_outline = TRUE
                        ),
                        ## display table of dosing event dataset
                        reactable::reactableOutput("dosing_event_data"),
                        tags$style("#dosing_event_data {font-size:13px;}"),
                        tags$style("#dosing_event_data .highlight {font-size:13px;}"),
                        # style = "background-color:orange; max-height:300px", div(style = "height:0px;")
                        div(style = "height:0px;")
                        ),
               )),
             
             hr(),
             
             fluidRow(
               column(3,
                      conditionalPanel(condition = "input.load_model != 0", style = "display: none;",
                                       
                                       ## Update Simulation Options Buttons: tgrid, simulation settings, solver settings, expand Rx data
                                       wellPanel(
                                         h4("Simulation Options", align = "center"),
                                         actionButton("update_tgrid", "Time Grid", class="sim-opts-button"),
                                         br(),
                                         actionButton("update_sim_settings", "Simulation Settings", class="sim-opts-button"),
                                         br(),
                                         actionButton("update_solver_settings", "Solver Settings", class="sim-opts-button"),
                                         br(),
                                         
                                         conditionalPanel(condition = "input.rx_submit != 0",
                                              actionButton("expand_rx_data", "Expand Rx Dataset", class="stepButton"),
                                         ),
                                       )
                      )
               ),  
               column(9,
                      ## Diplay datatable (DT) of full simulation input data
                      conditionalPanel(condition = "input.expand_rx_data != 0", style = "display: none;",
                                       # Download the simulated data
                                       downloadBttn(
                                         "download_full_sim_input_data",
                                         label = "Download Simulation Input Dataset (.csv)",
                                         style = "bordered",
                                         color = "primary",
                                         size = "sm",
                                         block = FALSE,
                                         no_outline = TRUE
                                       ),
                                       DT::dataTableOutput("full_sim_template_data")
                      # ), style = "background-color:red;", div(style = "height:0px;"))       
                      ), div(style = "height:0px;"))
             ),  
             
             ## Modal pop-up windows for updating model parameters & simulation settings
             tablerTabItem(tabName = "select_simulation_options",
                           
                           
                           reorder_modal_popup_children(popup_html =
                                                          bsModal("rx_docs_modal_window", "Rx Dosing Docs", "rx_docs_modal_button", size = "large",
                                                                  fluidRow(column(12,
                                                                                  tags$div(class="user_input_popup",
                                                                                           includeMarkdown('www/mrgsolve_ev_rx_docs.rmd')
                                                                                  )))
                                                          )
                           ),
                           
                           ## update tgrid parameters pop-up window
                           reorder_modal_popup_elements(popup_html =
                                                          bsModal(id = "tgrid_popup", title = "Specify Time Grid", trigger = "update_tgrid", size = "large",
                                                                  fluidRow(column(12,tags$div(class="user_input_popup"),
                                                                                             textInput("tgrid_start", label = "start", value = extract_tgrid_defaults()$start),
                                                                                             withTags(div(class='css_tgrid_end',
                                                                                             textInput("tgrid_end", label = "end", value = extract_tgrid_defaults()$end) )),
                                                                                             withTags(div(class='css_tgrid_end_check',
                                                                                             checkboxInput("tgrid_end_check", label= "modify end", value = FALSE, width = NULL) )),
                                                                                             textInput("tgrid_delta", label = "delta", value = extract_tgrid_defaults()$delta),
                                                                                             textInput("tgrid_add", label = "add", value = extract_tgrid_defaults()$add),
                                                                                             textInput("tgrid_offset", label = "offset", value = extract_tgrid_defaults()$offset),
                                                                                             textInput("tgrid_scale", label = "scale", value = extract_tgrid_defaults()$scale)

                                                                  )))),

                           ## update solver settings pop-up window
                           reorder_modal_popup_elements(popup_html = 
                             bsModal(id = "solver_settings_popup", title = "Specify Solver Settings", trigger = "update_solver_settings", size = "large",
                                     fluidRow(column(6,
                                                     tags$div(class="user_input_popup",
                                                              HTML(Reduce(paste,
                                                                          lapply(1:length(fetch_solver_settings()), function(i) {
                                                                            textInput(
                                                                              inputId = paste0("solver_", names(fetch_solver_settings())[i]),
                                                                              label = span(names(fetch_solver_settings())[i], style="color: #086296; font-weight: bold;"),
                                                                              value = fetch_solver_settings()[i])
                                                                          })))),
                                     )))),
                           
                           ## update simulation settings pop-up window
                           reorder_modal_popup_elements(popup_html = 
                             bsModal(id = "sim_settings_popup", title = "Specify Simulation Settings", trigger = "update_sim_settings", size = "large",
                                     fluidRow(column(6,
                                                     textInput(
                                                       inputId = "nsubj_per_regimen",
                                                       label = span("Specify number of subjects", style="color: #086296; font-weight: bold;"),
                                                       value = fetch_default_sim_settings()$num_subj
                                                     ),
                                                     textInput(
                                                       inputId = "nsim_iterations",
                                                       label = span("Specify number of simulation iterations", style="color: #086296; font-weight: bold;"),
                                                       value = fetch_default_sim_settings()$num_sims
                                                     ),
                                                     textInput(
                                                       inputId = "random_seed",
                                                       label = span("Specify random seed", style="color: #086296; font-weight: bold;"),
                                                       value = fetch_default_sim_settings()$sim_seed
                                                     ),
                                     )))),
             ),  ## tablerTabItem 
             
             
  )
}
