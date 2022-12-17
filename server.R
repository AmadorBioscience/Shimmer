## Shiny server code
server <- function(input, output, session) {
  
  #############################################################################|
  ########### Update user data options dropdown based on user uploaded data ----
  df_unfiltered <- eventReactive(input$user_data_upload,{
    print_function_calls(fn_name = "df_unfiltered", fn_type = "eventReactive")
    return (read.csv(input$user_data_upload$datapath))
  })
  
  
  observeEvent(df_unfiltered(),{
    updateSelectInput(session, "plot_user_dv", label = "Select Plot DV User",
                      selected = "TIME",
                      choices = colnames_numint(df_unfiltered()))
  })

  observeEvent(df_unfiltered(),{
    updateSelectInput(session, "plot_user_facet", label = "Facet Variable",
                      selected = "AMT",
                      choices = colnames_numint(df_unfiltered()))
  })
  
  #############################################################################|
  ########## Update Dependent Variable list for the current model selection ----
  observeEvent(input$load_model, {
    updateSelectInput(session, "select_output_var", label = "Select Plot DV",
                      choices = list_valid_output_vars_fn(mod = load_model_file()),
                      selected = default_model_output_var(mod = load_model_file()))
  })
  
  
  #############################################################################|
  ################ Format YNAME/YDATA in sim_out_data with user selected DV ----
  
  # select_sim_output_variable adds the user selected dependent variable to the sim_out_data as YNAME/YDATA
  select_sim_output_variable <- eventReactive(input$do, label = "select_sim_output_variable <- eventReactive(input$do)", {
    print_function_calls(fn_name = "select_sim_output_variable", fn_type = "eventReactive")
    if (is.null(load_model_file())) return()
    
    select_sim_output_variable_fn(df = run_sim_series(), var = input$select_output_var)
  })
  
  #############################################################################|
  ################# Run mrgsolve simulation upon button click ------------------
  
  # DF_Simulation holds simulated data
  DF_Simulation <- eventReactive(input$do, label = "DF_Simulation <- eventReactive(input$do)", {
    print_function_calls(fn_name = "DF_Simulation", fn_type = "eventReactive")
    if (is.null(load_model_file())) return()
    if (is.null(rx_multi_dose_regs())) return()
    
    select_sim_output_variable()
  })
    
  #############################################################################|
  ################################### Output - Graph ---------------------------
  ### Calculate prediction intervals based on user input
  
  # prediction_intervals holds simulated prediction intervals
  prediction_intervals <- reactive(label = "prediction_intervals <- reactive()", {
    print_function_calls(fn_name = "prediction_intervals", fn_type = "reactive")
    if (is.null(load_model_file())) return()
    if (is.null(rx_multi_dose_regs())) return()
    
    rx_pi_function(DF_Simulation(), isolate(input))
  })
  
  # lin_log_plots is a ggplot object of the simulated PK profiles
  lin_log_plots <- eventReactive({input$do} , label = "lin_log_plots <- eventReactive(input$do)", {
    print_function_calls(fn_name = "lin_log_plots", fn_type = "eventReactive")
    if (is.null(load_model_file())) return()
    if (is.null(rx_multi_dose_regs())) return()
    
    rx_graph_function(prediction_intervals(), isolate(input),  mod = load_model_file())
  })
  
  ## Show the plot 
  create_pk_plot <- eventReactive({input$do;input$plotting_options} , label = "create_pk_plot <- eventReactive(input$do)", {
    print_function_calls(fn_name = "create_pk_plot", fn_type = "eventReactive")
    if (is.null(load_model_file())) return()
    if (is.null(rx_multi_dose_regs())) return()
    
    res <- lin_log_plots()
    
    if (input$plotting_options == "linear"){res$lin}
    else if (input$plotting_options == "log"){res$log}
    else{res$comb}
  })
  
  
  output$PKplot <- renderPlot({
    print_function_calls(fn_name = "output$PKplot", fn_type = "renderPlot")
    create_pk_plot()
  })
  
  #############################################################################|
  ################################### Output - Table ---------------------------
  ### Calculate summary statistics
  
  # sum_stats holds the summary statistics of the simulation per dose
  
  # Updating UI slider for summary statistic once expand rx data set is clicked
  observeEvent(input$expand_rx_data, {
    updateSliderInput(session, "sumstat_interval_slider", label = "Interval",
                      min = input$tgrid_start, max = input$tgrid_end, 
                      value = c (input$tgrid_start, input$tgrid_end))
  })
  
  sum_stats <- reactive(label = "sum_stats <- reactive()", {
    print_function_calls(fn_name = "sum_stats", fn_type = "reactive")
    if (is.null(load_model_file())) return()
    if (is.null(rx_multi_dose_regs())) return()
    input$sumstat_interval_slider
    rx_numeric_stats_function(df = DF_Simulation(), 
                              tgrid_end = input$sumstat_interval_slider[2], 
                              tgrid_start = input$sumstat_interval_slider[1], 
                              units = input$units, 
                              sim_end_time = NULL)
  })

  
  calc_sim_end_time <- reactive(label = "calc_sim_end_time <- reactive()", { 
    
    print_function_calls(fn_name = "calc_sim_end_time", fn_type = "reactive") 
    adjust_time_grid_dosing_fn(dose_data = rx_multi_dose_regs(), 
                               tg_input = build_time_grid_object(), 
                               user_spec = input$tgrid_end_check)
  })
  
  observeEvent({input$tgrid_end_check;calc_sim_end_time()},{
    
    print_function_calls(fn_name = "disable button", fn_type = "ObserveEvent(input$tgrid_end_check;calc_sim_end_time())")
    
    if (input$tgrid_end_check){
      shinyjs::enable("tgrid_end")
    }
    else {
      updateTextInput(session, inputId = "tgrid_end", value = calc_sim_end_time()@end)
      shinyjs::disable("tgrid_end")
      }
  })
  
  ## Show the summary stats table 
  output$sumstattable <- renderTable({
    print_function_calls(fn_name = "output$sumstattable", fn_type = "renderTable")
    if (input$do == 0) return()
    sum_stats()
  }, align='c', bordered = TRUE)
  
  #############################################################################|
  ############# Downloadable csv of generated PK dataset -----------------------
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Simulated_dataset",input$cmp_name,".csv",sep="") # File name with compound name (if inserted in report tab)
    },
    content = function(file) {
      write.csv(DF_Simulation(), file, row.names = FALSE)
    }
  )
  
  #############################################################################|
  ############# Generate .pdf report using rmarkdown ---------------------------
  
  output$report <- downloadHandler(
    filename = function() {
      paste("popPK-simulation-report_", Sys.Date(), ".pdf", sep="") # File name
    },
    content = function(file) {
      
      if (is.null(load_model_file())) return()
      if (is.null(rx_multi_dose_regs())) return()
      
      ## handle OS-dependent temporary filepath
      temp_filepath <- format_temp_file_path(file = file)
      
      ## copy rmd report files to temporary directory before knitting report
      temp_dir <- dirname(temp_filepath)
      
      if (input$report_watermark){tempReport <- copy_file_to_temp_dir(temp_dir, target_file = "reports/report.Rmd")}
      else {tempReport <- copy_file_to_temp_dir(temp_dir, target_file = "reports/report2.Rmd")}
      
      tempLogo <- copy_file_to_temp_dir(temp_dir, target_file = "./www/Logo.png")
      
      if (!is.null(load_model_file()) && !is.null(rx_multi_dose_regs())) {
        
        ## convert data structure for multiple Rx dosing regimens to a list
        rx_regs <- rx_table$df_regs
        rx_regs_nms <- setNames(rx_regs$RXLABEL, rx_regs$REGLAB)
        
        
        ## construct rmarkdown (.rmd) input parameter list  
        params_for_rmd <- construct_rmd_param_list(
          mod = update_model(), dose_data = rx_multi_dose_regs(), tg_input = calc_sim_end_time(), 
          sum_stats = sum_stats(), sim_plots = lin_log_plots(), title = input$title, 
          author = input$author, compound = input$cmp_name, description = input$description, 
          units = input$units, rx_input = rx_regs_nms, modup_params = update_params(), 
          sim_settings = update_sim_settings(), model_code = fetch_model_code()
        )
        
        ## generate parameterized rmarkdown report in new environment
        rmarkdown::render(tempReport, output_file = temp_filepath,
                          params = params_for_rmd,
                          envir = new.env(parent = globalenv())
        )
      } 
    }    
  )
  
  #############################################################################|
  ############# Save Feedback to Saved.CSV file --------------------------------
  save_feedback_csv <- observeEvent(input$submit, label = "save_feedback_csv <- observeEvent(input$submit)", {
    print_function_calls(fn_name = "save_feedback_csv", fn_type = "observeEvent")
    feedback_file <- file.path("data", "saved.csv")
    feedback_data <- data.frame(input$text1, input$text2, input$text3, input$text4, Sys.time())
    if (!file.exists(feedback_file)) {
      write.table(feedback_data, sep = ",", file = feedback_file, row.names = FALSE, col.names=c("Name", "Do you like the UI?", "Additional Models?","Additional Comments","Date/Time"), append = FALSE)
    } else {
      write.table(feedback_data, sep = ",", file = feedback_file, row.names = FALSE, col.names=FALSE, append = TRUE)
    }
  })
  
  #############################################################################|
  ############# Modal Dialog for Feedback --------------------------------------
  observeEvent(input$submit, label = "submit_feedback <- observeEvent(input$submit)", {
    print_function_calls(fn_name = "submit_feedback", fn_type = "observeEvent")
    showModal(modalDialog(
      title = "Thank you for your feedback.",
      easyClose = TRUE
    ))
  })
  
  #############################################################################|
  ## Simulation Steps ----------------------------------------------------------
  #############################################################################|
  
  sim_steps <- reactiveValues(
    load_model = TRUE,
    specify_rx_dosing = TRUE,
    expand_rx_dataset = TRUE,
    run_simulation = TRUE
  )
  
  update_sim_step_list <- reactive(label = "update_sim_step_list <- reactive()", {
    print_function_calls(fn_name = "update_sim_step_list_fn", fn_type = "reactive")
    reactive_step_list <- reactiveValuesToList(sim_steps)[names(initial_step_list())]
    curr_steps_in_list <- names(reactive_step_list)[unlist(reactive_step_list)]
    if (identical(curr_steps_in_list, character(0))) return(NULL)
    update_sim_step_list_fn(sim_steps = sim_steps)
  })
  
  observe(label = "sim_steps_notification <- observe()", {
    print_function_calls(fn_name = "sim_steps_notification", fn_type = "observe")
    if (!is.null(update_sim_step_list())) {
      showNotification(update_sim_step_list(), id = "sim_steps_notification", 
                       type = "message", duration = NULL, closeButton = FALSE)
    } else {
      removeNotification(id = "sim_steps_notification")
    }
  })
  
  observeEvent(input$load_model, label = "update_load_model_sim_step <- observeEvent(input$load_model)", {
    print_function_calls(fn_name = "update_load_model_sim_step", fn_type = "observeEvent")
    if(is.null(load_model_file())) return()
    update_load_model_sim_step(sim_steps = sim_steps)
  })
  
  observeEvent(update_rx_table(), label = "update_rx_table_sim_step <- observeEvent(update_rx_table())", {
    print_function_calls(fn_name = "update_rx_table_sim_step", fn_type = "observeEvent")
    if (isTRUE(nrow(rx_table$df_regs) > 0)) {
      update_rx_submit_sim_step(sim_steps = sim_steps)
    }
  })
  
  observe(label = "update_model_params_sim_step <- observe()", {
    print_function_calls(fn_name = "update_model_params_sim_step", fn_type = "observe")
    if(is.null(load_model_file())) return()
    check_re_matrix_update()
    check_sim_options_update()
    check_model_param_update()
    update_model_params_sim_step(sim_steps = sim_steps)
  })
  
  observeEvent(input$expand_rx_data, label = "update_expand_rx_data_sim_step <- observeEvent(input$expand_rx_data)", {
    print_function_calls(fn_name = "update_expand_rx_data_sim_step", fn_type = "observeEvent")
    update_expand_rx_data_sim_step(sim_steps = sim_steps)
  })
  
  observeEvent(input$do, label = "update_do_run_sim_step <- observeEvent(input$do)", {
    print_function_calls(fn_name = "update_do_run_sim_step", fn_type = "observeEvent")
    update_do_run_sim_step(sim_steps = sim_steps)
  })
  
  check_model_param_update <- reactive(label = "check_model_param_update <- reactive()", {
    print_function_calls(fn_name = "check_model_param_update", fn_type = "reactive")
    if(is.null(load_model_file())) return()
    map_chr(build_model_param_value_ids(mod = load_model_file()), ~ input[[.x]] %||% "")
  })
  
  check_re_matrix_update <- reactive(label = "check_re_matrix_update <- reactive()", {
    print_function_calls(fn_name = "check_re_matrix_update", fn_type = "reactive")
    if(is.null(load_model_file())) return()
    input$random_effects_selected
    update_omega$matrix
    update_sigma$matrix
  })
  
  check_sim_options_update  <- reactive(label = "check_sim_options_update <- reactive()", {
    print_function_calls(fn_name = "check_sim_options_update", fn_type = "reactive")
    if(is.null(load_model_file())) return()
    build_time_grid_object()
    update_solver_settings()
    update_sim_settings()
  })
  
  #############################################################################|
  ############# Rx Input Dosing Dataset ----------------------------------------
  #############################################################################|
  
  output$rx_input  <- renderText({ input$rx_input })
  output$rx_submit <- renderText({ input$rx_submit })
  
  rx_table <- reactiveValues(
    df_regs = empty_rx_table()
  )
  
  ## confirm valid Rx input
  valid_rx_input <- eventReactive(input$rx_submit, label = "valid_rx_input <- eventReactive(input$rx_submit)", {
    print_function_calls(fn_name = "valid_rx_input", fn_type = "eventReactive")
    valid_rx_input_fn(rx_input = input$rx_input)
  })
  
  ## build dosing event dataset from user-specified Rx input 
  submit_rx_input <- eventReactive(input$rx_submit, label = "submit_rx_input <- eventReactive(input$rx_submit)", {
    print_function_calls(fn_name = "submit_rx_input", fn_type = "eventReactive")
    submit_rx_input_fn(rx_input = input$rx_input)
  })
  
  rx_dose_data <- eventReactive(input$rx_submit, ignoreNULL = FALSE, label = "rx_dose_data <- eventReactive(input$rx_submit, ignoreNULL = FALSE)", {
    print_function_calls(fn_name = "rx_dose_data", fn_type = "eventReactive")
    if (input$rx_submit == 0) return()
    rx_dose_data_fn(is_valid_rx = valid_rx_input(), submit_rx = submit_rx_input())
  })
  
  observeEvent(input$rx_regimen_hot$changes$changes, ignoreNULL = TRUE, priority = 10, label = "update_rx_regimen_hot <- observeEvent()", {
    print_function_calls(fn_name = "update_rx_regimen_hot", fn_type = "observeEvent")
    
    if (input$rx_submit > 0) {
      rx_table <- modify_rx_table_regimen_fn(hot_inp_chgs = input$rx_regimen_hot$changes$changes, 
                                             rx_table = rx_table)
    } 
  })
  
  observeEvent(input$rx_submit, ignoreNULL = TRUE, priority = 10, label = "add_new_rx_regimen_hot <- observeEvent()", {
    print_function_calls(fn_name = "add_new_rx_regimen_hot", fn_type = "observeEvent")
    
    if (input$rx_submit > 0 && !identical(class(try(rx_dose_data())), "try-error")) {
      
      ## add conditional logic that verifies unique `rx_input` string before appending dosing regimen information
      curr_reg_numid <- extract_rx_regimen_id()
      
      ## add new rx regimen to `rx_table`
      rx_table$df_regs <- add_new_rx_regimen(df_regs = rx_table$df_regs, rx_label = input$rx_input)
    }
  })
  
  extract_rx_regimen_id <- reactive(label = "extract_rx_regimen_id <- reactive()", {
    print_function_calls(fn_name = "extract_rx_regimen_id", fn_type = "reactive")
    max_plus_1(rx_table$df_regs$REGNUM)
  })
  
  update_rx_table <- reactive(label = "update_rx_table <- reactive()", {
    print_function_calls(fn_name = "update_rx_table", fn_type = "reactive")
    rx_table$df_regs
  })
  
  rx_multi_dose_regs <- eventReactive(update_rx_table(), ignoreInit = FALSE, ignoreNULL = TRUE, label = "rx_multi_dose_regs <- eventReactive(input$rx_regimen_hot)", {
    print_function_calls(fn_name = "rx_multi_dose_regs", fn_type = "eventReactive")
    print_obj_to_console(isolate(input$rx_regimen_hot$changes$changes))
    
    if (input$rx_submit == 0) return()
    if (isTRUE(nrow(rx_table$df_regs) %in% 0)) return()
    
    sim_rx_regs <- rx_table$df_regs %>% filter(Include)
    
    if (nrow(sim_rx_regs) > 0 && !isTRUE(unique(purrr::map_lgl(sim_rx_regs %>% pull(RXLABEL), valid_rx_input_fn)))){
      validate("Invalid Rx specification!", errorClass = "invalid-input")
    }
    
    derive_rx_multi_reg_data()
  })
  
  derive_rx_multi_reg_data <- reactive(label = "derive_rx_multi_reg_data <- reactive()", {
    print_function_calls(fn_name = "derive_rx_multi_reg_data", fn_type = "reactive")
    
    if (isTRUE(nrow(rx_table$df_regs) %in% 0)) return()
    rx_regs_info <- unnest_rx_table_dose_data(rx_table$df_regs)
    print_obj_to_console(rx_regs_info)
    rx_regs_info
  })
  
  output$rx_regimen_hot <- rhandsontable::renderRHandsontable({
    ## https://jrowen.github.io/rhandsontable/#Custom_Renderer
    ## https://jrowen.github.io/rhandsontable/#custom_renderer_using_r
    ## https://stackoverflow.com/questions/58142090/color-a-whole-row-in-rhandsontable-based-on-a-string-value-in-one-column
    
    if (isTRUE(print_console_msgs())) {
      cat(">>>>>   START   <<<<<\noutput$rx_regimen_hot <- renderRHandsontable")
      print(rx_table$df_regs)
      cat("output$rx_regimen_hot <- renderRHandsontable\n>>>>>   END   <<<<<")
    }
    
    if (!isTRUE(nrow(rx_table$df_regs) > 0)) return(NULL)
    
    if (isTRUE(nrow(rx_table$df_regs) > 0)) {
      rhandsontable::rhandsontable(rx_table$df_regs %>% select(-RXDOSE),
                                   rowHeaders = NULL, 
                                   height = 225) %>% 
        rhandsontable::hot_col(col = "Include",
                               renderer = "function(instance, td, row, col, prop, value, cellProperties) {
                                              td.style.textAlign = 'center';
                                              Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
                                              return td;
                                            }") %>%
        rhandsontable::hot_col("REGNUM", readOnly = TRUE) %>% 
        rhandsontable::hot_col(c("REGNUM", "REGLAB", "Include"), halign = "htCenter") %>% 
        rhandsontable::hot_cols(colWidths = c(60, 60, 50, 560)) %>% 
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, 
                                 contextMenu = FALSE, overflow = "hidden",
                                 stretchH = "all")
      }
  })
  
  outputOptions(output, "rx_regimen_hot", suspendWhenHidden = FALSE)
  
  #############################################################################|
  ####### Table of Rx dosing dataset -------------------------------------------
  
  output$dosing_event_data <- reactable::renderReactable({
    print_function_calls(fn_name = "output$dosing_event_data", fn_type = "renderReactable")
    if (input$rx_submit == 0) return()
    build_dosing_event_dataset()
  })
  
  build_dosing_event_dataset <- eventReactive(update_rx_table(), ignoreNULL = TRUE, ignoreInit = FALSE, 
    label = "build_dosing_event_dataset <- eventReactive(update_rx_table())", {
      print_function_calls(fn_name = "build_dosing_event_dataset", fn_type = "eventReactive")
      
      if (is.null(rx_multi_dose_regs())) return()
      
      ## source:  https://glin.github.io/reactable/articles/examples.html#no-pagination
      reactable::reactable(rx_multi_dose_regs() %>% select(-RXLABEL), pagination = FALSE, highlight = TRUE, bordered = TRUE, wrap = FALSE,
                           height = 300,
                           compact = TRUE, fullWidth = TRUE,
                           defaultColDef = reactable::colDef(minWidth = 40,
                                                             style = "font-size:13px;", headerStyle = "font-size:13px;"),
                           columns = list(REGNUM = reactable::colDef(minWidth = 55), REGLAB = reactable::colDef(minWidth = 60)))
    })
  
  
  #############################################################################|
  ####### Plot Rx multiple dosing regimen profiles -----------------------------
  
  sim_dosing_dataset <- eventReactive(update_rx_table(), ignoreNULL = TRUE, ignoreInit = FALSE, 
                                      label = "sim_dosing_dataset <- eventReactive({input$load_model, input$rx_submit})", {
    print_function_calls(fn_name = "sim_dosing_dataset", fn_type = "eventReactive")
    
    if (is.null(load_model_file())) return()
    if (input$rx_submit == 0) return()
    if (is.null(rx_multi_dose_regs())) return()
    if (isTRUE(nrow(rx_multi_dose_regs()) %in% 0)) return()
    sim_dosing_data <- preview_rx_input_profile(ev_data = rx_multi_dose_regs(), mod = load_model_file())
    if (is.null(sim_dosing_data)) return()
    
    plot_sim_model_ev_data_dosing_regimen(mod_sim_data = sim_dosing_data)
  })
  
  output$rx_dosing_profile <- renderPlot({
    print_function_calls(fn_name = "output$rx_dosing_profile", fn_type = "renderPlot")
    if (input$rx_submit == 0) return()
    sim_dosing_dataset()
  }, res = 90)
  
  #############################################################################|
  ####### Load model ".cpp" file -----------------------------------------------
  
  load_model_file <- eventReactive(input$load_model, ignoreNULL = FALSE, label = "load_model_file <- eventReactive(input$load_model, ignoreNULL = FALSE)", {
    print_function_calls(fn_name = "load_model_file", fn_type = "eventReactive")
    if (input$load_model == 0) return()
    model_file <- read_model_file_fn(model_selection = input$model_selection, upload_model = input$upload_model, model_selection_method = input$model_selection_method)
    if (is.null(model_file)) return()
    collapse_re(model_file, show_matrices = FALSE)
  })
  
  fetch_model_code <- reactive({
    print_function_calls(fn_name = "fetch_model_code", fn_type = "reactive")
    if (is.null(load_model_file())) return()
    fetch_model_code_fn(mod = load_model_file())
  })
  
  fetch_model_name <- reactive({
    print_function_calls(fn_name = "fetch_model_name", fn_type = "reactive")
    if (is.null(load_model_file())) return()
    fetch_model_name_fn(mod = load_model_file())
  })
  
  output$model_name <- renderText({ 
    print_function_calls(fn_name = "output$model_name", fn_type = "renderText")
    fetch_model_name()
  })
  
  output$model_code <- renderText({ 
    print_function_calls(fn_name = "output$model_code", fn_type = "renderText")
    fetch_model_code()
  })
  
  loaded_model_msg <- reactive(label = "loaded_model_msg <- reactive()", { 
    print_function_calls(fn_name = "loaded_model_msg", fn_type = "reactive")
    loaded_model_msg_fn(model_name = fetch_model_name())
  })
  
  observeEvent(input$load_model, label = "display_loaded_model_msg <- observeEvent(input$load_model)", {
    if (!is.null(load_model_file())) {
      showNotification(HTML(loaded_model_msg()), id = "loaded_model_notification",
                       type = "message", duration = NULL, closeButton = FALSE)
    }
  })

  #############################################################################|
  ####### Update Simulation Options via modal windows --------------------------
  #############################################################################|
  
  build_time_grid_object <- reactive(label = "build_time_grid_object <- reactive()", {
    print_function_calls(fn_name = "build_time_grid_object", fn_type = "reactive")
    
    ## extract user-specified time grid parameters
    start  <- input_as_numeric(input$tgrid_start)
    end    <- input_as_numeric(input$tgrid_end)
    delta  <- input_as_numeric(input$tgrid_delta)
    add    <- input_as_numeric(input$tgrid_add)
    offset <- input_as_numeric(input$tgrid_offset)
    scale  <- input_as_numeric(input$tgrid_scale)
    
    tg <- build_time_grid_object_fn(start = start, end = end, delta = delta, add = add, 
                                    offset = offset, scale = scale)
    tg
  })
  
  update_sim_settings <- reactive(label = "update_sim_settings <- reactive()", {
    print_function_calls(fn_name = "update_sim_settings", fn_type = "reactive")
    sim_settings <- update_sim_settings_fn(
      num_subj = input_as_numeric(input$nsubj_per_regimen),
      num_sims = input_as_numeric(input$nsim_iterations),
      sim_seed = input_as_numeric(input$random_seed)
    )
    sim_settings
  })
  
  update_solver_settings <- reactive(label = "update_solver_settings <- reactive()", {
    print_function_calls(fn_name = "update_solver_settings", fn_type = "reactive")
    solver_settings <- update_solver_settings_fn(
      atol     = input_as_numeric(input$solver_atol),
      rtol     = input_as_numeric(input$solver_rtol),
      ss_atol  = input_as_numeric(input$solver_ss_atol),
      ss_rtol  = input_as_numeric(input$solver_ss_rtol),
      maxsteps = input_as_numeric(input$solver_maxsteps),
      hmin     = input_as_numeric(input$solver_hmin),
      hmax     = input_as_numeric(input$solver_hmax),
      ixpr     = input_as_numeric(input$solver_ixpr),
      mxhnil   = input_as_numeric(input$solver_mxhnil)
    )
    solver_settings
  })

  #############################################################################|
  ####### Model Parameter Pop-up windows ---------------------------------------
  #############################################################################|
  
  ## fixed effects pop-up box --------------------------------------------------
  output$fixed_params_modal_uiout <- renderUI ({
    print_function_calls(fn_name = "output$fixed_params_modal_uiout", fn_type = "renderUI")
    fixed_params_modal_uiout_fn(mod = load_model_file())
  })
  
  ## omega matrix --------------------------------------------------------------
  update_omega <- reactiveValues(matrix = as.matrix(0, nrow = 1, ncol = 1))
  
  observeEvent(input$load_model, label = "extract_model_omega_matrix <- observeEvent()", {
    print_function_calls(fn_name = "update_omega$matrix <- fetch_model_omega", fn_type = "observeEvent")
    update_omega$matrix <- fetch_model_omega()
  })
  
  fetch_model_omega <- reactive(label = "fetch_model_omega <- reactive()", {
    print_function_calls(fn_name = "fetch_model_omega", fn_type = "reactive")
    if(is.null(load_model_file())) return()
    convert_model_re_matrices_to_corr(mod = load_model_file())$omega
  })
  
  observeEvent(input$omega_matrix_hot, label = "update_omega_matrix_hot <- observeEvent()", {
    print_function_calls(fn_name = "update_omega$matrix <- omega_matrix_hot", fn_type = "observeEvent")
    update_omega$matrix <- update_re_matrix_hot_fn(re_matrix_hot = input$omega_matrix_hot)
  })
  
  build_omega_table <- eventReactive(input$load_model, label = "build_omega_table <- eventReactive(input$load_model)", {
    print_function_calls(fn_name = "build_omega_table", fn_type = "eventReactive")
    build_re_matrix_table_fn(re_matrix = update_omega$matrix)
  })
  
  output$omega_matrix_hot <- renderRHandsontable({
    print_function_calls(fn_name = "output$omega_matrix_hot", fn_type = "renderRHandsontable")
    build_omega_table()
  }) 
  
  ## sigma matrix --------------------------------------------------------------
  update_sigma <- reactiveValues(matrix = as.matrix(0, nrow = 1, ncol = 1))
  
  observeEvent(input$load_model, label = "extract_model_sigma_matrix <- observeEvent()", {
    print_function_calls(fn_name = "update_sigma$matrix <- fetch_model_sigma", fn_type = "observeEvent")
    update_sigma$matrix <- fetch_model_sigma()
  })
  
  fetch_model_sigma <- reactive(label = "fetch_model_sigma <- reactive()", {
    print_function_calls(fn_name = "fetch_model_sigma", fn_type = "reactive")
    if(is.null(load_model_file())) return()
    convert_model_re_matrices_to_corr(mod = load_model_file())$sigma
  })
  
  observeEvent(input$sigma_matrix_hot, label = "update_sigma_matrix_hot <- observeEvent()", {
    print_function_calls(fn_name = "update_sigma$matrix <- sigma_matrix_hot", fn_type = "observeEvent")
    update_sigma$matrix <- update_re_matrix_hot_fn(re_matrix_hot = input$sigma_matrix_hot)
  })
  
  build_sigma_table <- eventReactive(input$load_model, label = "build_sigma_table <- eventReactive(input$load_model)", {
    print_function_calls(fn_name = "build_sigma_table", fn_type = "eventReactive")
    build_re_matrix_table_fn(re_matrix = update_sigma$matrix)
  })
  
  output$sigma_matrix_hot <- renderRHandsontable({
    print_function_calls(fn_name = "output$sigma_matrix_hot", fn_type = "renderRHandsontable")
    build_sigma_table()
  }) 
  
  ## validate model parameters -------------------------------------------------
  ## just returns NULL so validate_params() never changes and always passes check
  validate_params <- eventReactive(input$expand_rx_data, ignoreNULL = FALSE, label = "validate_params <- eventReactive(input$expand_rx_data, ignoreNULL = FALSE)", {
    print_function_calls(fn_name = "validate_params", fn_type = "eventReactive")
    if (is.null(load_model_file())) return()
    mod <- load_model_file()
    return(NULL)
  })
  
  validate_re_matrices <- eventReactive(input$check_valid_re_matrices, ignoreNULL = TRUE, label = "validate_re_matrices <- eventReactive(input$check_valid_re_matrices, ignoreNULL = TRUE)", {
    print_function_calls(fn_name = "validate_re_matrices", fn_type = "eventReactive")
    if (is.null(load_model_file())) return()
    validate_re_matrices_fn(re_omega = update_omega$matrix, re_sigma = update_sigma$matrix, re_selected = input$random_effects_selected)
  })
  
  check_re_matrices <- observe(label = "check_re_matrices <- observe()", {
    print_function_calls(fn_name = "check_re_matrices", fn_type = "reactive")
    check_re_matrices_fn(is_valid_re = validate_re_matrices())
  })
  
  ## get updated params --------------------------------------------------------
  update_params <- eventReactive(input$expand_rx_data, label = "update_params  <- eventReactive(input$expand_rx_data)", {
    print_function_calls(fn_name = "update_params", fn_type = "eventReactive")
    build_mod_param_info_fn(mod = load_model_file(), input = input)
  }) 
  
  ## update model --------------------------------------------------------------
  ## updated model with updated parameters
  update_model <- eventReactive(input$expand_rx_data, ignoreNULL = FALSE, label = "update_model <- eventReactive(input$expand_rx_data, ignoreNULL = FALSE)", {
    print_function_calls(fn_name = "update_model", fn_type = "eventReactive")
    
    if(!is.null(validate_params())) return()
    if(is.null(load_model_file())) return()
    if (isTRUE(input$expand_rx_data %in% 0)) return()
    
    update_model_fn(mod = load_model_file(), param_info = update_params(), 
                    solver_settings = update_solver_settings(), 
                    omega_mat = update_omega$matrix, sigma_mat = update_sigma$matrix, 
                    re_selected = input$random_effects_selected)
  })
  
  expand_sim_dataset <- reactive(label = "expand_sim_dataset <- reactive()", {
    ## link:  https://stackoverflow.com/questions/44747751/use-withprogress-in-shiny
    print_function_calls(fn_name = "expand_sim_dataset", fn_type = "reactive")
    
    if (!is.null(validate_params())) return()
    if (input$expand_rx_data %in% 0) return()
    
    expand_sim_dataset_fn(mod = update_model(), 
                          dose_data = rx_multi_dose_regs(), 
                          sim_settings = update_sim_settings(), 
                          tg_input = calc_sim_end_time(),
                          end_time_scale_factor = define_tgrid_end_scaling_factor())
  })
  
  get_full_sim_template <- eventReactive(input$expand_rx_data, label = "get_full_sim_template <- eventReactive(input$expand_rx_data)", {
    print_function_calls(fn_name = "get_full_sim_template", fn_type = "eventReactive")
    
    if (input$expand_rx_data == 0) return()
    if (is.null(load_model_file())) return()
    
    withProgress(message = "Expanding Rx Dataset...", value = 0, max = 1, {
      setProgress(message="Expanding Rx Dataset...", value = 0.3)
      incProgress(message="Expanding Rx Dataset...", amount = 0.6)
      
      sim_data <- expand_sim_dataset()
      
      incProgress(message="Expanding Rx Dataset...", amount = 0.02)
      incProgress(message="Expanding Rx Dataset...", amount = 0.02)
      incProgress(message="Expanding Rx Dataset...", amount = 0.02)
      incProgress(message="Expanding Rx Dataset...", amount = 0.02)
      incProgress(message="Expanding Rx Dataset...", amount = 0.02)
      setProgress(message="Rx Dataset Complete!", 1)
      Sys.sleep(0.15)
    })
    sim_data
  })
  
  ## sim setup & post-processing -----------------------------------------------
  ## Setup simulation & post-process the results
  run_sim_series <- eventReactive(input$expand_rx_data, label = "run_sim_series <- eventReactive(input$expand_rx_data)", {
    print_function_calls(fn_name = "run_sim_series", fn_type = "eventReactive")
  
    if(!is.null(validate_params())) return()

    withProgress(message = "Running Simulation... Please Wait", value = 0, max = 1, {
      setProgress(message = "Running Simulation... Please Wait", 0)
      
      sim_series_data <- run_sim_series_from_data_template(mod = update_model(), 
                                                           sim_template = expand_sim_dataset(), 
                                                           sim_settings = update_sim_settings())
      
      setProgress(message = "Running Simulation... Please Wait", 1)
      Sys.sleep(0.1)
      setProgress(message="Simulation Complete!", 1)
      Sys.sleep(0.1)
      return(sim_series_data)
    })
  })
  
  ## sim output datatable ------------------------------------------------------
  # Create an output datatable
  output$full_sim_template_data <- DT::renderDataTable({
    input$expand_rx_data
    
    print_function_calls(fn_name = "output$full_sim_template_data", fn_type = "DT::renderDataTable")
    
    if(is.null(get_full_sim_template())) return()
    if(!is.null(get_full_sim_template())) {
      dt_sim_template_data_fn(sim_template_data = get_full_sim_template())
    }
  })

  # Downloadable csv of selected dataset ---------------------------------------
  output$download_rx_dose_event_data <- downloadHandler(
    filename = function() {
      paste0("dose_events.csv")
    },
    content = function(file) {
      write.csv(rx_multi_dose_regs(), file, row.names = FALSE)
    }
  )
  
  # Downloadable csv of full simulation input dataset --------------------------
  output$download_full_sim_input_data <- downloadHandler(
    filename = function() {
      if (!is.null(input$upload_model) && input$model_selection_method %in% "Upload Custom Model File") {
        user_model_info <- input$upload_model
        curr_model_name <- gsub(pattern="(?i)\\.cpp", replacement="", basename(user_model_info$name), perl=TRUE)
      } else if (!is.null(input$model_selection) && input$model_selection_method %in% "Select from Model Library") {
        curr_model_name <- input$model_selection
      } 
      paste0(gsub(pattern="(?i)\\.csv$", replacement="", curr_model_name, perl=TRUE), ".csv")
    },
    content = function(file) {
      write.csv(get_full_sim_template(), file, row.names = FALSE)
    }
  )
}




