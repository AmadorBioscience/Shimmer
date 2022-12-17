###############################################################################|
## Converted server functions --------------------------------------------------
###############################################################################|

format_temp_file_path <- function(file) {
  # Handle OS-dependent temporary filepath
  if (identical(.Platform$OS.type, "windows")) {
    temp_dir_path <- normalizePath(path = dirname(file), winslash = .Platform$file.sep)
    temp_filepath <- file.path(temp_dir_path, basename(file))
  } else {
    temp_filepath <- file
  }
}

copy_file_to_temp_dir <- function(temp_dir, target_file) {
  temp_file <- file.path(temp_dir, basename(target_file))
  file.copy(target_file, temp_file, overwrite = TRUE)
  return(temp_file)
}

construct_rmd_param_list <- function(mod, dose_data, tg_input, sum_stats, sim_plots, 
                                     title, author, compound, description, units, 
                                     rx_input, modup_params, sim_settings, model_code) {
  
  mod_slots <- list_S4_slots(mod)
  mod_details <- extract_model_annotations_data(mod)
  
  params_for_rmd = list(
    sumstat     = sum_stats,
    graphs      = sim_plots,
    set_title   = title,
    set_author  = author,
    compound    = compound,
    description = description,
    dose_units  = dose_units(units),
    model_name  = mod@model,
    model_file  = mod@modfile,
    model_param = modup_params,
    model_omega = convert_model_re_matrices_to_corr(mod)$omega,
    model_sigma = convert_model_re_matrices_to_corr(mod)$sigma,
    mod_names   = names(mod),
    mod_details = mod_details,
    mod_init    = init(mod),
    mod_outvars = outvars(mod),
    sim_opts    = sim_settings,
    solver_opts = mod_slots[define_solver_settings_params()],
    tgrid_list  = list_S4_slots(tg_input),
    sim_times   = stime(tg_input),
    rx_input    = rx_input,
    rx_dataset  = dose_data,
    mod_code    = model_code,
    mod_slots   = mod_slots,
    mod_obj     = mod
  )
  params_for_rmd
}

initial_step_list <- function() {
  list(load_model = TRUE,
       specify_rx_dosing = TRUE,
       expand_rx_dataset = TRUE,
       run_simulation = TRUE
  )
}

sim_steps_html_list <- function() {
  list(
    load_model = "<li>Load Model</li>",
    specify_rx_dosing = "<li>Specify Rx Dosing</li>",
    expand_rx_dataset = "<li>Expand Rx Dataset</li>",
    run_simulation = "<li>Run Simulation</li>"
  )
}

update_sim_step_list_fn <- function(sim_steps) {
  print_function_calls(fn_name = "update_sim_step_list_fn", fn_type = "function")
  reactive_step_list <- reactiveValuesToList(sim_steps)[names(initial_step_list())]
  curr_steps_in_list <- names(reactive_step_list)[unlist(reactive_step_list)]
  if (identical(curr_steps_in_list, character(0))) return(NULL)
  curr_step_list_html <- paste(c("<strong>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Simulation Steps:</strong>", "<ul>", unlist(sim_steps_html_list()[curr_steps_in_list]), "</ul>"), sep = "", collapse = "")
  HTML(curr_step_list_html)
}


update_load_model_sim_step <- function(sim_steps) {
  print_function_calls(fn_name = "update_load_model_sim_step", fn_type = "function")
  sim_steps$load_model <- FALSE
  sim_steps$expand_rx_dataset <- TRUE
  sim_steps$run_simulation    <- TRUE
  sim_steps
}

update_rx_submit_sim_step <- function(sim_steps) {
  print_function_calls(fn_name = "update_rx_submit_sim_step", fn_type = "function")
  sim_steps$specify_rx_dosing <- FALSE
  sim_steps$expand_rx_dataset <- TRUE
  sim_steps$run_simulation    <- TRUE
  sim_steps
}

update_model_params_sim_step <- function(sim_steps) {
  print_function_calls(fn_name = "update_model_params_sim_step", fn_type = "function")
  sim_steps$expand_rx_dataset <- TRUE
  sim_steps$run_simulation    <- TRUE
  sim_steps
}

update_expand_rx_data_sim_step <- function(sim_steps) {
  print_function_calls(fn_name = "update_expand_rx_data_sim_step", fn_type = "function")
  sim_steps$expand_rx_dataset <- FALSE
  sim_steps$run_simulation    <- TRUE
  sim_steps
}

update_do_run_sim_step <- function(sim_steps) {
  print_function_calls(fn_name = "update_do_run_sim_step", fn_type = "function")
  sim_steps$run_simulation    <- FALSE
  sim_steps
}

valid_rx_input_fn <- function(rx_input) {
  print_function_calls(fn_name = "valid_rx_input_fn", fn_type = "function")
  rx_split_periods <- split_rx_dosing_periods(x = rx_input)
  validate_rx_part_counts(rx_split_periods = rx_split_periods)
}

submit_rx_input_fn <- function(rx_input) {
  print_function_calls(fn_name = "submit_rx_input_fn", fn_type = "function")
  ev_data <- mrgsolve::ev_rx(rx_input)
  fill_default_ev_data_cols(ev_data = ev_data)
}

rx_dose_data_fn <- function(is_valid_rx, submit_rx) {
  print_function_calls(fn_name = "rx_dose_data_fn", fn_type = "function")
  
  shinyFeedback::feedbackDanger("rx_input", !is_valid_rx, "Invalid Rx specification! Please follow Rx syntax rules...") ## add feedback for user inputs:  https://mastering-shiny.org/action-feedback.html#validating-input
  
  if (isFALSE(is_valid_rx)) {
    validate("Invalid Rx specification!", errorClass = "invalid-input")
  } else {
    submit_rx
  }
}

read_model_file_fn <- function(model_selection, upload_model, model_selection_method) {
  print_function_calls(fn_name = "read_model_file_fn", fn_type = "function")
  
  if (!is.null(upload_model) && model_selection_method %in% "Upload Custom Model File") {
    curr_selection_method <- "upload_model"
    user_model_info <- upload_model
    model_file_path <- user_model_info$datapath
    curr_model_name <- gsub(pattern="(?i)\\.cpp", replacement="", basename(model_file_path), perl=TRUE)
    curr_model_path <- dirname(model_file_path)
  } else if (!is.null(model_selection) && model_selection_method %in% "Select from Model Library") {
    curr_selection_method <- "model_selection"
    model_file_path <- file.path(getwd(), "model_library", paste0(model_selection, ".cpp"))
    curr_model_name <- model_selection
    curr_model_path <- "model_library"
  } else {
    validate("Missing model file!", errorClass = "invalid-input")
  }
  
  model_file_exists <- file.exists(model_file_path)
  if (isTRUE(shiny::isRunning())) shinyFeedback::feedbackDanger(model_selection_method, !model_file_exists, "Model file doesn't exist! Please select a different model...")
  
  if (isFALSE(model_file_exists))  validate("Model file doesn't exist!", errorClass = "invalid-input")
  
  mread_cache_result <- try(expr = mread_cache(model = curr_model_name, project = curr_model_path, soloc = "."), silent = TRUE)
  invalid_model_file <- dplyr::if_else(isTRUE("try-error" %in% class(mread_cache_result)), TRUE, FALSE)
  if (isTRUE(shiny::isRunning())) shinyFeedback::feedbackDanger(model_selection_method, invalid_model_file, "Invalid model specification file! Please select a different model...")
  
  if (!isTRUE(invalid_model_file)) {
    if (!is.null(upload_model) && curr_selection_method %in% "upload_model" && !identical(mread_cache_result@model, user_model_info$name)) {
      mread_cache_result@model <- gsub(pattern="(?i)\\.cpp", "", user_model_info$name, perl = TRUE)
    }
  }
  
  if (!isTRUE(invalid_model_file)) {
    invalid_block_combo <- is_invalid_block_combo(mread_cache_result)
    invalid_blocks_msg <- dplyr::if_else(isTRUE(invalid_block_combo), "INVALID MODEL BLOCKS:  please add `@annotated` blocks (>= 1) or remove `$FIXED` block!", "Valid combination of model blocks")
    
    if (isTRUE(invalid_block_combo)) {
      validate(invalid_blocks_msg, errorClass = "invalid-input") 
    }
  }
  
  if (isTRUE(invalid_model_file)) {
    validate("Invalid model specification!", errorClass = "invalid-input")
  } else {
    return(mread_cache_result)
  }
}

fetch_model_code_fn <- function(mod) {
  print_function_calls(fn_name = "fetch_model_code_fn", fn_type = "function")
  paste(mod@code, sep = "", collapse = "\n")
}

fetch_model_name_fn <- function(mod) {
  print_function_calls(fn_name = "fetch_model_name_fn", fn_type = "function")
  mod@model
}

loaded_model_msg_fn <- function(model_name) { 
  print_function_calls(fn_name = "loaded_model_msg_fn", fn_type = "function")
  paste("<b>Loaded Model:</b>&nbsp;&nbsp;&nbsp;", model_name, sep = "")
}

calc_dosing_end_time <- function(dose_data, delta = 1, scaling_factor = define_tgrid_end_scaling_factor()) {
  req_dose_data_cols <- c("TIME", "ADDL", "II")
  if (!is.null(dose_data) & isTRUE(unique(req_dose_data_cols %in% names(dose_data)))) {
    max_idx <- which.max(dose_data$TIME)
    end <- scaling_factor*(dose_data$TIME[max_idx] + (dose_data$ADDL[max_idx]*dose_data$II[max_idx]))
  }
  end_adj <- end - (end %% delta)
  end_adj
}

build_time_grid_object_fn <- function(start, end, delta, add, offset, scale) {
  print_function_calls(fn_name = "build_time_grid_object_fn", fn_type = "function")
  tg <- mrgsolve::tgrid(start = start, end = end, delta = delta,
                        add = add, .offset = offset, .scale = scale)
  tg
}

update_time_grid_end_time <- function(tg, end_dosing) {
  time_grid <- mrgsolve::tgrid(start = tg@start, end = end_dosing, delta = tg@delta, 
                               add = tg@add, .offset = tg@offset, .scale = tg@scale)
  time_grid
}

adjust_time_grid_dosing_fn <- function(dose_data, tg_input, user_spec = FALSE, scaling_factor = define_tgrid_end_scaling_factor()) {
  if (is.null(dose_data)) return(tg_input)
  end_time <- tg_input@end
  if (!isTRUE(user_spec)) {
    end_time <- calc_dosing_end_time(dose_data = dose_data, 
                                     delta = tg_input@delta, 
                                     scaling_factor = scaling_factor)
  }
  tg <- update_time_grid_end_time(tg = tg_input, end_dosing = end_time)
  tg
}

update_solver_settings_fn <- function(atol, rtol, ss_atol, ss_rtol, maxsteps, hmin, hmax, ixpr, mxhnil) {
  print_function_calls(fn_name = "update_solver_settings_fn", fn_type = "function")
  solver_settings <- list(
    atol     = atol,
    rtol     = rtol,
    ss_atol  = ss_atol,
    ss_rtol  = ss_rtol,
    maxsteps = maxsteps,
    hmin     = hmin,
    hmax     = hmax,
    ixpr     = ixpr,
    mxhnil   = mxhnil
  )
  solver_settings
}

update_model_solver_settings_fn <- function(mod, solver_settings) {
  print_function_calls(fn_name = "update_model_solver_settings_fn", fn_type = "function")
  
  ## fetch solver settings & the current model
  solver_settings$object <- mod
  mrgsolve_update <- mrgsolve::update
  modup <- do.call("mrgsolve_update", solver_settings)
  modup
}

fixed_params_modal_uiout_fn <- function(mod) {
  print_function_calls(fn_name = "fixed_params_modal_uiout_fn", fn_type = "function")
  reorder_modal_popup_children(popup_html = 
                                 bsModal("fixed_params_modal_window", "Fixed Parameters Selection", "fixed_params_modal_button", size = "large",
                                         if(is.null(mod)) {
                                           return(NULL)
                                         },
                                         if(!is.null(mod)) {
                                           HTML(build_fixed_param_modal_window(mod = mod))
                                         }
                                 )
  )
}

build_re_matrix_table_fn <- function(re_matrix) {
  print_function_calls(fn_name = "build_omega_table_fn", fn_type = "function")
  
  if (is.null(re_matrix)) return(NULL)
  if (isTRUE(0 %in% dim(re_matrix))) return(NULL)
  
  rhandsontable(re_matrix, rowHeaderWidth=100)  %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
    hot_cols(manualColumnResize = TRUE, format = "0.000",
             renderer = render_interactive_matrix())
}

update_re_matrix_hot_fn <- function(re_matrix_hot) {
  print_function_calls(fn_name = "update_re_matrix_hot_fn", fn_type = "function")
  if (is.null(re_matrix_hot)) return(NULL)
  re_matrix <- hot_to_r(re_matrix_hot)
  matrix(re_matrix, nrow = nrow(re_matrix), ncol = ncol(re_matrix), dimnames = list(colnames(re_matrix), colnames(re_matrix)))
}

validate_re_matrices_fn <- function(re_omega, re_sigma, re_selected, print_results = TRUE) {
  print_function_calls(fn_name = "validate_re_matrices_fn", fn_type = "function")
  
  omega_corr <- as.matrix(re_omega)
  sigma_corr <- as.matrix(re_sigma)
  omega_rows <- nrow(omega_corr)
  sigma_rows <- nrow(sigma_corr)
  
  if (omega_rows %in% 0 & sigma_rows %in% 0) {
    return(list(res = NULL, msg = "Matrices not specified!"))
  }
  
  if (omega_rows > 0 && re_selected %in% c("Interindividual Variability", "Both")) {
    is_valid_omega_diag <- validate_matrix_diag_GE_0(omega_corr, return_res = TRUE)
    if (isTRUE(is_valid_omega_diag)) {
      is_valid_omega_corr <- validate_corr_matrix(omega_corr, return_res = TRUE)
      omega_matrix <- double_sweep_corr_to_cov_matrix(corr_matrix = omega_corr, validate_corr = FALSE)
      is_valid_omega_matrix <- is_valid_omega_corr & is_valid_re_matrix(omega_matrix, print_results = print_results)
    } else {
      is_valid_omega_matrix <- FALSE
    }
  } else {
    is_valid_omega_matrix <- TRUE
  }
  
  
  if (sigma_rows > 0 && re_selected %in% c("Residual Variability", "Both")) {
    is_valid_sigma_diag <- validate_matrix_diag_GE_0(sigma_corr, return_res = TRUE)
    if (isTRUE(is_valid_sigma_diag)) {
      is_valid_sigma_corr <- validate_corr_matrix(sigma_corr, return_res = TRUE)
      sigma_matrix <- double_sweep_corr_to_cov_matrix(corr_matrix = sigma_corr, validate_corr = FALSE)
      is_valid_sigma_matrix <- is_valid_sigma_corr & is_valid_re_matrix(sigma_matrix, print_results = print_results)
    } else {
      is_valid_sigma_matrix <- FALSE
    }
  } else {
    is_valid_sigma_matrix <- TRUE
  }
  
  is_valid_re_matrices  <- isTRUE(is_valid_omega_matrix) & isTRUE(is_valid_sigma_matrix)
  
  
  if (re_selected %in% "Interindividual Variability") {
    valid_re_matrices <- list(
      res = isTRUE(is_valid_omega_matrix),
      msg = if_else(isTRUE(is_valid_omega_matrix), "Valid OMEGA matrix!", "Invalid OMEGA matrix!!!")
    )
  } else if (re_selected %in% "Residual Variability") {
    valid_re_matrices <- list(
      res = isTRUE(is_valid_sigma_matrix),
      msg = if_else(isTRUE(is_valid_sigma_matrix), "Valid SIGMA matrix!", "Invalid SIGMA matrix!!!")
    )
  } else if (re_selected %in% "Both") {
    valid_re_matrices <- list(
      res = isTRUE(is_valid_re_matrices),
      msg = if_else(isTRUE(is_valid_re_matrices), "Valid random effect matrices!", 
                    if_else(isFALSE(is_valid_omega_matrix) & isFALSE(is_valid_sigma_matrix), "Invalid random effect matrices!!!", 
                            if_else(isFALSE(is_valid_omega_matrix) & isTRUE(is_valid_sigma_matrix), "Invalid OMEGA matrix!!!", "Invalid SIGMA matrix!!!")))
    )
  }
  return(valid_re_matrices)
}

check_re_matrices_fn <- function(is_valid_re) {
  print_function_calls(fn_name = "check_re_matrices_fn", fn_type = "function")
  if (isTRUE(is_valid_re$res)) {
    show_msg <- showToast(type = "success", message = is_valid_re$msg, title = "Successful Check", .options = list(showDuration = 4))
  } else if (is.null(is_valid_re$res)) {
    show_msg <- showToast(type = "warning", message = is_valid_re$msg, title = "Indeterminate Check", .options = list(showDuration = 4))
  } else {
    show_msg <- showToast(type = "error", message = is_valid_re$msg, title = "Failed Check", .options = list(showDuration = 4))
  }
  show_msg
}

build_mod_param_info_fn <- function(mod, input) {
  print_function_calls(fn_name = "build_mod_param_info_fn", fn_type = "function")
  
  if (is.null(mod)) return(NULL)
  if (is.null(input)) return(NULL)
  
  param_names <- names(extract_model_params(mod = mod))
  
  df_param_info <- tibble::tibble(
    param = param_names,
    value = extract_model_param_value(mod, input, param_names),
    units = extract_model_param_units(mod, input, param_names),
    descr = extract_model_param_descr(mod, param_names)
  )
  
  return(df_param_info)
}

extract_model_param_value <- function(mod, input, param_names = NULL) {
  print_function_calls(fn_name = "extract_model_param_value", fn_type = "function")
  param_names <- param_names %||% names(extract_model_params(mod = mod))
  unlist(lapply(setNames(build_model_param_value_ids(mod = mod), param_names), function(id) input_as_numeric(input[[id]])))
}

extract_model_param_units <- function(mod, input, param_names = NULL) {
  print_function_calls(fn_name = "extract_model_param_units", fn_type = "function")
  param_names <- param_names %||% names(extract_model_params(mod = mod))
  unlist(lapply(setNames(build_model_param_units_ids(mod = mod), param_names), function(id) input[[id]]))
}

extract_model_param_descr <- function(mod, param_names = NULL) {
  print_function_calls(fn_name = "extract_model_param_descr", fn_type = "function")
  param_names <- param_names %||% names(extract_model_params(mod = mod))
  setNames(extract_model_param_annot_data(mod = mod)$descr, param_names)
}

update_model_fn <- function(mod, param_info, solver_settings, omega_mat, sigma_mat, re_selected) {
  print_function_calls(fn_name = "update_model_fn", fn_type = "function")
  if(is.null(mod)) return(NULL)
  
  modup_params <- param(mod, param_info$value)
  modup_solver <- update_model_solver_settings_fn(mod = modup_params, solver_settings)
  
  mod_param_annot_data <- extract_model_param_annot_data(mod = modup_solver)
  mod_param_annot_data$unit <- unname(param_info$units)
  modup_solver@annot$data <- mod_param_annot_data
  n_omega_blocks <- length(omat(modup_solver))
  n_sigma_blocks <- length(smat(modup_solver))
  
  if (n_omega_blocks > 2) stop("cannot specify more than 2 omega matrix blocks at this time...\nPlease specify full omega matrix in mrgsolve model specification (.cpp) file")
  if (n_sigma_blocks > 2) stop("cannot specify more than 2 sigma matrix blocks at this time...\nPlease specify full sigma matrix in mrgsolve model specification (.cpp) file")
  
  if (re_selected %in% "None") {
    
    modup_re <- modup_solver %>% zero_re()
    
  } else if (re_selected %in% "Interindividual Variability") {
    
    omega_matrix <- double_sweep_corr_to_cov_matrix(corr_matrix = as.matrix(omega_mat))
    modup_re <- modup_solver %>% omat(omega_matrix) %>% zero_re("sigma")
    
  } else if (re_selected %in% "Residual Variability") {
    
    sigma_matrix <- double_sweep_corr_to_cov_matrix(corr_matrix = as.matrix(sigma_mat))
    modup_re <- modup_solver %>% smat(sigma_matrix) %>% zero_re("omega")
    
  } else if (re_selected %in% "Both") {
    
    omega_matrix <- double_sweep_corr_to_cov_matrix(corr_matrix = as.matrix(omega_mat))
    sigma_matrix <- double_sweep_corr_to_cov_matrix(corr_matrix = as.matrix(sigma_mat))
    modup_re <- modup_solver %>% omat(omega_matrix) %>% smat(sigma_matrix)
    
  } else {
    
    ## shouldn't be possible to trigger this error
    validate("Invalid selection of random effects!", errorClass = "invalid-random-effects-selected")
    
  }
  modup_re
}

dt_sim_template_data_fn <- function(sim_template_data) {
  print_function_calls(fn_name = "full_sim_template_data_fn", fn_type = "function")
  
  if (is.null(sim_template_data)) return(NULL)
  if (!is.null(sim_template_data)) {
    
    full_sim_template_data <- sim_template_data %>% 
      mutate(across(where(is.numeric), ~ signif(.x, 4)))
    
    DT::datatable(full_sim_template_data,
                  class = "compact",
                  options = list(
                    ## see section 4.3 Callbacks in Options:  https://rstudio.github.io/DT/options.html
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#0A5EAA', 'color': '#FCFCFC'});",
                      "}"
                    ),
                    # pageLength = 10, autoWidth = TRUE,
                    pageLength = 5, autoWidth = TRUE,
                    ## see section 4. ColVis (Retired):  https://rstudio.github.io/DT/extensions.html
                    dom = "lBfrtip", buttons = I("colvis")
                  ),
                  rownames = FALSE,
                  filter = "bottom", style = "default", selection = "none",
                  extensions = "Buttons"
    )
  }
}




