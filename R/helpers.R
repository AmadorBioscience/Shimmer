###############################################################################|
## shiny app development tools -------------------------------------------------
###############################################################################|
run_helpers_in_test_mode <- function() {
  FALSE
}

run_app_with_reactlog <- function() {
  FALSE
}

print_console_msgs <- function() {
  # TRUE
  FALSE
}

print_function_calls <- function(fn_name, fn_type, print_call = print_console_msgs()) {
  if (isTRUE(print_call)) {
    cat("\n   call ", fn_type,"()  -->  ", fn_name,"()   \n", sep='', fill=TRUE)
  }
}

print_obj_to_console <- function(obj, print_obj = print_console_msgs()) {
  if (isTRUE(print_obj)) {
    print(obj)
  }
}

get_shiny_app_project_path <- function() {
  curr_wd <- getwd()
  on.exit(setwd(curr_wd))
  setwd(dirname(this.path::this.path()))
  here::i_am("R/helpers.R")
  project_path <- here::here()
  return(project_path)
}

get_this_script_path <- function() {
  this.path::this.path()
}

setwd_shiny_app_project <- function() {
  this_script_dirpath <-  dirname(get_this_script_path())
  shiny_app_dirpath <- dplyr::if_else(identical(basename(this_script_dirpath) %in% c("R", "Functions"), TRUE), dirname(this_script_dirpath), this_script_dirpath)
  setwd(shiny_app_dirpath)
}

run_all_unit_tests <- function() {
  test_files <- list.files(path = file.path(get_shiny_app_project_path(), "tests", "testthat"), pattern = "(?i)^test-.*\\.R$", full.names = TRUE)
  purrr::walk(test_files, testthat::test_file)
}

set_mod_envir_slot <- function(mod, envir = environment()) {
  if (isTRUE(.hasSlot(mod, "envir"))) slot(mod, "envir") <- envir
  mod
}


###############################################################################|
## shiny app default global options --------------------------------------------
###############################################################################|

specify_default_rx_input <- function() {
  "three_periods"
}

fetch_default_rx_input_list <- function() {
  default_rx_input <- list(
    placeholder = "Rx specification",
    two_periods = "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28",
    three_periods = "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5"
  )
  default_rx_input
}

define_default_rx_input <- function() {
  fetch_default_rx_input_list()[[specify_default_rx_input()]]
}


###############################################################################|
## shiny app model specification file list -------------------------------------
###############################################################################|

fetch_custom_modlist <- function(names_only = FALSE) {
  custom_modlist_paths <- list.files(path = "model_library", "(?i)\\.cpp$", full.names = TRUE)
  custom_modlist_named <- setNames(custom_modlist_paths, extract_path_file_stem(custom_modlist_paths))
  if (isTRUE(names_only)) return(names(custom_modlist_named))
  custom_modlist_named
}

fetch_mrgsolve_modlist <- function(names_only = FALSE) {
  if (isFALSE("mrgsolve" %in% (.packages()))) require(mrgsolve)
  mrgsolve_modlist_paths <- list.files(mrgsolve::modlib(), "(?i)\\.cpp$", full.names = TRUE)
  mrgsolve_modlist_named <- setNames(mrgsolve_modlist_paths, extract_path_file_stem(mrgsolve_modlist_paths))
  if (isTRUE(names_only)) return(names(mrgsolve_modlist_named))
  mrgsolve_modlist_named
}

extract_path_file_stem <- function(path) {
  basename(tools::file_path_sans_ext(path))
}

modify_modlist <- function(modlist = NULL, first = NULL, drop = NULL) {
  modlist <- modlist %||% fetch_custom_modlist(names_only = TRUE)
  first <- first %||% modlist[1]
  drop <- drop %||% NA_character_
  if (!identical(first, modlist[1]))   modlist <- c(first, setdiff(modlist, first))
  if (!identical(drop, NA_character_)) modlist <- setdiff(modlist, drop)
  modlist
}

drop_custom_modlist_models <- function(test_mode = NULL) {
  test_mode <- test_mode %||% run_helpers_in_test_mode()
  drop <- dplyr::if_else(isFALSE(test_mode), "invalid_MM", NA_character_)
  drop
}


###############################################################################|
## dosing event data defaults & formatting functions ---------------------------
###############################################################################|

## dosing event data default definition functions:
define_default_ev_data_cols <- function() {
  list(ID = 1L, TIME = 0, AMT = 0, RATE = 0, EVID = 1L, CMT = 1L, ADDL = 0L, II = 0, SS = 0L)
}

define_integer_ev_data_cols <- function() {
  c("ID", "EVID", "CMT", "ADDL", "SS")
}

## dosing event data formatting functions:
sort_ev_colnames <- function(ev_data) {
  ev_data_cols <- names(ev_data)
  ev_data_cols_upper <- toupper(ev_data_cols)
  ev_cols_LUT <- setNames(ev_data_cols, ev_data_cols_upper)
  default_cols <- names(define_default_ev_data_cols())
  ev_cols_default_order <- setdiff(default_cols, setdiff(default_cols, ev_data_cols_upper))
  ev_cols_ordered <- unname(ev_cols_LUT[ev_cols_default_order])
  ev_cols_ordered
}

ev_cols_tolower <- function(ev_data) {
  ev_cols_default_order <- sort_ev_colnames(ev_data)
  
  df_data <- tibble::as_tibble(ev_data) %>% 
    select(tidyselect::all_of(ev_cols_default_order), everything())
  
  cols_to_lowercase <- setdiff(names(df_data), "ID")
  df_data_lowercase <-  df_data %>% 
    rename_with(., .fn = tolower, .cols = tidyselect::all_of(cols_to_lowercase))
  
  df_data_lowercase
}

fill_default_ev_data_cols <- function(ev_data) {
  ## called in `submit_rx_input` eventReactive
  df_data_uppercase <- tibble::as_tibble(ev_data) %>% 
    rename_with(., .fn = toupper)
  
  default_col_vals <- define_default_ev_data_cols()
  default_colnames <- names(default_col_vals)
  
  df_data <- df_data_uppercase %>% 
    tibble::add_column(!!!default_col_vals[setdiff(names(default_col_vals), names(df_data_uppercase))]) %>% 
    select(tidyselect::all_of(default_colnames), everything())
  
  df_data_col_order <- sort_ev_colnames(df_data)
  int_ev_cols <- define_integer_ev_data_cols()
  
  df_data_out <- df_data %>% 
    mutate(across(.cols = matches(match = int_ev_cols, ignore.case = TRUE, perl = TRUE), .fns = as.integer)) %>% 
    rename_with(., .fn = toupper) %>% 
    select(tidyselect::all_of(df_data_col_order), everything())
  
  df_data_out
}


###############################################################################|
## `mrgsolve` time grid (`tgrid` S4 object) default definitions ----------------
###############################################################################|

define_tgrid_end_scaling_factor <- function() {
  ## called in `build_time_grid_object` reactive
  scaling_factor <- 1.50
  scaling_factor
}

extract_tgrid_defaults <- function() {
  ## called in "UI_dose_selection.R"
  list_S4_slots(mrgsolve::tgrid())
}

###############################################################################|
## simulation settings default definitions -------------------------------------
###############################################################################|

fetch_default_sim_settings <- function() {
  sim_settings <- list(
    num_subj = 10,
    num_sims = 10,
    sim_seed = 2674474
  )
  sim_settings
}

update_sim_settings_fn <- function(num_subj = fetch_default_sim_settings()$num_subj, 
                                   num_sims = fetch_default_sim_settings()$num_sims, 
                                   sim_seed = fetch_default_sim_settings()$sim_seed) {
  print_function_calls(fn_name = "update_sim_settings_fn", fn_type = "function")
  sim_settings <- list(
    num_subj = num_subj,
    num_sims = num_sims,
    sim_seed = sim_seed
  )
  sim_settings
}



###############################################################################|
## solver settings default definitions -----------------------------------------
###############################################################################|

define_solver_settings_params <- function() {
  ## called in `output$report` downloadHandler
  solver_settings <- c("rtol", "atol", "ss_rtol", "ss_atol", 
                       "maxsteps", "hmin", "hmax", "ixpr", "mxhnil")
  solver_settings
}

fetch_solver_settings <- function(use_custom = TRUE) {
  ## called in `update_model_solver_settings` server function
  ## called in "UI_dose_selection.R" 
  solver_settings_params <- define_solver_settings_params()
  if (isTRUE(use_custom)) {
    custom_solver_settings <- define_custom_solver_settings()
    solver_settings <- custom_solver_settings[solver_settings_params]
  } else {
    mrgmod_solver_settings <- define_mrgmod_solver_settings()
    solver_settings <- mrgmod_solver_settings[solver_settings_params]
  }
  solver_settings
}

define_custom_solver_settings <- function() {
  list(
    rtol = 1E-10,
    atol = 1E-12,
    ss_rtol = 1e-10, 
    ss_atol = 1e-12,
    maxsteps = 10000,
    hmin = 0,
    hmax = 0,
    ixpr = 0,
    mxhnil = 2
  )
}

define_mrgmod_solver_settings <- function() {
  list(
    rtol = 1E-8,
    atol = 1E-8,
    ss_rtol = 1E-8, 
    ss_atol = 1E-8,
    maxsteps = 20000,
    hmin = 0,
    hmax = 0,
    ixpr = 0,
    mxhnil = 2
  )
}


###############################################################################|
## model parameter functions ---------------------------------------------------
###############################################################################|

reserved_library_model_params <- function(context = "output") {
  ## only applies to library models
  valid_contexts <- c("input", "output")
  if (!isTRUE(context %in% valid_contexts)) {
    stop(build_invalid_context_error_msg(valid_contexts))
  }
  reserved_params <- c("DOSE", "REGNUM", "WT", "WTREF")
  
  if (isTRUE(context %in% "output")) {
    return(reserved_params)
  } else if(isTRUE(context %in% "input")) {
    return(setdiff(reserved_params, c("WT", "WTREF")))
  } else {
    stop(build_invalid_context_error_msg(valid_contexts))
  }
}

build_invalid_context_error_msg <- function(valid_contexts) {
  paste0("\n   Unrecognized `context` for reserved library model parameters!\n   Please input one of the following valid contexts:  ", paste(paste0("'", valid_contexts, "'"), sep = "", collapse = ", "))
}

## extract model parameters
extract_model_params <- function(mod) {
  ## called in `build_df_param_info` reactive
  as.list(param(mod))
}


###############################################################################|
## model annotation data functions ---------------------------------------------
###############################################################################|

model_details_col_order <- function() {
  ## called in `output$report` downloadHandler
  c("block", "name", "value", "unit", "descr","options")
}

build_default_model_param_annot_data <- function(mod) {
  mod_params <- as.list(mod)$param
  tibble::tibble(block = "PARAM", name = names(mod_params), 
                 descr = ".", unit = ".", options = ".", 
                 value = unlist(mod_params)) %>% 
    select(tidyselect::any_of(model_details_col_order()))
}

## extract model parameter annotation data
extract_model_annotations_data <- function(mod) {
  mod_annot <- mod@annot$data
  if (isTRUE(identical(nrow(mod_annot), 0L))) mod_annot <- build_default_model_param_annot_data(mod)
  if (isFALSE("value" %in% names(mod_annot))) mod_annot <- mrgsolve:::details(mod, values = TRUE)
  if (isTRUE("units" %in% names(mod_annot)))  mod_annot <- mod_annot %>% rename(unit = units)
  mod_annot %>% select(tidyselect::any_of(model_details_col_order()))
}

extract_model_param_annot_data <- function(mod) {
  ## called in `build_df_param_info` reactive & `update_model` eventReactive
  mod_annot <- extract_model_annotations_data(mod = mod)
  mod_param_annot <- mod_annot[mod_annot$block %in% "PARAM",]
  if (identical(nrow(mod_param_annot), 0L)) mod_param_annot <- build_default_model_param_annot_data(mod)
  mod_param_annot
}

list_S4_slots <- function(x) {
  ## called in `output$report` downloadHandler
  stopifnot(isTRUE(isS4(x)))
  set_names(map(slotNames(x), ~ slot(x, .)), slotNames(x))
}


###############################################################################|
## model code blocks functions -------------------------------------------------
###############################################################################|

mod_has_fixed_block <- function(mod) {
  !identical(mod@fixed, list())
}

mod_has_annotations <- function(mod) {
  isTRUE(nrow(mod@annot[["data"]]) > 0)
}

mod_has_fixed_block_no_annot <- function(mod) {
  isTRUE(isTRUE(mod_has_fixed_block(mod)) && isFALSE(mod_has_annotations(mod)))
}

is_invalid_block_combo <- function(mod) {
  invalid_blocks <- mod_has_fixed_block_no_annot(mod)
  if (isTRUE(invalid_blocks)) {
    try_mod_details <- try(expr = mrgsolve:::details(mod), silent = TRUE)
    invalid_blocks <- dplyr::if_else(isTRUE("try-error" %in% class(try_mod_details)), TRUE, FALSE)
  }
  invalid_blocks
}

###############################################################################|
## Collapse OMEGA/SIGMA matrices -----------------------------------------------
###############################################################################|

collapse_re <- function(mod, show_matrices = TRUE) {
  ## called in `load_model_file` eventReactive
  n_blocks_in <- count_re_matrix_blocks(mod = mod)
  mod <- mrgsolve::collapse_omega(mod)
  mod <- mrgsolve::collapse_sigma(mod)
  n_blocks_out <- count_re_matrix_blocks(mod = mod)
  
  ## verify the correct number of collapsed OMEGA/SIGMA matrices (0 or 1)
  if (isTRUE(max(unlist(n_blocks_in), na.rm = TRUE) >= 1)) {
    stopifnot(identical(unique(unlist(n_blocks_out[n_blocks_in >= 1])), 1L))
  }
  
  if (isTRUE(min(unlist(n_blocks_in), na.rm = TRUE) %in% 0)) {
    stopifnot(identical(unique(unlist(n_blocks_out[n_blocks_in %in% 0])), 0L))
  }
  
  if (isTRUE(show_matrices) && isTRUE(print_console_msgs())) {
    cat("\n   OMEGA matrix (full):", fill = TRUE, sep = "")
    print(omat(mod))
    cat("\n   SIGMA matrix (full):", fill = TRUE, sep = "")
    print(smat(mod))
  }
  invisible(mod)
}

count_re_matrix_blocks <- function(mod) {
  list(omega=length(mod@omega), sigma=length(mod@sigma))
}


###############################################################################|
## Extract & label OMEGA/SIGMA random effect matrices --------------------------
###############################################################################|

## extract random effect matrices & labels from model object
extract_model_re_matrices <- function(mod) {
  lapply(revar(mod), as.matrix)
}

extract_model_re_labels <- function(mod) {
  setNames(lapply(as.list(mod)$random[c("omega_labels", "sigma_labels")], unlist), c("omega", "sigma"))
}

label_re_matrix <- function(re_matrix, re_labels) {
  attributes(re_matrix)$dimnames <- list(re_labels, re_labels)
  re_matrix
}

fetch_valid_matrix_types <- function() {
  c("omega", "sigma")
}

validate_matrix_type <- function(matrix_type) {
  stopifnot(isTRUE(unique(tolower(matrix_type) %in% fetch_valid_matrix_types())))
  tolower(matrix_type)
}

label_model_re_matrices <- function(mod, matrix_type = c("omega", "sigma")) {
  matrix_type <- validate_matrix_type(matrix_type)
  re_matrices <- extract_model_re_matrices(mod = mod)
  re_labels <- extract_model_re_labels(mod = mod)
  setNames(lapply(matrix_type, function(curr_type) label_re_matrix(re_matrix = re_matrices[[curr_type]], re_labels = re_labels[[curr_type]])), matrix_type)
}

###############################################################################|
## covariance -> correlation random effect matrix conversion functions ---------
###############################################################################|

double_sweep_cov_to_corr_matrix <- function(cov_matrix, validate_corr = TRUE, zero_nan = TRUE) {
  diag_variances <- diag(cov_matrix)
  diag_stdev <- sqrt(diag_variances)
  corr_matrix <- sweep(sweep(cov_matrix, 1, diag_stdev, "/"), 2, diag_stdev, "/")
  if (isTRUE(zero_nan)) corr_matrix[is.nan(corr_matrix)] <- 0
  diag(corr_matrix) <- diag_variances
  if (isTRUE(validate_corr)) {
    validate_corr_matrix(corr_matrix = corr_matrix)
  }
  corr_matrix
}

convert_model_re_matrices_to_corr <- function(mod, matrix_type = c("omega", "sigma")) {
  ## called in `fetch_model_omega`, `fetch_model_sigma`, & `output$report` downloadHandler
  matrix_type <- validate_matrix_type(matrix_type)
  cov_re_matrices <- label_model_re_matrices(mod = mod, matrix_type = matrix_type)
  setNames(lapply(matrix_type, function(curr_type) double_sweep_cov_to_corr_matrix(cov_matrix = cov_re_matrices[[curr_type]], zero_nan = TRUE)), matrix_type)
}


###############################################################################|
## correlation -> covariance random effect matrix conversion functions ---------
###############################################################################|

force_symmetric_re_matrix <- function(re_matrix) {
  sym_re_matrix <- as_bmat(t(re_matrix)[upper.tri(t(re_matrix), diag = TRUE)])
  stopifnot(isTRUE(isSymmetric(sym_re_matrix)))
  dimnames(sym_re_matrix) <- dimnames(re_matrix)
  sym_re_matrix
}

double_sweep_corr_to_cov_matrix <- function(corr_matrix, validate_corr = TRUE) {
  if (isTRUE(validate_corr)) {
    validate_corr_matrix(corr_matrix = corr_matrix)
  }
  diag_variances <- diag(corr_matrix)
  diag_stdev <- sqrt(diag_variances)
  cov_matrix <- sweep(sweep(corr_matrix, 1, diag_stdev, "*"), 2, diag_stdev, "*")
  diag(cov_matrix) <- diag_variances
  cov_matrix <- force_symmetric_re_matrix(re_matrix = cov_matrix)
  cov_matrix
}


###############################################################################|
## validate random effect matrix functions -------------------------------------
###############################################################################|

validate_matrix_diag_GE_0 <- function(re_matrix, return_res = TRUE) {
  is_valid_diag <- unique(diag(re_matrix) >= 0)
  if (isTRUE(return_res) && isTRUE(is_valid_diag)) {
    return(TRUE)
  } else if (isTRUE(return_res) && !isTRUE(is_valid_diag)) {
    return(FALSE)
  } else if (isFALSE(return_res) && !isTRUE(is_valid_diag)) {
    validate("Variances along matrix diagonal must be >= 0", errorClass = "invalid-input")
  }
}

validate_corr_matrix <- function(corr_matrix, return_res = FALSE) {
  if (isTRUE(unique(dim(corr_matrix)) <= 1)) return(TRUE)
  corr_lower_tri <- corr_matrix[lower.tri(corr_matrix, diag = FALSE)]
  corr_range <- signif(range(corr_lower_tri, na.rm = TRUE), digits = 12)
  is_valid_corr_range <- corr_range[1] >= -1 && corr_range[2] <= 1
  if (isTRUE(return_res) && isTRUE(is_valid_corr_range)) {
    return(TRUE)
  } else if (isTRUE(return_res) && !isTRUE(is_valid_corr_range)) {
    return(FALSE)
  } else if (isFALSE(return_res) && !isTRUE(is_valid_corr_range)) {
    validate("Correlation coefficient must be in the range [-1, 1]", errorClass = "invalid-input")
  }
}

eigen_decomp_GE_0 <- function(re_matrix, print_results = FALSE) {
  print_function_calls(fn_name = "eigen_decomp_GE_0", fn_type = "function")
  re_eigendecomp <- try(expr = eigen(re_matrix), silent = TRUE)
  is_valid_eigendecomp <- dplyr::if_else(isTRUE("try-error" %in% class(re_eigendecomp)), FALSE, TRUE)
  if (isTRUE(is_valid_eigendecomp)) {
    if (isTRUE(print_results)) print_obj_to_console(re_eigendecomp$values)
    is_eigendecomp_GE_0 <- isTRUE(unique(re_eigendecomp$values >= 0))
  } else {
    is_eigendecomp_GE_0 <- FALSE
  }
  if (isTRUE(print_results)) print_obj_to_console(is_eigendecomp_GE_0)
  return(is_eigendecomp_GE_0)
}

is_valid_mrgsolve_mvgauss <- function(re_matrix, print_results = FALSE) {
  print_function_calls(fn_name = "is_valid_mrgsolve_mvgauss", fn_type = "function")
  re_mvgauss <- mrgsolve::mvgauss(re_matrix)
  is_valid_mvgauss <- dplyr::if_else(isTRUE(unique(unlist(as.list(is.nan(re_mvgauss))))), FALSE, TRUE)
  if (isTRUE(print_results)) print_obj_to_console(re_mvgauss)
  if (isTRUE(print_results)) print_obj_to_console(is_valid_mvgauss)
  is_valid_mvgauss
}

is_valid_re_matrix <- function(re_matrix, print_results = FALSE) {
  print_function_calls(fn_name = "is_valid_re_matrix", fn_type = "function")
  
  is_sq_matrix <- matrixcalc::is.square.matrix(re_matrix)
  is_sym_matrix <- matrixcalc::is.symmetric.matrix(re_matrix)
  det_matrix <- det(re_matrix)
  is_det_GE_0 <- isTRUE(det_matrix >= 0)
  is_psd_matrix <- matrixcalc::is.positive.semi.definite(re_matrix)
  
  is_eigendecomp_GE_0 <- eigen_decomp_GE_0(re_matrix, print_results = print_results)
  is_valid_mvgauss <- is_valid_mrgsolve_mvgauss(re_matrix, print_results = print_results)
  
  is_valid_matrix <- is_valid_mvgauss & is_sq_matrix & is_sym_matrix & is_det_GE_0 & is_psd_matrix
  if (isTRUE(print_results)) print_obj_to_console(is_valid_matrix)
  is_valid_matrix
}


###############################################################################|
## UI & user input functions ---------------------------------------------------
###############################################################################|

input_as_numeric <- function(x) {
  as.numeric(as.character(interpret(x)))
}

interpret <- function(expr_str, max_length = 32, 
                      whitelist = c("-", "+", "/", "*", "sqrt", "c", "numeric")) {
  ## source:  https://community.rstudio.com/t/secure-way-to-accept-user-input-for-shiny-apps/13554
  safer_eval <- function(expr) {
    if (rlang::is_call(expr)) {
      fn_name <- rlang::call_name(expr)
      if (!fn_name %in% whitelist) stop("Disallowed function: ", fn_name)
      do.call(get(fn_name, baseenv()), Map(safer_eval, rlang::call_args(expr)))
    } else if (rlang::is_syntactic_literal(expr)) {
      expr
    } else {
      stop("Unknown expression: ", expr)
    }
  }
  
  stopifnot(length(expr_str) < max_length)
  
  try_parse <- try(parse(text = expr_str), silent = TRUE)
  if ("try-error" %in% class(try_parse)) return(NULL)
  
  if (!isTRUE(nzchar(expr_str))) return(NULL)
  
  safer_eval(rlang::parse_expr(expr_str))
}


## programmatically create model parameter input ids
build_model_param_value_ids <- function(mod) {
  paste0(names(extract_model_params(mod = mod)), "_value")
}

build_model_param_units_ids <- function(mod) {
  paste0(names(extract_model_params(mod = mod)), "_units")
}

make_model_param_value_labels <- function(mod) {
  gsub(pattern="_v", replacement=" V", build_model_param_value_ids(mod = mod), fixed = TRUE)
}

make_model_param_units_labels <- function(mod) {
  gsub(pattern="_u", replacement=" U", build_model_param_units_ids(mod = mod), fixed = TRUE)
}

split_param_idxs_in_half <- function(mod) {
  n_params <- length(param(mod))
  idxs_midpoint <- ceiling(n_params/2)
  
  split_idxs <- list(
    `1st` = 1:idxs_midpoint,
    `2nd` = (idxs_midpoint+1):n_params
  )
  stopifnot(identical(sum(unlist(lapply(split_idxs, length))), n_params))
  split_idxs
}

build_fixed_param_text_input_boxes <- function(idxs, param_ids, param_labels, param_values, box_type = "value") {
  valid_box_types <- c("value", "units")
  stopifnot(isTRUE(box_type %in% valid_box_types))
  box_style <- dplyr::if_else(isTRUE(box_type %in% "value"), 
                              "color: #0A5EAA; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;", 
                              "color: #808080; font-weight: bold; margin-botton: -10px; text-align: center; vertical-align: middle;")
  
  fixed_param_boxes <- Reduce(paste0,
                              lapply(idxs, function(idx) {
                                textInput(
                                  inputId = param_ids[idx], 
                                  label = span(param_labels[idx], style=box_style),
                                  value = param_values[[idx]]
                                )}
                              )
  )
  as.character(fixed_param_boxes)
}

build_fixed_param_modal_window <- function(mod) {
  ## called in `output$fixed_params_modal_uiout` renderUI
  
  model_params <- extract_model_params(mod = mod)
  param_values <- unlist(model_params)
  param_units <- extract_model_param_annot_data(mod)$unit
  param_value_ids <- build_model_param_value_ids(mod = mod)
  param_units_ids <- build_model_param_units_ids(mod = mod)
  param_value_labels <- make_model_param_value_labels(mod = mod)
  param_units_labels <- make_model_param_units_labels(mod = mod)
  split_idxs <- split_param_idxs_in_half(mod = mod)
  
  fixed_param_lists <- list(
    list(idxs = split_idxs$`1st`, param_ids = param_value_ids, 
         param_labels = param_value_labels, param_values = param_values, box_type = "value"),
    list(idxs = split_idxs$`1st`, param_ids = param_units_ids, 
         param_labels = param_units_labels, param_values = param_units, box_type = "units"),
    list(idxs = split_idxs$`2nd`, param_ids = param_value_ids, 
         param_labels = param_value_labels, param_values = param_values, box_type = "value"),
    list(idxs = split_idxs$`2nd`, param_ids = param_units_ids, 
         param_labels = param_units_labels, param_values = param_units, box_type = "units")
  )
  
  fp_popup <- purrr::map(fixed_param_lists, ~ do.call("build_fixed_param_text_input_boxes", .x))
  fp_popup_html <- paste0(
    tags$div(class="user_input_popup",
             tags$div(class="left_align_col",  HTML(fp_popup[[1]])),
             tags$div(class="left_align_col",  HTML(fp_popup[[2]])),
             tags$div(class="left_align_col",  HTML(fp_popup[[3]])),
             tags$div(class="left_align_col",  HTML(fp_popup[[4]]))
    )
  )
  
  fp_popup_full <- paste0(tags$div(class="user_input_popup", purrr::map(fixed_param_lists, ~ tags$div(class="left_align_col",  HTML(do.call("build_fixed_param_text_input_boxes", .x))))))
  stopifnot(identical(fp_popup_full, fp_popup_html))
  
  return(fp_popup_html)
}

reorder_modal_popup_elements <- function(popup_html) {
  ## called in "UI_dose_selection.R"
  popup_string <- as.character(popup_html)
  popup_reorder <- gsub(pattern = '(<button type=\\"button\\" class=\"close\" data-dismiss=\"modal\">\n          <span>&times;</span>\n        </button>)(\\n        )(<h4 class=\\"modal-title\\">.*</h4>)', "\\3\\2\\1", popup_string, perl = TRUE)
  popup_reorder <- gsub(pattern = '<span>&times;</span>', "", popup_reorder, perl = TRUE)
  return(HTML(popup_reorder))
}


reorder_modal_popup_children <- function(popup_html) {
  ## called in "UI_dose_selection.R" & `output$fixed_params_modal_uiout` renderUI
  popup_html$children[[1]]$children[[1]]$children[[1]]$children[[1]]$children[[1]]$children <- ""
  popup_children <- popup_html$children[[1]]$children[[1]]$children[[1]]$children
  popup_html$children[[1]]$children[[1]]$children[[1]]$children <- rev(popup_children)
  popup_html
}

render_interactive_matrix <- function() {
  ## called in `build_omega_table` & `build_sigma_table` eventReactive functions
  ## docs:  https://jrowen.github.io/rhandsontable/#Conditional_Formatting
  renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (row == col) {
              td.style.background = 'lightgrey';
             } else if (col > row) {
              td.style.background = 'grey';
              td.style.color = 'grey';
              cellProperties.readOnly = 'true';
             } else if (value <= -0.75) {
              td.style.background = 'pink';
             } else if (value >= 0.75) {
              td.style.background = 'lightgreen';
             }
           }"
  renderer
}

###############################################################################|
## Rx input dosing regimen parsing functions -----------------------------------
###############################################################################|

regexec_match <- function(x, pattern, print_result = FALSE) {
  m  <- regexec(pattern, x, ignore.case = TRUE, perl = TRUE)
  xm <- regmatches(x, m)
  if (isTRUE(print_result)) print_obj_to_console(xm)
  return(xm)
}

dosing_period_regex_patterns <- function() {
  list(dose_amount  = "^ *(\\d+[\\.]*\\d*[Ee\\+\\-]{0,2}\\d*)",
       dose_route   = "(?i)\\b(iv|sc)+\\b",
       infusion_dur = "ov[er]* +(\\d+[\\.]*\\d*)",
       cmt_number   = "in +(\\d+)",
       dose_freq    = "[Qq] *(\\d+[\\.]*\\d*)",
       total_doses  = " *x *(\\d+)",
       delay_time   = "aft[er]* +(-*\\d+[\\.]*\\d*)"
  )
}

parse_rx_period_parts <- function(rx_split_periods, print_result = FALSE) {
  df_rx_period_parts_parsed <- tibble::as_tibble(data.table::rbindlist(setNames(lapply(rx_split_periods, function(curr_period) tibble::as_tibble(lapply(dosing_period_regex_patterns(), function(curr_pattern) unlist(regexec_match(curr_period, pattern = curr_pattern))[2]))), rx_split_periods), fill = TRUE, idcol = "rx_period"))
  if (isTRUE(print_result))  print_obj_to_console(df_rx_period_parts_parsed)
  return(df_rx_period_parts_parsed)
}

count_rx_period_parts <- function(rx_split_periods, print_result = FALSE) {
  df_rx_period_parts_counts <- tibble::as_tibble(data.table::rbindlist(setNames(lapply(rx_split_periods, function(curr_rx_period) tibble::as_tibble(as.list(setNames(stringi::stri_count_regex(curr_rx_period, dosing_period_regex_patterns(), case_insensitive=TRUE), names(dosing_period_regex_patterns()))))), rx_split_periods), fill = TRUE, idcol = "rx_period"))
  if (isTRUE(print_result))  print_obj_to_console(df_rx_period_parts_counts)
  return(df_rx_period_parts_counts)
}

df_has_rx_period_parts <- function(df_rx) {
  ## verify data.frame input for `df_rx`
  stopifnot(isTRUE("data.frame" %in% class(df_rx)))  
  df_rx_cols <- names(df_rx)
  df_rx_idcol <- df_rx_cols[1]
  df_rx_parts_cols <- setdiff(df_rx_cols, df_rx_idcol)
  
  df_rx_has_parts <- df_rx %>% 
    dplyr::na_if(., "") %>% 
    dplyr::mutate(dplyr::across(.cols = tidyselect::all_of(df_rx_parts_cols), 
                                .fns = ~ dplyr::if_else(!is.na(.x), TRUE, FALSE), .names = "has_{.col}"))
  return(df_rx_has_parts)
}

validate_rx_part_counts <- function(rx_split_periods, print_result = FALSE) {
  ## called by `valid_rx_input({})` eventReactive
  valid_rx_parts <- NA
  df_rx_period_parts_parsed <- parse_rx_period_parts(rx_split_periods = rx_split_periods)
  df_rx_period_parts_exists <- df_has_rx_period_parts(df_rx = df_rx_period_parts_parsed)
  if (!exists("print_result") || isTRUE(print_result)) print_obj_to_console(df_rx_period_parts_exists)
  
  if (!isTRUE(unique(df_rx_period_parts_exists$has_dose_amount))) {
    valid_rx_parts <- FALSE
  }
  
  df_rx_period_parts_counts <- count_rx_period_parts(rx_split_periods = rx_split_periods)
  if (!exists("print_result") || isTRUE(print_result)) print_obj_to_console(df_rx_period_parts_counts)
  is_valid_rx_part_counts <- unlist(
    setNames(
      lapply(1:nrow(df_rx_period_parts_counts), 
             function(row_idx) {
               max_counts_LE_1 <- isTRUE(unique(unlist(as.list(df_rx_period_parts_counts[row_idx, setdiff(names(df_rx_period_parts_counts), "rx_period")] <= 1))))
               min_counts_GE_0 <- isTRUE(unique(unlist(as.list(df_rx_period_parts_counts[row_idx, setdiff(names(df_rx_period_parts_counts), "rx_period")] >= 0))))
               amt_counts_EQ_1 <- isTRUE(df_rx_period_parts_counts$dose_amount[row_idx] == 1)
               all_counts_true <- max_counts_LE_1 & min_counts_GE_0 & amt_counts_EQ_1
               return(all_counts_true)
             }
      ), df_rx_period_parts_counts$rx_period)
  )
  
  if (!isTRUE(unique(is_valid_rx_part_counts))) {
    valid_rx_parts <- FALSE
  }
  if (isTRUE(is.na(valid_rx_parts))) {
    valid_rx_parts <- TRUE
  }
  return(valid_rx_parts)
}

clean_simple_text <- function(simple_text, to_lower = FALSE) {
  trim_text <- trimws(simple_text, which = "both")
  clean_text <- gsub(pattern="(?i)\\s+", replacement=" ", trim_text, perl=TRUE)
  if (isTRUE(to_lower)) {
    clean_text <- tolower(clean_text)
  }
  return(clean_text)
}

split_rx_dosing_periods <- function(x) {
  rx_clean_text <- clean_simple_text(simple_text = x, to_lower = TRUE)
  rx_split_periods <- trimws(unlist(strsplit(rx_clean_text, "then|,")))
  rx_split_periods
}

###############################################################################|
## Rx dosing profile plotting function -----------------------------------------
###############################################################################|
## consider moving these functions to "R/backend.R"

plot_sim_model_ev_data_dosing_regimen <- function(mod_sim_data) {
  ## called by `sim_dosing_dataset({})` eventReactive
  print_function_calls(fn_name = "plot_sim_model_ev_data_dosing_regimen", fn_type = "function")

  dv_nms <- c("DV", "CP", "IPRED")
  ev_nms <- toupper(names(mod_sim_data))
  dv_col <- intersect(dv_nms, ev_nms)[1]
  if (is.null(dv_col) || is.na(dv_col)) {
    dv_col <- ev_nms[length(ev_nms)]
  }
  
  plot_formula <- as.formula(paste0(dv_col, "~", "TIME"))
  mrgsolve::plot(mod_sim_data, plot_formula, groups = REGNUM, type = "l", 
                 auto.key = list(title = "  REGNUM", points = FALSE, lines = TRUE, space = "right", cex.title = 1.25),
                 aspect	= "fill")
}

preview_rx_input_profile <- function(ev_data, mod, start = 0, end_time = NULL, delta = 1.0, add = numeric(0)) {
  ## called by `sim_dosing_dataset({})` eventReactive
  print_function_calls(fn_name = "preview_rx_input_profile", fn_type = "function")
  mod_tv <- mrgsolve::zero_re(mod)
  df_ev <- ev_cols_tolower(fill_default_ev_data_cols(ev_data)) %>% rename_all(toupper)
  
  if (!"ID"  %in% names(df_ev)) df_ev <- df_ev %>% dplyr::mutate(ID = 1L)
  if ("RATE" %in% names(df_ev)) df_ev <- df_ev %>% tidyr::replace_na(list(RATE = 0))
  if (!"REGNUM" %in% names(df_ev)) df_ev <- df_ev %>% dplyr::mutate(REGNUM = ID)
  
  if (isTRUE(is.null(end_time))) {
    df_ev_regs <- df_ev %>% rename_all(toupper) %>% group_by(REGNUM) %>% group_split()
    end_time <- max(purrr::map_dbl(df_ev_regs, ~ calc_dosing_end_time(dose_data = .x, delta = delta)), na.rm = TRUE)
  }
  
  tg <- mrgsolve::tgrid(start = start, end = end_time, delta = delta, add = add)
  mod_update  <- mrgsolve::update(mod_tv, rtol = 1e-10, atol = 1e-12, ss_rtol = 1e-10, ss_atol = 1e-12, maxsteps = 10000,
                                  start = tg@start, end = tg@end, delta = tg@delta,add = tg@add, offset = tg@offset, tscale = tg@scale)
  carry_sim_cols <- c(names(define_default_ev_data_cols()), "REGNUM")
  mod_sim_data <- mrgsolve::mrgsim_d(mod_update, df_ev, carry_out = carry_sim_cols)
  return(mod_sim_data)
}


###############################################################################|
## utility functions -----------------------------------------------------------
###############################################################################|


## for assigning default argument values (rhs) in functions if input argument is
## NULL indicating that it's missing
`%||%` <- function(lhs, rhs) { 
  if (!is.null(lhs)) { 
    invisible(lhs) 
  } else { 
    invisible(rhs) 
  } 
}

tc <- function(x) { 
  cat("   Type:  ", typeof(x),sep="", fill=TRUE) ; cat("   Class: ", paste(class(x), collapse=", "),sep="", fill=TRUE) 
}

tidyerr <- function() { 
  cat("\n\n   Last error:  \n", sep="", fill=TRUE)
  print(rlang::last_error())
  cat("\n\n   Last trace:  \n", sep="", fill=TRUE)
  print(rlang::last_trace()) 
}

list_fn_defs_in_file <- function(filename) {
  ## https://stackoverflow.com/questions/20259681/how-to-list-all-the-functions-signatures-in-an-r-file
  temp.env <- new.env()
  sys.source(filename, envir = temp.env)
  functions <- lsf.str(envir = temp.env)
  rm(temp.env)
  return(functions)
}

pkg_load_order <- function() {
  c("shiny", "tablerDash", "shinyWidgets", "shinyFeedback", "shinyBS", 
    "shinyjs", "rhandsontable", "reactable", "htmlwidgets", "plotly", "bs4Dash", 
    "mrgsolve", "dplyr", "purrr", "data.table", "DT", "ggplot2", "flux", 
    "markdown", "rmarkdown", "ggpubr", "matrixcalc", "assertr", "tidyr", 
    "tinytex")
}

session_pkg_versions <- function(load_order = TRUE) {
  pkg_versions <- purrr::map(sessionInfo()$otherPkgs, ~ .x[["Version"]])
  if (isTRUE(load_order)) pkg_versions <- pkg_versions[match(pkg_load_order(), names(pkg_versions))]
  pkg_versions
}

cat_pkg_versions <- function() {
  pkg_versions <- session_pkg_versions(load_order = TRUE)
  cat(paste(names(pkg_versions), unname(pkg_versions), sep = " = "), sep = "\n")
}

list_fns_shiny_app_Rscripts <- function() {
  fns_file_paths <- list.files(path = c(".", "R", "Functions"), pattern = "(?i)\\.R$", full.names = TRUE)
  fns_file_names <- basename(fns_file_paths)
  fns <- setNames(map(fns_file_paths, NCmisc::list.functions.in.file), fns_file_names)
  fns
}

verify_no_browser_fn_calls <- function(print_test_result = TRUE) {
  fns <- list_fns_shiny_app_Rscripts()
  browser_fn_idxs <- grep(pattern = "(?i)browser", unlist(fns), perl = TRUE)
  stopifnot(identical(browser_fn_idxs, integer(0)))
  if (isTRUE(print_test_result)) cat("\n\n   function()  -->  ", sub("(?i)^test_", "", match.call()[1]), "() passed all tests!!!   \n\n", sep='', fill=TRUE)
}

list_server_fns <- function(alphabetic = TRUE) {
  server_fns <- NCmisc::list.functions.in.file("server.R", alphabetic = alphabetic) 
  names(server_fns)[which(names(server_fns) == ".GlobalEnv")] <- "external_fns"
  names(server_fns)[which(names(server_fns) == "character(0)")] <- "defined_fns"
  renamed_elements <- c("defined_fns", "external_fns")
  new_list_order <- c(renamed_elements, setdiff(names(server_fns), renamed_elements))
  server_fns[new_list_order]
}


