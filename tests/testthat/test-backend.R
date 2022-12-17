## print log debugging statements
print("test-backend.R")
print(getwd())


## docs for `testthat::source_*` functions:  https://testthat.r-lib.org/reference/source_file.html
here::i_am("tests/testthat/test-backend.R")
if (isTRUE(interactive())) {
  testthat::source_test_helpers("tests/testthat", env = rlang::current_env())
  testthat::source_test_setup("tests/testthat", env = rlang::current_env())
}


#######################################################################################################################|
## mock `print_function_calls()` "R/helpers.R" function so messages are suppressed during unit testing -------------
#######################################################################################################################|

mockr::with_mock(
  print_function_calls = omit_function_calls,
  .env = topenv(), {
    
    
#########################################################################################|
## shiny app backend functions -----------------------------------------------------------
#########################################################################################|


## format_temp_file_path() ---------------------------------------------------------------
testthat::test_that("format_temp_file_path", {
  test_file <- "popPK-simulation-report_1969-08-17.pdf"
  test_tempdir <- tempdir()
  test_path <- file.path(test_tempdir, test_file)
  test_path_out <- file.path(normalizePath(path = test_tempdir, winslash = .Platform$file.sep), test_file)
  temp_file_path <- withr::with_tempfile(test_path, format_temp_file_path(test_path), tmpdir = test_tempdir)
  testthat::skip_on_os(os = c("mac", "linux", "solaris"))
  testthat::expect_identical(temp_file_path, test_path_out)
})



## copy_file_to_temp_dir() ---------------------------------------------------------------
testthat::test_that("copy_file_to_temp_dir", {
  test_file <- here::here("reports/report.Rmd")
  test_tempdir <- tempdir()
  test_path <- format_temp_file_path(file.path(test_tempdir, basename(test_file)))
  if (isTRUE(file.exists(test_path))) unlink(test_path)
  testthat::expect_false(file.exists(test_path))
  test_result <- withr::with_tempfile(test_path, copy_file_to_temp_dir(dirname(test_path), test_file), tmpdir = test_tempdir)
  testthat::expect_identical(test_result, test_path)
  testthat::expect_true(file.exists(test_path))
})



## construct_rmd_param_list() ------------------------------------------------------------
testthat::test_that("construct_rmd_param_list", {
    
    input_as_numeric_mock <- function(x) {
      as.numeric(as.character(x))
    }
    
    mockr::local_mock(input_as_numeric = input_as_numeric_mock, .env = topenv())
    
    build_libmod_user_input_data_structure <- function(mod_name, mod_obj = NULL) {
      cur_mod <- mod_obj %||% app_libmod_objs[[mod_name]]
      model_params <- extract_model_params(mod = cur_mod)
      param_values <- unlist(model_params)
      param_annot <- extract_model_param_annot_data(cur_mod)
      param_units <- setNames(param_annot$unit,  param_annot$name)
      param_descr <- setNames(param_annot$descr, param_annot$name)
      param_value_ids <- build_model_param_value_ids(mod = cur_mod)
      param_units_ids <- build_model_param_units_ids(mod = cur_mod)
      test_input_values <- purrr::map(param_value_ids %>% purrr::set_names(.), ~ param_values[[gsub("(?i)_value", "", .x)]])
      test_input_units  <- purrr::map(param_units_ids %>% purrr::set_names(.), ~ param_units[[gsub("(?i)_units", "", .x)]])
      test_input_struct <- c(test_input_values, test_input_units)
      test_input_struct
    }
    
    strip_mod_solver_settings <- function(mod) {
      list_S4_slots(mod)[define_solver_settings_params()]
    }
  
    def_mod <- app_libmod_objs[[fetch_default_libmod_name()]]
    def_mod_input <- build_libmod_user_input_data_structure(mod_obj = def_mod, mod_name = NULL)
    def_mod_info <- build_mod_param_info_fn(def_mod, def_mod_input)
    rx_input <- define_default_rx_input()
    dose_data <- fill_default_ev_data_cols(mrgsolve::ev_rx(rx_input))
    tg_input <- mrgsolve::tgrid()
    sim_settings <- fetch_default_sim_settings()
    sum_stats <- NULL
    sim_plots <- NULL
    title <- "Test Title"
    author <- "Unit McTester"
    compound <- "Test Drug"
    description <- "This is for UNIT TESTING!!!"
    units <- "mg/L"
    model_code <- def_mod@code
    mod_slots <- list_S4_slots(def_mod)
    tg <- adjust_time_grid_dosing_fn(dose_data = dose_data, tg_input = tg_input, user_spec = FALSE)
    mod_details <- extract_model_annotations_data(def_mod)
    
    
    def_mod_rmd_list <- construct_rmd_param_list(
      mod = def_mod, dose_data, tg, sum_stats, sim_plots, title, author, compound,
      description, units, rx_input, modup_params = def_mod_info, sim_settings, model_code)
    
  
    ## test each individual field in the Rmd parameter list
    testthat::expect_type(def_mod_rmd_list, "list")
    testthat::expect_length(def_mod_rmd_list, 25L)
    testthat::expect_named(def_mod_rmd_list, ignore.order = TRUE,
                           c("sumstat", "graphs", "set_title", "set_author", "compound", 
                             "description", "dose_units", "model_name", "model_file", "model_param", 
                             "model_omega", "model_sigma", "mod_names", "mod_details", "mod_init", 
                             "mod_outvars", "sim_opts", "solver_opts", "tgrid_list", "sim_times", 
                             "rx_input", "rx_dataset", "mod_code", "mod_slots", "mod_obj"))
    testthat::expect_identical(def_mod_rmd_list$sumstat, sum_stats)
    testthat::expect_identical(def_mod_rmd_list$graphs, sim_plots)
    testthat::expect_identical(def_mod_rmd_list$set_title, title)
    testthat::expect_identical(def_mod_rmd_list$set_author, author)
    testthat::expect_identical(def_mod_rmd_list$compound, compound)
    testthat::expect_identical(def_mod_rmd_list$description, description)
    testthat::expect_identical(def_mod_rmd_list$dose_units, dose_units(units))
    testthat::expect_identical(def_mod_rmd_list$model_name, def_mod@model)
    testthat::expect_identical(def_mod_rmd_list$model_file, def_mod@modfile)
    testthat::expect_identical(def_mod_rmd_list$model_param, def_mod_info)
    testthat::expect_identical(def_mod_rmd_list$model_omega, convert_model_re_matrices_to_corr(def_mod)$omega)
    testthat::expect_identical(def_mod_rmd_list$model_sigma, convert_model_re_matrices_to_corr(def_mod)$sigma)
    testthat::expect_identical(def_mod_rmd_list$mod_names, names(def_mod))
    testthat::expect_identical(def_mod_rmd_list$mod_details, mod_details)
    testthat::expect_identical(def_mod_rmd_list$mod_init, init(def_mod))
    testthat::expect_identical(def_mod_rmd_list$mod_outvars, outvars(def_mod))
    testthat::expect_identical(def_mod_rmd_list$sim_opts, sim_settings)
    testthat::expect_identical(def_mod_rmd_list$solver_opts, mod_slots[define_solver_settings_params()])
    testthat::expect_identical(def_mod_rmd_list$tgrid_list, list_S4_slots(tg))
    testthat::expect_identical(def_mod_rmd_list$sim_times, stime(tg))
    testthat::expect_identical(def_mod_rmd_list$rx_input, rx_input)
    testthat::expect_identical(def_mod_rmd_list$rx_dataset, dose_data)
    testthat::expect_identical(def_mod_rmd_list$mod_code, model_code)
    testthat::expect_identical(def_mod_rmd_list$mod_slots, mod_slots)
    testthat::expect_identical(def_mod_rmd_list$mod_obj, def_mod)
    
    
    ## test case for user specified simulation end time 
    tg_user <- adjust_time_grid_dosing_fn(dose_data = dose_data, 
                                          tg_input = mrgsolve::tgrid(), 
                                          user_spec = TRUE)
    user_spec_rmd_list <- construct_rmd_param_list(
      mod = def_mod, dose_data, tg_user, sum_stats, sim_plots, title, author, compound,
      description, units, rx_input, modup_params = def_mod_info, sim_settings, model_code)
    testthat::expect_identical(user_spec_rmd_list$tgrid_list, list_S4_slots(tg_user))
    testthat::expect_identical(user_spec_rmd_list$sim_times, stime(tg_user))
})



## initial_step_list() -------------------------------------------------------------------
testthat::test_that("initial_step_list is named list with all TRUE values", {
  testthat::expect_type(initial_step_list(), "list")
  testthat::expect_named(initial_step_list(), c("load_model", "specify_rx_dosing", "expand_rx_dataset", "run_simulation"))
  testthat::expect_identical(unique(unname(unlist(initial_step_list()))), TRUE)
})



## sim_steps_html_list() -----------------------------------------------------------------
testthat::test_that("sim_steps_html_list is named list with matching html list elements", {
  
  sim_step_list <- list(
    load_model = "<li>Load Model</li>",
    specify_rx_dosing = "<li>Specify Rx Dosing</li>",
    expand_rx_dataset = "<li>Expand Rx Dataset</li>",
    run_simulation = "<li>Run Simulation</li>"
  )
  
  sim_step_names <- names(sim_step_list)
  sim_step_names_hardcode <- c("load_model", "specify_rx_dosing", "expand_rx_dataset", "run_simulation")
  
  testthat::expect_identical(sim_step_names, sim_step_names_hardcode)
  
  wrap_as_html_list_item <- function(x) {
    paste0("<li>", x, "</li>")
  }
  
  step_names_to_labels <- function(x) {
    wrap_as_html_list_item(stringr::str_to_title(stringr::str_replace_all(x, pattern = "_", replacement = " ")))
  }
  
  testthat::expect_type(sim_steps_html_list(), "list")
  testthat::expect_named(sim_steps_html_list(), sim_step_names)
  testthat::expect_identical(sim_steps_html_list(), sim_step_list)
  testthat::expect_identical(unname(unlist(sim_steps_html_list())), step_names_to_labels(sim_step_names))
})




## update_sim_step_list_fn() -------------------------------------------------------------




## update_load_model_sim_step() ----------------------------------------------------------




## update_rx_submit_sim_step() -----------------------------------------------------------




## update_model_params_sim_step() --------------------------------------------------------




## update_expand_rx_data_sim_step() ------------------------------------------------------




## update_do_run_sim_step() --------------------------------------------------------------




## valid_rx_input_fn() -------------------------------------------------------------------




## submit_rx_input_fn() ------------------------------------------------------------------




## rx_dose_data_fn() ---------------------------------------------------------------------




## read_model_file_fn() ------------------------------------------------------------------
testthat::test_that("read_model_file_fn model library files", {
  
  ## test the first model in the custom model library (different from default model)
  test_env <- new.env()
  test_mod <- app_libmod_names[1]
  test_res <- withr::with_dir(here::here(), read_model_file_fn(model_selection =test_mod, upload_model = NULL, model_selection_method = "Select from Model Library"))
  true_res <- app_libmod_objs[[test_mod]]
  
  testthat::expect_s4_class(test_res, "mrgmod")
  testthat::expect_named(names(test_res), c("param", "init", "capture", "omega", "sigma", "omega_labels", "sigma_labels"))
  testthat::expect_named(test_res, list(param = c("TVCL", "TVV2", "TVKA", "TVF1", "TVVMAX", "TVKM", "DOSE", "WT", "WTREF", "WTCL", "WTV2"), 
                                        init = c("ABS", "CENT"), 
                                        capture = c("DOSE", "WT", "WTREF", "DV", "IPRED"), 
                                        omega = "...", 
                                        sigma = "...", 
                                        omega_labels = list(c("ETA_CL", "ETA_V2", "ETA_KA", "ETA_F1", "ETA_VMAX", "ETA_KM")), 
                                        sigma_labels = list(c("PROP", "ADD"))))
  testthat::expect_identical(set_mod_envir_slot(test_res, envir = test_env), set_mod_envir_slot(true_res, envir = test_env))
  
  ## read in all custom library models and check for equality
  all_libmod_test <- withr::with_dir(here::here(), purrr::map(app_libmod_names, ~ read_model_file_fn(model_selection = .x, upload_model = NULL, model_selection_method = "Select from Model Library")))
  testthat::expect_equal(all_libmod_test, app_libmod_objs)
  
  
  ## read in all mrgsolve package models library models and check for equality
  mrgmod_upload_list <- purrr::map(all_mrgmod_objs, ~ list(
    datapath = as.list(.x)$cfile,
    name = extract_path_file_stem(as.list(.x)$cfile)
  ))
  mrgmod_upload_test <- withr::with_dir(here::here(), purrr::map(mrgmod_upload_list, ~ read_model_file_fn(model_selection = NULL, upload_model = .x, model_selection_method = "Upload Custom Model File")))
  testthat::expect_equal(mrgmod_upload_test, all_mrgmod_objs)
  
  
  ## test errors
  testthat::expect_error(withr::with_dir(here::here(), read_model_file_fn(model_selection = "missing_model", upload_model = NULL, model_selection_method = "Select from Model Library")), regexp = "Model file doesn't exist!")
  testthat::expect_error(withr::with_dir(here::here(), read_model_file_fn(model_selection = "invalid_MM", upload_model = NULL, model_selection_method = "Select from Model Library")), regexp = "Invalid model specification!")
  
  mod_invalid_block_combo_code <- '
  $PARAM VMAX = 3110, KM = 188, VC = 2340, VP = 1324, CL = 2.75
  $FIXED Q = 18.67, KA = 0.00592, F = 0.72
  '
  
  mod_invalid_block_combo <- mrgsolve::mcode(model = "invalid_block_combo", code = mod_invalid_block_combo_code, compile = FALSE)
  mod_invalid_block_combo_upload <- list(
    datapath = as.list(mod_invalid_block_combo)$cfile,
    name = extract_path_file_stem(as.list(mod_invalid_block_combo)$cfile)
  )
  
  testthat::expect_s3_class(mrgsolve:::details(mod_invalid_block_combo), "data.frame")
  testthat::expect_s4_class(withr::with_dir(here::here(), read_model_file_fn(model_selection = NULL, upload_model = mod_invalid_block_combo_upload, model_selection_method = "Upload Custom Model File")), "mrgmod")
})



## fetch_model_code_fn() -----------------------------------------------------------------




## fetch_model_name_fn() -----------------------------------------------------------------




## loaded_model_msg_fn() -----------------------------------------------------------------




## calc_dosing_end_time() ----------------------------------------------------------------
testthat::test_that("calc_dosing_end_time", {
  dose_data  <- fill_default_ev_data_cols(ev_data = mrgsolve::ev_rx(define_default_rx_input()))
  tg <- extract_tgrid_defaults()
  scl <- define_tgrid_end_scaling_factor()
  
  testthat::expect_type(calc_dosing_end_time(dose_data), "double")
  testthat::expect_length(calc_dosing_end_time(dose_data), 1L)
  testthat::expect_named(calc_dosing_end_time(dose_data), NULL)
  testthat::expect_identical(calc_dosing_end_time(dose_data), 242)
  testthat::expect_identical(calc_dosing_end_time(dose_data, delta = tg$delta), 242)
  testthat::expect_identical(calc_dosing_end_time(dose_data, delta = 0.25), 242.25)
  testthat::expect_identical(calc_dosing_end_time(dose_data, delta = tg$delta), 242)
  testthat::expect_identical(calc_dosing_end_time(dose_data, delta = tg$delta, scaling_factor = scl), 242)
  testthat::expect_identical(calc_dosing_end_time(dose_data, delta = tg$delta, scaling_factor = scl), 242)
  testthat::expect_identical(calc_dosing_end_time(dose_data, delta = tg$delta, scaling_factor = 2), 323)
  testthat::expect_identical(calc_dosing_end_time(dose_data, delta = tg$delta, scaling_factor = 2), 323)
})



## build_time_grid_object_fn() -----------------------------------------------------------
testthat::test_that("build_time_grid_object_fn", {
  tg_res <- build_time_grid_object_fn(start = 1, end = 24, delta = 1, add = 0, offset = 0, scale = 1)
  tg_res_list <- list_S4_slots(tg_res)
  
  ## test results from `tgrid` s4 class
  testthat::expect_s4_class(tg_res, "tgrid")
  testthat::expect_identical(slotNames(tg_res), c("start", "end", "delta", "add", "offset", "scale"))
  testthat::expect_length(slotNames(tg_res), 6L)
  testthat::expect_identical(tg_res@start,  1)
  testthat::expect_identical(tg_res@end,   24)
  testthat::expect_identical(tg_res@delta,  1)
  testthat::expect_identical(tg_res@add,    0)
  testthat::expect_identical(tg_res@offset, 0)
  testthat::expect_identical(tg_res@scale,  1)
  
  ## test results from `tgrid` slot list
  testthat::expect_type(tg_res_list, "list")
  testthat::expect_named(tg_res_list, c("start", "end", "delta", "add", "offset", "scale"))
  testthat::expect_length(tg_res_list, 6L)
  testthat::expect_type(unlist(tg_res_list), "double")
  testthat::expect_identical(tg_res_list$start,  1)
  testthat::expect_identical(tg_res_list$end,   24)
  testthat::expect_identical(tg_res_list$delta,  1)
  testthat::expect_identical(tg_res_list$add,    0)
  testthat::expect_identical(tg_res_list$offset, 0)
  testthat::expect_identical(tg_res_list$scale,  1)
  
  ## test against default `tgrid` S4 values
  tg0 <- mrgsolve::tgrid()
  tg_build <- build_time_grid_object_fn(start = tg0@start, end = tg0@end, delta = tg0@delta, add = tg0@add, offset = tg0@offset, scale = tg0@scale)
  testthat::expect_identical(tg_build, tg0)
  
  ## test `tgrid()` function results against `build_time_grid_object_fn()`
  tg_true <- mrgsolve::tgrid(start = 10, end = 300, delta = 1, add = 0, .offset = 5, .scale = 2)
  tg_test <- build_time_grid_object_fn(start = 10, end = 300, delta = 1, add = 0, offset = 5, scale = 2)
  testthat::expect_identical(tg_test, tg_true)
})



## update_time_grid_end_time() -----------------------------------------------------------
testthat::test_that("update_time_grid_end_time", {
  tg0 <- mrgsolve::tgrid()
  tg0_res <- update_time_grid_end_time(tg = tg0, end_dosing = tg0@end)
  
  ## test default results from `tgrid` s4 class
  testthat::expect_s4_class(tg0_res, "tgrid")
  testthat::expect_identical(slotNames(tg0_res), c("start", "end", "delta", "add", "offset", "scale"))
  testthat::expect_length(slotNames(tg0_res), 6L)
  testthat::expect_identical(tg0_res@start,  0)
  testthat::expect_identical(tg0_res@end,   24)
  testthat::expect_identical(tg0_res@delta,  1)
  testthat::expect_identical(tg0_res@add,    numeric(0))
  testthat::expect_identical(tg0_res@offset, 0)
  testthat::expect_identical(tg0_res@scale,  1)
  testthat::expect_identical(tg0_res, tg0)
  
  
  ## test results after updating the `end_dosing` time to non-default values
  tg_update <- mrgsolve::tgrid(end = 1000)
  tg_update_res <- update_time_grid_end_time(tg = tg_update, end_dosing = 300)
  testthat::expect_identical(tg_update@end,  1000)
  testthat::expect_s4_class(tg_update_res, "tgrid")
  testthat::expect_identical(slotNames(tg_update_res), c("start", "end", "delta", "add", "offset", "scale"))
  testthat::expect_length(slotNames(tg_update_res), 6L)
  testthat::expect_identical(tg_update_res@start,  0)
  testthat::expect_identical(tg_update_res@end,   300)
  testthat::expect_identical(tg_update_res@delta,  1)
  testthat::expect_identical(tg_update_res@add,    numeric(0))
  testthat::expect_identical(tg_update_res@offset, 0)
  testthat::expect_identical(tg_update_res@scale,  1)
  testthat::expect_identical(tg_update_res, mrgsolve::tgrid(end = 300))
})



## adjust_time_grid_dosing_fn() ----------------------------------------------------------
testthat::test_that("adjust_time_grid_dosing_fn", {
  dose_data  <- fill_default_ev_data_cols(ev_data = mrgsolve::ev_rx(define_default_rx_input()))
  tg0 <- mrgsolve::tgrid()
  scl <- define_tgrid_end_scaling_factor()
  
  
  ## test results against default `tgrid` s4 object
  testthat::expect_identical(tg0@end, 24)
  testthat::expect_s4_class(adjust_time_grid_dosing_fn(dose_data, tg_input = tg0, scaling_factor = scl), "tgrid")
  testthat::expect_identical(slotNames(adjust_time_grid_dosing_fn(dose_data, tg_input = tg0, scaling_factor = scl)), c("start", "end", "delta", "add", "offset", "scale"))
  testthat::expect_length(slotNames(adjust_time_grid_dosing_fn(dose_data, tg_input = tg0, scaling_factor = scl)), 6L)
  testthat::expect_identical(adjust_time_grid_dosing_fn(dose_data, tg_input = tg0, scaling_factor = scl)@start,  0)
  testthat::expect_identical(adjust_time_grid_dosing_fn(dose_data, tg_input = tg0, scaling_factor = scl)@end,   242)
  testthat::expect_identical(adjust_time_grid_dosing_fn(dose_data, tg_input = mrgsolve::tgrid(delta=0.25), scaling_factor = scl)@end, 242.25)
  testthat::expect_identical(adjust_time_grid_dosing_fn(dose_data, tg_input = tg0, scaling_factor = scl)@delta,  1)
  testthat::expect_identical(adjust_time_grid_dosing_fn(dose_data, tg_input = tg0, scaling_factor = scl)@add,    numeric(0))
  testthat::expect_identical(adjust_time_grid_dosing_fn(dose_data, tg_input = tg0, scaling_factor = scl)@offset, 0)
  testthat::expect_identical(adjust_time_grid_dosing_fn(dose_data, tg_input = tg0, scaling_factor = scl)@scale,  1)
  
  
  ## use non-default `tgrid` end value
  testthat::expect_identical(adjust_time_grid_dosing_fn(dose_data, tg_input = tgrid(end = 300), scaling_factor = scl)@start,   0)
  testthat::expect_identical(adjust_time_grid_dosing_fn(dose_data, tg_input = tgrid(end = 50),  user_spec = TRUE,  scaling_factor = scl)@end, 50)
  testthat::expect_identical(adjust_time_grid_dosing_fn(dose_data, tg_input = tgrid(end = 300), user_spec = TRUE,  scaling_factor = scl)@end, 300)
  testthat::expect_identical(adjust_time_grid_dosing_fn(dose_data, tg_input = tgrid(end = 300), user_spec = FALSE, scaling_factor = scl)@end, 242)
  testthat::expect_identical(adjust_time_grid_dosing_fn(dose_data, tg_input = tgrid(end = 300), scaling_factor = scl)@delta,   1)
  testthat::expect_identical(adjust_time_grid_dosing_fn(dose_data, tg_input = tgrid(end = 300), scaling_factor = scl)@add,     numeric(0))
  testthat::expect_identical(adjust_time_grid_dosing_fn(dose_data, tg_input = tgrid(end = 300), scaling_factor = scl)@offset,  0)
  testthat::expect_identical(adjust_time_grid_dosing_fn(dose_data, tg_input = tgrid(end = 300), scaling_factor = scl)@scale,   1)
  testthat::expect_identical(adjust_time_grid_dosing_fn(dose_data, tg_input = tgrid(end = 75),  user_spec = TRUE,  scaling_factor = scl), tgrid(end = 75))
  testthat::expect_identical(adjust_time_grid_dosing_fn(dose_data, tg_input = tgrid(end = 300), user_spec = TRUE,  scaling_factor = scl), tgrid(end = 300))
  testthat::expect_identical(adjust_time_grid_dosing_fn(dose_data, tg_input = tgrid(end = 300), user_spec = FALSE, scaling_factor = scl), tgrid(end = 242))
  
  ## test scaling factor (independent from `scale` parameter in `tgrid` object)
  testthat::expect_identical(adjust_time_grid_dosing_fn(dose_data, tg_input = tgrid(end = 300), scaling_factor = 2.0)@end,   323)
  testthat::expect_identical(adjust_time_grid_dosing_fn(dose_data, tg_input = tgrid(end = 300), scaling_factor = 2.0),   tgrid(end = 323))
  
  
  ## use non-default `tgrid` all values
  tg_nodef <- mrgsolve::tgrid(start = 10, end = 300, delta = 1, add = 0, .offset = 5, .scale = 2)
  testthat::expect_identical(adjust_time_grid_dosing_fn(dose_data, tg_input = tg_nodef, scaling_factor = scl)@start,  10)
  testthat::expect_identical(adjust_time_grid_dosing_fn(dose_data, tg_input = tg_nodef, user_spec = TRUE,  scaling_factor = scl)@end, 300)
  testthat::expect_identical(adjust_time_grid_dosing_fn(dose_data, tg_input = tg_nodef, user_spec = FALSE, scaling_factor = scl)@end, 242)
  testthat::expect_identical(adjust_time_grid_dosing_fn(dose_data, tg_input = tg_nodef, scaling_factor = scl)@delta,   1)
  testthat::expect_identical(adjust_time_grid_dosing_fn(dose_data, tg_input = tg_nodef, scaling_factor = scl)@add,     0)
  testthat::expect_identical(adjust_time_grid_dosing_fn(dose_data, tg_input = tg_nodef, scaling_factor = scl)@offset,  5)
  testthat::expect_identical(adjust_time_grid_dosing_fn(dose_data, tg_input = tg_nodef, scaling_factor = scl)@scale,   2)
  testthat::expect_identical(adjust_time_grid_dosing_fn(dose_data, tg_input = tg_nodef, user_spec = TRUE,  scaling_factor = tg_nodef@scale), tg_nodef)
  testthat::expect_identical(adjust_time_grid_dosing_fn(dose_data, tg_input = tg_nodef, user_spec = FALSE, scaling_factor = tg_nodef@scale), 
                             mrgsolve::tgrid(start = 10, end = 323, delta = 1, add = 0, .offset = 5, .scale = 2))
})



## update_solver_settings_fn() -----------------------------------------------------------
testthat::test_that("update_solver_settings_fn", {
  ss_result <- fetch_solver_settings(use_custom = TRUE)
  ss_custom <- define_custom_solver_settings()
  ss_mrgmod <- define_mrgmod_solver_settings()
  ss_custom_test <- update_solver_settings_fn(atol = ss_custom$atol, rtol = ss_custom$rtol, ss_atol = ss_custom$ss_atol, ss_rtol = ss_custom$ss_rtol, maxsteps = ss_custom$maxsteps, hmin = ss_custom$hmin, hmax = ss_custom$hmax, ixpr = ss_custom$ixpr, mxhnil = ss_custom$mxhnil)
  ss_mrgmod_test <- update_solver_settings_fn(atol = ss_mrgmod$atol, rtol = ss_mrgmod$rtol, ss_atol = ss_mrgmod$ss_atol, ss_rtol = ss_mrgmod$ss_rtol, maxsteps = ss_mrgmod$maxsteps, hmin = ss_mrgmod$hmin, hmax = ss_mrgmod$hmax, ixpr = ss_mrgmod$ixpr, mxhnil = ss_mrgmod$mxhnil)
  
  ## test against custom solver settings
  testthat::expect_type(ss_custom_test, "list")
  testthat::expect_length(ss_custom_test, 9L)
  testthat::expect_named(ss_custom_test, c("atol", "rtol", "ss_atol", "ss_rtol", "maxsteps", "hmin", "hmax", "ixpr", "mxhnil"))
  testthat::expect_identical(ss_custom_test$atol, 1e-12)
  testthat::expect_identical(ss_custom_test$rtol, 1e-10)
  testthat::expect_identical(ss_custom_test$ss_atol, 1e-12)
  testthat::expect_identical(ss_custom_test$ss_rtol, 1e-10)
  testthat::expect_identical(ss_custom_test$maxsteps, 10000)
  testthat::expect_identical(ss_custom_test$hmin, 0)
  testthat::expect_identical(ss_custom_test$hmax, 0)
  testthat::expect_identical(ss_custom_test$ixpr, 0)
  testthat::expect_identical(ss_custom_test$mxhnil, 2)
  testthat::expect_identical(ss_custom_test, ss_custom[names(ss_custom_test)])
  testthat::expect_identical(ss_custom_test, ss_result[names(ss_custom_test)])
  testthat::expect_identical(ss_custom_test, list(atol = 1e-12, rtol = 1e-10, ss_atol = 1e-12, ss_rtol = 1e-10, 
                                                  maxsteps = 10000, hmin = 0, hmax = 0, ixpr = 0, mxhnil = 2))
  
  ## test against mrgmod default solver settings
  testthat::expect_type(ss_mrgmod_test, "list")
  testthat::expect_length(ss_mrgmod_test, 9L)
  testthat::expect_named(ss_mrgmod_test, c("atol", "rtol", "ss_atol", "ss_rtol", "maxsteps", "hmin", "hmax", "ixpr", "mxhnil"))
  testthat::expect_identical(ss_mrgmod_test$atol, 1e-8)
  testthat::expect_identical(ss_mrgmod_test$rtol, 1e-8)
  testthat::expect_identical(ss_mrgmod_test$ss_atol, 1e-8)
  testthat::expect_identical(ss_mrgmod_test$ss_rtol, 1e-8)
  testthat::expect_identical(ss_mrgmod_test$maxsteps, 20000)
  testthat::expect_identical(ss_mrgmod_test$hmin, 0)
  testthat::expect_identical(ss_mrgmod_test$hmax, 0)
  testthat::expect_identical(ss_mrgmod_test$ixpr, 0)
  testthat::expect_identical(ss_mrgmod_test$mxhnil, 2)
  testthat::expect_identical(ss_mrgmod_test, ss_mrgmod[names(ss_mrgmod_test)])
  testthat::expect_identical(ss_mrgmod_test, list(atol = 1e-08, rtol = 1e-08, ss_atol = 1e-08, ss_rtol = 1e-08, 
                                                  maxsteps = 20000, hmin = 0, hmax = 0, ixpr = 0, mxhnil = 2))
  
  ## test against solver settings with arbitrary values
  ss_test <- update_solver_settings_fn(atol = 1e-2, rtol = 1e-3, ss_atol = 1e-4, ss_rtol = 1e-5, maxsteps = 50000, hmin = 1e-20, hmax = 50, ixpr = 5, mxhnil = 1)
  testthat::expect_type(ss_test, "list")
  testthat::expect_length(ss_test, 9L)
  testthat::expect_named(ss_test, c("atol", "rtol", "ss_atol", "ss_rtol", "maxsteps", "hmin", "hmax", "ixpr", "mxhnil"))
  testthat::expect_identical(ss_test$atol, 1e-2)
  testthat::expect_identical(ss_test$rtol, 1e-3)
  testthat::expect_identical(ss_test$ss_atol, 1e-4)
  testthat::expect_identical(ss_test$ss_rtol, 1e-5)
  testthat::expect_identical(ss_test$maxsteps, 50000)
  testthat::expect_identical(ss_test$hmin, 1e-20)
  testthat::expect_identical(ss_test$hmax, 50)
  testthat::expect_identical(ss_test$ixpr, 5)
  testthat::expect_identical(ss_test$mxhnil, 1)
  testthat::expect_identical(ss_test, list(atol = 0.01, rtol = 0.001, ss_atol = 1e-04, ss_rtol = 1e-05, 
                                           maxsteps = 50000, hmin = 1e-20, hmax = 50, ixpr = 5, mxhnil = 1))
})



## update_model_solver_settings_fn() -----------------------------------------------------
testthat::test_that("update_model_solver_settings_fn", {
  def_mod <- app_libmod_objs[[fetch_default_libmod_name()]]
  ss_def_mod <- list_S4_slots(def_mod)[define_solver_settings_params()]
  ss_custom <- define_custom_solver_settings()
  ss_mrgmod <- define_mrgmod_solver_settings()
  def_mod_ss_custom <- update_model_solver_settings_fn(mod = def_mod, solver_settings = ss_custom)
  def_mod_ss_mrgmod <- update_model_solver_settings_fn(mod = def_mod, solver_settings = ss_mrgmod)
  ss_def_mod_ss_custom <- list_S4_slots(def_mod_ss_custom)[define_solver_settings_params()]
  ss_def_mod_ss_mrgmod <- list_S4_slots(def_mod_ss_mrgmod)[define_solver_settings_params()]

  ## test against default library model with default `mrgmod` solver settings
  testthat::expect_s4_class(def_mod, "mrgmod")
  testthat::expect_s4_class(def_mod_ss_mrgmod, "mrgmod")
  testthat::expect_identical(ss_mrgmod, list(rtol = 1e-08, atol = 1e-08, ss_rtol = 1e-08, ss_atol = 1e-08, 
                                             maxsteps = 20000, hmin = 0, hmax = 0, ixpr = 0, mxhnil = 2))
  testthat::expect_identical(ss_def_mod_ss_mrgmod, ss_mrgmod)
  testthat::expect_identical(def_mod, def_mod_ss_mrgmod)
  
  
  ## test against default library model with custom solver settings
  testthat::expect_s4_class(def_mod_ss_custom, "mrgmod")
  testthat::expect_identical(ss_custom, list(rtol = 1e-10, atol = 1e-12, ss_rtol = 1e-10, ss_atol = 1e-12, 
                                             maxsteps = 10000, hmin = 0, hmax = 0, ixpr = 0, mxhnil = 2))
  testthat::expect_identical(ss_def_mod_ss_custom, ss_custom)
  testthat::expect_false(identical(def_mod, def_mod_ss_custom))
  
  ## test inconversion between default & custom solver settings
  testthat::expect_identical(update_model_solver_settings_fn(def_mod_ss_custom, ss_mrgmod), def_mod)
  testthat::expect_identical(update_model_solver_settings_fn(def_mod_ss_mrgmod, ss_custom), def_mod_ss_custom)
})



## fixed_params_modal_uiout_fn() ---------------------------------------------------------
testthat::test_that("fixed_params_modal_uiout_fn", {
  
  testthat::local_edition(3)
  
  ## test a NULL model input
  testthat::expect_type(fixed_params_modal_uiout_fn(mod = NULL), "NULL")
  testthat::expect_length(fixed_params_modal_uiout_fn(mod = NULL), 0)
  testthat::expect_identical(fixed_params_modal_uiout_fn(mod = NULL), NULL)
  
  ## default library model
  def_mod <- app_libmod_objs[[fetch_default_libmod_name()]]
  testthat::expect_type(fixed_params_modal_uiout_fn(mod = def_mod), "list")
  testthat::expect_length(fixed_params_modal_uiout_fn(mod = def_mod), 3)
  testthat::expect_named(fixed_params_modal_uiout_fn(mod = def_mod), c("name", "attribs", "children"))
  testthat::expect_snapshot(build_fixed_param_modal_window(mod = def_mod), cran = FALSE)
})



## build_re_matrix_table_fn() ------------------------------------------------------------
testthat::test_that("build_re_matrix_table_fn", {
  
  testthat::local_edition(3)
  
  ## test the default library model
  def_mod <- app_libmod_objs[[fetch_default_libmod_name()]]
  def_mod_re <- label_model_re_matrices(def_mod)
  
  ## test NULL and empty matrix inputs
  testthat::expect_identical(build_re_matrix_table_fn(re_matrix = NULL), NULL)
  testthat::expect_identical(build_re_matrix_table_fn(re_matrix = matrix(1, nrow = 0, ncol = 0)), NULL)
  
  ## test default library model OMEGA matrix
  def_mod_omega_table <- build_re_matrix_table_fn(re_matrix = def_mod_re$omega)
  testthat::expect_type(def_mod_omega_table, "list")
  testthat::expect_snapshot(htmlwidgets:::toJSON(def_mod_omega_table), cran = FALSE)
  
  ## test default library model SIGMA matrix
  def_mod_sigma_table <- build_re_matrix_table_fn(re_matrix = def_mod_re$sigma)
  testthat::expect_type(def_mod_sigma_table, "list")
  testthat::expect_snapshot(htmlwidgets:::toJSON(def_mod_sigma_table), cran = FALSE)
  
  ## test a labeled matrix containing both the min & max values [-1, 1]
  test_matrix <- matrix(c(0.116, 0.0812, -1, 0.0812, 0.116, 1, -1, 1, 0.04), 
                        nrow = 3, ncol = 3, dimnames = list(c("ETA_CL", "ETA_VC", "ETA_R0"), c("ETA_CL", "ETA_VC", "ETA_R0")))
  test_matrix_table <- build_re_matrix_table_fn(test_matrix)
  testthat::expect_snapshot(htmlwidgets:::toJSON(test_matrix_table), cran = FALSE)
})



## update_re_matrix_hot_fn() -------------------------------------------------------------
testthat::test_that("update_re_matrix_hot_fn", {
  
  ## test default library model OMEGA matrix
  
  
  ## test default library model SIGMA matrix
  
  
  ## test a labeled matrix containing both the min & max values [-1, 1]
  
  
  
  ## test NULL & empty matrix inputs inputs
  testthat::expect_identical(update_re_matrix_hot_fn(re_matrix_hot = NULL), NULL)
  testthat::expect_error(update_re_matrix_hot_fn(re_matrix_hot = matrix(0, 0, 0)))
  
  ## read-in exported shiny app user input data structure for testing
  test_input <- readRDS(here::here("tests/testthat/shiny-app_input-data-struct_20220501-01.RDS"))
  test_input_omega_hot <- test_input$omega_matrix_hot
  testthat::expect_identical(update_re_matrix_hot_fn(test_input_omega_hot), 
                             matrix(c(0.304, 0, 0, 0, 0, 0, 0, 0, 0, 0.0265, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                      0, 0, 0, 0, 0.223, 0, 0, 0, 0, 0, 0, 0, 0, 0.489, 0, 0, 0, 0, 0, 0, 0, 0, 0.119, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                                    nrow = 8, ncol = 8, dimnames = list(c("ETA_CL", "ETA_V2", "ETA_Q", "ETA_V3", "ETA_KA", "ETA_F1", "ETA_VMAX", "ETA_KM"), c("ETA_CL", "ETA_V2", "ETA_Q", "ETA_V3", "ETA_KA", "ETA_F1", "ETA_VMAX", "ETA_KM")))
  )
})



## validate_re_matrices_fn() -------------------------------------------------------------
testthat::test_that("validate_re_matrices_fn", {
  
  ## set option for printing matrix validation test results during unit testing
  print_re_results <- FALSE
  
  ## define some test matrices
  empty_matrix <- matrix(0, nrow = 0, ncol = 0)
  zero_1x1_mat <- matrix(0, nrow = 1, ncol = 1)
  neg_diag_mat <- matrix(c(-0.1, 0, 0, -1), nrow = 2, ncol = 2)
  invalid_cor_pos <- matrix(c(0.1, 1.5, 1.5, 1), nrow = 2, ncol = 2)
  invalid_cor_neg <- matrix(c(0.1, -1.5, -1.5, 1), nrow = 2, ncol = 2)
  
  
  ## test empty matrices
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = empty_matrix, re_sigma = empty_matrix, re_selected = "None", print_results = print_re_results), 
    list(res = NULL, msg = "Matrices not specified!"))
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = empty_matrix, re_sigma = empty_matrix, re_selected = "Interindividual Variability", print_results = print_re_results), 
    list(res = NULL, msg = "Matrices not specified!"))
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = empty_matrix, re_sigma = empty_matrix, re_selected = "Residual Variability", print_results = print_re_results), 
    list(res = NULL, msg = "Matrices not specified!"))
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = empty_matrix, re_sigma = empty_matrix, re_selected = "Both", print_results = print_re_results), 
    list(res = NULL, msg = "Matrices not specified!"))
  
  ## test error for non-empty matrices with `re_selected` = "None"
  testthat::expect_error(validate_re_matrices_fn(re_omega = zero_1x1_mat, re_sigma = zero_1x1_mat, re_selected = "None", print_results = print_re_results))
  
  ## test valid results for all other options of `re_selected`
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = zero_1x1_mat, re_sigma = zero_1x1_mat, re_selected = "Interindividual Variability", print_results = print_re_results), 
    list(res = TRUE, msg = "Valid OMEGA matrix!"))
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = zero_1x1_mat, re_sigma = zero_1x1_mat, re_selected = "Residual Variability", print_results = print_re_results), 
    list(res = TRUE, msg = "Valid SIGMA matrix!"))
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = zero_1x1_mat, re_sigma = zero_1x1_mat, re_selected = "Both", print_results = print_re_results), 
    list(res = TRUE, msg = "Valid random effect matrices!"))
  
  ## test valid results using empty matrix as input for selected random effect matrix
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = empty_matrix, re_sigma = zero_1x1_mat, re_selected = "Interindividual Variability", print_results = print_re_results), 
    list(res = TRUE, msg = "Valid OMEGA matrix!"))
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = zero_1x1_mat, re_sigma = empty_matrix, re_selected = "Residual Variability", print_results = print_re_results), 
    list(res = TRUE, msg = "Valid SIGMA matrix!"))
  
  
  ## test matrix with negative diagonal elements
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = neg_diag_mat, re_sigma = zero_1x1_mat, re_selected = "Interindividual Variability", print_results = print_re_results), 
    list(res = FALSE, msg = "Invalid OMEGA matrix!!!"))
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = zero_1x1_mat, re_sigma = neg_diag_mat, re_selected = "Interindividual Variability", print_results = print_re_results), 
    list(res = TRUE, msg = "Valid OMEGA matrix!"))
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = zero_1x1_mat, re_sigma = neg_diag_mat, re_selected = "Residual Variability", print_results = print_re_results), 
    list(res = FALSE, msg = "Invalid SIGMA matrix!!!"))
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = neg_diag_mat, re_sigma = zero_1x1_mat, re_selected = "Residual Variability", print_results = print_re_results), 
    list(res = TRUE, msg = "Valid SIGMA matrix!"))
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = zero_1x1_mat, re_sigma = neg_diag_mat, re_selected = "Both", print_results = print_re_results), 
    list(res = FALSE, msg = "Invalid SIGMA matrix!!!"))
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = neg_diag_mat, re_sigma = zero_1x1_mat, re_selected = "Both", print_results = print_re_results), 
    list(res = FALSE, msg = "Invalid OMEGA matrix!!!"))
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = neg_diag_mat, re_sigma = neg_diag_mat, re_selected = "Both", print_results = print_re_results), 
    list(res = FALSE, msg = "Invalid random effect matrices!!!"))
  
  
  ## test invalid correlation matrix with positive value outside the range [-1, 1]
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = invalid_cor_pos, re_sigma = zero_1x1_mat, re_selected = "Interindividual Variability", print_results = print_re_results), 
    list(res = FALSE, msg = "Invalid OMEGA matrix!!!"))
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = zero_1x1_mat, re_sigma = invalid_cor_pos, re_selected = "Interindividual Variability", print_results = print_re_results), 
    list(res = TRUE, msg = "Valid OMEGA matrix!"))
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = zero_1x1_mat, re_sigma = invalid_cor_pos, re_selected = "Residual Variability", print_results = print_re_results), 
    list(res = FALSE, msg = "Invalid SIGMA matrix!!!"))
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = invalid_cor_pos, re_sigma = zero_1x1_mat, re_selected = "Residual Variability", print_results = print_re_results), 
    list(res = TRUE, msg = "Valid SIGMA matrix!"))
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = zero_1x1_mat, re_sigma = invalid_cor_pos, re_selected = "Both", print_results = print_re_results), 
    list(res = FALSE, msg = "Invalid SIGMA matrix!!!"))
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = invalid_cor_pos, re_sigma = zero_1x1_mat, re_selected = "Both", print_results = print_re_results), 
    list(res = FALSE, msg = "Invalid OMEGA matrix!!!"))
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = invalid_cor_pos, re_sigma = invalid_cor_pos, re_selected = "Both", print_results = print_re_results), 
    list(res = FALSE, msg = "Invalid random effect matrices!!!"))
  
  
  ## test invalid correlation matrix with negative value outside the range [-1, 1]
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = invalid_cor_neg, re_sigma = zero_1x1_mat, re_selected = "Interindividual Variability", print_results = print_re_results), 
    list(res = FALSE, msg = "Invalid OMEGA matrix!!!"))
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = zero_1x1_mat, re_sigma = invalid_cor_neg, re_selected = "Interindividual Variability", print_results = print_re_results), 
    list(res = TRUE, msg = "Valid OMEGA matrix!"))
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = zero_1x1_mat, re_sigma = invalid_cor_neg, re_selected = "Residual Variability", print_results = print_re_results), 
    list(res = FALSE, msg = "Invalid SIGMA matrix!!!"))
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = invalid_cor_neg, re_sigma = zero_1x1_mat, re_selected = "Residual Variability", print_results = print_re_results), 
    list(res = TRUE, msg = "Valid SIGMA matrix!"))
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = zero_1x1_mat, re_sigma = invalid_cor_neg, re_selected = "Both", print_results = print_re_results), 
    list(res = FALSE, msg = "Invalid SIGMA matrix!!!"))
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = invalid_cor_neg, re_sigma = zero_1x1_mat, re_selected = "Both", print_results = print_re_results), 
    list(res = FALSE, msg = "Invalid OMEGA matrix!!!"))
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = invalid_cor_neg, re_sigma = invalid_cor_neg, re_selected = "Both", print_results = print_re_results), 
    list(res = FALSE, msg = "Invalid random effect matrices!!!"))
  
  
  ## test using matrix edge cases
  edge_case_01 <- matrix(1, nrow = 8, ncol = 8)
  diag(edge_case_01) <- 0.75
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = edge_case_01, re_sigma = edge_case_01, re_selected = "Interindividual Variability", print_results = print_re_results), 
    list(res = FALSE, msg = "Invalid OMEGA matrix!!!"))
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = edge_case_01, re_sigma = edge_case_01, re_selected = "Residual Variability", print_results = print_re_results), 
    list(res = FALSE, msg = "Invalid SIGMA matrix!!!"))
  testthat::expect_identical(
    validate_re_matrices_fn(re_omega = edge_case_01, re_sigma = edge_case_01, re_selected = "Both", print_results = print_re_results), 
    list(res = FALSE, msg = "Invalid random effect matrices!!!"))
  
  
  ## test all library model matrices
  libmod_re_matrices <- purrr::map(app_libmod_objs, label_model_re_matrices)
  valid_libmod_re <- purrr::map(app_libmod_names, ~ validate_re_matrices_fn(re_omega = libmod_re_matrices[[.x]]$omega, re_sigma = libmod_re_matrices[[.x]]$sigma, re_selected = "Both", print_results = print_re_results))
  reslt_libmod_re <- purrr::map(app_libmod_names %>% purrr::set_names(), ~ list(res = TRUE, msg = "Valid random effect matrices!"))
  testthat::expect_identical(valid_libmod_re, reslt_libmod_re)
})



## check_re_matrices_fn() ----------------------------------------------------------------




## build_mod_param_info_fn() -------------------------------------------------------------
testthat::test_that("build_mod_param_info_fn", {
  def_mod <- app_libmod_objs[[fetch_default_libmod_name()]]
  model_params <- extract_model_params(mod = def_mod)
  param_values <- unlist(model_params)
  param_annot <- extract_model_param_annot_data(def_mod)
  param_units <- setNames(param_annot$unit,  param_annot$name)
  param_descr <- setNames(param_annot$descr, param_annot$name)
  param_value_ids <- build_model_param_value_ids(mod = def_mod)
  param_units_ids <- build_model_param_units_ids(mod = def_mod)
  
  test_input_values <- purrr::map(param_value_ids %>% purrr::set_names(.), ~ param_values[[gsub("(?i)_value", "", .x)]])
  test_input_units  <- purrr::map(param_units_ids %>% purrr::set_names(.), ~ param_units[[gsub("(?i)_units", "", .x)]])
  test_input_struct <- c(test_input_values, test_input_units)
  
  input_as_numeric_mock <- function(x) {
    as.numeric(as.character(x))
  }
  
  mockr::local_mock(input_as_numeric = input_as_numeric_mock, .env = topenv())
  
  
  ## test `build_mod_param_info_fn` <backend> function  -->  build_mod_param_info_fn(mod, input)
  def_mod_param_info_result <- tibble::as_tibble(param_annot) %>% 
    select(param = name, value, units = unit, descr) %>% 
    mutate(units = setNames(units, param), 
           descr = setNames(descr, param))
  
  testthat::expect_type(build_mod_param_info_fn(mod = def_mod, input = test_input_struct), "list")
  testthat::expect_named(build_mod_param_info_fn(mod = def_mod, input = test_input_struct), c("param", "value", "units", "descr"))
  testthat::expect_length(build_mod_param_info_fn(mod = def_mod, input = test_input_struct), 4L)
  testthat::expect_identical(build_mod_param_info_fn(mod = def_mod, input = test_input_struct), def_mod_param_info_result)
  
  
  ## wrap unit test logic into function & test against all library models
  test_library_model_param_info <- function(cur_mod_name) {
    cur_mod <- app_libmod_objs[[cur_mod_name]]
    model_params <- extract_model_params(mod = cur_mod)
    param_values <- unlist(model_params)
    param_annot <- extract_model_param_annot_data(cur_mod)
    param_units <- setNames(param_annot$unit,  param_annot$name)
    param_descr <- setNames(param_annot$descr, param_annot$name)
    param_value_ids <- build_model_param_value_ids(mod = cur_mod)
    param_units_ids <- build_model_param_units_ids(mod = cur_mod)
    
    test_input_values <- purrr::map(param_value_ids %>% purrr::set_names(.), ~ param_values[[gsub("(?i)_value", "", .x)]])
    test_input_units  <- purrr::map(param_units_ids %>% purrr::set_names(.), ~ param_units[[gsub("(?i)_units", "", .x)]])
    test_input_struct <- c(test_input_values, test_input_units)
    
    ## test `build_mod_param_info_fn` <backend> function  -->  build_mod_param_info_fn(mod, input)
    cur_mod_param_info_result <- tibble::as_tibble(param_annot) %>% 
      select(param = name, value, units = unit, descr) %>% 
      mutate(units = setNames(units, param), 
             descr = setNames(descr, param))
    
    testthat::expect_type(build_mod_param_info_fn(mod = cur_mod, input = test_input_struct), "list")
    testthat::expect_named(build_mod_param_info_fn(mod = cur_mod, input = test_input_struct), c("param", "value", "units", "descr"))
    testthat::expect_length(build_mod_param_info_fn(mod = cur_mod, input = test_input_struct), 4L)
    testthat::expect_identical(build_mod_param_info_fn(mod = cur_mod, input = test_input_struct), cur_mod_param_info_result)
  }
  ## test against all shiny app libray models
  purrr::walk(app_libmod_names, test_library_model_param_info)
})



## extract_model_param_value() -----------------------------------------------------------
testthat::test_that("extract_model_param_value", {
  def_mod <- app_libmod_objs[[fetch_default_libmod_name()]]
  model_params <- extract_model_params(mod = def_mod)
  param_values <- unlist(model_params)
  param_annot <- extract_model_param_annot_data(def_mod)
  param_units <- setNames(param_annot$unit,  param_annot$name)
  param_descr <- setNames(param_annot$descr, param_annot$name)
  param_value_ids <- build_model_param_value_ids(mod = def_mod)
  param_units_ids <- build_model_param_units_ids(mod = def_mod)

  test_input_values <- purrr::map(param_value_ids %>% purrr::set_names(.), ~ param_values[[gsub("(?i)_value", "", .x)]])
  test_input_units  <- purrr::map(param_units_ids %>% purrr::set_names(.), ~ param_units[[gsub("(?i)_units", "", .x)]])
  
  input_as_numeric_mock <- function(x) {
    as.numeric(as.character(x))
  }
  
  mockr::local_mock(input_as_numeric = input_as_numeric_mock, .env = topenv())

  
  ## test `extract_model_param_value` <backend> function  -->  extract_model_param_value(mod, input, param_names = NULL)
  
  ## test NULL `input` data structure
  testthat::expect_identical(extract_model_param_value(mod = def_mod, input = NULL), numeric(0))
  testthat::expect_type(extract_model_param_value(mod = def_mod, input = NULL), "double")
  
  ## test against default library model `input` data structure build for unit testing
  testthat::expect_type(extract_model_param_value(mod = def_mod, input = test_input_values), "double")
  testthat::expect_length(extract_model_param_value(mod = def_mod, input = test_input_values), length(param_values))
  testthat::expect_named(extract_model_param_value(mod = def_mod, input = test_input_values), names(param_values))
  testthat::expect_identical(extract_model_param_value(mod = def_mod, input = test_input_values), param_values)
  testthat::expect_identical(extract_model_param_value(mod = def_mod, input = test_input_values, param_names = names(param_values)), param_values)
  testthat::expect_identical(extract_model_param_value(mod = def_mod, input = test_input_values), extract_model_param_value(mod = def_mod, input = test_input_values, param_names = names(param_values)))
})



## extract_model_param_units() -----------------------------------------------------------
testthat::test_that("extract_model_param_units", {
  def_mod <- app_libmod_objs[[fetch_default_libmod_name()]]
  model_params <- extract_model_params(mod = def_mod)
  param_values <- unlist(model_params)
  param_annot <- extract_model_param_annot_data(def_mod)
  param_units <- setNames(param_annot$unit,  param_annot$name)
  param_descr <- setNames(param_annot$descr, param_annot$name)
  param_units_ids <- build_model_param_units_ids(mod = def_mod)
  test_input_units  <- purrr::map(param_units_ids %>% purrr::set_names(.), ~ param_units[[gsub("(?i)_units", "", .x)]])
  
  
  ## test `extract_model_param_units` <backend> function  -->  extract_model_param_units(mod, input, param_names = NULL)
  
  ## test NULL `input` data structure
  testthat::expect_identical(extract_model_param_units(mod = def_mod, input = NULL), NULL)
  testthat::expect_type(extract_model_param_units(mod = def_mod, input = NULL), "NULL")
  
  ## test against default library model `input` data structure build for unit testing
  testthat::expect_type(extract_model_param_units(mod = def_mod, input = test_input_units), "character")
  testthat::expect_length(extract_model_param_units(mod = def_mod, input = test_input_units), length(param_units))
  testthat::expect_named(extract_model_param_units(mod = def_mod, input = test_input_units), names(param_units))
  testthat::expect_identical(extract_model_param_units(mod = def_mod, input = test_input_units), param_units)
  testthat::expect_identical(extract_model_param_units(mod = def_mod, input = test_input_units, param_names = names(param_units)), param_units)
  testthat::expect_identical(extract_model_param_units(mod = def_mod, input = test_input_units), extract_model_param_units(mod = def_mod, input = test_input_units, param_names = names(param_units)))
})



## extract_model_param_descr() -----------------------------------------------------------
testthat::test_that("extract_model_param_descr", {
  def_mod <- app_libmod_objs[[fetch_default_libmod_name()]]
  model_params <- extract_model_params(mod = def_mod)
  param_values <- unlist(model_params)
  param_annot <- extract_model_param_annot_data(def_mod)
  param_descr <- setNames(param_annot$descr, param_annot$name)
  
  ## test `extract_model_param_descr` <backend> function  -->  extract_model_param_descr(mod, input, param_names = NULL)
  
  ## test against default library model `input` data structure build for unit testing
  testthat::expect_type(extract_model_param_descr(mod = def_mod), "character")
  testthat::expect_length(extract_model_param_descr(mod = def_mod), length(param_descr))
  testthat::expect_named(extract_model_param_descr(mod = def_mod), names(param_descr))
  testthat::expect_identical(extract_model_param_descr(mod = def_mod), param_descr)
  testthat::expect_identical(extract_model_param_descr(mod = def_mod, param_names = names(param_descr)), param_descr)
  testthat::expect_identical(extract_model_param_descr(mod = def_mod), extract_model_param_descr(mod = def_mod, param_names = names(param_descr)))
})



## update_model_fn() ---------------------------------------------------------------------
testthat::test_that("update_model_fn", {
  
  input_as_numeric_mock <- function(x) {
    as.numeric(as.character(x))
  }
  
  mockr::local_mock(input_as_numeric = input_as_numeric_mock, .env = topenv())
  
  
  ## define local helper for constructing the user-input data structure
  build_libmod_user_input_data_structure <- function(mod_name, mod_obj = NULL) {
    cur_mod <- mod_obj %||% app_libmod_objs[[mod_name]]
    model_params <- extract_model_params(mod = cur_mod)
    param_values <- unlist(model_params)
    param_annot <- extract_model_param_annot_data(cur_mod)
    param_units <- setNames(param_annot$unit,  param_annot$name)
    param_descr <- setNames(param_annot$descr, param_annot$name)
    param_value_ids <- build_model_param_value_ids(mod = cur_mod)
    param_units_ids <- build_model_param_units_ids(mod = cur_mod)
    test_input_values <- purrr::map(param_value_ids %>% purrr::set_names(.), ~ param_values[[gsub("(?i)_value", "", .x)]])
    test_input_units  <- purrr::map(param_units_ids %>% purrr::set_names(.), ~ param_units[[gsub("(?i)_units", "", .x)]])
    test_input_struct <- c(test_input_values, test_input_units)
    test_input_struct
  }
  
  strip_mod_solver_settings <- function(mod) {
    list_S4_slots(mod)[define_solver_settings_params()]
  }
  
  ## unit tests:  `update_model_fn(mod, param_info, solver_settings, omega_mat, sigma_mat, re_selected)`
  
  ## test the default library model
  def_mod <- app_libmod_objs[[fetch_default_libmod_name()]]
  def_mod_input <- build_libmod_user_input_data_structure(fetch_default_libmod_name())
  def_mod_info <- build_mod_param_info_fn(def_mod, def_mod_input)
  def_mod_re <- label_model_re_matrices(def_mod)
  def_mod_solver <- strip_mod_solver_settings(def_mod)
  solver_settings <- fetch_solver_settings()
  
  ## Random Effects Selected == "None"
  def_modup_no <- update_model_fn(def_mod, def_mod_info, solver_settings, def_mod_re$omega, def_mod_re$sigma, re_selected = "None")
  def_modup_no_re <- label_model_re_matrices(def_modup_no)
  testthat::expect_error(update_model_fn(def_mod, def_mod_info, solver_settings, def_mod_re$omega, def_mod_re$sigma, re_selected = NA_character_), regexp = "Invalid selection of random effects!")
  testthat::expect_false(identical(strip_mod_solver_settings(def_modup_no), strip_mod_solver_settings(def_mod)))
  testthat::expect_identical(strip_mod_solver_settings(def_modup_no), solver_settings)
  testthat::expect_identical(dim(def_modup_no_re$omega), dim(def_mod_re$omega))
  testthat::expect_identical(dim(def_modup_no_re$sigma), dim(def_mod_re$sigma))
  testthat::expect_identical(dimnames(def_modup_no_re$omega), dimnames(def_mod_re$omega))
  testthat::expect_identical(dimnames(def_modup_no_re$sigma), dimnames(def_mod_re$sigma))
  testthat::expect_identical(def_modup_no_re$omega, matrix(0, nrow = nrow(def_mod_re$omega), ncol = ncol(def_mod_re$omega), dimnames = dimnames(def_mod_re$omega)))
  testthat::expect_identical(def_modup_no_re$sigma, matrix(0, nrow = nrow(def_mod_re$sigma), ncol = ncol(def_mod_re$sigma), dimnames = dimnames(def_mod_re$sigma)))
  
  
  ## Random Effects Selected == "Interindividual Variability"
  def_modup_om <- update_model_fn(def_mod, def_mod_info, solver_settings, def_mod_re$omega, def_mod_re$sigma, re_selected = "Interindividual Variability")
  def_modup_om_re <- label_model_re_matrices(def_modup_om)
  testthat::expect_false(identical(strip_mod_solver_settings(def_modup_om), strip_mod_solver_settings(def_mod)))
  testthat::expect_identical(strip_mod_solver_settings(def_modup_om), solver_settings)
  testthat::expect_identical(dim(def_modup_om_re$omega), dim(def_mod_re$omega))
  testthat::expect_identical(dim(def_modup_om_re$sigma), dim(def_mod_re$sigma))
  testthat::expect_identical(dimnames(def_modup_om_re$omega), dimnames(def_mod_re$omega))
  testthat::expect_identical(dimnames(def_modup_om_re$sigma), dimnames(def_mod_re$sigma))
  testthat::expect_identical(def_modup_om_re$omega, def_mod_re$omega)
  testthat::expect_identical(def_modup_om_re$sigma, matrix(0, nrow = nrow(def_mod_re$sigma), ncol = ncol(def_mod_re$sigma), dimnames = dimnames(def_mod_re$sigma)))
  
  
  ## Random Effects Selected == "Residual Variability"
  def_modup_sg <- update_model_fn(def_mod, def_mod_info, solver_settings, def_mod_re$omega, def_mod_re$sigma, re_selected = "Residual Variability")
  def_modup_sg_re <- label_model_re_matrices(def_modup_sg)
  testthat::expect_false(identical(strip_mod_solver_settings(def_modup_sg), strip_mod_solver_settings(def_mod)))
  testthat::expect_identical(strip_mod_solver_settings(def_modup_sg), solver_settings)
  testthat::expect_identical(dim(def_modup_sg_re$omega), dim(def_mod_re$omega))
  testthat::expect_identical(dim(def_modup_sg_re$sigma), dim(def_mod_re$sigma))
  testthat::expect_identical(dimnames(def_modup_sg_re$omega), dimnames(def_mod_re$omega))
  testthat::expect_identical(dimnames(def_modup_sg_re$sigma), dimnames(def_mod_re$sigma))
  testthat::expect_identical(def_modup_sg_re$omega, matrix(0, nrow = nrow(def_mod_re$omega), ncol = ncol(def_mod_re$omega), dimnames = dimnames(def_mod_re$omega)))
  testthat::expect_identical(def_modup_sg_re$sigma, def_mod_re$sigma)
  
  ## Random Effects Selected == "Both"
  def_modup_bt <- update_model_fn(def_mod, def_mod_info, solver_settings, def_mod_re$omega, def_mod_re$sigma, re_selected = "Both")
  def_modup_bt_re <- label_model_re_matrices(def_modup_bt)
  testthat::expect_false(identical(strip_mod_solver_settings(def_modup_bt), strip_mod_solver_settings(def_mod)))
  testthat::expect_identical(strip_mod_solver_settings(def_modup_bt), solver_settings)
  testthat::expect_identical(dim(def_modup_bt_re$omega), dim(def_mod_re$omega))
  testthat::expect_identical(dim(def_modup_bt_re$sigma), dim(def_mod_re$sigma))
  testthat::expect_identical(dimnames(def_modup_bt_re$omega), dimnames(def_mod_re$omega))
  testthat::expect_identical(dimnames(def_modup_bt_re$sigma), dimnames(def_mod_re$sigma))
  testthat::expect_identical(def_modup_bt_re$omega, def_mod_re$omega)
  testthat::expect_identical(def_modup_bt_re$sigma, def_mod_re$sigma)
  
  ## Update Values in Random Effect matrices with RE Selected == "Both"
  def_mod_re_ones <- list(  ## pass in matrices of all ones
    omega = matrix(1, nrow = nrow(def_mod_re$omega), ncol = ncol(def_mod_re$omega), dimnames = dimnames(def_mod_re$omega)),
    sigma = matrix(1, nrow = nrow(def_mod_re$sigma), ncol = ncol(def_mod_re$sigma), dimnames = dimnames(def_mod_re$sigma)))
  def_modup_bt_ones <- update_model_fn(def_mod, def_mod_info, solver_settings, def_mod_re_ones$omega, def_mod_re_ones$sigma, re_selected = "Both")
  def_modup_bt_ones_re <- label_model_re_matrices(def_modup_bt_ones)
  testthat::expect_false(identical(strip_mod_solver_settings(def_modup_bt_ones), strip_mod_solver_settings(def_mod)))
  testthat::expect_identical(strip_mod_solver_settings(def_modup_bt_ones), solver_settings)
  testthat::expect_identical(dim(def_modup_bt_ones_re$omega), dim(def_mod_re$omega))
  testthat::expect_identical(dim(def_modup_bt_ones_re$sigma), dim(def_mod_re$sigma))
  testthat::expect_identical(dimnames(def_modup_bt_ones_re$omega), dimnames(def_mod_re$omega))
  testthat::expect_identical(dimnames(def_modup_bt_ones_re$sigma), dimnames(def_mod_re$sigma))
  testthat::expect_identical(def_modup_bt_ones_re$omega, def_mod_re_ones$omega)
  testthat::expect_identical(def_modup_bt_ones_re$sigma, def_mod_re_ones$sigma)
  
  
  ## test updates to model parameter info
  def_mod <- app_libmod_objs[[fetch_default_libmod_name()]]
  def_mod_input <- build_libmod_user_input_data_structure(fetch_default_libmod_name())
  def_mod_info <- build_mod_param_info_fn(def_mod, def_mod_input)
  
  ## modified model param info input data structure
  def_mod_info_update <- def_mod_info %>% 
    mutate(value = dplyr::if_else(param %in% "TVCL", 1.25, value),
           value = setNames(value, param)) %>%  
    mutate(units = dplyr::if_else(param %in% "TVCL", "volume/time", units), 
           units = setNames(units, param)) %>% 
    mutate(descr = dplyr::if_else(param %in% "TVCL", "NOT UPDATED IN MODEL", descr), 
           descr = setNames(descr, param)) 
  
  ## modified model param info expected output data structure (NOTE: model description cannot be modified via shiny app & isn't update in model object)
  def_mod_info_result <- def_mod_info %>% 
    mutate(value = dplyr::if_else(param %in% "TVCL", 1.25, value),
           value = setNames(value, param)) %>%  
    mutate(units = dplyr::if_else(param %in% "TVCL", "volume/time", units), 
           units = setNames(units, param))
  
  def_modup_params <- update_model_fn(def_mod, def_mod_info_update, solver_settings, def_mod_re$omega, def_mod_re$sigma, re_selected = "None")
  def_modup_params_input <- build_libmod_user_input_data_structure(mod_obj = def_modup_params, mod_name = NULL)
  def_modup_params_info <- build_mod_param_info_fn(def_modup_params, def_modup_params_input)
  def_modup_params_data <- extract_model_annotations_data(def_modup_params)
  def_modup_params_info_derived <- tibble::as_tibble(def_modup_params_data) %>% 
    filter(block %in% "PARAM") %>% 
    select(-block, param = name, value, units = unit, descr, -options) %>% 
    mutate(value = setNames(value, param), units = setNames(units, param), descr = setNames(descr, param))
  def_modup_params_TVCL <- def_modup_params_data %>% filter(name %in% "TVCL")
  testthat::expect_identical(extract_model_params(def_modup_params)$TVCL, 1.25)
  testthat::expect_identical(unname(def_modup_params_TVCL$value), 1.25)
  testthat::expect_identical(unname(def_modup_params_TVCL$unit), "volume/time")
  testthat::expect_identical(unname(def_modup_params_TVCL$descr), "Clearance")
  testthat::expect_identical(def_modup_params_info, def_mod_info_result)
  testthat::expect_identical(def_modup_params_info_derived, def_mod_info_result)
  
  ## test NULL model input 
  testthat::expect_identical(update_model_fn(mod = NULL, param_info = NULL, solver_settings = NULL, omega_mat = NULL, sigma_mat = NULL, re_selected = NULL), NULL)
})



## dt_sim_template_data_fn() -------------------------------------------------------------







})



