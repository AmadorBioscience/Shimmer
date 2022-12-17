## print log debugging statements
print("test-helpers.R")
print(getwd())


## docs for `testthat::source_*` functions:  https://testthat.r-lib.org/reference/source_file.html
here::i_am("tests/testthat/test-helpers.R")
if (isTRUE(interactive())) {
  testthat::source_test_helpers("tests/testthat", env = rlang::current_env())
  testthat::source_test_setup("tests/testthat", env = rlang::current_env())
}


## determine version of `mrgsolve` package on the current machine
mrgsolve_version_raw <- packageVersion("mrgsolve")
mrgsolve_version <- setNames(as.list(as.numeric(strsplit(as.character(mrgsolve_version_raw), "\\.")[[1]])), c("MAJOR", "MINOR", "PATCH"))


#######################################################################################################################|
## mock `print_function_calls()` "R/helpers.R" function so messages are suppressed during unit testing -------------
#######################################################################################################################|

mockr::with_mock(
  print_function_calls = omit_function_calls,
  print_console_msgs = force_console_msgs,
  .env = topenv(), {
    
    
#########################################################################################|
## SETUP --------------------------------------------------------------------------------
#########################################################################################|
    
    
## model with 3 parameters
code_3par <- '
[ PARAM ] CL = 1, V = 30, KA = 1.3
[ PKMODEL ] cmt = "GUT CENT", depot = TRUE
$TABLE
capture CP = CENT/V;'

mod_3par <- mrgsolve::mcode(model = "test_mod_3par", code = code_3par, compile = FALSE)
nparams_3par <- length(names(mrgsolve::param(mod_3par)))
param_idxs_3par <- 1:nparams_3par

## model with 6 parameters
code_6par <- '
$PARAM CL1 = 1, V1 = 30, KA1 = 1.3, CL2 = 1, V2 = 30, KA2 = 1.3
'

mod_6par <- mrgsolve::mcode(model = "test_mod_6par", code = code_6par, compile = FALSE)
nparams_6par <- length(names(mrgsolve::param(mod_6par)))
param_idxs_6par <- 1:nparams_6par

## model with 6 parameters (1 in fixed block)
code_6par_fixed <- '
$PARAM CL1 = 1, V1 = 30, KA1 = 1.3, CL2 = 1, V2 = 30
$FIXED KA2 = 1.3
'
mod_6par_fixed <- mrgsolve::mcode(model = "test_mod_6par_fixed", code = code_6par_fixed, compile = FALSE)


## model with 2 parameters
code_2par <- '
$PARAM CL1 = 1, V1 = 30
'
mod_2par <- mrgsolve::mcode(model = "test_mod_2par", code = code_2par, compile = FALSE)


## model with 2 parameters & parameter annotations
code_2par_annot <- '
$PARAM @annotated
CL1 : 1  : clearance (L/day)
V1  : 30 : volume    (L)
'
mod_2par_annot <- mrgsolve::mcode(model = "test_mod_2par_annot", code = code_2par_annot, compile = FALSE)



## model with 2 parameters (1 fixed) without annotations
code_2par_fixed <- '
$PARAM CL1 = 1
$FIXED V1 = 30
'
mod_2par_fixed <- mrgsolve::mcode(model = "test_mod_2par_fixed", code = code_2par_fixed, compile = FALSE)


## model with 2 parameters (1 fixed) & parameter annotations
code_2par_fixed_annot <- '
$PARAM @annotated
CL1 : 1  : clearance (L/day)
$FIXED V1 = 30
'
mod_2par_fixed_annot <- mrgsolve::mcode(model = "test_mod_2par_fixed_annot", code = code_2par_fixed_annot, compile = FALSE)



#########################################################################################|
## Random Effect Matrices -------------------------------------------------------------------
#########################################################################################|

## model with 1 OMEGA block & 1 SIGMA block
code_full_block_re <- '
$OMEGA @labels a b c d e f
1 2 3 4 5 6
$SIGMA @labels g h i j k l
1 2 3 4 5 6 
'
mod_full_block_re <- mcode("test_model_full_block_re", code_full_block_re, compile = FALSE)

## model with 3 OMEGA & 3 SIGMA blocks
code_multi_block_re <- '
$OMEGA @labels a b
1 2
$OMEGA @labels c d e
3 4 5
$OMEGA @labels f
6
$SIGMA @labels g h
1 2
$SIGMA @labels i j k
3 4 5
$SIGMA @labels l
6
'
mod_multi_block_re <- mcode("test_model_multi_block_re", code_multi_block_re, compile = FALSE)


## model with 0 OMEGA & 0 SIGMA blocks
code_no_re_blocks <- '
$PARAM V1 = 1
'
mod_no_re_blocks <- mcode("test_model_no_re_blocks", code_no_re_blocks, compile = FALSE)


code_omega_block_only <- '
$OMEGA @labels a b c d e f
1 2 3 4 5 6
'
mod_omega_block_only <- mrgsolve::mcode("test_model_omega_block_only", code_omega_block_only, compile = FALSE)


#########################################################################################|
## define shiny app defaults -------------------------------------------------------------
#########################################################################################|

### run_helpers_in_test_mode() ------------------------------------------------------------
testthat::test_that("run_helpers_in_test_mode returns a logical constant", {
  testthat::expect_type(run_helpers_in_test_mode(), "logical")
  testthat::expect_identical(run_helpers_in_test_mode(), FALSE)
  testthat::expect_false(run_helpers_in_test_mode())
})


### run_app_with_reactlog() ---------------------------------------------------------------
testthat::test_that("run_app_with_reactlog returns a logical constant", {
  testthat::expect_type(run_app_with_reactlog(), "logical")
  testthat::expect_identical(run_app_with_reactlog(), FALSE)
  testthat::expect_false(run_app_with_reactlog())
})


### get_shiny_app_project_path() ----------------------------------------------------------

### NO TESTS ###



### get_this_script_path() ----------------------------------------------------------------

### NO TESTS ###



### setwd_shiny_app_project() -------------------------------------------------------------

### NO TESTS ###



### run_all_unit_tests() ------------------------------------------------------------------

### NO TESTS ###



### specify_default_rx_input() ------------------------------------------------------------
testthat::test_that("specify_default_rx_input returns a string constant", {
  testthat::expect_type(specify_default_rx_input(), "character")
  testthat::expect_identical(specify_default_rx_input(), "three_periods")
})



### fetch_default_rx_input_list() ---------------------------------------------------------
testthat::test_that("fetch_default_rx_input_list returns a named list with 3 constant string elements", {
  
  default_rx_input <- list(
    placeholder = "Rx specification",
    two_periods = "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28",
    three_periods = "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5"
  )
  
  testthat::expect_type(fetch_default_rx_input_list(), "list")
  testthat::expect_named(fetch_default_rx_input_list(), names(default_rx_input))
  testthat::expect_length(fetch_default_rx_input_list(), 3)
  testthat::expect_type(unlist(fetch_default_rx_input_list()), "character")
  testthat::expect_identical(fetch_default_rx_input_list(), default_rx_input)
  testthat::expect_identical(fetch_default_rx_input_list()[[1]], default_rx_input[[1]])
  testthat::expect_identical(fetch_default_rx_input_list()[[2]], default_rx_input[[2]])
  testthat::expect_identical(fetch_default_rx_input_list()[[3]], default_rx_input[[3]])
})



### define_default_rx_input() -------------------------------------------------------------
testthat::test_that("define_default_rx_input returns the 'three_periods' rx input string", {
  
  default_rx_input <- list(
    placeholder = "Rx specification",
    two_periods = "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28",
    three_periods = "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5"
  )
  
  testthat::expect_type(define_default_rx_input(), "character")
  testthat::expect_length(define_default_rx_input(), 1)
  testthat::expect_identical(define_default_rx_input(), default_rx_input$three_periods)
  testthat::expect_identical(define_default_rx_input(), "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5")
  testthat::expect_identical(define_default_rx_input(), fetch_default_rx_input_list()$three_periods)
  testthat::expect_identical(define_default_rx_input(), fetch_default_rx_input_list()[[specify_default_rx_input()]])
})



### print_function_calls() ----------------------------------------------------------------
testthat::test_that("print_function_calls prints the function type & name to console", {
  
  ## test case 1
  fn_name <- "test_function_call"
  fn_type <- "function"
  expected_output <- "call function()  -->  test_function_call()"
  
  ## verify correct output
  testthat::expect_output(
    print_function_calls(fn_name = fn_name, fn_type = fn_type, print_call = TRUE),
    regexp = expected_output, fixed = TRUE
  )
  
  ## verify no output
  testthat::expect_output(
    print_function_calls(fn_name = fn_name, fn_type = fn_type, print_call = FALSE),
    regexp = NA
  )
  
  
  ## test case 2
  fn_name <- "test_reactive_call"
  fn_type <- "reactive"
  expected_output <- "call reactive()  -->  test_reactive_call()"
  
  ## verify correct output
  testthat::expect_output(
    print_function_calls(fn_name = fn_name, fn_type = fn_type, print_call = TRUE),
    regexp = expected_output, fixed = TRUE
  )
  
  ## verify no output
  testthat::expect_output(
    print_function_calls(fn_name = fn_name, fn_type = fn_type, print_call = FALSE),
    regexp = NA
  )
})



### fetch_custom_modlist() ----------------------------------------------------------------
testthat::test_that("fetch_custom_modlist returns model names included in the shiny app model library", {
  
  lib_model_paths <- list.files(path = here::here("model_library"), "(?i)\\.cpp$", full.names = TRUE)
  lib_model_names <- tools::file_path_sans_ext(basename(lib_model_paths))
  named_lib_model_paths <- setNames(lib_model_paths, lib_model_names)
  
  lib_model_paths_hc <- here::here(c("model_library/invalid_MM.cpp", "model_library/popPK_1cmt_linear-mm_iv-sc.cpp", 
                          "model_library/popPK_1cmt_linear_iv-sc.cpp", "model_library/popPK_1cmt_mm_iv-sc.cpp", 
                          "model_library/popPK_2cmt_linear-mm_iv-sc.cpp", "model_library/popPK_2cmt_linear_iv-sc.cpp", 
                          "model_library/popPK_2cmt_mm_iv-sc.cpp", "model_library/popPK_3cmt_linear_iv-sc.cpp", 
                          "model_library/tmdd_full_2cmt_1abs.cpp", "model_library/tmdd_mm-const_2cmt_1abs.cpp", 
                          "model_library/tmdd_mm_2cmt_1abs.cpp", "model_library/tmdd_qe-const_2cmt_1abs.cpp", 
                          "model_library/tmdd_qe_2cmt_1abs.cpp", "model_library/tmdd_qss-const_2cmt_1abs.cpp", 
                          "model_library/tmdd_qss_2cmt_1abs.cpp"))
  
  
  lib_model_names_hc <- c("invalid_MM", "popPK_1cmt_linear-mm_iv-sc", "popPK_1cmt_linear_iv-sc", 
                          "popPK_1cmt_mm_iv-sc", "popPK_2cmt_linear-mm_iv-sc", "popPK_2cmt_linear_iv-sc", 
                          "popPK_2cmt_mm_iv-sc", "popPK_3cmt_linear_iv-sc", "tmdd_full_2cmt_1abs", 
                          "tmdd_mm-const_2cmt_1abs", "tmdd_mm_2cmt_1abs", "tmdd_qe-const_2cmt_1abs", 
                          "tmdd_qe_2cmt_1abs", "tmdd_qss-const_2cmt_1abs", "tmdd_qss_2cmt_1abs")
  
  named_lib_model_paths_hc <- c(
     invalid_MM                  = here::here("model_library/invalid_MM.cpp"), 
    `popPK_1cmt_linear-mm_iv-sc` = here::here("model_library/popPK_1cmt_linear-mm_iv-sc.cpp"), 
    `popPK_1cmt_linear_iv-sc`    = here::here("model_library/popPK_1cmt_linear_iv-sc.cpp"), 
    `popPK_1cmt_mm_iv-sc`        = here::here("model_library/popPK_1cmt_mm_iv-sc.cpp"), 
    `popPK_2cmt_linear-mm_iv-sc` = here::here("model_library/popPK_2cmt_linear-mm_iv-sc.cpp"), 
    `popPK_2cmt_linear_iv-sc`    = here::here("model_library/popPK_2cmt_linear_iv-sc.cpp"), 
    `popPK_2cmt_mm_iv-sc`        = here::here("model_library/popPK_2cmt_mm_iv-sc.cpp"), 
    `popPK_3cmt_linear_iv-sc`    = here::here("model_library/popPK_3cmt_linear_iv-sc.cpp"), 
     tmdd_full_2cmt_1abs         = here::here("model_library/tmdd_full_2cmt_1abs.cpp"), 
    `tmdd_mm-const_2cmt_1abs`    = here::here("model_library/tmdd_mm-const_2cmt_1abs.cpp"), 
     tmdd_mm_2cmt_1abs           = here::here("model_library/tmdd_mm_2cmt_1abs.cpp"), 
    `tmdd_qe-const_2cmt_1abs`    = here::here("model_library/tmdd_qe-const_2cmt_1abs.cpp"), 
     tmdd_qe_2cmt_1abs           = here::here("model_library/tmdd_qe_2cmt_1abs.cpp"), 
    `tmdd_qss-const_2cmt_1abs`   = here::here("model_library/tmdd_qss-const_2cmt_1abs.cpp"), 
     tmdd_qss_2cmt_1abs          = here::here("model_library/tmdd_qss_2cmt_1abs.cpp")
  )
  
  
  ## call function to get results for all input parameter combinations
  res_default_raw <- withr::with_dir(here::here(), fetch_custom_modlist(names_only = FALSE))
  res_default_names <- names(res_default_raw)
  res_default <- setNames(here::here(res_default_raw), res_default_names)
  res_names_only <- withr::with_dir(here::here(), fetch_custom_modlist(names_only = TRUE))
  
  ## test against programmatically generated expected results
  testthat::expect_identical(unname(res_default), lib_model_paths)
  testthat::expect_identical(res_default, named_lib_model_paths)
  testthat::expect_identical(res_names_only, lib_model_names)
  
  ## test against hard-coded expected results
  testthat::expect_identical(unname(res_default), lib_model_paths_hc)
  testthat::expect_identical(res_default, named_lib_model_paths_hc)
  testthat::expect_identical(res_names_only, lib_model_names_hc)
})



### fetch_mrgsolve_modlist() --------------------------------------------------------------
testthat::test_that("fetch_mrgsolve_modlist returns model names included with the `mrgsolve` package", {
  
  if (isFALSE("mrgsolve" %in% (.packages()))) require(mrgsolve)
  
  mrgsolve_modlib_paths <- list.files(path = mrgsolve::modlib(), "(?i)\\.cpp$", full.names = TRUE)
  mrgsolve_modlib_names <- tools::file_path_sans_ext(basename(mrgsolve_modlib_paths))
  named_mrgsolve_modlib_paths <- setNames(mrgsolve_modlib_paths, mrgsolve_modlib_names)
  
  ## call function to get results for all input parameter combinations
  res_default <- fetch_mrgsolve_modlist(names_only = FALSE)
  res_names_only <- fetch_mrgsolve_modlist(names_only = TRUE)
  
  ## test against programmatically generated expected results
  testthat::expect_identical(unname(res_default), mrgsolve_modlib_paths)
  testthat::expect_identical(res_default, named_mrgsolve_modlib_paths)
  testthat::expect_identical(res_names_only, mrgsolve_modlib_names)
})



### extract_path_file_stem() --------------------------------------------------------------
testthat::test_that("extract_path_file_stem returns path basename without file extension (if any)", {
  
  test_res  <- "test-file"
  test_file <- "test-file.txt"
  test_root <- "C:/Users/test-account/test-dir"
  test_path <- file.path(test_root, test_file)
  
  testthat::expect_type(extract_path_file_stem(path = test_path), "character")
  testthat::expect_identical(extract_path_file_stem(path = test_res), test_res)
  testthat::expect_identical(extract_path_file_stem(path = test_file), test_res)
  testthat::expect_identical(extract_path_file_stem(path = test_path), test_res)
  testthat::expect_identical(extract_path_file_stem(path = test_root), basename(test_root))
})



### modify_modlist() ----------------------------------------------------------------------
testthat::test_that("modify_modlist returns modified vector of library model names", {

  ## test against shiny app model library
  libmod_names <- fetch_custom_modlist(names_only = TRUE)
  libmod_last <- libmod_names[length(libmod_names)] 
  libmod_names_last_first <- c(libmod_last, setdiff(libmod_names, libmod_last))
  
  testthat::expect_identical(modify_modlist(modlist = NULL, first = NULL, drop = NULL), libmod_names)
  testthat::expect_identical(modify_modlist(modlist = NULL, first = libmod_last, drop = NULL), libmod_names_last_first)
  testthat::expect_identical(modify_modlist(modlist = NULL, first = libmod_last, drop = libmod_last), setdiff(libmod_names, libmod_last))
  testthat::expect_identical(modify_modlist(modlist = NULL, first = libmod_last, drop = libmod_names), character(0))
  
  
  ## create simple test model list 
  test_modlist <- c("mod1", "mod2", "mod3")
  test_modlist_last <- test_modlist[length(test_modlist)]
  test_modlist_last_first <- c("mod3", "mod1", "mod2")
  
  testthat::expect_identical(modify_modlist(modlist = test_modlist, first = NULL, drop = NULL), test_modlist)
  testthat::expect_identical(modify_modlist(modlist = test_modlist, first = test_modlist_last, drop = NULL), test_modlist_last_first)
  testthat::expect_identical(modify_modlist(modlist = test_modlist, first = test_modlist_last, drop = test_modlist_last), setdiff(test_modlist_last_first, test_modlist_last))
  testthat::expect_identical(modify_modlist(modlist = test_modlist, first = test_modlist_last, drop = test_modlist), character(0))
  
})



### drop_custom_modlist_models() ----------------------------------------------------------
testthat::test_that("drop_custom_modlist_models returns subset of library model names after excluding specified models", {
  testthat::expect_identical(drop_custom_modlist_models(test_mode = TRUE), NA_character_)
  testthat::expect_identical(drop_custom_modlist_models(test_mode = FALSE), "invalid_MM")
  testthat::expect_identical(drop_custom_modlist_models(test_mode = run_helpers_in_test_mode()), "invalid_MM")
  testthat::expect_identical(drop_custom_modlist_models(test_mode = NULL), "invalid_MM")
})



#########################################################################################|
## shiny app helper functions ------------------------------------------------------------
#########################################################################################|

### define_default_ev_data_cols() ----------------------------------------------------------
testthat::test_that("define_default_ev_data_cols returns a list of dosing event data items with their default values", {
  
  dflt_ev_cols <- list(ID = 1L, TIME = 0, AMT = 0, RATE = 0, EVID = 1L, CMT = 1L, ADDL = 0L, II = 0, SS = 0L)
  all_ev_cols <- names(dflt_ev_cols)
  int_ev_cols <- c("ID", "EVID", "CMT", "ADDL", "SS")
  dbl_ev_cols <- setdiff(all_ev_cols, int_ev_cols)
  
  testthat::expect_type(define_default_ev_data_cols(), "list")
  testthat::expect_length(define_default_ev_data_cols(), 9)
  testthat::expect_named(define_default_ev_data_cols(), c("ID", "TIME", "AMT", "RATE", "EVID", "CMT", "ADDL", "II", "SS"))
  testthat::expect_named(define_default_ev_data_cols(), names(dflt_ev_cols))
  testthat::expect_type(unlist(define_default_ev_data_cols()[int_ev_cols]), "integer")
  testthat::expect_type(unlist(define_default_ev_data_cols()[define_integer_ev_data_cols()]), "integer")
  testthat::expect_type(unlist(define_default_ev_data_cols()[dbl_ev_cols]), "double")
  testthat::expect_identical(define_default_ev_data_cols()$ID, 1L)
  testthat::expect_identical(define_default_ev_data_cols()$TIME, 0)
  testthat::expect_identical(define_default_ev_data_cols()$AMT, 0)
  testthat::expect_identical(define_default_ev_data_cols()$RATE, 0)
  testthat::expect_identical(define_default_ev_data_cols()$EVID, 1L)
  testthat::expect_identical(define_default_ev_data_cols()$CMT, 1L)
  testthat::expect_identical(define_default_ev_data_cols()$ADDL, 0L)
  testthat::expect_identical(define_default_ev_data_cols()$II, 0)
  testthat::expect_identical(define_default_ev_data_cols()$SS, 0L)
  testthat::expect_identical(define_default_ev_data_cols(), dflt_ev_cols)
})


### define_integer_ev_data_cols() -----------------------------------------------------------
testthat::test_that("define_integer_ev_data_cols returns a character vector of all dosing event data items with integer type", {
  ## is this a redundant test if `define_integer_ev_data_cols()` is called within tests for `define_default_ev_data_cols()`???
  int_ev_cols <- c("ID", "EVID", "CMT", "ADDL", "SS")
  testthat::expect_identical(define_integer_ev_data_cols(), int_ev_cols)
})


### sort_ev_colnames() -----------------------------------------------------------
testthat::test_that("sort_ev_colnames sorts the dataframe column names according to the `define_default_ev_data_cols()` definition (case independent) & drops any non-ev data columns", {
  test_ev_data <- ev_rx(define_default_rx_input())
  test_ev_cols <- sort_ev_colnames(test_ev_data)
  test_ev_caps <- as_tibble(test_ev_data) %>% rename_with(.fn = toupper)
  test_ev_data_dummy <- test_ev_data %>% mutate(col1 = 0, col2 = 0, col3 = 0)
  test_ev_caps_dummy <- test_ev_caps %>% mutate(col1 = 0, col2 = 0, col3 = 0)
  
  testthat::expect_s4_class(ev_rx(define_default_rx_input()), class = "ev")
  testthat::expect_type(sort_ev_colnames(ev_rx(define_default_rx_input())), "character")
  testthat::expect_length(sort_ev_colnames(ev_rx(define_default_rx_input())), length(names(ev_rx(define_default_rx_input()))))
  testthat::expect_identical(test_ev_cols, c("time", "amt", "rate", "evid", "cmt", "addl", "ii"))
  testthat::expect_identical(sort_ev_colnames(test_ev_caps), c("TIME", "AMT", "RATE", "EVID", "CMT", "ADDL", "II"))
  testthat::expect_identical(sort_ev_colnames(test_ev_data_dummy), c("time", "amt", "rate", "evid", "cmt", "addl", "ii"))
  testthat::expect_identical(sort_ev_colnames(test_ev_caps_dummy), c("TIME", "AMT", "RATE", "EVID", "CMT", "ADDL", "II"))
  testthat::expect_identical(sort_ev_colnames(cbind(test_ev_data, test_ev_caps)), c("time", "amt", "rate", "evid", "cmt", "addl", "ii"))
  testthat::expect_identical(sort_ev_colnames(cbind(test_ev_caps, test_ev_data)), c("TIME", "AMT", "RATE", "EVID", "CMT", "ADDL", "II"))
})


### ev_cols_tolower() -----------------------------------------------------------
testthat::test_that("ev_cols_tolower sorts & converts dataframe dosing columns to lower case, expect for ID, and returns sorted dataset", {
  ## `ev_cols_tolower()` takes & returns a data.frame
  test_ev_data <- ev_rx(define_default_rx_input())
  test_ev_lowr <- ev_cols_tolower(test_ev_data)
  test_ev_caps <- as_tibble(test_ev_data) %>% rename_with(.fn = toupper)
  test_ev_data_dummy <- test_ev_lowr %>% mutate(ID = 1L, col1 = 0, col2 = 0, col3 = 0)
  test_ev_caps_dummy <- test_ev_caps %>% mutate(ID = 1L, col1 = 0, col2 = 0, col3 = 0)

  testthat::expect_s4_class(ev_rx(define_default_rx_input()), class = "ev")
  testthat::expect_type(ev_cols_tolower(ev_rx(define_default_rx_input())), "list")
  testthat::expect_identical(names(test_ev_lowr), c("time", "amt", "rate", "evid", "cmt", "addl", "ii"))
  testthat::expect_identical(names(test_ev_lowr), c("time", "amt", "rate", "evid", "cmt", "addl", "ii"))
  testthat::expect_identical(names(ev_cols_tolower(test_ev_caps)), c("time", "amt", "rate", "evid", "cmt", "addl", "ii"))
  testthat::expect_identical(names(ev_cols_tolower(test_ev_data_dummy)), c("ID", "time", "amt", "rate", "evid", "cmt", "addl", "ii", "col1", "col2", "col3"))
  testthat::expect_identical(names(ev_cols_tolower(test_ev_caps_dummy)), c("ID", "time", "amt", "rate", "evid", "cmt", "addl", "ii", "col1", "col2", "col3"))
  testthat::expect_error(ev_cols_tolower(cbind(test_ev_data, test_ev_caps)))
})



### fill_default_ev_data_cols() -----------------------------------------------------------
testthat::test_that("fill_default_ev_data_cols takes a data.frame & adds all missing dosing data columns with default values", {
  ## `ev_cols_tolower()` takes & returns a data.frame
  test_ev_data <- ev_rx(define_default_rx_input())
  test_ev_dflt <- fill_default_ev_data_cols(test_ev_data)
  fill_ev_cols <- setdiff(names(test_ev_dflt), toupper(names(test_ev_data)))
  diff_ev_data <- test_ev_dflt[fill_ev_cols]
  test_ev_caps <- as_tibble(test_ev_data) %>% rename_with(.fn = toupper)
  test_ev_data_dummy <- test_ev_data %>% mutate(ID = 1L, col1 = 0, col2 = 0, col3 = 0)
  test_ev_caps_dummy <- test_ev_caps %>% mutate(ID = 1L, col1 = 0, col2 = 0, col3 = 0)
  
  
  testthat::expect_s4_class(test_ev_data, class = "ev")
  testthat::expect_identical(names(test_ev_data), c("time", "amt", "rate", "ii", "addl", "cmt", "evid"))
  testthat::expect_type(test_ev_dflt, "list")
  testthat::expect_identical(names(test_ev_dflt), c("ID", "TIME", "AMT", "RATE", "EVID", "CMT", "ADDL", "II", "SS"))
  testthat::expect_identical(fill_ev_cols, c("ID", "SS"))
  testthat::expect_identical(unique(diff_ev_data$ID), define_default_ev_data_cols()$ID)
  testthat::expect_identical(unique(diff_ev_data$SS), define_default_ev_data_cols()$SS)
  testthat::expect_identical(fill_default_ev_data_cols(data.frame(ID = 1)), as_tibble(define_default_ev_data_cols()))
  testthat::expect_identical(names(fill_default_ev_data_cols(test_ev_caps)), c("ID", "TIME", "AMT", "RATE", "EVID", "CMT", "ADDL", "II", "SS"))
  testthat::expect_identical(names(fill_default_ev_data_cols(test_ev_data_dummy)), c("ID", "TIME", "AMT", "RATE", "EVID", "CMT", "ADDL", "II", "SS", "COL1", "COL2", "COL3"))
  testthat::expect_identical(names(fill_default_ev_data_cols(test_ev_caps_dummy)), c("ID", "TIME", "AMT", "RATE", "EVID", "CMT", "ADDL", "II", "SS", "COL1", "COL2", "COL3"))
  testthat::expect_error(fill_default_ev_data_cols(cbind(test_ev_data, test_ev_caps)))
})


### define_tgrid_end_scaling_factor() -----------------------------------------------------------
testthat::test_that("define_tgrid_end_scaling_factor returns a numeric constant", {
  testthat::expect_type(define_tgrid_end_scaling_factor(), "double")
  testthat::expect_length(define_tgrid_end_scaling_factor(), 1)
  testthat::expect_identical(define_tgrid_end_scaling_factor(), 1.5)
})


### extract_tgrid_defaults() -----------------------------------------------------------
testthat::test_that("extract_tgrid_defaults returns `tgrid` S4 object slots with default values as a list", {
  testthat::expect_type(extract_tgrid_defaults(), "list")
  testthat::expect_length(extract_tgrid_defaults(), 6)
  testthat::expect_named(extract_tgrid_defaults(), c("start", "end", "delta", "add", "offset", "scale"))
  testthat::expect_type(unlist(extract_tgrid_defaults()), "double")
  testthat::expect_identical(extract_tgrid_defaults()$start, 0)
  testthat::expect_identical(extract_tgrid_defaults()$end, 24)
  testthat::expect_identical(extract_tgrid_defaults()$delta, 1)
  testthat::expect_identical(extract_tgrid_defaults()$add, numeric(0))
  testthat::expect_identical(extract_tgrid_defaults()$offset, 0)
  testthat::expect_identical(extract_tgrid_defaults()$scale, 1)
  testthat::expect_identical(extract_tgrid_defaults(), setNames(lapply(slotNames(tgrid()), function(curr_slot) slot(tgrid(), name = curr_slot)), slotNames(tgrid())))
  testthat::expect_identical(extract_tgrid_defaults(), list(start = 0, end = 24, delta = 1, add = numeric(0), offset = 0, scale = 1))
})


### set_mod_envir_slot() -----------------------------------------------------------
testthat::test_that("set_mod_envir_slot allows replacement of `mrgmod` object environments", {
  mod <- app_libmod_objs[[fetch_default_libmod_name()]]
  mod_envir <- mod@envir
  new_envir <- environment()
  
  testthat::expect_s4_class(set_mod_envir_slot(mod = mod), class = "mrgmod")
  testthat::expect_s4_class(set_mod_envir_slot(mod = mod, envir = mod_envir), class = "mrgmod")
  testthat::expect_length(set_mod_envir_slot(mod = mod), 1)
  testthat::expect_length(set_mod_envir_slot(mod = mod, envir = mod_envir), 1)
  testthat::expect_false(identical(set_mod_envir_slot(mod = mod)@envir, mod_envir))
  testthat::expect_identical(set_mod_envir_slot(mod = mod, envir = new_envir)@envir, new_envir)
})


### fetch_default_sim_settings() -----------------------------------------------------------
testthat::test_that("fetch_default_sim_settings returns a list of simulation settings and their default numerical values", {
  default_sim_settings <- list(
    num_subj = 10,
    num_sims = 10,
    sim_seed = 2674474
  )
  testthat::expect_type(fetch_default_sim_settings(), "list")
  testthat::expect_length(fetch_default_sim_settings(), 3)
  testthat::expect_named(fetch_default_sim_settings(), names(default_sim_settings))
  testthat::expect_identical(fetch_default_sim_settings()$num_subj, 10)
  testthat::expect_identical(fetch_default_sim_settings()$num_sims, 10)
  testthat::expect_identical(fetch_default_sim_settings()$sim_seed, 2674474)
  testthat::expect_identical(fetch_default_sim_settings(), default_sim_settings)
})


### update_sim_settings_fn() -----------------------------------------------------------
testthat::test_that("update_sim_settings_fn updates the numerical values for each specified simulation setting in the list", {
  test_sim_settings <- list(
    num_subj = 500,
    num_sims = 30,
    sim_seed = 12345
  )
  updated_sim_settings <- update_sim_settings_fn(num_subj = test_sim_settings$num_subj,
                                                 num_sims = test_sim_settings$num_sims,
                                                 sim_seed = test_sim_settings$sim_seed)
  
  ## check defaults values
  testthat::expect_type(update_sim_settings_fn(), "list")
  testthat::expect_length(update_sim_settings_fn(), length(fetch_default_sim_settings()))
  testthat::expect_named(update_sim_settings_fn(), names(fetch_default_sim_settings()))
  testthat::expect_identical(update_sim_settings_fn()$num_subj, fetch_default_sim_settings()$num_subj)
  testthat::expect_identical(update_sim_settings_fn()$num_sims, fetch_default_sim_settings()$num_sims)
  testthat::expect_identical(update_sim_settings_fn()$sim_seed, fetch_default_sim_settings()$sim_seed)
  testthat::expect_identical(update_sim_settings_fn(), fetch_default_sim_settings())
  
  ## update with custom values
  testthat::expect_type(updated_sim_settings, "list")
  testthat::expect_length(updated_sim_settings, length(test_sim_settings))
  testthat::expect_named(updated_sim_settings, names(test_sim_settings))
  testthat::expect_identical(updated_sim_settings$num_subj, test_sim_settings$num_subj)
  testthat::expect_identical(updated_sim_settings$num_sims, test_sim_settings$num_sims)
  testthat::expect_identical(updated_sim_settings$sim_seed, test_sim_settings$sim_seed)
  testthat::expect_identical(updated_sim_settings, test_sim_settings)
})


### define_solver_settings_params() -----------------------------------------------------------
testthat::test_that("define_solver_settings_params returns a character vector of solver settings parameter names", {
  testthat::expect_type(define_solver_settings_params(), "character")
  testthat::expect_length(define_solver_settings_params(), 9)
  testthat::expect_identical(define_solver_settings_params(), c("rtol", "atol", "ss_rtol", "ss_atol", "maxsteps", "hmin", "hmax", "ixpr", "mxhnil"))
})


### fetch_solver_settings() -----------------------------------------------------------
testthat::test_that("fetch_solver_settings returns a list of solver settings with custom or defaut mrgsolve parameter values", {
  testthat::expect_type(fetch_solver_settings(), "list")
  testthat::expect_length(fetch_solver_settings(), 9)
  testthat::expect_named(fetch_solver_settings(), c("rtol", "atol", "ss_rtol", "ss_atol", "maxsteps", "hmin", "hmax", "ixpr", "mxhnil"))
  testthat::expect_identical(fetch_solver_settings(use_custom = TRUE), define_custom_solver_settings()[define_solver_settings_params()])
  testthat::expect_identical(fetch_solver_settings(use_custom = FALSE), define_mrgmod_solver_settings()[define_solver_settings_params()])
})



### define_custom_solver_settings() -----------------------------------------------------------
testthat::test_that("define_custom_solver_settings returns a list of solver settings and their default numerical values", {
  custom_solver_settings <- list(
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
  
  testthat::expect_type(define_custom_solver_settings(), "list")
  testthat::expect_length(define_custom_solver_settings(), 9)
  testthat::expect_named(define_custom_solver_settings(), names(custom_solver_settings))
  testthat::expect_named(define_custom_solver_settings(), define_solver_settings_params())
  testthat::expect_identical(define_custom_solver_settings()$rtol, 1e-10)
  testthat::expect_identical(define_custom_solver_settings()$atol, 1e-12)
  testthat::expect_identical(define_custom_solver_settings()$ss_rtol, 1e-10)
  testthat::expect_identical(define_custom_solver_settings()$ss_atol, 1e-12)
  testthat::expect_identical(define_custom_solver_settings()$maxsteps, 10000)
  testthat::expect_identical(define_custom_solver_settings()$hmin, 0)
  testthat::expect_identical(define_custom_solver_settings()$hmax, 0)
  testthat::expect_identical(define_custom_solver_settings()$ixpr, 0)
  testthat::expect_identical(define_custom_solver_settings()$mxhnil, 2)
  testthat::expect_identical(define_custom_solver_settings(), custom_solver_settings)
})


### define_mrgmod_solver_settings() -----------------------------------------------------------
testthat::test_that("define_mrgmod_solver_settings returns a list of default solver settings for mrgsolve", {
  mrgmod_solver_settings <- list(
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
  
  testthat::expect_type(define_mrgmod_solver_settings(), "list")
  testthat::expect_length(define_mrgmod_solver_settings(), 9)
  testthat::expect_named(define_mrgmod_solver_settings(), names(mrgmod_solver_settings))
  testthat::expect_named(define_mrgmod_solver_settings(), define_solver_settings_params())
  testthat::expect_identical(define_mrgmod_solver_settings()$rtol, 1e-8)
  testthat::expect_identical(define_mrgmod_solver_settings()$atol, 1e-8)
  testthat::expect_identical(define_mrgmod_solver_settings()$ss_rtol, 1e-8)
  testthat::expect_identical(define_mrgmod_solver_settings()$ss_atol, 1e-8)
  testthat::expect_identical(define_mrgmod_solver_settings()$maxsteps, 20000)
  testthat::expect_identical(define_mrgmod_solver_settings()$hmin, 0)
  testthat::expect_identical(define_mrgmod_solver_settings()$hmax, 0)
  testthat::expect_identical(define_mrgmod_solver_settings()$ixpr, 0)
  testthat::expect_identical(define_mrgmod_solver_settings()$mxhnil, 2)
  testthat::expect_identical(define_mrgmod_solver_settings(), mrgmod_solver_settings)
})


### render_interactive_matrix() -----------------------------------------------------------
testthat::test_that("render_interactive_matrix returns a custom JavaScript function as a multi-line text string", {
  renderer <- "
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
  
  testthat::expect_type(render_interactive_matrix(), "character")
  testthat::expect_length(render_interactive_matrix(), 1)
  testthat::expect_identical(render_interactive_matrix(), renderer)
})


### input_as_numeric() -----------------------------------------------------------
testthat::test_that("input_as_numeric converts a single valid string inputs to numeric values ", {
  testthat::expect_type(input_as_numeric("12345"), "double")
  testthat::expect_length(input_as_numeric("12345"), 1)
  testthat::expect_identical(input_as_numeric("12345"), 12345)
  testthat::expect_error(input_as_numeric(123))
  testthat::expect_identical(input_as_numeric(c("1", "2")), numeric(0))
})


### interpret() -----------------------------------------------------------
testthat::test_that("interpret safely parses user input expressions & constrains the length & functions in the input string", {
  
  ## basic tests
  testthat::expect_type(interpret("12345"), "double")
  testthat::expect_type(interpret("'12345'"), "character")
  testthat::expect_length(interpret("12345"), 1)
  testthat::expect_identical(interpret("12345"), 12345)
  testthat::expect_error(interpret(123))
  testthat::expect_identical(interpret(c("1", "2")), NULL)
  
  ## complex exression tests
  testthat::expect_identical(interpret("2*7"), 14)
  testthat::expect_identical(interpret("7-2"), 5)
  testthat::expect_identical(interpret("7+2"), 9)
  testthat::expect_identical(interpret("10/2"), 5)
  testthat::expect_identical(interpret("sqrt(16)"), 4)
  testthat::expect_identical(interpret("numeric(0)"), numeric(0))
  testthat::expect_identical(interpret("c()"), NULL)
  testthat::expect_identical(interpret("c(1)"), 1)
  testthat::expect_identical(interpret("c(1, 2)"), c(1, 2))
  testthat::expect_identical(interpret("c(1, 2, 3, 4, 5)"), c(1, 2, 3, 4, 5))
  testthat::expect_error(interpret("c(1, 2, 3, 4, 5)", max_length = 1))
  testthat::expect_error(interpret(c('1', '2', '3', '4', '5'), max_length = 5))
  testthat::expect_identical(interpret(c('1', '2', '3', '4', '5'), max_length = 6), NULL)
  
  ## return NULL for imcomplete expressions
  testthat::expect_identical(interpret("1e-"), NULL)
  testthat::expect_identical(interpret("1e"), NULL)
  testthat::expect_identical(interpret("1e+"), NULL)
  testthat::expect_identical(interpret("1-"), NULL)
  testthat::expect_identical(interpret("1+"), NULL)
  testthat::expect_identical(interpret("1/"), NULL)
  testthat::expect_identical(interpret("1*"), NULL)
  testthat::expect_identical(interpret("sqrt(16"), NULL)
  
  ## test empty expression
  testthat::expect_identical(interpret(""), NULL)
  
  
  ## disallowed function tests
  disallowed_fn <- "^"
  input_expr <- paste0(2, disallowed_fn, 7)
  error_regex <- escape_string_metachars(paste0("Disallowed function: ", disallowed_fn))
  testthat::expect_error(interpret(input_expr), regexp = error_regex)
  
  disallowed_fn <- "%%"
  input_expr <- paste0(2, disallowed_fn, 7)
  error_regex <- escape_string_metachars(paste0("Disallowed function: ", disallowed_fn))
  testthat::expect_error(interpret(input_expr), regexp = error_regex)
})


### split_param_idxs_in_half() -----------------------------------------------------------
testthat::test_that("split_param_idxs_in_half split model parameters indices in half and returns a list of `1st` & `2nd` halves", {
  
  ## model with 3 parameters
  testthat::expect_type(split_param_idxs_in_half(mod_3par), "list")
  testthat::expect_length(split_param_idxs_in_half(mod_3par), 2)
  testthat::expect_named(split_param_idxs_in_half(mod_3par), c("1st", "2nd"))
  testthat::expect_type(unname(unlist(split_param_idxs_in_half(mod_3par))), "integer")
  testthat::expect_length(unname(unlist(split_param_idxs_in_half(mod_3par))), nparams_3par)
  testthat::expect_identical(unname(unlist(split_param_idxs_in_half(mod_3par))), param_idxs_3par)
  testthat::expect_identical(split_param_idxs_in_half(mod_3par)$`1st`, c(1L, 2L))
  testthat::expect_identical(split_param_idxs_in_half(mod_3par)$`2nd`, c(3L))

  ## model with 6 parameters
  testthat::expect_type(split_param_idxs_in_half(mod_6par), "list")
  testthat::expect_length(split_param_idxs_in_half(mod_6par), 2)
  testthat::expect_named(split_param_idxs_in_half(mod_6par), c("1st", "2nd"))
  testthat::expect_type(unname(unlist(split_param_idxs_in_half(mod_6par))), "integer")
  testthat::expect_length(unname(unlist(split_param_idxs_in_half(mod_6par))), nparams_6par)
  testthat::expect_identical(unname(unlist(split_param_idxs_in_half(mod_6par))), param_idxs_6par)
  testthat::expect_identical(split_param_idxs_in_half(mod_6par)$`1st`, c(1L, 2L, 3L))
  testthat::expect_identical(split_param_idxs_in_half(mod_6par)$`2nd`, c(4L, 5L, 6L))
})


#######################################################################################################################|
## Extract model parameter information ---------------------------------------------------------------------------------
#######################################################################################################################|

### reserved_library_model_params() -----------------------------------------------------------
testthat::test_that("reserved_library_model_params returns a character vector of reserved library model parameters", {
  
  ## context = "output"
  testthat::expect_type(reserved_library_model_params(context = "output"), "character")
  testthat::expect_length(reserved_library_model_params(context = "output"), 4)
  testthat::expect_identical(reserved_library_model_params(context = "output"), c("DOSE", "REGNUM", "WT", "WTREF"))
  
  ## context = "input"
  testthat::expect_type(reserved_library_model_params(context = "input"), "character")
  testthat::expect_length(reserved_library_model_params(context = "input"), 2)
  testthat::expect_identical(reserved_library_model_params(context = "input"), c("DOSE", "REGNUM"))
  
  ## invalid context
  testthat::expect_error(reserved_library_model_params(context = "error"))
})


### build_invalid_context_error_msg() -----------------------------------------------------------
testthat::test_that("build_invalid_context_error_msg returns a error message string that includes the valid context options", {
  testthat::expect_type(build_invalid_context_error_msg(valid_contexts = c("input", "output")), "character")
  testthat::expect_length(build_invalid_context_error_msg(valid_contexts = c("input", "output")), 1)
  testthat::expect_identical(build_invalid_context_error_msg(valid_contexts = c("input", "output")), "\n   Unrecognized `context` for reserved library model parameters!\n   Please input one of the following valid contexts:  'input', 'output'")
  testthat::expect_identical(build_invalid_context_error_msg(valid_contexts = c("context_1", "context_2", "context_3")), "\n   Unrecognized `context` for reserved library model parameters!\n   Please input one of the following valid contexts:  'context_1', 'context_2', 'context_3'")
  testthat::expect_error(build_invalid_context_error_msg(), regexp = 'argument "valid_contexts" is missing, with no default')
})


### extract_model_params() -----------------------------------------------------------
testthat::test_that("extract_model_params returns a named list of model parameter values", {

  ## model with 6 parameters
  params_6par <- names(mrgsolve::param(mod_6par))
  testthat::expect_type(extract_model_params(mod_6par), "list")
  testthat::expect_length(extract_model_params(mod_6par), nparams_6par)
  testthat::expect_named(extract_model_params(mod_6par), params_6par)
  testthat::expect_identical(extract_model_params(mod_6par), list(CL1 = 1, V1 = 30, KA1 = 1.3, CL2 = 1, V2 = 30, KA2 = 1.3))
})

### model_details_col_order() -----------------------------------------------------------
testthat::test_that("model_details_col_order returns a character vector of ordered column names for the model annotations data.frame", {
  
  testthat::expect_type(model_details_col_order(), "character")
  testthat::expect_length(model_details_col_order(),6)
  testthat::expect_identical(model_details_col_order(), c("block", "name", "value", "unit", "descr", "options"))
})


### extract_model_annotations_data() -----------------------------------------------------------
testthat::test_that("extract_model_annotations_data returns data.frame of model annotation data", {
  
  ## model with 6 parameters
  param_value_6par <- unlist(as.list(mrgsolve::param(mod_6par)))
  param_names_6par <- names(param_value_6par)
  n_params_6par <- length(param_names_6par)
  testthat::expect_type(extract_model_annotations_data(mod_6par), "list")
  testthat::expect_length(extract_model_annotations_data(mod_6par), length(model_details_col_order()))
  testthat::expect_identical(nrow(extract_model_annotations_data(mod_6par)), n_params_6par)
  testthat::expect_named(extract_model_annotations_data(mod_6par), model_details_col_order())
  testthat::expect_identical(extract_model_annotations_data(mod_6par)$block, rep("PARAM", times = n_params_6par))
  testthat::expect_identical(extract_model_annotations_data(mod_6par)$name, param_names_6par)
  testthat::expect_identical(extract_model_annotations_data(mod_6par)$value, param_value_6par)
  testthat::expect_identical(extract_model_annotations_data(mod_6par)$unit, rep(".", times = n_params_6par))
  testthat::expect_identical(extract_model_annotations_data(mod_6par)$descr, rep(".", times = n_params_6par))
  testthat::expect_identical(extract_model_annotations_data(mod_6par)$options, rep(".", times = n_params_6par))
  
  ## default library model
  def_mod <- app_libmod_objs[[fetch_default_libmod_name()]]
  param_value_def <- unlist(as.list(mrgsolve::param(def_mod)))
  param_names_def <- names(param_value_def)
  n_params_def <- length(param_names_def)
  def_mod_annot_data <- extract_model_annotations_data(def_mod)
  
  testthat::expect_type(extract_model_annotations_data(def_mod), "list")
  testthat::expect_length(extract_model_annotations_data(def_mod), length(model_details_col_order()))
  testthat::expect_identical(nrow(extract_model_annotations_data(def_mod)), 31L)
  testthat::expect_identical(nrow(extract_model_annotations_data(def_mod) %>% filter(block %in% "PARAM")), n_params_def)
  testthat::expect_named(extract_model_annotations_data(def_mod), model_details_col_order())
  testthat::expect_type(extract_model_annotations_data(def_mod)$block,  "character")
  testthat::expect_type(extract_model_annotations_data(def_mod)$name,   "character")
  testthat::expect_type(extract_model_annotations_data(def_mod)$value,  "double")
  testthat::expect_type(extract_model_annotations_data(def_mod)$unit,   "character")
  testthat::expect_type(extract_model_annotations_data(def_mod)$descr,   "character")
  testthat::expect_type(extract_model_annotations_data(def_mod)$options, "character")
  testthat::expect_identical(extract_model_annotations_data(def_mod)$options, rep(".", times = nrow(extract_model_annotations_data(def_mod))))
})


### extract_model_param_annot_data() -----------------------------------------------------------
testthat::test_that("extract_model_param_annot_data returns data.frame of model parameter annotation data", {
  
  ## model with 6 parameters
  param_value_6par <- unlist(as.list(mrgsolve::param(mod_6par)))
  param_names_6par <- names(param_value_6par)
  n_params_6par <- length(param_names_6par)
  testthat::expect_type(extract_model_param_annot_data(mod_6par), "list")
  testthat::expect_length(extract_model_param_annot_data(mod_6par), length(model_details_col_order()))
  testthat::expect_identical(nrow(extract_model_param_annot_data(mod_6par)), n_params_6par)
  testthat::expect_named(extract_model_param_annot_data(mod_6par), model_details_col_order())
  testthat::expect_identical(extract_model_param_annot_data(mod_6par)$block, rep("PARAM", times = n_params_6par))
  testthat::expect_identical(extract_model_param_annot_data(mod_6par)$name, param_names_6par)
  testthat::expect_identical(extract_model_param_annot_data(mod_6par)$value, param_value_6par)
  testthat::expect_identical(extract_model_param_annot_data(mod_6par)$unit, rep(".", times = n_params_6par))
  testthat::expect_identical(extract_model_param_annot_data(mod_6par)$descr, rep(".", times = n_params_6par))
  testthat::expect_identical(extract_model_param_annot_data(mod_6par)$options, rep(".", times = n_params_6par))
  
  ## default library model
  def_mod <- app_libmod_objs[[fetch_default_libmod_name()]]
  def_mod_lst <- as.list(def_mod)
  def_mod_code <- def_mod_lst$code
  param_value_def <- unlist(as.list(mrgsolve::param(def_mod)))
  param_names_def <- names(param_value_def)
  n_params_def <- length(param_names_def)
  def_mod_annot_data <- extract_model_param_annot_data(def_mod)
  
  testthat::expect_type(extract_model_param_annot_data(def_mod), "list")
  testthat::expect_length(extract_model_param_annot_data(def_mod), length(model_details_col_order()))
  testthat::expect_identical(nrow(extract_model_param_annot_data(def_mod)), n_params_def)
  testthat::expect_identical(nrow(extract_model_param_annot_data(def_mod) %>% filter(block %in% "PARAM")), n_params_def)
  testthat::expect_named(extract_model_param_annot_data(def_mod), model_details_col_order())
  
  testthat::expect_type(extract_model_param_annot_data(def_mod)$block, "character")
  testthat::expect_identical(extract_model_param_annot_data(def_mod)$block, rep("PARAM", times = n_params_def))
  
  testthat::expect_type(extract_model_param_annot_data(def_mod)$name, "character")
  testthat::expect_identical(extract_model_param_annot_data(def_mod)$name, param_names_def)
  
  testthat::expect_type(extract_model_param_annot_data(def_mod)$value, "double")
  testthat::expect_identical(extract_model_param_annot_data(def_mod)$value,  param_value_def)
  
  testthat::expect_type(extract_model_param_annot_data(def_mod)$unit, "character")
  testthat::expect_identical(extract_model_param_annot_data(def_mod)$unit, c("L/day", "L", "L/day", "L", "1/day", "unitless", "mg/day", "mg/L", "mg", "kg", "kg", "unitless", "unitless"))
  
  testthat::expect_type(extract_model_param_annot_data(def_mod)$descr, "character")
  testthat::expect_identical(extract_model_param_annot_data(def_mod)$descr, c("Clearance", "Central volume of distribution", "Inter-compartmental clearance", 
                                                                              "Peripheral volume of distribution", "Absorption rate constant", 
                                                                              "Bioavailability", "Maximum MM reaction velocity", "Michaelis-Menten (MM) constant", 
                                                                              "Dose amount", "Baseline subject body weight", 
                                                                              "Reference body weight", "Power law exponent for body weight on CL", 
                                                                              "Power law exponent for body weight on V2"))
  
  testthat::expect_type(extract_model_param_annot_data(def_mod)$options,"character")
  testthat::expect_identical(extract_model_param_annot_data(def_mod)$options, rep(".", times = nrow(extract_model_param_annot_data(def_mod))))
})

    
### list_S4_slots() -----------------------------------------------------------
testthat::test_that("list_S4_slots returns a named list of all defined slots & values for the input s4 class", {
  mrgmod_slot_class <- getSlots("mrgmod")
  mrgmod_slot_names <- slotNames("mrgmod")
  mrgmod_slot_length <- length(mrgmod_slot_names)
  
  ## model with 6 parameters
  testthat::expect_type(list_S4_slots(mod_6par), "list")
  testthat::expect_length(list_S4_slots(mod_6par), mrgmod_slot_length)
  testthat::expect_named(list_S4_slots(mod_6par), mrgmod_slot_names)
  testthat::expect_identical(purrr::map_chr(list_S4_slots(mod_6par), class), mrgmod_slot_class)
  testthat::expect_identical(list_S4_slots(mod_6par), set_names(map(slotNames(mod_6par), ~ slot(mod_6par, .)), slotNames(mod_6par)))
  
  
  ## default library model
  def_mod <- app_libmod_objs[[fetch_default_libmod_name()]]
  testthat::expect_type(list_S4_slots(def_mod), "list")
  testthat::expect_length(list_S4_slots(def_mod), mrgmod_slot_length)
  testthat::expect_named(list_S4_slots(def_mod), mrgmod_slot_names)
  testthat::expect_identical(purrr::map_chr(list_S4_slots(def_mod), class), mrgmod_slot_class)
  testthat::expect_identical(list_S4_slots(def_mod), set_names(map(slotNames(def_mod), ~ slot(def_mod, .)), slotNames(def_mod)))
  
  ## time grid S4 object
  tgrid_slot_class <- getSlots("tgrid")
  tgrid_slot_names <- slotNames("tgrid")
  tgrid_slot_length <- length(tgrid_slot_names)
  tg_def <- mrgsolve::tgrid()
  
  testthat::expect_type(list_S4_slots(tg_def), "list")
  testthat::expect_length(list_S4_slots(tg_def), tgrid_slot_length)
  testthat::expect_named(list_S4_slots(tg_def), tgrid_slot_names)
  testthat::expect_identical(purrr::map_chr(list_S4_slots(tg_def), class), tgrid_slot_class)
})


### mod_has_fixed_block() -----------------------------------------------------------
testthat::test_that("mod_has_fixed_block returns TRUE if model has `$FIXED` block and FALSE otherwise", {
  
  ## model with 6 parameters
  testthat::expect_type(mod_has_fixed_block(mod_6par), "logical")
  testthat::expect_length(mod_has_fixed_block(mod_6par), 1)
  testthat::expect_identical(mod_has_fixed_block(mod_6par), FALSE)
  
  ## model with 6 parameters (1 in fixed block)
  testthat::expect_type(mod_has_fixed_block(mod_6par_fixed), "logical")
  testthat::expect_length(mod_has_fixed_block(mod_6par_fixed), 1)
  testthat::expect_identical(mod_has_fixed_block(mod_6par_fixed), TRUE)
  
  ## test errors
  testthat::expect_error(mod_has_fixed_block(mod = NULL))
  testthat::expect_error(mod_has_fixed_block(), regexp = 'argument "mod" is missing, with no default')
})



### mod_has_annotations() -----------------------------------------------------------
testthat::test_that("mod_has_annotations returns TRUE if model has block annotations and FALSE otherwise", {
  
  ## model with 2 parameters
  testthat::expect_type(mod_has_annotations(mod_2par), "logical")
  testthat::expect_length(mod_has_annotations(mod_2par), 1)
  testthat::expect_identical(mod_has_annotations(mod_2par), FALSE)
  
  ## model with 6 parameters (1 in fixed block)
  testthat::expect_type(mod_has_annotations(mod_2par_annot), "logical")
  testthat::expect_length(mod_has_annotations(mod_2par_annot), 1)
  testthat::expect_identical(mod_has_annotations(mod_2par_annot), TRUE)
  
  ## test errors
  testthat::expect_error(mod_has_annotations(mod = NULL))
  testthat::expect_error(mod_has_annotations(), regexp = 'argument "mod" is missing, with no default')
})


### mod_has_fixed_block_no_annot() -----------------------------------------------------------
testthat::test_that("mod_has_fixed_block_no_annot returns TRUE if model has a `$FIXED` block and is devoid of any block annotations and FALSE otherwise", {

  ## model with 2 parameters without annotations
  testthat::expect_type(mod_has_fixed_block_no_annot(mod_2par), "logical")
  testthat::expect_length(mod_has_fixed_block_no_annot(mod_2par), 1)
  testthat::expect_identical(mod_has_fixed_block_no_annot(mod_2par), FALSE)
  
  ## model with 2 parameters (1 fixed) without annotations
  testthat::expect_type(mod_has_fixed_block_no_annot(mod_2par_fixed), "logical")
  testthat::expect_length(mod_has_fixed_block_no_annot(mod_2par_fixed), 1)
  testthat::expect_identical(mod_has_fixed_block_no_annot(mod_2par_fixed), TRUE)

  ## model with 2 parameters with annotations
  testthat::expect_type(mod_has_fixed_block_no_annot(mod_2par_annot), "logical")
  testthat::expect_length(mod_has_fixed_block_no_annot(mod_2par_annot), 1)
  testthat::expect_identical(mod_has_fixed_block_no_annot(mod_2par_annot), FALSE)

  ## model with 2 parameters (1 fixed) with annotations
  testthat::expect_type(mod_has_fixed_block_no_annot(mod_2par_fixed_annot), "logical")
  testthat::expect_length(mod_has_fixed_block_no_annot(mod_2par_fixed_annot), 1)
  testthat::expect_identical(mod_has_fixed_block_no_annot(mod_2par_fixed_annot), FALSE)
  
  ## test errors
  testthat::expect_error(mod_has_fixed_block_no_annot(mod = NULL))
  testthat::expect_error(mod_has_fixed_block_no_annot(), regexp = 'argument "mod" is missing, with no default')
})


### is_invalid_block_combo() -----------------------------------------------------------
testthat::test_that("is_invalid_block_combo returns TRUE if model has a `$FIXED` block and is devoid of any block annotations and FALSE otherwise", {
  
  ## model with 2 parameters without annotations
  testthat::expect_type(is_invalid_block_combo(mod_2par), "logical")
  testthat::expect_length(is_invalid_block_combo(mod_2par), 1)
  testthat::expect_identical(is_invalid_block_combo(mod_2par), FALSE)
  
  ## model with 2 parameters (1 fixed) without annotations
  testthat::expect_type(is_invalid_block_combo(mod_2par_fixed), "logical")
  testthat::expect_length(is_invalid_block_combo(mod_2par_fixed), 1)
  testthat::expect_identical(is_invalid_block_combo(mod_2par_fixed), FALSE)
  
  ## model with 2 parameters with annotations
  testthat::expect_type(is_invalid_block_combo(mod_2par_annot), "logical")
  testthat::expect_length(is_invalid_block_combo(mod_2par_annot), 1)
  testthat::expect_identical(is_invalid_block_combo(mod_2par_annot), FALSE)
  
  ## model with 2 parameters (1 fixed) with annotations
  testthat::expect_type(is_invalid_block_combo(mod_2par_fixed_annot), "logical")
  testthat::expect_length(is_invalid_block_combo(mod_2par_fixed_annot), 1)
  testthat::expect_identical(is_invalid_block_combo(mod_2par_fixed_annot), FALSE)
  
  ## test errors
  testthat::expect_error(is_invalid_block_combo(mod = NULL))
  testthat::expect_error(is_invalid_block_combo(), regexp = 'argument "mod" is missing, with no default')
})


#######################################################################################################################|
## Collapse OMEGA/SIGMA matrices to full matrix by default for all models ----------------------------------------------
#######################################################################################################################|

### collapse_re() -----------------------------------------------------------
testthat::test_that("collapse_re irreversibly collapses multi-block matrices into a single matrix block for both OMEGA and SIGMA matrices", {
  
  mod_full_block_re_collapse <- collapse_re(mod_full_block_re, show_matrices = FALSE)
  
  testthat::expect_s4_class(collapse_re(mod_full_block_re, show_matrices = FALSE), "mrgmod")
  testthat::expect_length(collapse_re(mod_full_block_re, show_matrices = FALSE), 1)
  testthat::expect_identical(as.matrix(mrgsolve::revar(mod_full_block_re_collapse)$omega), as.matrix(mrgsolve::revar(mod_full_block_re)$omega))
  testthat::expect_identical(as.matrix(mrgsolve::revar(mod_full_block_re_collapse)$sigma), as.matrix(mrgsolve::revar(mod_full_block_re)$sigma))
  
  mod_multi_block_re_collapse <- collapse_re(mod_multi_block_re, show_matrices = FALSE)
  
  
  testthat::expect_s4_class(collapse_re(mod_multi_block_re, show_matrices = FALSE), "mrgmod")
  testthat::expect_length(collapse_re(mod_multi_block_re, show_matrices = FALSE), 1)
  testthat::expect_identical(revar(mod_multi_block_re_collapse), revar(mod_full_block_re_collapse))
  
  testthat::expect_identical(as.matrix(mrgsolve::revar(mod_full_block_re_collapse)$omega), as.matrix(mrgsolve::revar(mod_full_block_re)$omega))
  testthat::expect_identical(as.matrix(mrgsolve::revar(mod_full_block_re_collapse)$sigma), as.matrix(mrgsolve::revar(mod_full_block_re)$sigma))
  
  
  testthat::expect_s4_class(omat(mod_multi_block_re_collapse), "omegalist")
  testthat::expect_length(omat(mod_multi_block_re_collapse), 1)
  testthat::expect_identical(omat(mod_multi_block_re_collapse), 
                             new("omegalist", data = list(`...` = structure(c(1, 0, 0, 0, 0, 
                                                                            0, 0, 2, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 
                                                                            0, 0, 5, 0, 0, 0, 0, 0, 0, 6), .Dim = c(6L, 6L))), n = 6L, labels = list(
                                                                              c("a", "b", "c", "d", "e", "f"))))

  
  testthat::expect_s4_class(smat(mod_multi_block_re_collapse), "sigmalist")
  testthat::expect_length(smat(mod_multi_block_re_collapse), 1)
  testthat::expect_identical(smat(mod_multi_block_re_collapse), 
                             new("sigmalist", data = list(`...` = structure(c(1, 0, 0, 0, 0, 
                                                                            0, 0, 2, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 
                                                                            0, 0, 5, 0, 0, 0, 0, 0, 0, 6), .Dim = c(6L, 6L))), n = 6L, labels = list(
                                                                              c("g", "h", "i", "j", "k", "l"))))
  
  
  ## test for printed output
  testthat::expect_output(collapse_re(mod_multi_block_re, show_matrices = TRUE),  regexp = NULL)
  testthat::expect_output(collapse_re(mod_multi_block_re, show_matrices = FALSE), regexp = NA)
})


### count_re_matrix_blocks() -----------------------------------------------------------
testthat::test_that("count_re_matrix_blocks", {
  
  ## model with 1 OMEGA & 1 SIGMA blocks
  testthat::expect_type(count_re_matrix_blocks(mod_full_block_re), "list")
  testthat::expect_length(count_re_matrix_blocks(mod_full_block_re), 2)
  testthat::expect_named(count_re_matrix_blocks(mod_full_block_re), c("omega", "sigma"))
  testthat::expect_type(unlist(count_re_matrix_blocks(mod_full_block_re)), "integer")
  testthat::expect_length(unlist(count_re_matrix_blocks(mod_full_block_re)), 2)
  testthat::expect_identical(count_re_matrix_blocks(mod_full_block_re)$omega, 1L)
  testthat::expect_identical(count_re_matrix_blocks(mod_full_block_re)$sigma, 1L)
  testthat::expect_identical(count_re_matrix_blocks(mod_full_block_re), list(omega = 1L, sigma = 1L))

  ## model with 3 OMEGA & 3 SIGMA blocks
  testthat::expect_type(count_re_matrix_blocks(mod_multi_block_re), "list")
  testthat::expect_length(count_re_matrix_blocks(mod_multi_block_re), 2)
  testthat::expect_named(count_re_matrix_blocks(mod_multi_block_re), c("omega", "sigma"))
  testthat::expect_type(unlist(count_re_matrix_blocks(mod_multi_block_re)), "integer")
  testthat::expect_length(unlist(count_re_matrix_blocks(mod_multi_block_re)), 2)
  testthat::expect_identical(count_re_matrix_blocks(mod_multi_block_re)$omega, 3L)
  testthat::expect_identical(count_re_matrix_blocks(mod_multi_block_re)$sigma, 3L)
  testthat::expect_identical(count_re_matrix_blocks(mod_multi_block_re), list(omega = 3L, sigma = 3L))
  
  ## model with 0 OMEGA & 0 SIGMA blocks
  testthat::expect_type(count_re_matrix_blocks(mod_no_re_blocks), "list")
  testthat::expect_length(count_re_matrix_blocks(mod_no_re_blocks), 2)
  testthat::expect_named(count_re_matrix_blocks(mod_no_re_blocks), c("omega", "sigma"))
  testthat::expect_type(unlist(count_re_matrix_blocks(mod_no_re_blocks)), "integer")
  testthat::expect_length(unlist(count_re_matrix_blocks(mod_no_re_blocks)), 2)
  testthat::expect_identical(count_re_matrix_blocks(mod_no_re_blocks)$omega, 0L)
  testthat::expect_identical(count_re_matrix_blocks(mod_no_re_blocks)$sigma, 0L)
  testthat::expect_identical(count_re_matrix_blocks(mod_no_re_blocks), list(omega = 0L, sigma = 0L))
})


#######################################################################################################################|
## Extract OMEGA/SIGMA matrices & convert to correlation form ----------------------------------------------------------
#######################################################################################################################|

### extract_model_re_matrices() -----------------------------------------------------------
testthat::test_that("extract_model_re_matrices", {
  
  re_matrix_result <- matrix(0, nrow = 6L, ncol = 6L)
  diag(re_matrix_result) <- 1:6

  testthat::expect_type(extract_model_re_matrices(mod_full_block_re), "list")
  testthat::expect_length(extract_model_re_matrices(mod_full_block_re), 2)
  testthat::expect_named(extract_model_re_matrices(mod_full_block_re), c("omega", "sigma"))
  testthat::expect_type(extract_model_re_matrices(mod_full_block_re)$omega, "double")
  testthat::expect_type(extract_model_re_matrices(mod_full_block_re)$sigma, "double")
  testthat::expect_identical(dim(extract_model_re_matrices(mod_full_block_re)$omega), c(6L, 6L))
  testthat::expect_identical(dim(extract_model_re_matrices(mod_full_block_re)$sigma), c(6L, 6L))
  testthat::expect_identical(extract_model_re_matrices(mod_full_block_re)$omega, re_matrix_result)
  testthat::expect_identical(extract_model_re_matrices(mod_full_block_re)$sigma, re_matrix_result)
  
  testthat::expect_type(extract_model_re_matrices(mod_omega_block_only), "list")
  testthat::expect_length(extract_model_re_matrices(mod_omega_block_only), 2)
  testthat::expect_named(extract_model_re_matrices(mod_omega_block_only), c("omega", "sigma"))
  testthat::expect_type(extract_model_re_matrices(mod_omega_block_only)$omega, "double")
  testthat::expect_type(extract_model_re_matrices(mod_omega_block_only)$sigma, "logical")
  testthat::expect_identical(dim(extract_model_re_matrices(mod_omega_block_only)$omega), c(6L, 6L))
  testthat::expect_identical(dim(extract_model_re_matrices(mod_omega_block_only)$sigma), c(0L, 0L))
  testthat::expect_identical(extract_model_re_matrices(mod_omega_block_only)$omega, re_matrix_result)
  testthat::expect_identical(extract_model_re_matrices(mod_omega_block_only)$sigma, matrix(NA, 0, 0))
  
  testthat::expect_type(extract_model_re_matrices(mod_6par), "list")
  testthat::expect_length(extract_model_re_matrices(mod_6par), 2)
  testthat::expect_named(extract_model_re_matrices(mod_6par), c("omega", "sigma"))
  testthat::expect_type(extract_model_re_matrices(mod_6par)$omega, "logical")
  testthat::expect_type(extract_model_re_matrices(mod_6par)$sigma, "logical")
  testthat::expect_identical(dim(extract_model_re_matrices(mod_6par)$omega), c(0L, 0L))
  testthat::expect_identical(dim(extract_model_re_matrices(mod_6par)$sigma), c(0L, 0L))
  testthat::expect_identical(extract_model_re_matrices(mod_6par)$omega, matrix(NA, 0, 0))
  testthat::expect_identical(extract_model_re_matrices(mod_6par)$sigma, matrix(NA, 0, 0))
})


### extract_model_re_labels() -----------------------------------------------------------
testthat::test_that("extract_model_re_labels", {
  
  re_matrix_result <- matrix(0, nrow = 6L, ncol = 6L)
  diag(re_matrix_result) <- 1:6
  dimnames(re_matrix_result) <- list(paste0(1:6, ": "), NULL)
  
  testthat::expect_type(extract_model_re_labels(mod_full_block_re), "list")
  testthat::expect_length(extract_model_re_labels(mod_full_block_re), 2)
  testthat::expect_named(extract_model_re_labels(mod_full_block_re), c("omega", "sigma"))
  testthat::expect_type(extract_model_re_labels(mod_full_block_re)$omega, "character")
  testthat::expect_type(extract_model_re_labels(mod_full_block_re)$sigma, "character")
  testthat::expect_length(extract_model_re_labels(mod_full_block_re)$omega, 6)
  testthat::expect_length(extract_model_re_labels(mod_full_block_re)$sigma, 6)
  testthat::expect_identical(extract_model_re_labels(mod_full_block_re)$omega, c("a", "b", "c", "d", "e", "f"))
  testthat::expect_identical(extract_model_re_labels(mod_full_block_re)$sigma, c("g", "h", "i", "j", "k", "l"))
  
  testthat::expect_type(extract_model_re_labels(mod_omega_block_only), "list")
  testthat::expect_length(extract_model_re_labels(mod_omega_block_only), 2)
  testthat::expect_named(extract_model_re_labels(mod_omega_block_only), c("omega", "sigma"))
  testthat::expect_type(extract_model_re_labels(mod_omega_block_only)$omega, "character")
  testthat::expect_type(extract_model_re_labels(mod_omega_block_only)$sigma, "NULL")
  testthat::expect_length(extract_model_re_labels(mod_omega_block_only)$omega, 6)
  testthat::expect_length(extract_model_re_labels(mod_omega_block_only)$sigma, 0)
  testthat::expect_identical(extract_model_re_labels(mod_omega_block_only)$omega, c("a", "b", "c", "d", "e", "f"))
  testthat::expect_identical(extract_model_re_labels(mod_omega_block_only)$sigma, NULL)
  
  testthat::expect_type(extract_model_re_labels(mod_6par), "list")
  testthat::expect_length(extract_model_re_labels(mod_6par), 2)
  testthat::expect_named(extract_model_re_labels(mod_6par), c("omega", "sigma"))
  testthat::expect_type(extract_model_re_labels(mod_6par)$omega, "NULL")
  testthat::expect_type(extract_model_re_labels(mod_6par)$sigma, "NULL")
  testthat::expect_length(extract_model_re_labels(mod_6par)$omega, 0)
  testthat::expect_length(extract_model_re_labels(mod_6par)$sigma, 0)
  testthat::expect_identical(extract_model_re_labels(mod_6par)$omega, NULL)
  testthat::expect_identical(extract_model_re_labels(mod_6par)$sigma, NULL)
})


### label_re_matrix() -----------------------------------------------------------
testthat::test_that("label_re_matrix", {
  
  re_matrix_result <- matrix(0, nrow = 6L, ncol = 6L)
  diag(re_matrix_result) <- 1:6
  om_matrix_result <- re_matrix_result
  sg_matrix_result <- re_matrix_result
  dimnames(om_matrix_result) <- list(c("a", "b", "c", "d", "e", "f"), c("a", "b", "c", "d", "e", "f"))
  dimnames(sg_matrix_result) <- list(c("g", "h", "i", "j", "k", "l"), c("g", "h", "i", "j", "k", "l"))
  
  empty_matrix_result <- matrix(NA, nrow = 0, ncol = 0, dimnames = list(NULL, NULL))
  
  mat_full_block_re <- extract_model_re_matrices(mod_full_block_re)
  lab_full_block_re <- extract_model_re_labels(mod_full_block_re)
  res_full_block_re <- purrr::map2(mat_full_block_re, lab_full_block_re, label_re_matrix)
  
  testthat::expect_type(res_full_block_re, "list")
  testthat::expect_length(res_full_block_re, 2)
  testthat::expect_named(res_full_block_re, c("omega", "sigma"))
  testthat::expect_type(res_full_block_re$omega, "double")
  testthat::expect_type(res_full_block_re$sigma, "double")
  testthat::expect_identical(dim(res_full_block_re$omega), c(6L, 6L))
  testthat::expect_identical(dim(res_full_block_re$sigma), c(6L, 6L))
  testthat::expect_identical(dimnames(res_full_block_re$omega), list(c("a", "b", "c", "d", "e", "f"), c("a", "b", "c", "d", "e", "f")))
  testthat::expect_identical(dimnames(res_full_block_re$sigma), list(c("g", "h", "i", "j", "k", "l"), c("g", "h", "i", "j", "k", "l")))
  testthat::expect_identical(res_full_block_re$omega, om_matrix_result)
  testthat::expect_identical(res_full_block_re$sigma, sg_matrix_result)
  
  mat_omega_block_only <- extract_model_re_matrices(mod_omega_block_only)
  lab_omega_block_only <- extract_model_re_labels(mod_omega_block_only)
  res_omega_block_only <- purrr::map2(mat_omega_block_only, lab_omega_block_only, label_re_matrix)
  
  testthat::expect_type(res_omega_block_only, "list")
  testthat::expect_length(res_omega_block_only, 2)
  testthat::expect_named(res_omega_block_only, c("omega", "sigma"))
  testthat::expect_type(res_omega_block_only$omega, "double")
  testthat::expect_type(res_omega_block_only$sigma, "logical")
  testthat::expect_identical(dim(res_omega_block_only$omega), c(6L, 6L))
  testthat::expect_identical(dim(res_omega_block_only$sigma), c(0L, 0L))
  testthat::expect_identical(dimnames(res_omega_block_only$omega), list(c("a", "b", "c", "d", "e", "f"), c("a", "b", "c", "d", "e", "f")))
  testthat::expect_identical(dimnames(res_omega_block_only$sigma), list(NULL, NULL))
  testthat::expect_identical(res_omega_block_only$omega, om_matrix_result)
  testthat::expect_identical(res_omega_block_only$sigma, matrix(NA, nrow = 0, ncol = 0, dimnames = list(NULL, NULL)))

  
  mat_6par <- extract_model_re_matrices(mod_6par)
  lab_6par <- extract_model_re_labels(mod_6par)
  res_6par <- purrr::map2(mat_6par, lab_6par, label_re_matrix)
  
  
  testthat::expect_type(res_6par, "list")
  testthat::expect_length(res_6par, 2)
  testthat::expect_named(res_6par, c("omega", "sigma"))
  testthat::expect_type(res_6par$omega, "logical")
  testthat::expect_type(res_6par$sigma, "logical")
  testthat::expect_identical(dim(res_6par$omega), c(0L, 0L))
  testthat::expect_identical(dim(res_6par$sigma), c(0L, 0L))
  testthat::expect_identical(dimnames(res_6par$omega), list(NULL, NULL))
  testthat::expect_identical(dimnames(res_6par$sigma), list(NULL, NULL))
  testthat::expect_identical(res_6par$omega, matrix(NA, nrow = 0, ncol = 0, dimnames = list(NULL, NULL)))
  testthat::expect_identical(res_6par$sigma, matrix(NA, nrow = 0, ncol = 0, dimnames = list(NULL, NULL)))
})


### fetch_valid_matrix_types() -----------------------------------------------------------
testthat::test_that("fetch_valid_matrix_types", {
  testthat::expect_type(fetch_valid_matrix_types(), "character")
  testthat::expect_length(fetch_valid_matrix_types(), 2)
  testthat::expect_identical(fetch_valid_matrix_types(), c("omega", "sigma"))
})


### validate_matrix_type() -----------------------------------------------------------
testthat::test_that("validate_matrix_type", {
  matrix_types <- fetch_valid_matrix_types()
  
  testthat::expect_type(validate_matrix_type("omega"), "character")
  testthat::expect_length(validate_matrix_type("omega"), 1)
  testthat::expect_identical(validate_matrix_type("omega"), "omega")
  testthat::expect_identical(validate_matrix_type("OMEGA"), "omega")
  
  testthat::expect_type(validate_matrix_type("sigma"), "character")
  testthat::expect_length(validate_matrix_type("sigma"), 1)
  testthat::expect_identical(validate_matrix_type("sigma"), "sigma")
  testthat::expect_identical(validate_matrix_type("SIGMA"), "sigma")
  
  testthat::expect_type(validate_matrix_type(fetch_valid_matrix_types()), "character")
  testthat::expect_length(validate_matrix_type(fetch_valid_matrix_types()), length(fetch_valid_matrix_types()))
  testthat::expect_identical(validate_matrix_type(fetch_valid_matrix_types()), fetch_valid_matrix_types())
  testthat::expect_identical(validate_matrix_type(toupper(fetch_valid_matrix_types())), fetch_valid_matrix_types())
  
  testthat::expect_error(validate_matrix_type(), regexp = 'argument "matrix_type" is missing, with no default')
  testthat::expect_error(validate_matrix_type("om"))
  testthat::expect_error(validate_matrix_type("omeg"))
  testthat::expect_error(validate_matrix_type("sig"))
  testthat::expect_error(validate_matrix_type(c("theta", fetch_valid_matrix_types())))
})


### label_model_re_matrices() -----------------------------------------------------------
testthat::test_that("label_model_re_matrices", {
  
  re_matrix_result <- matrix(0, nrow = 6L, ncol = 6L)
  diag(re_matrix_result) <- 1:6
  om_matrix_result <- re_matrix_result
  sg_matrix_result <- re_matrix_result
  dimnames(om_matrix_result) <- list(c("a", "b", "c", "d", "e", "f"), c("a", "b", "c", "d", "e", "f"))
  dimnames(sg_matrix_result) <- list(c("g", "h", "i", "j", "k", "l"), c("g", "h", "i", "j", "k", "l"))
  
  empty_matrix_result <- matrix(NA, nrow = 0, ncol = 0, dimnames = list(NULL, NULL))
  
  ## full block omega & sigma random effect matrices
  res_full_block_re <- label_model_re_matrices(mod_full_block_re, matrix_type = c("omega", "sigma"))
  
  testthat::expect_type(res_full_block_re, "list")
  testthat::expect_length(res_full_block_re, 2)
  testthat::expect_named(res_full_block_re, c("omega", "sigma"))
  testthat::expect_type(res_full_block_re$omega, "double")
  testthat::expect_type(res_full_block_re$sigma, "double")
  testthat::expect_identical(dim(res_full_block_re$omega), c(6L, 6L))
  testthat::expect_identical(dim(res_full_block_re$sigma), c(6L, 6L))
  testthat::expect_identical(dimnames(res_full_block_re$omega), list(c("a", "b", "c", "d", "e", "f"), c("a", "b", "c", "d", "e", "f")))
  testthat::expect_identical(dimnames(res_full_block_re$sigma), list(c("g", "h", "i", "j", "k", "l"), c("g", "h", "i", "j", "k", "l")))
  testthat::expect_identical(res_full_block_re$omega, om_matrix_result)
  testthat::expect_identical(res_full_block_re$sigma, sg_matrix_result)
  
  ## label omega matrix only
  res_full_block_omega_re <- label_model_re_matrices(mod_full_block_re, matrix_type = c("omega"))
  testthat::expect_type(res_full_block_omega_re, "list")
  testthat::expect_length(res_full_block_omega_re, 1)
  testthat::expect_named(res_full_block_omega_re, "omega")
  testthat::expect_type(res_full_block_omega_re$omega, "double")
  testthat::expect_identical(dim(res_full_block_omega_re$omega), c(6L, 6L))
  testthat::expect_identical(dimnames(res_full_block_omega_re$omega), list(c("a", "b", "c", "d", "e", "f"), c("a", "b", "c", "d", "e", "f")))
  testthat::expect_identical(res_full_block_omega_re$omega, om_matrix_result)
  
  
  ## label sigma matrix only
  res_full_block_sigma_re <- label_model_re_matrices(mod_full_block_re, matrix_type = c("sigma"))
  testthat::expect_type(res_full_block_sigma_re, "list")
  testthat::expect_length(res_full_block_sigma_re, 1)
  testthat::expect_named(res_full_block_sigma_re, "sigma")
  testthat::expect_type(res_full_block_sigma_re$sigma, "double")
  testthat::expect_identical(dim(res_full_block_sigma_re$sigma), c(6L, 6L))
  testthat::expect_identical(dimnames(res_full_block_sigma_re$sigma), list(c("g", "h", "i", "j", "k", "l"), c("g", "h", "i", "j", "k", "l")))
  testthat::expect_identical(res_full_block_sigma_re$sigma, sg_matrix_result)
  
  
  
  ## model with omega block only
  res_omega_block_only <- label_model_re_matrices(mod_omega_block_only, matrix_type = c("omega", "sigma"))
  testthat::expect_type(res_omega_block_only, "list")
  testthat::expect_length(res_omega_block_only, 2)
  testthat::expect_named(res_omega_block_only, c("omega", "sigma"))
  testthat::expect_type(res_omega_block_only$omega, "double")
  testthat::expect_type(res_omega_block_only$sigma, "logical")
  testthat::expect_identical(dim(res_omega_block_only$omega), c(6L, 6L))
  testthat::expect_identical(dim(res_omega_block_only$sigma), c(0L, 0L))
  testthat::expect_identical(dimnames(res_omega_block_only$omega), list(c("a", "b", "c", "d", "e", "f"), c("a", "b", "c", "d", "e", "f")))
  testthat::expect_identical(dimnames(res_omega_block_only$sigma), list(NULL, NULL))
  testthat::expect_identical(res_omega_block_only$omega, om_matrix_result)
  testthat::expect_identical(res_omega_block_only$sigma, matrix(NA, nrow = 0, ncol = 0, dimnames = list(NULL, NULL)))
  
  ## model without random effect matrices
  res_6par <- label_model_re_matrices(mod_6par, matrix_type = c("omega", "sigma"))
  testthat::expect_type(res_6par, "list")
  testthat::expect_length(res_6par, 2)
  testthat::expect_named(res_6par, c("omega", "sigma"))
  testthat::expect_type(res_6par$omega, "logical")
  testthat::expect_type(res_6par$sigma, "logical")
  testthat::expect_identical(dim(res_6par$omega), c(0L, 0L))
  testthat::expect_identical(dim(res_6par$sigma), c(0L, 0L))
  testthat::expect_identical(dimnames(res_6par$omega), list(NULL, NULL))
  testthat::expect_identical(dimnames(res_6par$sigma), list(NULL, NULL))
  testthat::expect_identical(res_6par$omega, matrix(NA, nrow = 0, ncol = 0, dimnames = list(NULL, NULL)))
  testthat::expect_identical(res_6par$sigma, matrix(NA, nrow = 0, ncol = 0, dimnames = list(NULL, NULL)))
})


### convert_model_re_matrices_to_corr() -----------------------------------------------------------
testthat::test_that("convert_model_re_matrices_to_corr", {
  
  re_matrix_result <- matrix(0, nrow = 6L, ncol = 6L)
  diag(re_matrix_result) <- 1:6
  om_matrix_result <- re_matrix_result
  sg_matrix_result <- re_matrix_result
  dimnames(om_matrix_result) <- list(c("a", "b", "c", "d", "e", "f"), c("a", "b", "c", "d", "e", "f"))
  dimnames(sg_matrix_result) <- list(c("g", "h", "i", "j", "k", "l"), c("g", "h", "i", "j", "k", "l"))
  
  empty_matrix_result <- matrix(NA, nrow = 0, ncol = 0, dimnames = list(NULL, NULL))
  
  ## full block omega & sigma random effect matrices
  res_full_block_re <- convert_model_re_matrices_to_corr(mod_full_block_re, matrix_type = c("omega", "sigma"))
  
  testthat::expect_type(res_full_block_re, "list")
  testthat::expect_length(res_full_block_re, 2)
  testthat::expect_named(res_full_block_re, c("omega", "sigma"))
  testthat::expect_type(res_full_block_re$omega, "double")
  testthat::expect_type(res_full_block_re$sigma, "double")
  testthat::expect_identical(dim(res_full_block_re$omega), c(6L, 6L))
  testthat::expect_identical(dim(res_full_block_re$sigma), c(6L, 6L))
  testthat::expect_identical(dimnames(res_full_block_re$omega), list(c("a", "b", "c", "d", "e", "f"), c("a", "b", "c", "d", "e", "f")))
  testthat::expect_identical(dimnames(res_full_block_re$sigma), list(c("g", "h", "i", "j", "k", "l"), c("g", "h", "i", "j", "k", "l")))
  testthat::expect_identical(res_full_block_re$omega, om_matrix_result)
  testthat::expect_identical(res_full_block_re$sigma, sg_matrix_result)
  
  ## label omega matrix only
  res_full_block_omega_re <- convert_model_re_matrices_to_corr(mod_full_block_re, matrix_type = c("omega"))
  testthat::expect_type(res_full_block_omega_re, "list")
  testthat::expect_length(res_full_block_omega_re, 1)
  testthat::expect_named(res_full_block_omega_re, "omega")
  testthat::expect_type(res_full_block_omega_re$omega, "double")
  testthat::expect_identical(dim(res_full_block_omega_re$omega), c(6L, 6L))
  testthat::expect_identical(dimnames(res_full_block_omega_re$omega), list(c("a", "b", "c", "d", "e", "f"), c("a", "b", "c", "d", "e", "f")))
  testthat::expect_identical(res_full_block_omega_re$omega, om_matrix_result)
  
  
  ## label sigma matrix only
  res_full_block_sigma_re <- convert_model_re_matrices_to_corr(mod_full_block_re, matrix_type = c("sigma"))
  testthat::expect_type(res_full_block_sigma_re, "list")
  testthat::expect_length(res_full_block_sigma_re, 1)
  testthat::expect_named(res_full_block_sigma_re, "sigma")
  testthat::expect_type(res_full_block_sigma_re$sigma, "double")
  testthat::expect_identical(dim(res_full_block_sigma_re$sigma), c(6L, 6L))
  testthat::expect_identical(dimnames(res_full_block_sigma_re$sigma), list(c("g", "h", "i", "j", "k", "l"), c("g", "h", "i", "j", "k", "l")))
  testthat::expect_identical(res_full_block_sigma_re$sigma, sg_matrix_result)
  
  
  
  ## model with omega block only
  res_omega_block_only <- convert_model_re_matrices_to_corr(mod_omega_block_only, matrix_type = c("omega"))

  testthat::expect_type(res_omega_block_only, "list")
  testthat::expect_length(res_omega_block_only, 1)
  testthat::expect_named(res_omega_block_only, c("omega"))
  testthat::expect_type(res_omega_block_only$omega, "double")
  testthat::expect_identical(dim(res_omega_block_only$omega), c(6L, 6L))
  testthat::expect_identical(dimnames(res_omega_block_only$omega), list(c("a", "b", "c", "d", "e", "f"), c("a", "b", "c", "d", "e", "f")))
  testthat::expect_identical(res_omega_block_only$omega, om_matrix_result)

  
  ## model without random effect matrices
  
  ## test tmdd library model with off-diagonal matrix elements
  mod_tmdd <- app_libmod_objs[["tmdd_full_2cmt_1abs"]]
  lab_tmdd <- extract_model_re_labels(mod_tmdd)
  res_tmdd <- convert_model_re_matrices_to_corr(mod_tmdd, matrix_type = c("omega", "sigma"))
  res_tmdd_omat <- matrix(c(0.116, 0.7, 0, 0.7, 0.116, 0, 0, 0, 0.04), nrow = length(lab_tmdd$omega), ncol = length(lab_tmdd$omega), dimnames = list(lab_tmdd$omega, lab_tmdd$omega))
  res_tmdd_smat <- matrix(c(0.04, 0, 0, 0, 0.04, 0, 0, 0, 0), nrow = length(lab_tmdd$sigma), ncol = length(lab_tmdd$sigma), dimnames = list(lab_tmdd$sigma, lab_tmdd$sigma))
  
  testthat::expect_type(res_tmdd, "list")
  testthat::expect_length(res_tmdd, 2)
  testthat::expect_named(res_tmdd, c("omega", "sigma"))
  testthat::expect_type(res_tmdd$omega, "double")
  testthat::expect_type(res_tmdd$sigma, "double")
  testthat::expect_identical(dim(res_tmdd$omega), c(length(lab_tmdd$omega), length(lab_tmdd$omega)))
  testthat::expect_identical(dim(res_tmdd$sigma), c(length(lab_tmdd$sigma), length(lab_tmdd$sigma)))
  testthat::expect_identical(dimnames(res_tmdd$omega), list(lab_tmdd$omega, lab_tmdd$omega))
  testthat::expect_identical(dimnames(res_tmdd$sigma), list(lab_tmdd$sigma, lab_tmdd$sigma))
  testthat::expect_identical(res_tmdd$omega, res_tmdd_omat)
  testthat::expect_identical(res_tmdd$sigma, res_tmdd_smat)
  
  ## test conversion from covariance matrix to correlation matrix
  mod_tmdd_coll <- collapse_re(mod_tmdd, show_matrices = FALSE)
  mod_tmdd_full_cov <- extract_model_re_matrices(mod_tmdd_coll)
  mod_tmdd_full_cor <- map(mod_tmdd_full_cov, double_sweep_cov_to_corr_matrix)
  testthat::expect_identical(unlist(as.list(mod_tmdd_full_cor$omega)), unlist(as.list(res_tmdd_omat)))
  testthat::expect_identical(unlist(as.list(mod_tmdd_full_cor$sigma)), unlist(as.list(res_tmdd_smat)))
})


#######################################################################################################################|
## Correlation to covariance matrix conversion functions ---------------------------------------------------------------
#######################################################################################################################|

### force_symmetric_re_matrix() -----------------------------------------------------------
testthat::test_that("force_symmetric_re_matrix", {
  
  ## test 3x3 asymmetric matrix without defined dimnames
  tst_mat <- matrix(c(2, 1, 1, 0, 4, 2, 0, 0, 3), nrow = 3L, ncol = 3L)
  testthat::expect_false(isSymmetric(tst_mat))
  testthat::expect_true(isSymmetric(force_symmetric_re_matrix(tst_mat)))
  testthat::expect_type(force_symmetric_re_matrix(tst_mat), "double")
  testthat::expect_named(force_symmetric_re_matrix(tst_mat), dimnames(tst_mat))
  testthat::expect_identical(dim(force_symmetric_re_matrix(tst_mat)), c(3L, 3L))
  testthat::expect_identical(force_symmetric_re_matrix(tst_mat), matrix(c(2, 1, 1, 1, 4, 2, 1, 2, 3), nrow = 3L, ncol = 3L))
  
  ## test 3x3 asymmetric matrix with defined dimnames
  tst_mat_names <- list(c("ETA1", "ETA2", "ETA3"), c("ETA1", "ETA2", "ETA3"))
  tst_mat_named <- matrix(c(2, 1, 1, 0, 4, 2, 0, 0, 3), nrow = 3L, ncol = 3L, dimnames = tst_mat_names)
  testthat::expect_false(isSymmetric(tst_mat_named))
  testthat::expect_true(isSymmetric(force_symmetric_re_matrix(tst_mat_named)))
  testthat::expect_type(force_symmetric_re_matrix(tst_mat_named), "double")
  testthat::expect_identical(dimnames(force_symmetric_re_matrix(tst_mat_named)), dimnames(tst_mat_named))
  testthat::expect_identical(dimnames(force_symmetric_re_matrix(tst_mat_named)), tst_mat_names)
  testthat::expect_identical(dimnames(force_symmetric_re_matrix(tst_mat_named)), list(c("ETA1", "ETA2", "ETA3"), c("ETA1", "ETA2", "ETA3")))
  testthat::expect_identical(dim(force_symmetric_re_matrix(tst_mat_named)), c(3L, 3L))
  testthat::expect_identical(force_symmetric_re_matrix(tst_mat_named), matrix(c(2, 1, 1, 1, 4, 2, 1, 2, 3), nrow = 3L, ncol = 3L, dimnames = tst_mat_names))
})


### double_sweep_corr_to_cov_matrix() -----------------------------------------------------------

testthat::test_that("double_sweep_corr_to_cov_matrix", {
  
  #######################################################################################|
  ## test 3x3 asymmetric matrix with defined dimnames
  #######################################################################################|
  tst_mat_nms <- list(c("ETA1", "ETA2", "ETA3"), c("ETA1", "ETA2", "ETA3"))
  tst_mat_cov <- matrix(c(2, 1, 1, 1, 4, 2, 1, 2, 3), nrow = 3L, ncol = 3L, dimnames = tst_mat_nms)
  tst_mat_corr <- cov2cor(tst_mat_cov)
  diag(tst_mat_corr) <- diag(tst_mat_cov)
  
  testthat::expect_true(isSymmetric(tst_mat_corr))
  testthat::expect_type(double_sweep_corr_to_cov_matrix(tst_mat_corr), "double")
  testthat::expect_identical(dimnames(double_sweep_corr_to_cov_matrix(tst_mat_corr)), tst_mat_nms)
  testthat::expect_identical(dim(double_sweep_corr_to_cov_matrix(tst_mat_corr)), map_int(tst_mat_nms, length))
  testthat::expect_equal(double_sweep_corr_to_cov_matrix(tst_mat_corr), tst_mat_cov)
  
  
  #######################################################################################|
  ## test random effect matrices from a TMDD library model
  #######################################################################################|
  mod_tmdd <- app_libmod_objs[["tmdd_full_2cmt_1abs"]]
  modc_tmdd <- collapse_re(mod_tmdd, show_matrices = FALSE)
  modc_tmdd_labels <- extract_model_re_labels(modc_tmdd)
  modc_tmdd_re_mat <- extract_model_re_matrices(modc_tmdd)
  modc_tmdd_re_lbl <- label_model_re_matrices(modc_tmdd)
  modc_tmdd_re_cor <- purrr::map(modc_tmdd_re_lbl, double_sweep_cov_to_corr_matrix)
  
  ## build true OMEGA/SIGMA full block matrices to test against
  modc_tmdd_true_om_cov <- matrix(c(0.116, 0.0812, 0, 0.0812, 0.116, 0, 0, 0, 0.04), nrow = 3L, ncol = 3L, dimnames = list(c("ETA_CL", "ETA_VC", "ETA_R0"), c("ETA_CL", "ETA_VC", "ETA_R0")))
  modc_tmdd_true_om_cor <- matrix(c(0.116, 0.7, 0, 0.7, 0.116, 0, 0, 0, 0.04), nrow = 3L, ncol = 3L, dimnames = list(c("ETA_CL", "ETA_VC", "ETA_R0"), c("ETA_CL", "ETA_VC", "ETA_R0")))
  modc_tmdd_true_sg_cov <- matrix(c(0.04, 0, 0, 0, 0.04, 0, 0, 0, 0), nrow = 3L, ncol = 3L, dimnames = list(c("PROP_DRUG", "PROP_TARGET", "PROP_COMPLEX"), c("PROP_DRUG", "PROP_TARGET", "PROP_COMPLEX")))
  modc_tmdd_true_sg_cor <- matrix(c(0.04, 0, 0, 0, 0.04, 0, 0, 0, 0), nrow = 3L, ncol = 3L, dimnames = list(c("PROP_DRUG", "PROP_TARGET", "PROP_COMPLEX"), c("PROP_DRUG", "PROP_TARGET", "PROP_COMPLEX")))
  
  ## test OMEGA matrix conversion: corr --> cov
  testthat::expect_type(double_sweep_corr_to_cov_matrix(modc_tmdd_re_cor$omega), "double")
  testthat::expect_identical(dimnames(double_sweep_corr_to_cov_matrix(modc_tmdd_re_cor$omega)), list(c("ETA_CL", "ETA_VC", "ETA_R0"), c("ETA_CL", "ETA_VC", "ETA_R0")))
  testthat::expect_identical(dimnames(double_sweep_corr_to_cov_matrix(modc_tmdd_re_cor$omega)), unname(rep(modc_tmdd_labels["omega"], 2)))
  testthat::expect_identical(dim(double_sweep_corr_to_cov_matrix(modc_tmdd_re_cor$omega)), rep(length(modc_tmdd_labels$omega), 2))
  testthat::expect_identical(modc_tmdd_re_cor$omega, modc_tmdd_true_om_cor)
  testthat::expect_equal(double_sweep_corr_to_cov_matrix(modc_tmdd_re_cor$omega), modc_tmdd_true_om_cov)
  
  
  ## test SIGMA matrix conversion: corr --> cov
  testthat::expect_type(double_sweep_corr_to_cov_matrix(modc_tmdd_re_cor$sigma), "double")
  testthat::expect_identical(dimnames(double_sweep_corr_to_cov_matrix(modc_tmdd_re_cor$sigma)), list(c("PROP_DRUG", "PROP_TARGET", "PROP_COMPLEX"), c("PROP_DRUG", "PROP_TARGET", "PROP_COMPLEX")))
  testthat::expect_identical(dimnames(double_sweep_corr_to_cov_matrix(modc_tmdd_re_cor$sigma)), unname(rep(modc_tmdd_labels["sigma"], 2)))
  testthat::expect_identical(dim(double_sweep_corr_to_cov_matrix(modc_tmdd_re_cor$sigma)), rep(length(modc_tmdd_labels$sigma), 2))
  testthat::expect_identical(modc_tmdd_re_cor$sigma, modc_tmdd_true_sg_cor)
  testthat::expect_equal(double_sweep_corr_to_cov_matrix(modc_tmdd_re_cor$sigma), modc_tmdd_true_sg_cov)
  
  ## test `validate_corr` input argument
  
  ## test invalid correlation coefficients > 1
  invalid_cov_pos <- matrix(c(0.116, 0.232, 0, 0.232, 0.116, 0, 0, 0, 0.04), nrow = 3L, ncol = 3L, dimnames = list(c("ETA_CL", "ETA_VC", "ETA_R0"), c("ETA_CL", "ETA_VC", "ETA_R0")))
  invalid_cor_pos <- matrix(c(0.116, 2, 0, 2, 0.116, 0, 0, 0, 0.04), nrow = 3L, ncol = 3L, dimnames = list(c("ETA_CL", "ETA_VC", "ETA_R0"), c("ETA_CL", "ETA_VC", "ETA_R0")))
  testthat::expect_error(double_sweep_corr_to_cov_matrix(invalid_cor_pos, validate_corr = TRUE))
  testthat::expect_equal(double_sweep_corr_to_cov_matrix(invalid_cor_pos, validate_corr = FALSE), invalid_cov_pos)
  
  ## test invalid correlation coefficients < -1
  invalid_cov_neg <- matrix(c(0.116, -0.232, 0, -0.232, 0.116, 0, 0, 0, 0.04), nrow = 3L, ncol = 3L, dimnames = list(c("ETA_CL", "ETA_VC", "ETA_R0"), c("ETA_CL", "ETA_VC", "ETA_R0")))
  invalid_cor_neg <- matrix(c(0.116, -2, 0, -2, 0.116, 0, 0, 0, 0.04), nrow = 3L, ncol = 3L, dimnames = list(c("ETA_CL", "ETA_VC", "ETA_R0"), c("ETA_CL", "ETA_VC", "ETA_R0")))
  testthat::expect_error(double_sweep_corr_to_cov_matrix(invalid_cor_neg, validate_corr = TRUE))
  testthat::expect_equal(double_sweep_corr_to_cov_matrix(invalid_cor_neg, validate_corr = FALSE), invalid_cov_neg)
})


#######################################################################################################################|
## Covariance to correlation matrix conversion functions ---------------------------------------------------------------
#######################################################################################################################|

### validate_matrix_diag_GE_0() -----------------------------------------------------------

testthat::test_that("validate_matrix_diag_GE_0", {
  
  #######################################################################################|
  ## test 3x3 matrix
  #######################################################################################|
  tst_mat <- matrix(c(2, 1, 1, 1, 4, 2, 1, 2, 3), nrow = 3L, ncol = 3L)
  tst_mat_diag0 <- tst_mat
  diag(tst_mat_diag0) <- 0
  neg_mat_diag0 <- -1*tst_mat_diag0
  
  ## expect true results
  testthat::expect_true(validate_matrix_diag_GE_0(matrix(0, ncol = 1, nrow = 1), return_res = TRUE))
  testthat::expect_true(validate_matrix_diag_GE_0(matrix(0, ncol = 3, nrow = 3), return_res = TRUE))
  testthat::expect_true(validate_matrix_diag_GE_0(matrix(0, ncol = 10, nrow = 10), return_res = TRUE))
  testthat::expect_true(validate_matrix_diag_GE_0(tst_mat, return_res = TRUE))
  testthat::expect_true(validate_matrix_diag_GE_0(tst_mat_diag0, return_res = TRUE))
  testthat::expect_true(validate_matrix_diag_GE_0(neg_mat_diag0, return_res = TRUE))
  
  ## expect false results
  testthat::expect_false(validate_matrix_diag_GE_0(matrix(0, ncol = 0, nrow = 0), return_res = TRUE))
  testthat::expect_false(validate_matrix_diag_GE_0(matrix(-0.1, ncol = 3, nrow = 3), return_res = TRUE))
  testthat::expect_false(validate_matrix_diag_GE_0(matrix(-1e-12, ncol = 3, nrow = 3), return_res = TRUE))
  
  
  ## expect no results (returns `NULL`)
  testthat::expect_identical(validate_matrix_diag_GE_0(matrix(0, ncol = 1, nrow = 1), return_res = FALSE), NULL)
  testthat::expect_identical(validate_matrix_diag_GE_0(matrix(0, ncol = 3, nrow = 3), return_res = FALSE), NULL)
  testthat::expect_identical(validate_matrix_diag_GE_0(matrix(0, ncol = 10, nrow = 10), return_res = FALSE), NULL)
  testthat::expect_identical(validate_matrix_diag_GE_0(tst_mat, return_res = FALSE), NULL)
  testthat::expect_identical(validate_matrix_diag_GE_0(tst_mat_diag0, return_res = FALSE), NULL)
  testthat::expect_identical(validate_matrix_diag_GE_0(neg_mat_diag0, return_res = FALSE), NULL)
  
  ## expect `validate` error message for `errorClass = "invalid-input"`
  testthat::expect_error(validate_matrix_diag_GE_0(matrix(0, ncol = 0, nrow = 0), return_res = FALSE), class = "invalid-input")
  testthat::expect_error(validate_matrix_diag_GE_0(matrix(-0.1, ncol = 3, nrow = 3), return_res = FALSE), class = "invalid-input")
  testthat::expect_error(validate_matrix_diag_GE_0(matrix(-1e-12, ncol = 3, nrow = 3), return_res = FALSE), class = "invalid-input")
})


### validate_corr_matrix() -----------------------------------------------------------
testthat::test_that("validate_corr_matrix", {
  
  #######################################################################################|
  ## test 3x3 matrix
  #######################################################################################|
  tst_mat <- matrix(c(2, 1, 1, 1, 4, 2, 1, 2, 3), nrow = 3L, ncol = 3L)
  tst_mat_diag0 <- tst_mat
  diag(tst_mat_diag0) <- 0
  neg_mat_diag0 <- -1*tst_mat_diag0
  
  ## expect true results with a warning due low matrix dimensionality (no lower triangular elements)
  testthat::expect_true(validate_corr_matrix(matrix(0, ncol = 0, nrow = 0), return_res = TRUE))
  testthat::expect_true(validate_corr_matrix(matrix(0, ncol = 1, nrow = 1), return_res = TRUE))
  
  ## expect true results
  testthat::expect_true(validate_corr_matrix(matrix(0, ncol = 3, nrow = 3), return_res = TRUE))
  testthat::expect_true(validate_corr_matrix(matrix(0, ncol = 10, nrow = 10), return_res = TRUE))
  testthat::expect_true(validate_corr_matrix(matrix(-0.1, ncol = 3, nrow = 3), return_res = TRUE))
  testthat::expect_true(validate_corr_matrix(matrix(-1e-12, ncol = 3, nrow = 3), return_res = TRUE))
  testthat::expect_true(validate_corr_matrix(tst_mat/max(tst_mat), return_res = TRUE))
  testthat::expect_true(validate_corr_matrix(tst_mat_diag0/max(tst_mat_diag0), return_res = TRUE))
  testthat::expect_true(validate_corr_matrix(neg_mat_diag0/min(neg_mat_diag0), return_res = TRUE))
  
  ## expect false results
  testthat::expect_false(validate_corr_matrix(tst_mat, return_res = TRUE))
  testthat::expect_false(validate_corr_matrix(tst_mat_diag0, return_res = TRUE))
  testthat::expect_false(validate_corr_matrix(neg_mat_diag0, return_res = TRUE))
  
  
  ## expect no results (returns `NULL`)
  testthat::expect_identical(validate_corr_matrix(matrix(0, ncol = 3, nrow = 3), return_res = FALSE), NULL)
  testthat::expect_identical(validate_corr_matrix(matrix(0, ncol = 10, nrow = 10), return_res = FALSE), NULL)
  testthat::expect_identical(validate_corr_matrix(matrix(-0.1, ncol = 3, nrow = 3), return_res = FALSE), NULL)
  testthat::expect_identical(validate_corr_matrix(matrix(-1e-12, ncol = 3, nrow = 3), return_res = FALSE), NULL)
  testthat::expect_identical(validate_corr_matrix(tst_mat/max(tst_mat), return_res = FALSE), NULL)
  testthat::expect_identical(validate_corr_matrix(tst_mat_diag0/max(tst_mat_diag0), return_res = FALSE), NULL)
  testthat::expect_identical(validate_corr_matrix(neg_mat_diag0/min(neg_mat_diag0), return_res = FALSE), NULL)
  
  ## expect `validate` error message for `errorClass = "invalid-input"`
  testthat::expect_error(validate_corr_matrix(tst_mat, return_res = FALSE), class = "invalid-input")
  testthat::expect_error(validate_corr_matrix(tst_mat_diag0, return_res = FALSE), class = "invalid-input")
  testthat::expect_error(validate_corr_matrix(neg_mat_diag0, return_res = FALSE), class = "invalid-input")
})


### double_sweep_cov_to_corr_matrix() -----------------------------------------------------------

testthat::test_that("double_sweep_cov_to_corr_matrix", {
  
  #######################################################################################|
  ## test 3x3 asymmetric matrix with defined dimnames
  #######################################################################################|
  tst_mat_names <- list(c("ETA1", "ETA2", "ETA3"), c("ETA1", "ETA2", "ETA3"))
  tst_mat_named <- matrix(c(2, 1, 1, 1, 4, 2, 1, 2, 3), nrow = 3L, ncol = 3L, dimnames = tst_mat_names)
  res_mat_named <- cov2cor(tst_mat_named)
  diag(res_mat_named) <- diag(tst_mat_named)
  
  testthat::expect_true(isSymmetric(tst_mat_named))
  testthat::expect_type(double_sweep_cov_to_corr_matrix(tst_mat_named), "double")
  testthat::expect_identical(dimnames(double_sweep_cov_to_corr_matrix(tst_mat_named)), tst_mat_names)
  testthat::expect_identical(dim(double_sweep_cov_to_corr_matrix(tst_mat_named)), c(length(tst_mat_names[[1]]), length(tst_mat_names[[2]])))
  testthat::expect_equal(double_sweep_cov_to_corr_matrix(tst_mat_named), res_mat_named)
  
  
  #######################################################################################|
  ## test random effect matrices from a TMDD library model
  #######################################################################################|
  mod_tmdd <- app_libmod_objs[["tmdd_full_2cmt_1abs"]]
  modc_tmdd <- collapse_re(mod_tmdd, show_matrices = FALSE)
  modc_tmdd_labels <- extract_model_re_labels(modc_tmdd)
  modc_tmdd_re_mat <- extract_model_re_matrices(modc_tmdd)
  modc_tmdd_re_lbl <- label_model_re_matrices(modc_tmdd)
  
  ## build true OMEGA/SIGMA full block matrices to test against
  modc_tmdd_true_om_cov <- matrix(c(0.116, 0.0812, 0, 0.0812, 0.116, 0, 0, 0, 0.04), nrow = 3L, ncol = 3L, dimnames = list(c("ETA_CL", "ETA_VC", "ETA_R0"), c("ETA_CL", "ETA_VC", "ETA_R0")))
  modc_tmdd_true_om_cor <- matrix(c(0.116, 0.7, 0, 0.7, 0.116, 0, 0, 0, 0.04), nrow = 3L, ncol = 3L, dimnames = list(c("ETA_CL", "ETA_VC", "ETA_R0"), c("ETA_CL", "ETA_VC", "ETA_R0")))
  modc_tmdd_true_sg_cov <- matrix(c(0.04, 0, 0, 0, 0.04, 0, 0, 0, 0), nrow = 3L, ncol = 3L, dimnames = list(c("PROP_DRUG", "PROP_TARGET", "PROP_COMPLEX"), c("PROP_DRUG", "PROP_TARGET", "PROP_COMPLEX")))
  modc_tmdd_true_sg_cor <- matrix(c(0.04, 0, 0, 0, 0.04, 0, 0, 0, 0), nrow = 3L, ncol = 3L, dimnames = list(c("PROP_DRUG", "PROP_TARGET", "PROP_COMPLEX"), c("PROP_DRUG", "PROP_TARGET", "PROP_COMPLEX")))
  
  ## test OMEGA matrix conversion: cov --> corr
  testthat::expect_type(double_sweep_cov_to_corr_matrix(modc_tmdd_re_lbl$omega), "double")
  testthat::expect_identical(dimnames(double_sweep_cov_to_corr_matrix(modc_tmdd_re_lbl$omega)), list(c("ETA_CL", "ETA_VC", "ETA_R0"), c("ETA_CL", "ETA_VC", "ETA_R0")))
  testthat::expect_identical(dimnames(double_sweep_cov_to_corr_matrix(modc_tmdd_re_lbl$omega)), unname(rep(modc_tmdd_labels["omega"], 2)))
  testthat::expect_identical(dim(double_sweep_cov_to_corr_matrix(modc_tmdd_re_lbl$omega)), rep(length(modc_tmdd_labels$omega), 2))
  testthat::expect_identical(modc_tmdd_re_lbl$omega, modc_tmdd_true_om_cov)
  testthat::expect_identical(double_sweep_cov_to_corr_matrix(modc_tmdd_re_lbl$omega), modc_tmdd_true_om_cor)
  
  
  ## test SIGMA matrix conversion: cov --> corr
  testthat::expect_type(double_sweep_cov_to_corr_matrix(modc_tmdd_re_lbl$sigma), "double")
  testthat::expect_identical(dimnames(double_sweep_cov_to_corr_matrix(modc_tmdd_re_lbl$sigma)), list(c("PROP_DRUG", "PROP_TARGET", "PROP_COMPLEX"), c("PROP_DRUG", "PROP_TARGET", "PROP_COMPLEX")))
  testthat::expect_identical(dimnames(double_sweep_cov_to_corr_matrix(modc_tmdd_re_lbl$sigma)), unname(rep(modc_tmdd_labels["sigma"], 2)))
  testthat::expect_identical(dim(double_sweep_cov_to_corr_matrix(modc_tmdd_re_lbl$sigma)), rep(length(modc_tmdd_labels$sigma), 2))
  testthat::expect_identical(modc_tmdd_re_lbl$sigma, modc_tmdd_true_sg_cov)
  testthat::expect_identical(double_sweep_cov_to_corr_matrix(modc_tmdd_re_lbl$sigma), modc_tmdd_true_sg_cor)
  
  
  ## test `validate_corr` input argument
  
  ## test invalid correlation coefficients > 1
  invalid_cov_pos <- matrix(c(0.116, 0.232, 0, 0.232, 0.116, 0, 0, 0, 0.04), nrow = 3L, ncol = 3L, dimnames = list(c("ETA_CL", "ETA_VC", "ETA_R0"), c("ETA_CL", "ETA_VC", "ETA_R0")))
  invalid_cor_pos <- matrix(c(0.116, 2, 0, 2, 0.116, 0, 0, 0, 0.04), nrow = 3L, ncol = 3L, dimnames = list(c("ETA_CL", "ETA_VC", "ETA_R0"), c("ETA_CL", "ETA_VC", "ETA_R0")))
  testthat::expect_error(double_sweep_cov_to_corr_matrix(invalid_cov_pos, validate_corr = TRUE, zero_nan = TRUE))
  testthat::expect_error(double_sweep_cov_to_corr_matrix(invalid_cov_pos, validate_corr = TRUE, zero_nan = FALSE))
  testthat::expect_equal(double_sweep_cov_to_corr_matrix(invalid_cov_pos, validate_corr = FALSE), invalid_cor_pos)
  
  ## test invalid correlation coefficients < -1
  invalid_cov_neg <- matrix(c(0.116, -0.232, 0, -0.232, 0.116, 0, 0, 0, 0.04), nrow = 3L, ncol = 3L, dimnames = list(c("ETA_CL", "ETA_VC", "ETA_R0"), c("ETA_CL", "ETA_VC", "ETA_R0")))
  invalid_cor_neg <- matrix(c(0.116, -2, 0, -2, 0.116, 0, 0, 0, 0.04), nrow = 3L, ncol = 3L, dimnames = list(c("ETA_CL", "ETA_VC", "ETA_R0"), c("ETA_CL", "ETA_VC", "ETA_R0")))
  testthat::expect_error(double_sweep_cov_to_corr_matrix(invalid_cov_neg, validate_corr = TRUE, zero_nan = TRUE))
  testthat::expect_error(double_sweep_cov_to_corr_matrix(invalid_cov_neg, validate_corr = TRUE, zero_nan = FALSE))
  testthat::expect_equal(double_sweep_cov_to_corr_matrix(invalid_cov_neg, validate_corr = FALSE), invalid_cor_neg)
  
  
  ## test `zero_nan` input argument
  
  ## test against valid matrices
  cov_mat_diag0 <- matrix(c(0.116, 0.0812, 0, 0.0812, 0.116, 0, 0, 0, 0), nrow = 3L, ncol = 3L, dimnames = list(c("ETA_CL", "ETA_VC", "ETA_R0"), c("ETA_CL", "ETA_VC", "ETA_R0")))
  cor_mat_diag0 <- matrix(c(0.116, 0.7, 0, 0.7, 0.116, 0, 0, 0, 0), nrow = 3L, ncol = 3L, dimnames = list(c("ETA_CL", "ETA_VC", "ETA_R0"), c("ETA_CL", "ETA_VC", "ETA_R0")))
  cor_mat_diag0_NaN <- matrix(c(0.116, 0.7, NaN, 0.7, 0.116, NaN, NaN, NaN, 0), nrow = 3L, ncol = 3L, dimnames = list(c("ETA_CL", "ETA_VC", "ETA_R0"), c("ETA_CL", "ETA_VC", "ETA_R0")))
  testthat::expect_equal(double_sweep_cov_to_corr_matrix(cov_mat_diag0, validate_corr = TRUE,  zero_nan = TRUE), cor_mat_diag0)
  testthat::expect_equal(double_sweep_cov_to_corr_matrix(cov_mat_diag0, validate_corr = FALSE, zero_nan = TRUE), cor_mat_diag0)
  testthat::expect_equal(double_sweep_cov_to_corr_matrix(cov_mat_diag0, validate_corr = TRUE,  zero_nan = FALSE), cor_mat_diag0_NaN)
  testthat::expect_equal(double_sweep_cov_to_corr_matrix(cov_mat_diag0, validate_corr = FALSE, zero_nan = FALSE), cor_mat_diag0_NaN)
  
  ## test against invalid matrices
  invalid_cov_mat_diag0 <- matrix(c(0.116, 0.232, 0, 0.232, 0.116, 0, 0, 0, 0), nrow = 3L, ncol = 3L, dimnames = list(c("ETA_CL", "ETA_VC", "ETA_R0"), c("ETA_CL", "ETA_VC", "ETA_R0")))
  invalid_cor_mat_diag0 <- matrix(c(0.116, 2, 0, 2, 0.116, 0, 0, 0, 0), nrow = 3L, ncol = 3L, dimnames = list(c("ETA_CL", "ETA_VC", "ETA_R0"), c("ETA_CL", "ETA_VC", "ETA_R0")))
  invalid_cor_mat_diag0_NaN <- matrix(c(0.116, 2, NaN, 2, 0.116, NaN, NaN, NaN, 0), nrow = 3L, ncol = 3L, dimnames = list(c("ETA_CL", "ETA_VC", "ETA_R0"), c("ETA_CL", "ETA_VC", "ETA_R0")))
  testthat::expect_error(double_sweep_cov_to_corr_matrix(invalid_cov_mat_diag0, validate_corr = TRUE,  zero_nan = TRUE))
  testthat::expect_error(double_sweep_cov_to_corr_matrix(invalid_cov_mat_diag0, validate_corr = TRUE,  zero_nan = FALSE))
  testthat::expect_equal(double_sweep_cov_to_corr_matrix(invalid_cov_mat_diag0, validate_corr = FALSE, zero_nan = TRUE), invalid_cor_mat_diag0)
  testthat::expect_equal(double_sweep_cov_to_corr_matrix(invalid_cov_mat_diag0, validate_corr = FALSE, zero_nan = FALSE), invalid_cor_mat_diag0_NaN)
})

### eigen_decomp_GE_0() -----------------------------------------------------------

#### SKIP UNIT TESTS --> ONLY CALL IN `is_valid_re_matrix()` helpers function & result isn't used



### is_valid_mrgsolve_mvgauss() -----------------------------------------------------------
testthat::test_that("is_valid_mrgsolve_mvgauss", {
  
  #######################################################################################|
  ## test 3x3 matrix
  #######################################################################################|
  tst_mat <- matrix(c(2, 1, 1, 1, 4, 2, 1, 2, 3), nrow = 3L, ncol = 3L)
  tst_mat_diag0 <- tst_mat
  diag(tst_mat_diag0) <- 0
  neg_mat_diag0 <- -1*tst_mat_diag0
  neg_mat <- neg_mat_diag0
  diag(neg_mat) <- diag(tst_mat)
  
  ## test input argument `print_results`
  capture.output(testthat::expect_true(is_valid_mrgsolve_mvgauss(tst_mat, print_results = TRUE)))
  testthat::expect_output(is_valid_mrgsolve_mvgauss(tst_mat, print_results = TRUE))
  testthat::expect_true(is_valid_mrgsolve_mvgauss(tst_mat, print_results = FALSE))
  
  ## expect true results
  testthat::expect_true(is_valid_mrgsolve_mvgauss(matrix(0, ncol = 0, nrow = 0)))
  testthat::expect_true(is_valid_mrgsolve_mvgauss(matrix(0, ncol = 1, nrow = 1)))
  testthat::expect_true(is_valid_mrgsolve_mvgauss(matrix(0, ncol = 3, nrow = 3)))
  testthat::expect_true(is_valid_mrgsolve_mvgauss(matrix(0, ncol = 10, nrow = 10)))
  testthat::expect_true(is_valid_mrgsolve_mvgauss(tst_mat/max(tst_mat)))
  testthat::expect_true(is_valid_mrgsolve_mvgauss(tst_mat))

  
  ## expect false results
  testthat::expect_false(is_valid_mrgsolve_mvgauss(matrix(-0.1, ncol = 3, nrow = 3)))
  testthat::expect_false(is_valid_mrgsolve_mvgauss(matrix(-1e-12, ncol = 3, nrow = 3)))
  testthat::expect_false(is_valid_mrgsolve_mvgauss(tst_mat_diag0/max(tst_mat_diag0)))
  testthat::expect_false(is_valid_mrgsolve_mvgauss(neg_mat_diag0/min(neg_mat_diag0)))
  testthat::expect_false(is_valid_mrgsolve_mvgauss(tst_mat_diag0))
  testthat::expect_false(is_valid_mrgsolve_mvgauss(neg_mat_diag0))
  
  
  ## edge cases
  cor_0.50 <- matrix(0.50, ncol = 5, nrow = 5)
  cor_0.75 <- matrix(0.75, ncol = 5, nrow = 5)
  cor_1.00 <- matrix(1.00, ncol = 5, nrow = 5)
  
  cov_0.50 <- double_sweep_corr_to_cov_matrix(cor_0.50)
  cov_0.75 <- double_sweep_corr_to_cov_matrix(cor_0.75)
  cov_1.00 <- double_sweep_corr_to_cov_matrix(cor_1.00)
  
  testthat::expect_true(is_valid_mrgsolve_mvgauss(cov_0.50))
  testthat::expect_true(is_valid_mrgsolve_mvgauss(cov_0.75))
  testthat::expect_true(is_valid_mrgsolve_mvgauss(cov_1.00))
  
  cov_0.75_diag_1.00 <- cov_0.75
  diag(cov_0.75_diag_1.00) <- 1.00
  testthat::expect_true(is_valid_mrgsolve_mvgauss(cov_0.75_diag_1.00))
  
  ## failing edge cases
  cov_1.00_diag_0.75 <- cov_1.00
  diag(cov_1.00_diag_0.75) <- 0.75
  testthat::expect_false(is_valid_mrgsolve_mvgauss(cov_1.00_diag_0.75))
  
  cov_0.75_diag_0.50 <- cov_0.75
  diag(cov_0.75_diag_0.50) <- 0.50
  testthat::expect_false(is_valid_mrgsolve_mvgauss(cov_0.75_diag_0.50))
})


### is_valid_re_matrix() -----------------------------------------------------------
testthat::test_that("is_valid_re_matrix", {
  
  #######################################################################################|
  ## test 3x3 matrix
  #######################################################################################|
  tst_mat <- matrix(c(2, 1, 1, 1, 4, 2, 1, 2, 3), nrow = 3L, ncol = 3L)
  tst_mat_diag0 <- tst_mat
  diag(tst_mat_diag0) <- 0
  neg_mat_diag0 <- -1*tst_mat_diag0
  neg_mat <- neg_mat_diag0
  diag(neg_mat) <- diag(tst_mat)
  
  ## test input argument `print_results`
  capture.output(testthat::expect_true(is_valid_re_matrix(tst_mat, print_results = TRUE)))
  testthat::expect_output(is_valid_re_matrix(tst_mat, print_results = TRUE))
  testthat::expect_true(is_valid_re_matrix(tst_mat, print_results = FALSE))
  
  ## expect error
  testthat::expect_error(is_valid_re_matrix(matrix(0, ncol = 0, nrow = 0)), regexp = "0 x 0 matrix", perl = TRUE)
  
  ## expect true results
  testthat::expect_true(is_valid_re_matrix(matrix(0, ncol = 1, nrow = 1)))
  testthat::expect_true(is_valid_re_matrix(matrix(0, ncol = 3, nrow = 3)))
  testthat::expect_true(is_valid_re_matrix(matrix(0, ncol = 10, nrow = 10)))
  testthat::expect_true(is_valid_re_matrix(tst_mat/max(tst_mat)))
  testthat::expect_true(is_valid_re_matrix(tst_mat))
  
  
  ## expect false results
  testthat::expect_false(is_valid_re_matrix(matrix(-0.1, ncol = 3, nrow = 3)))
  testthat::expect_false(is_valid_re_matrix(matrix(-1e-12, ncol = 3, nrow = 3)))
  testthat::expect_false(is_valid_re_matrix(tst_mat_diag0/max(tst_mat_diag0)))
  testthat::expect_false(is_valid_re_matrix(neg_mat_diag0/min(neg_mat_diag0)))
  testthat::expect_false(is_valid_re_matrix(tst_mat_diag0))
  testthat::expect_false(is_valid_re_matrix(neg_mat_diag0))
  
  
  ## edge cases
  cor_0.50 <- matrix(0.50, ncol = 5, nrow = 5)
  cor_0.75 <- matrix(0.75, ncol = 5, nrow = 5)
  cor_1.00 <- matrix(1.00, ncol = 5, nrow = 5)
  
  cov_0.50 <- double_sweep_corr_to_cov_matrix(cor_0.50)
  cov_0.75 <- double_sweep_corr_to_cov_matrix(cor_0.75)
  cov_1.00 <- double_sweep_corr_to_cov_matrix(cor_1.00)
  
  testthat::expect_true(is_valid_re_matrix(cov_0.50))
  testthat::expect_true(is_valid_re_matrix(cov_0.75))
  testthat::expect_true(is_valid_re_matrix(cov_1.00))
  
  cov_0.75_diag_1.00 <- cov_0.75
  diag(cov_0.75_diag_1.00) <- 1.00
  testthat::expect_true(is_valid_re_matrix(cov_0.75_diag_1.00))
  
  ## failing edge cases
  cov_1.00_diag_0.75 <- cov_1.00
  diag(cov_1.00_diag_0.75) <- 0.75
  testthat::expect_false(is_valid_re_matrix(cov_1.00_diag_0.75))
  
  cov_0.75_diag_0.50 <- cov_0.75
  diag(cov_0.75_diag_0.50) <- 0.50
  testthat::expect_false(is_valid_re_matrix(cov_0.75_diag_0.50))
})


#######################################################################################################################|
## Model Parameter UI Inputs -------------------------------------------------------------------------------------------
#######################################################################################################################|

### build_model_param_value_ids() -----------------------------------------------------------
    testthat::test_that("build_model_param_value_ids", {
      
      ## model with 6 parameters
      params_6par <- names(mrgsolve::param(mod_6par))
      param_value_ids_6par <- paste0(params_6par, "_value")
      testthat::expect_type(build_model_param_value_ids(mod_6par), "character")
      testthat::expect_length(build_model_param_value_ids(mod_6par), nparams_6par)
      testthat::expect_identical(build_model_param_value_ids(mod_6par), param_value_ids_6par)
      
      ## default library model
      def_mod <- app_libmod_objs[[fetch_default_libmod_name()]]
      param_names_def <- names(mrgsolve::param(def_mod))
      n_params_def <- length(param_names_def)
      param_value_ids_def <- paste0(param_names_def, "_value")
      
      testthat::expect_type(build_model_param_value_ids(def_mod), "character")
      testthat::expect_length(build_model_param_value_ids(def_mod), n_params_def)
      testthat::expect_identical(build_model_param_value_ids(def_mod), param_value_ids_def)
    })


### build_model_param_units_ids() -----------------------------------------------------------
    testthat::test_that("build_model_param_units_ids", {
      
      ## model with 6 parameters
      params_6par <- names(mrgsolve::param(mod_6par))
      param_value_ids_6par <- paste0(params_6par, "_units")
      testthat::expect_type(build_model_param_units_ids(mod_6par), "character")
      testthat::expect_length(build_model_param_units_ids(mod_6par), nparams_6par)
      testthat::expect_identical(build_model_param_units_ids(mod_6par), param_value_ids_6par)
      
      ## default library model
      def_mod <- app_libmod_objs[[fetch_default_libmod_name()]]
      param_names_def <- names(mrgsolve::param(def_mod))
      n_params_def <- length(param_names_def)
      param_value_ids_def <- paste0(param_names_def, "_units")
      
      testthat::expect_type(build_model_param_units_ids(def_mod), "character")
      testthat::expect_length(build_model_param_units_ids(def_mod), n_params_def)
      testthat::expect_identical(build_model_param_units_ids(def_mod), param_value_ids_def)
    })


### make_model_param_value_labels() -----------------------------------------------------------
    testthat::test_that("make_model_param_value_labels", {
      
      ## model with 6 parameters
      params_6par <- names(mrgsolve::param(mod_6par))
      param_value_ids_6par <- paste0(params_6par, " Value")
      testthat::expect_type(make_model_param_value_labels(mod_6par), "character")
      testthat::expect_length(make_model_param_value_labels(mod_6par), nparams_6par)
      testthat::expect_identical(make_model_param_value_labels(mod_6par), param_value_ids_6par)
      
      ## default library model
      def_mod <- app_libmod_objs[[fetch_default_libmod_name()]]
      param_names_def <- names(mrgsolve::param(def_mod))
      n_params_def <- length(param_names_def)
      param_value_ids_def <- paste0(param_names_def, " Value")
      
      testthat::expect_type(make_model_param_value_labels(def_mod), "character")
      testthat::expect_length(make_model_param_value_labels(def_mod), n_params_def)
      testthat::expect_identical(make_model_param_value_labels(def_mod), param_value_ids_def)
    })


### make_model_param_units_labels() -----------------------------------------------------------
    testthat::test_that("make_model_param_units_labels", {
      
      ## model with 6 parameters
      params_6par <- names(mrgsolve::param(mod_6par))
      param_value_ids_6par <- paste0(params_6par, " Units")
      testthat::expect_type(make_model_param_units_labels(mod_6par), "character")
      testthat::expect_length(make_model_param_units_labels(mod_6par), nparams_6par)
      testthat::expect_identical(make_model_param_units_labels(mod_6par), param_value_ids_6par)
      
      ## default library model
      def_mod <- app_libmod_objs[[fetch_default_libmod_name()]]
      param_names_def <- names(mrgsolve::param(def_mod))
      n_params_def <- length(param_names_def)
      param_value_ids_def <- paste0(param_names_def, " Units")
      
      testthat::expect_type(make_model_param_units_labels(def_mod), "character")
      testthat::expect_length(make_model_param_units_labels(def_mod), n_params_def)
      testthat::expect_identical(make_model_param_units_labels(def_mod), param_value_ids_def)
    })


### build_fixed_param_text_input_boxes() -------------------------------------------------
    testthat::test_that("build_fixed_param_text_input_boxes", {
      
      ## define unit test helper functions
      build_fixed_param_popup_list <- function(mod) {
        mod_list <- as.list(mod)
        if (isTRUE(identical(nrow(mod_list$details$data), 0L))) {
          mod_annot_data <- mrgsolve:::details(mod)
          
          if (isTRUE("units" %in% names(mod_annot_data))) {
            mod_annot_data <- mod_annot_data %>% rename(unit = units)
          }
          
          mod_list$details$data <- mod_annot_data
        }
        
        if (isFALSE("unit" %in% names(mod_list$details$data))) {
          mod_list$details$data$unit <- "."
        }
        
        model_params <- extract_model_params(mod = mod)
        param_values <- unlist(model_params)
        param_units <- mod_list$details$data$unit
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
        fixed_param_lists
      }
      
      build_fixed_param_popup_cols <- function(fplst) {
        build_fixed_param_text_input_boxes(idxs = fplst$idxs, 
                                           param_ids = fplst$param_ids, 
                                           param_labels = fplst$param_labels, 
                                           param_values = fplst$param_values, 
                                           box_type = fplst$box_type)
      }

      ## model with 6 parameters
      fplst_6par <- build_fixed_param_popup_list(mod_6par)
      fppop_6par <- purrr::map(fplst_6par, ~ do.call("build_fixed_param_text_input_boxes", .x))
      fp_col1_6par <- build_fixed_param_popup_cols(fplst = fplst_6par[[1]])
      fp_col2_6par <- build_fixed_param_popup_cols(fplst = fplst_6par[[2]])
      fp_col3_6par <- build_fixed_param_popup_cols(fplst = fplst_6par[[3]])
      fp_col4_6par <- build_fixed_param_popup_cols(fplst = fplst_6par[[4]])
      
      testthat::expect_type(fppop_6par, "list")
      testthat::expect_type(unlist(fppop_6par), "character")
      testthat::expect_length(fppop_6par, 4)
      testthat::expect_identical(fppop_6par[[1]], fp_col1_6par)
      testthat::expect_identical(fppop_6par[[2]], fp_col2_6par)
      testthat::expect_identical(fppop_6par[[3]], fp_col3_6par)
      testthat::expect_identical(fppop_6par[[4]], fp_col4_6par)
      
      
      ## default library model
      def_mod <- app_libmod_objs[[fetch_default_libmod_name()]]
      fplst_def <- build_fixed_param_popup_list(def_mod)
      fppop_def <- purrr::map(fplst_def, ~ do.call("build_fixed_param_text_input_boxes", .x))
      fp_col1_def <- build_fixed_param_popup_cols(fplst = fplst_def[[1]])
      fp_col2_def <- build_fixed_param_popup_cols(fplst = fplst_def[[2]])
      fp_col3_def <- build_fixed_param_popup_cols(fplst = fplst_def[[3]])
      fp_col4_def <- build_fixed_param_popup_cols(fplst = fplst_def[[4]])
      
      testthat::expect_type(fppop_def, "list")
      testthat::expect_type(unlist(fppop_def), "character")
      testthat::expect_length(fppop_def, 4)
      testthat::expect_identical(fppop_def[[1]], fp_col1_def)
      testthat::expect_identical(fppop_def[[2]], fp_col2_def)
      testthat::expect_identical(fppop_def[[3]], fp_col3_def)
      testthat::expect_identical(fppop_def[[4]], fp_col4_def)
      
      ## error expectations
      fplst_def_col1_err <- fplst_def[[1]]
      fplst_def_col1_err$box_type <- "vals"
      testthat::expect_error(build_fixed_param_text_input_boxes(idxs = fplst_def_col1_err$idxs,
                                                                param_ids = fplst_def_col1_err$param_ids,
                                                                param_labels = fplst_def_col1_err$param_labels,
                                                                param_values = fplst_def_col1_err$param_values,
                                                                box_type = fplst_def_col1_err$box_type))
      fplst_def_col2_err <- fplst_def[[2]]
      fplst_def_col2_err$box_type <- "unit"
      testthat::expect_error(build_fixed_param_text_input_boxes(idxs = fplst_def_col2_err$idxs,
                                                                param_ids = fplst_def_col2_err$param_ids,
                                                                param_labels = fplst_def_col2_err$param_labels,
                                                                param_values = fplst_def_col2_err$param_values,
                                                                box_type = fplst_def_col2_err$box_type))
    })



### build_fixed_param_modal_window() -----------------------------------------------------
    testthat::test_that("build_fixed_param_modal_window", {
      
      testthat::local_edition(3)
      
      ## model with 6 parameters
      testthat::expect_type(build_fixed_param_modal_window(mod_6par), "character")
      testthat::expect_length(build_fixed_param_modal_window(mod_6par), 1L)
      testthat::expect_snapshot(build_fixed_param_modal_window(mod_6par), cran = FALSE)
      
      ## default library model
      def_mod <- app_libmod_objs[[fetch_default_libmod_name()]]
      testthat::expect_type(build_fixed_param_modal_window(def_mod), "character")
      testthat::expect_length(build_fixed_param_modal_window(def_mod), 1L)
      testthat::expect_snapshot(build_fixed_param_modal_window(def_mod), cran = FALSE)
    })
    


### reorder_modal_popup_elements() -------------------------------------------------------
    testthat::test_that("reorder_modal_popup_elements", {
      
      testthat::local_edition(3)
      
      ## model with 6 parameters
      mod_6par_modal <- build_fixed_param_modal_window(mod_6par)
      testthat::expect_false(identical(reorder_modal_popup_elements(mod_6par_modal), mod_6par_modal))
      testthat::expect_type(reorder_modal_popup_elements(mod_6par_modal), "character")
      testthat::expect_length(reorder_modal_popup_elements(mod_6par_modal), 1L)
      testthat::expect_snapshot(reorder_modal_popup_elements(mod_6par_modal), cran = FALSE)
      
      ## default library model
      def_mod <- app_libmod_objs[[fetch_default_libmod_name()]]
      def_mod_modal <- build_fixed_param_modal_window(def_mod)
      testthat::expect_false(identical(reorder_modal_popup_elements(def_mod_modal), def_mod_modal))
      testthat::expect_type(reorder_modal_popup_elements(def_mod_modal), "character")
      testthat::expect_length(reorder_modal_popup_elements(def_mod_modal), 1L)
      testthat::expect_snapshot(reorder_modal_popup_elements(def_mod_modal), cran = FALSE)
    })


### reorder_modal_popup_children() -------------------------------------------------------
    testthat::test_that("reorder_modal_popup_children", {
      
      testthat::local_edition(3)
      
      test_modal <- bsModal("rx_docs_modal_window", "Rx Dosing Docs", "rx_docs_modal_button", size = "large",
                            fluidRow(column(12,
                                            tags$div(class="user_input_popup",
                                                     includeMarkdown(here::here('www/mrgsolve_ev_rx_docs.rmd'))
                                            )))
      )
      
      testthat::expect_false(identical(reorder_modal_popup_children(test_modal), test_modal))
      testthat::expect_type(reorder_modal_popup_children(test_modal), "list")
      testthat::expect_length(reorder_modal_popup_children(test_modal), 3L)
      testthat::expect_named(reorder_modal_popup_children(test_modal), c("name", "attribs", "children"))
      testthat::expect_snapshot(reorder_modal_popup_children(test_modal), cran = FALSE)
    })


######################################################################################################################|
## "parse_dosing_regimens.R" helper functions --------------------------------------------------------------------------
#######################################################################################################################|

### regexec_match() -----------------------------------------------------------
    testthat::test_that("regexec_match", {
      
      ## test default rx input 
      rx_split <- split_rx_dosing_periods(define_default_rx_input())
      rx_ptrns <- dosing_period_regex_patterns()

      ## dose amount pattern
      testthat::expect_type(regexec_match(rx_split, pattern = rx_ptrns$dose_amount), "list")
      testthat::expect_length(regexec_match(rx_split, pattern = rx_ptrns$dose_amount), length(rx_split))
      testthat::expect_identical(regexec_match(rx_split, pattern = rx_ptrns$dose_amount), list(c("150", "150"), c("100", "100"), c("100", "100")))
      
      ## dose route pattern
      testthat::expect_type(regexec_match(rx_split, pattern = rx_ptrns$dose_route), "list")
      testthat::expect_length(regexec_match(rx_split, pattern = rx_ptrns$dose_route), length(rx_split))
      testthat::expect_identical(regexec_match(rx_split, pattern = rx_ptrns$dose_route), list(character(0), character(0), character(0)))
    
      ## infusion duration pattern
      testthat::expect_type(regexec_match(rx_split, pattern = rx_ptrns$infusion_dur), "list")
      testthat::expect_length(regexec_match(rx_split, pattern = rx_ptrns$infusion_dur), length(rx_split))
      testthat::expect_identical(regexec_match(rx_split, pattern = rx_ptrns$infusion_dur), list(c("over 0.25", "0.25"), c("over 0.25", "0.25"), c("over 0.25", "0.25")))
      
      ## compartment number pattern
      testthat::expect_type(regexec_match(rx_split, pattern = rx_ptrns$cmt_number), "list")
      testthat::expect_length(regexec_match(rx_split, pattern = rx_ptrns$cmt_number), length(rx_split))
      testthat::expect_identical(regexec_match(rx_split, pattern = rx_ptrns$cmt_number), list(c("in 2", "2"), c("in 1", "1"), c("in 2", "2")))
      
      ## dose frequency pattern
      testthat::expect_type(regexec_match(rx_split, pattern = rx_ptrns$dose_freq), "list")
      testthat::expect_length(regexec_match(rx_split, pattern = rx_ptrns$dose_freq), length(rx_split))
      testthat::expect_identical(regexec_match(rx_split, pattern = rx_ptrns$dose_freq), list(character(0), c("q14", "14"), c("q7", "7")))
      
      ## total doses pattern
      testthat::expect_type(regexec_match(rx_split, pattern = rx_ptrns$total_doses), "list")
      testthat::expect_length(regexec_match(rx_split, pattern = rx_ptrns$total_doses), length(rx_split))
      testthat::expect_identical(regexec_match(rx_split, pattern = rx_ptrns$total_doses), list(character(0), c(" x 6", "6"), c(" x 8", "8")))
      
      ## delay time pattern
      testthat::expect_type(regexec_match(rx_split, pattern = rx_ptrns$delay_time), "list")
      testthat::expect_length(regexec_match(rx_split, pattern = rx_ptrns$delay_time), length(rx_split))
      testthat::expect_identical(regexec_match(rx_split, pattern = rx_ptrns$delay_time), list(character(0), c("after 28", "28"), c("after 0.5", "0.5")))
      
      ## test input parameters
      testthat::expect_output(regexec_match(rx_split, pattern = rx_ptrns$dose_amount), NA)
      testthat::expect_output(regexec_match(rx_split, pattern = rx_ptrns$dose_amount, print_result = FALSE), NA)
      testthat::expect_output(regexec_match(rx_split, pattern = rx_ptrns$dose_amount, print_result = TRUE), NULL)
    })


### dosing_period_regex_patterns() -----------------------------------------------------------
    testthat::test_that("dosing_period_regex_patterns", {
      testthat::expect_type(dosing_period_regex_patterns(), "list")
      testthat::expect_type(unlist(dosing_period_regex_patterns()), "character")
      testthat::expect_length(dosing_period_regex_patterns(), 7L)
      testthat::expect_named(dosing_period_regex_patterns(), c("dose_amount", "dose_route", "infusion_dur", "cmt_number", "dose_freq", "total_doses", "delay_time"))
      
      ## verify each dosing period regex pattern
      testthat::expect_identical(dosing_period_regex_patterns()$dose_amount,  "^ *(\\d+[\\.]*\\d*[Ee\\+\\-]{0,2}\\d*)")
      testthat::expect_identical(dosing_period_regex_patterns()$dose_route,   "(?i)\\b(iv|sc)+\\b")
      testthat::expect_identical(dosing_period_regex_patterns()$infusion_dur, "ov[er]* +(\\d+[\\.]*\\d*)")
      testthat::expect_identical(dosing_period_regex_patterns()$cmt_number,   "in +(\\d+)")
      testthat::expect_identical(dosing_period_regex_patterns()$dose_freq,    "[Qq] *(\\d+[\\.]*\\d*)")
      testthat::expect_identical(dosing_period_regex_patterns()$total_doses,  " *x *(\\d+)")
      testthat::expect_identical(dosing_period_regex_patterns()$delay_time,   "aft[er]* +(-*\\d+[\\.]*\\d*)")
    })


### parse_rx_period_parts() -----------------------------------------------------------
    testthat::test_that("parse_rx_period_parts", {
      
      ## test default rx input 
      rx_split <- split_rx_dosing_periods(define_default_rx_input())
      rx_reslt <- parse_rx_period_parts(rx_split)
      
      ## dose amount pattern
      testthat::expect_type(parse_rx_period_parts(rx_split), "list")
      testthat::expect_named(parse_rx_period_parts(rx_split), c("rx_period", "dose_amount", "dose_route", "infusion_dur", "cmt_number", "dose_freq", "total_doses", "delay_time"))
      testthat::expect_identical(nrow(parse_rx_period_parts(rx_split)), length(rx_split))
      testthat::expect_identical(rx_reslt$rx_period, c("150 mg over 0.25 in 2", "100 mg in 1 over 0.25 q14 x 6 after 28", "100 mg over 0.25 in 2 q7 x 8 after 0.5"))
      testthat::expect_identical(rx_reslt$dose_amount, c("150", "100", "100"))
      testthat::expect_identical(rx_reslt$dose_route, c(NA_character_, NA_character_, NA_character_))
      testthat::expect_identical(rx_reslt$infusion_dur, c("0.25", "0.25", "0.25"))
      testthat::expect_identical(rx_reslt$cmt_number, c("2", "1", "2"))
      testthat::expect_identical(rx_reslt$dose_freq, c(NA, "14", "7"))
      testthat::expect_identical(rx_reslt$total_doses, c(NA, "6", "8"))
      testthat::expect_identical(rx_reslt$delay_time, c(NA, "28", "0.5"))
      
      
      ## test input parameters
      testthat::expect_output(parse_rx_period_parts(rx_split), NA)
      testthat::expect_output(parse_rx_period_parts(rx_split, print_result = FALSE), NA)
      testthat::expect_output(parse_rx_period_parts(rx_split, print_result = TRUE), NULL)
      
      
      ## test rx input with all components (single period)
      rx_all_parts <- "1.005e3 mg iv in 2 over 3 q5 x 7 after 1"
      rx_all_reslt <- parse_rx_period_parts(rx_all_parts)
      
      testthat::expect_type(parse_rx_period_parts(rx_all_parts), "list")
      testthat::expect_named(parse_rx_period_parts(rx_all_parts), c("rx_period", "dose_amount", "dose_route", "infusion_dur", "cmt_number", "dose_freq", "total_doses", "delay_time"))
      testthat::expect_identical(nrow(parse_rx_period_parts(rx_all_parts)), length(rx_all_parts))
      testthat::expect_identical(rx_all_reslt$rx_period, rx_all_parts) 
      testthat::expect_identical(rx_all_reslt$rx_period, "1.005e3 mg iv in 2 over 3 q5 x 7 after 1") 
      testthat::expect_identical(rx_all_reslt$dose_amount, "1.005e3")
      testthat::expect_identical(rx_all_reslt$dose_route, "iv")
      testthat::expect_identical(rx_all_reslt$infusion_dur, "3")
      testthat::expect_identical(rx_all_reslt$cmt_number, "2")
      testthat::expect_identical(rx_all_reslt$dose_freq, "5")
      testthat::expect_identical(rx_all_reslt$total_doses, "7")
      testthat::expect_identical(rx_all_reslt$delay_time, "1")        
    })


### count_rx_period_parts() -----------------------------------------------------------
    testthat::test_that("count_rx_period_parts", {
      
      ## test default rx input 
      rx_split <- split_rx_dosing_periods(define_default_rx_input())
      rx_count <- count_rx_period_parts(rx_split)
      
      ## count the number of rx components in each dosing period
      testthat::expect_type(count_rx_period_parts(rx_split), "list")
      testthat::expect_named(count_rx_period_parts(rx_split), c("rx_period", "dose_amount", "dose_route", "infusion_dur", "cmt_number", "dose_freq", "total_doses", "delay_time"))
      testthat::expect_identical(nrow(count_rx_period_parts(rx_split)), length(rx_split))
      testthat::expect_identical(rx_count$rx_period, rx_split)
      testthat::expect_identical(rx_count$rx_period, c("150 mg over 0.25 in 2", "100 mg in 1 over 0.25 q14 x 6 after 28", "100 mg over 0.25 in 2 q7 x 8 after 0.5"))
      testthat::expect_identical(rx_count$dose_amount,  c(1L, 1L, 1L))
      testthat::expect_identical(rx_count$dose_route,   c(0L, 0L, 0L))
      testthat::expect_identical(rx_count$infusion_dur, c(1L, 1L, 1L))
      testthat::expect_identical(rx_count$cmt_number,   c(1L, 1L, 1L))
      testthat::expect_identical(rx_count$dose_freq,    c(0L, 1L, 1L))
      testthat::expect_identical(rx_count$total_doses,  c(0L, 1L, 1L))
      testthat::expect_identical(rx_count$delay_time,   c(0L, 1L, 1L))

      ## count repeated rx components > 1
      rx_split_2x <- purrr::map_chr(rx_split, ~ paste(rep(.x, 2), sep = "", collapse = " "))
      rx_count_2x <- count_rx_period_parts(rx_split_2x)
      
      ## test to verify rx input strings
      testthat::expect_identical(rx_count_2x$rx_period, rx_split_2x)
      testthat::expect_identical(rx_count_2x$rx_period, c("150 mg over 0.25 in 2 150 mg over 0.25 in 2", "100 mg in 1 over 0.25 q14 x 6 after 28 100 mg in 1 over 0.25 q14 x 6 after 28", "100 mg over 0.25 in 2 q7 x 8 after 0.5 100 mg over 0.25 in 2 q7 x 8 after 0.5"))
      
      ## NOTE: `dose_amount` count max == 1 (b/c syntax rules require
      ## `dose_amount` to be specified at beginning of string)
      testthat::expect_identical(rx_count_2x$dose_amount,  c(1L, 1L, 1L))
      testthat::expect_identical(rx_count_2x$dose_route,   c(0L, 0L, 0L))
      testthat::expect_identical(rx_count_2x$infusion_dur, c(2L, 2L, 2L))
      testthat::expect_identical(rx_count_2x$cmt_number,   c(2L, 2L, 2L))
      testthat::expect_identical(rx_count_2x$dose_freq,    c(0L, 2L, 2L))
      testthat::expect_identical(rx_count_2x$total_doses,  c(0L, 2L, 2L))
      testthat::expect_identical(rx_count_2x$delay_time,   c(0L, 2L, 2L))
      
      ## test input parameters
      testthat::expect_output(count_rx_period_parts(rx_split), NA)
      testthat::expect_output(count_rx_period_parts(rx_split, print_result = FALSE), NA)
      testthat::expect_output(count_rx_period_parts(rx_split, print_result = TRUE), NULL)
      
      
      ## test rx input with all components (single period)
      rx_all_parts <- "1.005e3 mg iv in 2 over 3 q5 x 7 after 1"
      rx_all_count <- count_rx_period_parts(rx_all_parts)
      testthat::expect_identical(nrow(count_rx_period_parts(rx_all_parts)), length(rx_all_parts))
      testthat::expect_identical(rx_all_count$rx_period, rx_all_parts) 
      testthat::expect_identical(rx_all_count$rx_period, "1.005e3 mg iv in 2 over 3 q5 x 7 after 1") 
      testthat::expect_identical(rx_all_count$dose_amount,  1L)
      testthat::expect_identical(rx_all_count$dose_route,   1L)
      testthat::expect_identical(rx_all_count$infusion_dur, 1L)
      testthat::expect_identical(rx_all_count$cmt_number,   1L)
      testthat::expect_identical(rx_all_count$dose_freq,    1L)
      testthat::expect_identical(rx_all_count$total_doses,  1L)
      testthat::expect_identical(rx_all_count$delay_time,   1L)        
    })


### df_has_rx_period_parts() -----------------------------------------------------------
    testthat::test_that("df_has_rx_period_parts", {
      
      ## test default rx input 
      rx_split <- split_rx_dosing_periods(define_default_rx_input())
      rx_parse <- parse_rx_period_parts(rx_split)
      df_rxprt <- df_has_rx_period_parts(rx_parse)
      df_rxprt_names <- c(names(rx_parse), paste0("has_", names(rx_parse)[-1]))
      
      ## dose amount pattern
      testthat::expect_type(df_rxprt, "list")
      testthat::expect_named(df_rxprt, df_rxprt_names)
      testthat::expect_identical(nrow(df_rxprt), length(rx_split))
      testthat::expect_identical(df_rxprt$rx_period, rx_split)
      testthat::expect_identical(df_rxprt$rx_period, c("150 mg over 0.25 in 2", "100 mg in 1 over 0.25 q14 x 6 after 28", "100 mg over 0.25 in 2 q7 x 8 after 0.5"))
      testthat::expect_identical(df_rxprt$dose_amount,  c("150", "100", "100"))
      testthat::expect_identical(df_rxprt$dose_route,   c(NA_character_, NA_character_, NA_character_))
      testthat::expect_identical(df_rxprt$infusion_dur, c("0.25", "0.25", "0.25"))
      testthat::expect_identical(df_rxprt$cmt_number,   c("2", "1", "2"))
      testthat::expect_identical(df_rxprt$dose_freq,    c(NA, "14", "7"))
      testthat::expect_identical(df_rxprt$total_doses,  c(NA, "6", "8"))
      testthat::expect_identical(df_rxprt$delay_time,   c(NA, "28", "0.5"))
      testthat::expect_identical(df_rxprt$has_dose_amount,  c(TRUE,  TRUE,  TRUE))
      testthat::expect_identical(df_rxprt$has_dose_route,   c(FALSE, FALSE, FALSE))
      testthat::expect_identical(df_rxprt$has_infusion_dur, c(TRUE,  TRUE,  TRUE))
      testthat::expect_identical(df_rxprt$has_cmt_number,   c(TRUE,  TRUE,  TRUE))
      testthat::expect_identical(df_rxprt$has_dose_freq,    c(FALSE, TRUE,  TRUE))
      testthat::expect_identical(df_rxprt$has_total_doses,  c(FALSE, TRUE,  TRUE))
      testthat::expect_identical(df_rxprt$has_delay_time,   c(FALSE, TRUE,  TRUE))
      
      
      ## test rx input with all components (single period)
      rx_all_parts <- "1.005e3 mg iv in 2 over 3 q5 x 7 after 1"
      rx_all_parse <- parse_rx_period_parts(rx_all_parts)
      df_all_rxprt <- df_has_rx_period_parts(rx_all_parse)
      testthat::expect_identical(nrow(parse_rx_period_parts(rx_all_parts)), length(rx_all_parts))
      testthat::expect_identical(df_all_rxprt$rx_period, rx_all_parts) 
      testthat::expect_identical(df_all_rxprt$rx_period, "1.005e3 mg iv in 2 over 3 q5 x 7 after 1") 
      testthat::expect_identical(df_all_rxprt$dose_amount,  "1.005e3")
      testthat::expect_identical(df_all_rxprt$dose_route,   "iv")
      testthat::expect_identical(df_all_rxprt$infusion_dur, "3")
      testthat::expect_identical(df_all_rxprt$cmt_number,   "2")
      testthat::expect_identical(df_all_rxprt$dose_freq,    "5")
      testthat::expect_identical(df_all_rxprt$total_doses,  "7")
      testthat::expect_identical(df_all_rxprt$delay_time,   "1")       
      testthat::expect_identical(df_all_rxprt$has_dose_amount,  TRUE)
      testthat::expect_identical(df_all_rxprt$has_dose_route,   TRUE)
      testthat::expect_identical(df_all_rxprt$has_infusion_dur, TRUE)
      testthat::expect_identical(df_all_rxprt$has_cmt_number,   TRUE)
      testthat::expect_identical(df_all_rxprt$has_dose_freq,    TRUE)
      testthat::expect_identical(df_all_rxprt$has_total_doses,  TRUE)
      testthat::expect_identical(df_all_rxprt$has_delay_time,   TRUE) 
    })
    

### validate_rx_part_counts() -----------------------------------------------------------
    testthat::test_that("validate_rx_part_counts", {
      
      ## test default rx input 
      rx_input_default <- split_rx_dosing_periods(define_default_rx_input())
      testthat::expect_type(validate_rx_part_counts(rx_input_default), "logical")
      testthat::expect_length(validate_rx_part_counts(rx_input_default), 1L)
      testthat::expect_true(validate_rx_part_counts(rx_input_default))
      
      ## test input parameters
      testthat::expect_output(validate_rx_part_counts(rx_input_default), NA)
      testthat::expect_output(validate_rx_part_counts(rx_input_default, print_result = FALSE), NA)
      testthat::expect_output(validate_rx_part_counts(rx_input_default, print_result = TRUE), NULL)
      
      ## test single period rx input
      rx_1prd_test <- "100"
      rx_1prd_splt <- split_rx_dosing_periods(rx_1prd_test)
      testthat::expect_type(validate_rx_part_counts(rx_1prd_splt), "logical")
      testthat::expect_length(validate_rx_part_counts(rx_1prd_splt), 1L)
      testthat::expect_true(validate_rx_part_counts(rx_1prd_splt))
      
      ## test 5-period rx input
      rx_5prd_test <- "100 q1 then 200 q2 then 300 q3, 400 q4, 500 q5"
      rx_5prd_splt <- split_rx_dosing_periods(rx_5prd_test)
      testthat::expect_type(validate_rx_part_counts(rx_5prd_splt), "logical")
      testthat::expect_length(validate_rx_part_counts(rx_5prd_splt), 1L)
      testthat::expect_true(validate_rx_part_counts(rx_5prd_splt))
      
      ## test invalid rx input
      testthat::expect_false(validate_rx_part_counts("qx2"))
      testthat::expect_false(validate_rx_part_counts("then"))
      testthat::expect_false(validate_rx_part_counts("100 in 1 in 2"))
      testthat::expect_false(validate_rx_part_counts("100 over 1 over 2"))
      testthat::expect_false(validate_rx_part_counts("100 after 1 after 2"))
      testthat::expect_false(validate_rx_part_counts("100 q2 q10"))
      
      ###################################################################################|
      ## convert invalid rx to valid input by splitting into 2 periods
      ###################################################################################|
      
      ## 2x dosing routes
      testthat::expect_false(validate_rx_part_counts("100 in 1 then 200 in 2"))        ## invalid rx (1 period)
      testthat::expect_false(validate_rx_part_counts("100 in 1 , 200 in 2"))           ## invalid rx (1 period)
      testthat::expect_true(validate_rx_part_counts("100 in 1", "200 in 2"))           ## valid rx   (2 periods)
      
      ## 2x infusion
      testthat::expect_false(validate_rx_part_counts("100 over 1 then 200 over 2"))    ## invalid rx (1 period) 
      testthat::expect_false(validate_rx_part_counts("100 over 1 , 200 over 2"))       ## invalid rx (1 period)
      testthat::expect_true(validate_rx_part_counts("100 over 1", "200 over 2"))       ## valid rx   (2 periods)
      
      ## 2x delay times
      testthat::expect_false(validate_rx_part_counts("100 after 1 then 200 after 2"))  ## invalid rx (1 period)
      testthat::expect_false(validate_rx_part_counts("100 after 1 , 200 after 2"))     ## invalid rx (1 period)
      testthat::expect_true(validate_rx_part_counts("100 after 1", "200 after 2"))     ## valid rx   (2 periods)
      
      ## 2x dosing interval
      testthat::expect_false(validate_rx_part_counts("100 q2 then 200 q10"))           ## invalid rx (1 period)
      testthat::expect_false(validate_rx_part_counts("100 q2, 200 q10"))               ## invalid rx (1 period)
      testthat::expect_true(validate_rx_part_counts("100 q2", "200 q10"))              ## valid rx   (2 periods)
      
      ## 2x total doses
      testthat::expect_false(validate_rx_part_counts("100 x10 then 200 x20"))          ## invalid rx (1 period)
      testthat::expect_false(validate_rx_part_counts("100 x10, 200 x20"))              ## invalid rx (1 period)
      testthat::expect_true(validate_rx_part_counts("100 x10", "200 x20"))             ## valid rx   (2 periods)
    })


### clean_simple_text() -----------------------------------------------------------
    testthat::test_that("clean_simple_text", {
      
      ## test a single text string
      test_text <- "   THIS \n  IS  SOME    \nTEXT   "
      testthat::expect_type(clean_simple_text(test_text, to_lower = FALSE), "character")
      testthat::expect_length(clean_simple_text(test_text, to_lower = FALSE), length(test_text))
      testthat::expect_identical(clean_simple_text(test_text, to_lower = FALSE), "THIS IS SOME TEXT")
      testthat::expect_identical(clean_simple_text(test_text, to_lower = TRUE),  "this is some text")
      
      
      ## test multiple text strings
      multi_str_text <- c("  LINE \n 1   ", "  LINE \n 2   ", "  LINE \n 3   ")
      testthat::expect_type(clean_simple_text(multi_str_text, to_lower = FALSE), "character")
      testthat::expect_length(clean_simple_text(multi_str_text, to_lower = FALSE), length(multi_str_text))
      testthat::expect_identical(clean_simple_text(multi_str_text, to_lower = FALSE), c("LINE 1", "LINE 2", "LINE 3"))
      testthat::expect_identical(clean_simple_text(multi_str_text, to_lower = TRUE),  c("line 1", "line 2", "line 3"))
    })


### split_rx_dosing_periods() -----------------------------------------------------------
    testthat::test_that("split_rx_dosing_periods", {
      
      ## test default rx input 
      testthat::expect_type(split_rx_dosing_periods(define_default_rx_input()), "character")
      testthat::expect_length(split_rx_dosing_periods(define_default_rx_input()), 3L)
      testthat::expect_identical(split_rx_dosing_periods(define_default_rx_input()), c("150 mg over 0.25 in 2", "100 mg in 1 over 0.25 q14 x 6 after 28", 
                                                                                       "100 mg over 0.25 in 2 q7 x 8 after 0.5"))
      
      ## test single period rx input
      rx_1prd_test <- "100"
      testthat::expect_type(split_rx_dosing_periods(rx_1prd_test), "character")
      testthat::expect_length(split_rx_dosing_periods(rx_1prd_test), 1L)
      testthat::expect_identical(split_rx_dosing_periods(rx_1prd_test), rx_1prd_test)
      
      ## test 5-period rx input
      rx_5prd_test <- "100 q1 then 200 q2 then 300 q3, 400 q4, 500 q5"
      testthat::expect_type(split_rx_dosing_periods(rx_5prd_test), "character")
      testthat::expect_length(split_rx_dosing_periods(rx_5prd_test), 5L)
      testthat::expect_identical(split_rx_dosing_periods(rx_5prd_test), c("100 q1", "200 q2", "300 q3", "400 q4", "500 q5"))
      testthat::expect_identical(split_rx_dosing_periods(rx_5prd_test), trimws(unlist(strsplit(rx_5prd_test, split = ",|then")), which = "both"))
    })

#######################################################################################################################|
## Run model simulations from Rx input ---------------------------------------------------------------------------------
#######################################################################################################################|

### plot_sim_model_ev_data_dosing_regimen() -----------------------------------------------------------
    testthat::test_that("plot_sim_model_ev_data_dosing_regimen", {
      
      ## `plot_sim_model_ev_data_dosing_regimen()` <helpers> takes output from: -
      ##  - `sim_dosing_dataset()` eventReactive <server>, which calls `run_sim_model_ev_data_dosing_regimen()` <helpers>
      
      ## verify consistent outputs for default library model
      def_mod <- app_libmod_objs[[fetch_default_libmod_name()]]
      def_ev_data <- fill_default_ev_data_cols(ev_data = mrgsolve::ev_rx(define_default_rx_input())) %>% 
        mutate(REGNUM = 1L)
      def_sim_prview <- preview_rx_input_profile(mod = def_mod, ev_data = def_ev_data)
      plt_sim_prview <- plot_sim_model_ev_data_dosing_regimen(def_sim_prview)
      
      cur_id_col <- "ID"
      cur_time_col <- mrgsolve:::timename(def_sim_prview@data)
      sim_ev_range <- range(def_sim_prview@data[[cur_time_col]], na.rm = TRUE)
      sim_id_count <- length(unique(def_sim_prview@data[[cur_id_col]]))
      
      
      def_sim_cp_var <- def_sim_prview
      def_sim_cp_var@data <- def_sim_prview@data %>% rename(CP = DV)
      plt_sim_cp_var <- plot_sim_model_ev_data_dosing_regimen(def_sim_cp_var)
      
      def_sim_ipred <- def_sim_prview
      def_sim_ipred@data <- def_sim_prview@data %>% select(-DV)
      plt_sim_ipred <- plot_sim_model_ev_data_dosing_regimen(def_sim_ipred)
      
      def_sim_last_col <- def_sim_prview
      def_sim_last_col@data <- def_sim_prview@data %>% rename(RESP = IPRED) %>% select(-DV)
      plt_sim_last_col <- plot_sim_model_ev_data_dosing_regimen(def_sim_last_col)
      
      
      ## test rx profile output plots
      testthat::expect_type(plt_sim_prview, "list")
      testthat::expect_identical(plt_sim_prview$xlab, cur_time_col)
      testthat::expect_identical(plt_sim_prview$ylab, "DV")
      testthat::expect_identical(plt_sim_prview$panel.args[[1]]$x, def_sim_prview@data[[cur_time_col]])
      testthat::expect_identical(plt_sim_prview$panel.args[[1]]$y, def_sim_prview@data[[plt_sim_prview$ylab]])
      
      testthat::expect_identical(plt_sim_cp_var$xlab, cur_time_col)
      testthat::expect_identical(plt_sim_cp_var$ylab, "CP")
      testthat::expect_identical(plt_sim_cp_var$panel.args[[1]]$x, def_sim_cp_var@data[[cur_time_col]])
      testthat::expect_identical(plt_sim_cp_var$panel.args[[1]]$y, def_sim_cp_var@data[[plt_sim_cp_var$ylab]])
      
      testthat::expect_identical(plt_sim_ipred$xlab, cur_time_col)
      testthat::expect_identical(plt_sim_ipred$ylab, "IPRED")
      testthat::expect_identical(plt_sim_ipred$panel.args[[1]]$x, def_sim_ipred@data[[cur_time_col]])
      testthat::expect_identical(plt_sim_ipred$panel.args[[1]]$y, def_sim_ipred@data[[plt_sim_ipred$ylab]])
      
      testthat::expect_identical(plt_sim_last_col$xlab, cur_time_col)
      testthat::expect_identical(plt_sim_last_col$ylab, "RESP")
      testthat::expect_identical(plt_sim_last_col$panel.args[[1]]$x, def_sim_last_col@data[[cur_time_col]])
      testthat::expect_identical(plt_sim_last_col$panel.args[[1]]$y, def_sim_last_col@data[[plt_sim_last_col$ylab]])
      
      
      ## define multiple dosing regimen test data with all regimens included
      df_regs <- data.frame(REGNUM = 1L:4L,
                            REGLAB = c("REGN1", "REGN2", "REGN3", "REGN4"),
                            Include = c(TRUE, TRUE, TRUE, TRUE),
                            RXLABEL = c("150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5",
                                        "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28",
                                        "150 mg over 0.25 in 2",
                                        "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5"))
      
      df_regnums <- df_regs %>% filter(Include %in% TRUE) %>% pull(REGNUM) %>% unique()
      
      ## test `preview_rx_input_profile()` with multiple dosing regimen input data
      test_df_regs_info <- rx_table_to_multi_reg_data(df_regs = df_regs)
      test_rx_regs_prof <- preview_rx_input_profile(ev_data = test_df_regs_info, mod = def_mod)
      test_rx_regs_data <- test_rx_regs_prof@data
      testthat::expect_error(plot_sim_model_ev_data_dosing_regimen(test_rx_regs_prof), NA)
      test_rx_regs_plot <- plot_sim_model_ev_data_dosing_regimen(test_rx_regs_prof)
      
      ## test rx profile output plot with multiple dosing regimens
      testthat::expect_type(test_rx_regs_plot, "list")
      testthat::expect_s3_class(test_rx_regs_plot, "trellis")
      testthat::expect_identical(test_rx_regs_plot$xlab, "TIME")
      testthat::expect_identical(test_rx_regs_plot$ylab, "DV")
      testthat::expect_identical(test_rx_regs_plot$panel.args[[1]]$x, test_rx_regs_data$TIME)
      testthat::expect_identical(test_rx_regs_plot$panel.args[[1]]$y, test_rx_regs_data[[plt_sim_prview$ylab]])
      
      
      ## change the dependent variable in the data/model to "CP"
      test_rx_regs_prof_cp <- test_rx_regs_prof
      test_rx_regs_prof_cp@data <- test_rx_regs_prof@data %>% rename(CP = DV)
      test_rx_regs_plot_cp <- plot_sim_model_ev_data_dosing_regimen(test_rx_regs_prof_cp)
      testthat::expect_identical(test_rx_regs_plot_cp$xlab, "TIME")
      testthat::expect_identical(test_rx_regs_plot_cp$ylab, "CP")
      testthat::expect_identical(test_rx_regs_plot_cp$panel.args[[1]]$x, test_rx_regs_prof_cp@data$TIME)
      testthat::expect_identical(test_rx_regs_plot_cp$panel.args[[1]]$y, test_rx_regs_prof_cp@data[[test_rx_regs_plot_cp$ylab]])
      
      ## change the dependent variable in the data/model to "IPRED"
      test_rx_regs_prof_ipred <- test_rx_regs_prof
      test_rx_regs_prof_ipred@data <- test_rx_regs_prof@data %>% select(-DV)
      test_rx_regs_plot_ipred <- plot_sim_model_ev_data_dosing_regimen(test_rx_regs_prof_ipred)
      testthat::expect_identical(test_rx_regs_plot_ipred$xlab, "TIME")
      testthat::expect_identical(test_rx_regs_plot_ipred$ylab, "IPRED")
      testthat::expect_identical(test_rx_regs_plot_ipred$panel.args[[1]]$x, test_rx_regs_prof_ipred@data$TIME)
      testthat::expect_identical(test_rx_regs_plot_ipred$panel.args[[1]]$y, test_rx_regs_prof_ipred@data[[test_rx_regs_plot_ipred$ylab]])
      
      
      ## change the dependent variable in the data/model to "RESP"
      test_rx_regs_prof_resp <- test_rx_regs_prof
      test_rx_regs_prof_resp@data <- test_rx_regs_prof@data %>% rename(RESP = DV) %>% select(-IPRED)
      test_rx_regs_plot_resp <- plot_sim_model_ev_data_dosing_regimen(test_rx_regs_prof_resp)
      testthat::expect_identical(test_rx_regs_plot_resp$xlab, "TIME")
      testthat::expect_identical(test_rx_regs_plot_resp$ylab, "RESP")
      testthat::expect_identical(test_rx_regs_plot_resp$panel.args[[1]]$x, test_rx_regs_prof_resp@data$TIME)
      testthat::expect_identical(test_rx_regs_plot_resp$panel.args[[1]]$y, test_rx_regs_prof_resp@data[[test_rx_regs_plot_resp$ylab]])
    })

### preview_rx_input_profile() -----------------------------------------------------------
    testthat::test_that("preview_rx_input_profile", {
      
      ## previous Rx profile functions in <helpers>
      run_sim_model_ev_data_dosing_regimen <- function(ev_data, mod, start = 0, end_time = NULL, delta = 1.0, add = numeric(0)) {
        mod_tv <- mrgsolve::zero_re(mod)
        mod_sim_data <- sim_model_ev_data_dosing_regimen(mod = mod_tv, ev_data = ev_data, start = start, end_time = end_time, delta = delta, add = add)
        return(mod_sim_data)
      }
      
      sim_model_ev_data_dosing_regimen <- function(mod, ev_data, start = 0, end_time = NULL, delta = 1.0, add = numeric(0)) {
        df_ev <- ev_cols_tolower(fill_default_ev_data_cols(ev_data)) %>% rename_all(toupper)
        df_ev_cols <- names(df_ev)
        if (!"ID" %in% df_ev_cols) df_ev <- df_ev %>% dplyr::mutate(ID = 1L)
        if ("RATE" %in% df_ev_cols) df_ev <- df_ev %>% tidyr::replace_na(list(RATE = 0))
        if (!"REGNUM" %in% df_ev_cols) df_ev <- df_ev %>% dplyr::mutate(REGNUM = ID)
        if (isTRUE(is.null(end_time))) {
          df_ev_regs <- df_ev %>% rename_all(toupper) %>% group_by(REGNUM) %>% group_split()
          end_time <- max(purrr::map_dbl(df_ev_regs, ~ calc_dosing_end_time(dose_data = .x, delta = delta)), na.rm = TRUE)
        }
        mod_update1 <- update_mrgsolve_solver(mod = mod)
        mod_update2 <- update_mrgsolve_tgrid(mod = mod_update1, start = start, end = end_time, delta = delta, add = add)
        mod_sim_data <- mrgsolve::mrgsim_d(mod_update2, df_ev, carry_out = def_carry_sim_cols())
        return(mod_sim_data)
      }
      
      def_carry_sim_cols <- function() {
        c(names(define_default_ev_data_cols()), "REGNUM")
      } 
      
      update_mrgsolve_solver <- function(mod) {
        mrgsolve::update(mod, rtol = 1e-10, atol = 1e-12, ss_rtol = 1e-10, ss_atol = 1e-12, maxsteps = 10000)
      }
      
      update_mrgsolve_tgrid <- function(mod, start, end, delta, add) {
        tg  <- mrgsolve::tgrid(start = start, end = end, delta = delta, add = add)
        mrgsolve::update(mod, start = tg@start, end = tg@end, delta = tg@delta, add = tg@add, offset = tg@offset, tscale = tg@scale)
      }
      
      calc_regimen_end_time <- function(df_ev, end = 24, delta = 1.0) {
        max(purrr::map_dbl(df_ev %>% rename_all(toupper) %>% group_by(REGNUM) %>% group_split(), ~ calc_dosing_end_time(dose_data = .x, delta = delta)), na.rm = TRUE)
      }
      
      ## fn-call previously in `sim_dosing_dataset()` eventReactive <server> function:  
      ##   - run_sim_model_ev_data_dosing_regimen(ev_data = rx_dose_data(), mod = load_model_file())
      
      
      ## verify consistent outputs for default library model
      def_mod <- app_libmod_objs[[fetch_default_libmod_name()]]
      def_ev_data <- fill_default_ev_data_cols(ev_data = mrgsolve::ev_rx(define_default_rx_input())) %>% 
        mutate(REGNUM = 1L)
      end_ev_time <- calc_regimen_end_time(def_ev_data)
      min_ev_time <- min(def_ev_data$TIME, na.rm = TRUE)
      
      def_sim_output <- run_sim_model_ev_data_dosing_regimen(ev_data = def_ev_data, mod = def_mod)
      def_sim_prview <- preview_rx_input_profile(mod = def_mod, ev_data = def_ev_data)
      
      cur_id_col <- "ID"
      cur_time_col <- mrgsolve:::timename(def_sim_prview@data)
      sim_ev_range <- range(def_sim_prview@data[[cur_time_col]], na.rm = TRUE)
      sim_id_count <- length(unique(def_sim_prview@data[[cur_id_col]]))
      
      ## test rx profile output datasets
      testthat::expect_s4_class(def_sim_prview, "mrgsims")
      testthat::expect_named(def_sim_prview, c(def_carry_sim_cols(), unname(unlist(outvars(def_mod)))), ignore.order = TRUE)
      testthat::expect_named(def_sim_prview, c(def_carry_sim_cols(), def_sim_prview@request, def_sim_prview@outnames), ignore.order = TRUE)
      testthat::expect_identical(min_ev_time, min(sim_ev_range, na.rm = TRUE))
      testthat::expect_identical(end_ev_time, max(sim_ev_range, na.rm = TRUE))
      testthat::expect_identical(length(unique(def_ev_data[[cur_id_col]])), sim_id_count)
      testthat::expect_identical(def_sim_prview, def_sim_output)
      testthat::expect_error(preview_rx_input_profile(mod = def_mod, ev_data = def_ev_data %>% select(-REGNUM)), NA)
      testthat::expect_identical(preview_rx_input_profile(mod = def_mod, ev_data = def_ev_data %>% select(-REGNUM)), def_sim_prview)
      
      
      ## verify consistent outputs for all library models
      libmods_sim_output <- map(app_libmod_objs, ~ run_sim_model_ev_data_dosing_regimen(ev_data = def_ev_data, mod = .x))
      libmods_sim_prview <- map(app_libmod_objs, ~ preview_rx_input_profile(mod = .x, ev_data = def_ev_data))
      
      ## test rx profile output datasets
      testthat::expect_identical(libmods_sim_prview, libmods_sim_output)
      
      
      
      ## define multiple dosing regimen test data with all regimens included
      df_regs <- data.frame(REGNUM = 1L:4L,
                            REGLAB = c("REGN1", "REGN2", "REGN3", "REGN4"),
                            Include = c(TRUE, TRUE, TRUE, TRUE),
                            RXLABEL = c("150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5",
                                        "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28",
                                        "150 mg over 0.25 in 2",
                                        "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5"))
      
      df_regnums <- df_regs %>% filter(Include %in% TRUE) %>% pull(REGNUM) %>% unique()
      
      ## test `preview_rx_input_profile()` with multiple dosing regimen input data
      test_df_regs_info <- rx_table_to_multi_reg_data(df_regs = df_regs)
      test_rx_regs_prof <- preview_rx_input_profile(ev_data = test_df_regs_info, mod = def_mod)
      test_rx_regs_data <- test_rx_regs_prof@data
      expc_rx_regs_prof <- run_sim_model_ev_data_dosing_regimen(ev_data = test_df_regs_info, mod = def_mod)
      expc_rx_regs_data <- expc_rx_regs_prof@data
      
      ## calculate test results
      end_regs_time <- calc_regimen_end_time(test_df_regs_info)
      min_regs_time <- min(test_df_regs_info$TIME, na.rm = TRUE)
      
      ## calculate expected results
      regs_time_range <- range(expc_rx_regs_data$TIME, na.rm = TRUE)
      regs_id_count <- length(unique(expc_rx_regs_data$ID))
      
      ## test rx profile output datasets
      testthat::expect_s4_class(test_rx_regs_prof, "mrgsims")
      testthat::expect_s3_class(test_rx_regs_data, "data.frame")
      testthat::expect_named(test_rx_regs_data, c(def_carry_sim_cols(), unname(unlist(outvars(def_mod)))), ignore.order = TRUE)
      testthat::expect_named(test_rx_regs_data, c(def_carry_sim_cols(), test_rx_regs_prof@request, test_rx_regs_prof@outnames), ignore.order = TRUE)
      testthat::expect_identical(min_regs_time, min(regs_time_range, na.rm = TRUE))
      testthat::expect_identical(end_regs_time, max(regs_time_range, na.rm = TRUE))
      testthat::expect_identical(length(unique(test_df_regs_info$ID)), regs_id_count)
      testthat::expect_identical(as.integer(unique(test_rx_regs_data$REGNUM)), df_regnums)
      testthat::expect_identical(as.integer(unique(test_rx_regs_data$REGNUM)), as.integer(unique(expc_rx_regs_data$REGNUM)))
      testthat::expect_identical(test_rx_regs_data, expc_rx_regs_data)
      
      
      ## define multiple dosing regimen test data with at least 1 regimen excluded
      df_regs_drop_4 <- data.frame(REGNUM = 1L:4L,
                                   REGLAB = c("REGN1", "REGN2", "REGN3", "REGN4"),
                                   Include = c(TRUE, TRUE, TRUE, FALSE),
                                   RXLABEL = c("150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5",
                                               "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28",
                                               "150 mg over 0.25 in 2",
                                               "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5"))
      
      df_regnums_drop_4 <- df_regs_drop_4 %>% filter(Include %in% TRUE) %>% pull(REGNUM) %>% unique()
      
      ## test `preview_rx_input_profile()` with multiple dosing regimen input data
      test_df_regs_info_drop_4 <- rx_table_to_multi_reg_data(df_regs = df_regs_drop_4)
      test_rx_regs_prof_drop_4 <- preview_rx_input_profile(ev_data = test_df_regs_info_drop_4, mod = def_mod)
      test_rx_regs_data_drop_4 <- test_rx_regs_prof_drop_4@data
      expc_rx_regs_prof_drop_4 <- run_sim_model_ev_data_dosing_regimen(ev_data = test_df_regs_info_drop_4, mod = def_mod)
      expc_rx_regs_data_drop_4 <- expc_rx_regs_prof_drop_4@data
      
      ## calculate test results
      end_regs_time_drop_4 <- calc_regimen_end_time(test_df_regs_info_drop_4)
      min_regs_time_drop_4 <- min(test_df_regs_info_drop_4$TIME, na.rm = TRUE)
      
      ## calculate expected results
      regs_time_range_drop_4 <- range(expc_rx_regs_data_drop_4$TIME, na.rm = TRUE)
      regs_id_count_drop_4 <- length(unique(expc_rx_regs_data_drop_4$ID))
      
      ## test rx profile output datasets
      testthat::expect_s4_class(test_rx_regs_prof_drop_4, "mrgsims")
      testthat::expect_s3_class(test_rx_regs_data_drop_4, "data.frame")
      testthat::expect_named(test_rx_regs_data_drop_4, c(def_carry_sim_cols(), unname(unlist(outvars(def_mod)))), ignore.order = TRUE)
      testthat::expect_named(test_rx_regs_data_drop_4, c(def_carry_sim_cols(), test_rx_regs_prof_drop_4@request, test_rx_regs_prof_drop_4@outnames), ignore.order = TRUE)
      testthat::expect_identical(min_regs_time_drop_4, min(regs_time_range, na.rm = TRUE))
      testthat::expect_identical(end_regs_time_drop_4, max(regs_time_range, na.rm = TRUE))
      testthat::expect_identical(length(unique(test_df_regs_info_drop_4$ID)), regs_id_count_drop_4)
      testthat::expect_identical(as.integer(unique(test_rx_regs_data_drop_4$REGNUM)), df_regnums_drop_4)
      testthat::expect_identical(as.integer(unique(test_rx_regs_data_drop_4$REGNUM)), as.integer(unique(expc_rx_regs_data_drop_4$REGNUM)))
      testthat::expect_identical(test_rx_regs_data_drop_4, expc_rx_regs_data_drop_4)

      
    })
    

#######################################################################################################################|
## utility functions ---------------------------------------------------------------------------------------------------
#######################################################################################################################|

## NO TESTS ##


#######################################################################################################################|
## additional template function placeholders ---------------------------------------------------------------------------
#######################################################################################################################|

### replace_template_function() -----------------------------------------------------------



### replace_template_function() -----------------------------------------------------------



### replace_template_function() -----------------------------------------------------------



### replace_template_function() -----------------------------------------------------------



### replace_template_function() -----------------------------------------------------------



### replace_template_function() -----------------------------------------------------------



### replace_template_function() -----------------------------------------------------------



### replace_template_function() -----------------------------------------------------------



### replace_template_function() -----------------------------------------------------------



### replace_template_function() -----------------------------------------------------------



### replace_template_function() -----------------------------------------------------------



### replace_template_function() -----------------------------------------------------------

  
})

