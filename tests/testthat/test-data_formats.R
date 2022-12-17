## print log debugging statements
print("test-data_formats.R")
print(getwd())


## docs for `testthat::source_*` functions:  https://testthat.r-lib.org/reference/source_file.html
here::i_am("tests/testthat/test-data_formats.R")
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
## SETUP --------------------------------------------------------------------------------
#########################################################################################|

mod <- app_libmod_objs[[fetch_default_libmod_name()]]
dose_data  <- fill_default_ev_data_cols(ev_data = mrgsolve::ev_rx(define_default_rx_input()))
sim_settings <- fetch_default_sim_settings()
dataset <- replicate_dose_data_ids(dose_data = dose_data, num_subj = sim_settings$num_subj)
tg <- adjust_time_grid_dosing_fn(dose_data = dose_data, tg_input = tgrid(), scaling_factor = define_tgrid_end_scaling_factor())
sim_out_data <- expand_sim_input_data(mod = mod, dataset = dataset, tg = tg, sim_seed = sim_settings$sim_seed)


#########################################################################################|
## shiny app data_formats.R functions ----------------------------------------------------
#########################################################################################|


## extract_model_cmt_names() -------------------------------------------------------------------
testthat::test_that("extract_model_cmt_names is a character vector and returns appropriate outputs for default model", {
 testthat::expect_equal(extract_model_cmt_names(mod), as.character(c("ABS","CENT","PERIPH")))
})



## extract_model_capture_vars() -------------------------------------------------------------------
testthat::test_that("extract_model_capture_vars is a character vector and returns appropriate outputs for default model", {
  testthat::expect_equal(extract_model_capture_vars(mod), as.character(c("DOSE", "WT", "WTREF", "DV", "IPRED")))
})



## default_model_output_var() ------------------------------------------------------------------- 
testthat::test_that("default_model_output_var .....", {
  testthat::expect_equal(default_model_output_var(mod), as.character("DV"))
})


## format_sim_output_data() ------------------------------------------------------------------- 
## test if this function builds a sim template dataset for sim output.
testthat::test_that("format_sim_output_data ......",  {
  ## test if sim_out_data contains any of the cmt vars -- test if function is dropping mod$cmt cols.
  x<-names(sim_out_data)
  y<-extract_model_cmt_names(mod)
  contains<-any('x'== 'y')
  testthat::expect_false(contains)
  testthat::expect_length(x, 16)
  testthat::expect_length(y, 3)
  ## test if the dosing regimen labels are added to the dataframe this function generates
  z<-c("REGNUM", "REGLAB")
  testthat::expect_named(z, NULL)
  ## test if the dataset before (using the dosing regimen 'dataset' contains a different length)
  n<-names(dataset)
  testthat::expect_length(n, 11)
  ## test if format_sim_output_data adds REGNUM/REGLAB columns
  sim_out_data.noreg<-sim_out_data
  sim_out_data.noreg<-select(sim_out_data.noreg, -REGNUM, -REGLAB)
  sim_out_data.addreg<-format_sim_output_data(sim_out_data.noreg)
  testthat::expect_true(any(str_detect(names(sim_out_data.addreg), "REGNUM")))
  testthat::expect_true(any(str_detect(names(sim_out_data.addreg), "REGLAB")))
  ## test if drop_cols returns cmt variables if we don't pass anything
  test_drop_cols<-format_sim_output_data(sim_out_data.noreg)
  t<-names(test_drop_cols)
  testthat::expect_false(any('t'== 'y'))
  ## test if drop_cols drops cols that are used as arg
  randcmt<-c("ID", "TIME", "eee") # here eee is random purposely
  testrandcmt<-format_sim_output_data(sim_out_data.noreg, drop_cols=randcmt)
  b<-names(testrandcmt)
  ## expect everything but ID and TIME
  testthat::expect_equal(b, c("AMT", "RATE","EVID","CMT","ADDL" ,"II","SS","DOSE","WT","WTREF","DV","IPRED","REGLAB","REGNUM"))
})



## add_dose_regimen_label() ------------------------------------------------------------------- 
testthat::test_that("add_dose_regimen_label ......",  {
  ## test if this function adds REGLAB and REGNUM with intended values REGN1 and 1
  adrl<-add_dose_regimen_label(dose_data) 
  testthat::expect_false("REGLAB" %in% colnames(dose_data))
  testthat::expect_false("REGNUM" %in% colnames(dose_data))
  testthat::expect_true("REGLAB" %in% colnames(adrl))
  testthat::expect_true("REGNUM" %in% colnames(adrl))
  ## test if this function adds the variables with the intended values REGN1 and 1
  testthat::expect_true(any(adrl$REGLAB=="REGN1"))
  testthat::expect_true(any(adrl$REGNUM==1))
})



## select_sim_output_variable_fn() ------------------------------------------------------------------- 
testthat::test_that("select_sim_output_variable_fn ......",  {
  ## test if YDATA and YNAME variables are appended to sim_out_data using this function
  df1 <- select_sim_output_variable_fn(sim_out_data)
  testthat::expect_true("YNAME" %in% colnames(df1))
  testthat::expect_true("YDATA" %in% colnames(df1))
  ## test if it defaults to YNAME=DV and contains YDATA=VALUES of DV
  testthat::expect_true(any(df1$YNAME=="DV"))
  df1$is_match = df1$DV %in% df1$YDATA
  testthat::expect_true(all(df1$is_match))
})



## list_valid_output_vars_fn() -------------------------------------------------------------------
testthat::test_that("list_valid_output_vars_fn ......",  {
  ## already tested if func extract_model_capture_vars() is returning capture vars.
  ## detect if all values contained in output of extract_model_capture_vars() and intersect between sim_out_data of loaded model are identical
  x <- extract_model_capture_vars(mod)
  y <- intersect(x, names(sim_out_data))
  testthat::expect_setequal(x, rev(y))
  ## check if reserved_library_model_params() excludes everything that's not a reserved library model param in the loaded model (gives DV's)
  z <- reserved_library_model_params()
  ## if x and y are identical, then z should not be identical
  testthat::expect_false(any('y'== 'z'))
  ## detect the model-specific dependent variables in reserved_library_model_params()
  q <- all(stringr::str_detect(z,'IPRED|DV'))
  testthat::expect_false(q)
  ## check if setdiff between output of list_valid_output_vars_fn(): `out_vars_list` returns the dependent variables located in model spec file
  dv <- c("IPRED", "DV")
  dif <- setdiff(list_valid_output_vars_fn(mod), reserved_library_model_params())
  testthat::expect_setequal(dv, rev(dif))
})


})
