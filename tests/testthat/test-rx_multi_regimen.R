## print log debugging statements
print("test-rx_multi_regimen.R")
print(getwd())


## docs for `testthat::source_*` functions:  https://testthat.r-lib.org/reference/source_file.html
here::i_am("tests/testthat/test-rx_multi_regimen.R")
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
    
    
    ###########################################################################|
    ## shiny app <rx_multi_regimen> functions ----------------------------------
    ###########################################################################|
    
    
    ## strict_left_join() ------------------------------------------------------
    testthat::test_that("strict_left_join", {
      ## source:  https://github.com/tidyverse/dplyr/issues/2278
      df1 <- data.frame(day = c(1, 2, 1, 1), month = c("Jan", "Jan",  "Jan", "Feb"))
      df2 <- data.frame(df1[-1, ], year = 2016)
      df3 <- data.frame(day = c(1, 2, NA_real_, 1), month = c("Jan", "Jan",  "Jan", "Feb"))
      
      testthat::expect_identical(strict_left_join(df1, df2, by = c("day", "month")), dplyr::left_join(df1, df2, by = c("day", "month")))
      testthat::expect_error(strict_left_join(df2, df1, by = c("day", "month")), "Duplicate values in foreign key")
      testthat::expect_error(strict_left_join(df1, df2, by = "day"), "Duplicate values in foreign key")
      testthat::expect_error(strict_left_join(df1, rbind(df2, df2), by = c("day", "month")), "Duplicate values in foreign key")
    })
    
    
    
    ## length_plus_1() ---------------------------------------------------------
    testthat::test_that("length_plus_1", {
      testthat::expect_error(length_plus_1(), 'argument "x" is missing, with no default')
      
      test_cases <- list(NULL, NaN, NA_character_, NA_complex_, NA_integer_, NA_real_, NA, 
                         character(0), complex(0), integer(0), numeric(0), logical(0), 
                         data.frame(), c(), list(), c(1, 2, 3), c("str1", "str2", "str3", "str4"))
      
      test_results <- purrr::map_int(test_cases, length) + 1L
      testthat::expect_identical(purrr::map_int(test_cases, length_plus_1), test_results)
    })
    
    
    ## max_plus_1() ------------------------------------------------------------
    testthat::test_that("max_plus_1", {
      
      ## test bare call to `max_plus_1` function
      testthat::expect_error(max_plus_1(), 'argument "x" is missing, with no default')
      
      ## test error cases
      error_cases <- list(NA_character_, NA_complex_, NA, 
                          character(0), complex(0), logical(0), 
                          data.frame(), list(), c("str1", "str2", "str3", "str4"))
      
      purrr::walk(error_cases, ~ testthat::expect_error(max_plus_1(.x), "input argument `x` must be a numeric type!"))
      
      
      ## test use cases
      test_cases <- list(`1` = NULL, `1` = NaN, `1` = NA_integer_, `1` = NA_real_, 
                         `1` = integer(0), `1` = numeric(0), `1` = c(),
                         `4` = c(1, 2, 3), `-9` = -10, `1` = 0, `101` = c(0, 1, 2, 100), 
                         `11` = c(NA_real_, NA_real_, 10, NA_real_), 
                         `1` = c(NA_real_, NA_real_, NA_real_))
      
      test_results <- as.integer(names(test_cases))
      testthat::expect_identical(purrr::map_int(unname(test_cases), max_plus_1), test_results)
    })
    
    
    
    ## build_default_regimen_label() -------------------------------------------
    testthat::test_that("build_default_regimen_label", {
      
      ## test default return value
      default_label <- build_default_regimen_label()
      testthat::expect_type(default_label, "character")
      testthat::expect_length(default_label, 1L)
      testthat::expect_identical(default_label, "REGN1")
      
      ## test intended use cases
      testthat::expect_identical(build_default_regimen_label(label_str = "TEST"), "TEST1")
      testthat::expect_identical(build_default_regimen_label(label_str = "TEST", regimen_id = 123), "TEST123")
      testthat::expect_identical(build_default_regimen_label(label_str = "<label-string>", regimen_id = "<numeric-id>"), "<label-string><numeric-id>")
      
      ## test empty, missing, NULL, & other problematic inputs
      testthat::expect_identical(build_default_regimen_label(label_str = NULL), "1")
      testthat::expect_identical(build_default_regimen_label(label_str = NULL, regimen_id = NULL), character(0))
      testthat::expect_identical(build_default_regimen_label(label_str = character(0)), "1")
      testthat::expect_identical(build_default_regimen_label(label_str = character(0), regimen_id = character(0)), character(0))
      testthat::expect_identical(build_default_regimen_label(label_str = numeric(0), regimen_id = numeric(0)), character(0))
      testthat::expect_identical(build_default_regimen_label(label_str = logical(0), regimen_id = logical(0)), character(0))
      testthat::expect_identical(build_default_regimen_label(label_str = NA_character_), "NA1")
      testthat::expect_identical(build_default_regimen_label(label_str = NA_character_, regimen_id = NA_character_), "NANA")
      testthat::expect_identical(build_default_regimen_label(label_str = NA, regimen_id = NA), "NANA")
      testthat::expect_identical(build_default_regimen_label(label_str = NaN), "NaN1")
      testthat::expect_identical(build_default_regimen_label(label_str = NaN, regimen_id = NaN), "NaNNaN")
      testthat::expect_identical(build_default_regimen_label(label_str = character(1)), "1")
      testthat::expect_identical(build_default_regimen_label(label_str = character(1), regimen_id = character(1)), "")
    })
    
    ## empty_rx_table() --------------------------------------------------------
    testthat::test_that("empty_rx_table", {
      testthat::expect_error(empty_rx_table(), NA)
      testthat::expect_type(empty_rx_table(), "list")
      testthat::expect_s3_class(empty_rx_table(), "tbl_df")
      testthat::expect_named(empty_rx_table(), 
                             c("REGNUM", "REGLAB", "Include", "RXLABEL", "RXDOSE"), 
                             ignore.order = TRUE)
      testthat::expect_identical(empty_rx_table(), 
                                 tibble::tibble(REGNUM  = integer(0),
                                                REGLAB  = character(0),
                                                Include = logical(0),
                                                RXLABEL = character(0), 
                                                RXDOSE  = list(tibble::tibble())))
    })
    
    
    ## add_new_rx_regimen() ----------------------------------------------------
    testthat::test_that("add_new_rx_regimen", {
      
      ## test individual function calls to `add_new_rx_regimen()`
      rx_label_add <- list(
        REGN1 = define_default_rx_input(),
        REGN2 = "400 mg in 2 q14 x 6",
        REGN3 = "300 mg in 1 q7 x 10"
      )
      
      build_rx_regs_data <- function(rx_label_add, rx_label_idx) {
        tibble::tibble(REGNUM  = as.integer(rx_label_idx),
                       REGLAB  = names(rx_label_add)[rx_label_idx],
                       Include = TRUE,
                       RXLABEL = unname(unlist(rx_label_add[rx_label_idx])), 
                       RXDOSE  = purrr::map(unname(unlist(rx_label_add[rx_label_idx])), submit_rx_input_fn))
      }
      
      
      ## verify that `add_new_rx_regimen()` runs without an error for valid Rx input labels
      testthat::expect_error(add_new_rx_regimen(df_regs = empty_rx_table(), rx_label = define_default_rx_input()), NA)
      
      ## verify that `add_new_rx_regimen()` produces an error for invalid Rx input strings
      invalid_rx_label <- "500 mg in 1,,"  ## 2 trailing commas result in an empty dosing period (invalid syntax)
      testthat::expect_error(add_new_rx_regimen(df_regs = empty_rx_table(), rx_label = invalid_rx_label))
      
      
      ## sequentially add dosing regimens
      df_regs_x1 <- add_new_rx_regimen(df_regs = empty_rx_table(), rx_label = define_default_rx_input())
      df_regs_x2 <- add_new_rx_regimen(df_regs = df_regs_x1, rx_label = "400 mg in 2 q14 x 6")
      df_regs_x3 <- add_new_rx_regimen(df_regs = df_regs_x2, rx_label = "300 mg in 1 q7 x 10")
      
      testthat::expect_type(df_regs_x1, "list")
      testthat::expect_s3_class(df_regs_x1, "tbl_df")
      testthat::expect_named(df_regs_x1, c("REGNUM", "REGLAB", "Include", "RXLABEL", "RXDOSE"), ignore.order = TRUE)
      testthat::expect_identical(df_regs_x1, build_rx_regs_data(rx_label_add, 1L))
      testthat::expect_identical(df_regs_x2, build_rx_regs_data(rx_label_add, 1L:2L))
      testthat::expect_identical(df_regs_x3, build_rx_regs_data(rx_label_add, 1L:3L))
      
      
      ## test function calls to `add_new_rx_regimen()` in a single pipeline
      df_regs_pipe <- empty_rx_table() %>% 
        add_new_rx_regimen(rx_label = define_default_rx_input()) %>% 
        add_new_rx_regimen(rx_label = "400 mg in 2 q14 x 6") %>% 
        add_new_rx_regimen(rx_label = "300 mg in 1 q7 x 10")
      
      testthat::expect_identical(df_regs_pipe, build_rx_regs_data(rx_label_add, 1L:3L))
      
      
      #########################################################################################|
      ### setup test `rx_table$df_regs` data structure with nested dosing event datasets -------
      #########################################################################################|
      
      df_regs_manual <- tibble::tibble(
        REGNUM = 1L:4L,
        REGLAB = c("REGN1", "REGN2", "REGN3", "REGN4"),
        Include = c(TRUE, TRUE, TRUE, TRUE),
        RXLABEL = c("150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5",
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28",
                    "150 mg over 0.25 in 2",
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5")) %>%
        mutate(RXDOSE = purrr::map(RXLABEL, submit_rx_input_fn))
      
      df_regs_test <- empty_rx_table() %>%
        add_new_rx_regimen(rx_label = "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5") %>%
        add_new_rx_regimen(rx_label = "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28") %>%
        add_new_rx_regimen(rx_label = "150 mg over 0.25 in 2") %>%
        add_new_rx_regimen(rx_label = "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5")
      
      testthat::expect_identical(df_regs_test, df_regs_manual)
    })
    
    
    ## parse_hot_changes() -----------------------------------------------------
    testthat::test_that("parse_hot_changes", {
      #########################################################################|
      ### test adding new Rx regimen entry to existing `rx_table$df_regs` ------
      #########################################################################|
      
      setup_dummy_hot_chg <- function(row_idx, col_idx, old_val, new_val) {
        list(list(
          as.integer(row_idx - 1L),  ## row index (+1 for zero indexing)
          as.integer(col_idx - 1L),  ## col index (+1 for zero indexing)
          old_val,                   ## old value
          new_val                    ## new value
        )) ## always check for old_value == new_value  -->  do nothing if TRUE
      }
      
      #########################################################################|
      ### test adding new Rx regimen entry to existing `rx_table$df_regs` ------
      #########################################################################|
      
      df_add_regs <- empty_rx_table() %>% 
        add_new_rx_regimen(rx_label = "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5") %>% 
        add_new_rx_regimen(rx_label = "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28") %>% 
        add_new_rx_regimen(rx_label = "150 mg over 0.25 in 2") %>% 
        add_new_rx_regimen(rx_label = "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5") %>% 
        add_new_rx_regimen(rx_label = "300 mg in 1 q7 x 10")
      
      ## perform assignment after adding row following rx_submit button click 
      rx_table <- list(
        df_regs = df_add_regs
      )
      
      
      ## setup test dummy data
      test_hot_chg_exclude_REGN4   <- setup_dummy_hot_chg(4, 3, TRUE, FALSE)
      test_hot_chg_include_REGN4   <- setup_dummy_hot_chg(4, 3, FALSE, TRUE)
      test_hot_chg_reglab          <- setup_dummy_hot_chg(1, 2, "REGN1", "TRT-1")
      test_hot_chg_reglab_no_chg   <- setup_dummy_hot_chg(2, 2, "REGN2", "REGN2")
      test_hot_chg_rxlabel         <- setup_dummy_hot_chg(3, 4, "150 mg over 0.25 in 2", "999 mg over 0.25 in 2")
      test_hot_chg_rxlabel_no_chg  <- setup_dummy_hot_chg(5, 4, "300 mg in 1 q7 x 10", "300 mg in 1 q7 x 10")
      test_hot_chg_rxlabel_invalid <- setup_dummy_hot_chg(4, 4, define_default_rx_input(), paste0(define_default_rx_input(), ",,"))
      
      ## parse test dummy data
      parse_hot_chg_exclude_REGN4   <- parse_hot_changes(test_hot_chg_exclude_REGN4, hot_colnames = names(rx_table$df_regs))
      parse_hot_chg_include_REGN4   <- parse_hot_changes(test_hot_chg_include_REGN4, hot_colnames = names(rx_table$df_regs))
      parse_hot_chg_reglab          <- parse_hot_changes(test_hot_chg_reglab, hot_colnames = names(rx_table$df_regs))
      parse_hot_chg_reglab_no_chg   <- parse_hot_changes(test_hot_chg_reglab_no_chg, hot_colnames = names(rx_table$df_regs))
      parse_hot_chg_rxlabel         <- parse_hot_changes(test_hot_chg_rxlabel, hot_colnames = names(rx_table$df_regs))
      parse_hot_chg_rxlabel_no_chg  <- parse_hot_changes(test_hot_chg_rxlabel_no_chg, hot_colnames = names(rx_table$df_regs))
      parse_hot_chg_rxlabel_invalid <- parse_hot_changes(test_hot_chg_rxlabel_invalid, hot_colnames = names(rx_table$df_regs))
      
      ## test parsed dummy data against expected results
      testthat::expect_identical(parse_hot_chg_exclude_REGN4,   list(row_idx = 4L, col_idx = 3L, col_name = "Include", chg_rxlbl = FALSE, old_val = TRUE, new_val = FALSE, val_chg = TRUE))
      testthat::expect_identical(parse_hot_chg_include_REGN4,   list(row_idx = 4L, col_idx = 3L, col_name = "Include", chg_rxlbl = FALSE, old_val = FALSE, new_val = TRUE, val_chg = TRUE))
      testthat::expect_identical(parse_hot_chg_reglab,          list(row_idx = 1L, col_idx = 2L, col_name = "REGLAB", chg_rxlbl = FALSE, old_val = "REGN1", new_val = "TRT-1", val_chg = TRUE))
      testthat::expect_identical(parse_hot_chg_reglab_no_chg,   list(row_idx = 2L, col_idx = 2L, col_name = "REGLAB", chg_rxlbl = FALSE, old_val = "REGN2", new_val = "REGN2", val_chg = FALSE))
      testthat::expect_identical(parse_hot_chg_rxlabel,         list(row_idx = 3L, col_idx = 4L, col_name = "RXLABEL", chg_rxlbl = TRUE, old_val = "150 mg over 0.25 in 2", new_val = "999 mg over 0.25 in 2", val_chg = TRUE))
      testthat::expect_identical(parse_hot_chg_rxlabel_no_chg,  list(row_idx = 5L, col_idx = 4L, col_name = "RXLABEL", chg_rxlbl = TRUE, old_val = "300 mg in 1 q7 x 10", new_val = "300 mg in 1 q7 x 10", val_chg = FALSE))
      testthat::expect_identical(parse_hot_chg_rxlabel_invalid, list(row_idx = 4L, col_idx = 4L, col_name = "RXLABEL", chg_rxlbl = TRUE, 
                                                                     old_val = "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                                                                     new_val = "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5,,", 
                                                                     val_chg = TRUE))
    })
    
    
    ## modify_rx_table_regimen_fn() --------------------------------------------
    testthat::test_that("modify_rx_table_regimen_fn", {
      
      #########################################################################|
      ### test adding new Rx regimen entry to existing `rx_table$df_regs` ------
      #########################################################################|
      
      setup_dummy_hot_chg <- function(row_idx, col_idx, old_val, new_val) {
        list(list(
          as.integer(row_idx - 1L),  ## row index (+1 for zero indexing)
          as.integer(col_idx - 1L),  ## col index (+1 for zero indexing)
          old_val,                   ## old value
          new_val                    ## new value
        )) ## always check for old_value == new_value  -->  do nothing if TRUE
      }
      
      #########################################################################|
      ### test adding new Rx regimen entry to existing `rx_table$df_regs` ------
      #########################################################################|
      
      df_add_regs <- empty_rx_table() %>% 
        add_new_rx_regimen(rx_label = "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5") %>% 
        add_new_rx_regimen(rx_label = "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28") %>% 
        add_new_rx_regimen(rx_label = "150 mg over 0.25 in 2") %>% 
        add_new_rx_regimen(rx_label = "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5") %>% 
        add_new_rx_regimen(rx_label = "300 mg in 1 q7 x 10")
      
      ## perform assignment after adding row following rx_submit button click 
      rx_table <- list(
        df_regs = df_add_regs
      )
      
      
      ## setup test dummy data
      test_hot_chg_exclude_REGN4   <- setup_dummy_hot_chg(4, 3, TRUE, FALSE)
      test_hot_chg_include_REGN4   <- setup_dummy_hot_chg(4, 3, FALSE, TRUE)
      test_hot_chg_reglab          <- setup_dummy_hot_chg(1, 2, "REGN1", "TRT-1")
      test_hot_chg_reglab_no_chg   <- setup_dummy_hot_chg(2, 2, "REGN2", "REGN2")
      test_hot_chg_rxlabel         <- setup_dummy_hot_chg(3, 4, "150 mg over 0.25 in 2", "999 mg over 0.25 in 2")
      test_hot_chg_rxlabel_no_chg  <- setup_dummy_hot_chg(5, 4, "300 mg in 1 q7 x 10", "300 mg in 1 q7 x 10")
      test_hot_chg_rxlabel_invalid <- setup_dummy_hot_chg(4, 4, define_default_rx_input(), paste0(define_default_rx_input(), ",,"))
      
      ## parse test dummy data
      parse_hot_chg_exclude_REGN4   <- parse_hot_changes(test_hot_chg_exclude_REGN4, hot_colnames = names(rx_table$df_regs))
      parse_hot_chg_include_REGN4   <- parse_hot_changes(test_hot_chg_include_REGN4, hot_colnames = names(rx_table$df_regs))
      parse_hot_chg_reglab          <- parse_hot_changes(test_hot_chg_reglab, hot_colnames = names(rx_table$df_regs))
      parse_hot_chg_reglab_no_chg   <- parse_hot_changes(test_hot_chg_reglab_no_chg, hot_colnames = names(rx_table$df_regs))
      parse_hot_chg_rxlabel         <- parse_hot_changes(test_hot_chg_rxlabel, hot_colnames = names(rx_table$df_regs))
      parse_hot_chg_rxlabel_no_chg  <- parse_hot_changes(test_hot_chg_rxlabel_no_chg, hot_colnames = names(rx_table$df_regs))
      parse_hot_chg_rxlabel_invalid <- parse_hot_changes(test_hot_chg_rxlabel_invalid, hot_colnames = names(rx_table$df_regs))
      
      
      
      ## test `rx_table$df_regs` modifications using simulated changes to {rhandsontable}
      testthat::expect_identical(modify_rx_table_regimen_fn(test_hot_chg_exclude_REGN4, rx_table)$df_regs[[parse_hot_chg_exclude_REGN4$col_idx]][[parse_hot_chg_exclude_REGN4$row_idx]], parse_hot_chg_exclude_REGN4$new_val)
      testthat::expect_identical(modify_rx_table_regimen_fn(test_hot_chg_include_REGN4, rx_table)$df_regs[[parse_hot_chg_include_REGN4$col_idx]][[parse_hot_chg_include_REGN4$row_idx]], parse_hot_chg_include_REGN4$new_val)
      testthat::expect_identical(modify_rx_table_regimen_fn(test_hot_chg_reglab, rx_table)$df_regs[[parse_hot_chg_reglab$col_idx]][[parse_hot_chg_reglab$row_idx]], parse_hot_chg_reglab$new_val)
      testthat::expect_identical(modify_rx_table_regimen_fn(test_hot_chg_reglab_no_chg, rx_table)$df_regs[[parse_hot_chg_reglab_no_chg$col_idx]][[parse_hot_chg_reglab_no_chg$row_idx]], parse_hot_chg_reglab_no_chg$new_val)
      testthat::expect_identical(modify_rx_table_regimen_fn(test_hot_chg_rxlabel, rx_table)$df_regs[[parse_hot_chg_rxlabel$col_idx]][[parse_hot_chg_rxlabel$row_idx]], parse_hot_chg_rxlabel$new_val)
      testthat::expect_identical(modify_rx_table_regimen_fn(test_hot_chg_rxlabel, rx_table)$df_regs[["RXDOSE"]][[parse_hot_chg_rxlabel$row_idx]], submit_rx_input_fn(parse_hot_chg_rxlabel$new_val))
      testthat::expect_identical(modify_rx_table_regimen_fn(test_hot_chg_rxlabel_no_chg, rx_table)$df_regs[[parse_hot_chg_rxlabel_no_chg$col_idx]][[parse_hot_chg_rxlabel_no_chg$row_idx]], parse_hot_chg_rxlabel_no_chg$new_val)
      testthat::expect_identical(modify_rx_table_regimen_fn(test_hot_chg_rxlabel_invalid, rx_table)$df_regs[[parse_hot_chg_rxlabel_invalid$col_idx]][[parse_hot_chg_rxlabel_invalid$row_idx]], parse_hot_chg_rxlabel_invalid$new_val)
      testthat::expect_error(submit_rx_input_fn(parse_hot_chg_rxlabel_invalid$new_val))
      testthat::expect_identical(modify_rx_table_regimen_fn(test_hot_chg_rxlabel_invalid, rx_table)$df_regs[["RXDOSE"]][[parse_hot_chg_rxlabel_invalid$row_idx]], submit_rx_input_fn(parse_hot_chg_rxlabel_invalid$old_val))
      
    })
    
    
    
    ## unnest_rx_table_dose_data() ---------------------------------------------
    testthat::test_that("unnest_rx_table_dose_data", {
      
      #########################################################################|
      ### test adding new Rx regimen entry to existing `rx_table$df_regs` ------
      #########################################################################|
      
      df_add_regs <- empty_rx_table() %>% 
        add_new_rx_regimen(rx_label = "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5") %>% 
        add_new_rx_regimen(rx_label = "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28") %>% 
        add_new_rx_regimen(rx_label = "150 mg over 0.25 in 2") %>% 
        add_new_rx_regimen(rx_label = "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5") %>% 
        add_new_rx_regimen(rx_label = "300 mg in 1 q7 x 10")

      ## perform assignment after adding row following rx_submit button click 
      rx_table <- list(
        df_regs = df_add_regs
      )

      
      rx_multi_dose <- unnest_rx_table_dose_data(rx_table$df_regs)
      
      
      testthat::expect_error(unnest_rx_table_dose_data(rx_table$df_regs), NA)
      testthat::expect_type(rx_multi_dose, "list")
      testthat::expect_s3_class(rx_multi_dose, "tbl_df")
      testthat::expect_named(rx_multi_dose, c("REGNUM", "REGLAB", "RXLABEL", names(define_default_ev_data_cols())), 
                             ignore.order = TRUE)
      testthat::expect_gte(nrow(rx_multi_dose), nrow(rx_table$df_regs))
      testthat::expect_identical(rx_multi_dose,
                                 tibble::tibble(
                                   REGNUM = c(1L, 1L, 1L, 2L, 2L, 3L, 4L, 4L, 4L, 5L), 
                                   REGLAB = c("REGN1", "REGN1", "REGN1", "REGN2", "REGN2","REGN3", "REGN4", "REGN4", "REGN4", "REGN5"), 
                                   RXLABEL = c(
                                     "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5",
                                     "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5",
                                     "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5",
                                     "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28",
                                     "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28",
                                     "150 mg over 0.25 in 2", 
                                     "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5",
                                     "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5",
                                     "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5",
                                     "300 mg in 1 q7 x 10"), 
                                   ID = c(1L, 1L, 1L, 2L, 2L, 3L, 4L, 4L, 4L, 5L), 
                                   TIME = c(0, 28, 112.5, 0, 28, 0, 0, 28, 112.5, 0), 
                                   AMT = c(150, 100, 100, 150, 100, 150, 150, 100, 100, 300), 
                                   RATE = c(600, 400,400, 600, 400, 600, 600, 400, 400, 0), 
                                   EVID = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), 
                                   CMT = c(2L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 2L, 1L), 
                                   ADDL = c(0L, 5L, 7L, 0L, 5L, 0L, 0L, 5L, 7L, 9L),
                                   II = c(0, 14, 7, 0, 14, 0, 0, 14, 7, 7), 
                                   SS = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)
                                 ))
      
      rx_multi_dose_exclude_REGN4 <- unnest_rx_table_dose_data(rx_table$df_regs %>% mutate(Include = dplyr::if_else(REGNUM %in% 4, FALSE, Include)))
      testthat::expect_identical(unique(rx_multi_dose_exclude_REGN4$REGNUM), c(1L, 2L, 3L, 5L))
      
      
      rx_multi_dose_exclude_REGN4_REGN5 <- unnest_rx_table_dose_data(rx_table$df_regs %>% mutate(Include = dplyr::if_else(REGNUM %in% c(4, 5), FALSE, Include)))
      testthat::expect_identical(unique(rx_multi_dose_exclude_REGN4_REGN5$REGNUM), c(1L, 2L, 3L))
      testthat::expect_identical(rx_multi_dose_exclude_REGN4_REGN5,
                                 tibble::tibble(
                                   REGNUM = c(1L, 1L, 1L, 2L, 2L, 3L), 
                                   REGLAB = c("REGN1", "REGN1", "REGN1", "REGN2", "REGN2","REGN3"), 
                                   RXLABEL = c(
                                     "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5",
                                     "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5",
                                     "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5",
                                     "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28",
                                     "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28",
                                     "150 mg over 0.25 in 2"), 
                                   ID = c(1L, 1L, 1L, 2L, 2L, 3L), 
                                   TIME = c(0, 28, 112.5, 0, 28, 0), 
                                   AMT = c(150, 100, 100, 150, 100, 150), 
                                   RATE = c(600, 400,400, 600, 400, 600), 
                                   EVID = c(1L, 1L, 1L, 1L, 1L, 1L), 
                                   CMT = c(2L, 1L, 2L, 2L, 1L, 2L), 
                                   ADDL = c(0L, 5L, 7L, 0L, 5L, 0L),
                                   II = c(0, 14, 7, 0, 14, 0), 
                                   SS = c(0L, 0L, 0L, 0L, 0L, 0L)
                                 ))
      
      ## test case where all dosing regimens are excluded
      testthat::expect_error(unnest_rx_table_dose_data(rx_table$df_regs %>% mutate(Include = FALSE)), NA)
      
      rx_multi_dose_exclude_all <- unnest_rx_table_dose_data(rx_table$df_regs %>% mutate(Include = FALSE))
      testthat::expect_type(rx_multi_dose_exclude_all, "list")
      testthat::expect_s3_class(rx_multi_dose_exclude_all, "tbl_df")
      testthat::expect_named(rx_multi_dose_exclude_all, c("REGNUM", "REGLAB", "RXLABEL", names(define_default_ev_data_cols())), 
                             ignore.order = TRUE)
      testthat::expect_identical(nrow(rx_multi_dose_exclude_all), 0L)
      testthat::expect_identical(rx_multi_dose_exclude_all, tibble:::tibble(REGNUM = integer(0), REGLAB = character(0), RXLABEL = character(0), 
                                                                ID = integer(0), TIME = numeric(0), AMT = numeric(0), RATE = numeric(0), 
                                                                EVID = integer(0), CMT = integer(0), ADDL = integer(0), II = numeric(0), 
                                                                SS = integer(0)))
      
    })
    
    
    
    ## add_rx_table_regs_info() ------------------------------------------------
    testthat::test_that("add_rx_table_regs_info", {
      
 
      ## setup test data with all dosing regimens included for `add_rx_table_regs_info()`
      df_regs <- data.frame(REGNUM = 1:4, 
                            REGLAB = c("REGN1", "REGN2", "REGN3", "REGN4"), 
                            Include = c(TRUE, TRUE, TRUE, TRUE), 
                            RXLABEL = c("150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                                        "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28", 
                                        "150 mg over 0.25 in 2", 
                                        "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5"))
      
      
      expc_rx_regs_data <- tibble::tibble(
        REGNUM = c(1L, 1L, 1L, 2L, 2L, 3L, 4L, 4L, 4L), 
        REGLAB = c("REGN1", "REGN1", "REGN1", "REGN2", "REGN2", "REGN3", "REGN4", "REGN4", "REGN4"), 
        ID = c(1L, 1L, 1L, 2L, 2L, 3L,4L, 4L, 4L), 
        TIME = c(0, 28, 112.5, 0, 28, 0, 0, 28, 112.5), 
        AMT = c(150, 100, 100, 150, 100, 150, 150, 100, 100), 
        RATE = c(600, 400, 400, 600, 400, 600, 600, 400, 400), 
        EVID = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), 
        CMT = c(2L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 2L), 
        ADDL = c(0L, 5L, 7L, 0L, 5L, 0L, 0L, 5L, 7L), 
        II = c(0, 14, 7, 0, 14, 0, 0, 14, 7), 
        SS = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L), 
        RXLABEL = c("150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28", 
                    "150 mg over 0.25 in 2", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5"))
      
      test_rx_regs_data <- tibble::tibble(
        ID = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), 
        TIME = c(0, 28, 112.5, 0, 28, 0, 0, 28, 112.5), 
        AMT = c(150, 100, 100, 150, 100, 150, 150, 100, 100), 
        RATE = c(600, 400, 400, 600, 400, 600, 600, 400, 400), 
        EVID = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), 
        CMT = c(2L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 2L), 
        ADDL = c(0L, 5L, 7L, 0L, 5L, 0L, 0L, 5L, 7L), 
        II = c(0, 14, 7, 0, 14, 0, 0, 14, 7), 
        SS = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L), 
        REGNUM = c(1L, 1L, 1L, 2L, 2L, 3L, 4L, 4L, 4L), 
        REGLAB = c("REGN1", "REGN1", "REGN1", "REGN2", "REGN2", "REGN3", "REGN4", "REGN4", "REGN4"), 
        Include = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), 
        RXLABEL = c("150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28", 
                    "150 mg over 0.25 in 2", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5"))
      
      
      ## test `add_rx_table_regs_info()` with all dosing regimens included
      testthat::expect_error(add_rx_table_regs_info(test_rx_regs_data), NA)
      testthat::expect_s3_class(add_rx_table_regs_info(test_rx_regs_data), "data.frame")
      testthat::expect_named(add_rx_table_regs_info(test_rx_regs_data), names(expc_rx_regs_data))
      testthat::expect_identical(nrow(add_rx_table_regs_info(test_rx_regs_data)), nrow(expc_rx_regs_data))
      testthat::expect_identical(sort(unique(add_rx_table_regs_info(test_rx_regs_data)$REGNUM)), sort(unique(df_regs %>% filter(Include) %>% pull(REGNUM))))
      testthat::expect_identical(sort(unique(add_rx_table_regs_info(test_rx_regs_data)$REGLAB)), sort(unique(df_regs %>% filter(Include) %>% pull(REGLAB))))
      testthat::expect_identical(add_rx_table_regs_info(test_rx_regs_data)$REGNUM, expc_rx_regs_data$REGNUM)
      testthat::expect_identical(add_rx_table_regs_info(test_rx_regs_data)$REGLAB, expc_rx_regs_data$REGLAB)
      testthat::expect_identical("Include" %in% names(add_rx_table_regs_info(test_rx_regs_data)), FALSE)
      testthat::expect_identical(add_rx_table_regs_info(test_rx_regs_data)$RXLABEL, expc_rx_regs_data$RXLABEL)
      testthat::expect_identical(add_rx_table_regs_info(test_rx_regs_data), expc_rx_regs_data)
      
      
      
      ## setup test data with at least 1 dosing regimen excluded for `add_rx_table_regs_info()`
      df_regs_exclude_4 <- data.frame(REGNUM = 1:4, 
                                      REGLAB = c("REGN1", "REGN2", "MY-3", "REGN4"), 
                                      Include = c(TRUE, TRUE, TRUE, FALSE), 
                                      RXLABEL = c("150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                                                  "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28", 
                                                  "150 mg over 0.25 in 2", 
                                                  "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5"))
      
      expc_rx_regs_data_exclude_4 <- tibble::tibble(
        REGNUM = c(1L, 1L, 1L, 2L, 2L, 3L), 
        REGLAB = c("REGN1", "REGN1", "REGN1", "REGN2", "REGN2", "MY-3"),
        ID = c(1L, 1L, 1L, 2L, 2L, 3L), 
        TIME = c(0, 28, 112.5, 0, 28, 0), 
        AMT = c(150, 100, 100, 150, 100, 150), 
        RATE = c(600, 400, 400, 600, 400, 600), 
        EVID = c(1L, 1L, 1L, 1L, 1L, 1L), 
        CMT = c(2L, 1L, 2L, 2L, 1L, 2L), 
        ADDL = c(0L, 5L, 7L, 0L, 5L, 0L), 
        II = c(0, 14, 7, 0, 14, 0), 
        SS = c(0L, 0L, 0L, 0L, 0L, 0L), 
        RXLABEL = c("150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28", 
                    "150 mg over 0.25 in 2"))
      
      
      test_rx_regs_data_exclude_4 <- tibble::tibble(
        ID = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), 
        TIME = c(0, 28, 112.5, 0, 28, 0, 0, 28, 112.5), 
        AMT = c(150, 100, 100, 150, 100, 150, 150, 100, 100), 
        RATE = c(600, 400, 400, 600, 400, 600, 600, 400, 400), 
        EVID = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), 
        CMT = c(2L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 2L), 
        ADDL = c(0L, 5L, 7L, 0L, 5L, 0L, 0L, 5L, 7L), 
        II = c(0, 14, 7, 0, 14, 0, 0, 14, 7), 
        SS = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L), 
        REGNUM = c(1L, 1L, 1L, 2L, 2L, 3L, 4L, 4L, 4L), 
        REGLAB = c("REGN1", "REGN1", "REGN1", "REGN2", "REGN2", "MY-3", "REGN4", "REGN4", "REGN4"), 
        Include = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE), 
        RXLABEL = c("150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28", 
                    "150 mg over 0.25 in 2", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5"))
      
      
      ## test `add_rx_table_regs_info()` with at least 1 dosing regimen included
      testthat::expect_error(add_rx_table_regs_info(test_rx_regs_data_exclude_4), NA)
      testthat::expect_s3_class(add_rx_table_regs_info(test_rx_regs_data_exclude_4), "data.frame")
      testthat::expect_named(add_rx_table_regs_info(test_rx_regs_data_exclude_4), names(expc_rx_regs_data_exclude_4))
      testthat::expect_identical(nrow(add_rx_table_regs_info(test_rx_regs_data_exclude_4)), nrow(expc_rx_regs_data_exclude_4))
      testthat::expect_identical(sort(unique(add_rx_table_regs_info(test_rx_regs_data_exclude_4)$REGNUM)), sort(unique(df_regs_exclude_4 %>% filter(Include) %>% pull(REGNUM))))
      testthat::expect_identical(sort(unique(add_rx_table_regs_info(test_rx_regs_data_exclude_4)$REGLAB)), sort(unique(df_regs_exclude_4 %>% filter(Include) %>% pull(REGLAB))))
      testthat::expect_identical(add_rx_table_regs_info(test_rx_regs_data_exclude_4)$REGNUM, expc_rx_regs_data_exclude_4$REGNUM)
      testthat::expect_identical(add_rx_table_regs_info(test_rx_regs_data_exclude_4)$REGLAB, expc_rx_regs_data_exclude_4$REGLAB)
      testthat::expect_identical("Include" %in% names(add_rx_table_regs_info(test_rx_regs_data_exclude_4)), FALSE)
      testthat::expect_identical(add_rx_table_regs_info(test_rx_regs_data_exclude_4)$RXLABEL, expc_rx_regs_data_exclude_4$RXLABEL)
      testthat::expect_identical(add_rx_table_regs_info(test_rx_regs_data_exclude_4), expc_rx_regs_data_exclude_4)
      
    })
    
    
    
    ## rx_table_to_multi_reg_data() --------------------------------------------
    testthat::test_that("rx_table_to_multi_reg_data", {
      
      df_regs_empty <- data.frame(
        REGNUM = integer(0),
        REGLAB = character(0),
        Include = logical(0),
        RXLABEL = character(0)
      )
      
      df_regs <- data.frame(REGNUM = 1:4, 
                            REGLAB = c("REGN1", "REGN2", "REGN3", "REGN4"), 
                            Include = c(TRUE, TRUE, TRUE, TRUE), 
                            RXLABEL = c("150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                                        "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28", 
                                        "150 mg over 0.25 in 2", 
                                        "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5"))
      
      df_regs_data_expc <- tibble::tibble(
        REGNUM = c(1L, 1L, 1L, 2L, 2L, 3L, 4L, 4L, 4L), 
        REGLAB = c("REGN1", "REGN1", "REGN1", "REGN2", "REGN2", "REGN3", "REGN4", "REGN4", "REGN4"), 
        ID = c(1L, 1L, 1L, 2L, 2L, 3L, 4L, 4L, 4L), 
        TIME = c(0, 28, 112.5, 0, 28, 0, 0, 28, 112.5), 
        AMT = c(150, 100, 100, 150, 100, 150, 150, 100, 100), 
        RATE = c(600, 400, 400, 600, 400, 600, 600, 400, 400), 
        EVID = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), 
        CMT = c(2L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 2L), 
        ADDL = c(0L, 5L, 7L, 0L, 5L, 0L, 0L, 5L, 7L), 
        II = c(0, 14, 7, 0, 14, 0, 0, 14, 7), 
        SS = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L), 
        RXLABEL = c("150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28", 
                    "150 mg over 0.25 in 2", "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5"
        ))
      
      subset_rx_regs <- function(df_regs, index = 1) {
        df_regs %>% filter(REGNUM %in% index)
      }
      
      df_regs_data_test <- rx_table_to_multi_reg_data(df_regs)
      
      reg_data_to_rx_table <- function(reg_data) {
        reg_data %>% 
          select(REGNUM, REGLAB, RXLABEL) %>% 
          mutate(Include = TRUE, .before = RXLABEL) %>% 
          distinct() %>% 
          as.data.frame()
      }
      
      
      ## test individual entries from `rx_table$df_regs` reactiveValues data structure
      rx_table_reg_01 <- subset_rx_regs(df_regs, index = 1)
      rx_table_reg_02 <- subset_rx_regs(df_regs, index = 2)
      rx_table_reg_03 <- subset_rx_regs(df_regs, index = 3)
      rx_table_reg_04 <- subset_rx_regs(df_regs, index = 4)
      rx_regs_data_01 <- rx_table_to_multi_reg_data(rx_table_reg_01)
      rx_regs_data_02 <- rx_table_to_multi_reg_data(rx_table_reg_02)
      rx_regs_data_03 <- rx_table_to_multi_reg_data(rx_table_reg_03)
      rx_regs_data_04 <- rx_table_to_multi_reg_data(rx_table_reg_04)
      testthat::expect_error(rx_table_to_multi_reg_data(rx_table_reg_01), NA)
      testthat::expect_error(rx_table_to_multi_reg_data(rx_table_reg_02), NA)
      testthat::expect_error(rx_table_to_multi_reg_data(rx_table_reg_03), NA)
      testthat::expect_error(rx_table_to_multi_reg_data(rx_table_reg_04), NA)
      testthat::expect_identical(reg_data_to_rx_table(reg_data = rx_regs_data_01), rx_table_reg_01)
      testthat::expect_identical(reg_data_to_rx_table(reg_data = rx_regs_data_02), rx_table_reg_02)
      testthat::expect_identical(reg_data_to_rx_table(reg_data = rx_regs_data_03), rx_table_reg_03)
      testthat::expect_identical(reg_data_to_rx_table(reg_data = rx_regs_data_04), rx_table_reg_04)
      rx_table_reg_01_exclude <- rx_table_reg_01 %>% mutate(Include = FALSE)
      rx_regs_data_01_exclude <- rx_table_to_multi_reg_data(rx_table_reg_01_exclude)
      testthat::expect_error(rx_regs_data_01_exclude, NA)
      testthat::expect_s3_class(rx_regs_data_01_exclude, "tbl_df")
      testthat::expect_named(rx_regs_data_01_exclude, c("REGNUM", "REGLAB", "ID", "TIME", "AMT", "RATE", "EVID", "CMT", "ADDL", "II", "SS", "RXLABEL"), ignore.order = TRUE)
      testthat::expect_identical(nrow(rx_regs_data_01_exclude), 0L)
      
      
      ## test empty `rx_table$df_regs` reactiveValues object/data structure (no entries)
      rx_table_reg_00 <- subset_rx_regs(df_regs, index = 0)
      testthat::expect_identical(rx_table_reg_00, df_regs_empty)
      testthat::expect_error(rx_table_to_multi_reg_data(rx_table_reg_00), NA)
      testthat::expect_identical(rx_table_to_multi_reg_data(rx_table_reg_00), df_regs_empty)
      
      
      ## test `rx_table$df_regs` reactiveValues object with multiple dosing regimen entries
      df_regs_dose_period_counts <- sum(stringr::str_count(df_regs$RXLABEL, pattern = ",|then") + 1L)
      testthat::expect_s3_class(df_regs_data_test, "tbl_df")
      testthat::expect_named(df_regs_data_test, c("REGNUM", "REGLAB", "ID", "TIME", "AMT", "RATE", "EVID", "CMT", "ADDL", "II", "SS", "RXLABEL"), ignore.order = TRUE)
      testthat::expect_identical(nrow(df_regs_data_test), df_regs_dose_period_counts)
      testthat::expect_identical(df_regs_data_test, df_regs_data_expc)
      
      # test `rx_table$df_regs` reactiveValues object with multiple regimens & one excluded regimen
      df_regs_exclude_4 <- df_regs %>% mutate(Include = dplyr::if_else(REGNUM %in% 4, FALSE, Include))
      df_regs_exclude_4_data <- rx_table_to_multi_reg_data(df_regs_exclude_4)
      df_regs_exclude_4_dose_period_counts <- sum(stringr::str_count(df_regs_exclude_4 %>% filter(Include %in% TRUE) %>% pull(RXLABEL), pattern = ",|then") + 1L)
      testthat::expect_s3_class(df_regs_exclude_4_data, "tbl_df")
      testthat::expect_named(df_regs_exclude_4_data, c("REGNUM", "REGLAB", "ID", "TIME", "AMT", "RATE", "EVID", "CMT", "ADDL", "II", "SS", "RXLABEL"), ignore.order = TRUE)
      testthat::expect_identical(nrow(df_regs_exclude_4_data), df_regs_exclude_4_dose_period_counts)
      testthat::expect_identical(unique(df_regs_exclude_4_data$REGNUM), c(1L, 2L, 3L))
      testthat::expect_identical(df_regs_exclude_4_data, df_regs_data_expc %>% filter(!REGNUM %in% 4))
      
    })
    
    
    
    ## assign_distinct_subject_ids() -------------------------------------------
    testthat::test_that("assign_distinct_subject_ids", {
      
      df_regs_empty <- data.frame(
        REGNUM = integer(0),
        REGLAB = character(0),
        Include = logical(0),
        RXLABEL = character(0)
      )
      
      df_regs <- data.frame(REGNUM = 1:4, 
                            REGLAB = c("REGN1", "REGN2", "REGN3", "REGN4"), 
                            Include = c(TRUE, TRUE, TRUE, TRUE), 
                            RXLABEL = c("150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                                        "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28", 
                                        "150 mg over 0.25 in 2", 
                                        "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5"))
      
      
      df_regs_data_expc <- tibble::tibble(
        REGNUM = c(1L, 1L, 1L, 2L, 2L, 3L, 4L, 4L, 4L), 
        REGLAB = c("REGN1", "REGN1", "REGN1", "REGN2", "REGN2", "REGN3", "REGN4", "REGN4", "REGN4"), 
        ID = c(1L, 1L, 1L, 2L, 2L, 3L, 4L, 4L, 4L), 
        TIME = c(0, 28, 112.5, 0, 28, 0, 0, 28, 112.5), 
        AMT = c(150, 100, 100, 150, 100, 150, 150, 100, 100), 
        RATE = c(600, 400, 400, 600, 400, 600, 600, 400, 400), 
        EVID = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), 
        CMT = c(2L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 2L), 
        ADDL = c(0L, 5L, 7L, 0L, 5L, 0L, 0L, 5L, 7L), 
        II = c(0, 14, 7, 0, 14, 0, 0, 14, 7), 
        SS = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L), 
        RXLABEL = c("150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28", 
                    "150 mg over 0.25 in 2", "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5", 
                    "150 mg over 0.25 in 2, 100 mg in 1 over 0.25 q14 x 6 after 28, 100 mg over 0.25 in 2 q7 x 8 after 0.5"
        ))
      
      subset_rx_regs <- function(df_regs, index = 1) {
        df_regs %>% filter(REGNUM %in% index)
      }
      
      ## assign unique subject IDs for multiple dosing regimens in a single simulation
      sim_settings <- fetch_default_sim_settings()
      rx_reg_count <- length(unique(df_regs$REGNUM))
      rx_reg_multi <- rx_table_to_multi_reg_data(df_regs)
      expected_ids <- as.integer(1:(rx_reg_count * sim_settings$num_subj))
      expected_obs <- as.integer(nrow(rx_reg_multi) * sim_settings$num_subj)
      sim_dose_data <- assign_distinct_subject_ids(rx_reg_multi, num_subj = sim_settings$num_subj)
      testthat::expect_type(sim_dose_data, "list")
      testthat::expect_s3_class(sim_dose_data, "data.frame")
      testthat::expect_s3_class(sim_dose_data, "tbl_df")
      testthat::expect_named(sim_dose_data, c("ID", "TIME", "AMT", "RATE", "EVID", "CMT", "ADDL", "II", "SS", 
                                              "REGNUM", "REGLAB", "RXLABEL"))
      testthat::expect_identical(unique(sim_dose_data$ID), expected_ids)
      testthat::expect_identical(nrow(sim_dose_data), expected_obs)
      testthat::expect_identical(unique(sim_dose_data$REGNUM), unique(rx_reg_multi$REGNUM))
      testthat::expect_identical(unique(sim_dose_data$REGLAB), unique(rx_reg_multi$REGLAB))
      testthat::expect_identical(unique(sim_dose_data$RXLABEL), unique(rx_reg_multi$RXLABEL))
      
      
      ## verify same combination of dosing regimens after assignment of USUBJID
      rx_reg_unique   <- rx_reg_multi  %>% select(REGNUM, REGLAB, RXLABEL) %>% distinct()
      sim_dose_unique <- sim_dose_data %>% select(REGNUM, REGLAB, RXLABEL) %>% distinct()
      testthat::expect_identical(sim_dose_unique, rx_reg_unique)
      
      
      ## test invalid inputs:  `num_subj` < 1  -->  defaults to `num_subj = 1`
      df_regs_data_test <- rx_table_to_multi_reg_data(df_regs)
      df_regs_1sub <- assign_distinct_subject_ids(df_regs_data_test, num_subj = 1)
      testthat::expect_identical(assign_distinct_subject_ids(df_regs_data_test, num_subj = 0), df_regs_1sub)
      testthat::expect_identical(assign_distinct_subject_ids(df_regs_data_test, num_subj = -1), df_regs_1sub)
      testthat::expect_identical(assign_distinct_subject_ids(df_regs_data_test, num_subj = -1000), df_regs_1sub)
      
      
    })
    
    ## expand_sim_dataset_fn() -------------------------------------------------
    testthat::test_that("expand_sim_dataset_fn", {
      
      ## unit tests for:  `expand_sim_dataset_fn(mod, dose_data, sim_settings, tg_input, end_time_scale_factor = define_tgrid_end_scaling_factor())`
      def_mod <- app_libmod_objs[[fetch_default_libmod_name()]]
      dose_data <- fill_default_ev_data_cols(mrgsolve::ev_rx(define_default_rx_input()))
      tg0 <- mrgsolve::tgrid()
      sim_settings <- fetch_default_sim_settings()
      scl <- define_tgrid_end_scaling_factor()
      sim_expand <- expand_sim_dataset_fn(def_mod, dose_data, sim_settings, tg0, scl)
      
      ## test expected properties of output dataset via standard unit tests
      testthat::expect_type(sim_expand, "list")
      testthat::expect_named(sim_expand, c("ID", "TIME", "AMT", "RATE", "EVID", "CMT", "ADDL", "II", "SS", "REGNUM", "REGLAB", "DOSE", "WT", "WTREF", "DV", "IPRED"))
      testthat::expect_length(unique(sim_expand$ID), sim_settings$num_subj)
      testthat::expect_gt(length(unique(sim_expand$ID)), length(unique(dose_data$ID)))
      testthat::expect_gt(nrow(sim_expand), nrow(dose_data))
      testthat::expect_gt(ncol(sim_expand), ncol(dose_data))
      
      
      ## test NULL model input 
      testthat::expect_identical(expand_sim_dataset_fn(mod = NULL, dose_data = NULL, sim_settings = NULL, tg_input = NULL), NULL)
      
      
      ## test results against output from `expand_sim_input_data()` <backend> function
      dose_ds <- replicate_dose_data_ids(dose_data, num_subj = sim_settings$num_subj)
      tg_adj <- adjust_time_grid_dosing_fn(dose_data, tg0, scl)
      testthat::expect_identical(
        expand_sim_dataset_fn(def_mod, dose_data, sim_settings, tg_adj, scl),
        expand_sim_input_data(def_mod, dose_ds, tg_adj, sim_settings$sim_seed))
      
      
      ## use another dosing dataset to test against `expand_sim_input_data()` <backend> function
      dose_1prd <- fill_default_ev_data_cols(mrgsolve::ev_rx("1000 mg in 1 over 0.1 q2 x 20 after 1"))
      tg_adj_1prd <- adjust_time_grid_dosing_fn(dose_1prd, tg0, scl)
      ds_1prd <- replicate_dose_data_ids(dose_1prd, num_subj = sim_settings$num_subj)
      testthat::expect_identical(
        expand_sim_dataset_fn(def_mod, dose_1prd, sim_settings, tg_adj_1prd, scl),
        expand_sim_input_data(def_mod, ds_1prd, tg_adj_1prd, sim_settings$sim_seed))
      
      
      ## use another library model to test against `expand_sim_input_data()` <backend> function
      tmdd_mod <- app_libmod_objs$tmdd_full_2cmt_1abs
      testthat::expect_identical(
        expand_sim_dataset_fn(tmdd_mod, dose_data, sim_settings, tg_adj, scl),
        expand_sim_input_data(tmdd_mod, dose_ds, tg_adj, sim_settings$sim_seed))
      
      testthat::expect_named(
        expand_sim_dataset_fn(tmdd_mod, dose_data, sim_settings, tg0, scl), 
        c("ID", "TIME", "AMT", "RATE", "EVID", "CMT", "ADDL", "II", "SS", "REGNUM", "REGLAB", "CP", "TOTAL", 
          "DOSE", "WT", "WTREF", "DV", "IPRED"))
    })
    
    ## replicate_dose_data_ids() -----------------------------------------------
    testthat::test_that("replicate_dose_data_ids", {
      def_mod <- app_libmod_objs[[fetch_default_libmod_name()]]
      dose_data <- fill_default_ev_data_cols(mrgsolve::ev_rx(define_default_rx_input())) %>% 
        add_dose_regimen_label(.)
      dose_data_nid <- length(unique(dose_data$ID))
      dose_data_nrow <- nrow(dose_data)
      nrow_per_id <- as.integer(dose_data_nrow/dose_data_nid)
      
      
      ## test for `num_subj` = 1
      testthat::expect_type(replicate_dose_data_ids(dose_data, num_subj = 1), "list")
      testthat::expect_length(replicate_dose_data_ids(dose_data, num_subj = 1), ncol(dose_data))
      testthat::expect_named(replicate_dose_data_ids(dose_data, num_subj = 1), names(dose_data))
      testthat::expect_identical(nrow(replicate_dose_data_ids(dose_data, num_subj = 1)), nrow_per_id)
      testthat::expect_length(unique(replicate_dose_data_ids(dose_data, num_subj = 1)$ID), dose_data_nid)
      testthat::expect_identical(replicate_dose_data_ids(dose_data, num_subj = 1), dose_data)
      testthat::expect_identical(as.data.frame(replicate_dose_data_ids(dose_data, num_subj = 1)), dose_data %>% mrgsolve::ev_rep(., ID = 1L))
      
      
      ## test for `num_subj` = 3
      testthat::expect_type(replicate_dose_data_ids(dose_data, num_subj = 3), "list")
      testthat::expect_length(replicate_dose_data_ids(dose_data, num_subj = 3), ncol(dose_data))
      testthat::expect_named(replicate_dose_data_ids(dose_data, num_subj = 3), names(dose_data))
      testthat::expect_identical(nrow(replicate_dose_data_ids(dose_data, num_subj = 3)), nrow_per_id*3L)
      testthat::expect_length(unique(replicate_dose_data_ids(dose_data, num_subj = 3)$ID), 3L)
      testthat::expect_identical(replicate_dose_data_ids(dose_data, num_subj = 3), purrr::map_dfr(1L:3L, ~ dose_data %>% mutate(ID = .x)))
      testthat::expect_identical(as.data.frame(replicate_dose_data_ids(dose_data, num_subj = 3)), dose_data %>% mrgsolve::ev_rep(., ID = 1L:3L))
      
      
      ## test for `num_subj` = 10 (default)
      testthat::expect_type(replicate_dose_data_ids(dose_data, num_subj = 10), "list")
      testthat::expect_length(replicate_dose_data_ids(dose_data, num_subj = 10), ncol(dose_data))
      testthat::expect_named(replicate_dose_data_ids(dose_data, num_subj = 10), names(dose_data))
      testthat::expect_identical(nrow(replicate_dose_data_ids(dose_data, num_subj = 10)), nrow_per_id*10L)
      testthat::expect_length(unique(replicate_dose_data_ids(dose_data, num_subj = 10)$ID), 10L)
      testthat::expect_identical(replicate_dose_data_ids(dose_data, num_subj = 10), purrr::map_dfr(1L:10L, ~ dose_data %>% mutate(ID = .x)))
      testthat::expect_identical(as.data.frame(replicate_dose_data_ids(dose_data, num_subj = 10)), dose_data %>% mrgsolve::ev_rep(., ID = 1L:10L))
    })
    
    
    
    ## expand_sim_input_data() ---------------------------------------------------------------
    testthat::test_that("expand_sim_input_data", {
      
      ## test:  `expand_sim_input_data(mod, dataset, tg, sim_seed)`
      def_mod <- app_libmod_objs[[fetch_default_libmod_name()]]
      dose_data <- fill_default_ev_data_cols(mrgsolve::ev_rx(define_default_rx_input()))
      tg0 <- mrgsolve::tgrid()
      sim_settings <- fetch_default_sim_settings()
      sim_seed <- sim_settings$sim_seed
      num_subj <- sim_settings$num_subj
      ds <- replicate_dose_data_ids(dose_data, num_subj = num_subj)
      
      ## test NULL model input 
      testthat::expect_identical(expand_sim_input_data(mod = NULL, dataset = NULL, tg = NULL, sim_seed = NULL), NULL)
      
      ## test default inputs
      sim_input_test <- expand_sim_input_data(mod = def_mod, dataset = ds, tg = tg0, sim_seed = sim_seed)
      testthat::expect_type(expand_sim_input_data(mod = def_mod, dataset = ds, tg = tg0, sim_seed = sim_seed), "list")
      testthat::expect_named(expand_sim_input_data(mod = def_mod, dataset = ds, tg = tg0, sim_seed = sim_seed), c("ID", "TIME", "AMT", "RATE", "EVID", "CMT", "ADDL", "II", "SS", "REGNUM", "REGLAB", "DOSE", "WT", "WTREF", "DV", "IPRED"))
      testthat::expect_length(unique(sim_input_test$ID), num_subj)
      testthat::expect_length(unique(expand_sim_input_data(mod = def_mod, dataset = ds, tg = tg0, sim_seed = sim_seed)$ID), num_subj)
      testthat::expect_length(unique(expand_sim_input_data(mod = def_mod, dataset = replicate_dose_data_ids(dose_data, num_subj = num_subj), tg = tg0, sim_seed = sim_seed)$ID), num_subj)
      
      ## vary the number of subjects and confirm the correct number of UIDs in result
      testthat::expect_length(unique(expand_sim_input_data(mod = def_mod, dataset = replicate_dose_data_ids(dose_data, num_subj = 5), tg = tg0, sim_seed = sim_seed)$ID), 5)
      testthat::expect_length(unique(expand_sim_input_data(mod = def_mod, dataset = replicate_dose_data_ids(dose_data, num_subj = 1), tg = tg0, sim_seed = sim_seed)$ID), 1)
      testthat::expect_length(unique(expand_sim_input_data(mod = def_mod, dataset = replicate_dose_data_ids(dose_data, num_subj = 20), tg = tg0, sim_seed = sim_seed)$ID), 20)
      testthat::expect_false(identical(expand_sim_input_data(mod = def_mod, dataset = ds, tg = tg0, sim_seed = sim_seed), ds))
      testthat::expect_false(identical(nrow(expand_sim_input_data(mod = def_mod, dataset = ds, tg = tg0, sim_seed = sim_seed)), nrow(ds)))
      testthat::expect_false(identical(ncol(expand_sim_input_data(mod = def_mod, dataset = ds, tg = tg0, sim_seed = sim_seed)), ncol(ds)))
      
      ## test results against output from `expand_sim_dataset_fn()` <backend> function
      scl <- define_tgrid_end_scaling_factor()
      tg_adj <- adjust_time_grid_dosing_fn(dose_data, tg0, scl)
      testthat::expect_identical(
        expand_sim_input_data(def_mod, ds, tg_adj, sim_seed), 
        expand_sim_dataset_fn(def_mod, dose_data, sim_settings, tg_adj, scl))
      
      ## use another dosing dataset to test against `expand_sim_dataset_fn()` <backend> function
      dose_1prd <- fill_default_ev_data_cols(mrgsolve::ev_rx("1000 mg in 1 over 0.1 q2 x 20 after 1"))
      tg_adj_1prd <- adjust_time_grid_dosing_fn(dose_1prd, tg0, scl)
      ds_1prd <- replicate_dose_data_ids(dose_1prd, num_subj = num_subj)
      testthat::expect_identical(
        expand_sim_input_data(def_mod, ds_1prd, tg_adj_1prd, sim_seed), 
        expand_sim_dataset_fn(def_mod, dose_1prd, sim_settings, tg_adj_1prd, scl))
    })
    
    
    
    ## run_sim_series_from_data_template() ---------------------------------------------------
    testthat::test_that("run_sim_series_from_data_template", {
      
      ## define helper function for testing results
      run_sim_repeats <- function(mod, sim_template, sim_settings) {
        set.seed(sim_settings$sim_seed)
        sim_out <- parallel::mclapply(1:sim_settings$num_sims, function(i) {
          mod %>% mrgsim_d(sim_template, carry_out = names(define_default_ev_data_cols())) %>%
            mutate(REPI = i, .before = ID)
        }) %>% bind_rows(.) %>% select(REPI, tidyselect::any_of(names(sim_template)), everything()) %>% 
          format_sim_output_data(., drop_cols = extract_model_cmt_names(mod))
        sim_out
      }
      
      ## unit tests for:  `run_sim_series_from_data_template(mod, sim_template, sim_settings)`
      def_mod <- app_libmod_objs[[fetch_default_libmod_name()]]
      dose_data <- fill_default_ev_data_cols(mrgsolve::ev_rx(define_default_rx_input()))
      tg0 <- mrgsolve::tgrid()
      sim_settings <- update_sim_settings_fn(num_subj = 5, num_sims = 3)
      scl <- define_tgrid_end_scaling_factor()
      sim_template <- expand_sim_dataset_fn(def_mod, dose_data, sim_settings, tg0, scl)
      sim_out_cols <- c("REPI", names(sim_template))
      sim_out <- run_sim_series_from_data_template(def_mod, sim_template, sim_settings)
      sim_res <- run_sim_repeats(def_mod, sim_template, sim_settings) %>% 
        mutate(across(any_of(c(define_integer_ev_data_cols(), "REGNUM")), as.integer)) %>%
        relocate(REGNUM, .after = SS) %>% 
        relocate(REGLAB, .after = REGNUM)
      
      ## test expected properties of simulation output dataset via standard unit tests
      testthat::expect_type(sim_out, "list")
      testthat::expect_named(sim_out, sim_out_cols)
      testthat::expect_length(unique(sim_out$ID), sim_settings$num_subj)
      testthat::expect_length(unique(sim_out$REPI), sim_settings$num_sims)
      testthat::expect_identical(length(unique(sim_out$ID)), length(unique(sim_template$ID)))
      testthat::expect_gt(nrow(sim_out), nrow(sim_template))
      testthat::expect_gt(ncol(sim_out), ncol(sim_template))
      testthat::expect_identical(sim_out, sim_res)
      
      
      ## test NULL model input 
      testthat::expect_identical(run_sim_series_from_data_template(mod = NULL, sim_template, sim_settings), NULL)
      
      
      ## use another library model to test against `expand_sim_input_data()` <backend> function
      tmdd_mod <- app_libmod_objs$tmdd_full_2cmt_1abs
      tmdd_sim_template <- expand_sim_dataset_fn(tmdd_mod, dose_data, sim_settings, tg0, scl)
      sim_opts <- list(num_subj = 8, num_sims = 4, sim_seed = fetch_default_sim_settings()$sim_seed)
      tmdd_sim_out <- run_sim_series_from_data_template(tmdd_mod, tmdd_sim_template, sim_opts)
      tmdd_sim_res <- run_sim_repeats(tmdd_mod, tmdd_sim_template, sim_opts) %>% 
        mutate(across(any_of(c(define_integer_ev_data_cols(), "REGNUM")), as.integer)) %>%
        relocate(REGNUM, .after = SS) %>% 
        relocate(REGLAB, .after = REGNUM)
      testthat::expect_identical(tmdd_sim_out, tmdd_sim_res)
      testthat::expect_named(tmdd_sim_out, c("REPI", "ID", "TIME", "AMT", "RATE", "EVID", 
                                             "CMT", "ADDL", "II", "SS", "REGNUM", "REGLAB", 
                                             "CP", "TOTAL", "DOSE", "WT", "WTREF", "DV", "IPRED"))
    })
})

