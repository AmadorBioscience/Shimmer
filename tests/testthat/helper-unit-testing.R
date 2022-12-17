#########################################################################################|
## {testthat} setup file:  https://testthat.r-lib.org/articles/test-fixtures.html#package
#########################################################################################|

## print log debugging statements
print("helper-unit-testing.R")
print(getwd())


## load all function R scripts
here::i_am("tests/testthat/helper-unit-testing.R")
purrr::walk(list.files(path = here::here("R"), pattern = "(?i)\\.R$", full.names = TRUE), source)


#########################################################################################|
## define testing helper function --------------------------------------------------------
#########################################################################################|
ls_bfore_unit_test_helpers <- lsf.str()

fetch_default_libmod_name <- function() {
  "popPK_2cmt_linear-mm_iv-sc"
}

stop_watch <- function() {
  ## source: https://csgillespie.github.io/efficientR/programming.html#caching-variables
  start_time <- stop_time <- NULL
  start <- function() {
    assign("start_time", Sys.time(), envir = parent.env(environment()))
  }
  stop <- function() {
    assign("stop_time", Sys.time(), envir = parent.env(environment()))
    difftime(stop_time, start_time)
  }
  list(start = start, stop = stop)
}

example_fn_stop_watch <- function(sleep_sec = 0.1) {
  ## example use case:
  watch <- stop_watch()

  ## wait for time (in seconds) specified by input `sleep_sec`
  watch$start()
  Sys.sleep(sleep_sec)
  watch$stop() %>% print(.)

  ## repeat with 2x `sleep_sec`
  watch$start()
  Sys.sleep(2 * sleep_sec)
  watch$stop() %>% print(.)
  invisible(NULL)
}


#########################################################################################|
### Define metacharacter text analysis helper functions ----------------------------------
#########################################################################################|

get_R_metacharacters <- function() {
  metachars <- c("$", "*", "+", ".", "?", "[", "]", "^", "{", "}", "|", "(", ")", "\\")
  return(metachars)
}

metachar_search_string <- function(metachars=get_R_metacharacters()) {
  metachar_search_patterns <- paste0("\\", metachars)
  return(metachar_search_patterns)
}

metachar_replace_string <- function(metachars=get_R_metacharacters()) {
  metachar_search_patterns <- paste0("\\\\", metachars)
  return(metachar_search_patterns)
}

escape_string_metachars <- function(string_with_metachars) {
  R_metacharacters <- get_R_metacharacters()
  n_metachars <- length(R_metacharacters)
  n_strings <- length(string_with_metachars)

  string_escaped_metachars <- string_with_metachars
  for (sidx in 1:n_strings) {
    curr_string <- string_with_metachars[sidx]
    for (midx in 1:n_metachars) {
      curr_metachar <- R_metacharacters[midx]
      curr_string <- gsub(pattern = metachar_search_string(curr_metachar), replacement = metachar_replace_string(curr_metachar), curr_string, perl=TRUE)
    }
    string_escaped_metachars[sidx] <- curr_string
  }
  return(string_escaped_metachars)
}


create_fn_call <- function(args, body, env = parent.frame()) {
  ## https://stackoverflow.com/questions/12982528/how-to-create-an-r-function-programmatically
  as.function(c(args, body), env)
}

force_console_msgs <- function() {
  TRUE
}


#########################################################################################|
## mock `print_function_calls()` "R/helpers.R" to suppress messages during unit tests ----
#########################################################################################|

## finish modifying the input arguments for `omit_function_calls`
omit_function_calls <- print_function_calls
fns_input_args <- formals(print_function_calls)
fns_input_args$print_call <- FALSE
fn_env <- environment(print_function_calls)
formals(omit_function_calls) <- fns_input_args
fn_env$omit_function_calls <- omit_function_calls


#########################################################################################|
## Define all helper functions into the global environment -------------------------------
#########################################################################################|
ls_diff_unit_test_helpers <- setdiff(lsf.str(), ls_bfore_unit_test_helpers)



