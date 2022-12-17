#########################################################################################|
## {testthat} setup file:  https://testthat.r-lib.org/articles/test-fixtures.html#package
#########################################################################################|

## print log debugging statements
print("setup.R")
print(getwd())


## load all packages list at top of "ui.R" script
here::i_am("tests/testthat/setup.R")
ui_code <- readLines(here::here("ui.R"))
src_line <- grep("list.files", ui_code, fixed = TRUE)
load_libs <- ui_code[1:(src_line - 1)]
eval(parse(text = load_libs))


## load `testthat` package
library(testthat)
library(stringr)
library(purrr)


## locally use the 3rd edition of {testthat} to allow snapshot expectations
testthat::local_edition(3)  


#########################################################################################|
## Load all custom library models --------------------------------------------------------
#########################################################################################|

## load all custom models distributed with shiny app
app_libmod_names <- withr::with_dir(here::here(), modify_modlist(modlist = fetch_custom_modlist(names_only = TRUE), drop = "invalid_MM") %>% 
                                      purrr::set_names(.))
app_libmod_objs  <- withr::with_dir(here::here(), purrr::map(app_libmod_names, ~ mrgsolve::mread_cache(model = .x, project = "model_library", soloc = ".")))

## load all `mrgsolve` models distributed with `mrgsolve` package 
all_mrgmod_paths <- withr::with_dir(here::here(), fetch_mrgsolve_modlist())
all_mrgmod_objs  <- withr::with_dir(here::here(), purrr::map(names(all_mrgmod_paths) %>% purrr::set_names(.), ~ mrgsolve::mread_cache(model = .x, project = dirname(unname(all_mrgmod_paths[.x])), soloc = ".")))



