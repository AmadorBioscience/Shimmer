###############################################################################|
## set working directory -------------------------------------------------------
###############################################################################|
setwd(dirname(this.path::this.path()))
here::i_am("testscripts/test_data-io_simulation_output_functions.R")
setwd(here::here())

###############################################################################|
## setup environment -----------------------------------------------------------
###############################################################################|
library(dplyr)
library(purrr)
library(ggplot2)
library(cowplot)
library(flux)
library(mrgsolve)
library(tidyr)


## build paths to shiny app project directory 
proj_root_dir <- basename(here::here())
target_path <- here::here()
ui_path <- list.files(target_path, pattern = "(?i)^ui.R$", full.names = TRUE)
server_path <- list.files(target_path, pattern = "(?i)^server.R$", full.names = TRUE)
all_fn_paths <- list.files(file.path(target_path, "R"), pattern = "(?i)\\.R$", full.names = TRUE)
all_fn_files <- basename(all_fn_paths)
nmd_fn_paths <- all_fn_paths %>% set_names(., basename)
purrr::walk(nmd_fn_paths, source)
source(server_path)


###############################################################################|
## define helper functions for this script -------------------------------------
###############################################################################|

read_all_custom_library_models <- function(shiny_path) {
  withr::local_dir(shiny_path)
  
  exclude_models <- c("invalid_MM.cpp")
  library_model_paths <- list.files(path = "model_library", full.names = TRUE)
  library_model_files <- library_model_paths[!basename(library_model_paths) %in% exclude_models]
  library_model_names <- tools::file_path_sans_ext(basename(library_model_files))
  
  ## read, compile, & cache all library models
  model_library_list <- set_names(map(library_model_names, ~ mread_cache(model = ., project = "model_library", soloc = ".")), library_model_names)
  model_library_list
}

read_all_mrgsolve_modlib_models <- function(exclude_models = NULL) {
  library_model_paths <- list.files(path = mrgsolve::modlib(), pattern = "(?i)\\.cpp$", full.names = TRUE)
  library_model_files <- library_model_paths[!basename(library_model_paths) %in% exclude_models]
  library_model_names <- tools::file_path_sans_ext(basename(library_model_files))
  
  ## read, compile, & cache all library models
  model_library_list <- set_names(map(library_model_names, ~ mread_cache(model = ., project = mrgsolve::modlib(), soloc = ".")), library_model_names)
  model_library_list
}

extract_mod_table_vars <- function(mod, exclude_types = c("int")) {
  mod@shlib$cpp_variables %>%
    mutate(model = mod@model, .before = type) %>%
    filter(context %in% "table") %>% 
    filter(!type %in% exclude_types)
}

extract_mod_capture_table_vars <- function(mod, exclude_types = c()) {
  mod@shlib$cpp_variables %>% 
    filter(context %in% "table") %>% 
    filter(!type %in% exclude_types) %>% 
    rbind(., tibble::tibble(type = "capture", var = mrgsolve::outvars(mod)$capture, context = "capture")) %>% 
    mutate(model = mod@model, .before = type)
}

###############################################################################|
## Read all custom library models into list ------------------------------------
###############################################################################|
libmod_path <- here::here("model_library")
mrgmods <- read_all_mrgsolve_modlib_models()
libmods <- read_all_custom_library_models(shiny_path = target_path)


libmods_tables <- purrr::map_dfr(libmods, ~ extract_mod_capture_table_vars(.x, exclude_types = "int"))
mrgmods_tables <- purrr::map_dfr(mrgmods, ~ extract_mod_capture_table_vars(.x, exclude_types = "int"))
allmods_tables <- tibble::as_tibble(
  data.table::rbindlist(list(
    custom  = libmods_tables,
    mrgsolve = mrgmods_tables
  ), idcol = "library"))


allmods_tables_df <- as.data.frame(allmods_tables) %>% 
  print(.)

allmods_by_nvars <- allmods_tables %>% 
  group_by(model) %>% 
  summarise(nvars = n_distinct(var)) %>% 
  arrange(desc(nvars)) %>% 
  print(.)



###############################################################################|
## determine subset of valid model output variables ----------------------------
###############################################################################|

## custom model library example:  tmdd_qe_2cmt_1abs
tst_model <- "tmdd_qe_2cmt_1abs"
tst_mod <- libmods[[tst_model]]
out_vars_info <- extract_mod_capture_table_vars(mod = tst_mod, exclude_types = c("int"))
out_vars <- out_vars_info$var
reserved_params <- reserved_library_model_params() %>% 
  print(.)

valid_out_vars <- setdiff(out_vars, reserved_params) %>% 
  print(.)


###############################################################################|
## define function inputs ------------------------------------------------------
###############################################################################|

inp <- list(
  units = "ug/L",
  file1 = NULL,
  sep = ",",
  tgrid_delta = 1,
  variability = "5%-95%",
  sim_start = 0,
  sim_time = 24,
  plot_user_dv = "DV"
)


mod <- libmods[["popPK_2cmt_linear-mm_iv-sc"]]
dose_data <- fill_default_ev_data_cols(mrgsolve::ev_rx(define_default_rx_input()))
tg0 <- mrgsolve::tgrid()
sim_settings <- update_sim_settings_fn(num_subj = 5, num_sims = 3)
sim_template <- expand_sim_dataset_fn(mod, dose_data, sim_settings, tg0)
sim_out_data <- run_sim_series_from_data_template(mod, sim_template, sim_settings) %>% 
  select_sim_output_variable_fn(df = ., var = inp$plot_user_dv)


###############################################################################|
## test `rx_pi_function()` -----------------------------------------------------
###############################################################################|
rx_pi_res <- rx_pi_function(df = sim_out_data, inp = inp) %>% 
  print(.)

rx_pi_res_names <- names(rx_pi_res) %>% 
  print(.)


###############################################################################|
## test `rx_graph_function()` --------------------------------------------------
###############################################################################|
rx_graph_res <- rx_graph_function(df = rx_pi_res, inp = inp, mod = mod) 
print(rx_graph_res)

rx_graph_res_names <- names(rx_graph_res) %>% 
  print(.)


###############################################################################|
## test `rx_numeric_stats_function()` ------------------------------------------
###############################################################################|
rx_stats_res <- rx_numeric_stats_function(df = sim_out_data, 
                                          tgrid_end = inp$sim_time, 
                                          tgrid_start = inp$sim_start, 
                                          units = inp$units, 
                                          sim_end_time = NULL) %>% 
  print(.)

rx_stats_res_names <- names(rx_stats_res) %>% 
  print(.)


###############################################################################|
## minimal test `rx_pi_function()` ---------------------------------------------
###############################################################################|
rx_pi_min_res <- rx_pi_function(
  df = sim_out_data %>% 
    select(ID, TIME, AMT, ADDL, SS, DOSE, YDATA, YNAME, REGLAB), 
  inp = inp)
stopifnot(identical(rx_pi_res, rx_pi_min_res))


###############################################################################|
## minimal test `rx_numeric_stats_function()` ----------------------------------
###############################################################################|
rx_stats_min_res <- rx_numeric_stats_function(
  df = sim_out_data %>% 
    select(ID, TIME, AMT, ADDL, SS, DOSE, YDATA, YNAME, REGLAB), 
  tgrid_end = inp$sim_time, 
  tgrid_start = inp$sim_start, 
  units = inp$units, 
  sim_end_time = NULL)
stopifnot(identical(rx_stats_res, rx_stats_min_res))



