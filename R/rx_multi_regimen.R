#########################################################################################|
## Rx Multiple Dosing Regimen Functions --------------------------------------------------
#########################################################################################|

strict_left_join <- function(x, y, by = NULL, ...) {
  ## source:  https://github.com/tidyverse/dplyr/issues/2278
  by <- dplyr::common_by(by, x, y)
  if (any(duplicated(y[by$y]))) {
    stop("Duplicate values in foreign key")
  } else {
    dplyr::left_join(x, y, by = by, ...)
  }
}

length_plus_1 <- function(x) {
  as.integer(length(x) + 1L)
}

max_plus_1 <- function(x) {
  stopifnot("input argument `x` must be a numeric type!" = isTRUE(is.numeric(x)) || is.null(x))
  if (isTRUE(length(x) %in% 0) || isTRUE(unique(is.na(x)))) {
    return(1L)
  } else {
    as.integer(max(x, na.rm = TRUE) + 1L)
  }
}

build_default_regimen_label <- function(label_str = "REGN", regimen_id = 1L) {
  paste0(label_str, regimen_id)
}

#########################################################################################|
### efficient `rx_table$df_regs` data handling functions ---------------------------------
#########################################################################################|

empty_rx_table <- function() {
  tibble::tibble(REGNUM  = integer(0),
                 REGLAB  = character(0),
                 Include = logical(0),
                 RXLABEL = character(0), 
                 RXDOSE  = list(tibble::tibble()))
}

add_new_rx_regimen <- function(df_regs, rx_label) {
  print_function_calls(fn_name = "add_new_rx_regimen", fn_type = "function")
  df_add_regs <- df_regs %>%
    tibble::add_row(REGNUM = max_plus_1(df_regs$REGNUM),
                    REGLAB = paste0("REGN", REGNUM),
                    Include = TRUE,
                    RXLABEL = rx_label,
                    RXDOSE = list(submit_rx_input_fn(rx_label)))
  df_add_regs
}

add_new_rx_regimen_rv <- function(rx_table, rx_label) {
  print_function_calls(fn_name = "add_new_rx_regimen_rv", fn_type = "function")
  rx_table$df_regs <- rx_table$df_regs %>%
    tibble::add_row(REGNUM = max_plus_1(rx_table$df_regs$REGNUM),
                    REGLAB = paste0("REGN", REGNUM),
                    Include = TRUE,
                    RXLABEL = rx_label,
                    RXDOSE = list(submit_rx_input_fn(rx_label)))
  rx_table
}

parse_hot_changes <- function(hot_inp_chgs, hot_colnames) {
  print_function_calls(fn_name = "parse_hot_changes", fn_type = "function")
  ## TEST CASE:               hot_inp_chgs <- test_input_hot_chgchg; hot_colnames <- names(df_regs);
  
  hot_chgs  <- hot_inp_chgs[[1]]
  row_idx   <- hot_chgs[[1]] + 1L
  col_idx   <- hot_chgs[[2]] + 1L
  col_name  <- hot_colnames[col_idx]
  chg_rxlbl <- col_name %in% "RXLABEL"
  old_val   <- hot_chgs[[3]]
  new_val   <- hot_chgs[[4]]
  val_chg   <- !identical(old_val, new_val)
  list(
    row_idx   = row_idx,
    col_idx   = col_idx,
    col_name  = col_name,
    chg_rxlbl = chg_rxlbl,
    old_val   = old_val,
    new_val   = new_val,
    val_chg   = val_chg
  )
}

unnest_rx_table_dose_data <- function(df_regs) {
  print_function_calls(fn_name = "unnest_rx_table_dose_data", fn_type = "function")
  df_regs %>% 
    tidyr::unnest(RXDOSE) %>% 
    filter(Include) %>% 
    select(-Include) %>% 
    mutate(ID = REGNUM)
}

modify_rx_table_regimen_fn <- function(hot_inp_chgs, rx_table) {
  print_function_calls(fn_name = "modify_rx_table_regimen_fn", fn_type = "function")
  
  hot_chgs <- parse_hot_changes(hot_inp_chgs = hot_inp_chgs, hot_colnames = names(rx_table$df_regs))
  
  ## update `rx_table`
  if (isTRUE(hot_chgs$val_chg)) {
    rx_table$df_regs[hot_chgs$row_idx, hot_chgs$col_idx] <- hot_chgs$new_val
    
    ## recalculate dosing event dataset if any cell in the "RXLABEL" column was changed
    if (isTRUE(hot_chgs$chg_rxlbl)) {
      if (isTRUE(valid_rx_input_fn(hot_chgs$new_val))) {
        new_rx_hot_data <- tibble::as_tibble(submit_rx_input_fn(hot_chgs$new_val))
        rx_table$df_regs[hot_chgs$row_idx, "RXDOSE"] <- new_rx_hot_data %>% tidyr::nest(data = everything())
      } 
    }
  }
  return(rx_table)
}

#########################################################################################|
### DEPRECATED `rx_table$df_regs` functions with concurrency issues ---------------------
#########################################################################################|


add_rx_table_regs_info <- function(rx_regs_data) {
  
  ev_cols <- names(define_default_ev_data_cols())
  
  rx_regs_data %>% 
    mutate(ID = REGNUM) %>% 
    select(REGNUM, REGLAB, tidyselect::all_of(ev_cols), RXLABEL, Include) %>% 
    filter(Include %in% TRUE) %>% 
    select(-Include)
}

rx_table_to_multi_reg_data <- function(df_regs) {
  print_function_calls(fn_name = "rx_table_to_multi_reg_data", fn_type = "function")
  if (isTRUE(length(df_regs$REGNUM) <= 0)) return(df_regs)
  rx_regs_dose <- purrr::map_dfr(df_regs$REGNUM, ~ submit_rx_input_fn(df_regs$RXLABEL[df_regs$REGNUM %in% .x]) %>% mutate(REGNUM = .x))
  rx_regs_data <- rx_regs_dose %>% strict_left_join(., df_regs, by = "REGNUM")
  add_rx_table_regs_info(rx_regs_data)
}


#########################################################################################|
### expand multi regimen dose event data into simulation input dataset -------------------
#########################################################################################|


## redefine `replicate_dose_data_ids` using `assign_distinct_subject_ids`
replicate_dose_data_ids <- function(dose_data, num_subj) {
  print_function_calls(fn_name = "replicate_dose_data_ids", fn_type = "function")
  sim_dose_data <- assign_distinct_subject_ids(dose_data, num_subj = num_subj)
  sim_dose_data
}

assign_distinct_subject_ids <- function(multi_dose, num_subj) {
  print_function_calls(fn_name = "assign_distinct_subject_ids", fn_type = "function")
  if ("data.frame" %in% class(multi_dose) & !"REGNUM" %in% names(multi_dose)) multi_dose <- multi_dose %>% mutate(REGNUM = 1L)
  if ("data.frame" %in% class(multi_dose) & !"REGLAB" %in% names(multi_dose)) multi_dose <- multi_dose %>% mutate(REGLAB = build_default_regimen_label(regimen_id = REGNUM))
  if (as.numeric(num_subj) < 1) num_subj <- 1L
  multi_dose_lst <- if (!"list" %in% class(multi_dose)) list(multi_dose) else multi_dose
  multi_dose_rep <- fill_default_ev_data_cols(purrr::map_dfr(multi_dose_lst, ~ .x %>% ev_rep(ID = 1L:as.integer(num_subj))))
  multi_dose_ids <- multi_dose_rep %>% 
    mutate(IDWRAP = ID, .before = ID) %>% 
    arrange(REGNUM, ID, TIME) %>% 
    group_by(REGNUM, IDWRAP) %>% 
    mutate(ID = as.integer(cur_group_id())) %>% 
    ungroup() %>% 
    select(-IDWRAP)
  multi_dose_ids
}

expand_sim_input_data <- function(mod, dataset, tg, sim_seed) {
  print_function_calls(fn_name = "expand_sim_input_data", fn_type = "function")
  if (is.null(mod)) return(NULL)
  set.seed(sim_seed)
  
  if (!isTRUE(unique(c("REGNUM", "REGLAB") %in% names(dataset)))) dataset <- add_dose_regimen_label(dataset)
  regimen_id_LUT <- dataset %>% select(REGNUM, REGLAB) %>% distinct()
  
  if ("REGNUM" %in% unlist(outvars(mod), use.names = FALSE)) {
    dataset <- dataset %>% 
      rename(REGIDS = REGNUM)
  }
  
  sim_out <- mrgsim_d(x = mod, data = dataset, tgrid = tg,
                      carry_out = names(dataset), recsort=3) %>%
    select(names(define_default_ev_data_cols()), everything()) %>% 
    mutate(across(any_of(c(define_integer_ev_data_cols(), "REGNUM")), as.integer)) %>%
    select(everything())
  
  if ("REGNUM" %in% unlist(outvars(mod), use.names = FALSE)) {
    sim_out <- sim_out %>% 
      mutate(REGNUM = as.integer(REGIDS)) %>% 
      select(-REGIDS)
  }
  
  sim_out <- strict_left_join(sim_out, regimen_id_LUT, by = "REGNUM") %>% 
    relocate(REGLAB, .after = REGNUM)
  
  ## `format_sim_output_data` messes up REGLAB & REGNUM
  sim_template <- as_tibble(sim_out)[setdiff(names(sim_out), extract_model_cmt_names(mod))]
  sim_template
}

expand_sim_dataset_fn <- function(mod, dose_data, sim_settings, tg_input, end_time_scale_factor = define_tgrid_end_scaling_factor()) {
  print_function_calls(fn_name = "expand_sim_dataset_fn", fn_type = "function")
  
  if (is.null(mod)) return(NULL)
  
  ## expand event dataset to population of subjects
  dataset <- replicate_dose_data_ids(dose_data = dose_data, num_subj = sim_settings$num_subj)
  sim_template <- expand_sim_input_data(mod = mod, dataset = dataset, tg = tg_input, sim_seed = sim_settings$sim_seed)
  return(sim_template)
}

run_sim_series_from_data_template <- function(mod, sim_template, sim_settings) {
  print_function_calls(fn_name = "run_sim_series_from_data_template", fn_type = "function")
  if (is.null(mod)) return(NULL)
  
  sim_template_cols <- names(sim_template)
  carry_sim_cols <- names(define_default_ev_data_cols())
  
  regimen_id_LUT <- sim_template %>% select(REGNUM, REGLAB) %>% distinct()
  
  ## extract user-specified simulation settings
  num_subj <- sim_settings$num_subj
  num_sims <- sim_settings$num_sims
  sim_seed <- sim_settings$sim_seed
  
  set.seed(sim_seed)
  
  sim_out <- parallel::mclapply(1:num_sims, function(i) {
    mod %>% mrgsim_d(sim_template, carry_out = c(carry_sim_cols, "REGNUM")) %>%
      mutate(REPI = i, .before = ID)
  }) %>% bind_rows(.) %>% select(REPI, tidyselect::any_of(sim_template_cols), everything()) %>% 
    mutate(across(any_of(c(define_integer_ev_data_cols(), "REGNUM")), as.integer)) %>%
    select(everything())
  
  
  ## `format_sim_output_data` messes up REGLAB & REGNUM
  sim_out <- strict_left_join(sim_out, regimen_id_LUT, by = "REGNUM") %>% 
    relocate(REGLAB, .after = REGNUM)
  sim_series_data <- as_tibble(sim_out)[setdiff(names(sim_out), extract_model_cmt_names(mod))]
  sim_series_data
}







