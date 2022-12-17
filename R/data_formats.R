## adding functions for future event reactives
extract_model_cmt_names <- function(mod) {
  mrgsolve::outvars(mod)$cmt
}

extract_model_capture_vars <- function(mod) {
  mrgsolve::outvars(mod)$capture
}

default_model_output_var <- function(mod) {
  if("DV" %in% names(mod@capture)){
    return("DV")
  }
  else {
    rev(extract_model_capture_vars(mod))[1]
  }
}


## build sim template dataset --> copied over from "R/backend.R"
format_sim_output_data <- function(sim_data, drop_cols = NULL) {
  print_function_calls(fn_name = "format_sim_output_data", fn_type = "function")
  
  drop_col_idxs <- match(drop_cols, names(sim_data))[!is.na(match(drop_cols, names(sim_data)))]
  
  sim_data_template <- sim_data %>%
    as_tibble(.) %>%
    select(-tidyselect::any_of(drop_col_idxs)) %>%
    add_dose_regimen_label(df = .)
  
  sim_data_template
}


add_dose_regimen_label <- function(df) {
  df<-df %>%
    mutate(REGLAB = ifelse("REGLAB" %in% names(.), REGLAB, "REGN1"),
           REGNUM = as.integer(gsub(pattern="(?i)REGN", replacement="", REGLAB, perl=TRUE)))
  
  df
}


select_sim_output_variable_fn <- function(df, var = "DV") {
  print_function_calls(fn_name = "select_sim_output_variable_fn", fn_type = "function")
  var_name <- as.character(var)
  if (isTRUE(var_name %in% names(df))) {
    df <- df %>% mutate(YNAME = var_name) %>% mutate(YDATA = df[[var_name]])
  }
  df
}


list_valid_output_vars_fn <- function(mod, user_data = NULL) {
  print_function_calls(fn_name = "list_valid_output_vars_fn", fn_type = "function")
  mod_capture_vars <- extract_model_capture_vars(mod)
  out_vars_list <- intersect(mod_capture_vars, names(user_data))
  if (isTRUE(rlang::is_empty(out_vars_list))) out_vars_list <- mod_capture_vars
  setdiff(out_vars_list, reserved_library_model_params())
}


# return column names for user uploaded data containing class numeric or integer 
colnames_numint  <- function (df){
  print_function_calls(fn_name = "colnames_numint", fn_type = "function")
  df %>%
    verify(has_all_names("ID", "TIME")) %>% # has ID and TIME columns at a minimum
    verify(nrow(.) > 1) %>% # has at least 1 row other than columns
    verify(TIME >= 0) #time is >=0
  {if("EVID" %in% names(df))  df %>% assert(within_bounds(0,8), EVID)} # TODO: trying to figure out how to catch EVID within bounds without crashing
  return (names (sapply (df, class )[sapply (df, class ) == "integer" | # must be int or num
                                       sapply (df, class ) == "numeric"] ))
}