## print log debugging statements
print("test-rx_numeric_stats_function.R")
print(getwd())




## docs for `testthat::source_*` functions:  https://testthat.r-lib.org/reference/source_file.html
here::i_am("tests/testthat/test-rx_numeric_stats_function.R")
if (isTRUE(interactive())) {
  testthat::source_test_helpers("tests/testthat", env = rlang::current_env())
  testthat::source_test_setup("tests/testthat", env = rlang::current_env())
}

test_df_numeric_stat <- function (inp, numSubj = 11, sumSim = 1, zero_re = F){
  def_mod <- app_libmod_objs[[fetch_default_libmod_name()]]
  if (zero_re){def_mod <- def_mod %>% mrgsolve::zero_re()}
  dose_data <- fill_default_ev_data_cols(mrgsolve::ev_rx(define_default_rx_input()))
  tg0 <- mrgsolve::tgrid(start = as.numeric (inp$tgrid_start), end = as.numeric (inp$tgrid_end), 
                         delta = 1, add = numeric(0))
  sim_settings <- update_sim_settings_fn(num_subj = numSubj, num_sims = sumSim)
  sim_template <- expand_sim_dataset_fn(def_mod, dose_data, sim_settings, tg0)
  sim_out <- run_sim_series_from_data_template(def_mod, sim_template, sim_settings)
  df <- select_sim_output_variable_fn(df = sim_out, var = inp$select_output_var)
  return (df)
}

test_cmax_tmax <- function (df,inp){
  df %>%filter (TIME >= as.numeric (inp$tgrid_start),
                TIME <= as.numeric (inp$tgrid_end)) %>% 
    group_by (ID, REGLAB, REPI) %>% 
    summarise (Cmax= max(YDATA, na.rm = T), 
               Tmax=TIME[which.max(YDATA)] ) %>%
    ungroup () %>% 
    group_by (REGLAB, REPI) %>%
    summarise (Cmax = median (Cmax, na.rm = T),
               Tmax = median (Tmax, na.rm = T)) %>% 
    ungroup () %>% 
    group_by (REGLAB) %>% 
    summarise (cmax_median = median(Cmax, na.rm = T),
               cmax_mean = mean(Cmax, na.rm = T),
               cmax_sd = sd(Cmax, na.rm = T),
               Tmax_median = median(Tmax, na.rm = T),
               Tmax_mean = mean(Tmax, na.rm = T),
               Tmax_sd = sd(Tmax, na.rm = T)) %>% 
    ungroup () %>% 
    as.data.frame()
}

test_cmin_tmin <- function (df,inp){
  df %>%filter (TIME >= as.numeric (inp$tgrid_start),
                TIME <= as.numeric (inp$tgrid_end)) %>%
    group_by (ID, REGLAB, REPI) %>% 
    summarise (Cmin= YDATA[which.max(TIME)], 
               Tmin= max(TIME, na.rm = T) ) %>%
    ungroup () %>% 
    group_by (REGLAB, REPI) %>%
    summarise (Cmin = median (Cmin, na.rm = T),
               Tmin = median (Tmin, na.rm = T)) %>% 
    ungroup () %>% 
    group_by (REGLAB) %>% 
    summarise (cmin_median = median(Cmin, na.rm = T),
               cmin_mean = mean(Cmin, na.rm = T),
               cmin_sd = sd(Cmin, na.rm = T),
               Tmin_median = median(Tmin, na.rm = T),
               Tmin_mean = mean(Tmin, na.rm = T),
               Tmin_sd = sd(Tmin, na.rm = T)) %>% 
    ungroup () %>% 
    as.data.frame()
}

test_auc <- function (df, inp){
  df %>% filter (TIME >= as.numeric (inp$tgrid_start),
                 TIME <= as.numeric (inp$tgrid_end)) %>% 
    group_by(ID, REGLAB, REPI) %>%  
    summarize(AUC = auc(TIME,YDATA)) %>% 
    ungroup () %>% 
    group_by (REGLAB, REPI) %>%
    summarise (AUC = median (AUC, na.rm = T)) %>% 
    ungroup () %>% 
    group_by (REGLAB) %>%
    summarize(auc_median = median(AUC, na.rm = T),
              auc_mean = mean(AUC, na.rm = T),
              auc_sd = sd(AUC, na.rm = T))%>% 
    ungroup ()%>% 
    as.data.frame()
}

sumstat_struct_names <- summ_stat_table_struct() %>% names ()

#######################################################################################################################|
## mock `print_function_calls()` "R/helpers.R" function so messages are suppressed during unit testing -------------
#######################################################################################################################|

mockr::with_mock(
  print_function_calls = omit_function_calls,
  .env = topenv(), {
  
    
    #########################################################################################|
    ## SETUP --------------------------------------------------------------------------------
    #########################################################################################|
    inp <- list(
      units = "ug/L",
      tgrid_start = "0",
      tgrid_end = "24",
      select_output_var = "DV"
    )
    
    sim_end <- 24
    
    df <- test_df_numeric_stat (inp, numSubj = 1)
    com1 <- rx_numeric_stats_function(df, tgrid_end =inp$tgrid_end, tgrid_start= inp$tgrid_start, units = inp$units, sim_end_time = sim_end)
    test_cmax_tmax1 <- test_cmax_tmax(df,inp)
    test_cmin_tmin1 <- test_cmin_tmin(df,inp)
    test_auc1 <- test_auc (df,inp)
    
    ### rx_numeric_stats_function() ------------------------------------------------------------
    testthat::test_that("rx_numeric_stats_function", {
      testthat::expect_s3_class(com1, "data.frame")
      testthat::expect_named(com1, sumstat_struct_names)
      testthat::expect_identical(com1, 
                                 data.frame (Parameter = c("Cmax", "Tmax", "Clast", "Tlast", "AUC"), 
                                             Units = c("ug/L", "days", "ug/L", "days", "days * ug/L"), 
                                             Regimen = c("REGN1", "REGN1", "REGN1", "REGN1", "REGN1"), 
                                             Interval = c("0 - 24", "0 - 24", "0 - 24", "0 - 24", "0 - 24"), 
                                             Median = c(8.63, 1, 0.4, 24, 78.25), 
                                             Mean = c(8.63, 1, 0.4, 24, 78.25), 
                                             SD = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))%>% select(any_of(sumstat_struct_names)))
      
    })

    
    testthat::test_that("rx_numeric_stats_function", {
      # cmax and tmin
      testthat::expect_identical(com1[1,"Median"] , test_cmax_tmax1[1,"cmax_median"] %>% round (2))
      testthat::expect_identical(com1[2,"Median"] , test_cmax_tmax1[1,"Tmax_median"] %>% round (2))
      testthat::expect_identical(com1[1,"Mean"] , test_cmax_tmax1[1,"cmax_mean"] %>% round (2))
      testthat::expect_identical(com1[2,"Mean"] , test_cmax_tmax1[1,"Tmax_mean"] %>% round (2))
      testthat::expect_identical(com1[1,"SD"] , NA_real_)
      testthat::expect_identical(com1[2,"SD"] , NA_real_)
      # cmin and tmin
      testthat::expect_identical(com1[3,"Median"] , test_cmin_tmin1[1,"cmin_median"] %>% round (2))
      testthat::expect_identical(com1[4,"Median"] , test_cmin_tmin1[1,"Tmin_median"] %>% round (2))
      testthat::expect_identical(com1[3,"Mean"] , test_cmin_tmin1[1,"cmin_mean"] %>% round (2))
      testthat::expect_identical(com1[4,"Mean"] , test_cmin_tmin1[1,"Tmin_mean"] %>% round (2))
      testthat::expect_identical(com1[3,"SD"] , NA_real_)
      testthat::expect_identical(com1[4,"SD"] , NA_real_)
      # AUC
      testthat::expect_identical(com1[5,"Median"] , test_auc1[1,"auc_median"] %>% round (2))
      testthat::expect_identical(com1[5,"Mean"] , test_auc1[1,"auc_mean"] %>% round (2))
      testthat::expect_identical(com1[5,"SD"] , NA_real_)
    })
    
    #########################################################################################|
    ## SETUP 2-------------------------------------------------------------------------------
    #########################################################################################|
    
    inp <- list(
      units = "ug/L",
      tgrid_start = "0",
      tgrid_end = "48",
      select_output_var = "DV"
    )
    
    df <- test_df_numeric_stat (inp)
    com2 <- rx_numeric_stats_function(df, tgrid_end =inp$tgrid_end, tgrid_start= inp$tgrid_start, units = inp$units)
    test_cmax_tmax1 <- test_cmax_tmax(df,inp)
    test_cmin_tmin1 <- test_cmin_tmin(df,inp)
    test_auc1 <- test_auc (df,inp)
    
    ### rx_numeric_stats_function() ------------------------------------------------------------
    testthat::test_that("rx_numeric_stats_function", {
      testthat::expect_s3_class(com2, "data.frame")
      testthat::expect_named(com2, sumstat_struct_names)
      testthat::expect_identical(com2, 
                                 data.frame (Parameter = c("Cmax", "Tmax", "Clast", "Tlast", "AUC"), 
                                             Units = c("ug/L", "days", "ug/L", "days", "days * ug/L"), 
                                             Regimen = c("REGN1", "REGN1", "REGN1", "REGN1", "REGN1"), 
                                             Interval = c("0 - 48", "0 - 48", "0 - 48", "0 - 48", "0 - 48"), 
                                             Median = c(7.17, 1, 2.58, 48, 107.96), 
                                             Mean = c(7.17, 1, 2.58, 48, 107.96), 
                                             SD = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))%>% select(any_of(sumstat_struct_names)))
      
    })
    
    
    testthat::test_that("rx_numeric_stats_function", {
      # cmax and tmin
      testthat::expect_identical(com2[1,"Median"] , test_cmax_tmax1[1,"cmax_median"] %>% round (2))
      testthat::expect_identical(com2[2,"Median"] , test_cmax_tmax1[1,"Tmax_median"] %>% round (2))
      testthat::expect_identical(com2[1,"Mean"] , test_cmax_tmax1[1,"cmax_mean"] %>% round (2))
      testthat::expect_identical(com2[2,"Mean"] , test_cmax_tmax1[1,"Tmax_mean"] %>% round (2))
      testthat::expect_identical(com2[1,"SD"] , test_cmax_tmax1[1,"cmax_sd"] %>% round (2))
      testthat::expect_identical(com2[2,"SD"] , test_cmax_tmax1[1,"Tmax_sd"] %>% round (2))
      # cmin and tmin
      testthat::expect_identical(com2[3,"Median"] , test_cmin_tmin1[1,"cmin_median"] %>% round (2))
      testthat::expect_identical(com2[4,"Median"] , test_cmin_tmin1[1,"Tmin_median"] %>% round (2))
      testthat::expect_identical(com2[3,"Mean"] , test_cmin_tmin1[1,"cmin_mean"] %>% round (2))
      testthat::expect_identical(com2[4,"Mean"] , test_cmin_tmin1[1,"Tmin_mean"] %>% round (2))
      testthat::expect_identical(com2[3,"SD"] , test_cmin_tmin1[1,"cmin_sd"] %>% round (2))
      testthat::expect_identical(com2[4,"SD"] , test_cmin_tmin1[1,"Tmin_sd"] %>% round (2))
      # AUC
      testthat::expect_identical(com2[5,"Median"] , test_auc1[1,"auc_median"] %>% round (2))
      testthat::expect_identical(com2[5,"Mean"] , test_auc1[1,"auc_mean"] %>% round (2))
      testthat::expect_identical(com2[5,"SD"] , test_auc1[1,"auc_sd"] %>% round (2))
    })
    
    
    #########################################################################################|
    ## SETUP 3--------------------------------------------------------------------------------
    ## setup 1 with different end time 
    #########################################################################################|
    inp <- list(
      units = "ug/L",
      tgrid_start = "0",
      tgrid_end = "48",
      select_output_var = "DV"
    )
    
    sim_end <- 48
    
    df <- test_df_numeric_stat (inp, numSubj = 1)
    
    test_cmax_tmax1 <- test_cmax_tmax(df,inp)     
    test_cmin_tmin1 <- test_cmin_tmin(df,inp)
    test_auc1 <- test_auc (df,inp)
    
    inp <- list(
      units = "ug/L",
      tgrid_start = "0",
      tgrid_end = "24",
      select_output_var = "DV"
    )
    com3 <- rx_numeric_stats_function(df, tgrid_end =inp$tgrid_end, tgrid_start= inp$tgrid_start, units = inp$units, sim_end_time = sim_end)
    
    ### rx_numeric_stats_function() ------------------------------------------------------------
    testthat::test_that("rx_numeric_stats_function", {
      testthat::expect_s3_class(com3, "data.frame")
      testthat::expect_named(com3, sumstat_struct_names)
      testthat::expect_identical(com3, 
                                 data.frame (Parameter = c("Cmax", "Tmax", "Clast", "Tlast", "AUC"), 
                                             Units = c("ug/L", "days", "ug/L", "days", "days * ug/L"), 
                                             Regimen = c("REGN1", "REGN1", "REGN1", "REGN1", "REGN1"), 
                                             Interval = c("0 - 48", "0 - 48", "0 - 48", "0 - 48", "0 - 48"), 
                                             Median = c(8.75, 1, 2.35, 48, 128.67), 
                                             Mean = c(8.75, 1, 2.35, 48, 128.67), 
                                             SD = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))%>% select(any_of(sumstat_struct_names)))
      
    })
    
    
    testthat::test_that("rx_numeric_stats_function", {
      # cmax and tmin
      testthat::expect_identical(com3[1,"Median"] , test_cmax_tmax1[1,"cmax_median"] %>% round (2))
      testthat::expect_identical(com3[2,"Median"] , test_cmax_tmax1[1,"Tmax_median"] %>% round (2))
      testthat::expect_identical(com3[1,"Mean"] , test_cmax_tmax1[1,"cmax_mean"] %>% round (2))
      testthat::expect_identical(com3[2,"Mean"] , test_cmax_tmax1[1,"Tmax_mean"] %>% round (2))
      testthat::expect_identical(com3[1,"SD"] , test_cmax_tmax1[1,"cmax_sd"] %>% round (2))
      testthat::expect_identical(com3[2,"SD"] , test_cmax_tmax1[1,"Tmax_sd"] %>% round (2))
      # cmin and tmin
      testthat::expect_identical(com3[3,"Median"] , test_cmin_tmin1[1,"cmin_median"] %>% round (2))
      testthat::expect_identical(com3[4,"Median"] , test_cmin_tmin1[1,"Tmin_median"] %>% round (2))
      testthat::expect_identical(com3[3,"Mean"] , test_cmin_tmin1[1,"cmin_mean"] %>% round (2))
      testthat::expect_identical(com3[4,"Mean"] , test_cmin_tmin1[1,"Tmin_mean"] %>% round (2))
      testthat::expect_identical(com3[3,"SD"] , test_cmin_tmin1[1,"cmin_sd"] %>% round (2))
      testthat::expect_identical(com3[4,"SD"] , test_cmin_tmin1[1,"Tmin_sd"] %>% round (2))
      # AUC
      testthat::expect_identical(com3[5,"Median"] , test_auc1[1,"auc_median"] %>% round (2))
      testthat::expect_identical(com3[5,"Mean"] , test_auc1[1,"auc_mean"] %>% round (2))
      testthat::expect_identical(com3[5,"SD"] , test_auc1[1,"auc_sd"] %>% round (2))
    })
    
    #########################################################################################|
    ## SETUP 4-------------------------------------------------------------------------------
    ## setup 2 with inp$select_output_var = "IPRED"
    #########################################################################################|
    
    inp <- list(
      units = "ug/L",
      tgrid_start = "0",
      tgrid_end = "48",
      select_output_var = "IPRED"
    )
    
    df <- test_df_numeric_stat (inp)
    com4 <- rx_numeric_stats_function(df, tgrid_end =inp$tgrid_end, tgrid_start= inp$tgrid_start, units = inp$units)
    test_cmax_tmax1 <- test_cmax_tmax(df,inp)     
    test_cmin_tmin1 <- test_cmin_tmin(df,inp)
    test_auc1 <- test_auc (df,inp)
    
    ### rx_numeric_stats_function() ------------------------------------------------------------
    testthat::test_that("rx_numeric_stats_function", {
      testthat::expect_s3_class(com4, "data.frame")
      testthat::expect_named(com4, sumstat_struct_names)
      testthat::expect_identical(com4, 
                                 data.frame (Parameter = c("Cmax", "Tmax", "Clast", "Tlast", "AUC"), 
                                             Units = c("ug/L", "days", "ug/L", "days", "days * ug/L"), 
                                             Regimen = c("REGN1", "REGN1", "REGN1", "REGN1", "REGN1"), 
                                             Interval = c("0 - 48", "0 - 48", "0 - 48", "0 - 48", "0 - 48"), 
                                             Median = c(7.55, 1, 2.47, 48, 105.72), 
                                             Mean = c(7.55, 1, 2.47, 48, 105.72), 
                                             SD = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))%>% select(any_of(sumstat_struct_names)))
      
    })
    
    
    testthat::test_that("rx_numeric_stats_function", {
      # cmax and tmin
      testthat::expect_identical(com4[1,"Median"] , test_cmax_tmax1[1,"cmax_median"] %>% round (2))
      testthat::expect_identical(com4[2,"Median"] , test_cmax_tmax1[1,"Tmax_median"] %>% round (2))
      testthat::expect_identical(com4[1,"Mean"] , test_cmax_tmax1[1,"cmax_mean"] %>% round (2))
      testthat::expect_identical(com4[2,"Mean"] , test_cmax_tmax1[1,"Tmax_mean"] %>% round (2))
      testthat::expect_identical(com4[1,"SD"] , test_cmax_tmax1[1,"cmax_sd"] %>% round (2))
      testthat::expect_identical(com4[2,"SD"] , test_cmax_tmax1[1,"Tmax_sd"] %>% round (2))
      # cmin and tmin
      testthat::expect_identical(com4[3,"Median"] , test_cmin_tmin1[1,"cmin_median"] %>% round (2))
      testthat::expect_identical(com4[4,"Median"] , test_cmin_tmin1[1,"Tmin_median"] %>% round (2))
      testthat::expect_identical(com4[3,"Mean"] , test_cmin_tmin1[1,"cmin_mean"] %>% round (2))
      testthat::expect_identical(com4[4,"Mean"] , test_cmin_tmin1[1,"Tmin_mean"] %>% round (2))
      testthat::expect_identical(com4[3,"SD"] , test_cmin_tmin1[1,"cmin_sd"] %>% round (2))
      testthat::expect_identical(com4[4,"SD"] , test_cmin_tmin1[1,"Tmin_sd"] %>% round (2))
      # AUC
      testthat::expect_identical(com4[5,"Median"] , test_auc1[1,"auc_median"] %>% round (2))
      testthat::expect_identical(com4[5,"Mean"] , test_auc1[1,"auc_mean"] %>% round (2))
      testthat::expect_identical(com4[5,"SD"] , test_auc1[1,"auc_sd"] %>% round (2))
    })
    
    #########################################################################################|
    ## SETUP 5-------------------------------------------------------------------------------
    ## setup 2 num sims = 5
    #########################################################################################|
    
    inp <- list(
      units = "ug/L",
      tgrid_start = "0",
      tgrid_end = "48",
      select_output_var = "DV"
    )
    
    sim_end <- 48
    
    df <- test_df_numeric_stat (inp, sumSim = 5)
    #com5 <- rx_numeric_stats_function(df,inp,sim_end)
    test_cmax_tmax1 <- test_cmax_tmax(df,inp)     
    test_cmin_tmin1 <- test_cmin_tmin(df,inp)
    test_auc1 <- test_auc (df,inp)
    
    # change value inp, so function have to use sim_end in order to be correct
    inp <- list(
      units = "ug/L",
      tgrid_start = "0",
      tgrid_end = "200",
      select_output_var = "DV"
    )
    
    com5 <- rx_numeric_stats_function(df, tgrid_end =inp$tgrid_end, tgrid_start= inp$tgrid_start, units = inp$units, sim_end_time = sim_end)
    
    ### rx_numeric_stats_function() ------------------------------------------------------------
    testthat::test_that("rx_numeric_stats_function", {
      testthat::expect_s3_class(com5, "data.frame")
      testthat::expect_named(com5, sumstat_struct_names)
      testthat::expect_identical(com5, 
                                 data.frame (Parameter = c("Cmax", "Tmax", "Clast", "Tlast", "AUC"), 
                                             Units = c("ug/L", "days", "ug/L", "days", "days * ug/L" ), 
                                             Regimen = c("REGN1", "REGN1", "REGN1", "REGN1", "REGN1"), 
                                             Interval = c("0 - 48", "0 - 48", "0 - 48", "0 - 48", "0 - 48"), 
                                             Median = c(7.1, 1, 2.63, 48, 107.96), 
                                             Mean = c(6.86, 1, 2.67, 48, 105.58), 
                                             SD = c(0.61, 0, 0.13, 0, 6.49))%>% select(any_of(sumstat_struct_names)))
      
    })
    
    
    testthat::test_that("rx_numeric_stats_function", {
      # cmax and tmin
      testthat::expect_identical(com5[1,"Median"] , test_cmax_tmax1[1,"cmax_median"] %>% round (2))
      testthat::expect_identical(com5[2,"Median"] , test_cmax_tmax1[1,"Tmax_median"] %>% round (2))
      testthat::expect_identical(com5[1,"Mean"] , test_cmax_tmax1[1,"cmax_mean"] %>% round (2))
      testthat::expect_identical(com5[2,"Mean"] , test_cmax_tmax1[1,"Tmax_mean"] %>% round (2))
      testthat::expect_identical(com5[1,"SD"] , test_cmax_tmax1[1,"cmax_sd"] %>% round (2))
      testthat::expect_identical(com5[2,"SD"] , test_cmax_tmax1[1,"Tmax_sd"] %>% round (2))
      # cmin and tmin
      testthat::expect_identical(com5[3,"Median"] , test_cmin_tmin1[1,"cmin_median"] %>% round (2))
      testthat::expect_identical(com5[4,"Median"] , test_cmin_tmin1[1,"Tmin_median"] %>% round (2))
      testthat::expect_identical(com5[3,"Mean"] , test_cmin_tmin1[1,"cmin_mean"] %>% round (2))
      testthat::expect_identical(com5[4,"Mean"] , test_cmin_tmin1[1,"Tmin_mean"] %>% round (2))
      testthat::expect_identical(com5[3,"SD"] , test_cmin_tmin1[1,"cmin_sd"] %>% round (2))
      testthat::expect_identical(com5[4,"SD"] , test_cmin_tmin1[1,"Tmin_sd"] %>% round (2))
      # AUC
      testthat::expect_identical(com5[5,"Median"] , test_auc1[1,"auc_median"] %>% round (2))
      testthat::expect_identical(com5[5,"Mean"] , test_auc1[1,"auc_mean"] %>% round (2))
      testthat::expect_identical(com5[5,"SD"] , test_auc1[1,"auc_sd"] %>% round (2))
    })
    
    
    #########################################################################################|
    ## SETUP 6-------------------------------------------------------------------------------
    ## setup 2 with zero random effect 
    #########################################################################################|
    
    inp <- list(
      units = "ug/L",
      tgrid_start = "0",
      tgrid_end = "48",
      select_output_var = "DV"
    )
    
    df <- test_df_numeric_stat (inp, zero_re = T)
    com6 <- rx_numeric_stats_function(df, tgrid_end =inp$tgrid_end, tgrid_start= inp$tgrid_start, units = inp$units)
    test_cmax_tmax1 <- test_cmax_tmax(df,inp)     
    test_cmin_tmin1 <- test_cmin_tmin(df,inp)
    test_auc1 <- test_auc (df,inp)
    
    ### rx_numeric_stats_function() ------------------------------------------------------------
    testthat::test_that("rx_numeric_stats_function", {
      testthat::expect_s3_class(com6, "data.frame")
      testthat::expect_named(com6, sumstat_struct_names)
      testthat::expect_identical(com6, 
                                 data.frame (Parameter = c("Cmax", "Tmax", "Clast", "Tlast", "AUC"), 
                                             Units = c("ug/L", "days", "ug/L", "days", "days * ug/L"), 
                                             Regimen = c("REGN1", "REGN1", "REGN1", "REGN1", "REGN1"), 
                                             Interval = c("0 - 48", "0 - 48", "0 - 48", "0 - 48", "0 - 48"), 
                                             Median = c(6.9, 1, 2.66, 48, 107.83), 
                                             Mean = c(6.9, 1, 2.66, 48, 107.83), 
                                             SD = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))%>% select(any_of(sumstat_struct_names)))
      
    })
    
    
    testthat::test_that("rx_numeric_stats_function", {
      # cmax and tmin
      testthat::expect_identical(com6[1,"Median"] , test_cmax_tmax1[1,"cmax_median"] %>% round (2))
      testthat::expect_identical(com6[2,"Median"] , test_cmax_tmax1[1,"Tmax_median"] %>% round (2))
      testthat::expect_identical(com6[1,"Mean"] , test_cmax_tmax1[1,"cmax_mean"] %>% round (2))
      testthat::expect_identical(com6[2,"Mean"] , test_cmax_tmax1[1,"Tmax_mean"] %>% round (2))
      testthat::expect_identical(com6[1,"SD"] , test_cmax_tmax1[1,"cmax_sd"] %>% round (2))
      testthat::expect_identical(com6[2,"SD"] , test_cmax_tmax1[1,"Tmax_sd"] %>% round (2))
      # cmin and tmin
      testthat::expect_identical(com6[3,"Median"] , test_cmin_tmin1[1,"cmin_median"] %>% round (2))
      testthat::expect_identical(com6[4,"Median"] , test_cmin_tmin1[1,"Tmin_median"] %>% round (2))
      testthat::expect_identical(com6[3,"Mean"] , test_cmin_tmin1[1,"cmin_mean"] %>% round (2))
      testthat::expect_identical(com6[4,"Mean"] , test_cmin_tmin1[1,"Tmin_mean"] %>% round (2))
      testthat::expect_identical(com6[3,"SD"] , test_cmin_tmin1[1,"cmin_sd"] %>% round (2))
      testthat::expect_identical(com6[4,"SD"] , test_cmin_tmin1[1,"Tmin_sd"] %>% round (2))
      # AUC
      testthat::expect_identical(com6[5,"Median"] , test_auc1[1,"auc_median"] %>% round (2))
      testthat::expect_identical(com6[5,"Mean"] , test_auc1[1,"auc_mean"] %>% round (2))
      testthat::expect_identical(com6[5,"SD"] , test_auc1[1,"auc_sd"] %>% round (2))
    })
    
    #########################################################################################|
    ## SETUP 7-------------------------------------------------------------------------------
    # setup 2 with t_start < 0
    #########################################################################################|
    
    inp <- list(
      units = "ug/L",
      tgrid_start = "-100",
      tgrid_end = "48",
      select_output_var = "DV"
    )
    
    df <- test_df_numeric_stat (inp)
    com7 <- rx_numeric_stats_function(df, tgrid_end =inp$tgrid_end, tgrid_start= inp$tgrid_start, units = inp$units)
    test_cmax_tmax1 <- test_cmax_tmax(df,inp)     
    test_cmin_tmin1 <- test_cmin_tmin(df,inp)
    test_auc1 <- test_auc (df,inp)
    
    ### rx_numeric_stats_function() ------------------------------------------------------------
    testthat::test_that("rx_numeric_stats_function", {
      testthat::expect_s3_class(com7, "data.frame")
      testthat::expect_named(com7, sumstat_struct_names)
      testthat::expect_identical(com7, 
                                 data.frame (Parameter = c("Cmax", "Tmax", "Clast", "Tlast", "AUC"), 
                                             Units = c("ug/L", "days", "ug/L", "days", "days * ug/L" ), 
                                             Regimen = c("REGN1", "REGN1", "REGN1", "REGN1", "REGN1"), 
                                             Interval = c("-100 - 48", "-100 - 48", "-100 - 48", "-100 - 48", "-100 - 48"), 
                                             Median = c(7.78, 1, 2.42, 48, 113.23), 
                                             Mean = c(7.78, 1, 2.42, 48, 113.23), 
                                             SD = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))%>% select(any_of(sumstat_struct_names)))
      
    })
    
    
    testthat::test_that("rx_numeric_stats_function", {
      # cmax and tmin
      testthat::expect_identical(com7[1,"Median"] , test_cmax_tmax1[1,"cmax_median"] %>% round (2))
      testthat::expect_identical(com7[2,"Median"] , test_cmax_tmax1[1,"Tmax_median"] %>% round (2))
      testthat::expect_identical(com7[1,"Mean"] , test_cmax_tmax1[1,"cmax_mean"] %>% round (2))
      testthat::expect_identical(com7[2,"Mean"] , test_cmax_tmax1[1,"Tmax_mean"] %>% round (2))
      testthat::expect_identical(com7[1,"SD"] , test_cmax_tmax1[1,"cmax_sd"] %>% round (2))
      testthat::expect_identical(com7[2,"SD"] , test_cmax_tmax1[1,"Tmax_sd"] %>% round (2))
      # cmin and tmin
      testthat::expect_identical(com7[3,"Median"] , test_cmin_tmin1[1,"cmin_median"] %>% round (2))
      testthat::expect_identical(com7[4,"Median"] , test_cmin_tmin1[1,"Tmin_median"] %>% round (2))
      testthat::expect_identical(com7[3,"Mean"] , test_cmin_tmin1[1,"cmin_mean"] %>% round (2))
      testthat::expect_identical(com7[4,"Mean"] , test_cmin_tmin1[1,"Tmin_mean"] %>% round (2))
      testthat::expect_identical(com7[3,"SD"] , test_cmin_tmin1[1,"cmin_sd"] %>% round (2))
      testthat::expect_identical(com7[4,"SD"] , test_cmin_tmin1[1,"Tmin_sd"] %>% round (2))
      # AUC
      testthat::expect_identical(com7[5,"Median"] , test_auc1[1,"auc_median"] %>% round (2))
      testthat::expect_identical(com7[5,"Mean"] , test_auc1[1,"auc_mean"] %>% round (2))
      testthat::expect_identical(com7[5,"SD"] , test_auc1[1,"auc_sd"] %>% round (2))
    })
    
    #########################################################################################|
    ## SETUP 8-------------------------------------------------------------------------------
    # setup 2 with t_end > max (TIME)
    #########################################################################################|
    
    inp <- list(
      units = "ug/L",
      tgrid_start = "0",
      tgrid_end = "300",
      select_output_var = "DV"
    )
    
    df <- test_df_numeric_stat (inp)
    com8 <- rx_numeric_stats_function(df, tgrid_end =inp$tgrid_end, tgrid_start= inp$tgrid_start, units = inp$units)
    test_cmax_tmax1 <- test_cmax_tmax(df,inp)     
    test_cmin_tmin1 <- test_cmin_tmin(df,inp)
    test_auc1 <- test_auc (df,inp)
    
    ### rx_numeric_stats_function() ------------------------------------------------------------
    testthat::test_that("rx_numeric_stats_function", {
      testthat::expect_s3_class(com8, "data.frame")
      testthat::expect_named(com8, sumstat_struct_names)
      testthat::expect_identical(com8, 
                                 data.frame (Parameter = c("Cmax", "Tmax", "Clast", "Tlast", "AUC"), 
                                             Units = c("ug/L", "days", "ug/L", "days", "days * ug/L"), 
                                             Regimen = c("REGN1", "REGN1", "REGN1", "REGN1", "REGN1"), 
                                             Interval = c("0 - 300", "0 - 300", "0 - 300", "0 - 300", "0 - 300"), 
                                             Median = c(15.17, 162, 0.32, 300, 1126.4), 
                                             Mean = c(15.17, 162, 0.32, 300, 1126.4), 
                                             SD = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))%>% select(any_of(sumstat_struct_names)))
      
    })
    
    
    testthat::test_that("rx_numeric_stats_function", {
      # cmax and tmin
      testthat::expect_identical(com8[1,"Median"] , test_cmax_tmax1[1,"cmax_median"] %>% round (2))
      testthat::expect_identical(com8[2,"Median"] , test_cmax_tmax1[1,"Tmax_median"] %>% round (2))
      testthat::expect_identical(com8[1,"Mean"] , test_cmax_tmax1[1,"cmax_mean"] %>% round (2))
      testthat::expect_identical(com8[2,"Mean"] , test_cmax_tmax1[1,"Tmax_mean"] %>% round (2))
      testthat::expect_identical(com8[1,"SD"] , test_cmax_tmax1[1,"cmax_sd"] %>% round (2))
      testthat::expect_identical(com8[2,"SD"] , test_cmax_tmax1[1,"Tmax_sd"] %>% round (2))
      # cmin and tmin
      testthat::expect_identical(com8[3,"Median"] , test_cmin_tmin1[1,"cmin_median"] %>% round (2))
      testthat::expect_identical(com8[4,"Median"] , test_cmin_tmin1[1,"Tmin_median"] %>% round (2))
      testthat::expect_identical(com8[3,"Mean"] , test_cmin_tmin1[1,"cmin_mean"] %>% round (2))
      testthat::expect_identical(com8[4,"Mean"] , test_cmin_tmin1[1,"Tmin_mean"] %>% round (2))
      testthat::expect_identical(com8[3,"SD"] , test_cmin_tmin1[1,"cmin_sd"] %>% round (2))
      testthat::expect_identical(com8[4,"SD"] , test_cmin_tmin1[1,"Tmin_sd"] %>% round (2))
      # AUC
      testthat::expect_identical(com8[5,"Median"] , test_auc1[1,"auc_median"] %>% round (2))
      testthat::expect_identical(com8[5,"Mean"] , test_auc1[1,"auc_mean"] %>% round (2))
      testthat::expect_identical(com8[5,"SD"] , test_auc1[1,"auc_sd"] %>% round (2))
    })
    
    #########################################################################################|
    ## SETUP 9-------------------------------------------------------------------------------
    # setup 8 with t_end = 400
    #########################################################################################|
    
    inp <- list(
      units = "ug/L",
      tgrid_start = "0",
      tgrid_end = "400",
      select_output_var = "DV"
    )
    
    sim_end <- 400
    
    df <- test_df_numeric_stat (inp)
    com9 <- rx_numeric_stats_function(df, tgrid_end =inp$tgrid_end, tgrid_start= inp$tgrid_start, units = inp$units, sim_end_time = sim_end)
    test_cmax_tmax1 <- test_cmax_tmax(df,inp)     
    test_cmin_tmin1 <- test_cmin_tmin(df,inp)
    test_auc1 <- test_auc (df,inp)
    
    ### rx_numeric_stats_function() ------------------------------------------------------------
    testthat::test_that("rx_numeric_stats_function", {
      testthat::expect_s3_class(com9, "data.frame")
      testthat::expect_named(com9, sumstat_struct_names)
      testthat::expect_identical(com9, 
                                 data.frame (Parameter = c("Cmax", "Tmax", "Clast", "Tlast", "AUC"), 
                                             Units = c("ug/L", "days", "ug/L", "days", "days * ug/L"), 
                                             Regimen = c("REGN1", "REGN1", "REGN1", "REGN1", "REGN1"), 
                                             Interval = c("0 - 400", "0 - 400", "0 - 400", "0 - 400", "0 - 400"), 
                                             Median = c(15.69, 162, 0.08, 400, 1110.28), 
                                             Mean = c(15.69, 162, 0.08, 400, 1110.28), 
                                             SD = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))%>% select(any_of(sumstat_struct_names)))
      
    })
    
    
    testthat::test_that("rx_numeric_stats_function", {
      # cmax and tmin
      testthat::expect_identical(com9[1,"Median"] , test_cmax_tmax1[1,"cmax_median"] %>% round (2))
      testthat::expect_identical(com9[2,"Median"] , test_cmax_tmax1[1,"Tmax_median"] %>% round (2))
      testthat::expect_identical(com9[1,"Mean"] , test_cmax_tmax1[1,"cmax_mean"] %>% round (2))
      testthat::expect_identical(com9[2,"Mean"] , test_cmax_tmax1[1,"Tmax_mean"] %>% round (2))
      testthat::expect_identical(com9[1,"SD"] , test_cmax_tmax1[1,"cmax_sd"] %>% round (2))
      testthat::expect_identical(com9[2,"SD"] , test_cmax_tmax1[1,"Tmax_sd"] %>% round (2))
      # cmin and tmin
      testthat::expect_identical(com9[3,"Median"] , test_cmin_tmin1[1,"cmin_median"] %>% round (2))
      testthat::expect_identical(com9[4,"Median"] , test_cmin_tmin1[1,"Tmin_median"] %>% round (2))
      testthat::expect_identical(com9[3,"Mean"] , test_cmin_tmin1[1,"cmin_mean"] %>% round (2))
      testthat::expect_identical(com9[4,"Mean"] , test_cmin_tmin1[1,"Tmin_mean"] %>% round (2))
      testthat::expect_identical(com9[3,"SD"] , test_cmin_tmin1[1,"cmin_sd"] %>% round (2))
      testthat::expect_identical(com9[4,"SD"] , test_cmin_tmin1[1,"Tmin_sd"] %>% round (2))
      # AUC
      testthat::expect_identical(com9[5,"Median"] , test_auc1[1,"auc_median"] %>% round (2))
      testthat::expect_identical(com9[5,"Mean"] , test_auc1[1,"auc_mean"] %>% round (2))
      testthat::expect_identical(com9[5,"SD"] , test_auc1[1,"auc_sd"] %>% round (2))
    })
    
    #########################################################################################|
    ## SETUP 10-------------------------------------------------------------------------------
    ## setup 2 with NA
    #########################################################################################|
    
    inp <- list(
      units = "ug/L",
      tgrid_start = "0",
      tgrid_end = "48",
      select_output_var = "DV"
    )
    
    df <- test_df_numeric_stat (inp)
    df[3,"YDATA"] <- NA
    com10 <- rx_numeric_stats_function(df, tgrid_end =inp$tgrid_end, tgrid_start= inp$tgrid_start, units = inp$units)
    test_cmax_tmax1 <- test_cmax_tmax(df,inp)     
    test_cmin_tmin1 <- test_cmin_tmin(df,inp)
    test_auc1 <- test_auc (df,inp)
    
    ### rx_numeric_stats_function() ------------------------------------------------------------
    testthat::test_that("rx_numeric_stats_function", {
      testthat::expect_s3_class(com10, "data.frame")
      testthat::expect_named(com10, sumstat_struct_names)
      testthat::expect_identical(com10, 
                                 data.frame (Parameter = c("Cmax", "Tmax", "Clast", "Tlast", "AUC"), 
                                             Units = c("ug/L", "days", "ug/L", "days", "days * ug/L"), 
                                             Regimen = c("REGN1", "REGN1", "REGN1", "REGN1", "REGN1"), 
                                             Interval = c("0 - 48", "0 - 48", "0 - 48", "0 - 48", "0 - 48"), 
                                             Median = c(7.17, 1, 2.58, 48, 107.96), 
                                             Mean = c(7.17, 1, 2.58, 48, 107.96), 
                                             SD = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))%>% select(any_of(sumstat_struct_names)))
      
    })
    
    
    testthat::test_that("rx_numeric_stats_function", {
      # cmax and tmin
      testthat::expect_identical(com10[1,"Median"] , test_cmax_tmax1[1,"cmax_median"] %>% round (2))
      testthat::expect_identical(com10[2,"Median"] , test_cmax_tmax1[1,"Tmax_median"] %>% round (2))
      testthat::expect_identical(com10[1,"Mean"] , test_cmax_tmax1[1,"cmax_mean"] %>% round (2))
      testthat::expect_identical(com10[2,"Mean"] , test_cmax_tmax1[1,"Tmax_mean"] %>% round (2))
      testthat::expect_identical(com10[1,"SD"] , test_cmax_tmax1[1,"cmax_sd"] %>% round (2))
      testthat::expect_identical(com10[2,"SD"] , test_cmax_tmax1[1,"Tmax_sd"] %>% round (2))
      # cmin and tmin
      testthat::expect_identical(com10[3,"Median"] , test_cmin_tmin1[1,"cmin_median"] %>% round (2))
      testthat::expect_identical(com10[4,"Median"] , test_cmin_tmin1[1,"Tmin_median"] %>% round (2))
      testthat::expect_identical(com10[3,"Mean"] , test_cmin_tmin1[1,"cmin_mean"] %>% round (2))
      testthat::expect_identical(com10[4,"Mean"] , test_cmin_tmin1[1,"Tmin_mean"] %>% round (2))
      testthat::expect_identical(com10[3,"SD"] , test_cmin_tmin1[1,"cmin_sd"] %>% round (2))
      testthat::expect_identical(com10[4,"SD"] , test_cmin_tmin1[1,"Tmin_sd"] %>% round (2))
      # AUC
      testthat::expect_identical(com10[5,"Median"] , test_auc1[1,"auc_median"] %>% round (2))
      testthat::expect_identical(com10[5,"Mean"] , test_auc1[1,"auc_mean"] %>% round (2))
      testthat::expect_identical(com10[5,"SD"] , test_auc1[1,"auc_sd"] %>% round (2))
    })
    
    #########################################################################################|
    ## SETUP 11-------------------------------------------------------------------------------
    ## with 2 REGN -- setup 2 and setup 6
    #########################################################################################|
    inp <- list(
      units = "ug/L",
      tgrid_start = "0",
      tgrid_end = "48",
      select_output_var = "DV"
    )
    
    df1 <- test_df_numeric_stat (inp)
    df2 <- test_df_numeric_stat (inp, zero_re = T) %>% mutate (REGLAB = "REGN2")
    
    df_comb <- rbind (df1,df2)
    com11 <- rx_numeric_stats_function(df_comb, tgrid_end =inp$tgrid_end, tgrid_start= inp$tgrid_start, units = inp$units)
    testthat::test_that("rx_numeric_stats_function", {
      expect_identical(com2, com11 %>% filter (Regimen == "REGN1"))
      expect_identical(com6, com11 %>% filter (Regimen == "REGN2") %>% mutate (Regimen = "REGN1"))
    })
    
    #########################################################################################|
    ## COMPARISON
    #########################################################################################|
    
    
    ## case 1 vs case 3 --------------------------------------------------------
    testthat::test_that("rx_numeric_stats_function", {
      
      ## Tlast
      testthat::expect_false(identical(com1[4,"Median"], com3[4,"Median"]))
      testthat::expect_false(identical(com1[4,"Mean"],   com3[4,"Mean"]))
      
      ## Interval
      testthat::expect_false(identical(com1[4,"Interval"], com3[4,"Interval"]))
      
      ## Cmax
      testthat::expect_false(identical(com1[1,"Median"], com3[1,"Median"]))
      testthat::expect_false(identical(com1[1,"Mean"],   com3[1,"Mean"]))
      
      ## Clast
      testthat::expect_false(identical(com1[3,"Median"], com3[3,"Median"]))
      testthat::expect_false(identical(com1[3,"Mean"],   com3[3,"Mean"]))
      
      ## AUC
      testthat::expect_false(identical(com1[5,"Median"], com3[5,"Median"]))
      testthat::expect_false(identical(com1[5,"Mean"],   com3[5,"Mean"]))
    }) 
    

    ## case 2 vs case 4 --------------------------------------------------------
    testthat::test_that("rx_numeric_stats_function", {
      
      ## Tlast
      testthat::expect_true(identical(com2[4,"Median"], com4[4,"Median"]))
      testthat::expect_true(identical(com2[4,"Mean"],   com4[4,"Mean"]))
      
      ## Interval
      testthat::expect_true(identical(com2[4,"Interval"], com4[4,"Interval"]))
      
      ## Cmax
      testthat::expect_false(identical(com2[1,"Median"], com4[1,"Median"]))
      testthat::expect_false(identical(com2[1,"Mean"],   com4[1,"Mean"]))
      
      ## Clast
      testthat::expect_false(identical(com2[3,"Median"], com4[3,"Median"]))
      testthat::expect_false(identical(com2[3,"Mean"],   com4[3,"Mean"]))
      
      ## AUC
      testthat::expect_false(identical(com2[5,"Median"], com4[5,"Median"]))
      testthat::expect_false(identical(com2[5,"Mean"],   com4[5,"Mean"]))
    }) 
    

    ## case 2 vs case 5 --------------------------------------------------------
    testthat::test_that("rx_numeric_stats_function", {
      
      ## Tlast
      testthat::expect_true(identical(com2[4,"Median"], com5[4,"Median"]))
      testthat::expect_true(identical(com2[4,"Mean"],   com5[4,"Mean"]))
      
      ## Interval
      testthat::expect_true(identical(com2[4,"Interval"], com5[4,"Interval"]))
      
      ## Cmax
      testthat::expect_false(identical(com2[1,"Mean"],   com5[1,"Mean"]))
      testthat::expect_false(identical(com2[1,"SD"],     com5[1,"SD"]))
      
      ## Clast
      testthat::expect_false(identical(com2[3,"Mean"],   com5[3,"Mean"]))
      testthat::expect_false(identical(com2[3,"SD"],     com5[3,"SD"]))
      
      ## AUC
      testthat::expect_false(identical(com2[5,"Mean"],   com5[5,"Mean"]))
      testthat::expect_false(identical(com2[5,"SD"],     com5[5,"SD"]))
    }) 
    

    ## case 2 vs case 6 --------------------------------------------------------
    testthat::test_that("rx_numeric_stats_function", {
      
      ## Tlast
      testthat::expect_true(identical(com2[4,"Median"], com6[4,"Median"]))
      testthat::expect_true(identical(com2[4,"Mean"],   com6[4,"Mean"]))
      
      ## Interval
      testthat::expect_true(identical(com2[4,"Interval"], com6[4,"Interval"]))
      
      ## Cmax
      testthat::expect_false(identical(com2[1,"Median"], com6[1,"Median"]))
      testthat::expect_false(identical(com2[1,"Mean"],   com6[1,"Mean"]))
      
      ## Clast
      testthat::expect_false(identical(com2[3,"Median"], com6[3,"Median"]))
      testthat::expect_false(identical(com2[3,"Mean"],   com6[3,"Mean"]))
      
      ## AUC
      testthat::expect_false(identical(com2[5,"Median"], com6[5,"Median"]))
      testthat::expect_false(identical(com2[5,"Mean"],   com6[5,"Mean"]))
    }) 
    

    ## case 2 vs case 7 --------------------------------------------------------
    testthat::test_that("rx_numeric_stats_function", {
      
      ## Tlast
      testthat::expect_true(identical(com2[4,"Median"], com7[4,"Median"]))
      testthat::expect_true(identical(com2[4,"Mean"],   com7[4,"Mean"]))
      
      ## Interval
      testthat::expect_false(identical(com2[4,"Interval"], com7[4,"Interval"]))
      
      ## Cmax
      testthat::expect_false(identical(com2[1,"Median"], com7[1,"Median"]))
      testthat::expect_false(identical(com2[1,"Mean"],   com7[1,"Mean"]))
      
      ## Clast
      testthat::expect_false(identical(com2[3,"Median"], com7[3,"Median"]))
      testthat::expect_false(identical(com2[3,"Mean"],   com7[3,"Mean"]))
      
      ## AUC
      testthat::expect_false(identical(com2[5,"Median"], com7[5,"Median"]))
      testthat::expect_false(identical(com2[5,"Mean"],   com7[5,"Mean"]))
      
    }) 
    

    ## case 8 vs case 9 --------------------------------------------------------
    testthat::test_that("rx_numeric_stats_function", {
      
      ## Interval
      testthat::expect_false(identical(com8[4,"Interval"], com9[4,"Interval"]))
      
      ## Cmax
      testthat::expect_false(identical(com8[1,"Median"], com9[1,"Median"]))
      testthat::expect_false(identical(com8[1,"Mean"],   com9[1,"Mean"]))
      
      ## Tmax
      testthat::expect_true(identical(com8[2,"Median"], com9[2,"Median"]))
      testthat::expect_true(identical(com8[2,"Mean"],   com9[2,"Mean"]))
      
      ## Clast
      testthat::expect_false(identical(com8[3,"Median"], com9[3,"Median"]))
      testthat::expect_false(identical(com8[3,"Mean"],   com9[3,"Mean"]))
      
      ## Tlast
      testthat::expect_false(identical(com8[4,"Median"], com9[4,"Median"]))
      testthat::expect_false(identical(com8[4,"Mean"],   com9[4,"Mean"]))
      
      ## AUC
      testthat::expect_false(identical(com8[5,"Median"], com9[5,"Median"]))
      testthat::expect_false(identical(com8[5,"Mean"],   com9[5,"Mean"]))
    })

  })

