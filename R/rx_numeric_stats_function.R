######################################
## Calculate the summary statistics

summ_stat_table_struct <- function() {
  data.frame(Regimen= character (),
             Parameter=character(),
             Units=character(),
             Interval = character(),
             Median=numeric(), 
             Mean=numeric(), 
             SD=numeric(), 
             stringsAsFactors=FALSE)
}

summ_stat_table_rows <- function() {
  c("Cmax", "Tmax", "Clast", "Tlast", "AUC")
}

get_param_index <- function(param) {
  which(summ_stat_table_rows() %in% param)
}

rx_numeric_stats_function <- function(df, tgrid_end, tgrid_start, units, sim_end_time = NULL){
  
  print_function_calls(fn_name = "rx_numeric_stats_function", fn_type = "function")
  
  #sim_end_time <- as.numeric(inp$tgrid_end) 
  
  if (is.null (sim_end_time)){sim_end_time <- tgrid_end}
  
  # filter df based on start and end time 
  df <- df %>% filter (TIME >= as.numeric (tgrid_start),
                       TIME <= as.numeric (sim_end_time))
  
  
  ### Calculate individual data
  ind_sumstat <- df %>%
    group_by(ID,REGLAB, REPI) %>% 
    summarize(Cmax= max(YDATA, na.rm = T), 
              Tmax=TIME[which.max(YDATA)],
              Clast= YDATA[which.max(TIME)], 
              Tlast=max(TIME, na.rm = T),
              AUC = auc(TIME,YDATA)) %>% ungroup ()
  
  ### finding median for each REGLAB and REPI
  repi_sumstat <- ind_sumstat %>%
    group_by(REGLAB, REPI) %>% 
    summarize(Cmax= median(Cmax, na.rm = T),
              Tmax= median(Tmax, na.rm = T),
              Clast= median(Clast, na.rm = T),
              Tlast= median(Tlast, na.rm = T),
              AUC= median(AUC, na.rm = T)) %>% ungroup ()
  
  
  ## Average per REGLAB
  sumstat_data <- repi_sumstat %>% 
    group_by(REGLAB) %>%  
    summarize(Median_Cmax = median(Cmax, na.rm = T) %>% round (digits = 2),
              Mean_Cmax = mean(Cmax, na.rm = T)%>% round (digits = 2),
              SD_Cmax = sd(Cmax, na.rm = T)%>% round (digits = 2),
              Median_Tmax = median(Tmax, na.rm = T)%>% round (digits = 2),
              Mean_Tmax = mean(Tmax, na.rm = T)%>% round (digits = 2),
              SD_Tmax = sd(Tmax, na.rm = T)%>% round (digits = 2),
              Median_Clast = median(Clast, na.rm = T)%>% round (digits = 2),
              Mean_Clast = mean(Clast, na.rm = T)%>% round (digits = 2),
              SD_Clast = sd(Clast, na.rm = T)%>% round (digits = 2),
              Median_Tlast = median(Tlast, na.rm = T)%>% round (digits = 2),
              Mean_Tlast = mean(Tlast, na.rm = T)%>% round (digits = 2),
              SD_Tlast = sd(Tlast, na.rm = T)%>% round (digits = 2),
              Median_AUC = median(AUC, na.rm = T)%>% round (digits = 2),
              Mean_AUC = mean(AUC, na.rm = T)%>% round (digits = 2),
              SD_AUC = sd(AUC, na.rm = T)%>% round (digits = 2)) %>% ungroup ()
  
  # change format
  sumstat <- sumstat_data %>%
    pivot_longer(cols = -REGLAB, names_to = c(".value", "Parameter"), 
                 names_sep = "_", values_drop_na = FALSE)
  
  
  # units 
  sumstat [sumstat$Parameter %in% c ("Tmax", "Tlast"),"Units"] <- "days"
  sumstat [sumstat$Parameter %in% c ("Cmax", "Clast"),"Units"] <- units
  sumstat [sumstat$Parameter == "AUC","Units"] <- paste0 ("days * ",units)
  # Interval
  sumstat [,"Interval"]<- paste0 (tgrid_start, " - ", sim_end_time)
  
  
  # Change Format
  colnames(sumstat)[which(names(sumstat) == "REGLAB")] <- "Regimen" # Change col names
  sumstat <- sumstat %>% select(any_of(names(summ_stat_table_struct()))) # Change col order 
  sumstat <- sumstat %>% # change row order
    arrange(factor (Regimen, levels = unique (sumstat_data$REGLAB)) , 
            factor(Parameter, levels = summ_stat_table_rows())) %>% as.data.frame()
  return (sumstat)
  
}