############################################################
## Calculate the prediction interval based on user input
rx_pi_function <- function(df, inp) {
  
  print_function_calls(fn_name = "rx_pi_function", fn_type = "function")
  
  ## set lower & upper limits for the user-specified intervals
  if (inp$variability == "10%-90%") {
    minprobs = 0.1
    maxprobs = 0.9
  }
  if (inp$variability == "5%-95%") {
    minprobs = 0.05
    maxprobs = 0.95
  }
  if (inp$variability == "25%-75%") {
    minprobs = 0.25
    maxprobs = 0.75
  }
  if (inp$variability == "No variability") {
    minprobs = 0.5 # Set to median
    maxprobs = 0.5 # Set to median
  }
  
  
  ## Calculate summary statistics
  sum_stat <- df %>%
    group_by(TIME, REGLAB, YNAME) %>%
    summarise(Median_C = median(YDATA),
              Low_percentile = quantile(YDATA, probs = minprobs),
              High_percentile = quantile(YDATA, probs = maxprobs)
    )
  
  return(sum_stat)
}
