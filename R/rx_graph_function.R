# Function to create the PK profiles in ggplot
rx_graph_function <- function(df,inp, mod) {
  
  print_function_calls(fn_name = "rx_graph_function", fn_type = "function")
  
  # If no data is simulated, do not create anything
  if (is.null(df)) return()
  
  # Check if user data is provided
  userdata <- NULL
  if(!is.null(inp$user_data_upload)) {
    userdata <- read.csv(inp$user_data_upload$datapath,
                         header = T,
                         sep = inp$sep)
    # Set header to capitols
    colnames(userdata) <- toupper(colnames(userdata))
    if("REGLAB" %in% colnames(userdata)) {
      userdata$REGLAB <- paste(userdata$REGLAB,dose_units(inp$units))  # Add units to dose column
    }
  }
  
  
  # Weeks/Days Plot Label #####
  plotlabel <- "Time after dose (days)"
  intervallabel <- seq(min(df$TIME, na.rm = TRUE), max(df$TIME, na.rm = TRUE), as.numeric(inp$tgrid_delta))
  intervalbreak <- intervallabel
  
  
  yaxis_label<-inp$select_output_var
  gimmetable<-extract_model_annotations_data(mod)

  
  # Annotate DV
  DV.annot<-gimmetable[gimmetable$name == yaxis_label,]
  
  
  split_str_by_index <- function(target, index) {
    index <- sort(index)
    substr(rep(target, length(index) + 1),
           start = c(1, index),
           stop = c(index -1, nchar(target)))
  }
  
  interleave <- function(v1,v2)
  {
    ord1 <- 2*(1:length(v1))-1
    ord2 <- 2*(1:length(v2))
    c(v1,v2)[order(c(ord1,ord2))]
  }
  
  insert_str <- function(target, insert, index) {
    insert <- insert[order(index)]
    index <- sort(index)
    paste(interleave(split_str_by_index(target, index), insert), collapse="")
  }
  
  unit<-insert_str(inp$units, c("(", ")"), c(0,100)) # TODO:if unit is null, omit parenth.
  
  name<-DV.annot$name 
  descr<-DV.annot$descr 
  
  # Statements for handling prediction interval pasting to description of plot
  show_pred<-if (inp$variability == "No variability"){
    paste("No")
  } else {
   inp$variability
}

  

  # Create in-app plots
  p1lin <-ggplot(df) +
    geom_line(size=1, aes(x=TIME,y=Median_C,color=REGLAB)) +    
    guides(color=guide_legend(title="Dependent Variable ")) + 
    scale_color_viridis_d(begin = 0, end = 1, option = 'viridis', aesthetics = c("colour", "fill")) +
    geom_ribbon(aes(ymin=Low_percentile, ymax=High_percentile, x=TIME, fill=REGLAB), alpha = 0.15, linetype=0)+
    guides(fill=guide_legend(title="Dependent Variable ")) + 
    
    # Set axis and theme
    ylab(paste(name, unit)) +
    xlab(plotlabel)+
    # Set theme details
    theme_bw()+
    theme(
      legend.position="top",
          panel.grid.minor =  element_blank(), text=element_text(size=inp$slider1_text))+ # Remove minor grid lines because of log y-axis
    labs(subtitle = paste("Description: ", descr, ",", show_pred, "Prediction Interval"))
  

  
  # Include user data if available
  if(!is.null(userdata) & 'ID' %in% colnames(userdata) & 'TIME' %in% colnames(userdata) & inp$plot_user_dv %in% colnames(userdata)){
    if("REGLAB" %in% colnames(userdata)) {
      p1lin <- p1lin + 
        geom_point(data=userdata,aes(x=TIME,y=get(inp$plot_user_dv), color=paste(inp$plot_user_dv, '(User data)')), alpha=0.4, size=2) + labs(color = "Dependent Variable ")
    } 
  }
  
  
  # user facet selection
  if(!is.null(inp$user_data_upload)) {
    p1lin <- p1lin + facet_wrap(vars(!!sym(inp$plot_user_facet)), nrow=2, labeller = "label_both") + labs(color = paste0("Facet by ", inp$plot_user_facet, ":  ")) +
      theme(
        strip.background = element_rect(
          color="black", fill="#E5E7E9", size=1
        )
      )
  }
  
  p1lin<- p1lin + theme(axis.text.x = element_text(color="#000000", size=12),
                        axis.text.y = element_text(color="#000000", size=12),
                        axis.title.x = element_text(color="#000000", size=14),
                        axis.title.y = element_text(color="#000000", size=14),
                        plot.tag = element_text(size=14,
                                                color="black",
                                                face="bold")) + labs(tag = 'lin')
  
  
  # Same figure on log scale
  p1log <- p1lin + 
    scale_y_log10()
  
  p1log<- p1log + theme(axis.text.x = element_text(color="#000000", size=12),
                        axis.text.y = element_text(color="#000000", size=12),
                        axis.title.x = element_text(color="#000000", size=14),
                        axis.title.y = element_text(color="#000000", size=14),
                        plot.tag = element_text(size=14,
                                                color="black",
                                                face="bold")) + labs(tag = 'log')
  log_plot<-p1log
  linear_plot<-p1lin
  
  
  
  ## Combine in 1 plot with different widths to include the legend
  plot_combine <- ggpubr::ggarrange(linear_plot, log_plot, # list of plots
                                    common.legend = T, # COMMON LEGEND
                                    legend = "top", # legend position
                                    align = "hv", # Align them both, horizontal and vertical
                                    nrow = 3)  # number of rows
  
  ## Combine in 1 plot with different widths to include the legend
  linear_plot <- ggpubr::ggarrange(linear_plot, # list of plots
                                   common.legend = T, # COMMON LEGEND
                                   legend = "top", # legend position
                                   align = "hv", # Align them both, horizontal and vertical
                                   nrow = 3)  # number of rows
  
  ## Combine in 1 plot with different widths to include the legend
  log_plot <- ggpubr::ggarrange(log_plot, # list of plots
                                common.legend = T, # COMMON LEGEND
                                legend = "top", # legend position
                                align = "hv", # Align them both, horizontal and vertical
                                nrow = 3)  # number of rows
  
  
  # Report plots
  report_log_plot<-p1log
  report_linear_plot<-p1lin
  
  report_log_plot <- report_log_plot + theme(axis.text.x = element_text(color="#000000", size=16),
                                             axis.text.y = element_text(color="#000000", size=16),
                                             axis.title.x = element_text(color="#000000", size=22),
                                             axis.title.y = element_text(color="#000000", size=22),
                                             plot.subtitle = element_text(size = 24),
                                             legend.title = element_text(size=24),
                                             legend.text = element_text(size=24))
  
  
  report_linear_plot <- report_linear_plot + theme(axis.text.x = element_text(color="#000000", size=16),
                                             axis.text.y = element_text(color="#000000", size=16),
                                             axis.title.x = element_text(color="#000000", size=22),
                                             axis.title.y = element_text(color="#000000", size=22),
                                             plot.subtitle = element_text(size = 24),
                                             legend.title = element_text(size=24),
                                             legend.text = element_text(size=24))
  
  report_log_plot <- ggpubr::ggarrange(report_log_plot, # list of plots
                                    common.legend = T, # COMMON LEGEND
                                    legend = "top", # legend position
                                    align = "hv", # Align them both, horizontal and vertical
                                    nrow = 3)  # number of rows
  
  report_linear_plot <- ggpubr::ggarrange(report_linear_plot, # list of plots
                                       common.legend = T, # COMMON LEGEND
                                       legend = "top", # legend position
                                       align = "hv", # Align them both, horizontal and vertical
                                       nrow =3)  # number of rows

  
  # Combine plots
  result <- list (comb = plot_combine,
                  lin = linear_plot,
                  log = log_plot,
                  report_lin = report_linear_plot,
                  report_log = report_log_plot,
                  report_testing_lin = p1lin)
  return(result)
  
}