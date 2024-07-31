library(ggplot2)

# Plot sequential adjustment
adjustment_plot <- function(data, show_chi_squared = TRUE, show_n = FALSE, 
                            xbreaks = c(0.8, 0.9, 1.0, 1.1), 
                            title = "Median daily step count (per 1000-step increase)", 
                            xlabel = "PsA Hazard Ratio", display_plot = TRUE, 
                            save_plot = NULL, y_label_size = 15, marker_size = 5, 
                            scale_marker = NULL, plot_width = 10) {
  required_columns <- c("HR", "Variable", "CI_Low", "CI_High", if (show_chi_squared) "ChiSquared",
                        if (show_n) "N_cases", if (!is.null(scale_marker)) "Power")
  missing_columns <- setdiff(required_columns, colnames(data))
  
  if (length(missing_columns) > 0) {
    stop(paste("Missing columns in the input data: ", paste(missing_columns, collapse = ", ")))
  }
  
  xlim <- c(xbreaks[1], xbreaks[length(xbreaks)] + 0.3 + 0.2*show_chi_squared + 0.4*show_n)
   
  if (!is.null(scale_marker)) {
    marker_size <- marker_size/2 + (marker_size - marker_size/2) * data[[scale_marker]]
  }
  
  p <- ggplot(data, aes(x = HR, y = Variable)) +
    geom_point(shape = 15, size = marker_size) +
    geom_errorbarh(aes(xmin = CI_Low, xmax = CI_High), height = 0) +
    geom_hline(yintercept = Inf) +
    geom_segment(aes(x = xbreaks[1], y = 0, xend = xbreaks[length(xbreaks)], yend = 0)) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    labs(x = xlabel, y = NULL, title = title) +
    theme(legend.position = "none", axis.text.y = element_text(hjust = 0, size = y_label_size),
          plot.title = element_text(margin = margin(0, 0, 30, 0)),
          plot.caption = element_blank(), panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), panel.background = element_blank(), 
          axis.text = element_text(size = 15), axis.ticks.y = element_blank(), 
          axis.title.x = element_text(hjust = 0.15), plot.title.position = "plot") +
    scale_x_continuous(breaks = xbreaks, limits = xlim, trans = "log2") +
    annotate(geom = 'text', label = "HR (95% CI)", x = xbreaks[length(xbreaks)] + 0.2, 
             y = Inf, hjust = 0.5, vjust = -1) +
    geom_text(aes(x = xbreaks[length(xbreaks)] + 0.2, 
                  label = paste0(round_2_dp(HR), " (", round_2_dp(CI_Low), ", ", 
                                 round_2_dp(CI_High), ")"), hjust = 0.5)) +
    coord_cartesian(clip = 'off')
  
  if (show_chi_squared) {
    p <- p +
      annotate(geom = 'text', label = "χ²", x = 1.55, y = Inf, hjust = 0.5, vjust = -1) +
      geom_text(aes(x = 1.55, label = paste(format(ChiSquared, digits = 3)), hjust = 0.5))
  }
  if (show_n) {
    p <- p +
      annotate(geom = 'text', label = "Total (Events)", x = 1.65, y = Inf, hjust = 0.5, vjust = -1) +
      geom_text(aes(x = 1.65, label = N_cases, hjust = 0.5))
  }
  
  if (display_plot) {
    print(p)
  }
  
  if (!is.null(save_plot)) {
    ggsave(file = save_plot, plot = p, width = plot_width, height = 3, dpi = 600)
  }
  
  return(p)
}

# Shape plot for quantiles of exposure
shape_plot <- function(data, xlims = c(0, 19000), ybreaks = c(0.4, 0.6, 0.8, 1, 1.2, 1.4), 
                       xlim_scale = 1, ylim_scale = 1, ratio = 1.5, size_val = 14, 
                       line_val = 0.7, textsize = 4, ext = c(0, 0), display_plot = TRUE, 
                       save_plot = NULL) {
  required_columns <- c("mean_steps", "estimate", "stderr", "estlab", "nlab")
  missing_columns <- setdiff(required_columns, colnames(data))
  
  if (length(missing_columns) > 0) {
    stop(paste("Missing columns in the input data: ", paste(missing_columns, collapse = ", ")))
  }
  
  ylims <- c(ybreaks[1] - 0.02, ybreaks[length(ybreaks)] + 0.01)
  
  shapeplot <- ggplot(data, aes(x = `mean_steps`, y = exp(estimate))) +
    geom_point(aes(size = 1/stderr),
               shape = 15,
               colour = "black",
               fill = "black",
               stroke = 0.5) +
    geom_smooth(method = "lm", se = FALSE, formula = 'y ~ x', linetype = "dashed",
                colour = "black") +
    geom_text(aes(y = exp(estimate+1.96*stderr),
                  label = estlab),
              vjust = -0.8,
              size  = textsize,
              colour = "black") +
    geom_text(aes(y = exp(estimate-1.96*stderr),
                  label = nlab),
              vjust = 1.8,
              size  = textsize,
              colour = "black") +
    geom_linerange(aes(ymin = exp(estimate-1.96*stderr),
                       ymax = exp(estimate+1.96*stderr)),
                   colour = "black") +
    scale_radius(guide  = "none",
                 limits = c(0, xlim_scale / min(data$stderr)),
                 range  = c(0, 5)) +
    scale_shape_identity() +
    scale_colour_identity() +
    scale_fill_identity() +
    scale_y_continuous(trans  = "log",
                       breaks = ybreaks * ylim_scale) +
    scale_x_continuous(labels = function(x) {format_thousand(x)}) + 
    xlab("Median Daily Steps") +
    ylab("HR for PD Incidence")
  
 # shapeplot <- ckbplotr::plot_like_ckb(plot = shapeplot, 
  #                                     xlims          = xlims,
   #                                    ylims          = ylims,
    #                                   gap            = c(0.025, 0.025),
     #                                  ext            = ext,
      #                                 ratio          = ratio,
       #                                base_size      = size_val,
        #                               base_line_size = line_val,
         #                              colour         = "black")
  
  if (display_plot) {
    print(shapeplot)
  }
  
  if (!is.null(save_plot)) {
    ggsave(file = save_plot, plot = shapeplot, width = 10, height = 6, dpi = 600)
  }
  
  return(shapeplot)
}

