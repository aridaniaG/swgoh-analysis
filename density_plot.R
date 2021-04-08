source("ISU_palette.R")

density_plot = function(samples, name, lessthanzero = TRUE, 
                        color = ISU_primary_palette[1]) {
  q = quantile(samples, c(.025,.975))
  
  g = ggplot(data.frame(x = samples), aes(x = x)) +
    geom_density() 
  
  d = ggplot_build(g)$data[[1]] %>% filter(x < 0) %>% select(x,y)
  
  if (lessthanzero) {
    g = g + 
      geom_area(data = d, aes(x=x, y=y), fill = "#C8102E", color = NA) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_text(aes(x = Inf, y = Inf, hjust = 1.2, vjust = 1.2,
                    label = paste0("P < 0 = ", 
                                   formatC(mean(samples < 0), digits = 2, format="f"))))
    
    xlabel = paste(name, "coefficient")
  } else {
    xlabel = paste(name, "standard deviation")
  }
  
  g + geom_density(size = 2) +
    geom_linerange(y = -ggplot_build(g)$layout$panel_params[[1]]$y.range[2]/50, 
                   xmin = q[1], xmax = q[2],
                   color = "#F1BE48", size = 2) +
    labs(title = name,
         x = xlabel,
         y = "Probability density function") 
}
