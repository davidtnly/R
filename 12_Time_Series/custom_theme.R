theme_custom <- function(base_size = 12, base_family="") {
  theme(
    axis.line =         element_line(colour = "black", size=0.25),
    axis.text.x =       element_text(size = base_size * 0.6 , lineheight = 0.9, colour = "black", vjust = 1),
    axis.text.y =       element_text(size = base_size * 0.8, lineheight = 0.9, colour = "black", hjust = 1),
    axis.ticks =        element_line(colour = "black", size = 0.25),
    axis.title.x =      element_text(size = base_size, vjust = 0.5),
    axis.title.y =      element_text(size = base_size, angle = 90, vjust = 0.5),
    axis.ticks.length = unit(0.15, "cm"),
    axis.ticks.margin = unit(0.1, "cm"),
    
    legend.background = element_blank(), 
    legend.key =        element_blank(),
    legend.key.size =   unit(1.2, "lines"),
    legend.text =       element_text(size = base_size * 0.8),
    legend.title =      element_text(size = base_size * 0.8, face = "bold", hjust = 0),
    legend.position =   "right",
    
    panel.background =  element_rect(fill = NA, colour = NA, size = 0.25), 
    panel.border =      element_blank(),
    panel.grid.major =  element_blank(),
    panel.grid.minor =  element_blank(),
    panel.margin =      unit(0.25, "lines"),
    
    strip.background =  element_rect(fill = NA, colour = NA), 
    strip.text.x =      element_text(family = "Helvetica", colour = "black", size = base_size),
    strip.text.y =      element_text(family = "Helvetica", colour = "black", size = base_size, angle = -90),
    
    plot.background =   element_rect(colour = NA, fill = "white"),
    plot.title =        element_text(size = base_size * 1.2),
    plot.margin =       unit(c(1, 1, 0.5, 0.5), "lines")
  )
}

theme_ts <- function(base_size = 12, base_family="") {
  theme(
      panel.border = element_rect(fill = NA, colour = "grey10"),
      panel.background = element_blank(),
      panel.grid.minor = element_line(colour = "grey85"),
      panel.grid.major = element_line(colour = "grey85"),
      panel.grid.major.x = element_line(colour = "grey85"),
        
      axis.text = element_text(size = 13, face = "bold"),
      axis.title = element_text(size = 15, face = "bold"),
        
      plot.title = element_text(size = 16, face = "bold"),
        
      strip.text = element_text(size = 16, face = "bold"),
      strip.background = element_rect(colour = "black"),
        
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 16, face = "bold"),
      legend.background = element_rect(fill = "white"),
      legend.key = element_rect(fill = "white")
      )
}

