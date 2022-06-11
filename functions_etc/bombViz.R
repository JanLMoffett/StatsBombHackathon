
#blank soccer pitch!!!

ggplot() +
  annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "black", size = 0.6)+
  annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
  annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)

plot_pitch <- function(data = NULL, lineColor = "black"){
  
  ggp <- ggplot(data = data) +
    annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = lineColor, size = 0.6) +
    annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = lineColor, size = 0.6) +
    annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = lineColor, size = 0.6) +
    annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = lineColor, size = 0.6) +
    annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = lineColor, size = 0.6) +
    annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = lineColor, size = 0.6) +
    annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = lineColor, size = 0.6) +
    annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = lineColor, size = 0.6) +
    annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = lineColor, size = 0.6)+
    annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = lineColor, size = 0.6)+
    annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = lineColor, size = 0.6)
  
  return(ggp)
}




#i want to make a theme 
bombTurf <- theme(
  
  line = element_line(color = jmbn["mint"], linetype = "solid", lineend = "square", size = 0.5),
  rect = element_rect(fill = jmbn["turf"]),
  text = element_text(family = "sans", color = jmbn["blush"]),
  title = element_text(family = "sans", color = jmbn["blush"]),
  
  axis.title = element_blank(),#element_text(face = "bold", color = jmbn["blush"], size = 16, margin = margin(2,2,2,2,"pt")),
  axis.text = element_blank(), #element_text(color = jmbn["mint"], size = 11),
  axis.ticks = element_blank(), #element_line(size = 0.5, lineend = "square", color = jmbn["mint"]),
  axis.ticks.length = unit(2, "mm"),
  axis.line = element_line(color = jmbn["mint"], size = 0.5),
  
  legend.background = element_rect(fill = jmbn["hunter"], linetype = "blank"),
  legend.margin = margin(10,10,10,10, unit = "pt"),
  legend.key = element_rect(fill = jmbn["hunter"], linetype = "blank"),
  legend.text = element_text(color = jmbn["mint"], size = 11),
  legend.text.align = 0.5,
  legend.title = element_text(color = jmbn["blush"], face = "bold", size = 16),
  legend.title.align = 0.5,
  legend.position = "right",
  legend.direction = "vertical",
  legend.justification = "top",
  legend.box = "vertical",
  legend.box.just = "center",
  legend.box.margin = margin(6,6,6,6),
  legend.box.background = element_rect(fill = jmbn["turf"], color = jmbn["hunter"], size = 1),
  
  panel.background = element_rect(fill = jmbn["hunter"]),
  panel.border = element_rect(fill = NA, color = jmbn["turf"], size = 0.5),
  panel.spacing = unit(10, "pt"),
  panel.grid.major = element_line(color = jmbn["hunter"], size = 0.5),
  panel.grid.minor = element_line(color = jmbn["hunter"], size = 0.5),
  
  plot.background = element_rect(fill = jmbn["turf"]),
  plot.title = element_text(face = "bold", size = 24, color = jmbn["blush"], hjust = 0.5),
  plot.subtitle = element_text(family = "sans", face = "italic", size = 12, color = jmbn["thistle"], hjust = 0.5),
  plot.caption = element_text(family = "sans", color = jmbn["mint"], size = 11),
  plot.margin = margin(15,15,15,15),
  plot.tag = element_text(family = "sans", color = jmbn["mint"], size = 16),
  plot.tag.position = c(0.95,1),
  
  strip.background = element_rect(fill = jmbn["turf"], jmbn["mint"], size = 0.5),
  strip.text = element_text(family = "sans", face = "bold", size = 13, color = jmbn["mint"]),
  
  complete = FALSE,
  validate = TRUE
)


plot_pitch(lineColor = jmbn["mint"]) + 
  bombTurf + 
  labs(x = NULL, y = NULL)

