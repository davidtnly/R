library(tidyverse)
library(MASS)
library(gridExtra)
library(ggthemes)

## ggplot theme Reference:  http://ggplot2.tidyverse.org/reference/theme.html


pt(2.201, df = 429, lower.tail = FALSE) * 2 


# Graphing
ggplot(mtcars, aes(x = `car name`, y = mpg_z, label = mpg_z)) + 
  geom_bar(stat = 'identity', aes(fill = mpg_type), width = .5)  +
  geom_point() +
  scale_fill_manual(name = "Mileage", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above" = "#00ba38", "below" = "#f8766d")) + 
  labs(subtitle = "Normalised mileage from 'mtcars'", 
       title = "Diverging Bars") + 
  coord_flip() +
  theme(
    legend.position = "bottom",
    legend.key.size = unit(0.2, "cm"),
    legend.background = element_rect(colour = "black", fill = "lightblue"),
    plot.title = element_text(hjust = 0.5, face = "bold", lineheight = 0.5),
    plot.subtitle =  element_text(hjust = 0.5, size = 8, face = "italic", lineheight =  0.5),
    plot.background = element_rect("lightblue"),
    axis.title = element_text(colour = "black", face = "bold"),
    axis.text = element_text(colour = "black"),
    axis.line = element_line(size = 3, colour = "grey80"), 
    panel.grid.major = element_line(colour = "black"),
    panel.grid.minor = element_line(colour = "black")   
    
  )

# “Probability deals with predicting the likelihood of future events, 
# while statistics involves the analysis of the frequency of past events.”

