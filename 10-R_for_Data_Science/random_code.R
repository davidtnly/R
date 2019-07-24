## setwd('C:/Users/User/Documents/R/win-library/3.4')

####### Basic R Review Chapter 3

library(tidyverse)

p1 <- ggplot(data = mpg) + 
  ## Mapping argument: Defines how variables in your ds are mapped to visual properties
  geom_point(mapping = aes(x = displ, y = hwy))

## This looks the same w/o the mapping argument
p2 <- ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point()

####### Aesthetic mappings

p3 <- ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

p4 <- ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

p5 <- ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

p6 <- ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

## Facets

p7 <- ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

p8 <- ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

p9 <- ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(. ~ cyl)

## Creating smooth lines (LOESS)

p10 <- ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

## Split LOESS line by drv values (3 total)
p11 <- ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

p12 <- ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ,y = hwy, linetype = drv, color = drv)
              , show.legend = FALSE)

p13 <- ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(mapping = aes(x = displ, y = hwy)) +
  theme_bw()

## p13 = p14
p14 <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth() +
  theme_bw()

p15 <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(aes(color = drv)) + 
  geom_smooth(aes(linetype = drv)) +
  theme_bw()

p16 <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(aes(color = drv)) + 
  geom_smooth() +
  theme_bw()

####### Statistical transformations

p17 <- ggplot(data = diamonds) + 
  stat_count(mapping = aes(x = cut))

## Change stat count to proportion values 

p18 <- ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 10))

demo <- tribble(
  ~cut,         ~freq,
  "Fair",       1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551
)

## You can generally use geoms and stats interchangeably

p19 <- ggplot(data = demo) +
  geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")

## Review ggplot2 cheatsheet for more stat f(x)

p20 <- ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )
p20
####### Position adjustments

p21 <- ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))

p22 <- ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))

## Pos Adjustments: identity, dodge, fill

p23 <- ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity")

p24 <- ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")

p25 <- ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "stack")

p26 <- ggplot(data = diamonds, aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "stack")

p27 <- ggplot(data = diamonds) + 
  geom_bar(aes(x = cut, fill = clarity), position = "dodge")

p28 <- ggplot(data = diamonds, aes(x = cut, fill = clarity)) + 
  geom_bar(position = "dodge")

## Adding randomness seems like a strange way to improve your plot
## position = "jitter" to scatterplot especially if it's clustered

p29 <- ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")

## No jitter
p30 <- ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

####### Coordinate Systems
## Cartesian coordinate system where x & y positions act independently
## coord_flip() switches x and y axes

p31 <- ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

p32 <- ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot() +
  coord_flip()

## coord_quickmap() sets aspect ratio correctly for maps

nz <- map_data("nz")

p33 <- ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

p34 <- ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()

## coord_polar() uses polar coordinates; connection between bar & Coxcomb

bar <- ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, fill = cut),
    show.legend = FALSE,
    width = 1
  ) +
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

p35 <- bar + coord_flip()
p36 <- bar + coord_polar()


## 3.10 Layered Grammar of Graphics
# ggplot(data = <DATA>) + 
#   <GEOM_FUNCTION>(
#     mapping = aes(<MAPPINGS>),
#     stat = <STAT>, 
#     position = <POSITION>
#   ) +
#   <COORDINATE_FUNCTION> +
#   <FACET_FUNCTION>

####### Chapter 4 Workflow: Basics

p37 <- ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

filter(mpg, cyl == 8)
filter(diamonds, carat > 3)


####### End


























