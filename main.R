# Libraries ---------------------------------------------------------------

library(ggplot2)
library(ggforce)
library(ambient)
library(dplyr)

# Parameters --------------------------------------------------------------

triangleRotation <- pi/6
# circle packing radius
pr <- 0.05
# points per circle
ppc <- 5 
width <- 1.5 
height <- 1.5 
margin <- -0.2
seed <- 127
stepLength <- 0.001
numSteps <- 200

# Helpers -----------------------------------------------------------------


tri <- function (theta) 1/ifelse(between(theta, 0, 2*pi/3), 
                                 cos(theta)+sqrt(3)*sin(theta),
                                 ifelse(theta <= 4*pi/3,
                                        -2*cos(theta),
                                        cos(theta)-sqrt(3)*sin(theta))) 

lappend <- function (lst, ...) c(lst, list(...))

genField <- function(points, seed = 1, stepLength = 0.01, numSteps = 20) {
  o <- tibble() 
  state <- transmute(points, x = px, y = py, step = 1, line = 1:nrow(points))
  for (i in 1:numSteps) {
    o <- bind_rows(o, state)
    state <- mutate(state, 
                    g=gen_perlin(x,y,seed=seed),
                    x=x+stepLength*cospi(2*g), 
                    y=y+stepLength*sinpi(2*g), 
                    step=step+1)
  }
  print("Generated field")
  o
}

theme_custom <- function(bg, ...) theme(
  line = element_blank(), rect = element_blank(),
  text = element_blank(), title = element_blank(),
  axis.text = element_blank(), axis.title = element_blank(),
  axis.ticks.length = unit(0, "pt"), axis.ticks.length.x = NULL,
  axis.ticks.length.x.top = NULL, axis.ticks.length.x.bottom = NULL,
  axis.ticks.length.y = NULL, axis.ticks.length.y.left = NULL,
  axis.ticks.length.y.right = NULL, legend.box = NULL,
  legend.key.size = unit(0,"pt"), legend.position = "none",
  legend.text = element_blank(), legend.title = element_blank(),
  strip.text = element_blank(), strip.switch.pad.grid = unit(0,"pt"),
  strip.switch.pad.wrap = unit(0,"pt"), panel.ontop = FALSE,
  panel.spacing = unit(5.5, "pt"), plot.margin = unit(c(0,0,0,0), "lines"),
  plot.title = element_blank(), plot.subtitle = element_blank(),
  plot.caption = element_blank(), plot.tag = element_blank(),
  complete = TRUE, ...
)

# Main --------------------------------------------------------------------

points <- data.frame(merge(
    long_grid(seq(0,width,2*sqrt(3)*pr), seq(0,height,2*pr)),
    long_grid(seq(sqrt(3)*pr,width+sqrt(3)*pr,2*sqrt(3)*pr), seq(-pr,height+pr,2*pr)),
    all = TRUE
  )) %>%
  group_by(x, y) %>% 
  summarise(
    px = runif(ppc,0,2*pi), 
    z = runif(ppc)+runif(ppc),
    py = pr*ifelse(z > 1, 2-z, z),
    .groups = "drop"
  ) %>%
  transmute(px = x+cos(px)*py, py = y+sin(px)*py)

img <- genField(points, seed = seed, stepLength = stepLength, numSteps = numSteps) %>% 
  ggplot(aes(x=x, y=y, group=line, colour=as.factor(1))) +
  geom_path(lineend = "round") +
  scale_colour_manual(values=c("white")) +
  coord_fixed() +
  coord_cartesian(xlim = c(-margin,width+margin), ylim = c(-margin, height+margin)) +
  (theme_void() %+replace% theme(panel.background = element_rect(fill="black"), legend.position = "none"))
print("Finished plot")

ggsave("test.png")