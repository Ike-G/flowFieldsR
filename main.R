# df <- transmute(
#   seq(from = -pi, to = pi, by = 0.01) %>% expand.grid(x_i = ., y_i = .),
#   x = runif(1, -1, 10) * x_i^2 - sin(y-i^2),
#   y = runif(1, -1, 10) * y_i^3 - cos(x_i^2) * y_i^4
# )
# 
# plot <- df %>%
#   ggplot(aes(x=x, y=y)) +
#   geom_point(alpha = 0.1, size = 0, shape = 20, color = "#c1a06e") +
#   theme_void() +
#   coord_fixed() +
#   coord_polar() +
#   theme(
#     panel.background = element_rect(fill = "#1a3657"),
#     plot.background = element_rect(fill = "#1a3657")
#   )

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
ppc <- 3 
width <- 1.5 
height <- 1.5 
margin <- -0.2
seed <- 127
stepLength <- 0.01
numSteps <- 100

sprintf("Lines: %f", ppc*(width/sqrt(3) +height)/pr)

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
                    x=x+stepLength*cospi(2*gen_perlin(x,y,seed=seed)), 
                    y=y+stepLength*sinpi(2*gen_perlin(x,y,seed=seed)), 
                    step=step+1)
  }
  print("Generated field")
  o
}


seq(0, 2*pi, 0.01) %>%
  data.frame(x = ., y = tri((. + triangleRotation)%%(2*pi))) %>%
  ggplot(aes(x = x, y = y)) +
  coord_polar("x") +
  xlim(0, 2*pi) + ylim(0, 1) +
  geom_area()
  

# Main --------------------------------------------------------------------

# grid <- seq(1,10,length.out=50) %>% 
#   long_grid(.,.) %>% 
#   mutate(noise = gen_perlin(x, y, seed = seed))
# 
# grid$angle <- curl_noise(gen_perlin, seed = seed, x = grid$x, y = grid$y) %>%
#   { atan2(.$y, .$x) - atan2(grid$y, grid$x) }
# 
# grid %>%
#   ggplot() +
#   geom_regon(
#     aes(x0 = x, y0 = y, r = noise/3, angle = angle, sides = 3, fill = noise), 
#     alpha = 0.75
#   ) +
#   scale_fill_gradientn(
#     colors = c("#ffc1c1", "#c6b6e0", "#c6b6e0", "#326983"), 
#     guide = "none"
#   ) +
#   coord_fixed()

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
  ggplot(aes(x=x, y=y, group=line)) +
  geom_path(lineend = "round") +
  coord_fixed() +
  xlim(margin, width+margin) +
  ylim(margin, height+margin) +
  theme_void()
print("Finished plot")

ggsave("test.png")