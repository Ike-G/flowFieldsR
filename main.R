# Libraries ---------------------------------------------------------------

library(grid)
library(ggplot2)
library(ggforce)
library(ambient)
library(particles)
library(dplyr)
library(vctrs)
library(ptinpoly)
library(ggnewscale)
library(jcolors)
library(paletteer)

# Parameters --------------------------------------------------------------

theta <- 0.792 
# circle packing radius
pr <- 0.05
# points per circle
ppc <- 8 
width <- 1.5 
height <- 1.5 
margin <- -0.1 
innerMargin <- 0.1
seed <- 127
stepLength <- 0.01
numSteps <- 100

# Helpers -----------------------------------------------------------------


tri <- function (theta) 1/ifelse(between(theta, 0, 2*pi/3), 
                                 cos(theta)+sqrt(3)*sin(theta),
                                 ifelse(theta <= 4*pi/3,
                                        -2*cos(theta),
                                        cos(theta)-sqrt(3)*sin(theta))) 

lappend <- function (lst, ...) c(lst, list(...))
magnitude <- function (d) sqrt(d$x^2 + d$y^2)

genField <- function(points, seed = 1, stepLength = 0.01, numSteps = 20) {
  o <- tibble() 
  state <- transmute(points, x = px, y = py, step = 1, line = 1:nrow(points))
  for (i in 1:numSteps) {
    o <- bind_rows(o, state)
    state <- mutate(state, 
                    c=gen_perlin(x,y,seed=seed),
                    x=x+stepLength*cospi(2*c), 
                    y=y+stepLength*sinpi(2*c), 
                    step=step+1)
  }
  print("Generated field")
  bind_rows(o, state) %>%
    group_by(step) %>%
    mutate(c=magnitude(state[c("x","y")]-points[c("px","py")]))
}

genLines <- function(centres, length, seed) {
  cbind(centres,line=1:nrow(centres)) %>%
    rbind(tibble(g=gen_perlin(centres$x, centres$y,seed=seed)) %>%
            transmute(x=centres$x+length*cospi(2*g),
                      y=centres$y+length*sinpi(2*g),
                      line=1:nrow(centres)
                      )
          )
}

intersect <- function(polygon, points) {
  p <- as.matrix(points[c("x","y")],nrow=2,ncol=2)
  if (nrow(p) != 2) {
    print(p)
    stop("Incorrect number of rows")
  } else if (ncol(p) != 2) {
    print(p)
    stop("Incorrect number of columns")
  }
  for (i in 1:50) {
    m <- matrix(c(0.5,1,0,0.5,0,1),nrow=3,ncol=2)%*%p
    t <- pip2d(polygon,m)
    if (t[1] == 0) {
      return(m[1,])
    } 
    p[(1:2)[t[2:3] == t[1]],] <- m[1,]
  }
  as_tibble(m)[1,,drop=FALSE]
}

filterPoly <- function(data, polygon, within = TRUE) {
  data <- data[order(data$line),,drop=FALSE]
  numLines <- length(unique(data$line))
  inPoly <- ptinpoly::pip2d(polygon, as.matrix(data[c("x","y")]))
  if (!within) {
    inPoly <- -inPoly
    print("Starting outer")
  }
  inPoly[inPoly == 0] <- 1
  n <- nrow(data)
  diff <- inPoly[-1] != inPoly[-n] & data$line[-1] == data$line[-n]
  keep <- c(diff, FALSE) | c(FALSE, diff) | inPoly >= 0
  
  data <- data[keep,]
  inPoly <- inPoly[keep]
  n <- nrow(data)
  diff <- inPoly[-1] != inPoly[-n] & data$line[-1] == data$line[-n]
  
  # j <- 0
  # k <- 0
  # for (i in (2:n+1)[c(diff, F) & c(F,diff)]) {
  #   data[seq(i+j+2,n+1+j),] <- data[seq(i+j+1,n+j),]
  #   j <- j+1
  #   k <- k+j
  # }
  inPoly[c(diff, F) & c(F, diff)] <- 1
  diff <- inPoly[-1] != inPoly[-n] & data$line[-1] == data$line[-n]
  # For every i in the same group such that i+1 is different, replace it
  # with the intersection between i and i+1
  data[inPoly == -1,c("x","y")] <- purrr::map_dfr(
    (1:(n-1))[diff], ~ intersect(polygon, data[.:(.+1),c("x","y")])
  ) 
  for (i in 1:numLines) {
    s <- data$line == i
    t <- purrr::accumulate(inPoly[s][-length(inPoly[s])],
                                         function(a,b) ifelse(b == -1, a+1, a), 
                                         .init = 0)
    data[s, "line"] <- ifelse(t == 0, i, numLines+t)
    numLines <- numLines+max(t)
  }
  data
}

# Main --------------------------------------------------------------------

centres <- tibble(merge(
    long_grid(seq(0,width,2*sqrt(3)*pr), seq(0,height,2*pr)),
    long_grid(seq(sqrt(3)*pr,width+sqrt(3)*pr,2*sqrt(3)*pr), seq(-pr,height+pr,2*pr)),
    all = TRUE
  ))
points <- group_by(centres, x, y) %>% 
  summarise(
    px = runif(ppc,0,2*pi), 
    z = runif(ppc)+runif(ppc),
    py = pr*ifelse(z > 1, 2-z, z),
    .groups = "drop"
  ) %>%
  transmute(px = x+cos(px)*py, py = y+sin(px)*py)

radius <- (height - 2*innerMargin)/2
polygon <- c(rep(width/2,3),rep(height/2,3))+radius*
  matrix(c(1,-0.5,-0.5,0,sqrt(3)/2,-sqrt(3)/2),nrow=3,ncol=2)%*%
  matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),nrow=2,ncol=2)

lines <- genField(points, seed = seed, stepLength = stepLength, numSteps = numSteps)

img <- ggplot() +
  filterPoly(lines, polygon) %>%
  geom_path(mapping=aes(x=x, y=y, group=line, colour=c),lineend="round") +
  scale_colour_distiller(palette="RdPu") +
  # jcolors::scale_color_jcolors_contin("pal2") +
  new_scale_color() +
  filterPoly(lines, polygon, within = FALSE) %>%
  geom_path(mapping=aes(x=x,y=y,group=line, colour=c),lineend="round") +
#  jcolors::scale_color_jcolors_contin("pal3") +
  paletteer::scale_color_paletteer_c("scico::oslo") +
  #scale_colour_distiller(palette = "Greys") +
  #geom_path(tibble(x=polygon[c(1:3,1:2),1],y=polygon[c(1:3,1:2),2]), mapping=aes(x=x,y=y), size=2, linejoin="mitre", lineend="round") + 
  coord_cartesian(xlim = c(-margin,width+margin), ylim = c(-margin, height+margin)) +
  (theme_void() %+replace% theme(legend.position = "none", panel.background = element_rect(fill = "black")))

print("Finished plot")

ggsave("test.png")
img