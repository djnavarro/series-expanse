
seed <- 9
set.seed(seed)

n <- 500000
df <- data.frame(
  x = numeric(n),
  y = numeric(n),
  c = numeric(n)
)

df$x[1] <- runif(1, min = -1, max = 1)
df$y[1] <- runif(1, min = -1, max = 1)
df$c[1] <- runif(1, min = -1, max = 1)

p <- c(df$x[1], df$y[1], df$c[1])

transform <- function(p, b) {
  m <- c(
    b[1] * p[1] + b[2] * p[2] + b[3],
    b[4] * p[1] + b[5] * p[2] + b[6],
    b[7] * p[1] + b[8] * p[2] + b[9]
  )
  return(m)
}

variant <- function(p) {
  1/sum(p^2) * p
}

B <- list(
  runif(9, min = -1, max = 1),
  runif(9, min = -1, max = 1),
  runif(9, min = -1, max = 1),
  runif(9, min = -1, max = 1)
)

for(t in 2:n) {
  
  b <- sample(B, 1)[[1]]
  p <- transform(p, b)
  p <- variant(p)
    
  # store result
  df$x[t] <- p[1]
  df$y[t] <- p[2]
  df$c[t] <- (df$c[t-1] + p[3])/2 
}

library(ggplot2)

df <- df[-(1:100),]

bg <- "hotpink4"
p <- ggplot(df, aes(x,y, colour = c)) + 
  geom_point(size = 1.5, alpha = 1, stroke = 0, show.legend = FALSE) + 
  theme_void() + 
  theme(plot.background = element_rect(bg, bg)) + 
  paletteer::scale_color_paletteer_c("grDevices::PuOr")

fname <- paste0("~/Desktop/ff_01_", seed, ".png")
ggsave(fname, p, width = 16, height = 16, dpi = 100)

