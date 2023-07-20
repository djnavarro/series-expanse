library(Rcpp)
library(ggplot2)

sourceCpp(here::here("source", "ff_a.cpp"))

# parameters
seed <- 11
iter <- 1000000
bg <- "slateblue4"
pl <- "grDevices::TealRose"

set.seed(seed)

df <- flame(iter)
df <- as.data.frame(df)
names(df) <- c("x","y","c")
df <- df[-(1:100),]

p <- ggplot(df, aes(x,y, colour = c)) + 
  geom_point(size = 1.5, alpha = 1, stroke = 0, show.legend = FALSE) + 
  theme_void() + 
  theme(plot.background = element_rect(bg, bg)) + 
  paletteer::scale_color_paletteer_c(pl)

fname <- paste0("ff_02_", seed, "b.png")
fpath <- here::here("image", fname)
ggsave(fpath, p, width = 16, height = 16, dpi = 300)


