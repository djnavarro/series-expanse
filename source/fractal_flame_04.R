library(Rcpp)
library(cairocore)
library(cairobasic)

sourceCpp(here::here("source", "ff_c.cpp"))

# parameters
seed <- 9
layers <- 3
scheme <- 7

# fixed
iter <- 10000000


if(scheme == 1) {
  bg <- "white"
  pl <- "scico::grayC"
}
if(scheme == 2) {
  bg <- "hotpink4"
  pl <- "ggthemes::Gold-Purple Diverging"
}
if(scheme == 3) {
  bg <- "ghostwhite"
  pl <- "scico::oslo"
}
if(scheme == 4) {
  bg <- "bisque"
  pl <- "scico::bilbao"
}
if(scheme == 5) {
  bg <- "slateblue4"
  pl <- "grDevices::TealRose"
}
if(scheme == 6) {
  bg <- "black"
  pl <- "viridis::viridis"
}
if(scheme == 7) {
  bg <- "azure"
  pl <- "viridis::magma"
}


set.seed(seed)

cat("generating...\n")

df <- flame(iter, layers)
df <- as.data.frame(df)
names(df) <- c("x","y","c")
df <- df[-(1:100),]


# Manually scale the co-ordinates to the image size
df[,1] <- (df[,1] - min(df[,1])) / (max(df[,1]) - min(df[,1])) * 1600
df[,2] <- (df[,2] - min(df[,2])) / (max(df[,2]) - min(df[,2])) * 1600


# Manually create a vector of colours
col_idx <- as.integer((df[,3] - min(df[,3])) / (max(df[,3]) - min(df[,3])) * 255) + 1L
pal <- paletteer::paletteer_c(palette = pl, n = 256)
pal <- gsub("FF$","30", pal)
col <- pal[col_idx]

fname <- paste0("ff_04_", seed, "_", layers, "_", scheme, ".png")
fpath <- here::here("image", fname)

cat("rendering...\n")


cb <- cairobasic::CairoBasic$new(width = 1600, height = 1600, bg = bg, antialias = TRUE)
cb$add_circles(x=df[,1], y = df[,2], r = 1, fill = col, colour = NA)
cb$write_png(fpath)

