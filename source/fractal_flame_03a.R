library(Rcpp)
library(cairocore)
library(cairobasic)

sourceCpp(here::here("source", "ff_b.cpp"))

# parameters
seed <- 17
layers <- 8

# fixed
iter <- 10000000
bg <- "white"
pl <- "scico::grayC"

set.seed(seed)

cat("generating...\n")

df <- flame(iter, layers)
df <- as.data.frame(df)
names(df) <- c("x","y","c")
df <- df[-(1:100),]


# Manually scale the co-ordinates to the image size
px <- 5000
df[,1] <- (df[,1] - min(df[,1])) / (max(df[,1]) - min(df[,1])) * px
df[,2] <- (df[,2] - min(df[,2])) / (max(df[,2]) - min(df[,2])) * px


# Manually create a vector of colours
col_idx <- as.integer((df[,3] - min(df[,3])) / (max(df[,3]) - min(df[,3])) * 255) + 1L
pal <- paletteer::paletteer_c(palette = pl, n = 256)
pal <- gsub("FF$","30", pal)
col <- pal[col_idx]

fname <- paste0("ff_03a_", seed, "_", layers, ".png")
fpath <- here::here("image", fname)

cat("rendering...\n")


cb <- cairobasic::CairoBasic$new(width = px, height = px, bg = bg, antialias = TRUE)
cb$add_circles(x=df[,1], y = df[,2], r = 3, fill = col, colour = NA)
cb$write_png(fpath)

