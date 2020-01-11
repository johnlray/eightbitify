#' Add together two numbers
#'
#' @param img_path The image you want to work with
#' @return 
#' @examples
#' @export
#' 

library(magick)
library(imager)

pixelate <- function(img_path, resolution){
  img <- image_read('http://jeroen.github.io/images/tiger.svg')
  
  info <- image_info(img)
  
  #the_cimg <- magick2cimg(img)
  #cimg_df <- as.data.frame(the_cimg)
  
  w <- info$width
  h <- info$height
  
  winc <- round(w/256, 0)
  hinc <- round(h/256, 0)
  
  #Rs <- as.hexmode(as.integer(the_cimg[,,1]*255))
  #Gs <- as.hexmode(as.integer(the_cimg[,,2]*255))
  #Bs <- as.hexmode(as.integer(the_cimg[,,3]*255))
  
  # break the image into chunks
  xstart <- seq(1, w, by = winc*10)
  ystart <- seq(1, h, by = hinc*10)
  
  res_mat <- matrix(nrow = length(xstart), ncol = length(ystart))
  x_s <- NULL
  y_s <- NULL
  col_s <- NULL
  # find the average of each chunk
  for(i in 1:length(xstart)){
    for(j in 1:length(ystart)){
      crop_statement <- paste0(winc, "x", hinc, "+", xstart[i], "+", ystart[j])
      temp <- image_crop(img, crop_statement) %>% image_quantize(max = 2) %>% image_median(radius = 5)
      x_s <- c(x_s, i)
      y_s <- c(y_s, j)
      col_s <- c(col_s, (as.integer(temp) %>% as.hexmode() %>% as.character() %>% c() %>% table() %>% sort() %>% head(1) %>% names()))
      #res_mat[i, j] <- (as.integer(temp) %>% as.hexmode() %>% as.character() %>% c() %>% table() %>% sort() %>% head(1) %>% names())
    }
  }
  
  # turn into a plot
  dat <- cbind(x_s, y_s, col_s) %>% data.frame(stringsAsFactors = F)
  dat$x_s <- as.numeric(dat$x_s)
  dat$y_s <- as.numeric(dat$y_s)
  dat$col_s <- paste0("#", substr(dat$col_s, 1, 6))
  
  ggplot(dat, aes(x = x_s, y = y_s, fill = col_s)) +
    geom_tile() +
    scale_fill_manual(values=unique(dat$col_s)) +
    theme_void() +
    theme(legend.position = 'null')
}