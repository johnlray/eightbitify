#' Add together two numbers
#'
#' @param img_path The image you want to work with
#' @return 
#' @examples
#' @export
#' 

library(magick)
library(imager)
library(ggplot2)
library(reshape2)

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
  xstart <- seq(1, w, by = winc*5)
  ystart <- seq(1, h, by = hinc*5)
  
  res_mat <- matrix(nrow = length(xstart), ncol = length(ystart))
  
  # find the average of each chunk
  for(i in 1:length(xstart)){
    for(j in 1:length(ystart)){
      crop_statement <- paste0(winc, "x", hinc, "+", xstart[i], "+", ystart[j])
      temp <- image_crop(img, crop_statement) %>% image_quantize(max = 2) %>% image_median(radius = 5)

      res_mat[i, j] <- (as.integer(temp) %>% as.hexmode() %>% as.character() %>% c() %>% table() %>% sort() %>% head(1) %>% names())
    }
  }
  
  res <- melt(res_mat)
  res$value <- as.character(res$value)
  res$value <- paste0('#', res$value)#, substr(res$value, 3, 8))
  
  ggplot(res, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = 'black') +
    geom_text(aes(label = value), color = 'white') +
    scale_fill_manual(values = unique(res$value)) +
    theme_void() +
    theme(legend.position = 'null')
}