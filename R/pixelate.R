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
  
  the_cimg <- magick2cimg(img)
  cimg_df <- as.data.frame(the_cimg)
  
  w <- info$width
  h <- info$h
  
  winc <- round(w/256, 0)
  hinc <- round(h/256, 0)
  
  Rs <- as.hexmode(as.integer(the_cimg[,,1]*255))
  Gs <- as.hexmode(as.integer(the_cimg[,,2]*255))
  Bs <- as.hexmode(as.integer(the_cimg[,,3]*255))
  
  # break the image into chunks
  xstart <- seq(1, w, by = winc*10)
  ystart <- seq(1, h, by = hinc*10)
  
  # find the average of each chunk
  for(i in 1:length(xstart)){
    for(j in 1:length(ystart)){
      crop_statement <- paste0(winc, "x", hinc, "+", xstart[i], "+", ystart[j])
      temp <- image_crop(img, crop_statement) %>% image_quantize(max = 2) %>% image_median(radius = 5)
      
    }
  }
  
  # reassemble -- maybe with the same offsetting approach used to disassemble?
}