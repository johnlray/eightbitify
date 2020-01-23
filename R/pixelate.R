#' Add together two numbers
#'
#' @param img_path The image you want to work with
#' @return 
#' @examples
#' @export
#' 

library(colordistance)
library(magick)
library(imager)
library(ggplot2)
library(reshape2)
library(scales)
library(jpeg)

pixelate <- function(img_path, resolution){
  img <- readJPEG('~/Downloads/IMG_20190712_095348.jpg')
  info <- image_info(image_read('~/Downloads/IMG_20190712_095348.jpg'))
  
  #the_cimg <- imager::load.image('~/Downloads/IMG_20190712_095348.jpg') %>% imager::as.cimg()
  
  the_val <- rgb(img[,,1], img[,,2],img[,,3])
  
  allcols <- table(the_val)
  
  colordistance::getColorDistanceMatrix(getHistList('~/Downloads/IMG_20190712_095348.jpg'))
  
  myImg <- matrix(the_val, dim(img)[1], dim(img)[2])
  
  #cimg_df <- as.data.frame(the_cimg)
  
  w_fidelity <- round(info$width * 1/200, 0)
  h_fidelity <- round(info$height * 1/200, 0)
  
  ws <- seq(1, info$width, w_fidelity)
  hs <- seq(1, info$height, h_fidelity)
  
  myImg <- myImg[hs, ws]
  
  image(t(matrix(length(as.vector(myImg)):1, nrow = length(hs), ncol = length(ws))), col=as.vector(myImg), xaxt = "n", yaxt = "n")
  
}