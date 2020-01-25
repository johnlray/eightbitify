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

pixelate <- function(img_path, resolution = 200){
  img <- readJPEG('~/Downloads/IMG_20190712_095348.jpg')
  info <- image_info(image_read('~/Downloads/IMG_20190712_095348.jpg'))
  
  the_val <- rgb(img[,,1], img[,,2],img[,,3])
  
  # to smooth colors...
  # find the uncommon colors
  allcols <- table(the_val)
  
  # set a smoothing threshold
  threshold <- .5
  
  # find nearest color above threshold for every color below threshold
  # distance between each color and nearest color above threshold
  # minimize that distance
  # grepl the old color to the new color
  
  myImg <- matrix(the_val, dim(img)[1], dim(img)[2])
  
  w_fidelity <- round(info$width * 1/resolution, 0)
  h_fidelity <- round(info$height * 1/resolution, 0)
  
  ws <- seq(1, info$width, w_fidelity)
  hs <- seq(1, info$height, h_fidelity)
  
  myImg <- myImg[hs, ws]
  
  out <- image(t(matrix(length(as.vector(myImg)):1, nrow = length(hs), ncol = length(ws))), col=as.vector(myImg), xaxt = "n", yaxt = "n")
  
  return(out)
}