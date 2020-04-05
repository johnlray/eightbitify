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
library(tidyverse)

pixelate <- function(img_path, resolution = 200, imgpath){
  resolution <- 30
  
  img <- readJPEG('~/Desktop/vzxLeexP9rBn9zKfwhXPS6TK_cwIP9kaFnFN1r28dL4.jpg')
  info <- image_info(image_read('~/Desktop/vzxLeexP9rBn9zKfwhXPS6TK_cwIP9kaFnFN1r28dL4.jpg'))
  
  w_fidelity <- round(info$width * 1/resolution, 0)
  h_fidelity <- round(info$height * 1/resolution, 0)
  
  ws <- seq(1, info$width, w_fidelity)
  hs <- seq(1, info$height, h_fidelity)
  
  myImg <- matrix(rgb(img[,,1], img[,,2], img[,,3]), dim(img)[1], dim(img)[2])
  
  myImg <- myImg[hs, ws]
  
  out <- image(t(matrix(length(as.vector(myImg)):1, nrow = length(hs), ncol = length(ws))), col=as.vector(myImg), xaxt = "n", yaxt = "n")
  
  vals <- matrix(c(img[,,1], img[,,2],img[,,3]), ncol = 3, byrow = F)
  
  # pick a top n
  vals %>%
    data.frame() %>% 
    #sample_n(size = 100000) %>%
    .[order( .[, 1], .[, 2], .[, 3]), ] %>%
    head()

  
  return(out)
}