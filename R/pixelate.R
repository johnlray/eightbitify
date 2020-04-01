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

pixelate <- function(img_path, resolution = 200, imgpath){
  img <- readJPEG('~/Desktop/vzxLeexP9rBn9zKfwhXPS6TK_cwIP9kaFnFN1r28dL4.jpg')
  info <- image_info(image_read('~/Desktop/vzxLeexP9rBn9zKfwhXPS6TK_cwIP9kaFnFN1r28dL4.jpg'))
  
  vals <- matrix(c(img[,,1], img[,,2],img[,,3]), ncol = 3, byrow = F)
  vals[, 4] <- paste(vals[, 1], vals[, 2], vals[, 3], collapse=" ")
  
  # find the uncommon colors
  allcols <- rev(sort(table(vals)))
  
  # pick a top n
  top_n <- names(allcols)[1:200]
  
  # minimize that distance
  for(i in 1:nrow(vals)){
    closest <- top_n[which.min(abs(sum(vals[, i]) - top_n))]
    vals[i] <- closest
  }

  dist <- sqrt((b_col[1] - a_col[1]) + (b_col[2] - a_col[2]) + (b_col[3] - a_col[3]))
  
  # grepl the old color to the new color
  myImg <- matrix(the_val, dim(img)[1], dim(img)[2])
  
  # w_fidelity <- round(info$width * 1/resolution, 0)
  # h_fidelity <- round(info$height * 1/resolution, 0)
  # 
  # ws <- seq(1, info$width, w_fidelity)
  # hs <- seq(1, info$height, h_fidelity)
  # 
  # myImg <- myImg[hs, ws]
  # 
  # out <- image(t(matrix(length(as.vector(myImg)):1, nrow = length(hs), ncol = length(ws))), col=as.vector(myImg), xaxt = "n", yaxt = "n")
  
  return(out)
}