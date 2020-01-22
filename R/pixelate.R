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
library(scales)

pixelate <- function(img_path, resolution){
  info <- image_read('~/Downloads/IMG_20190712_095348.jpg') %>% image_info()
  
  the_cimg <- imager::load.image('~/Downloads/IMG_20190712_095348.jpg') %>% imager::as.cimg()
  
  cimg_df <- as.data.frame(the_cimg)
  
  w_fidelity <- round(info$width * 1/200, 0)
  h_fidelity <- round(info$height * 1/200, 0)
  
  ws <- seq(1, info$width, 1)
  hs <- seq(1, info$height, 1)
  
  samp_mat <- matrix(ncol = length(hs), nrow = length(ws))
  
  for(i in 1:nrow(samp_mat)){
    for(j in 1:ncol(samp_mat)){
      samp_mat[i, j] <- rgb(
        red = the_cimg[ws[i], hs[j], 1, 1]*255,
        blue = the_cimg[ws[i], hs[j], 1, 2]*255,
        green = the_cimg[ws[i] ,hs[j], 1, 3]*255,
        maxColorValue = 255
      )
      
      print(samp_mat[i, j])
    }
  }
  
  image(matrix(1:length(as.vector(samp_mat)), nrow = 404, ncol = 378), col=as.vector(samp_mat), xaxt = "n", yaxt = "n")
  
  #pdat <- reshape2::melt(samp_mat)
  
  #ggplot(pdat, aes(x = Var1, y = Var2, fill = value)) +
  #  geom_tile() +
  #  coord_flip() +
  #  scale_fill_manual(values = as.character(pdat$value)) +
  #  theme_void() +
  #  theme(legend.position = 'null')
}