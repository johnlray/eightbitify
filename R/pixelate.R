#' Add together two numbers
#'
#' @param img_path The image you want to work with
#' @return 
#' @examples
#' @export
#pixelate <- function(img_path, resolution){
  img <- image_read('http://jeroen.github.io/images/tiger.svg')
  
  info <- image_info(img)
  
  the_cimg <- magick2cimg(img)
  
  w <- info$width
  h <- info$h
  
  winc <- round(w/256, 0)
  hinc <- round(h/256, 0)
  
  imgradient(img, "x", scheme = 3)
#}