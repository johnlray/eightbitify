#' Add together two numbers
#'
#' @param img_path The image you want to work with
#' @return 
#' @examples
#' @export
pixelate <- function(img_path){
  img <- image_read('http://jeroen.github.io/images/tiger.svg')
  
  info <- image_info(img)
  
  w <- info$width
  h <- info$h
}