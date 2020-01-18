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

get_colorPal <- function(im, n=8, cs="RGB"){
  #print(cs) 
  tmp <-im %>% image_resize("100") %>% 
    image_quantize(max=n, colorspace=cs) %>%  ## reducing colours! different colorspace gives you different result
    magick2cimg() %>%  ## I'm converting, becauase I want to use as.data.frame function in imager package.
    RGBtoHSV() %>% ## i like sorting colour by hue rather than RGB (red green blue)
    as.data.frame(wide="c") %>%  #3 making it wide makes it easier to output hex colour
    mutate(hex=hsv(rescale(c.1, from=c(0,360)),c.2,c.3),
           hue = c.1,
           sat = c.2,
           value = c.3) %>%
    count(hex, hue, sat,value, sort=T) %>% 
    mutate(colorspace = cs)
  
  return(tmp %>% select(colorspace,hex,hue,sat,value,n)) ## I want data frame as a result.
  
}

pixelate <- function(img_path, resolution){
  img <- image_read('http://jeroen.github.io/images/tiger.svg')
  
  info <- image_info(img)
  
  w <- info$width
  h <- info$height
  
  winc <- round(w/256, 0)
  hinc <- round(h/256, 0)
  
  #Rs <- as.hexmode(as.integer(the_cimg[,,1]*255))
  #Gs <- as.hexmode(as.integer(the_cimg[,,2]*255))
  #Bs <- as.hexmode(as.integer(the_cimg[,,3]*255))
  
  # break the image into chunks
  xstart <- seq(1, w, by = winc*2)
  ystart <- seq(1, h, by = hinc*2)
  
  res_mat <- matrix(nrow = length(xstart), ncol = length(ystart))
  
  # find the average of each chunk
  for(i in 1:length(xstart)){
    for(j in 1:length(ystart)){
      crop_statement <- paste0(winc, "x", hinc, "+", xstart[i], "+", ystart[j])
      temp <- image_crop(img, crop_statement) %>%
        image_quantize(max = 1) %>%
        image_median(radius = 5)

      res_mat[i, j] <- (as.integer(temp) %>% as.hexmode() %>% as.character() %>% c() %>% table() %>% sort() %>% head(1) %>% names())
    }
  }
  
  res <- melt(res_mat)
  res$value <- as.character(res$value)
  res$value <- paste0('#', substr(res$value, 1, 6))
  
  ggplot(res, aes(x = Var1, y = Var2*-1, fill = value)) +
    geom_tile() +
    geom_text(aes(label = value), color = 'black') +
    scale_fill_manual(values = unique(res$value), aesthetics = 'identity') +
    theme_void() +
    theme(legend.position = 'null')
  
}