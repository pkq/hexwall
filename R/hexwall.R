#' @title hexwall
#' @description Neatly align hexagon stickers in R
#' @param path path to a folder of hexagon stickers
#' @param sticker_row_size number of stickers in the longest row
#' @param sticker_width width of each sticker (in pixels)
#' @param remove_small should hexagons smaller than sticker_width be removed?
#' @param remove_size Should hexagons of an abnormal size be removed?
#' @examples
#' hexwall("samplehex", sticker_row_size = 3, sticker_width = 200)
#' @export
#' @importFrom magick image_append image_blank image_composite image_info 
#'                    image_read image_resize image_scale image_trim
#' @importFrom purrr invoke map map2 map_dbl map_lgl reduce2 set_names 

hexwall <- function(path, sticker_row_size = 10, sticker_width = 200, 
                    remove_small = TRUE, remove_size = TRUE){
  # Load stickers
  sticker_files <- list.files(path)
  stickers <- file.path(path, sticker_files) %>% 
    map(image_read) %>%
    map(image_trim) %>%
    set_names(sticker_files)
  
  # Low resolution stickers
  low_res <- stickers %>%
    map_lgl(~ remove_small && image_info(.x)$width < sticker_width)
  which(low_res)
  
  stickers <- stickers %>%
    map(image_scale, sticker_width)
  
  # Incorrectly sized stickers
  bad_size <- stickers %>%
    map_lgl(~ remove_size && with(image_info(.x), height < (median(height)-2) | height > (median(height) + 2)))
  which(bad_size)
  
  # Remove bad stickers
  sticker_rm <- low_res | bad_size
  stickers <- stickers[!sticker_rm]
  
  if(any(sticker_rm)){
    message(sprintf("Automatically removed %i incompatible stickers: %s",
                    sum(sticker_rm), paste0(names(sticker_rm[sticker_rm]), collapse = ", ")))
  }
  
  # Coerce sticker sizes
  sticker_height <- stickers %>%
    map(image_info) %>%
    map_dbl("height") %>%
    median
  stickers <- stickers %>%
    map(image_resize, paste0(sticker_width, "x", sticker_height, "!"))
  
  # Arrange rows of stickers into images
  sticker_col_size <- ceiling(length(stickers)/(sticker_row_size-0.5))
  row_lens <- rep(c(sticker_row_size,sticker_row_size-1), length.out=sticker_col_size)
  row_lens[length(row_lens)] <- row_lens[length(row_lens)] - (length(stickers) - sum(row_lens))
  sticker_rows <- map2(row_lens, cumsum(row_lens),
                       ~ seq(.y-.x+1, by = 1, length.out = .x)) %>%
    map(~ stickers[.x] %>%
          invoke(c, .) %>%
          image_append)
  
  # Add stickers to canvas
  canvas <- image_blank(sticker_row_size*sticker_width, 
                        sticker_height + (sticker_col_size-1)*sticker_height/1.33526, "white")
  reduce2(sticker_rows, seq_along(sticker_rows), 
          ~ image_composite(
            ..1, ..2,
            offset = paste0("+", ((..3-1)%%2)*sticker_width/2, "+", round((..3-1)*sticker_height/1.33526))
          ),
          .init = canvas)
}

# hex_wall <- gen_hex_wall("samplehex", sticker_width = 200)
# image_write(hex_wall, "hexwall.png", format = "png")
