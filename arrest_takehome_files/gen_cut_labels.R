#' Create nice labels from a factor generated from the cut or cut_number() functions
#'
#' `gen_cut_labels` creates labels from a factor generated from `cut` or `ggplot2::cut_number()`
#'
#' @param x factor with quantitative levels
#' @param type character option of output: options are `"dollars"`, `"percent"`, and `"count"``
#' @param round numeric value that indicates number of decimals to round to
#' @param zero boolean value that indicates whether or not to round 0 
#'
#' @md
#' @export
gen_cut_labels <- function(x, type = "count", round = 0, inset = " to ") {
  
  my_levels<- levels(x) %>%
    str_remove_all( pattern = "\\[|\\]|\\(|\\)|\\]") %>%
    str_split(",") 
  
  levels_from <- my_levels %>%
    map_chr(1) %>%
    as.numeric()
  
  
  levels_to <- my_levels %>%
    map_chr(2) %>% 
    as.numeric
  
  my_accuracy = 1 / 10^round
  
  if(is.null(type) | type == "count"){
    final_levels <- paste0(scales::comma(levels_from, accuracy = my_accuracy), 
                           inset,
                           scales::comma(levels_to, accuracy = my_accuracy))
  } else if(type == "dollars"){
  final_levels <-  paste0(scales::dollar(levels_from, accuracy = my_accuracy), 
                          inset,
                          scales::dollar(levels_to, accuracy = my_accuracy))
    
  } else if(type == "percent"){

  final_levels <- paste0(scales::percent(levels_from,
                                         accuracy = my_accuracy, 
                                         big.mark = ","
                                         ), 
                         inset, 
                         scales::percent(levels_to, accuracy = my_accuracy, big.mark = ","))
    
  } else  {
    stop("invalid `type` value: valid options are: `dollars`, `percent`, and `count`", call. = T)
  }
  
  
  final_levels
  
}