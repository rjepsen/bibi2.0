#'Clean Data Frame
#'
#'@param x A data frame
#'@return Clean up data frame.
#'@export

clean_data_frame <- function(x) {
  final.df <- x %>%
    dplyr::rename_all(tolower) %>%
    dplyr::rename_all(stringr::str_trim) %>%
    dplyr::mutate_if(is.character, tolower) %>%
    dplyr::mutate_if(is.character, stringr::str_trim)
}


