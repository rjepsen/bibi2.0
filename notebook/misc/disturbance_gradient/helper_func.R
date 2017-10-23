clean_df <- function(x) {
  if (!is.data.frame(x)) stop("'x' must be a data frame or tibble")
  final.df <- x %>%
    dplyr::rename_all(tolower) %>%
    dplyr::rename_all(stringr::str_trim) %>%
    dplyr::mutate_if(is.character, tolower) %>%
    dplyr::mutate_if(is.character, stringr::str_trim)
  return(final.df)
}
