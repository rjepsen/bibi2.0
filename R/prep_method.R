#'Prepare subset specific to BIBI
#'
#'@param long.df Data in a long format.
#'@return A subset of the data containing sampling events with strahler stream
#'order less than or equal to 4 and methods comparable to kick net samples.
#'@export

prep_method <- function(long.df) {
  final.df <- long.df %>%
    dplyr::mutate(month = format(date, "%m"), month = as.integer(month)) %>%
    dplyr::filter(strahler_stream_order <= 4,
                  g_method %in% c(49, 57, 58, 59, 86,
                                87, 89, 92, 93, 94, 101, 103, 104),
                  !month %in% c(12, 1, 2))
  return(final.df)
}
