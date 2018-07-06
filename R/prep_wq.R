#'Prepare Water Quality Data
#'
#'@param long.df = Water quality data in long format
#'@return Transform water quality data from long to wide format
#'@export

prep_wq <- function(long.df){
  final.df <- long.df %>%
    dplyr::filter(reporting_parameter %in% c("spcond", "ph", "do"),
                  event_id != "228486" & sample_replicate_type != "fs1") %>%
    tidyr::complete(event_id, reporting_parameter) %>%
    dplyr::group_by(event_id) %>%
    dplyr::mutate(na_wq = sum(is.na(reported_value))) %>%
    dplyr::group_by(event_id, reporting_parameter, na_wq) %>%
    dplyr::summarize(reported_value = mean(reported_value)) %>%
    dplyr::ungroup() %>%
    tidyr::spread(reporting_parameter, reported_value) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      do = dplyr::if_else(do >= 22, as.numeric(NA), do),
      spcond = dplyr::if_else(spcond <= 25, as.numeric(NA), spcond),
      ph = dplyr::if_else(ph < 0 | ph > 14, as.numeric(NA), ph)
    )

  return(final.df)
}
