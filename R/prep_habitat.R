#'Prepare Habitat Data
#'
#'@param Habitat = Habitat data in long format
#'@param agg_sample_num = Should the sample number be treated as seperate
#'samples (FALSE) or aggregated into a single sample (TRUE)?
#'@return Transform habitat data from long to wide format
#'@export


prep_habitat <- function(long.df, agg_sample_num = TRUE) {

  h_params <- c("banks", "bankv", "ch_alt", "embed",
                "epi_sub", "flow", "riff", "sed")
  final.df <- long.df %>%
    dplyr::filter(habitat_reporting_parameter %in% h_params) %>%
    tidyr::unite(unique_id, event_id, sample_number) %>%
    tidyr::complete(unique_id, habitat_reporting_parameter) %>%
    tidyr::separate(unique_id, c("event_id", "sample_number")) %>%
    dplyr::mutate(event_id = as.integer(event_id),
                  sample_number = as.integer(sample_number),
                  reporting_parameter_value = dplyr::if_else(
                    reporting_parameter_value < 0 | reporting_parameter_value > 20,
                    as.double(NA),
                    as.double(reporting_parameter_value)
                  )) %>%
    dplyr::group_by(event_id) %>%
    dplyr::mutate(mean_hab = mean(reporting_parameter_value, na.rm = TRUE),
                  median_hab = median(reporting_parameter_value, na.rm = TRUE),
                  na_hab = sum(is.na(reporting_parameter_value)),
                  hab_6 = sum(reporting_parameter_value <= 6),
                  hab_12 = sum(reporting_parameter_value <= 12),
                  hab_16 = sum(reporting_parameter_value >= 16)) %>%
    dplyr::select(event_id, sample_number, mean_hab, median_hab, na_hab,
                  hab_6, hab_12, hab_16) %>%
    dplyr::distinct()
  return(final.df)

}
