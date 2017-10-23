#'Merge taxa, water quality, and tab.habitat data into one
#'
#'@param tab.taxa = Taxonomic counts in long data format
#'@param Taxonomy = TSN #'s with necessary taxonomic levels (CLASS, ORDER, FAMILY)
#'@param tab.wq = Water quality data in a long data format
#'@param tab.habitat = tab.habitat data in a long data format
#'@param tab.event = EVENT_ID
#'@param tab.stations = Station information (STATION_ID, ECOREGION, STRAHLER)
#'@param agg_sample_num = Should the sample number be treated as seperate
#'samples (FALSE) or aggregated into a single sample (TRUE)?
#'@param development = during index development, "development" should be
#'set to TRUE.  When TRUE the sample_number (replicate) with the largest reporting
#'value (# of individual observed) is selected for.  During final scoring development
#'should be set to FALSE.  When FALSE all replicates are included in the final
#'output.
#'@return Merge taxa water quality, and tab.habitat data into one large data frame
#'@export

prep_data <- function(tab.taxa, tab.wq, tab.habitat, tab.event, tab.stations,
                      tab.project, tab.huc,
                      agg_sample_num = TRUE,
                      development = TRUE){

  prep.tab.wq <- prep_wq(tab.wq)

  prep.habitat <- prep_habitat(tab.habitat, agg_sample_num)

  prep.df <- prep_merge(tab.event, tab.taxa, tab.stations,
                        prep.tab.wq, prep.habitat,
                        tab.project, tab.huc) %>%
    dplyr::mutate(date = as.Date(sample_date_time, format = "%m/%d/%Y"),
                  sample_number = dplyr::if_else(is.na(sample_number),
                                                  as.integer(1),
                                                  as.integer(sample_number)),
                  agency_code = dplyr::if_else(is.na(agency_code),
                                                "blank",
                                                agency_code)
    )

  final.df <- prep.df %>%
    prep_method() %>%
    dplyr::group_by_at(vars(-reporting_value)) %>%
    dplyr::summarize(reporting_value = sum(reporting_value)) %>%
    dplyr::ungroup()

  if (development == TRUE) {
    bigger.sample <- final.df %>%
      dplyr::group_by(event_id, sample_number) %>%
      dplyr::summarize(total = sum(reporting_value)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(event_id) %>%
      dplyr::filter(total == max(total))

    final.df <- dplyr::semi_join(final.df, bigger.sample, by = c("event_id", "sample_number"))
  }

  final.df <- final.df %>%
    tidyr::unite(unique_id, event_id, sample_number)

  final.df <- final.df %>%
    dplyr::group_by(unique_id) %>%
    dplyr::filter(sum(reporting_value) > 70)

  return(final.df)
}
