#'Merge taxa, water quality, and habitat data into one
#'
#'@param tab.event = EVENT_ID
#'@param taxa.prep = Taxa data in a long data format
#'@param tab.stations = Station information (STATION_ID, ECOREGION, STRAHLER)
#'@param tab.wq = Water quality data in a wide format
#'@param tab.habitat = Habitat data in a wide format
#'@param tab.project = Project information
#'@param tab.huc = HUC information merged on HUC-12
#'@return Merge taxa water quality, and habitat data into one large data frame.
#'@export

prep_merge <- function(tab.event, taxa.prep, tab.stations, tab.wq,
                       tab.habitat, tab.project, tab.huc) {

  final.df <- dplyr::full_join(tab.event, taxa.prep, by = "event_id") %>%
    dplyr::left_join(tab.stations, by = "station_id") %>%
    dplyr::left_join(tab.wq, by = "event_id") %>%
    dplyr::left_join(tab.habitat, by = c("event_id", "sample_number")) %>%
    dplyr::left_join(tab.project, by = "project_id") %>%
    dplyr::left_join(tab.huc, by = "huc_12")

  return(final.df)
}
