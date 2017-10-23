#'Ecoregion and Bioregion Modifications
#'
#'@param long.df = Long data frame.
#'@return Assigns Chessie BIBI related ecoregion and bioregion distinctions.
#'@export

prep_ecoregion <- function(long.df){
  final.df  <- long.df %>% 
    mutate(eco3 = stringr::str_sub(ecoregion_level_4, 1, 2),
           eco3 = dplyr::case_when(
             !subregion_description %in% "susquehanna" & eco3 == 67 ~ "67s",
             subregion_description %in% "susquehanna" & eco3 == 67 ~ "67n",
             !subregion_description %in% c("susquehanna", "upper chesapeake") & eco3 == 64 ~ "64s",
             subregion_description %in% c("susquehanna", "upper chesapeake") & eco3 == 64 ~ "64n",
             !subregion_description %in% c("susquehanna", "upper chesapeake") & eco3 == 69 ~ "69s",
             subregion_description %in% c("susquehanna", "upper chesapeake") & eco3 == 69 ~ "69n",
             icprb_bioregion_id %in% "sgv" ~ "67sgv",
             icprb_bioregion_id %in% "ngv" ~ "67ngv",
             icprb_bioregion_id %in% "blue" ~ "67blue",
             TRUE ~ eco3
           ),
           bioregion = dplyr::case_when(
             eco3 %in% c("58", "60", "83") ~ "napu",
             eco3 %in% "62" ~ "nca",
             eco3 %in% "67N" ~ "nrv",
             eco3 %in% c("64n", "67ngv") ~ "unp",
             eco3 %in% c("69s", "69n") ~ "ca",
             eco3 %in% "67s" ~ "srv",
             eco3 %in% "64s" ~ "lnp",
             eco3 %in% "45" ~ "spied",
             eco3 %in% c("66", "67blue") ~ "blue",
             eco3 %in% "65" ~ "sep",
             eco3 %in% "67sgv" ~ "sgv",
             eco3 %in% "63" ~ "mac",
             TRUE ~ "ERROR"
           ))
  
  return(long)  
}
#------------------------------------------------------------------------------