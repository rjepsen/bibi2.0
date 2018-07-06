

#==============================================================================
#'Prepare Class Multi
#'
#'@param long.df = the output of the prep_env function.
#'@return A data frame containing environmental data to classify sites.
#'@export

site_classification <- function(long.df){

  final.df <- long.df %>%
    dplyr::mutate(
      deg_spcond = dplyr::case_when(
        spcond >= 1000 ~ 3,
        spcond >= 750 ~ 2,
        spcond > 300 ~ 1,
        is.na(spcond) ~ as.numeric(NA),
        TRUE ~ 0
      ),
      deg_ph = dplyr::case_when(
        ph > 9.5 | ph < 4 ~ 3,
        ph > 8.5 | ph < 6 ~ 1,
        is.na(ph) ~ as.numeric(NA),
        TRUE ~ 0
      ),
      deg_do = dplyr::case_when(
        do <= 5 ~ 1,
        is.na(do) ~ as.numeric(NA),
        TRUE ~ 0
      ),
      deg_hab = dplyr::case_when(
        mean_hab >= 16 & na_hab <= 3 ~ 0,
        mean_hab >= 14 & na_hab <= 3 ~ 1,
        mean_hab > 12 & na_hab <= 3 ~ 2,
        mean_hab <= 12 & na_hab <= 3 ~ 3,
        TRUE ~ as.numeric(NA)
      ),
      deg_wq = deg_spcond + deg_ph + deg_do,
      category = dplyr::case_when(
        deg_wq == 0 & hab_16 == (8 - na_hab) ~ "ref", #"ref_plus",
        deg_wq == 0 & hab_16 / (8 - na_hab) * 100 >= 75 & hab_12 == 0 ~ "ref",
        deg_wq == 0 & hab_16 / (8 - na_hab) * 100 >= 66 ~ "min",
        deg_wq == 0 & hab_12 / (8 - na_hab) * 100 >= 50 ~ "mod",
        deg_wq > 0 | hab_6 / (8 - na_hab) * 100 >= 50 ~ "deg",
        8 - na_hab < 3 ~ "mix",
        TRUE ~ "mix"
      )
    )

  return(final.df)
}


