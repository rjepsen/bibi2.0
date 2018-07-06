## ---- echo=FALSE---------------------------------------------------------
suppressPackageStartupMessages(
  library(tidyverse)
)
library(toolbox)

## ------------------------------------------------------------------------
score.path <- "H:/Projects/Chessie_BIBI/report/FINAL_May25_2017/2017_Data/Scores_Ratings"
bio.fam.path <- "Bioregion/BIOREGION_FAMILY_All_Event_Ratings_2017-06-29.csv"

bio.fam.df <- data.table::fread(file.path(score.path, bio.fam.path)) %>%
  toolbox::prep_df() %>%
  mutate(date = as.Date(date, "%m_%d_%Y"),
         category = if_else(category == "sev", "deg", category))


## ------------------------------------------------------------------------
catchment.df <- data.table::fread(file.path(rprojroot::find_rstudio_root_file(),
                                            "data/assigned_catchments_oct16.csv")) %>%
  toolbox::prep_df()

## ------------------------------------------------------------------------
bio.fam.df <- left_join(bio.fam.df, catchment.df,
                        by = c("station_id"))
rm(catchment.df)

## ------------------------------------------------------------------------
test <- bio.fam.df %>%
  filter(is.na(featureid))
if(nrow(test) > 0) {
test %>% 
    kableExtra::kable() %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
}

## ------------------------------------------------------------------------
stream.cat.path <- "H:/GIS3/GIS5/New_GIS/streamcat/unzip"

imp.filename.vec <- c("ImperviousSurfaces2001_Region02.csv",
                      "ImperviousSurfaces2006_Region02.csv",
                      "ImperviousSurfaces2011_Region02.csv")

imp.df <- map(imp.filename.vec, function(file.i) {
  data.table::fread(file.path(stream.cat.path, file.i)) %>%
    toolbox::prep_df()
}) %>%
  reduce(full_join, by = c("comid", "catareasqkm",
                           "wsareasqkm", "catpctfull",
                           "wspctfull"))

## ------------------------------------------------------------------------
imp.bibi.df <- left_join(bio.fam.df, imp.df, by = c("featureid" = "comid"))

## ------------------------------------------------------------------------
imp.bibi.df <- imp.bibi.df %>%
  mutate(
    # pctimp2001ws = if_else(date < as.Date("2003-06-01"), pctimp2001ws, as.numeric(NA)),
    pctimp2001ws = if_else(date >= as.Date("1998-06-01") & date < as.Date("2003-06-01"),
                           pctimp2001ws, as.numeric(NA)),
    pctimp2006ws = if_else(date >= as.Date("2003-06-01") & date < as.Date("2008-06-01"),
                           pctimp2006ws, as.numeric(NA)),
    pctimp2011ws = if_else(date >= as.Date("2008-06-01") & date < as.Date("2013-06-01"),
                           pctimp2011ws, as.numeric(NA))
    # pctimp2011ws = if_else(date >= as.Date("2008-06-01"), pctimp2011ws, as.numeric(NA))
  ) %>% 
  mutate(
    # pctimp2001cat = if_else(date < as.Date("2003-06-01"), pctimp2001cat, as.numeric(NA)),
    pctimp2001cat = if_else(date >= as.Date("1998-06-01") & date < as.Date("2003-06-01"),
                           pctimp2001cat, as.numeric(NA)),
    pctimp2006cat = if_else(date >= as.Date("2003-06-01") & date < as.Date("2008-06-01"),
                           pctimp2006cat, as.numeric(NA)),
    pctimp2011cat = if_else(date >= as.Date("2008-06-01") & date < as.Date("2013-06-01"),
                           pctimp2011cat, as.numeric(NA))
    # pctimp2011cat = if_else(date >= as.Date("2008-06-01"), pctimp2011cat, as.numeric(NA))
  )

## ------------------------------------------------------------------------
ref.df <- imp.bibi.df %>%
  filter(category == "ref") %>%
  mutate(ref_cat = if_else(final_score < ref_10, "PVP", "EGF"),
         ref_cat = factor(ref_cat, levels = c("PVP", "EGF"))) %>%
  arrange(final_score)

## ------------------------------------------------------------------------
ref.imp <- ref.df %>%
  select(event_id, date, sample_number, final_score, ref_cat,
         pctimp2001ws, pctimp2006ws, pctimp2011ws) %>%
  gather(imp, imp_value, pctimp2001ws:pctimp2011ws) %>%
  filter(!is.na(imp_value))
  # mutate(keep = case_when(
  #   date < as.Date("1998-06-01") | date >= as.Date("2013-06-01") ~ "keep",
  #   date >= as.Date("1998-06-01") & date < as.Date("2013-06-01") & !is.na(imp_value) ~ "keep",
  #   date >= as.Date("1998-06-01") & date < as.Date("2013-06-01") & is.na(imp_value) ~ "remove",
  #   TRUE ~ "ERROR"
  #   )) %>% 
  # filter(keep == "keep") %>% 
  # select(-keep)

