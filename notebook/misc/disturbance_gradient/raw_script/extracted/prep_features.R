## ------------------------------------------------------------------------

channel <- RODBC::odbcConnect("CBIBI_2017")

tab.vec <- c("TAB_WQ_DATA",
             "TAB_HABITAT_ASSESSMENT",
             "TAB_TAXONOMIC_COUNT")

tab.list <- purrr::map(tab.vec, function(tab.i) {
 test <-  RODBC::sqlFetch(channel, tab.i, stringsAsFactors = FALSE) %>% 
   toolbox::prep_df()
})  %>% 
  set_names(tolower(tab.vec))

RODBC::odbcCloseAll()


## ------------------------------------------------------------------------
wq.df <- tab.list$tab_wq_data %>% 
  select(event_id, reporting_parameter, reported_value) %>% 
  group_by(event_id, reporting_parameter) %>% 
  summarize(reported_value = mean(reported_value)) %>% 
  ungroup() %>% 
  mutate(event_id = as.character(event_id)) %>% 
  rename(wq_param = "reporting_parameter",
         wq_value = "reported_value")

## ------------------------------------------------------------------------
habitat.df <- tab.list$tab_habitat_assessment %>% 
  filter(!is.na(habitat_reporting_parameter)) %>% 
  group_by(event_id, sample_number, habitat_reporting_parameter) %>% 
  summarize(reporting_parameter_value = mean(reporting_parameter_value)) %>% 
  ungroup() %>% 
  mutate(event_id = as.character(event_id)) %>% 
  rename(hab_param = "habitat_reporting_parameter",
         hab_value = "reporting_parameter_value")

## ------------------------------------------------------------------------
method.df <- tab.list$tab_taxonomic_count %>% 
  select(event_id, sample_number, bio_method, g_method, sample_type) %>% 
  distinct() %>% 
  mutate(event_id = as.character(event_id)) %>% 
  gather(method_param, method_value, bio_method:sample_type)

rm(tab.list)

## ------------------------------------------------------------------------
scat.file.path <- "H:/GIS3/GIS5/New_GIS/streamcat/unzip"
scat.files.vec <- list.files(scat.file.path)

## ---- eval=FALSE---------------------------------------------------------
## scat.df <- map(scat.files.vec , function(file.i) {
##   print(file.i)
##   data.table::fread(file.path(scat.file.path, file.i)) %>%
##     prep_df()
## }) %>%
##   reduce(full_join) %>%
##   gather(scat_param, scat_value, -comid) %>%
##   mutate(scat_value = as.numeric(scat_value)) %>%
##   filter(stringi::stri_sub(scat_param,-3,-1) != "cat",
##          !stringr::str_detect(scat_param, "catrp"))

## ---- include=FALSE, eval=FALSE------------------------------------------
## data.table::fwrite(scat.df, "H:/GIS3/GIS5/New_GIS/streamcat/all_streamcat.csv")

## ---- echo=FALSE---------------------------------------------------------
scat.df <- data.table::fread("H:/GIS3/GIS5/New_GIS/streamcat/all_streamcat.csv",
                             showProgress = FALSE) %>% 
  filter(stringi::stri_sub(scat_param, -3, -1) != "cat") %>% 
  mutate(scat_value = as.numeric(scat_value))

## ------------------------------------------------------------------------
bio.fam.sub <- bio.fam.df %>%
  select(spatial, category, event_id, sample_number, final_score, ref_10, featureid) %>% 
  rename(class = "category",
         comid = "featureid") %>% 
  mutate(event_id = as.character(event_id),
         sample_number = as.numeric(sample_number))

## ---- message=FALSE------------------------------------------------------
# feat.df <- reduce(list(bio.fam.sub, wq.df, habitat.df, scat.df), full_join)
bio.fam.ref <- bio.fam.sub %>%
    filter(class == "ref") %>%
    mutate(Classification = if_else(final_score >= ref_10, "TP", "FN"))


