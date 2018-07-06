## ------------------------------------------------------------------------
stream.cat.path <- "H:/GIS3/GIS5/New_GIS/streamcat/unzip"

nlcd.filename.vec <- c("NLCD2001_Region02.csv",
                         "NLCD2006_Region02.csv",
                         "NLCD2011_Region02.csv")

nlcd.df <- map(nlcd.filename.vec, function(file.i) {
  data.table::fread(file.path(stream.cat.path, file.i)) %>%
    toolbox::prep_df() %>% 
    select(comid, matches("pctdecid|pctconif|pctmxfst|pcturb")) %>% 
    select(comid, ends_with("ws"))
}) %>%
  reduce(full_join, by = c("comid"))

## ------------------------------------------------------------------------
imp.sub <- imp.df %>% 
  select(comid, pctimp2001ws, pctimp2006ws, pctimp2011ws) #%>% 
  # gather(group_imp, pct_imp, -comid) %>% 
  # mutate(year = case_when(
  #   stringr::str_detect(group_imp, "2001") ~ "2001",
  #   stringr::str_detect(group_imp, "2006") ~ "2006",
  #   stringr::str_detect(group_imp, "2011") ~ "2011",
  #   TRUE ~ "ERROR"
  # ))

## ------------------------------------------------------------------------
nlcd.df <- left_join(nlcd.df, imp.sub, by = c("comid"))

## ------------------------------------------------------------------------
nlcd.long <- nlcd.df %>% 
  gather(param, value, -comid) %>% 
  mutate(group = case_when(
    stringr::str_detect(param, "pctdecid|pctconif|pctmxfst") ~ "pct_forest",
    stringr::str_detect(param, "pcturb") ~ "pct_urban",
    stringr::str_detect(param, "pctimp") ~ "pct_imp",
    TRUE ~ "ERROR"
  ),
  year = case_when(
    stringr::str_detect(param, "2001") ~ "2001",
    stringr::str_detect(param, "2006") ~ "2006",
    stringr::str_detect(param, "2011") ~ "2011",
    TRUE ~ "ERROR"
  )
  ) %>% 
  group_by(comid, group, year) %>% 
  summarize(value = sum(value))

## ------------------------------------------------------------------------
nlcd.bibi.df <- left_join(bio.fam.df, nlcd.long, by = c("featureid" = "comid")) %>% 
  mutate(keep = case_when(
    year == "2001" & date >= as.Date("1998-06-01") & date < as.Date("2003-06-01") ~ TRUE,
    year == "2006" & date >= as.Date("2003-06-01") & date < as.Date("2008-06-01") ~ TRUE,
    year == "2011" & date >= as.Date("2008-06-01") & date < as.Date("2013-06-01") ~ TRUE,
    TRUE ~ FALSE
  )) %>% 
  filter(keep == TRUE) %>% 
  spread(group, value) %>% 
  select(event_id, category, date, year, sample_number,
         final_score, ref_10, pct_forest, pct_imp, pct_urban) %>% 
  mutate(classification = if_else(final_score >= ref_10, "TP", "FN"))

## ------------------------------------------------------------------------
nlcd.bibi.ref <- nlcd.bibi.df %>% 
  filter(category == "ref")

## ------------------------------------------------------------------------
point.plot <- ggplot(nlcd.bibi.ref, aes(final_score, pct_forest, color = classification)) +
  geom_point(alpha = 0.25) +
  scale_color_manual(values = c("TP" = "#56B4E9",
                               "FN" = "#E69F00"),
                     name = "Rating") +
   geom_hline(aes(yintercept = 50), linetype = "dashed") +
   geom_label(aes(-5, 50, label = "50%"), color = "black") +
  ylab("Forest (%)") +
  xlab("BIBI Score")


box.plot <- ggplot(nlcd.bibi.ref, aes(classification, pct_forest, color = classification)) +
  geom_jitter(alpha = 0.25) +
    scale_color_manual(values = c("TP" = "#56B4E9",
                               "FN" = "#E69F00"),
                     name = "Rating") +
  geom_boxplot(outlier.alpha = 0, alpha = 0, color = "black") +
  geom_hline(aes(yintercept = 50), linetype = "dashed") +
  geom_label(aes(0.5, 50, label = "50%"), color = "black") +
  ylab("Forest (%)") +
  xlab("Classification")

cowplot::plot_grid(point.plot, box.plot, ncol = 1, labels = "auto")

## ------------------------------------------------------------------------
nlcd.ce <- purrr::map_df(c(50, 60, 70, 80, 90), function(j) {
  purrr::map_df(c(2, 5, 10), function(i) {
    # print(paste(j, i))
 nlcd.bibi.df %>% 
    filter(pct_forest < j | pct_imp > i) %>% 
    anti_join(bio.fam.df, ., by = c("event_id", "sample_number", "final_score")) %>% 
  cutpoints() %>% 
  ce() %>% 
  mutate(new_ref_count = tp + fn,
         new_deg_count = tn + fp) %>% 
  select(spatial, ce, new_ref_count, new_deg_count) %>% 
  rename(new_ce = ce)  %>% 
    mutate(forest_thresh =">= 50%",
           forest_thresh = paste(">=", j),
           imp_thresh = paste("<=", i))
})
})

## ------------------------------------------------------------------------
comp.df <- full_join(org.ce, nlcd.ce , by = "spatial") %>% 
  mutate(change_ce = new_ce - orginal_ce,
         change_ref_count = new_ref_count - org_ref_count) %>% 
  mutate_if(is.numeric, funs(round(., 2))) %>% 
  arrange(desc(change_ce)) %>% 
  select(spatial, forest_thresh, imp_thresh, org_ref_count, new_ref_count, change_ref_count,
         orginal_ce, new_ce, change_ce) 

## ------------------------------------------------------------------------
comp.df  %>% 
  kableExtra::kable() %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

