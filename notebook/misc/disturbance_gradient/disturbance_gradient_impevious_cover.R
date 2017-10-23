# Packages ----------------------------------------------------------------
library(tidyverse)
library(toolbox)
# source("notebook/misc/disturbance_gradient/helper_func.R")
# Load BIBI Data ---------------------------------------------------------------
score.path <- "H:/Projects/Chessie_BIBI/report/FINAL_May25_2017/2017_Data/Scores_Ratings"
bio.fam.path <- "Bioregion/BIOREGION_FAMILY_All_Event_Ratings_2017-06-29.csv"

bio.fam.df <- data.table::fread(file.path(score.path, bio.fam.path)) %>%
  prep_df() %>%
  mutate(date = as.Date(date, "%m_%d_%Y"),
         category = if_else(category == "sev", "deg", category))

# BIBI Catchments ---------------------------------------------------------
catchment.df <- data.table::fread("data/assigned_catchments_oct16.csv") %>%
  prep_df()

bio.fam.df2 <- left_join(bio.fam.df, catchment.df,
                        by = c("station_id"))

na.df <- bio.fam.df2 %>%
  filter(is.na(featureid))
# Load StreamCat Data -----------------------------------------------------
stream.cat <- "H:/GIS3/GIS5/New_GIS/streamcat/unzip"

imp.filename.vec <- c("ImperviousSurfaces2001_Region02.csv",
                      "ImperviousSurfaces2006_Region02.csv",
                      "ImperviousSurfaces2011_Region02.csv")

imp.df <- map(imp.filename.vec, function(file.i) {
  data.table::fread(file.path(stream.cat, file.i)) %>%
    prep_df()
}) %>%
  reduce(full_join, by = c("comid", "catareasqkm",
                           "wsareasqkm", "catpctfull",
                           "wspctfull"))

# Join BIBI with StreamCat ------------------------------------------------
imp.bibi.df <- left_join(bio.fam.df2, imp.df, by = c("featureid" = "comid"))

imp.bibi.df <- imp.bibi.df %>%
  mutate(pctimp2001ws = if_else(date < as.Date("2003-06-01"), pctimp2001ws, as.numeric(NA)),
         pctimp2006ws = if_else(date >= as.Date("2003-06-01") & date < as.Date("2008-06-01"), pctimp2006ws, as.numeric(NA)),
         pctimp2011ws = if_else(date >= as.Date("2008-06-01"), pctimp2011ws, as.numeric(NA)),
         pctimp2001cat = if_else(date < as.Date("2003-06-01"), pctimp2001cat, as.numeric(NA)),
         pctimp2006cat = if_else(date >= as.Date("2003-06-01") & date < as.Date("2008-06-01"), pctimp2006cat, as.numeric(NA)),
         pctimp2011cat = if_else(date >= as.Date("2008-06-01"), pctimp2011cat, as.numeric(NA))
         )

# Reference ---------------------------------------------------------------
ref.df <- imp.bibi.df %>%
  filter(category == "ref") %>%
  mutate(ref_score = if_else(final_score < ref_10, "low", "high"),
         ref_score = factor(ref_score, levels = c("high", "low"))) %>%
  arrange(final_score)


# Imp Plots ---------------------------------------------------------------
ggplot(ref.df, aes(final_score, pctimp2001ws, color = ref_score)) +
  geom_point()

ggplot(ref.df, aes(final_score, pctimp2006ws, color = ref_score)) +
  geom_point()

ggplot(ref.df, aes(final_score, pctimp2011ws, color = ref_score)) +
  geom_point()

ref.df %>%
  group_by(agency_code) %>%
  mutate(count = sum(ref_score == "low")) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  mutate(agency_code = factor(agency_code, levels = unique(agency_code))) %>%
  filter(ref_score == "low") %>%
  complete(agency_code) %>%
ggplot(aes(final_score, agency_code, color = ref_score)) +
  geom_boxplot()


test <- ref.df %>%
  select(event_id, sample_number, final_score, ref_score,
         pctimp2001ws, pctimp2006ws, pctimp2011ws) %>%
  gather(imp, imp_value, pctimp2001ws:pctimp2011ws) %>%
  filter(!is.na(imp_value))

test2 <- test %>%
  mutate(imp_cat = if_else(imp_value >= 2, "high", "low")) %>%
  filter(imp_cat == "high")
table(test2$ref_score, test2$imp_cat)

ref.sub <- anti_join(ref.df, test2,
                     by = c("event_id", "sample_number",
                            "final_score", "ref_score"))

bio.fam.sub <- anti_join(bio.fam.df, test2,
                         by = c("event_id", "sample_number", "final_score")) %>%
  filter(category %in% c("ref", "deg"))

bio.wide <- bio.fam.sub %>%
  select(spatial, category, final_score, ref_10) %>%
  mutate(class = case_when(
    category == "ref" & final_score >= ref_10 ~ "tp",
    category == "ref" & final_score < ref_10 ~ "fn",
    category == "deg" & final_score < ref_10 ~ "tn",
    category == "deg" & final_score >= ref_10 ~ "fp",
    TRUE ~ "ERROR"
  )) %>%
  group_by(spatial, class, ref_10) %>%
  summarize(count = n()) %>%
  spread(class, count) %>%
  mutate(ce = (tp + tn) / (tp + fp + tn + fn) * 100)


