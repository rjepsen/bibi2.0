bio.metric.sel <- data.table::fread(
  file.path("H:/Projects/Chessie_BIBI/report/FINAL_May25_2017",
            "2017_Data/Metric_Thresholds/metric_thresholds.csv"), data.table = FALSE)

bio.metric.sel <- bio.metric.sel %>%
  toolbox::prep_df() %>%
  filter(taxonomic_resolution == "family",
         !spatial_resolution %in% c("basin", "coast", "inland"))

bio.metric.sel <- bio.metric.sel %>%
  group_by(spatial_resolution) %>%
  summarize(index_metrics = list(metric)) %>%
  ungroup() %>%
  rename(spatial = "spatial_resolution")

# metrics -----------------------------------------------------------------
metrics.df <- data.table::fread(
file.path("H:/Projects/Chessie_BIBI/report/FINAL_May25_2017/2017_Data",
          "Metrics/Bioregion/BIOREGION_FAMILY_raw_metrics_2017-06-21.csv"))

metrics.df <- metrics.df %>%
  toolbox::prep_df() %>%
  mutate(category = if_else(category == "sev", "deg", category)) %>%
  filter(category %in% c("ref", "deg")) %>%
  rename(spatial = "bioregion")

join.df <- right_join( bio.metric.sel, metrics.df, by = "spatial")

cols.vec <- c("spatial", "index_metrics", "event_id", "sample_number",
              "station_id","agency_code", "date", "category")
join.sub <- join.df %>%
  filter(spatial == "blue") %>%
  mutate(category = factor(category))

join.sub <- join.sub[, names(join.sub) %in% c(cols.vec,
                                               unique(unlist(join.sub$index_metrics)))] %>%
  as.data.frame()
i <- unique(unlist(join.sub$index_metrics))[1]
cutpoints.df <- map_df(unique(unlist(join.sub$index_metrics)), function(i) {

  final.df <- data.frame(spatial = i, stringsAsFactors = FALSE)
  final.df$cutpoint <- OptimalCutpoints::optimal.cutpoints(i, "category",
                                                           data = join.sub,
                                                           tag.healthy = "ref",
                                                           methods = "SpEqualSe"
  )$SpEqualSe$Global$optimal.cutoff$cutoff
  return(final.df)
})
# score_metrics <- function(metrics.df, scoring.method,
#                           sensitivity.df, sensitivity.colname, sensitivity.threshold = 0,
#                           first.metric, condition.colname = NULL, ref.cond = NULL)

test <- cutpoints(join.sub)
mmir::score_metrics(join.sub)


library(mmir)
metrics.long <- join.df %>%
  gather(metric, value, -cols.vec)

org.sens.df <-  map_df(unique(metrics.long$spatial), function(spatial.i) {
    metrics.long %>%
      filter(spatial == spatial.i) %>%
    filter(metric %in% unlist(index_metrics)) %>%
    sensitivity(metric, value, category, "ref", "deg") %>%
    mutate(spatial = spatial.i) %>%
    select(spatial, everything())
  })

new.metrics.long <- metrics.long %>%
  anti_join(imp2.df, by = c("event_id",  "sample_number"))

new.sens.df <-  map_df(unique(new.metrics.long$spatial), function(spatial.i) {
  new.metrics.long %>%
    filter(spatial == spatial.i) %>%
    filter(metric %in% unlist(index_metrics)) %>%
    sensitivity(metric, value, category, "ref", "deg") %>%
    mutate(spatial = spatial.i) %>%
    select(spatial, everything())
})


org.sens.df2 <- org.sens.df %>%
  rename(org_bde_thresh = bde_thresh,
         org_bde = bde)

new.sens.df2 <- new.sens.df %>%
  rename(new_bde_thresh = bde_thresh,
         new_bde = bde)

test <- full_join(org.sens.df2, new.sens.df2, by = c("spatial", "metric")) %>%
  mutate(diff_bde = new_bde - org_bde,
         diff_bde_thresh = new_bde_thresh - org_bde_thresh)

prep.df <- prep_sensitivity(metrics.long, metric, value, category, "ref", "deg")

metric.i <- i
map.df <- prep.df %>%
  mutate(thresh = map(metric, function(metric.i) {
    prep.sub <- prep.df  %>%
      filter(metric == metric.i) %>%
      gather(category, value, ref_values:deg_values) %>%
      unnest()

  })
  )
