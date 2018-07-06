## ------------------------------------------------------------------------
cutpoints <- function(x) {
  ref.deg.df <- x %>% 
    filter(category %in% c("ref", "deg")) 
  
  cutpoints.df <- map_df(unique(ref.deg.df$spatial), function(i) {
   # print(i)
    sub.df <- subset(ref.deg.df, spatial == i)
    final.df <- data.frame(spatial = i, stringsAsFactors = FALSE)
    if (sum(sub.df$category == "ref") == 0 | sum(sub.df$category == "deg") == 0) {
      final.df$cutpoint <- as.numeric(NA)
    } else {
      final.df$cutpoint <-
        OptimalCutpoints::optimal.cutpoints(
        final_score ~ category,
        data = sub.df,
        tag.healthy = "ref",
        methods = "SpEqualSe"
        )$SpEqualSe$Global$optimal.cutoff$cutoff[[1]]
    }
    return(final.df)
  })
  
  final.df <- left_join(x, cutpoints.df, by = "spatial")
  
  return(final.df)
}


## ------------------------------------------------------------------------
ce <- function(x) {
  ref.deg.df <- x %>% 
  filter(category %in% c("ref", "deg")) 
  ref.deg.df %>% 
  select(spatial, category, final_score, cutpoint) %>% 
  mutate(class = case_when(
    is.na(cutpoint) ~ "not_applicable",
    category == "ref" & final_score >= cutpoint ~ "tp",
    category == "ref" & final_score < cutpoint ~ "fn",
    category == "deg" & final_score < cutpoint ~ "tn",
    category == "deg" & final_score >= cutpoint ~ "fp",
    TRUE ~ "ERROR"
  )) %>% 
  group_by(spatial, class, cutpoint) %>% 
  summarize(count = n()) %>% 
    ungroup() %>% 
  spread(class, count) %>% 
  mutate(ce = (tp + tn) / (tp + fp + tn + fn) * 100)
}

