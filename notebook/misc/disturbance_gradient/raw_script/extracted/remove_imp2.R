## ------------------------------------------------------------------------
org.ce <- cutpoints(bio.fam.df) %>% 
  ce() %>% 
  mutate(org_ref_count = tp + fn,
         org_deg_count = tn +fp) %>% 
  select(spatial, ce, org_ref_count, org_deg_count)  %>% 
  rename(orginal_ce = ce)

## ------------------------------------------------------------------------
imp.ce <- purrr::map_df(c(2, 5, 10), function(i) {
  ref.imp %>% 
    filter(imp_value >= i) %>% 
    anti_join(bio.fam.df, ., by = c("event_id", "sample_number", "final_score")) %>% 
  cutpoints() %>% 
  ce() %>% 
  mutate(new_ref_count = tp + fn,
         new_deg_count = tn + fp) %>% 
  select(spatial, ce, new_ref_count, new_deg_count) %>% 
  rename(new_ce = ce)  %>% 
    mutate(imp_thresh = paste("<", i))
})

## ------------------------------------------------------------------------
comp.df <- full_join(org.ce, imp.ce, by = "spatial") %>% 
  mutate(change_ce = new_ce - orginal_ce,
         change_ref_count = new_ref_count - org_ref_count) %>% 
  mutate_if(is.numeric, funs(round(., 2))) %>% 
  arrange(desc(change_ce)) %>% 
  select(spatial, imp_thresh, org_ref_count, new_ref_count, change_ref_count,
         orginal_ce, new_ce, change_ce) 

## ------------------------------------------------------------------------
comp.df  %>% 
  kableExtra::kable() %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

## ------------------------------------------------------------------------
org.quant <- bio.fam.df %>% 
  filter(category == "ref") %>% 
   group_by(spatial) %>% 
  summarize(org_quant25 = quantile(final_score, 0.25),
            org_quant10 = quantile(final_score, 0.10))

## ------------------------------------------------------------------------
imp.quant <- purrr::map_df(c(2, 5, 10), function(i) {
  ref.imp %>% 
    filter(imp_value >= i) %>% 
    anti_join(bio.fam.df, ., by = c("event_id", "sample_number", "final_score")) %>% 
  filter(category == "ref") %>% 
  group_by(spatial) %>% 
  summarize(new_quant25 = quantile(final_score, 0.25),
            new_quant10 = quantile(final_score, 0.10)) %>% 
    mutate(imp_thresh = paste("<", i))
})

## ------------------------------------------------------------------------
quant.df <- full_join(org.quant, imp.quant, by = "spatial") %>% 
  mutate(diff_25 = new_quant25 - org_quant25,
         diff_10 = new_quant10 - org_quant10) %>% 
  select(spatial, imp_thresh,
         org_quant25, new_quant25, diff_25,
         org_quant10, new_quant10, diff_10) %>% 
  arrange(spatial, imp_thresh)

## ------------------------------------------------------------------------
quant.df  %>% 
  kableExtra::kable() %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

## ---- fig.width = 8, fig.height=25---------------------------------------
bio.fam.ref <- bio.fam.df %>% 
  filter(category == "ref") %>% 
  mutate(group = "original",
         quant_10 = quantile(final_score, 0.1))
i<- 2
test <- purrr::map_df(c(2, 5, 10), function(i) {
  test <- ref.imp %>% 
    filter(imp_value >= i) %>% 
    anti_join(bio.fam.ref, ., by = c("event_id", "sample_number", "final_score")) %>% 
  mutate(group = paste("<", i),
         quant_10 = quantile(final_score, 0.1))
}) %>% 
  bind_rows(bio.fam.ref) %>% 
  mutate(group = factor(group, levels = c("original", "< 10", "< 5", "< 2")))
  

ggplot(test, aes(group, final_score)) +
  geom_boxplot() +
  geom_errorbar(aes(ymin = quant_10, ymax = quant_10),
                width = 0.5,
                color = "red") +
  facet_wrap(~spatial, scale = "free", ncol = 1)
  

