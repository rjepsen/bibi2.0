## ------------------------------------------------------------------------
bioregions.vec <- unique(bio.fam.ref$spatial)[!is.na(unique(bio.fam.ref$spatial))]

## ---- fig.width=8, fig.height=15-----------------------------------------
plot.features <- function(x, y, wrapby, title) {
  ggplot(x, aes_string("final_score", y), na.rm = TRUE) +
    geom_point(aes(color = Classification), alpha = 0.5) +
    geom_smooth(method = "lm", formula = y~x, na.rm = TRUE) +
    facet_wrap(as.formula(paste("~", wrapby)), scale = "free", ncol = 4) +
    ggtitle(toupper(title)) +
    theme(plot.title = element_text(hjust = 0.5))
}

## ------------------------------------------------------------------------
feat.wq <- left_join(bio.fam.ref, wq.df, by = "event_id")

## ---- fig.width=8, fig.height=10-----------------------------------------
wq.plots <- purrr::map(bioregions.vec, function(spatial.i) {
  test2 <- feat.wq %>%
    filter(spatial == spatial.i,
           !is.na(wq_value)) %>%
    plot.features("wq_value", "wq_param", spatial.i)
}) %>%
  set_names(bioregions.vec)

wq.plots

## ------------------------------------------------------------------------
feat.hab <- left_join(bio.fam.ref, habitat.df, by = c("event_id", "sample_number"))

## ---- fig.width=8, fig.height=10-----------------------------------------
hab.plots <- purrr::map(bioregions.vec, function(spatial.i) {
  test2 <- feat.hab %>%
    filter(spatial == spatial.i,
           !is.na(hab_value)) %>%
    plot.features("hab_value", "hab_param", spatial.i)
}) %>%
  set_names(bioregions.vec)

hab.plots

## ------------------------------------------------------------------------
feat.method <- left_join(bio.fam.ref, method.df, by = c("event_id", "sample_number"))

## ---- fig.width=8, fig.height=5------------------------------------------
method.plots <- purrr::map(bioregions.vec, function(spatial.i) {
  test2 <- feat.method %>%
    filter(spatial == spatial.i,
           !is.na(method_value)) %>%
    ggplot(aes_string("final_score", "method_value"), na.rm = TRUE) +
    geom_jitter(aes(color = Classification), alpha = 0.5, width = 0.2) +
    facet_wrap(as.formula(paste("~", "method_param")), scale = "free", ncol = 4) +
    ggtitle(toupper(spatial.i)) +
    theme(plot.title = element_text(hjust = 0.5))
}) %>%
  set_names(bioregions.vec)

method.plots

## ------------------------------------------------------------------------
feat.scat <- left_join(bio.fam.ref, scat.df, by = "comid")

## ------------------------------------------------------------------------
feat.scat <- feat.scat %>% 
  filter(!is.na(scat_value)) %>% 
  group_by(spatial, scat_param) %>% 
  mutate(iqr = IQR(scat_value) * 1.5,
         quant_25 = quantile(scat_value, 0.25),
         quant_75 = quantile(scat_value, 0.75),
         upper_thresh = quant_75 + iqr,
         lower_thresh = quant_25 - iqr,
         outlier = case_when(
           scat_value > upper_thresh ~ TRUE,
           scat_value < lower_thresh ~ TRUE,
           TRUE ~ FALSE
         )) %>% 
  ungroup()

## ---- fig.width=12, fig.height=150, message=FALSE, warning=FALSE---------

scat.plots <- purrr::map(bioregions.vec, function(spatial.i) {
  test2 <- feat.scat %>%
    filter(spatial == spatial.i,
           !is.na(scat_value)) %>%
    group_by(scat_param) %>%
    mutate(count = n(),
           zero_count = sum(scat_value == 0)) %>%
    ungroup() %>%
    filter(count != zero_count) %>%
    # plot.features("scat_value", "scat_param", spatial.i) +
    ggplot(aes(final_score, scat_value), na.rm = TRUE) +
    geom_point(aes(color = Classification, shape = outlier, size = outlier), alpha = 0.5) +
    geom_smooth(method = "lm", formula = y~x, na.rm = TRUE) +
    facet_wrap(~scat_param, scale = "free", ncol = 4) +
    ggtitle(toupper(spatial.i)) +
    theme(plot.title = element_text(hjust = 0.5))
    
}) %>%
  set_names(bioregions.vec)

scat.plots


