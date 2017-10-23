## ------------------------------------------------------------------------
imp.long <- imp.bibi.df %>% 
  select(spatial, category, event_id, sample_number, date, agency_code, final_score,
         pctimp2001ws, pctimp2006ws, pctimp2011ws) %>% 
  gather(imp, imp_value, pctimp2001ws:pctimp2011ws) %>% 
  filter(!is.na(imp_value),
         !category == "mix") %>% 
  mutate(category = factor(category, levels = c("ref", "min", "mod", "deg")))

## ------------------------------------------------------------------------
library(RColorBrewer)
imp.long %>% 
ggplot(aes(final_score, imp_value, color = category)) +
  geom_point(alpha = 0.25) +
  scale_colour_manual(values = rev(brewer.pal(4, "Spectral"))) +
  facet_wrap(~category) + 
  ylab("Impervious Cover (%)")  +
  xlab("Chessie BIBI Score")

## ------------------------------------------------------------------------

imp.long %>% 
ggplot(aes(category, imp_value, color = category)) +
  geom_boxplot() +
  scale_colour_manual(values = rev(brewer.pal(4, "Spectral"))) +
  ylab("Impervious Cover (%)")  +
  xlab("Disturbance Gradient")

## ---- fig.width=8, fig.height=10-----------------------------------------
imp.long %>% 
ggplot(aes(category, imp_value, color = category)) +
  geom_boxplot() +
  scale_colour_manual(values = rev(brewer.pal(4, "Spectral"))) +
  facet_wrap(~spatial, ncol = 2, scales = "free")+
  ylab("Impervious Cover (%)") +
  xlab("Disturbance Gradient")

