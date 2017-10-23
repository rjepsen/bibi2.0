## ---- fig.width=8, fig.height=8------------------------------------------

point.plot <- ggplot(ref.imp, aes(final_score, imp_value, color = ref_cat)) +
  geom_point(alpha = 0.25) +
  scale_color_manual(values = c("EGF" = "#56B4E9",
                               "PVP" = "#E69F00"),
                     name = "Rating") +
  geom_hline(aes(yintercept = 2), linetype = "dashed") +
  geom_label(aes(-5, 2, label = "2%"), color = "black") +
    geom_hline(aes(yintercept = 5), linetype = "dashed") +
  geom_label(aes(-5, 5, label = "5%"), color = "black") +
  geom_hline(aes(yintercept = 10), linetype = "dashed") +
  geom_label(aes(-5, 10, label = "10%"), color = "black") +
  # facet_wrap(~ref_cat) +
  ylab("Impervious Cover (%)") +
  xlab("BIBI Score")

box.plot <- ggplot(ref.imp, aes(ref_cat, imp_value, color = ref_cat)) +
  
  geom_jitter(alpha = 0.25) +
    scale_color_manual(values = c("EGF" = "#56B4E9",
                               "PVP" = "#E69F00"),
                     name = "Rating") +
  geom_boxplot(outlier.alpha = 0, alpha = 0, color = "black") +
  geom_hline(aes(yintercept = 2), linetype = "dashed") +
  geom_label(aes(0.5, 2, label = "2%"), color = "black") +
    geom_hline(aes(yintercept = 5), linetype = "dashed") +
  geom_label(aes(0.5, 5, label = "5%"), color = "black") +
  geom_hline(aes(yintercept = 10), linetype = "dashed") +
  geom_label(aes(0.5, 10, label = "10%"), color = "black") +
  ylab("Impervious Cover (%)") +
  xlab("BIBI Score")

cowplot::plot_grid(point.plot, box.plot, ncol = 1, labels = "auto")

## ---- fig.width=8, fig.height=10-----------------------------------------
ref.imp %>% 
  left_join(bio.fam.df, by = c("event_id", "sample_number", "final_score")) %>% 
ggplot(aes(final_score, imp_value, color = ref_cat)) +
  geom_point() +
  scale_color_manual(values = c("EGF" = "#56B4E9",
                               "PVP" = "#E69F00"),
                     name = "Rating") +
  geom_hline(aes(yintercept = 2), linetype = "dashed") +
  geom_label(aes(-5, 2, label = "2%"), color = "black") +
    geom_hline(aes(yintercept = 5), linetype = "dashed") +
  geom_label(aes(-5, 5, label = "5%"), color = "black") +
  geom_hline(aes(yintercept = 10), linetype = "dashed") +
  geom_label(aes(-5, 10, label = "10%"), color = "black") +
  xlim(-10, 100) +
  facet_wrap(~spatial, ncol = 2, scales = "free_y") +
  ylab("Impervious Cover (%)") +
  xlab("BIBI Score")

## ---- fig.width=8, fig.height=10-----------------------------------------
ref.imp %>% 
  left_join(bio.fam.df, by = c("event_id", "sample_number", "final_score")) %>% 
ggplot(aes(ref_cat, imp_value, color = ref_cat, fill = ref_cat)) +
  geom_violin(draw_quantiles = c(0.5), color = "black") +
  # scale_color_manual(values = c("EGF" = "#56B4E9",
  #                              "PVP" = "#E69F00"),
  #                    name = "Rating") +
  scale_fill_manual(values = c("EGF" = "#56B4E9",
                               "PVP" = "#E69F00"),
                     name = "Rating") +
  geom_hline(aes(yintercept = 2), linetype = "dashed") +
  geom_label(aes(0.6, 2, label = "2%"), color = "black", fill = "white", alpha = 0.5) +
  facet_wrap(~spatial, ncol = 2, scales = "free") +
  ylab("Impervious Cover (%)") +
  xlab("BIBI Score")

