### Create graphic output of results

# Create a colour palette for plotting

palette1 <- c("#ef3b2c", # Revenue equivalent inefficiency tax
              "#08519c", # CP 55
              "#9ecae1", # CP 55 + lump sum recycling
              "#41ab5d") # Feebate

### Comparison of three instruments

df <- bind_rows(df_cp55,
                df_ineff_cp55,
                df_cp55_r,
                df_bonus_malus_marginal) %>%
  ungroup() %>%
  mutate(quintile = factor(quintile))

df$legend <- ordered(df$legend,
                     levels = c("Revenue equivalent inefficiency tax",
                                "CP 55",
                                "CP 55 + Lump sum recycling",
                                "Feebate"))

df_ <- df %>% group_by(quintile, legend) %>%
  summarise("mean" = weighted.mean(h_inc.share, w))

# Boxplot (Figure 3)

ggplot(df, aes(y = h_inc.share, x = quintile)) +
  geom_hline(yintercept = 0, linetype="dotted", color = "black", size=0.5) +
  geom_boxplot(aes(weight = w, fill = legend), position = position_dodge(0.9), outlier.shape = NA) +
  geom_point(data = df_, aes(y = mean, group = legend), position = position_dodge(0.9)) +
  coord_cartesian(ylim = c(-3,3)) +
  scale_y_continuous(breaks=seq(-3, 3, by = 1)) +
  scale_x_discrete(labels=c("1\nÄrmste\n20%", "2", "3", "4", "5\nReichste\n20%")) + 
  xlab("Einkommensquintil") +
  theme(legend.position = "bottom") + 
  theme(legend.title = element_blank()) +
  theme(axis.title.y  = element_blank()) +
  scale_fill_manual(values = palette1,
                    labels=c("CP 55" = expression(paste(CO[2]-Preis, " 55€")),
                             "Revenue equivalent inefficiency tax" = expression(paste(CO[2]-Standard)),
                             "CP 55 + Lump sum recycling" = expression(paste(CO[2],"-Preis mit Pauschalrückzahlung")),
                             "Feebate" = "Bonus-Malus-System"))

ggsave(path = "graphs", filename = "main_boxplot_g_eq.pdf", device = "pdf", width = 9, height = 6, units = "in", encoding = "ISOLatin9.enc") #Potentially save the output

# Mean/median lineplot

df_m <- df %>% group_by(quintile, legend) %>%
  summarise("mean" = weighted.mean(h_inc.share, w),
            "median" = matrixStats::weightedMedian(h_inc.share, w, ties = "mean"),
            "sd" = matrixStats::weightedSd(h_inc.share, w),
            "IQR" = matrixStats::iqr(h_inc.share)) %>%
  mutate("CV" = sd/mean) %>%
  pivot_wider(names_from = legend,  values_from =c("median", "mean", "sd", "CV", "IQR"), names_glue = "{legend} {.value}") %>%
  pivot_longer(cols = c(-1))

df_m$name <- ordered(df_m$name, levels = unique(df_m$name))

df_mean <- df_m %>% filter(str_detect(name, "mean"))
df_median <- df_m %>% filter(str_detect(name, "median"))
df_sd <- df_m %>% filter(str_detect(name, "sd"))
df_cv <- df_m %>% filter(str_detect(name, "CV"))
df_iqr <- df_m %>% filter(str_detect(name, "IQR"))

df_mean_ <- ungroup(df_mean) %>%
  mutate(quintile = as.integer(quintile))

# Mean Lineplot (Figure 1)

ggplot(df_mean_, aes( x=quintile, y= value, colour = name)) +
  geom_hline(yintercept = 0, linetype="dotted", color = "black", size=0.5) +
  geom_line(size = 1, alpha = 0.9) +
  geom_point(aes(shape = name), size = 4, alpha = 1) +
  scale_x_continuous(labels=c("1\nÄrmste\n20%", "2", "3", "4", "5\nReichste\n20%")) + 
  xlab("Einkommensquintil") +
  scale_colour_manual(values = palette1,
                      labels=c("CP 55 mean" = expression(paste(CO[2]-Preis," 55€")),
                               "Revenue equivalent inefficiency tax mean" = expression(paste(CO[2]-Standard)),
                               "CP 55 + Lump sum recycling mean" = expression(paste(CO[2],"-Preis mit Pauschalrückzahlung")),
                               "Feebate mean" = "Bonus-Malus-System")) +
  scale_shape_manual(values = c(15,16,17,18),
                     labels=c("CP 55 mean" = expression(paste(CO[2]-Preis, " 55€")),
                              "Revenue equivalent inefficiency tax mean" = expression(paste(CO[2]-Standard)),
                              "CP 55 + Lump sum recycling mean" = expression(paste(CO[2],"-Preis mit Pauschalrückzahlung")),
                              "Feebate mean" = "Bonus-Malus-System")) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank(),
        axis.title.y = element_blank())

ggsave(path = "graphs", filename = "main_mean_g.pdf", device = "pdf", encoding = "ISOLatin9.enc", width = 9, height = 6, units = "in")

#### Mechanism plots

mechanism_df <- dat_MiD %>%
  group_by(H_ID) %>%
  summarise(anzauto = unique(H_ANZAUTO),
            fuel_eff = weighted.mean(fuel_eff, A_JAHRESFL),
            A_JAHRESFL = sum(A_JAHRESFL, na.rm = TRUE),
            inc = unique(aq_eink),
            w = unique(H_HOCH))

mechanism_df <- mechanism_df %>%
  mutate(quintile = cut(inc, MetricsWeighted::weighted_quantile(mechanism_df$inc, mechanism_df$w, seq(0,1,0.2)), include.lowest = TRUE, labels = FALSE))

mechanism_df_w0_ <- mechanism_df %>%
  group_by(quintile) %>%
  summarise(hatauto = weighted.mean(anzauto>0,w),
            anzauto = weighted.mean(anzauto, w),
            fuel_eff = weighted.mean(fuel_eff, w, na.rm = TRUE),
            A_JAHRESFL = weighted.mean(A_JAHRESFL, w),
            inc = weighted.mean(inc, w))

mechanism_df_ <- mechanism_df %>%
  filter(anzauto > 0) %>%
  group_by(quintile) %>%
  summarise(jahresfl_per_inc = weighted.mean(A_JAHRESFL/inc*10, w),
            anzauto_per_inc = weighted.mean(anzauto/inc*10, w),
            anzauto_per_1000_inc = weighted.mean(anzauto/inc*1000, w),
            fl_car = weighted.mean(A_JAHRESFL/anzauto, w),
            anzauto = weighted.mean(anzauto, w),
            fuel_eff = weighted.mean(fuel_eff, w, na.rm = TRUE),
            A_JAHRESFL = weighted.mean(A_JAHRESFL, w),
            inc = weighted.mean(inc, w))

# Fuel Efficiency Boxplot (Figure 2a)

ggplot(mechanism_df, aes(y = fuel_eff, x = factor(quintile))) +
  geom_boxplot(aes(weight = w), outlier.shape = NA) +
  geom_point(data = mechanism_df_, aes(y = fuel_eff)) +
  scale_x_discrete(labels=c("1\nÄrmste\n20%", "2", "3", "4", "5\nReichste\n20%")) + 
  coord_cartesian(ylim = c(4,11)) +
  labs(x = "Einkommensquintil") +
  theme(axis.title.y = element_blank())

ggsave(path = "graphs", filename = "fuel_eff_quintile_box.pdf", device = "pdf", width = 9, height = 6, units = "in")

# Yearly Distance Boxplot (Figure 2b)

ggplot(filter(mechanism_df, anzauto > 0), aes(y = A_JAHRESFL, x = factor(quintile))) +
  geom_boxplot(aes(weight = w), outlier.shape = NA) +
  geom_point(data = mechanism_df_, aes(y = A_JAHRESFL)) +
  scale_x_discrete(labels=c("1\nÄrmste\n20%", "2", "3", "4", "5\nReichste\n20%")) + 
  coord_cartesian(ylim = c(0,60000)) +
  labs(x = "Einkommensquintil") +
  theme(axis.title.y = element_blank())

ggsave(path = "graphs", filename = "jahresfl_quintile_box.pdf", device = "pdf", width = 9, height = 6, units = "in")

### Hybrid and Electric Cars

plot <- dat_MiD %>%
  group_by(H_ID) %>%
  summarise(hybrid = sum((A_ANTRIEB == 4), na.rm = TRUE),
            electric = sum((A_ANTRIEB == 5), na.rm = TRUE),
            inc = unique(aq_eink),
            w = unique(H_HOCH))

plot <-  plot %>%
  mutate(quintile = cut(inc, MetricsWeighted::weighted_quantile(plot$inc, plot$w, seq(0,1,0.2)), include.lowest = TRUE, labels = FALSE)) %>%
  group_by(quintile) %>%
  summarise(hybrid = sum(hybrid*w, na.rm = TRUE),
            electric = sum(electric*w, na.rm = TRUE)) %>%
  gather("type", "number", -quintile) %>%
  mutate(type = ordered(type, levels = c("hybrid", "electric")))

# Figure 4

ggplot(filter(plot, type == "electric"), aes(x = factor(quintile), y = number)) +
  geom_col() +
  scale_x_discrete(labels=c("1\nÄrmste\n20%", "2", "3", "4", "5\nReichste\n20%")) +
  labs(x = "Einkommensquintil") +
  theme(axis.title.y = element_blank())

ggsave(path = "graphs", filename = "eautos_hoch.pdf", device = "pdf", width = 9, height = 6, units = "in")

#####  Driving ban plots

df_A4_ <- df_A4_stack %>%
  group_by(quintile, legend) %>%
  summarise(value = weighted.mean(h_inc.share, w)) %>%
  ungroup()

# Figure 5

ggplot(df_A4_, aes(x=as.numeric(quintile), y= value, colour = legend)) +
  geom_hline(yintercept = 0, linetype="dotted", color = "black", size=0.5) +
  geom_line(size = 1, alpha = 0.9) +
  geom_point(aes(shape = legend), size = 4, alpha = 1) +
  scale_x_continuous(labels=c("1\nÄrmste\n20%", "2", "3", "4", "5\nReichste\n20%")) + 
  xlab("Einkommensquintil") +
  scale_colour_brewer(palette = "Reds",
                      labels= c("Metropolis" = "Großstadtmitte",
                                "Large city" = "Großstadtgebiet",
                                "Medium-sized town" = "Vorstadt",
                                "Urban region" = "Städtischer Raum",
                                "Rural region" = "Ländlicher Raum")) +
  scale_shape_manual(values = c(15,16,17,18,3),
                     labels=c("Metropolis" = "Großstadtmitte",
                              "Large city" = "Großstadtgebiet",
                              "Medium-sized town" = "Vorstadt",
                              "Urban region" = "Städtischer Raum",
                              "Rural region" = "Ländlicher Raum")) +
  theme(legend.position = "bottom") + 
  theme(legend.title = element_blank()) + theme(axis.title.y = element_blank())

ggsave(path = "graphs", filename = "driving_ban_line_0_g.pdf", device = "pdf", width = 9, height = 6, units = "in")


