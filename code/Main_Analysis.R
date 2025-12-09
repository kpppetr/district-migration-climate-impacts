# Load all data
library(readr)
library(dplyr)
library(readr)
library(reshape2)
library(tidyr)
require(rgdal)
library(sf)
library(dplyr)
library(data.table)

library(mvtnorm)
library(tidyverse)
library(sandwich)
library(lme4)
library(data.table)
library("arm")
library("foreign")
library(brms)
library(LRTesteR)
library(ggplot2)
library(optimx)
library(car)
library(stargazer)
library(caret)

# load data.csv
data <- readRDS("data.rds")
names(data)


##############################################
# Run OLS regression with random effects
# NET MIGRATION
# single events
##############################################
# create subset of positive and negative net migration


# Create a summary table
library(dplyr)

# Summarize the number of years each district has positive and negative net migration
summary_table <- data %>%
  group_by(GID_2) %>%
  summarise(
    positive_years = sum(log_net_migration > 0),
    negative_years = sum(log_net_migration < 0)
  )

# Filter districts based on the criteria
positive_districts <- summary_table %>%
  filter(positive_years > 14) %>%
  pull(GID_2)

negative_districts <- summary_table %>%
  filter(negative_years > 14) %>%
  pull(GID_2)

# Subset the original data based on the filtered districts
net_pos <- data %>%
  filter(GID_2 %in% positive_districts)

net_neg <- data %>%
  filter(GID_2 %in% negative_districts)


# run the models
summary(data$log_net_migration)
library(lme4)
lm6<-lmer(log_net_migration~ log_net_migration_lag1 +
            fld_share_pop+ drt_share_pop + cyc_share_pop+ 
             log_armed_conflict_brd_lag1+  log_com_conflict_brd_lag1+ log_osv_conflict_brd_lag1 +
             v2x_libdem_lag1 +  log_world_pop_sum_l1 +
            ethnic_status_bin_size_lag1+
             log_gdp_pc_lag1+log_area+
             shdi_lag1+
             (1 | GID_0)+
            (1 | year), data=data)
summary (lm6)

lm6.1<-lmer(log_net_migration~ log_net_migration_lag1 +
              fld_share_pop+ drt_share_pop + cyc_share_pop+
              log_armed_conflict_brd_lag1+  log_com_conflict_brd_lag1+ log_osv_conflict_brd_lag1 +
              v2x_libdem_lag1 +  log_world_pop_sum_l1 +
              ethnic_status_bin_size_lag1+
              log_gdp_pc_lag1+log_area+
              shdi_lag1+
            (1 | GID_0)+
            (1 | year), data=net_neg)


lm6.2<-lmer(log_net_migration~ log_net_migration_lag1 +
             fld_share_pop+ drt_share_pop + cyc_share_pop+
             log_armed_conflict_brd_lag1+  log_com_conflict_brd_lag1+ log_osv_conflict_brd_lag1 +
             v2x_libdem_lag1 +  log_world_pop_sum_l1 +
             ethnic_status_bin_size_lag1+
             log_gdp_pc_lag1+log_area+
             shdi_lag1+
            (1 | GID_0)+
             (1 | year), data=net_pos)
summary (lm6.2)

nice_labels <- c(
  "log_net_migration_lag1"     = "$\\log$(net migration) ($t{-}1$)",
  "fld_share_pop"            = "Flood",
  "drt_share_pop"           = "Drought",
  "cyc_share_pop"                    = "Trop cyclone",
  "log_armed_conflict_brd_lag1" = "$\\log$(armed conflict brd) ($t{-}1$)",
  "log_com_conflict_brd_lag1"  = "$\\log$(non$-$state conflict brd) ($t{-}1$)",
  "log_osv_conflict_brd_lag1"  = "$\\log$(OSV conflict brd) ($t{-}1$)",
  "v2x_libdem_lag1"            = "Liberal democracy ($t{-}1$)",
  "log_world_pop_sum_l1"       = "$\\log$(population) ($t{-}1$)",
  "ethnic_status_bin_size_lag1" = "Ethnic discrimination ($t{-}1$)",
  "log_gdp_pc_lag1"            = "GDP pc ($t{-}1$)",
  "shdi_lag1"                  = "SHDI index ($t{-}1$)",
  "log_area"                   = "$\\log$(area)"
)


library(stargazer)

stargazer(
  lm6, lm6.1, lm6.2,
  type = "latex",
  out = "Table1.tex",
  covariate.labels = nice_labels,
  column.labels = c("Net Migration", "Negative Net Migration", "Positive Net Migration"),
  dep.var.labels.include = FALSE,
  title = "",
  style = "default",
  omit.stat = c("ll", "ser", "f"),
  no.space = TRUE
)

###################################
# PLOTS AND MODEL FIT 
###################################
library(ggplot2)

# FULL SAMPLE 
summary_lm6 <- summary(lm6)

vars <- c(
  "log_net_migration_lag1",
  "fld_share_pop",
  "drt_share_pop",
  "cyc_share_pop",
  "log_armed_conflict_brd_lag1",
  "log_com_conflict_brd_lag1",
  "log_osv_conflict_brd_lag1",
  "v2x_libdem_lag1",
  "log_world_pop_sum_l1",
  "ethnic_status_bin_size_lag1",
  "log_gdp_pc_lag1",
  "log_area",
  "shdi_lag1"
)

# Pull only those rows 
coef_mat <- summary_lm6$coefficients[vars, , drop = FALSE]
all_coefs <- coef_mat[, "Estimate"]
all_ses   <- coef_mat[, "Std. Error"]

# 95% CI
ylo <- all_coefs - 1.96 * all_ses
yhi <- all_coefs + 1.96 * all_ses

# Pretty labels 
labels <- c(
  "Log(net migration) (t-1)",
  "Flood",
  "Drought",
  "Trop cyclone",
  "Log(armed conflict brd) (t-1)",
  "Log(non-state conflict brd) (t-1)",
  "Log(OSV conflict brd) (t-1)",
  "Liberal democracy (t-1)",
  "Log(population) (t-1)",
  "Ethnic discrimination (t-1)",
  "GDP pc (t-1)",
  "Log(area)",
  "SHDI index (t-1)"
)

dfplot <- data.frame(
  name = factor(labels, levels = rev(labels)),
  coef = as.numeric(all_coefs),
  ylo  = as.numeric(ylo),
  yhi  = as.numeric(yhi)
)

# Significance color
dfplot$color_indicator <- ifelse(dfplot$ylo < 0 & dfplot$yhi > 0, "not significant", "significant")


# NEGATIVE NET (lm6.1) 
summary_lm6 <- summary(lm6.1)

coef_mat <- summary_lm6$coefficients[vars, , drop = FALSE]
all_coefs <- coef_mat[, "Estimate"]
all_ses   <- coef_mat[, "Std. Error"]

ylo <- all_coefs - 1.96 * all_ses
yhi <- all_coefs + 1.96 * all_ses

dfplot2 <- data.frame(
  name = factor(labels, levels = rev(labels)),
  coef = as.numeric(all_coefs),
  ylo  = as.numeric(ylo),
  yhi  = as.numeric(yhi)
)
dfplot2$color_indicator <- ifelse(dfplot2$ylo < 0 & dfplot2$yhi > 0, "not significant", "significant")


# POSITIVE NET (lm6.2) 
summary_lm6 <- summary(lm6.2)

coef_mat <- summary_lm6$coefficients[vars, , drop = FALSE]
all_coefs <- coef_mat[, "Estimate"]
all_ses   <- coef_mat[, "Std. Error"]

ylo <- all_coefs - 1.96 * all_ses
yhi <- all_coefs + 1.96 * all_ses

dfplot3 <- data.frame(
  name = factor(labels, levels = rev(labels)),
  coef = as.numeric(all_coefs),
  ylo  = as.numeric(ylo),
  yhi  = as.numeric(yhi)
)
dfplot3$color_indicator <- ifelse(dfplot3$ylo < 0 & dfplot3$yhi > 0, "not significant", "significant")

# Combine plot
library(ggplot2)
library(patchwork)
library(cowplot)

## Panel theme 
nature_theme <- function() {
  theme_minimal(base_family = "Helvetica") +
    theme(
      axis.text.x  = element_text(size = 6),
      axis.text.y  = element_text(size = 6),
      axis.title.x = element_text(size = 7),
      axis.title.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position  = "none",   # panels have no legends
      plot.margin      = margin(3, 3, 3, 3)
    )
}



# p1: full sample
p1 <- ggplot(
  dfplot,
  aes(name, coef, ymin = ylo, ymax = yhi, color = color_indicator)
) +
  geom_pointrange(linewidth = 0.25, fatten = 1.5) +
  scale_color_manual(
    values = c(
      "significant"     = "#377eb8",
      "not significant" = "#999999"
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.25) +
  coord_flip(ylim = c(-1, 2.5)) +
  labs(x = NULL, y = "") +
  nature_theme() +
  ggtitle("a. Net migration")

# p2: negative sample
p2_t <- ggplot(
  dfplot2,
  aes(name, coef, ymin = ylo, ymax = yhi, color = color_indicator)
) +
  geom_pointrange(linewidth = 0.25, fatten = 1.5) +
  scale_color_manual(
    values = c(
      "significant"     = "#377eb8",
      "not significant" = "#999999"
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.25) +
  coord_flip(ylim = c(-1, 2.5)) +
  labs(x = NULL, y = "") +
  nature_theme() +
  theme(
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  ggtitle("b. Negative\nnet migration")

# p3: positive sample
p3_t <- ggplot(
  dfplot3,
  aes(name, coef, ymin = ylo, ymax = yhi, color = color_indicator)
) +
  geom_pointrange(linewidth = 0.25, fatten = 1.5) +
  scale_color_manual(
    values = c(
      "significant"     = "#377eb8",
      "not significant" = "#999999"
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.25) +
  coord_flip(ylim = c(-1, 2.5)) +
  labs(x = NULL, y = "") +
  nature_theme() +
  theme(
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  ggtitle("c. Positive\nnet migration")

## Combine panels 
combined_panels <- p1 + p2_t + p3_t +
  plot_layout(widths = c(1.25, 1, 1)) &
  theme(
    plot.title = element_text(size = 7, face = "bold", hjust = 0)
  )

## shared legend 
legend_df <- data.frame(
  x = 1,
  y = 1,
  color_indicator = factor(
    c("significant", "not significant"),
    levels = c("significant", "not significant")
  )
)

legend_plot <- ggplot(legend_df, aes(x = x, y = y, color = color_indicator)) +
  geom_point(size = 1.5) +
  scale_color_manual(
    name   = NULL,
    values = c(
      "significant"     = "#377eb8",
      "not significant" = "#999999"
    ),
    labels = c(
      "significant"     = "Significant (95% CI)",
      "not significant" = "Not significant (95% CI)"
    )
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 6),
    axis.text        = element_blank(),
    axis.title       = element_blank(),
    axis.ticks       = element_blank(),
    panel.grid       = element_blank()
  )

shared_legend <- cowplot::get_legend(legend_plot)


final_fig <- cowplot::plot_grid(
  combined_panels,
  shared_legend,
  ncol        = 1,
  rel_heights = c(1, 0.12)  
)


w_in <- 180 / 25.4   # 180 mm
h_in <- 80  / 25.4   # 80 mm 

pdf(
  file   = "Fig2.pdf",
  width  = w_in,
  height = h_in,
  family = "Helvetica"
)
print(final_fig)
dev.off()
#####################################################
# With crop failure
####################################################

summary(data$crops_0_dev_neg)

library(lme4)
lm6<-lmer(log_net_migration~ log_net_migration_lag1 +
            fld_share_pop+ drt_share_pop + cyc_share_pop+ 
            crops_0_dev_neg+
            log_armed_conflict_brd_lag1+  log_com_conflict_brd_lag1+ log_osv_conflict_brd_lag1 +
            v2x_libdem_lag1 +  log_world_pop_sum_l1 +
            ethnic_status_bin_size_lag1+
            log_gdp_pc_lag1+log_area+
            shdi_lag1+
            (1 | GID_0)+
            (1 | year), data=data)
summary (lm6)

lm6.1<-lmer(log_net_migration~ log_net_migration_lag1 +
              fld_share_pop+ drt_share_pop + cyc_share_pop+
              crops_0_dev_neg+
              log_armed_conflict_brd_lag1+  log_com_conflict_brd_lag1+ log_osv_conflict_brd_lag1 +
              v2x_libdem_lag1 +  log_world_pop_sum_l1 +
              ethnic_status_bin_size_lag1+
              log_gdp_pc_lag1+log_area+
              shdi_lag1+
              (1 | GID_0)+
              (1 | year), data=net_neg)


lm6.2<-lmer(log_net_migration~ log_net_migration_lag1 +
              fld_share_pop+ drt_share_pop + cyc_share_pop+
              crops_0_dev_neg+
              log_armed_conflict_brd_lag1+  log_com_conflict_brd_lag1+ log_osv_conflict_brd_lag1 +
              v2x_libdem_lag1 +  log_world_pop_sum_l1 +
              ethnic_status_bin_size_lag1+
              log_gdp_pc_lag1+log_area+
              shdi_lag1+
              (1 | GID_0)+
              (1 | year), data=net_pos)
summary (lm6.2)



nice_labels <- c(
  "log_net_migration_lag1"     = "$\\log$(net migration) ($t{-}1$)",
  "fld_share_pop"            = "Flood",
  "drt_share_pop"           = "Drought",
  "cyc_share_pop"                    = "Trop cyclone",
  "crops_0_dev_neg"             = "Crop failure",
  "log_armed_conflict_brd_lag1" = "$\\log$(armed conflict brd) ($t{-}1$)",
  "log_com_conflict_brd_lag1"  = "$\\log$(non$-$state conflict brd) ($t{-}1$)",
  "log_osv_conflict_brd_lag1"  = "$\\log$(OSV conflict brd) ($t{-}1$)",
  "v2x_libdem_lag1"            = "Liberal democracy ($t{-}1$)",
  "log_world_pop_sum_l1"       = "$\\log$(population) ($t{-}1$)",
  "ethnic_status_bin_size_lag1" = "Ethnic discrimination ($t{-}1$)",
  "log_gdp_pc_lag1"            = "GDP pc ($t{-}1$)",
  "shdi_lag1"                  = "SHDI index ($t{-}1$)",
  "log_area"                   = "$\\log$(area)"
)


library(stargazer)

stargazer(
  lm6, lm6.1, lm6.2,
  type = "latex",
  out = "Table1.tex",
  covariate.labels = nice_labels,
  column.labels = c("Net Migration", "Negative Net Migration", "Positive Net Migration"),
  dep.var.labels.include = FALSE,
  title = "",
  style = "default",
  omit.stat = c("ll", "ser", "f"),
  no.space = TRUE
)

###################################
# PLOTS AND MODEL FIT 
###################################
library(ggplot2)

# FULL SAMPLE 
summary_lm6 <- summary(lm6)

vars <- c(
  "log_net_migration_lag1",
  "fld_share_pop",
  "drt_share_pop",
  "cyc_share_pop",
  "crops_0_dev_neg",
  "log_armed_conflict_brd_lag1",
  "log_com_conflict_brd_lag1",
  "log_osv_conflict_brd_lag1",
  "v2x_libdem_lag1",
  "log_world_pop_sum_l1",
  "ethnic_status_bin_size_lag1",
  "log_gdp_pc_lag1",
  "log_area",
  "shdi_lag1"
)

# Pull only those rows 
coef_mat <- summary_lm6$coefficients[vars, , drop = FALSE]
all_coefs <- coef_mat[, "Estimate"]
all_ses   <- coef_mat[, "Std. Error"]

# 95% CI
ylo <- all_coefs - 1.96 * all_ses
yhi <- all_coefs + 1.96 * all_ses

# Pretty labels 
labels <- c(
  "Log(net migration) (t-1)",
  "Flood",
  "Drought",
  "Trop cyclone",
  "Crop failure",
  "Log(armed conflict brd) (t-1)",
  "Log(non-state conflict brd) (t-1)",
  "Log(OSV conflict brd) (t-1)",
  "Liberal democracy (t-1)",
  "Log(population) (t-1)",
  "Ethnic discrimination (t-1)",
  "GDP pc (t-1)",
  "Log(area)",
  "SHDI index (t-1)"
)

dfplot <- data.frame(
  name = factor(labels, levels = rev(labels)),
  coef = as.numeric(all_coefs),
  ylo  = as.numeric(ylo),
  yhi  = as.numeric(yhi)
)

# Significance color
dfplot$color_indicator <- ifelse(dfplot$ylo < 0 & dfplot$yhi > 0, "not significant", "significant")


#  NEGATIVE NET (lm6.1)
summary_lm6 <- summary(lm6.1)

coef_mat <- summary_lm6$coefficients[vars, , drop = FALSE]
all_coefs <- coef_mat[, "Estimate"]
all_ses   <- coef_mat[, "Std. Error"]

ylo <- all_coefs - 1.96 * all_ses
yhi <- all_coefs + 1.96 * all_ses

dfplot2 <- data.frame(
  name = factor(labels, levels = rev(labels)),
  coef = as.numeric(all_coefs),
  ylo  = as.numeric(ylo),
  yhi  = as.numeric(yhi)
)
dfplot2$color_indicator <- ifelse(dfplot2$ylo < 0 & dfplot2$yhi > 0, "not significant", "significant")



#  POSITIVE NET (lm6.2) 
summary_lm6 <- summary(lm6.2)

coef_mat <- summary_lm6$coefficients[vars, , drop = FALSE]
all_coefs <- coef_mat[, "Estimate"]
all_ses   <- coef_mat[, "Std. Error"]

ylo <- all_coefs - 1.96 * all_ses
yhi <- all_coefs + 1.96 * all_ses

dfplot3 <- data.frame(
  name = factor(labels, levels = rev(labels)),
  coef = as.numeric(all_coefs),
  ylo  = as.numeric(ylo),
  yhi  = as.numeric(yhi)
)
dfplot3$color_indicator <- ifelse(dfplot3$ylo < 0 & dfplot3$yhi > 0, "not significant", "significant")

# TITLES & COMBINE 
library(ggplot2)
library(patchwork)
library(cowplot)

## Panel theme 
nature_theme <- function() {
  theme_minimal(base_family = "Helvetica") +
    theme(
      axis.text.x  = element_text(size = 6),
      axis.text.y  = element_text(size = 6),
      axis.title.x = element_text(size = 7),
      axis.title.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position  = "none",   # panels have no legends
      plot.margin      = margin(3, 3, 3, 3)
    )
}



# p1: full sample
p1 <- ggplot(
  dfplot,
  aes(name, coef, ymin = ylo, ymax = yhi, color = color_indicator)
) +
  geom_pointrange(linewidth = 0.25, fatten = 1.5) +
  scale_color_manual(
    values = c(
      "significant"     = "#377eb8",
      "not significant" = "#999999"
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.25) +
  coord_flip(ylim = c(-1, 2.5)) +
  labs(x = NULL, y = "") +
  nature_theme() +
  ggtitle("a. Net migration")

# p2: negative sample
p2_t <- ggplot(
  dfplot2,
  aes(name, coef, ymin = ylo, ymax = yhi, color = color_indicator)
) +
  geom_pointrange(linewidth = 0.25, fatten = 1.5) +
  scale_color_manual(
    values = c(
      "significant"     = "#377eb8",
      "not significant" = "#999999"
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.25) +
  coord_flip(ylim = c(-1, 2.5)) +
  labs(x = NULL, y = "") +
  nature_theme() +
  theme(
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  ggtitle("b. Negative\nnet migration")

# p3: positive sample
p3_t <- ggplot(
  dfplot3,
  aes(name, coef, ymin = ylo, ymax = yhi, color = color_indicator)
) +
  geom_pointrange(linewidth = 0.25, fatten = 1.5) +
  scale_color_manual(
    values = c(
      "significant"     = "#377eb8",
      "not significant" = "#999999"
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.25) +
  coord_flip(ylim = c(-1, 2.5)) +
  labs(x = NULL, y = "") +
  nature_theme() +
  theme(
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  ggtitle("c. Positive\nnet migration")

## Combine panels 
combined_panels <- p1 + p2_t + p3_t +
  plot_layout(widths = c(1.25, 1, 1)) &
  theme(
    plot.title = element_text(size = 7, face = "bold", hjust = 0)
  )

## shared legend 
legend_df <- data.frame(
  x = 1,
  y = 1,
  color_indicator = factor(
    c("significant", "not significant"),
    levels = c("significant", "not significant")
  )
)

legend_plot <- ggplot(legend_df, aes(x = x, y = y, color = color_indicator)) +
  geom_point(size = 1.5) +
  scale_color_manual(
    name   = NULL,
    values = c(
      "significant"     = "#377eb8",
      "not significant" = "#999999"
    ),
    labels = c(
      "significant"     = "Significant (95% CI)",
      "not significant" = "Not significant (95% CI)"
    )
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 6),
    axis.text        = element_blank(),
    axis.title       = element_blank(),
    axis.ticks       = element_blank(),
    panel.grid       = element_blank()
  )

shared_legend <- cowplot::get_legend(legend_plot)


final_fig <- cowplot::plot_grid(
  combined_panels,
  shared_legend,
  ncol        = 1,
  rel_heights = c(1, 0.12)  
)


w_in <- 180 / 25.4   # 180 mm
h_in <- 80  / 25.4   # 80 mm 

pdf(
  file   = "FigS8.pdf",
  width  = w_in,
  height = h_in,
  family = "Helvetica"
)
print(final_fig)
dev.off()


# ##############################################
# Run OLS regression with fixed effects 
##############################################
lm6_fe <- lm(
  log_net_migration ~ 
    log_net_migration_lag1 + 
    fld_share_pop + 
    drt_share_pop + 
    cyc_share_pop +
    log_armed_conflict_brd_lag1 + 
    log_com_conflict_brd_lag1 + 
    log_osv_conflict_brd_lag1 +
    v2x_libdem_lag1 + 
    log_world_pop_sum_l1 +
    ethnic_status_bin_size_lag1 +
    log_gdp_pc_lag1 +
    log_area +
    shdi_lag1 +
    factor(GID_0) + 
    factor(year),
  data = data
)
summary(lm6_fe)

lm6.1_fe <- lm(
  log_net_migration ~ 
    log_net_migration_lag1 + 
    fld_share_pop + 
    drt_share_pop + 
    cyc_share_pop +
    log_armed_conflict_brd_lag1 + 
    log_com_conflict_brd_lag1 + 
    log_osv_conflict_brd_lag1 +
    v2x_libdem_lag1 + 
    log_world_pop_sum_l1 +
    ethnic_status_bin_size_lag1 +
    log_gdp_pc_lag1 +
    log_area +
    shdi_lag1 +
    factor(GID_0) + 
    factor(year),
  data = net_neg
)
summary(lm6.1_fe)

lm6.2_fe <- lm(
  log_net_migration ~ 
    log_net_migration_lag1 + 
    fld_share_pop + 
    drt_share_pop + 
    cyc_share_pop +
    log_armed_conflict_brd_lag1 + 
    log_com_conflict_brd_lag1 + 
    log_osv_conflict_brd_lag1 +
    v2x_libdem_lag1 + 
    log_world_pop_sum_l1 +
    ethnic_status_bin_size_lag1 +
    log_gdp_pc_lag1 +
    log_area +
    shdi_lag1 +
    factor(GID_0) + 
    factor(year),
  data = net_pos
)
summary(lm6.2_fe)

library(stargazer)

stargazer(
  lm6_fe, lm6.1_fe, lm6.2_fe,
  type = "latex",
  out = "Table_FE.tex",
  covariate.labels = nice_labels,        # reuse your earlier label mapping
  omit = c("factor\\(GID_0\\)", "factor\\(year\\)"),  # REGEX to omit all fixed effects
  omit.labels = c("Country FE", "Year FE"),
  omit.stat = c("f", "ser", "adj.rsq"),  # optional: hide extra stats
  column.labels = c("Net Migration", "Negative", "Positive"),
  dep.var.labels.include = FALSE,
  title = "Fixed Effects OLS Models",
  no.space = TRUE,
  float.env = "table"
)

###################################
# PLOTS AND MODEL FIT 
###################################
library(ggplot2)

#  FULL SAMPLE 
summary_lm6 <- summary(lm6_fe)

vars <- c(
  "log_net_migration_lag1",
  "fld_share_pop",
  "drt_share_pop",
  "cyc_share_pop",
  "log_armed_conflict_brd_lag1",
  "log_com_conflict_brd_lag1",
  "log_osv_conflict_brd_lag1",
  "v2x_libdem_lag1",
  "log_world_pop_sum_l1",
  "ethnic_status_bin_size_lag1",
  "log_gdp_pc_lag1",
  "log_area",
  "shdi_lag1"
)

# Pull only those rows 
coef_mat <- summary_lm6$coefficients[vars, , drop = FALSE]
all_coefs <- coef_mat[, "Estimate"]
all_ses   <- coef_mat[, "Std. Error"]

# 95% CI
ylo <- all_coefs - 1.96 * all_ses
yhi <- all_coefs + 1.96 * all_ses

# Pretty labels 
labels <- c(
  "Log(net migration) (t-1)",
  "Flood",
  "Drought",
  "Trop cyclone",
  "Log(armed conflict brd) (t-1)",
  "Log(non-state conflict brd) (t-1)",
  "Log(OSV conflict brd) (t-1)",
  "Liberal democracy (t-1)",
  "Log(population) (t-1)",
  "Ethnic discrimination (t-1)",
  "GDP pc (t-1)",
  "Log(area)",
  "SHDI index (t-1)"
)

dfplot <- data.frame(
  name = factor(labels, levels = rev(labels)),
  coef = as.numeric(all_coefs),
  ylo  = as.numeric(ylo),
  yhi  = as.numeric(yhi)
)

# Significance color
dfplot$color_indicator <- ifelse(dfplot$ylo < 0 & dfplot$yhi > 0, "not significant", "significant")


# NEGATIVE NET (lm6.1) 
summary_lm6 <- summary(lm6.1_fe)

coef_mat <- summary_lm6$coefficients[vars, , drop = FALSE]
all_coefs <- coef_mat[, "Estimate"]
all_ses   <- coef_mat[, "Std. Error"]

ylo <- all_coefs - 1.96 * all_ses
yhi <- all_coefs + 1.96 * all_ses

dfplot2 <- data.frame(
  name = factor(labels, levels = rev(labels)),
  coef = as.numeric(all_coefs),
  ylo  = as.numeric(ylo),
  yhi  = as.numeric(yhi)
)
dfplot2$color_indicator <- ifelse(dfplot2$ylo < 0 & dfplot2$yhi > 0, "not significant", "significant")


#  POSITIVE NET (lm6.2) 
summary_lm6 <- summary(lm6.2_fe)

coef_mat <- summary_lm6$coefficients[vars, , drop = FALSE]
all_coefs <- coef_mat[, "Estimate"]
all_ses   <- coef_mat[, "Std. Error"]

ylo <- all_coefs - 1.96 * all_ses
yhi <- all_coefs + 1.96 * all_ses

dfplot3 <- data.frame(
  name = factor(labels, levels = rev(labels)),
  coef = as.numeric(all_coefs),
  ylo  = as.numeric(ylo),
  yhi  = as.numeric(yhi)
)
dfplot3$color_indicator <- ifelse(dfplot3$ylo < 0 & dfplot3$yhi > 0, "not significant", "significant")


#  TITLES & COMBINE 
library(ggplot2)
library(patchwork)
library(cowplot)

##  Panel theme 
nature_theme <- function() {
  theme_minimal(base_family = "Helvetica") +
    theme(
      axis.text.x  = element_text(size = 6),
      axis.text.y  = element_text(size = 6),
      axis.title.x = element_text(size = 7),
      axis.title.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position  = "none",   # panels have no legends
      plot.margin      = margin(3, 3, 3, 3)
    )
}

## Panels 

# p1: full sample
p1 <- ggplot(
  dfplot,
  aes(name, coef, ymin = ylo, ymax = yhi, color = color_indicator)
) +
  geom_pointrange(linewidth = 0.25, fatten = 1.5) +
  scale_color_manual(
    values = c(
      "significant"     = "#377eb8",
      "not significant" = "#999999"
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.25) +
  coord_flip(ylim = c(-1, 2.5)) +
  labs(x = NULL, y = "") +
  nature_theme() +
  ggtitle("a. Net migration")

# p2: negative sample
p2_t <- ggplot(
  dfplot2,
  aes(name, coef, ymin = ylo, ymax = yhi, color = color_indicator)
) +
  geom_pointrange(linewidth = 0.25, fatten = 1.5) +
  scale_color_manual(
    values = c(
      "significant"     = "#377eb8",
      "not significant" = "#999999"
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.25) +
  coord_flip(ylim = c(-1, 2.5)) +
  labs(x = NULL, y = "") +
  nature_theme() +
  theme(
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  ggtitle("b. Negative\nnet migration")

# p3: positive sample
p3_t <- ggplot(
  dfplot3,
  aes(name, coef, ymin = ylo, ymax = yhi, color = color_indicator)
) +
  geom_pointrange(linewidth = 0.25, fatten = 1.5) +
  scale_color_manual(
    values = c(
      "significant"     = "#377eb8",
      "not significant" = "#999999"
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.25) +
  coord_flip(ylim = c(-1, 2.5)) +
  labs(x = NULL, y = "") +
  nature_theme() +
  theme(
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  ggtitle("c. Positive\nnet migration")

## Combine panels 
combined_panels <- p1 + p2_t + p3_t +
  plot_layout(widths = c(1.25, 1, 1)) &
  theme(
    plot.title = element_text(size = 7, face = "bold", hjust = 0)
  )

## shared legend 
legend_df <- data.frame(
  x = 1,
  y = 1,
  color_indicator = factor(
    c("significant", "not significant"),
    levels = c("significant", "not significant")
  )
)

legend_plot <- ggplot(legend_df, aes(x = x, y = y, color = color_indicator)) +
  geom_point(size = 1.5) +
  scale_color_manual(
    name   = NULL,
    values = c(
      "significant"     = "#377eb8",
      "not significant" = "#999999"
    ),
    labels = c(
      "significant"     = "Significant (95% CI)",
      "not significant" = "Not significant (95% CI)"
    )
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 6),
    axis.text        = element_blank(),
    axis.title       = element_blank(),
    axis.ticks       = element_blank(),
    panel.grid       = element_blank()
  )

shared_legend <- cowplot::get_legend(legend_plot)

## Stack panels 
final_fig <- cowplot::plot_grid(
  combined_panels,
  shared_legend,
  ncol        = 1,
  rel_heights = c(1, 0.12)   # adjust legend height if needed
)


w_in <- 180 / 25.4   
h_in <- 80  / 25.4   

pdf(
  file   = "FigS7.pdf",
  width  = w_in,
  height = h_in,
  family = "Helvetica"
)
print(final_fig)
dev.off()
##########################################
# Fixed effects at the admin 2 year level
###########################################
library(fixest)

lm6_fe2 <- feols(
  log_net_migration ~ 
    log_net_migration_lag1 +  
    fld_share_pop + 
    drt_share_pop + 
    cyc_share_pop + 
    log_armed_conflict_brd_lag1 + 
    log_com_conflict_brd_lag1 + 
    log_osv_conflict_brd_lag1 + 
    v2x_libdem_lag1 + 
    log_world_pop_sum_l1 + 
    ethnic_status_bin_size_lag1 + 
    log_gdp_pc_lag1 + 
    shdi_lag1 | 
    GID_2 + year,
  data = data,
  se = "hetero"  
)
summary(lm6_fe2)


lm6.1_fe2 <- feols(
  log_net_migration ~ 
    log_net_migration_lag1 + 
    fld_share_pop + 
    drt_share_pop + 
    cyc_share_pop +
    log_armed_conflict_brd_lag1 + 
    log_com_conflict_brd_lag1 + 
    log_osv_conflict_brd_lag1 +
    v2x_libdem_lag1 + 
    log_world_pop_sum_l1 +
    ethnic_status_bin_size_lag1 +
    log_gdp_pc_lag1 +
    shdi_lag1 | 
    GID_2 + year,
  data = net_neg
)
summary(lm6.1_fe2)

lm6.2_fe2 <- feols(
  log_net_migration ~ 
    log_net_migration_lag1 + 
    fld_share_pop + 
    drt_share_pop + 
    cyc_share_pop +
    log_armed_conflict_brd_lag1 + 
    log_com_conflict_brd_lag1 + 
    log_osv_conflict_brd_lag1 +
    v2x_libdem_lag1 + 
    log_world_pop_sum_l1 +
    ethnic_status_bin_size_lag1 +
    log_gdp_pc_lag1 +
    shdi_lag1 | 
    GID_2 + year,
  data = net_pos
)
summary(lm6.2_fe2)
# Store the full summary with SEs and clustering
# Get clustered SEs summary
library(ggplot2)
summary_lm6 <- summary(lm6_fe2, vcov = ~GID_2)

# Extract coefficient table 
coefs_table <- summary_lm6$coeftable

# Drop intercept 
if ("(Intercept)" %in% rownames(coefs_table)) {
  coefs_table <- coefs_table[rownames(coefs_table) != "(Intercept)", ]
}

# Extract estimates and SEs
all_coefs <- coefs_table[, "Estimate"]
all_ses   <- coefs_table[, "Std. Error"]

# 95% confidence intervals
ylo <- all_coefs - 1.96 * all_ses
yhi <- all_coefs + 1.96 * all_ses

# Custom variable names 
names <- c(
  "Log(net migration) (t-1)",
  "Flood",
  "Drought",
  "Trop cyclone",
  "Log(armed conflict brd) (t-1)",
  "Log(non-state conflict brd) (t-1)",
  "Log(OSV conflict brd) (t-1)",
  "Liberal democracy (t-1)",
  "Log(population) (t-1)",
  "Ethnic discrimination (t-1)",
  "GDP pc (t-1)",
  "SHDI index (t-1)"
)

# Factor to preserve plotting order
names <- factor(names, levels = rev(names))

# Create dataframe for plotting
dfplot <- data.frame(names, all_coefs, ylo, yhi)

# Add significance flag
dfplot$color_indicator <- ifelse(dfplot$ylo < 0 & dfplot$yhi > 0, "not significant", "significant")

# negative
summary_lm6 <- summary(lm6.1_fe2, vcov = ~GID_2)

# Extract coefficient table 
coefs_table <- summary_lm6$coeftable

# Drop intercept 
if ("(Intercept)" %in% rownames(coefs_table)) {
  coefs_table <- coefs_table[rownames(coefs_table) != "(Intercept)", ]
}

# Extract estimates and SEs
all_coefs <- coefs_table[, "Estimate"]
all_ses   <- coefs_table[, "Std. Error"]

# 95% confidence intervals
ylo <- all_coefs - 1.96 * all_ses
yhi <- all_coefs + 1.96 * all_ses

# Custom variable names 
names <- c(
  "Log(net migration) (t-1)",
  "Flood",
  "Drought",
  "Trop cyclone",
  "Log(armed conflict brd) (t-1)",
  "Log(non-state conflict brd) (t-1)",
  "Log(OSV conflict brd) (t-1)",
  "Liberal democracy (t-1)",
  "Log(population) (t-1)",
  "Ethnic discrimination (t-1)",
  "GDP pc (t-1)",
  "SHDI index (t-1)"
)

# Factor to preserve plotting order
names <- factor(names, levels = rev(names))

# Create dataframe for plotting
dfplot2 <- data.frame(names, all_coefs, ylo, yhi)

# Add significance flag
dfplot2$color_indicator <- ifelse(dfplot2$ylo < 0 & dfplot2$yhi > 0, "not significant", "significant")

# positive
summary_lm6 <- summary(lm6.2_fe2, vcov = ~GID_2)

# Extract coefficient table 
coefs_table <- summary_lm6$coeftable

# Drop intercept 
if ("(Intercept)" %in% rownames(coefs_table)) {
  coefs_table <- coefs_table[rownames(coefs_table) != "(Intercept)", ]
}

# Extract estimates and SEs
all_coefs <- coefs_table[, "Estimate"]
all_ses   <- coefs_table[, "Std. Error"]

# 95% confidence intervals
ylo <- all_coefs - 1.96 * all_ses
yhi <- all_coefs + 1.96 * all_ses

# Custom variable names 
names <- c(
  "Log(net migration) (t-1)",
  "Flood",
  "Drought",
  "Trop cyclone",
  "Log(armed conflict brd) (t-1)",
  "Log(non-state conflict brd) (t-1)",
  "Log(OSV conflict brd) (t-1)",
  "Liberal democracy (t-1)",
  "Log(population) (t-1)",
  "Ethnic discrimination (t-1)",
  "GDP pc (t-1)",
  "SHDI index (t-1)"
)

# Factor to preserve plotting order
names <- factor(names, levels = rev(names))

# Create dataframe for plotting
dfplot3 <- data.frame(names, all_coefs, ylo, yhi)

# Add significance flag
dfplot3$color_indicator <- ifelse(dfplot3$ylo < 0 & dfplot3$yhi > 0, "not significant", "significant")

library(ggplot2)
library(cowplot)

## Panel theme 
nature_theme <- function() {
  theme_minimal(base_family = "Helvetica") +
    theme(
      axis.text.x  = element_text(size = 6),
      axis.text.y  = element_text(size = 6),
      axis.title.x = element_text(size = 7),
      axis.title.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position  = "none",
      plot.margin      = margin(3, 3, 3, 3)
    )
}

## Panels 

# p1: full sample
p1 <- ggplot(
  dfplot,
  aes(x = names, y = all_coefs, ymin = ylo, ymax = yhi, color = color_indicator)
) +
  geom_pointrange(linewidth = 0.25, fatten = 1.5) +
  scale_color_manual(
    values = c(
      "significant"     = "#377eb8",
      "not significant" = "#999999"
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.25) +
  coord_flip(ylim = c(-1, 2.5)) +
  labs(x = NULL, y = "") +
  nature_theme() +
  ggtitle("a. Net migration") +
  theme(plot.title = element_text(size = 7, face = "bold", hjust = 0))

# p2: negative sample
p2_t <- ggplot(
  dfplot2,
  aes(x = names, y = all_coefs, ymin = ylo, ymax = yhi, color = color_indicator)
) +
  geom_pointrange(linewidth = 0.25, fatten = 1.5) +
  scale_color_manual(
    values = c(
      "significant"     = "#377eb8",
      "not significant" = "#999999"
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.25) +
  coord_flip(ylim = c(-1, 2.5)) +
  labs(x = NULL, y = "") +
  nature_theme() +
  theme(
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title   = element_text(size = 7, face = "bold", hjust = 0)
  ) +
  ggtitle("b. Negative\nnet migration")

# p3: positive sample
p3_t <- ggplot(
  dfplot3,
  aes(x = names, y = all_coefs, ymin = ylo, ymax = yhi, color = color_indicator)
) +
  geom_pointrange(linewidth = 0.25, fatten = 1.5) +
  scale_color_manual(
    values = c(
      "significant"     = "#377eb8",
      "not significant" = "#999999"
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.25) +
  coord_flip(ylim = c(-1, 2.5)) +
  labs(x = NULL, y = "") +
  nature_theme() +
  theme(
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title   = element_text(size = 7, face = "bold", hjust = 0)
  ) +
  ggtitle("c. Positive\nnet migration")

## Combine panels

combined_panels <- cowplot::plot_grid(
  p1, p2_t, p3_t,
  nrow       = 1,
  rel_widths = c(1.25, 1, 1),
  align      = "h"
)

## Shared legend 

legend_df <- data.frame(
  x = c(1, 2),
  y = 1,
  color_indicator = factor(
    c("significant", "not significant"),
    levels = c("significant", "not significant")
  )
)

legend_plot <- ggplot(legend_df, aes(x = x, y = y, color = color_indicator)) +
  geom_point(size = 1.5) +
  scale_color_manual(
    name   = NULL,
    values = c(
      "significant"     = "#377eb8",
      "not significant" = "#999999"
    ),
    labels = c(
      "Significant (95% CI)",
      "Not significant (95% CI)"
    )
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 6),
    axis.text        = element_blank(),
    axis.title       = element_blank(),
    axis.ticks       = element_blank(),
    panel.grid       = element_blank()
  )

shared_legend <- cowplot::get_legend(legend_plot)

final_fig <- cowplot::plot_grid(
  combined_panels,
  shared_legend,
  ncol        = 1,
  rel_heights = c(1, 0.12)
)



w_in <- 180 / 25.4   # 180 mm
h_in <- 80  / 25.4   # 80 mm

pdf(
  file   = "FigS9.pdf",
  width  = w_in,
  height = h_in,
  family = "Helvetica"
)
print(final_fig)
dev.off()


#########################################
# Comparing Model Fit  for each climate variable
#########################################
# same variables
vars_all <- c(
  "log_net_migration","log_net_migration_lag1",
  "fld_share_pop","drt_share_pop","cyc_share_pop",
  "log_armed_conflict_brd_lag1","log_com_conflict_brd_lag1","log_osv_conflict_brd_lag1",
  "v2x_libdem_lag1","log_world_pop_sum_l1","ethnic_status_bin_size_lag1",
  "log_gdp_pc_lag1","log_area","shdi_lag1",
  "GID_0","year"
)

# common analysis sample 
data1 <- subset(data, complete.cases(data[ , vars_all]))


lm3.1<-lmer(log_net_migration~ log_net_migration_lag1 +
              log_armed_conflict_brd_lag1+  log_com_conflict_brd_lag1+ log_osv_conflict_brd_lag1 +
              v2x_libdem_lag1 +  log_world_pop_sum_l1 +
              ethnic_status_bin_size_lag1+
              log_gdp_pc_lag1+log_area+
              shdi_lag1+
              (1 | GID_0)
            + (1 | year), data=data1, REML = FALSE)
summary (lm3.1)

lm3.2<-lmer(log_net_migration~ log_net_migration_lag1 +
              drt_share_pop+
              log_armed_conflict_brd_lag1+  log_com_conflict_brd_lag1+ log_osv_conflict_brd_lag1 +
              v2x_libdem_lag1 +  log_world_pop_sum_l1 +
              ethnic_status_bin_size_lag1+
              log_gdp_pc_lag1+log_area+
              shdi_lag1+
             (1 | GID_0)
          + (1 | year), data=data1, REML = FALSE)
summary (lm3.2)

lm3.3<-lmer(log_net_migration~ log_net_migration_lag1 +
              fld_share_pop +
              log_armed_conflict_brd_lag1+  log_com_conflict_brd_lag1+ log_osv_conflict_brd_lag1 +
              v2x_libdem_lag1 +  log_world_pop_sum_l1 +
              ethnic_status_bin_size_lag1+
              log_gdp_pc_lag1+log_area+
              shdi_lag1+
             (1 | GID_0)
          + (1 | year), data=data1, REML = FALSE)
summary (lm3.3)

lm3.4<-lmer(log_net_migration~ log_net_migration_lag1 +
              cyc_share_pop+
              log_armed_conflict_brd_lag1+  log_com_conflict_brd_lag1+ log_osv_conflict_brd_lag1 +
              v2x_libdem_lag1 +  log_world_pop_sum_l1 +
              ethnic_status_bin_size_lag1+
              log_gdp_pc_lag1+log_area+
              shdi_lag1+
             (1 | GID_0)
          + (1 | year), data=data1, REML = FALSE)
summary (lm3.4)



lm3.5<-lmer(log_net_migration~ log_net_migration_lag1+
              fld_share_pop + drt_share_pop + cyc_share_pop+
              log_armed_conflict_brd_lag1+  log_com_conflict_brd_lag1+ log_osv_conflict_brd_lag1 +
              v2x_libdem_lag1 +  log_world_pop_sum_l1 +
              ethnic_status_bin_size_lag1+
              log_gdp_pc_lag1+log_area+
              shdi_lag1+
              (1 | GID_0)
            + (1 | year), data=data1, REML = FALSE)


aic_lm3_3 <- AIC(lm3.3) # flood
aic_lm3_4 <- AIC(lm3.4) # cyclone 
aic_lm3_1 <- AIC(lm3.1) # no climate
aic_lm3_2 <- AIC(lm3.2) # drought
aic_lm3_5 <- AIC(lm3.5) # all
         

# Load
library(ggplot2)
library(reshape2)

# AICs (lower is better)
aic_values <- c(2381841, 2381849, 2381856, 2381857, 2381861)

# Labels 
new_labels <- c(
  "Climate variables + SEP",                   # lm3.5 (all hazards + controls)
  "Trop cyc + SEP",                            # lm3.4
  "Flood + SEP",                               # lm3.3
  "Drought + SEP",                             # lm3.2
  "Socio-economic & pol variables (SEP)"       # lm3.1 (controls only)
)

# Model names 
model_names <- c("lm3.5", "lm3.4", "lm3.3", "lm3.2", "lm3.1")

# Build df
aic_df <- data.frame(
  Model = model_names,
  AIC   = aic_values,
  Label = new_labels,
  stringsAsFactors = FALSE
)

# Desired display order for labels
desired_order <- c(
  "Climate variables + SEP",
  "Trop cyc + SEP",
  "Flood + SEP",
  "Drought + SEP",
  "Socio-economic & pol variables (SEP)"
)

# Keep Label as factor only for ordering in plots, but sort the data by AIC
aic_df$Label <- factor(aic_df$Label, levels = desired_order)

# Order rows from best to worst AIC
aic_df <- aic_df[order(aic_df$AIC), ]

# Store the sorted labels as character
new_labels_sorted <- as.character(aic_df$Label)

# Comparison matrix of AIC differences
compare_aic <- function(aic, labels_chr) {
  n <- length(aic)
  m <- matrix(0, nrow = n, ncol = n)
  rownames(m) <- labels_chr
  colnames(m) <- labels_chr
  for (i in 1:n) for (j in 1:n) m[i, j] <- aic[i] - aic[j]
  m
}

aic_matrix <- compare_aic(aic_df$AIC, new_labels_sorted)

# Long format
melted_aic <- reshape2::melt(aic_matrix, varnames = c("Model1","Model2"), value.name = "AIC_Difference")

colnames(melted_aic) <- c("Model1", "Model2", "AIC_Difference")

# Keep only upper triangle 
ord_idx <- setNames(seq_along(new_labels_sorted), new_labels_sorted)
aic_long_filtered <- subset(
  melted_aic,
  Model1 != Model2 & ord_idx[as.character(Model1)] < ord_idx[as.character(Model2)]
)

# Factors for axes with desired order
aic_long_filtered$Model1 <- factor(aic_long_filtered$Model1,
                                   levels = rev(new_labels_sorted))
aic_long_filtered$Model2 <- factor(aic_long_filtered$Model2,
                                   levels = new_labels_sorted)

# Plot
p <- ggplot(aic_long_filtered, aes(x = Model2, y = Model1, fill = AIC_Difference)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "#f3e5f5", midpoint = 0,
    name = "AIC Difference\n(row − column)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title  = element_blank(),
    legend.position = "left",
    legend.direction = "vertical"
  ) +
  labs(
    title = "AIC Differences Between Models",
    subtitle = ""
  ) +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10,
                               title.position = "top", title.hjust = 0.5))

print(p)

p_aic <- p +
  theme(
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    plot.margin = margin(t = 10, r = 20, b = 20, l = 10)
  )
ggsave("aic_differences_heatmap.pdf", p, width = 10, height = 7)


##########################################
# 100 fold corss validation RMSE
###########################################

# Load necessary packages
library(lme4)
library(caret)
library(ggplot2)
library(dplyr)
library(ggpubr)

# Function to perform 100-fold cross-validation and calculate RMSE
perform_100fold_cv_rmse <- function(data, formula) {
  set.seed(123) # For reproducibility
  
  # Split data into 100 folds 
  folds <- createFolds(data$log_net_migration, k = 100, list = TRUE, returnTrain = TRUE)
  
  rmse_list <- numeric(100) # To store RMSE for each fold
  
  for (i in seq_along(folds)) {
    # Separate the data into training and testing based on the folds
    train_indices <- folds[[i]]
    test_indices <- setdiff(seq_len(nrow(data)), train_indices)
    
    train_data <- data[train_indices, ]
    test_data <- data[test_indices, ]
    
    # Fit the model on the training data
    model <- lmer(formula, data = train_data)
    
    # Predict on the test data
    predictions <- predict(model, newdata = test_data, re.form = NA)
    
    # Calculate RMSE for the fold
    rmse_list[i] <- sqrt(mean((predictions - test_data$log_net_migration)^2))
  }
  
  # Return the RMSE across all folds
  list(RMSE = rmse_list)
}

# Perform 100-fold CV 
cv_results1 <- perform_100fold_cv_rmse(data1, log_net_migration ~ log_net_migration_lag1 +
                                        log_armed_conflict_brd_lag1+  log_com_conflict_brd_lag1+ log_osv_conflict_brd_lag1 +
                                        v2x_libdem_lag1 +  log_world_pop_sum_l1 +
                                        ethnic_status_bin_size_lag1+
                                        log_gdp_pc_lag1+log_area+
                                        shdi_lag1 + (1 | GID_0) + (1 | year))

cv_results2 <- perform_100fold_cv_rmse(data1, log_net_migration ~ log_net_migration_lag1 +
                                        drt_share_pop + log_armed_conflict_brd_lag1+  log_com_conflict_brd_lag1+ log_osv_conflict_brd_lag1 +
                                        v2x_libdem_lag1 +  log_world_pop_sum_l1 +
                                        ethnic_status_bin_size_lag1+
                                        log_gdp_pc_lag1+log_area+
                                        shdi_lag1 + (1 | GID_0) + (1 | year))

cv_results3 <- perform_100fold_cv_rmse(data1, log_net_migration ~ log_net_migration_lag1 +
                                        fld_share_pop + log_armed_conflict_brd_lag1+  log_com_conflict_brd_lag1+ log_osv_conflict_brd_lag1 +
                                        v2x_libdem_lag1 +  log_world_pop_sum_l1 +
                                        ethnic_status_bin_size_lag1+
                                        log_gdp_pc_lag1+log_area+
                                        shdi_lag1 + (1 | GID_0) + (1 | year))

cv_results4 <- perform_100fold_cv_rmse(data1, log_net_migration ~ log_net_migration_lag1 +
                                        cyc_share_pop + log_armed_conflict_brd_lag1+  log_com_conflict_brd_lag1+ log_osv_conflict_brd_lag1 +
                                        v2x_libdem_lag1 +  log_world_pop_sum_l1 +
                                        ethnic_status_bin_size_lag1+
                                        log_gdp_pc_lag1+log_area+
                                        shdi_lag1 + (1 | GID_0) + (1 | year))

cv_results5 <- perform_100fold_cv_rmse(data1, log_net_migration ~ log_net_migration_lag1 +
                                        fld_share_pop + drt_share_pop + cyc_share_pop +
                                        log_armed_conflict_brd_lag1+  log_com_conflict_brd_lag1+ log_osv_conflict_brd_lag1 +
                                        v2x_libdem_lag1 +  log_world_pop_sum_l1 +
                                        ethnic_status_bin_size_lag1+
                                        log_gdp_pc_lag1+log_area+
                                        shdi_lag1 + (1 | GID_0) + (1 | year))


# Combine results for plotting
rmse_results <- data.frame(
  Model = rep(c("Baseline", "Drought", "Flood", "Trop cyc", "Full climate"), each = 10),
  RMSE = c(cv_results1$RMSE, cv_results2$RMSE, cv_results3$RMSE, cv_results4$RMSE, cv_results5$RMSE)
)


# Plotting RMSE results with boxplot
library(patchwork)
library(ggplot2)

# Update RMSE plot
p_rmse <- ggplot(rmse_results, aes(x = Model, y = RMSE, fill = Model)) +
  geom_boxplot(width = 0.7, outlier.size = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2, fill = "white") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Model performance (RMSE)",   # <- new short title
    y = "RMSE", 
    x = ""
  ) +
  guides(fill = "none") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    axis.text.x = element_text(size = 14, face = "plain"),
    axis.text.y = element_text(size = 13),
    axis.title.y = element_text(size = 15),
    plot.margin = margin(t = 10, r = 20, b = 20, l = 10)
  )

# zoom RMSE range 
p_rmse <- p_rmse + coord_cartesian(ylim = c(2.405, 2.415))

#  tidy AIC plot 
p_aic <- p_aic +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    plot.margin = margin(t = 10, r = 20, b = 20, l = 10)
  )

#  Combine both panels 
combined_plot <- p_aic + p_rmse +
  plot_layout(widths = c(1, 1.3)) +
  plot_annotation(
    tag_levels = 'a', 
    tag_suffix = '.', 
    theme = theme(
      plot.tag = element_text(size = 26, face = "bold", hjust = 0, vjust = 1.5)  # larger a. / b.
    )
  )

#  Save the figure 
ggsave(
  "aic_and_rmse_combined.pdf",
  plot = combined_plot,
  width = 16,   # wider
  height = 7,   # taller
  dpi = 300
)



