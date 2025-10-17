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
library(readr)
data <- read_csv("data.csv")
names(data)


##############################################
# Run OLS regression with random effects
# NET MIGRATION
# single events
##############################################
# create subset of positive and negative net migration


# Step 1: Create a summary table
library(dplyr)

# Summarize the number of years each district has positive and negative net migration
summary_table <- data %>%
  group_by(GID_2) %>%
  summarise(
    positive_years = sum(log_net_migration > 0),
    negative_years = sum(log_net_migration < 0)
  )

# Step 2: Filter districts based on the criteria
positive_districts <- summary_table %>%
  filter(positive_years > 14) %>%
  pull(GID_2)

negative_districts <- summary_table %>%
  filter(negative_years > 14) %>%
  pull(GID_2)

# Step 3: Subset the original data based on the filtered districts
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

# ---------- FULL SAMPLE ----------
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

# Pull only those rows (drop the intercept and any factor dummies)
coef_mat <- summary_lm6$coefficients[vars, , drop = FALSE]
all_coefs <- coef_mat[, "Estimate"]
all_ses   <- coef_mat[, "Std. Error"]

# 95% CI
ylo <- all_coefs - 1.96 * all_ses
yhi <- all_coefs + 1.96 * all_ses

# Pretty labels (same order as vars)
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

# Plot
p <- ggplot(dfplot, aes(x = name, y = coef, ymin = ylo, ymax = yhi, color = color_indicator)) +
  geom_pointrange(size = 1.2, fatten = 4) +
  scale_color_manual(values = c("significant" = "#377eb8", "not significant" = "#999999")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip(ylim = c(-1, 2.5)) +  # adjust if needed
  theme_bw() +
  labs(x = NULL, y = "") +
  theme(
    axis.text.x = element_text(size = 19),
    axis.text.y = element_text(size = 19),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_blank(),
    legend.position = "none"
  )

print(p)

# ---------- NEGATIVE NET (lm6.1) ----------
summary_lm6 <- summary(lm6.1)

coef_mat <- summary_lm6$coefficients[vars, , drop = FALSE]
all_coefs <- coef_mat[, "Estimate"]
all_ses   <- coef_mat[, "Std. Error"]

ylo <- all_coefs - 1.96 * all_ses
yhi <- all_coefs + 1.96 * all_ses

dfplot <- data.frame(
  name = factor(labels, levels = rev(labels)),
  coef = as.numeric(all_coefs),
  ylo  = as.numeric(ylo),
  yhi  = as.numeric(yhi)
)
dfplot$color_indicator <- ifelse(dfplot$ylo < 0 & dfplot$yhi > 0, "not significant", "significant")

p2 <- ggplot(dfplot, aes(x = name, y = coef, ymin = ylo, ymax = yhi, color = color_indicator)) +
  geom_pointrange(size = 1.2, fatten = 4) +
  scale_color_manual(values = c("significant" = "#377eb8", "not significant" = "#999999")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip(ylim = c(-1, 2.5)) +
  theme_bw() +
  labs(x = NULL, y = "Estimated Coefficients") +
  theme(
    axis.text.x = element_text(size = 19),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_blank(),
    legend.position = "none"
  )

print(p2)

# ---------- POSITIVE NET (lm6.2) ----------
summary_lm6 <- summary(lm6.2)

coef_mat <- summary_lm6$coefficients[vars, , drop = FALSE]
all_coefs <- coef_mat[, "Estimate"]
all_ses   <- coef_mat[, "Std. Error"]

ylo <- all_coefs - 1.96 * all_ses
yhi <- all_coefs + 1.96 * all_ses

dfplot <- data.frame(
  name = factor(labels, levels = rev(labels)),
  coef = as.numeric(all_coefs),
  ylo  = as.numeric(ylo),
  yhi  = as.numeric(yhi)
)
dfplot$color_indicator <- ifelse(dfplot$ylo < 0 & dfplot$yhi > 0, "not significant", "significant")

p3 <- ggplot(dfplot, aes(x = name, y = coef, ymin = ylo, ymax = yhi, color = color_indicator)) +
  geom_pointrange(size = 1.2, fatten = 4) +
  scale_color_manual(values = c("significant" = "#377eb8", "not significant" = "#999999")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip(ylim = c(-1, 2.5)) +
  theme_bw() +
  labs(x = NULL, y = "") +
  theme(
    axis.text.x = element_text(size = 19),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_blank(),
    legend.position = "none"
  )

print(p3)

# ---------- TITLES & COMBINE ----------
p  <- p  + ggtitle("a. Net Migration")
p2 <- p2 + ggtitle("b. Negative\nNet Migration")
p3 <- p3 + ggtitle("c. Positive\nNet Migration")

library(patchwork)
combined_plot <- p + p2 + p3 + 
  plot_layout(ncol = 3, widths = c(1.2, 1, 1)) & 
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )

ggsave("fig2.pdf", plot = combined_plot, width = 12, height = 8)

#####################################################
# With crop failure
####################################################

summary(data$crops_1_dev_neg)

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

# ---------- FULL SAMPLE ----------
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

# Pull only those rows (drop the intercept and any factor dummies)
coef_mat <- summary_lm6$coefficients[vars, , drop = FALSE]
all_coefs <- coef_mat[, "Estimate"]
all_ses   <- coef_mat[, "Std. Error"]

# 95% CI
ylo <- all_coefs - 1.96 * all_ses
yhi <- all_coefs + 1.96 * all_ses

# Pretty labels (same order as vars)
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

# Plot
p <- ggplot(dfplot, aes(x = name, y = coef, ymin = ylo, ymax = yhi, color = color_indicator)) +
  geom_pointrange(size = 1.2, fatten = 4) +
  scale_color_manual(values = c("significant" = "#377eb8", "not significant" = "#999999")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip(ylim = c(-1, 2.5)) +  # adjust if needed
  theme_bw() +
  labs(x = NULL, y = "") +
  theme(
    axis.text.x = element_text(size = 19),
    axis.text.y = element_text(size = 19),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_blank(),
    legend.position = "none"
  )

print(p)

#  NEGATIVE NET (lm6.1)
summary_lm6 <- summary(lm6.1)

coef_mat <- summary_lm6$coefficients[vars, , drop = FALSE]
all_coefs <- coef_mat[, "Estimate"]
all_ses   <- coef_mat[, "Std. Error"]

ylo <- all_coefs - 1.96 * all_ses
yhi <- all_coefs + 1.96 * all_ses

dfplot <- data.frame(
  name = factor(labels, levels = rev(labels)),
  coef = as.numeric(all_coefs),
  ylo  = as.numeric(ylo),
  yhi  = as.numeric(yhi)
)
dfplot$color_indicator <- ifelse(dfplot$ylo < 0 & dfplot$yhi > 0, "not significant", "significant")

p2 <- ggplot(dfplot, aes(x = name, y = coef, ymin = ylo, ymax = yhi, color = color_indicator)) +
  geom_pointrange(size = 1.2, fatten = 4) +
  scale_color_manual(values = c("significant" = "#377eb8", "not significant" = "#999999")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip(ylim = c(-1, 2.5)) +
  theme_bw() +
  labs(x = NULL, y = "Estimated Coefficients") +
  theme(
    axis.text.x = element_text(size = 19),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_blank(),
    legend.position = "none"
  )

print(p2)

#  POSITIVE NET (lm6.2) 
summary_lm6 <- summary(lm6.2)

coef_mat <- summary_lm6$coefficients[vars, , drop = FALSE]
all_coefs <- coef_mat[, "Estimate"]
all_ses   <- coef_mat[, "Std. Error"]

ylo <- all_coefs - 1.96 * all_ses
yhi <- all_coefs + 1.96 * all_ses

dfplot <- data.frame(
  name = factor(labels, levels = rev(labels)),
  coef = as.numeric(all_coefs),
  ylo  = as.numeric(ylo),
  yhi  = as.numeric(yhi)
)
dfplot$color_indicator <- ifelse(dfplot$ylo < 0 & dfplot$yhi > 0, "not significant", "significant")

p3 <- ggplot(dfplot, aes(x = name, y = coef, ymin = ylo, ymax = yhi, color = color_indicator)) +
  geom_pointrange(size = 1.2, fatten = 4) +
  scale_color_manual(values = c("significant" = "#377eb8", "not significant" = "#999999")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip(ylim = c(-1, 2.5)) +
  theme_bw() +
  labs(x = NULL, y = "") +
  theme(
    axis.text.x = element_text(size = 19),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_blank(),
    legend.position = "none"
  )

print(p3)

# TITLES & COMBINE 
p  <- p  + ggtitle("a. Net Migration")
p2 <- p2 + ggtitle("b. Negative\nNet Migration")
p3 <- p3 + ggtitle("c. Positive\nNet Migration")

library(patchwork)
combined_plot <- p + p2 + p3 + 
  plot_layout(ncol = 3, widths = c(1.2, 1, 1)) & 
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )

ggsave("fig2_cr.pdf", plot = combined_plot, width = 12, height = 8)


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

# Pull only those rows (drop the intercept and any factor dummies)
coef_mat <- summary_lm6$coefficients[vars, , drop = FALSE]
all_coefs <- coef_mat[, "Estimate"]
all_ses   <- coef_mat[, "Std. Error"]

# 95% CI
ylo <- all_coefs - 1.96 * all_ses
yhi <- all_coefs + 1.96 * all_ses

# Pretty labels (same order as vars)
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

# Plot
p <- ggplot(dfplot, aes(x = name, y = coef, ymin = ylo, ymax = yhi, color = color_indicator)) +
  geom_pointrange(size = 1.2, fatten = 4) +
  scale_color_manual(values = c("significant" = "#377eb8", "not significant" = "#999999")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip(ylim = c(-1, 2.5)) +  # adjust if needed
  theme_bw() +
  labs(x = NULL, y = "") +
  theme(
    axis.text.x = element_text(size = 19),
    axis.text.y = element_text(size = 19),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_blank(),
    legend.position = "none"
  )

print(p)

# NEGATIVE NET (lm6.1) 
summary_lm6 <- summary(lm6.1_fe)

coef_mat <- summary_lm6$coefficients[vars, , drop = FALSE]
all_coefs <- coef_mat[, "Estimate"]
all_ses   <- coef_mat[, "Std. Error"]

ylo <- all_coefs - 1.96 * all_ses
yhi <- all_coefs + 1.96 * all_ses

dfplot <- data.frame(
  name = factor(labels, levels = rev(labels)),
  coef = as.numeric(all_coefs),
  ylo  = as.numeric(ylo),
  yhi  = as.numeric(yhi)
)
dfplot$color_indicator <- ifelse(dfplot$ylo < 0 & dfplot$yhi > 0, "not significant", "significant")

p2 <- ggplot(dfplot, aes(x = name, y = coef, ymin = ylo, ymax = yhi, color = color_indicator)) +
  geom_pointrange(size = 1.2, fatten = 4) +
  scale_color_manual(values = c("significant" = "#377eb8", "not significant" = "#999999")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip(ylim = c(-1, 2.5)) +
  theme_bw() +
  labs(x = NULL, y = "Estimated Coefficients") +
  theme(
    axis.text.x = element_text(size = 19),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_blank(),
    legend.position = "none"
  )

print(p2)

#  POSITIVE NET (lm6.2) 
summary_lm6 <- summary(lm6.2_fe)

coef_mat <- summary_lm6$coefficients[vars, , drop = FALSE]
all_coefs <- coef_mat[, "Estimate"]
all_ses   <- coef_mat[, "Std. Error"]

ylo <- all_coefs - 1.96 * all_ses
yhi <- all_coefs + 1.96 * all_ses

dfplot <- data.frame(
  name = factor(labels, levels = rev(labels)),
  coef = as.numeric(all_coefs),
  ylo  = as.numeric(ylo),
  yhi  = as.numeric(yhi)
)
dfplot$color_indicator <- ifelse(dfplot$ylo < 0 & dfplot$yhi > 0, "not significant", "significant")

p3 <- ggplot(dfplot, aes(x = name, y = coef, ymin = ylo, ymax = yhi, color = color_indicator)) +
  geom_pointrange(size = 1.2, fatten = 4) +
  scale_color_manual(values = c("significant" = "#377eb8", "not significant" = "#999999")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip(ylim = c(-1, 2.5)) +
  theme_bw() +
  labs(x = NULL, y = "") +
  theme(
    axis.text.x = element_text(size = 19),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_blank(),
    legend.position = "none"
  )

print(p3)

#  TITLES & COMBINE 
p  <- p  + ggtitle("a. Net Migration")
p2 <- p2 + ggtitle("b. Negative\nNet Migration")
p3 <- p3 + ggtitle("c. Positive\nNet Migration")

library(patchwork)
combined_plot <- p + p2 + p3 + 
  plot_layout(ncol = 3, widths = c(1.2, 1, 1)) & 
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )

ggsave("fig2_fe.pdf", plot = combined_plot, width = 12, height = 8)


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

# Extract coefficient table (this contains SEs, t-stats, p-values)
coefs_table <- summary_lm6$coeftable

# Drop intercept if present
if ("(Intercept)" %in% rownames(coefs_table)) {
  coefs_table <- coefs_table[rownames(coefs_table) != "(Intercept)", ]
}

# Extract estimates and SEs
all_coefs <- coefs_table[, "Estimate"]
all_ses   <- coefs_table[, "Std. Error"]

# 90% confidence intervals
ylo <- all_coefs - 1.96 * all_ses
yhi <- all_coefs + 1.96 * all_ses

# Custom variable names (ordered to match model)
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

# Plot
p <- ggplot(dfplot, aes(x = names, y = all_coefs, ymin = ylo, ymax = yhi, color = color_indicator)) +
  geom_pointrange(size = 1.2, fatten = 4) +
  scale_color_manual(values = c("significant" = "#377eb8", "not significant" = "#999999")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip(ylim = c(-1.5, 2.5)) +
  theme_bw() +
  labs(x = NULL, y = "") +
  theme(
    axis.text.x = element_text(size = 19),
    axis.text.y = element_text(size = 19),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_blank(),
    legend.position = "none"
  )

print(p)

summary_lm6 <- summary(lm6.1_fe2, vcov = ~GID_2)

# Extract coefficient table (this contains SEs, t-stats, p-values)
coefs_table <- summary_lm6$coeftable

# Drop intercept if present
if ("(Intercept)" %in% rownames(coefs_table)) {
  coefs_table <- coefs_table[rownames(coefs_table) != "(Intercept)", ]
}

# Extract estimates and SEs
all_coefs <- coefs_table[, "Estimate"]
all_ses   <- coefs_table[, "Std. Error"]

# 95% confidence intervals
ylo <- all_coefs - 1.96 * all_ses
yhi <- all_coefs + 1.96 * all_ses

# Custom variable names (ordered to match model)
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

# Plot
p2 <- ggplot(dfplot, aes(x = names, y = all_coefs, ymin = ylo, ymax = yhi, color = color_indicator)) +
  geom_pointrange(size = 1.2, fatten = 4) +
  scale_color_manual(values = c("significant" = "#377eb8", "not significant" = "#999999")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip(ylim = c(-1.5, 2.5)) +  # Set y-axis limits
  theme_bw() +
  labs(x = NULL, y = "Estimated Coefficients") +
  theme(
    axis.text.x = element_text(size = 19),
    axis.text.y = element_blank(),               # <-- remove y labels
    axis.ticks.y = element_blank(),              # <-- remove y ticks
    axis.title.x = element_text(size = 22),
    axis.title.y = element_blank(),              # <-- remove y-axis title too
    legend.position = "none"
  )

print(p2)

summary_lm6 <- summary(lm6.2_fe2, vcov = ~GID_2)

# Extract coefficient table (this contains SEs, t-stats, p-values)
coefs_table <- summary_lm6$coeftable

# Drop intercept if present
if ("(Intercept)" %in% rownames(coefs_table)) {
  coefs_table <- coefs_table[rownames(coefs_table) != "(Intercept)", ]
}

# Extract estimates and SEs
all_coefs <- coefs_table[, "Estimate"]
all_ses   <- coefs_table[, "Std. Error"]

# 95% confidence intervals
ylo <- all_coefs - 1.96 * all_ses
yhi <- all_coefs + 1.96 * all_ses

# Custom variable names (ordered to match model)
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

# Plot

p3 <- ggplot(dfplot, aes(x = names, y = all_coefs, ymin = ylo, ymax = yhi, color = color_indicator)) +
  geom_pointrange(size = 1.2, fatten = 4) +
  scale_color_manual(values = c("significant" = "#377eb8", "not significant" = "#999999")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip(ylim = c(-1.5, 2.5)) +  # Set y-axis limits
  theme_bw() +
  labs(x = NULL, y = "") +
  theme(
    axis.text.x = element_text(size = 19),
    axis.text.y = element_blank(),               # <-- remove y labels
    axis.ticks.y = element_blank(),              # <-- remove y ticks
    axis.title.x = element_text(size = 22),
    axis.title.y = element_blank(),              # <-- remove y-axis title too
    legend.position = "none"
  )

print(p3)



p <- p + ggtitle("a. Net Migration")
p2 <- p2 + ggtitle("b. Negative\nNet Migration")
p3 <- p3 + ggtitle("c. Positive\nNet Migration")

library(patchwork)

combined_plot <- p + p2 + p3 + 
  plot_layout(ncol = 3, widths = c(1.2, 1, 1)) & 
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )

ggsave("fig2_admin2_fe.pdf", plot = combined_plot, width = 12, height = 8)


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

# Labels (human-readable) mapped to models in the same order
new_labels <- c(
  "Climate variables + SEP",                   # lm3.5 (all hazards + controls)
  "Trop cyc + SEP",                            # lm3.4
  "Flood + SEP",                               # lm3.3
  "Drought + SEP",                             # lm3.2
  "Socio-economic & pol variables (SEP)"       # lm3.1 (controls only)
)

# Model names (optional)
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

# Comparison matrix of AIC differences: AIC_i - AIC_j
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
  
  # Split data into 100 folds using createFolds
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
  Model = rep(c("Baseline", "Drought", "Flood", "Trop cyc", "Full climate"), each = 100),
  RMSE = c(cv_results1$RMSE, cv_results2$RMSE, cv_results3$RMSE, cv_results4$RMSE, cv_results5$RMSE)
)

mse_values$Model <- factor(rmse_values$Model, levels = c("Baseline", "Trop cyc", "Drought", "Flood", "Crop", "Full climate"))

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

# zoom RMSE range for better visibility 
p_rmse <- p_rmse + coord_cartesian(ylim = c(2.405, 2.415))

#  tidy AIC plot text margins
p_aic <- p_aic +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    plot.margin = margin(t = 10, r = 20, b = 20, l = 10)
  )

#  Combine both panels with patchwork 
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



