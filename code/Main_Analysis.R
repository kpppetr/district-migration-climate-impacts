# Load all data
setwd("C:/Users/kristina/Nextcloud/Documents/Paper 1 Climate mobility/Analysis/Data prep/data")
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

# GDP transformation
library(dplyr)
data <- data %>%
  arrange(Country, NAME_2, year) %>%  # sort before lagging
  group_by(Country, NAME_2) %>%
  mutate(
    log_gdp_pc = log(gdp_pc_kumm),              # log at time t
    log_gdp_pc_lag1 = lag(log_gdp_pc, n = 1)     # log at time t-1
  ) %>%
  ungroup()


# run the models
names(data$crops_20pctneg)
library(lme4)
lm6<-lmer(log_net_migration~ log_net_migration_lag1 +
            flood_threshold+ soilmoist_contin + top_cyc+ crops_20pctneg+
             log_armed_conflict_brd_lag1+  log_com_conflict_brd_lag1+ log_osv_conflict_brd_lag1 +
             v2x_libdem_lag1 +  log_world_pop_sum_l1 +
            ethnic_status_bin_size_lag1+
             log_gdp_pc_lag1+log_area+
             shdi_lag1+
             (1 | GID_0)+
            (1 | year), data=data)
summary (lm6)

lm6.1<-lmer(log_net_migration~ log_net_migration_lag1 +
              flood_threshold+ soilmoist_contin + top_cyc+crops_20pctneg+
              log_armed_conflict_brd_lag1+  log_com_conflict_brd_lag1+ log_osv_conflict_brd_lag1 +
              v2x_libdem_lag1 +  log_world_pop_sum_l1 +
              ethnic_status_bin_size_lag1+
              log_gdp_pc_lag1+log_area+
              shdi_lag1+
            (1 | GID_0)+
            (1 | year), data=net_neg)


lm6.2<-lmer(log_net_migration~ log_net_migration_lag1 +
             flood_threshold+ soilmoist_contin + top_cyc+crops_20pctneg+
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
  "flood_threshold"            = "Flood",
  "soilmoist_contin"           = "Drought",
  "top_cyc"                    = "Trop cyclone",
  "crops_20pctneg"             = "Crop failure",
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



# ##############################################
# Run OLS regression with fixed effects 
##############################################
lm6_fe <- lm(
  log_net_migration ~ 
    log_net_migration_lag1 + 
    flood_threshold + 
    soilmoist_contin + 
    top_cyc +
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
    flood_threshold + 
    soilmoist_contin + 
    top_cyc +
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
    flood_threshold + 
    soilmoist_contin + 
    top_cyc +
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
'PLOTS AND MODEL FIT 
#####################################
# Extract summary
summary_lm6 <- summary(lm6_fe)

# Drop intercept and extract coefficients + SEs
all_coefs <- summary_lm6$coefficients[-1, 1]
all_ses   <- summary_lm6$coefficients[-1, 2]

# 95% CI
ylo <- all_coefs - 1.96 * all_ses
yhi <- all_coefs + 1.96 * all_ses

# Update names in the correct order:
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
  "Log(area)",                 # log_area now before SHDI
  "SHDI index (t-1)"
)

# Factor to preserve plotting order
names <- factor(names, levels = rev(names))

# Create plot df
dfplot <- data.frame(names, all_coefs, ylo, yhi)

# Significance color
dfplot$color_indicator <- ifelse(dfplot$ylo < 0 & dfplot$yhi > 0, "not significant", "significant")

# Plot
library(ggplot2)
p <- ggplot(dfplot, aes(x = names, y = all_coefs, ymin = ylo, ymax = yhi, color = color_indicator)) +
  geom_pointrange(size = 1.2, fatten = 4) +
  scale_color_manual(values = c("significant" = "#377eb8", "not significant" = "#999999")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip(ylim = c(-0.5, 2.5)) +  # Set y-axis limits
  theme_bw() +
  labs(x = NULL, y = "") +  # No y-axis title
  theme(
    axis.text.x = element_text(size = 19),
    axis.text.y = element_text(size = 19),        # No bold
    axis.title.x = element_text(size = 22),
    axis.title.y = element_blank(),               # Ensure y title is gone
    legend.position = "none"
  )

print(p)


# negative net
# Extract summary
summary_lm6 <- summary(lm6.1_fe)

# Drop intercept and extract coefficients + SEs
all_coefs <- summary_lm6$coefficients[-1, 1]
all_ses   <- summary_lm6$coefficients[-1, 2]

# 95% CI
ylo <- all_coefs - 1.96 * all_ses
yhi <- all_coefs + 1.96 * all_ses

# Update names in the correct order:
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
  "Log(area)",                 # log_area now before SHDI
  "SHDI index (t-1)"
)

# Factor to preserve plotting order
names <- factor(names, levels = rev(names))

# Create plot df
dfplot <- data.frame(names, all_coefs, ylo, yhi)

# Significance color
dfplot$color_indicator <- ifelse(dfplot$ylo < 0 & dfplot$yhi > 0, "not significant", "significant")

# Plot
# Step 1: Create p2 with all labels and themes
p2 <- ggplot(dfplot, aes(x = names, y = all_coefs, ymin = ylo, ymax = yhi, color = color_indicator)) +
  geom_pointrange(size = 1.2, fatten = 4) +
  scale_color_manual(values = c("significant" = "#377eb8", "not significant" = "#999999")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip(ylim = c(-0.5, 2.5)) +  # Set y-axis limits
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


# positive 
summary_lm6 <- summary(lm6.2_fe)

# Drop intercept and extract coefficients + SEs
all_coefs <- summary_lm6$coefficients[-1, 1]
all_ses   <- summary_lm6$coefficients[-1, 2]

# 95% CI
ylo <- all_coefs - 1.96 * all_ses
yhi <- all_coefs + 1.96 * all_ses

# Update names in the correct order:
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
  "Log(area)",                 # log_area now before SHDI
  "SHDI index (t-1)"
)

# Factor to preserve plotting order
names <- factor(names, levels = rev(names))

# Create plot df
dfplot <- data.frame(names, all_coefs, ylo, yhi)

# Significance color
dfplot$color_indicator <- ifelse(dfplot$ylo < 0 & dfplot$yhi > 0, "not significant", "significant")

# Plot
p3 <- ggplot(dfplot, aes(x = names, y = all_coefs, ymin = ylo, ymax = yhi, color = color_indicator)) +
  geom_pointrange(size = 1.2, fatten = 4) +
  scale_color_manual(values = c("significant" = "#377eb8", "not significant" = "#999999")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip(ylim = c(-0.5, 2.5)) +  # Set y-axis limits
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

ggsave("fig2_cr.pdf", plot = combined_plot, width = 12, height = 8)

##########################################
# Fixed effects at the admin 2 year level
###########################################
library(fixest)

lm6_fe2 <- feols(
  log_net_migration ~ 
    log_net_migration_lag1 +  
    flood_threshold + 
    soilmoist_contin + 
    top_cyc + 
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
  se = "hetero"  # <--- this is critical
)
summary(lm6_fe2)


lm6.1_fe2 <- feols(
  log_net_migration ~ 
    log_net_migration_lag1 + 
    flood_threshold + 
    soilmoist_contin + 
    top_cyc +
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
    flood_threshold + 
    soilmoist_contin + 
    top_cyc +
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
library(ggplot)
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
ylo <- all_coefs - 1.645 * all_ses
yhi <- all_coefs + 1.645 * all_ses

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
  coord_flip(ylim = c(-0.5, 2.5)) +
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
ylo <- all_coefs - 1.645 * all_ses
yhi <- all_coefs + 1.645 * all_ses

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
  coord_flip(ylim = c(-0.5, 2.5)) +  # Set y-axis limits
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
ylo <- all_coefs - 1.645 * all_ses
yhi <- all_coefs + 1.645 * all_ses

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
# Plot
p3 <- ggplot(dfplot, aes(x = names, y = all_coefs, ymin = ylo, ymax = yhi, color = color_indicator)) +
  geom_pointrange(size = 1.2, fatten = 4) +
  scale_color_manual(values = c("significant" = "#377eb8", "not significant" = "#999999")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip(ylim = c(-0.5, 2.5)) +  # Set y-axis limits
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

lm3.1<-lmer(log_net_migration~ log_net_migration_lag1 +
              log_armed_conflict_brd_lag1+  log_com_conflict_brd_lag1+ log_osv_conflict_brd_lag1 +
              v2x_libdem_lag1+  log_world_pop_sum_l1 +
              ethnic_status_bin_size_lag1+
              log_gdp_pc_lag1+
              shdi_lag1+
              (1 | GID_0)
            + (1 | year), data=data1)
summary (lm3.1)

lm3.2<-lmer(log_net_migration~ log_net_migration_lag1 +
              soilmoist_contin+
              log_armed_conflict_brd_lag1+  log_com_conflict_brd_lag1+ log_osv_conflict_brd_lag1 +
              v2x_libdem_lag1+  log_world_pop_sum_l1 +
              ethnic_status_bin_size_lag1+
              log_gdp_pc_lag1+
              shdi_lag1+
             (1 | GID_0)
          + (1 | year), data=data1)
summary (lm3.2)

lm3.3<-lmer(log_net_migration~ log_net_migration_lag1 +
              flood_threshold +
              log_armed_conflict_brd_lag1+  log_com_conflict_brd_lag1+ log_osv_conflict_brd_lag1 +
              v2x_libdem_lag1+  log_world_pop_sum_l1 +
              ethnic_status_bin_size_lag1+
              log_gdp_pc_lag1+
              shdi_lag1+
             (1 | GID_0)
          + (1 | year), data=data1)
summary (lm3.3)

lm3.4<-lmer(log_net_migration~ log_net_migration_lag1 +
              top_cyc+
              log_armed_conflict_brd_lag1+  log_com_conflict_brd_lag1+ log_osv_conflict_brd_lag1 +
              v2x_libdem_lag1+  log_world_pop_sum_l1 +
              ethnic_status_bin_size_lag1+
              log_gdp_pc_lag1+
              shdi_lag1+
             (1 | GID_0)
          + (1 | year), data=data1)
summary (lm3.4)



lm3.5<-lmer(log_net_migration~ log_net_migration_lag1+
              flood_threshold + soilmoist_contin + top_cyc+
              log_armed_conflict_brd_lag1+  log_com_conflict_brd_lag1+ log_osv_conflict_brd_lag1 +
              v2x_libdem_lag1+  log_world_pop_sum_l1 +
              ethnic_status_bin_size_lag1+
              log_gdp_pc_lag1+
              shdi_lag1+
              (1 | GID_0)
            + (1 | year), data=data1)


aic_lm3_3 <- AIC(lm3.3)
aic_lm3_4 <- AIC(lm3.4)
aic_lm3_1 <- AIC(lm3.1)
aic_lm3_2 <- AIC(lm3.2)
aic_lm3_5 <- AIC(lm3.5)
         

# Load necessary libraries
library(reshape2)
library(ggplot2)

# Extract AIC values
aic_values <- c(2520636, 2520640, 2520659, 2520661, 2520668)

# New labels for the models
new_labels <- c("Flood + SEP",
                 "Climate variables + SEP",
                "Trop cyc + SEP",
                "Socio-economic & pol variables (SEP)",
                "Drought + SEP")

# Correct the model names to be strings
model_names <- c("lm3.3", "lm3.6", "lm3.4", "lm3.1", "lm3.2")

# Create a data frame with model names, AIC values, and new labels
aic_df <- data.frame(Model = model_names, AIC = aic_values, Label = new_labels, stringsAsFactors = FALSE)

# Ensure that the desired order is maintained
desired_order <- c("Flood + SEP",
                   "Climate variables + SEP",
                   "Trop cyc + SEP",
                   "Socio-economic & pol variables (SEP)",
                   "Drought + SEP")

aic_df$Label <- factor(aic_df$Label, levels = desired_order)

# Order the data frame by AIC values from best to worst
aic_df <- aic_df[order(aic_df$AIC),]

# Update the new labels in the sorted order
new_labels_sorted <- aic_df$Label

# Create a comparison matrix with differences in AIC values
compare_aic <- function(aic, labels) {
  n <- length(aic)
  m <- matrix(0, nrow = n, ncol = n)
  rownames(m) <- labels
  colnames(m) <- labels
  for (i in 1:n) {
    for (j in 1:n) {
      m[i, j] <- aic[i] - aic[j]
    }
  }
  return(m)
}

# Create the comparison matrix
aic_matrix <- compare_aic(aic_df$AIC, new_labels_sorted)

# Convert the matrix to long format
melted_aic <- melt(aic_matrix)
colnames(melted_aic) <- c("Model1", "Model2", "AIC_Difference")

# Filtering the data: Keep only entries from the upper triangle
aic_long_filtered <- melted_aic[melted_aic$Model1 != melted_aic$Model2 & match(melted_aic$Model1, new_labels_sorted) < match(melted_aic$Model2, new_labels_sorted),]

# Ensure Model1 and Model2 are factors with levels matching the sorted new_labels
aic_long_filtered$Model1 <- factor(aic_long_filtered$Model1, levels = rev(new_labels_sorted))
aic_long_filtered$Model2 <- factor(aic_long_filtered$Model2, levels = new_labels_sorted)

# Print the filtered data to check
print(aic_long_filtered)

# Plotting the heatmap with ggplot2 using the filtered dataset
plot <- ggplot(aic_long_filtered, aes(x = Model2, y = Model1, fill = AIC_Difference)) +
  geom_tile(color = "white") +  # Adding a white border to each tile for better visibility
  scale_fill_gradient2(low = "blue", high = "red", mid = "#f3e5f5", midpoint = 0,  # Change midpoint color to a lighter purple
                       name = "AIC Difference") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Increase X axis label size
        axis.text.y = element_text(size = 14),  # Increase Y axis label size
        axis.title = element_blank(),
        legend.position = "left",  # Position the legend vertically on the left
        legend.direction = "vertical") +  # Make the legend vertical
  labs(title = "AIC Differences Between Models",
       x = "Model",
       y = "Model") +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10,  # Adjust dimensions for a vertical legend
                               title.position = "top", title.hjust = 0.5))

# Save the plot as a PDF
ggsave("aic_differences_heatmap.pdf", plot, width = 10, height = 7)  # Adjust width and height as needed


##########################################
# 100 fold corss validation RMSE
###########################################

# Load necessary packages
library(lme4)
library(caret)
library(ggplot2)
library(dplyr)
library(ggpubr)

# Function to perform 10-fold cross-validation and calculate RMSE
perform_10fold_cv_rmse <- function(data, formula) {
  set.seed(123) # For reproducibility
  
  # Split data into 10 folds using createFolds
  folds <- createFolds(data$log_net_migration, k = 10, list = TRUE, returnTrain = TRUE)
  
  rmse_list <- numeric(10) # To store RMSE for each fold
  
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

# Perform 10-fold CV on your data
cv_results1 <- perform_10fold_cv_rmse(data1, log_net_migration ~ log_net_migration_lag1 +
                                        log_armed_conflict_brd_lag1 + log_com_conflict_brd_lag1 + log_osv_conflict_brd_lag1 +
                                        v2x_libdem_lag1 + log_world_pop_sum_l1 +
                                        ethnic_status_bin_size_lag1 + log_gdp_pc_lag1 +
                                        shdi_lag1 + (1 | GID_0) + (1 | year))

cv_results2 <- perform_10fold_cv_rmse(data1, log_net_migration ~ log_net_migration_lag1 +
                                        soilmoist_contin + log_armed_conflict_brd_lag1 + log_com_conflict_brd_lag1 + log_osv_conflict_brd_lag1 +
                                        v2x_libdem_lag1 + log_world_pop_sum_l1 +
                                        ethnic_status_bin_size_lag1 + log_gdp_pc_lag1 +
                                        shdi_lag1 + (1 | GID_0) + (1 | year))

cv_results3 <- perform_10fold_cv_rmse(data1, log_net_migration ~ log_net_migration_lag1 +
                                        flood_threshold + log_armed_conflict_brd_lag1 + log_com_conflict_brd_lag1 + log_osv_conflict_brd_lag1 +
                                        v2x_libdem_lag1 + log_world_pop_sum_l1 +
                                        ethnic_status_bin_size_lag1 + log_gdp_pc_lag1 +
                                        shdi_lag1 + (1 | GID_0) + (1 | year))

cv_results4 <- perform_10fold_cv_rmse(data1, log_net_migration ~ log_net_migration_lag1 +
                                        top_cyc + log_armed_conflict_brd_lag1 + log_com_conflict_brd_lag1 + log_osv_conflict_brd_lag1 +
                                        v2x_libdem_lag1 + log_world_pop_sum_l1 +
                                        ethnic_status_bin_size_lag1 + log_gdp_pc_lag1 +
                                        shdi_lag1 + (1 | GID_0) + (1 | year))

cv_results5 <- perform_10fold_cv_rmse(data1, log_net_migration ~ log_net_migration_lag1 +
                                        flood_threshold + soilmoist_contin + top_cyc +
                                        log_armed_conflict_brd_lag1 + log_com_conflict_brd_lag1 + log_osv_conflict_brd_lag1 +
                                        v2x_libdem_lag1 + log_world_pop_sum_l1 +
                                        ethnic_status_bin_size_lag1 + log_gdp_pc_lag1 +
                                        shdi_lag1 + (1 | GID_0) + (1 | year))


# Combine results for plotting
rmse_results <- data.frame(
  Model = rep(c("Baseline", "Drought", "Flood", "Trop cyc", "Full climate"), each = 10),
  RMSE = c(cv_results1$RMSE, cv_results2$RMSE, cv_results3$RMSE, cv_results4$RMSE, cv_results5$RMSE)
)

rmse_results <- data.frame(
  Model = c("Baseline", "Drought", "Flood", "Trop cyc", "Full climate"),
  mean_RMSE = c(2.36, 2.371956, 2.371940, 2.371936, 2.371960, 2.39)
)

# Perform paired t-tests between models
model_pairs <- combn(unique(rmse_results$Model), 2, simplify = FALSE)
p_values <- sapply(model_pairs, function(pair) {
  t.test(rmse_results$RMSE[rmse_results$Model == pair[1]], rmse_results$RMSE[rmse_results$Model == pair[2]], paired = TRUE)$p.value
})

# Adjust p-values for multiple comparisons using Bonferroni correction
adjusted_p_values <- p.adjust(p_values, method = "bonferroni")
names(adjusted_p_values) <- sapply(model_pairs, function(pair) paste(pair, collapse = " vs "))

# Print adjusted p-values
print(adjusted_p_values)



rmse_values$Model <- factor(rmse_values$Model, levels = c("Baseline", "Trop cyc", "Drought", "Flood", "Crop", "Full climate"))

# Plotting RMSE results with boxplot
ggplot(rmse_results, aes(x = Model, y = RMSE, fill = Model)) +
  geom_boxplot() +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  theme_minimal() +
  labs(title = "", y = "RMSE", x = "") +
  guides(fill = guide_legend(title = "Model"))

# mean RMSE
mean_rmse <- rmse_results %>%
  group_by(Model) %>%
  summarise(mean_RMSE = mean(RMSE))

# Print the mean RMSE for each model
print(mean_rmse)


