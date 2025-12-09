######################################################################
# Run BVAR model for single lag flood, drought and tropical cyclones
######################################################################
data <- readRDS("data.rds")

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


data_climate<-subset(data, select = c( "log_net_migration", 
                                              "fld_share_pop", "fld_share_pop_1","fld_share_pop_2", "fld_share_pop_3", "fld_share_pop_4", "fld_share_pop_5",
                                              "drt_share_pop", "drt_share_pop_1", "drt_share_pop_2", "drt_share_pop_3", "drt_share_pop_4", "drt_share_pop_5",
                                              "cyc_share_pop"  ,                    "cyc_share_pop_1"   ,                 "cyc_share_pop_2"  ,                  "cyc_share_pop_3" ,                  
                                              "cyc_share_pop_4"   ,                 "cyc_share_pop_5" ,
                                               "log_armed_conflict_brd_lag1",   
                                              "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                              "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                              "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                              "shdi_lag1"))

data_climate<-na.omit(data_climate)

neg_climate<-subset(net_neg, select = c( "log_net_migration", 
                                         "fld_share_pop", "fld_share_pop_1","fld_share_pop_2", "fld_share_pop_3", "fld_share_pop_4", "fld_share_pop_5",
                                         "drt_share_pop", "drt_share_pop_1", "drt_share_pop_2", "drt_share_pop_3", "drt_share_pop_4", "drt_share_pop_5",
                                         "cyc_share_pop"  ,                    "cyc_share_pop_1"   ,                 "cyc_share_pop_2"  ,                  "cyc_share_pop_3" ,                  
                                         "cyc_share_pop_4"   ,                 "cyc_share_pop_5" ,
                                         "log_armed_conflict_brd_lag1",   
                                         "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                         "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                         "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                         "shdi_lag1"))

neg_climate<-na.omit(neg_climate)

posit_climate<-subset(net_pos, select = c("log_net_migration", 
                                        "fld_share_pop", "fld_share_pop_1","fld_share_pop_2", "fld_share_pop_3", "fld_share_pop_4", "fld_share_pop_5",
                                        "drt_share_pop", "drt_share_pop_1", "drt_share_pop_2", "drt_share_pop_3", "drt_share_pop_4", "drt_share_pop_5",
                                        "cyc_share_pop"  ,                    "cyc_share_pop_1"   ,                 "cyc_share_pop_2"  ,                  "cyc_share_pop_3" ,                  
                                        "cyc_share_pop_4"   ,                 "cyc_share_pop_5" ,
                                        "log_armed_conflict_brd_lag1",   
                                        "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                        "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                        "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                        "shdi_lag1"))

posit_climate<-na.omit(posit_climate)


library(BVAR)
# Setting up priors

mn <- bv_minnesota(
  lambda = bv_lambda(mode = 0.2, sd = 0.4, min = 0.0001, max = 5),
  alpha = bv_alpha(mode = 2), var = 1e07)

# the sum-of-coefficient
#  Setting mode = 1 might reflect a belief that, over the long term, the total impact of all lagged values of a variable on itself (or another variable) should be neutral or balanced
soc <- bv_soc(mode = 1, sd = 1, min = 1e-04, max = 50)

# single-unit-root priors - stablity of the VAR model
# mode 1 where the system is neither too sluggish nor too prone to overshooting in its return to equilibrium
sur <- bv_sur(mode = 1, sd = 1, min = 1e-04, max = 50)


# Once the priors are defined, we provide them to bv_priors() Via hyper we choose which hyperparameters should be treated hierarchically. 
priors <- bv_priors(hyper = "auto", mn = mn, soc = soc, sur = sur)



mh <- bv_metropolis(scale_hess = c(0.05, 0.0001, 0.0001),
                    adjust_acc = TRUE, acc_lower = 0.25, acc_upper = 0.45)


# Wrapper function to automatically determine lags and run BVAR
run_bvar_with_auto_lags <- function(data, n_draw = 15000, n_burn = 5000, n_thin = 1, priors, mh, verbose = TRUE) {
  # Infer the maximum number of lags from the dataset
  # This assumes your dataset has a specific naming convention for lagged variables, e.g., var1_lag1, var1_lag2, etc.
  lag_columns <- grep("_lag", names(data), value = TRUE)
  max_lags <- max(as.integer(sub(".*_lag", "", lag_columns)))
  
  # If no lagged columns are found, set max_lags to a default value (e.g., 1)
  if (length(lag_columns) == 0) {
    max_lags <- 1
  }
  
  # Run the bvar function with the determined number of lags
  bvar_result <- bvar(data, lags = max_lags, n_draw = n_draw, n_burn = n_burn, n_thin = n_thin, priors = priors, mh = mh, verbose = verbose)
  
  return(bvar_result)
}

# Run the model
full_climate <- run_bvar_with_auto_lags(data_climate, priors = priors, mh = mh)
pos_climate <- run_bvar_with_auto_lags(posit_climate, priors = priors, mh = mh)
negat_climate <- run_bvar_with_auto_lags(neg_climate, priors = priors, mh = mh)


# Extract indices
log_net_migration_index <- which(full_climate$variables == "log_net_migration")

flood_index <- 4
flood_index_1 <- 5
flood_index_2 <- 6
flood_index_3 <- 7
flood_index_4 <- 8
flood_index_5 <- 9

drought_index <- 10
drought_index_1 <- 11
drought_index_2 <- 12
drought_index_3 <- 13
drought_index_4 <- 14
drought_index_5 <- 15

tc_index <- 16
tc_index_1 <- 17
tc_index_2 <- 18
tc_index_3 <- 19
tc_index_4 <- 20
tc_index_5 <- 21

# Extracting the posterior samples for flood coefficient
posterior_samples <- full_climate$beta[, flood_index, log_net_migration_index]
posterior_samples_1 <- full_climate$beta[, flood_index_1, log_net_migration_index]
posterior_samples_2 <- full_climate$beta[, flood_index_2, log_net_migration_index]
posterior_samples_3 <- full_climate$beta[, flood_index_3, log_net_migration_index]
posterior_samples_4 <- full_climate$beta[, flood_index_4, log_net_migration_index]
posterior_samples_5 <- full_climate$beta[, flood_index_5, log_net_migration_index]

posterior_samples <- full_climate$beta[, drought_index, log_net_migration_index]
posterior_samples_1 <- full_climate$beta[, drought_index_1, log_net_migration_index]
posterior_samples_2 <- full_climate$beta[, drought_index_2, log_net_migration_index]
posterior_samples_3 <- full_climate$beta[, drought_index_3, log_net_migration_index]
posterior_samples_4 <- full_climate$beta[, drought_index_4, log_net_migration_index]
posterior_samples_5 <- full_climate$beta[, drought_index_5, log_net_migration_index]

posterior_samples <- full_climate$beta[, tc_index, log_net_migration_index]
posterior_samples_1 <- full_climate$beta[, tc_index_1, log_net_migration_index]
posterior_samples_2 <- full_climate$beta[, tc_index_2, log_net_migration_index]
posterior_samples_3 <- full_climate$beta[, tc_index_3, log_net_migration_index]
posterior_samples_4 <- full_climate$beta[, tc_index_4, log_net_migration_index]
posterior_samples_5 <- full_climate$beta[, tc_index_5, log_net_migration_index]

# Computing the 95% credible interval

CI_lower <- quantile(posterior_samples, probs = 0.05)
CI_upper <- quantile(posterior_samples, probs = 0.95)

CI_lower_1 <- quantile(posterior_samples_1, probs = 0.05)
CI_upper_1 <- quantile(posterior_samples_1, probs = 0.95)

CI_lower_2 <- quantile(posterior_samples_2, probs = 0.05)
CI_upper_2 <- quantile(posterior_samples_2, probs = 0.95)

CI_lower_3 <- quantile(posterior_samples_3, probs = 0.05)
CI_upper_3 <- quantile(posterior_samples_3, probs = 0.95)

CI_lower_4 <- quantile(posterior_samples_4, probs = 0.05)
CI_upper_4 <- quantile(posterior_samples_4, probs = 0.95)

CI_lower_5 <- quantile(posterior_samples_5, probs = 0.05)
CI_upper_5 <- quantile(posterior_samples_5, probs = 0.95)


# Median as the point estimate
point_estimate <- median(posterior_samples)
point_estimate_1 <- median(posterior_samples_1)
point_estimate_2 <- median(posterior_samples_2)
point_estimate_3 <- median(posterior_samples_3)
point_estimate_4 <- median(posterior_samples_4)
point_estimate_5 <- median(posterior_samples_5)


# Summarizing the results
cat("95% Credible Interval for floods impact on log_net_migration:", CI_lower, CI_upper, "\n")
cat("Point Estimate:", point_estimate, "\n")

library(ggplot2)

df <- data.frame(
  Effect = c("F (t-5)", "F (t-4)", "F (t-3)", "F (t-2)", "F (t-1)", "F"),
  PointEstimate = c(point_estimate_5,point_estimate_4, point_estimate_3,point_estimate_2,  point_estimate_1, point_estimate) ,
  CI_Lower = c(CI_lower_5,CI_lower_4, CI_lower_3, CI_lower_2, CI_lower_1 ,CI_lower),  # Assuming you have these values
  CI_Upper = c(CI_upper_5,CI_upper_4, CI_upper_3 , CI_upper_2 , CI_upper_1, CI_upper )
)

df <- data.frame(
  Effect = c("D (t-5)","D (t-4)", "D (t-3)", "D (t-2)", "D (t-1)",  "D"),
  PointEstimate = c(point_estimate_5,point_estimate_4, point_estimate_3,point_estimate_2,  point_estimate_1, point_estimate) ,
  CI_Lower = c(CI_lower_5,CI_lower_4, CI_lower_3, CI_lower_2, CI_lower_1 ,CI_lower),  # Assuming you have these values
  CI_Upper = c(CI_upper_5,CI_upper_4, CI_upper_3 , CI_upper_2 , CI_upper_1, CI_upper )
)

df <- data.frame(
  Effect = c("TC (t-5)","TC (t-4)", "TC (t-3)", "TC (t-2)", "TC (t-1)",  "TC"),
  PointEstimate = c(point_estimate_5,point_estimate_4, point_estimate_3,point_estimate_2,  point_estimate_1, point_estimate) ,
  CI_Lower = c(CI_lower_5,CI_lower_4, CI_lower_3, CI_lower_2, CI_lower_1 ,CI_lower),  # Assuming you have these values
  CI_Upper = c(CI_upper_5,CI_upper_4, CI_upper_3 , CI_upper_2 , CI_upper_1, CI_upper )
)


# Rotated order

df$Effect <- factor(df$Effect, levels = c("F", "F (t-1)", "F (t-2)", "F (t-3)", "F (t-4)", "F (t-5)"))

df$Effect <- factor(df$Effect, levels = c("D", "D (t-1)", "D (t-2)", "D (t-3)", "D (t-4)", "D (t-5)"))

df$Effect <- factor(df$Effect, levels = c("TC", "TC (t-1)", "TC (t-2)", "TC (t-3)", "TC (t-4)", "TC (t-5)"))

library(ggplot2)
# Create the theme
theme_nature_panel <- theme_minimal(base_family = "Arial") +
  theme(
    axis.title.x = element_text(size = 7),
    axis.title.y = element_text(size = 7),
    axis.text.x  = element_text(size = 6),
    axis.text.y  = element_text(size = 6),
    plot.title   = element_text(size = 7, face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position  = "none",
    plot.margin = margin(3, 3, 3, 3)  # tight panels for combining
  )

## Flood – Negative net migration 
flood_neg <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(color = "orange", linewidth = 0.7) +
  geom_point(color = "lightblue", size = 2) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper),
                width = 0.15,
                color = "lightblue",
                linewidth = 0.6) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "black",
             linewidth = 0.5) +
  labs(
    x = "Time lag",
    y = "Point estimate of net migration",
    title = ""   # you can add "a) Flood – negative net-migration" later if needed
  ) +
  scale_y_continuous(
    limits = c(-1, 1),
    breaks = seq(-1, 1, by = 0.2)
  ) +
  theme_nature_panel

flood_neg

drought_neg <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(color = "orange", linewidth = 0.7) +
  geom_point(color = "lightblue", size = 2) +
  geom_errorbar(
    aes(ymin = CI_Lower, ymax = CI_Upper),
    width = 0.15,
    color = "lightblue",
    linewidth = 0.6
  ) +
  geom_hline(
    yintercept = 0,
    linetype   = "dashed",
    color      = "black",
    linewidth  = 0.5
  ) +
  labs(
    x     = "Time lag",
    y     = "Point estimate of net migration",
    title = ""   # e.g. "b) Drought – negative net migration"
  ) +
  scale_y_continuous(
    limits = c(-0.5, 0.5),
    breaks = seq(-0.5, 0.5, by = 0.1)
  ) +
  theme_nature_panel

drought_neg

tc_neg <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(color = "orange", linewidth = 0.7) +
  geom_point(color = "lightblue", size = 2) +
  geom_errorbar(
    aes(ymin = CI_Lower, ymax = CI_Upper),
    width = 0.15,
    color = "lightblue",
    linewidth = 0.6
  ) +
  geom_hline(
    yintercept = 0,
    linetype   = "dashed",
    color      = "black",
    linewidth  = 0.5
  ) +
  labs(
    x     = "Time lag",
    y     = "Point estimate of net migration",
    title = ""   # Add "TC – negative net migration" later if needed
  ) +
  scale_y_continuous(
    limits = c(-0.2, 0.2),
    breaks = seq(-0.2, 0.2, by = 0.1)
  ) +
  theme_nature_panel

tc_neg

# positive net 
flood_pos <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(color = "orange", linewidth = 0.7) +
  geom_point(color = "lightblue", size = 2) +
  geom_errorbar(
    aes(ymin = CI_Lower, ymax = CI_Upper),
    width = 0.15,
    color = "lightblue",
    linewidth = 0.6
  ) +
  geom_hline(
    yintercept = 0,
    linetype   = "dashed",
    color      = "black",
    linewidth  = 0.5
  ) +
  labs(
    x = "Time lag",
    y = "Point estimate of net migration",
    title = ""
  ) +
  scale_y_continuous(
    limits = c(-1, 1),
    breaks = seq(-1, 1, by = 0.2)
  ) +
  theme_nature_panel

flood_pos

drought_pos <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(color = "orange", linewidth = 0.7) +
  geom_point(color = "lightblue", size = 2) +
  geom_errorbar(
    aes(ymin = CI_Lower, ymax = CI_Upper),
    width = 0.15,
    color = "lightblue",
    linewidth = 0.6
  ) +
  geom_hline(
    yintercept = 0,
    linetype   = "dashed",
    color      = "black",
    linewidth  = 0.5
  ) +
  labs(
    x = "Time lag",
    y = "Point estimate of net migration",
    title = ""
  ) +
  scale_y_continuous(
    limits = c(-0.5, 0.5),
    breaks = seq(-0.5, 0.5, by = 0.1)
  ) +
  theme_nature_panel


drought_pos

tc_pos <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(color = "orange", linewidth = 0.7) +
  geom_point(color = "lightblue", size = 2) +
  geom_errorbar(
    aes(ymin = CI_Lower, ymax = CI_Upper),
    width = 0.15,
    color = "lightblue",
    linewidth = 0.6
  ) +
  geom_hline(
    yintercept = 0,
    linetype   = "dashed",
    color      = "black",
    linewidth  = 0.5
  ) +
  labs(
    x = "Time lag",
    y = "Point estimate of net migration",
    title = ""
  ) +
  scale_y_continuous(
    limits = c(-0.2, 0.2),
    breaks = seq(-0.2, 0.2, by = 0.1)
  ) +
  theme_nature_panel


tc_pos

# Combine plots 
library(ggplot2)
library(cowplot)

## Legend

legend_df <- data.frame(
  x    = factor(c("Lag1", "Lag2"), levels = c("Lag1", "Lag2")),
  y    = c(0, 0),
  ymin = c(-0.1, -0.1),
  ymax = c(0.1, 0.1)
)

legend_plot <- ggplot(legend_df, aes(x = x, y = y, group = 1)) +
  geom_line(aes(color = "Trend"), linewidth = 0.7) +
  geom_point(aes(color = "Point estimate"), size = 2) +
  geom_errorbar(
    aes(ymin = ymin, ymax = ymax, color = "95% CI"),
    width = 0.15,
    linewidth = 0.6
  ) +
  scale_color_manual(
    name = NULL,  
    values = c(
      "Trend"          = "orange",
      "Point estimate" = "lightblue",
      "95% CI"         = "lightblue"
    )
  ) +
  theme_minimal() +  
  theme(
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.text      = element_text(size = 6),
    panel.background = element_blank(),
    panel.grid       = element_blank(),
    axis.text        = element_blank(),
    axis.title       = element_blank(),
    axis.ticks       = element_blank()
  )

# Extract the legend 
shared_legend <- cowplot::get_legend(legend_plot)


# For negative panels: show y-axis label
adjust_neg_plot <- function(p, xlab = NULL) {
  p + labs(
    y = "Point estimate of net migration",
    x = xlab
  )
}

# For positive panels: no y-axis label
adjust_pos_plot <- function(p, xlab = NULL) {
  p + labs(
    y = NULL,
    x = xlab
  )
}


## Apply to each panel 

# Negative plots
flood_neg_p   <- adjust_neg_plot(flood_neg,   xlab = NULL)
drought_neg_p <- adjust_neg_plot(drought_neg, xlab = NULL)
tc_neg_p      <- adjust_neg_plot(tc_neg,      xlab = "Time lag")

# Positive plots
flood_pos_p   <- adjust_pos_plot(flood_pos,   xlab = NULL)
drought_pos_p <- adjust_pos_plot(drought_pos, xlab = NULL)
tc_pos_p      <- adjust_pos_plot(tc_pos,      xlab = "Time lag")


## Build rows with panel labels 

row_flood <- plot_grid(
  flood_neg_p, flood_pos_p,
  nrow           = 1,
  rel_widths     = c(1, 1),
  align          = "h",
  labels         = c("a", "b"),
  label_size     = 7,
  label_fontface = "bold",
  label_y        = 1.05
)

row_drought <- plot_grid(
  drought_neg_p, drought_pos_p,
  nrow           = 1,
  rel_widths     = c(1, 1),
  align          = "h",
  labels         = c("c", "d"),
  label_size     = 7,
  label_fontface = "bold",
  label_y        = 1.05
)

row_tc <- plot_grid(
  tc_neg_p, tc_pos_p,
  nrow           = 1,
  rel_widths     = c(1, 1),
  align          = "h",
  labels         = c("e", "f"),
  label_size     = 7,
  label_fontface = "bold",
  label_y        = 1.05
)


## Add left-hand row labels 

row_flood <- ggdraw() +
  draw_plot(row_flood, x = 0.08, y = 0, width = 0.92, height = 1) +
  draw_label(
    "Flood",
    x        = 0.015,
    y        = 0.5,
    angle    = 90,
    size     = 7,
    fontface = "bold",
    hjust    = 0.5
  )

row_drought <- ggdraw() +
  draw_plot(row_drought, x = 0.08, y = 0, width = 0.92, height = 1) +
  draw_label(
    "Drought",
    x        = 0.015,
    y        = 0.5,
    angle    = 90,
    size     = 7,
    fontface = "bold",
    hjust    = 0.5
  )

row_tc <- ggdraw() +
  draw_plot(row_tc, x = 0.08, y = 0, width = 0.92, height = 1) +
  draw_label(
    "Trop. cyclone",
    x        = 0.015,
    y        = 0.5,
    angle    = 90,
    size     = 7,
    fontface = "bold",
    hjust    = 0.5
  )


## Column titles 

titles <- plot_grid(
  ggdraw() + draw_label(
    "Negative net-migration",
    fontface = "bold",
    size     = 7,
    hjust    = 0.5
  ),
  ggdraw() + draw_label(
    "Positive net-migration",
    fontface = "bold",
    size     = 7,
    hjust    = 0.5
  ),
  nrow       = 1,
  rel_widths = c(1, 1)
)


## Final layout 

final_plot <- plot_grid(
  titles,
  row_flood,
  row_drought,
  row_tc,
  ncol        = 1,
  rel_heights = c(0.18, 1, 1, 1)
)

# Add shared legend at the bottom
final_plot_with_legend <- plot_grid(
  final_plot,
  shared_legend,
  ncol        = 1,
  rel_heights = c(1, 0.12)  
)


## Save as PDF

w_in <- 180 / 25.4
h_in <- 180 / 25.4

quartz(
  file  = "fig3.pdf",
  type  = "pdf",
  width = w_in,
  height = h_in
)

print(final_plot_with_legend) 
dev.off()


# Full model
library(ggplot2)
# Create the plot
## Flood – Full sample
flood_full <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(color = "orange", linewidth = 0.7) +
  geom_point(color = "lightblue", size = 2) +
  geom_errorbar(
    aes(ymin = CI_Lower, ymax = CI_Upper),
    width = 0.15,
    color = "lightblue",
    linewidth = 0.6
  ) +
  geom_hline(
    yintercept = 0,
    linetype   = "dashed",
    color      = "black",
    linewidth  = 0.5
  ) +
  labs(
    x = "Time lag",
    y = "Point estimate of net migration",
    title = ""
  ) +
  scale_y_continuous(
    limits = c(-1, 1),
    breaks = seq(-1, 1, by = 0.2)
  ) +
  theme_nature_panel
flood_full

## Drought – Full sample
library(scales) 

drought_full <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(color = "orange", linewidth = 0.7) +
  geom_point(color = "lightblue", size = 2) +
  geom_errorbar(
    aes(ymin = CI_Lower, ymax = CI_Upper),
    width = 0.15,
    color = "lightblue",
    linewidth = 0.6
  ) +
  geom_hline(
    yintercept = 0,
    linetype   = "dashed",
    color      = "black",
    linewidth  = 0.5
  ) +
  labs(
    x = "Time lag",
    y = "Point estimate of net migration",
    title = ""
  ) +
  scale_y_continuous(
    limits = c(-0.6, 0.6),
    breaks = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6),
    labels = label_number(accuracy = 0.1)  # prints -0.6, -0.4, ..., 0.6
  ) +
  theme_nature_panel
drought_full

## Tropical cyclone – Full sample

tc_full <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(color = "orange", linewidth = 0.7) +
  geom_point(color = "lightblue", size = 2) +
  geom_errorbar(
    aes(ymin = CI_Lower, ymax = CI_Upper),
    width = 0.15,
    color = "lightblue",
    linewidth = 0.6
  ) +
  geom_hline(
    yintercept = 0,
    linetype   = "dashed",
    color      = "black",
    linewidth  = 0.5
  ) +
  labs(
    x = "Time lag",
    y = "Point estimate of net migration",
    title = ""
  ) +
  scale_y_continuous(
    limits = c(-0.3, 0.3),
    breaks = c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3),   # exact values
    labels = label_number(accuracy = 0.1)            # format cleanly
  ) +
  theme_nature_panel

tc_full

# Combined figure
##  Legend 

legend_df <- data.frame(
  x    = factor(c("Lag1", "Lag2"), levels = c("Lag1", "Lag2")),
  y    = c(0, 0),
  ymin = c(-0.1, -0.1),
  ymax = c(0.1, 0.1)
)

legend_plot <- ggplot(legend_df, aes(x = x, y = y, group = 1)) +
  geom_line(aes(color = "Trend"), linewidth = 0.7) +
  geom_point(aes(color = "Point estimate"), size = 2) +
  geom_errorbar(
    aes(ymin = ymin, ymax = ymax, color = "95% CI"),
    width    = 0.15,
    linewidth = 0.6
  ) +
  scale_color_manual(
    name   = NULL,
    values = c(
      "Trend"          = "orange",
      "Point estimate" = "lightblue",
      "95% CI"         = "lightblue"
    )
  ) +
  theme_minimal() +
  theme(
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.text      = element_text(size = 6),
    panel.background = element_blank(),
    panel.grid       = element_blank(),
    axis.text        = element_blank(),
    axis.title       = element_blank(),
    axis.ticks       = element_blank()
  )

shared_legend <- cowplot::get_legend(legend_plot)



adjust_full_plot <- function(p, xlab = NULL) {
  p + labs(
    x = xlab,
    y = "Point estimate of net migration"
  )
}

flood_full_p   <- adjust_full_plot(flood_full,   xlab = NULL)
drought_full_p <- adjust_full_plot(drought_full, xlab = NULL)
tc_full_p      <- adjust_full_plot(tc_full,      xlab = "Time lag")


## Add left-hand row labels 

row_flood_full <- ggdraw() +
  draw_plot(flood_full_p, x = 0.08, y = 0, width = 0.92, height = 1) +
  draw_label(
    "Flood",
    x        = 0.015,
    y        = 0.5,
    angle    = 90,
    size     = 7,
    fontface = "bold",
    hjust    = 0.5
  )

row_drought_full <- ggdraw() +
  draw_plot(drought_full_p, x = 0.08, y = 0, width = 0.92, height = 1) +
  draw_label(
    "Drought",
    x        = 0.015,
    y        = 0.5,
    angle    = 90,
    size     = 7,
    fontface = "bold",
    hjust    = 0.5
  )

row_tc_full <- ggdraw() +
  draw_plot(tc_full_p, x = 0.08, y = 0, width = 0.92, height = 1) +
  draw_label(
    "Trop. cyclone",
    x        = 0.015,
    y        = 0.5,
    angle    = 90,
    size     = 7,
    fontface = "bold",
    hjust    = 0.5
  )


## Stack the three rows 

full_3panel <- plot_grid(
  row_flood_full,
  row_drought_full,
  row_tc_full,
  ncol        = 1,
  rel_heights = c(1, 1, 1)
)

full_3panel_with_legend <- plot_grid(
  full_3panel,
  shared_legend,
  ncol        = 1,
  rel_heights = c(1, 0.12)   
)


## Save as PDF 

w_in <- 88 / 25.4
h_in <- 180 / 25.4  

quartz(
  file  = "Fig.S12.pdf",
  type  = "pdf",
  width = w_in,
  height = h_in
)

print(full_3panel_with_legend)
dev.off()
