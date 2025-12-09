# Split by income group
data <- readRDS("data.rds")
# start with net negative
names(data)

net_neg<-subset(net_neg, select = c( "log_net_migration", 
                                     "fld_share_pop", "fld_share_pop_1","fld_share_pop_2", "fld_share_pop_3", "fld_share_pop_4", "fld_share_pop_5",
                                     "drt_share_pop", "drt_share_pop_1", "drt_share_pop_2", "drt_share_pop_3", "drt_share_pop_4", "drt_share_pop_5",
                                     "cyc_share_pop"  ,                    "cyc_share_pop_1"   ,                 "cyc_share_pop_2"  ,                  "cyc_share_pop_3" ,                  
                                     "cyc_share_pop_4"   ,                 "cyc_share_pop_5" ,
                                     "log_armed_conflict_brd_lag1",   
                                     "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                     "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                     "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                     "shdi_lag1", "Income_Group"))

net_neg<-na.omit(net_neg)

unique(net_neg$Income_Group)
neg_low_income <- subset(net_neg, Income_Group %in% c("L", "LM"))
neg_high_income <- subset(net_neg, Income_Group %in% c("UM", "H"))

neg_low_income <- subset(neg_low_income, select = -Income_Group)
neg_high_income <- subset(neg_high_income, select = -Income_Group)


# positive
net_pos<-subset(net_pos, select = c( "log_net_migration", 
                                     "fld_share_pop", "fld_share_pop_1","fld_share_pop_2", "fld_share_pop_3", "fld_share_pop_4", "fld_share_pop_5",
                                     "drt_share_pop", "drt_share_pop_1", "drt_share_pop_2", "drt_share_pop_3", "drt_share_pop_4", "drt_share_pop_5",
                                     "cyc_share_pop"  ,                    "cyc_share_pop_1"   ,                 "cyc_share_pop_2"  ,                  "cyc_share_pop_3" ,                  
                                     "cyc_share_pop_4"   ,                 "cyc_share_pop_5" ,
                                     "log_armed_conflict_brd_lag1",   
                                     "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                     "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                     "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                     "shdi_lag1", "Income_Group"))

net_pos<-na.omit(net_pos)

unique(net_neg$Income_Group)
pos_low_income <- subset(net_pos, Income_Group %in% c("L", "LM"))
pos_high_income <- subset(net_pos, Income_Group %in% c("UM", "H"))

pos_low_income <- subset(pos_low_income, select = -Income_Group)
pos_high_income <- subset(pos_high_income, select = -Income_Group)


# Run the model
neg_LI <- run_bvar_with_auto_lags(neg_low_income, priors = priors, mh = mh)
neg_HI <- run_bvar_with_auto_lags(neg_high_income, priors = priors, mh = mh)
pos_LI <- run_bvar_with_auto_lags(pos_low_income, priors = priors, mh = mh)
pos_HI <- run_bvar_with_auto_lags(pos_high_income, priors = priors, mh = mh)

# Extract indices
log_net_migration_index <- which(pos_HI$variables == "log_net_migration")

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
posterior_samples <- pos_HI$beta[, flood_index, log_net_migration_index]
posterior_samples_1 <- pos_HI$beta[, flood_index_1, log_net_migration_index]
posterior_samples_2 <- pos_HI$beta[, flood_index_2, log_net_migration_index]
posterior_samples_3 <- pos_HI$beta[, flood_index_3, log_net_migration_index]
posterior_samples_4 <- pos_HI$beta[, flood_index_4, log_net_migration_index]
posterior_samples_5 <- pos_HI$beta[, flood_index_5, log_net_migration_index]

posterior_samples <- pos_HI$beta[, drought_index, log_net_migration_index]
posterior_samples_1 <- pos_HI$beta[, drought_index_1, log_net_migration_index]
posterior_samples_2 <- pos_HI$beta[, drought_index_2, log_net_migration_index]
posterior_samples_3 <- pos_HI$beta[, drought_index_3, log_net_migration_index]
posterior_samples_4 <- pos_HI$beta[, drought_index_4, log_net_migration_index]
posterior_samples_5 <- pos_HI$beta[, drought_index_5, log_net_migration_index]

posterior_samples <- pos_HI$beta[, tc_index, log_net_migration_index]
posterior_samples_1 <- pos_HI$beta[, tc_index_1, log_net_migration_index]
posterior_samples_2 <- pos_HI$beta[, tc_index_2, log_net_migration_index]
posterior_samples_3 <- pos_HI$beta[, tc_index_3, log_net_migration_index]
posterior_samples_4 <- pos_HI$beta[, tc_index_4, log_net_migration_index]
posterior_samples_5 <- pos_HI$beta[, tc_index_5, log_net_migration_index]

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

print(df)

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
# Create the plot
# negative net
flood_neg_low <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(color = "orange", linewidth = 0.7) +
  geom_point(color = "lightblue", size = 2) +
  geom_errorbar(
    aes(ymin = CI_Lower, ymax = CI_Upper),
    width = 0.15, color = "lightblue", linewidth = 0.6
  ) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "black", linewidth = 0.5) +
  labs(
    x = "Time lag",
    y = "Point estimate of net migration"
  ) +
  scale_y_continuous(
    limits = c(-2.5, 2.5),
    breaks = seq(-2.5, 2.5, by = 0.5)
  ) +
  theme_nature_panel
flood_neg_low

drought_neg_low <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(color = "orange", linewidth = 0.7) +
  geom_point(color = "lightblue", size = 2) +
  geom_errorbar(
    aes(ymin = CI_Lower, ymax = CI_Upper),
    width = 0.15, color = "lightblue", linewidth = 0.6
  ) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "black", linewidth = 0.5) +
  labs(
    x = "Time lag",
    y = "Point estimate of net migration"
  ) +
  scale_y_continuous(
    limits = c(-0.7, 0.7),
    breaks  = seq(-0.7, 0.7, by = 0.2),
    labels  = function(x) sprintf("%.1f", x)  
  ) +
  theme_nature_panel


tc_neg_low <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(color = "orange", linewidth = 0.7) +
  geom_point(color = "lightblue", size = 2) +
  geom_errorbar(
    aes(ymin = CI_Lower, ymax = CI_Upper),
    width = 0.15, color = "lightblue", linewidth = 0.6
  ) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "black", linewidth = 0.5) +
  labs(
    x = "Time lag",
    y = "Point estimate of net migration"
  ) +
  scale_y_continuous(
    limits = c(-0.3, 0.3),
    breaks = seq(-0.3, 0.3, by = 0.1),
    labels  = function(x) sprintf("%.1f", x) 
  ) +
  theme_nature_panel

# high income 
flood_neg_high <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(color = "orange", linewidth = 0.7) +
  geom_point(color = "lightblue", size = 2) +
  geom_errorbar(
    aes(ymin = CI_Lower, ymax = CI_Upper),
    width = 0.15, color = "lightblue", linewidth = 0.6
  ) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "black", linewidth = 0.5) +
  labs(
    x = "Time lag",
    y = "Point estimate of net migration"
  ) +
  scale_y_continuous(
    limits = c(-2.5, 2.5),
    breaks = seq(-2.5, 2.5, by = 0.5),
    labels = function(x) sprintf("%.1f", x)
  ) +
  theme_nature_panel

drought_neg_high <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(color = "orange", linewidth = 0.7) +
  geom_point(color = "lightblue", size = 2) +
  geom_errorbar(
    aes(ymin = CI_Lower, ymax = CI_Upper),
    width = 0.15, color = "lightblue", linewidth = 0.6
  ) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "black", linewidth = 0.5) +
  labs(
    x = "Time lag",
    y = "Point estimate of net migration"
  ) +
  scale_y_continuous(
    limits = c(-0.7, 0.7),
    breaks = seq(-0.7, 0.7, by = 0.2),
    labels = function(x) sprintf("%.1f", x)
  ) +
  theme_nature_panel

tc_neg_high <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(color = "orange", linewidth = 0.7) +
  geom_point(color = "lightblue", size = 2) +
  geom_errorbar(
    aes(ymin = CI_Lower, ymax = CI_Upper),
    width = 0.15, color = "lightblue", linewidth = 0.6
  ) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "black", linewidth = 0.5) +
  labs(
    x = "Time lag",
    y = "Point estimate of net migration"
  ) +
  scale_y_continuous(
    limits = c(-0.3, 0.3),
    breaks = seq(-0.3, 0.3, by = 0.1),
    labels = function(x) sprintf("%.1f", x)
  ) +
  theme_nature_panel

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
    aes(ymin = ymin, ymax = ymax, color = "90% CI"),
    width    = 0.15,
    linewidth = 0.6
  ) +
  scale_color_manual(
    name   = NULL,
    values = c(
      "Trend"          = "orange",
      "Point estimate" = "lightblue",
      "90% CI"         = "lightblue"
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




adjust_neg_plot <- function(p, xlab = NULL, show_y = TRUE) {
  p + labs(
    x = xlab,
    y = if (show_y) "Point estimate of net migration" else NULL
  ) +
    theme(
      plot.title   = element_blank(),
      axis.title.y = element_text(size = 7, margin = margin(r = 4)),
      axis.title.x = element_text(size = 7)
    )
}


## Apply to each panel 

# Left column: Low–lower middle income
flood_neg_low_p   <- adjust_neg_plot(flood_neg_low,   xlab = NULL,       show_y = TRUE)
drought_neg_low_p <- adjust_neg_plot(drought_neg_low, xlab = NULL,       show_y = TRUE)
tc_neg_low_p      <- adjust_neg_plot(tc_neg_low,      xlab = "Time lag", show_y = TRUE)

# Right column: Upper-middle–high income
flood_neg_high_p   <- adjust_neg_plot(flood_neg_high,   xlab = NULL,       show_y = FALSE)
drought_neg_high_p <- adjust_neg_plot(drought_neg_high, xlab = NULL,       show_y = FALSE)
tc_neg_high_p      <- adjust_neg_plot(tc_neg_high,      xlab = "Time lag", show_y = FALSE)




row_flood <- plot_grid(
  flood_neg_low_p, flood_neg_high_p,
  nrow           = 1,
  rel_widths     = c(1, 1),
  align          = "h",
  labels         = c("a", "b"),
  label_size     = 7,
  label_fontface = "bold",
  label_y        = 1.05
)

row_drought <- plot_grid(
  drought_neg_low_p, drought_neg_high_p,
  nrow           = 1,
  rel_widths     = c(1, 1),
  align          = "h",
  labels         = c("c", "d"),
  label_size     = 7,
  label_fontface = "bold",
  label_y        = 1.05
)

row_tc <- plot_grid(
  tc_neg_low_p, tc_neg_high_p,
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


##  Column titles

titles <- plot_grid(
  ggdraw() + draw_label(
    "Low- to lower-middle income",
    fontface = "bold",
    size     = 7,
    hjust    = 0.5
  ),
  ggdraw() + draw_label(
    "Upper-middle- to high income",
    fontface = "bold",
    size     = 7,
    hjust    = 0.5
  ),
  nrow       = 1,
  rel_widths = c(1, 1)
)




panel_block <- plot_grid(
  titles,
  row_flood,
  row_drought,
  row_tc,
  ncol        = 1,
  rel_heights = c(0.18, 1, 1, 1)
)

# Add shared legend at the bottom
final_plot_with_legend <- plot_grid(
  panel_block,
  shared_legend,
  ncol        = 1,
  rel_heights = c(1, 0.12)
)


## Save as PDF 

w_in <- 188 / 25.4
h_in <- 180 / 25.4  

quartz(
  file  = "FigS14.pdf",
  type  = "pdf",
  width = w_in,
  height = h_in
)

print(final_plot_with_legend)
dev.off()

# POSITIVE 
# Create the plot

flood_pos_low <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
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
    y = "Point estimate of net migration"
  ) +
  scale_y_continuous(
    limits = c(-2.5, 2.5),
    breaks = seq(-2.5, 2.5, by = 0.5)
  ) +
  theme_nature_panel

# Drought
drought_pos_low <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
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
    y = "Point estimate of net migration"
  ) +
  scale_y_continuous(
    limits = c(-1, 1),
    breaks = seq(-1, 1, by = 0.2)
  ) +
  theme_nature_panel

# Trop. cyclone
tc_pos_low <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
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
    y = "Point estimate of net migration"
  ) +
  scale_y_continuous(
    limits = c(-0.3, 0.3),
    breaks = seq(-0.3, 0.3, by = 0.1),
    labels = function(x) sprintf("%.1f", x)
  ) +
  theme_nature_panel

# Flood
flood_pos_high <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
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
    y = NULL
  ) +
  scale_y_continuous(
    limits = c(-2.5, 2.5),
    breaks = seq(-2.5, 2.5, by = 0.5)
  ) +
  theme_nature_panel

# Drought
drought_pos_high <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
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
    y = NULL
  ) +
  scale_y_continuous(
    limits = c(-1, 1),
    breaks = seq(-1, 1, by = 0.2)
  ) +
  theme_nature_panel

# Trop. cyclone
tc_pos_high <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
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
    y = NULL
  ) +
  scale_y_continuous(
    limits = c(-0.3, 0.3),
    breaks = seq(-0.3, 0.3, by = 0.1),
    labels = function(x) sprintf("%.1f", x)
  ) +
  theme_nature_panel


## Shared legend 

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
    aes(ymin = ymin, ymax = ymax, color = "90% CI"),
    width    = 0.15,
    linewidth = 0.6
  ) +
  scale_color_manual(
    name   = NULL,
    values = c(
      "Trend"          = "orange",
      "Point estimate" = "lightblue",
      "90% CI"         = "lightblue"
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



adjust_pos_panel <- function(p, xlab = NULL, show_y = TRUE) {
  p + labs(
    x = xlab,
    y = if (show_y) "Point estimate of net migration" else NULL
  )
}

# Left column (low income)
flood_pos_low_p   <- adjust_pos_panel(flood_pos_low,   xlab = NULL,       show_y = TRUE)
drought_pos_low_p <- adjust_pos_panel(drought_pos_low, xlab = NULL,       show_y = TRUE)
tc_pos_low_p      <- adjust_pos_panel(tc_pos_low,      xlab = "Time lag", show_y = TRUE)

# Right column (high income)
flood_pos_high_p   <- adjust_pos_panel(flood_pos_high,   xlab = NULL,       show_y = FALSE)
drought_pos_high_p <- adjust_pos_panel(drought_pos_high, xlab = NULL,       show_y = FALSE)
tc_pos_high_p      <- adjust_pos_panel(tc_pos_high,      xlab = "Time lag", show_y = FALSE)



row_flood <- plot_grid(
  flood_pos_low_p, flood_pos_high_p,
  nrow           = 1,
  rel_widths     = c(1, 1),
  align          = "h",
  labels         = c("a", "b"),
  label_size     = 7,
  label_fontface = "bold",
  label_y        = 1.05
)

row_drought <- plot_grid(
  drought_pos_low_p, drought_pos_high_p,
  nrow           = 1,
  rel_widths     = c(1, 1),
  align          = "h",
  labels         = c("c", "d"),
  label_size     = 7,
  label_fontface = "bold",
  label_y        = 1.05
)

row_tc <- plot_grid(
  tc_pos_low_p, tc_pos_high_p,
  nrow           = 1,
  rel_widths     = c(1, 1),
  align          = "h",
  labels         = c("e", "f"),
  label_size     = 7,
  label_fontface = "bold",
  label_y        = 1.05
)


##  Add left-hand row labels 

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
    "Low- to lower-middle income",
    fontface = "bold",
    size     = 7,
    hjust    = 0.5
  ),
  ggdraw() + draw_label(
    "Upper-middle- to high income",
    fontface = "bold",
    size     = 7,
    hjust    = 0.5
  ),
  nrow       = 1,
  rel_widths = c(1, 1)
)



panel_block_pos <- plot_grid(
  titles,
  row_flood,
  row_drought,
  row_tc,
  ncol        = 1,
  rel_heights = c(0.18, 1, 1, 1)
)

final_pos_with_legend <- plot_grid(
  panel_block_pos,
  shared_legend,
  ncol        = 1,
  rel_heights = c(1, 0.12)
)


## Save as PDF 

w_in <- 188 / 25.4
h_in <- 180 / 25.4  

quartz(
  file  = "FigS15.pdf",
  type  = "pdf",
  width = w_in,
  height = h_in
)

print(final_pos_with_legend)
dev.off()


