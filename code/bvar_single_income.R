# Split by income group

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
log_net_migration_index <- which(neg_LI$variables == "log_net_migration")

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
posterior_samples <- neg_LI$beta[, flood_index, log_net_migration_index]
posterior_samples_1 <- neg_LI$beta[, flood_index_1, log_net_migration_index]
posterior_samples_2 <- neg_LI$beta[, flood_index_2, log_net_migration_index]
posterior_samples_3 <- neg_LI$beta[, flood_index_3, log_net_migration_index]
posterior_samples_4 <- neg_LI$beta[, flood_index_4, log_net_migration_index]
posterior_samples_5 <- neg_LI$beta[, flood_index_5, log_net_migration_index]

posterior_samples <- neg_LI$beta[, drought_index, log_net_migration_index]
posterior_samples_1 <- neg_LI$beta[, drought_index_1, log_net_migration_index]
posterior_samples_2 <- neg_LI$beta[, drought_index_2, log_net_migration_index]
posterior_samples_3 <- neg_LI$beta[, drought_index_3, log_net_migration_index]
posterior_samples_4 <- neg_LI$beta[, drought_index_4, log_net_migration_index]
posterior_samples_5 <- neg_LI$beta[, drought_index_5, log_net_migration_index]

posterior_samples <- neg_LI$beta[, tc_index, log_net_migration_index]
posterior_samples_1 <- neg_LI$beta[, tc_index_1, log_net_migration_index]
posterior_samples_2 <- neg_LI$beta[, tc_index_2, log_net_migration_index]
posterior_samples_3 <- neg_LI$beta[, tc_index_3, log_net_migration_index]
posterior_samples_4 <- neg_LI$beta[, tc_index_4, log_net_migration_index]
posterior_samples_5 <- neg_LI$beta[, tc_index_5, log_net_migration_index]

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
# Create the plot
flood_neg_low <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(aes(color = "Trend"), size = 2) + # Make the trend line thicker
  geom_point(aes(color = "Point Estimates"), size = 4) + # Make the points larger
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "lightblue", size = 1) + # Add error bars
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) + # Horizontal dashed line at y = 0
  scale_color_manual(values = c("Trend" = "orange", "Point Estimates" = "lightblue")) + # Custom colors
  labs(
    title = "",
    x = "",
    y = "Point Estimate of Net Migration"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.position = "none" # <--- Remove legend here
  ) +
  scale_y_continuous(
    limits = c(-2.5, 2.5),
    breaks = seq(-2.5, 2.5, by = 0.5)
  )

flood_neg_low

drought_neg_low <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(aes(color = "Trend"), size = 2) + # Make the trend line thicker
  geom_point(aes(color = "Point Estimates"), size = 4) + # Make the points larger
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "lightblue", size = 1) + # Add error bars
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) + # Horizontal dashed line at y = 0
  scale_color_manual(values = c("Trend" = "orange", "Point Estimates" = "lightblue")) + # Custom colors
  labs(
    title = "",
    x = "",
    y = "Point Estimate of Net Migration"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.position = "none" # <--- Remove legend here
  ) +
  scale_y_continuous(
    limits = c(-0.6, 0.6),
    breaks = c(-0.6, -0.4,-0.2,0, 0.2, 0.4, 0.6)
  )

drought_neg_low

tc_neg_low <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(aes(color = "Trend"), size = 2) + # Make the trend line thicker
  geom_point(aes(color = "Point Estimates"), size = 4) + # Make the points larger
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "lightblue", size = 1) + # Add error bars
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) + # Horizontal dashed line at y = 0
  scale_color_manual(values = c("Trend" = "orange", "Point Estimates" = "lightblue")) + # Custom colors
  labs(
    title = "",
    x = "",
    y = "Point Estimate of Net Migration"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 18),
    legend.position = "none" # <--- Remove legend here
  ) +
  scale_y_continuous(
    limits = c(-0.3, 0.3),
    breaks = c(-0.3,-0.2, -0.1, 0, 0.1, 0.2, 0.3)
  )

tc_neg_low

# positive net 
flood_neg_high <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(aes(color = "Trend"), size = 2) + # Make the trend line thicker
  geom_point(aes(color = "Point Estimates"), size = 4) + # Make the points larger
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "lightblue", size = 1) + # Add error bars
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) + # Horizontal dashed line at y = 0
  scale_color_manual(values = c("Trend" = "orange", "Point Estimates" = "lightblue")) + # Custom colors
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.position = "none" # <--- Remove legend here
  ) +
  scale_y_continuous(
    limits = c(-2.5, 2.5),
    breaks = seq(-2.5, 2.5, by = 0.5)
  )

flood_neg_high

drought_neg_high <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(aes(color = "Trend"), size = 2) + # Make the trend line thicker
  geom_point(aes(color = "Point Estimates"), size = 4) + # Make the points larger
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "lightblue", size = 1) + # Add error bars
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) + # Horizontal dashed line at y = 0
  scale_color_manual(values = c("Trend" = "orange", "Point Estimates" = "lightblue")) + # Custom colors
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.position = "none" # <--- Remove legend here
  ) +
  scale_y_continuous(
    limits = c(-0.6, 0.6),
    breaks = c(-0.6, -0.4,-0.2,0, 0.2, 0.4, 0.6)
  )

drought_neg_high

tc_neg_high <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(aes(color = "Trend"), size = 2) + # Make the trend line thicker
  geom_point(aes(color = "Point Estimates"), size = 4) + # Make the points larger
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "lightblue", size = 1) + # Add error bars
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) + # Horizontal dashed line at y = 0
  scale_color_manual(values = c("Trend" = "orange", "Point Estimates" = "lightblue")) + # Custom colors
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 18),
    legend.position = "none" # <--- Remove legend here
  ) +
  scale_y_continuous(
    limits = c(-0.3, 0.3),
    breaks = c(-0.3,-0.2, -0.1, 0, 0.1, 0.2, 0.3)
  )

tc_neg_high

# Combine plots 
library(ggplot2)
library(cowplot)

# Set global white background 
theme_set(theme_minimal(base_size = 12) +
            theme(plot.background = element_rect(fill = "white", color = NA)))

# Y-axis label formatting
adjust_neg_plot <- function(p) {
  p + labs(y = "Point Estimate of Net Migration") +
    theme(
      plot.title = element_blank(),
      axis.title.y = element_text(size = 15, margin = margin(r = 1))
    )
}

# Adjust individual plots 

# Negative plots (left column)
flood_neg_low <- adjust_neg_plot(flood_neg_low)
drought_neg_low <- adjust_neg_plot(drought_neg_low + labs(x = NULL))
tc_neg_low <- adjust_neg_plot(tc_neg_low + labs(x = "Time lag") +
                            theme(axis.title.x = element_text(size = 24)))

# Right column, no y-axis label)
flood_neg_high <- flood_neg_high + labs(y = NULL, x = NULL) + theme(plot.title = element_blank())
drought_neg_high <- drought_neg_high + labs(y = NULL, x = NULL) + theme(plot.title = element_blank())
tc_neg_high <- tc_neg_high + labs(y = NULL, x = "Time lag") +
  theme(plot.title = element_blank(),
        axis.title.x = element_text(size = 24))

# Combine and label each row of plots 

row_flood <- plot_grid(
  flood_neg_low, flood_neg_high,
  nrow = 1, rel_widths = c(1, 1),
  align = "h",
  labels = c("a", "b"), label_size = 16, label_fontface = "bold"
)

row_drought <- plot_grid(
  drought_neg_low, drought_neg_high,
  nrow = 1, rel_widths = c(1, 1),
  align = "h",
  labels = c("c", "d"), label_size = 16, label_fontface = "bold"
)

row_tc <- plot_grid(
  tc_neg_low, tc_neg_high,
  nrow = 1, rel_widths = c(1, 1),
  align = "h",
  labels = c("e", "f"), label_size = 16, label_fontface = "bold"
)

# Add row labels

row_flood <- plot_grid(
  flood_neg_low, flood_neg_high,
  nrow = 1, rel_widths = c(1, 1),
  align = "h",
  labels = c("a)", "b)"),
  label_size = 18,
  label_fontface = "bold",
  label_y = 1.08
)

row_flood <- ggdraw() +
  draw_plot(row_flood, x = 0.07, y = 0, width = 0.93, height = 1) +
  draw_label("Flood", x = 0.01, y = 0.5, angle = 90, size = 22, hjust = 0.5)

row_drought <- plot_grid(
  drought_neg_low, drought_neg_high,
  nrow = 1, rel_widths = c(1, 1),
  align = "h",
  labels = c("c)", "d)"),
  label_size = 18,
  label_fontface = "bold",
  label_y = 1.08
)

row_drought <- ggdraw() +
  draw_plot(row_drought, x = 0.07, y = 0, width = 0.93, height = 1) +
  draw_label("Drought", x = 0.01, y = 0.5, angle = 90, size = 22, hjust = 0.5)


row_tc <- plot_grid(
  tc_neg_low, tc_neg_high,
  nrow = 1, rel_widths = c(1, 1),
  align = "h",
  labels = c("e)", "f)"),
  label_size = 18,
  label_fontface = "bold",
  label_y = 1.08
)

row_tc <- ggdraw() +
  draw_plot(row_tc, x = 0.07, y = 0, width = 0.93, height = 1) +
  draw_label("Trop cyclone", x = 0.01, y = 0.5, angle = 90, size = 22, hjust = 0.5)


# Add column titles

titles <- plot_grid(
  ggdraw() + draw_label("Low-to-Lower-Middle", fontface = "bold", size = 24, hjust = 0.5),
  ggdraw() + draw_label("Upper-Middle-to-High", fontface = "bold", size = 24, hjust = 0.5),
  nrow = 1
)

# Final full figure 

final_plot <- plot_grid(
  titles,
  row_flood,
  row_drought,
  row_tc,
  ncol = 1,
  rel_heights = c(0.1, 1, 1, 1)
)

# Save  image 

ggsave(
  filename = "fig4_income.png",
  plot = final_plot,
  width = 13,
  height = 12,
  dpi = 300,
  bg = "white"
)

# POSITIVE 
# Create the plot
flood_pos_low <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(aes(color = "Trend"), size = 2) + # Make the trend line thicker
  geom_point(aes(color = "Point Estimates"), size = 4) + # Make the points larger
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "lightblue", size = 1) + # Add error bars
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) + # Horizontal dashed line at y = 0
  scale_color_manual(values = c("Trend" = "orange", "Point Estimates" = "lightblue")) + # Custom colors
  labs(
    title = "",
    x = "",
    y = "Point Estimate of Net Migration"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.position = "none" # <--- Remove legend here
  ) +
  scale_y_continuous(
    limits = c(-2.5, 2.5),
    breaks = seq(-2.5, 2.5, by = 0.5)
  )

flood_pos_low

drought_pos_low <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(aes(color = "Trend"), size = 2) + # Make the trend line thicker
  geom_point(aes(color = "Point Estimates"), size = 4) + # Make the points larger
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "lightblue", size = 1) + # Add error bars
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) + # Horizontal dashed line at y = 0
  scale_color_manual(values = c("Trend" = "orange", "Point Estimates" = "lightblue")) + # Custom colors
  labs(
    title = "",
    x = "",
    y = "Point Estimate of Net Migration"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.position = "none" # <--- Remove legend here
  ) +
  scale_y_continuous(
    limits = c(-1, 1),
    breaks = c(-1,-0.8, -0.6,-0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
  )

drought_pos_low

tc_pos_low <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(aes(color = "Trend"), size = 2) + # Make the trend line thicker
  geom_point(aes(color = "Point Estimates"), size = 4) + # Make the points larger
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "lightblue", size = 1) + # Add error bars
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) + # Horizontal dashed line at y = 0
  scale_color_manual(values = c("Trend" = "orange", "Point Estimates" = "lightblue")) + # Custom colors
  labs(
    title = "",
    x = "",
    y = "Point Estimate of Net Migration"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 18),
    legend.position = "none" # <--- Remove legend here
  ) +
  scale_y_continuous(
    limits = c(-0.3, 0.3),
    breaks = c(-0.3,-0.2, -0.1, 0, 0.1, 0.2, 0.3)
  )

tc_pos_low

# positive net 
flood_pos_high <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(aes(color = "Trend"), size = 2) + # Make the trend line thicker
  geom_point(aes(color = "Point Estimates"), size = 4) + # Make the points larger
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "lightblue", size = 1) + # Add error bars
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) + # Horizontal dashed line at y = 0
  scale_color_manual(values = c("Trend" = "orange", "Point Estimates" = "lightblue")) + # Custom colors
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.position = "none" # <--- Remove legend here
  ) +
  scale_y_continuous(
    limits = c(-2.5, 2.5),
    breaks = seq(-2.5, 2.5, by = 0.5)
  )

flood_pos_high

drought_pos_high <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(aes(color = "Trend"), size = 2) + # Make the trend line thicker
  geom_point(aes(color = "Point Estimates"), size = 4) + # Make the points larger
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "lightblue", size = 1) + # Add error bars
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) + # Horizontal dashed line at y = 0
  scale_color_manual(values = c("Trend" = "orange", "Point Estimates" = "lightblue")) + # Custom colors
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.position = "none" # <--- Remove legend here
  ) +
  scale_y_continuous(
    limits = c(-0.4, 0.4),
    breaks = c(-0.4, -0.3,-0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4)
  )

drought_pos_high

tc_pos_high <- ggplot(df, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(aes(color = "Trend"), size = 2) + # Make the trend line thicker
  geom_point(aes(color = "Point Estimates"), size = 4) + # Make the points larger
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "lightblue", size = 1) + # Add error bars
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) + # Horizontal dashed line at y = 0
  scale_color_manual(values = c("Trend" = "orange", "Point Estimates" = "lightblue")) + # Custom colors
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 18),
    legend.position = "none" # <--- Remove legend here
  ) +
  scale_y_continuous(
    limits = c(-0.3, 0.3),
    breaks = c(-0.3,-0.2, -0.1, 0, 0.1, 0.2, 0.3)
  )

tc_pos_high

# Combine plots 
library(ggplot2)
library(cowplot)

# Set global white background 
theme_set(theme_minimal(base_size = 12) +
            theme(plot.background = element_rect(fill = "white", color = NA)))

# Y-axis label formatting
adjust_pos_plot <- function(p) {
  p + labs(y = "Point Estimate of Net Migration") +
    theme(
      plot.title = element_blank(),
      axis.title.y = element_text(size = 15, margin = margin(r = 1))
    )
}

# Adjust individual plots 

# posative plots 
flood_pos_low <- adjust_pos_plot(flood_pos_low)
drought_pos_low <- adjust_pos_plot(drought_pos_low + labs(x = NULL))
tc_pos_low <- adjust_pos_plot(tc_pos_low + labs(x = "Time lag") +
                                theme(axis.title.x = element_text(size = 24)))

# Right column
flood_pos_high <- flood_pos_high + labs(y = NULL, x = NULL) + theme(plot.title = element_blank())
drought_pos_high <- drought_pos_high + labs(y = NULL, x = NULL) + theme(plot.title = element_blank())
tc_pos_high <- tc_pos_high + labs(y = NULL, x = "Time lag") +
  theme(plot.title = element_blank(),
        axis.title.x = element_text(size = 24))

# Combine and label each row of plots 

row_flood <- plot_grid(
  flood_pos_low, flood_pos_high,
  nrow = 1, rel_widths = c(1, 1),
  align = "h",
  labels = c("a", "b"), label_size = 16, label_fontface = "bold"
)

row_drought <- plot_grid(
  drought_pos_low, drought_pos_high,
  nrow = 1, rel_widths = c(1, 1),
  align = "h",
  labels = c("c", "d"), label_size = 16, label_fontface = "bold"
)

row_tc <- plot_grid(
  tc_pos_low, tc_pos_high,
  nrow = 1, rel_widths = c(1, 1),
  align = "h",
  labels = c("e", "f"), label_size = 16, label_fontface = "bold"
)

#  Add row labels

row_flood <- plot_grid(
  flood_pos_low, flood_pos_high,
  nrow = 1, rel_widths = c(1, 1),
  align = "h",
  labels = c("a)", "b)"),
  label_size = 18,
  label_fontface = "bold",
  label_y = 1.08
)

row_flood <- ggdraw() +
  draw_plot(row_flood, x = 0.07, y = 0, width = 0.93, height = 1) +
  draw_label("Flood", x = 0.01, y = 0.5, angle = 90, size = 22, hjust = 0.5)

row_drought <- plot_grid(
  drought_pos_low, drought_pos_high,
  nrow = 1, rel_widths = c(1, 1),
  align = "h",
  labels = c("c)", "d)"),
  label_size = 18,
  label_fontface = "bold",
  label_y = 1.08
)

row_drought <- ggdraw() +
  draw_plot(row_drought, x = 0.07, y = 0, width = 0.93, height = 1) +
  draw_label("Drought", x = 0.01, y = 0.5, angle = 90, size = 22, hjust = 0.5)


row_tc <- plot_grid(
  tc_pos_low, tc_pos_high,
  nrow = 1, rel_widths = c(1, 1),
  align = "h",
  labels = c("e)", "f)"),
  label_size = 18,
  label_fontface = "bold",
  label_y = 1.08
)

row_tc <- ggdraw() +
  draw_plot(row_tc, x = 0.07, y = 0, width = 0.93, height = 1) +
  draw_label("Trop cyclone", x = 0.01, y = 0.5, angle = 90, size = 22, hjust = 0.5)


# Add column titles

titles <- plot_grid(
  ggdraw() + draw_label("Low-to-Lower-Middle", fontface = "bold", size = 24, hjust = 0.5),
  ggdraw() + draw_label("Upper-Middle-to-High", fontface = "bold", size = 24, hjust = 0.5),
  nrow = 1
)

# Final full figure

final_plot <- plot_grid(
  titles,
  row_flood,
  row_drought,
  row_tc,
  ncol = 1,
  rel_heights = c(0.1, 1, 1, 1)
)

# Save 

ggsave(
  filename = "fig4_income2.png",
  plot = final_plot,
  width = 13,
  height = 12,
  dpi = 300,
  bg = "white"
)

