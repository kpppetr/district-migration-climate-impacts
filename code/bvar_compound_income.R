# Compound events split by income group 
# TC

data_compound_tc_pos<-subset(net_pos, select = c("log_net_migration", 
                                                 "flood_threshold", "flood_threshold_1","flood_threshold_2", "flood_threshold_3", "flood_threshold_4", "flood_threshold_5",
                                                 "soilmoist_contin", "soilmoist_contin_1", "soilmoist_contin_2", "soilmoist_contin_3", "soilmoist_contin_4", "soilmoist_contin_5",
                                                 "top_cyc"  ,                    "top_cyc_1"   ,                 "top_cyc_2"  ,                  "top_cyc_3" ,                  
                                                 "top_cyc_4"   ,                 "top_cyc_5" ,
                                                 "log_armed_conflict_brd_lag1",   
                                                 "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                                 "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                                 "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                                 "shdi_lag1", "Income_Group"))

data_compound_tc_pos<-na.omit(data_compound_tc_pos)
names(data_compound_tc_pos)

compound_tc_pos_low_income <- subset(data_compound_tc_pos, Income_Group %in% c("L", "LM"))
compound_tc_pos_high_income <- subset(data_compound_tc_pos, Income_Group %in% c("UM", "H"))

compound_tc_pos_low_income <- subset(compound_tc_pos_low_income, select = -Income_Group)
compound_tc_pos_high_income <- subset(compound_tc_pos_high_income, select = -Income_Group)
names(compound_tc_pos_low_income)

# Run the model
compound_tc_pos_LI <- run_bvar_with_auto_lags(compound_tc_pos_low_income, priors = priors, mh = mh)
compound_tc_pos_HI <- run_bvar_with_auto_lags(compound_tc_pos_high_income, priors = priors, mh = mh)

data_compound_tc_neg<-subset(net_neg, select = c("log_net_migration", 
                                                 "flood_threshold", "flood_threshold_1","flood_threshold_2", "flood_threshold_3", "flood_threshold_4", "flood_threshold_5",
                                                 "soilmoist_contin", "soilmoist_contin_1", "soilmoist_contin_2", "soilmoist_contin_3", "soilmoist_contin_4", "soilmoist_contin_5",
                                                 "top_cyc"  ,                    "top_cyc_1"   ,                 "top_cyc_2"  ,                  "top_cyc_3" ,                  
                                                 "top_cyc_4"   ,                 "top_cyc_5" ,
                                                 "log_armed_conflict_brd_lag1",   
                                                 "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                                 "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                                 "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                                 "shdi_lag1", "Income_Group"))
data_compound_tc_neg<-na.omit(data_compound_tc_neg)

compound_tc_neg_low_income <- subset(data_compound_tc_neg, Income_Group %in% c("L", "LM"))
compound_tc_neg_high_income <- subset(data_compound_tc_neg, Income_Group %in% c("UM", "H"))

compound_tc_neg_low_income <- subset(compound_tc_neg_low_income, select = -Income_Group)
compound_tc_neg_high_income <- subset(compound_tc_neg_high_income, select = -Income_Group)
names(compound_tc_neg_low_income)

# Run the model
compound_tc_neg_LI <- run_bvar_with_auto_lags(compound_tc_neg_low_income, priors = priors, mh = mh)
compound_tc_neg_HI <- run_bvar_with_auto_lags(compound_tc_neg_high_income, priors = priors, mh = mh)

tc_index <- which(compound_tc_neg_LI$variables == "top_cyc_1")
tc_index_01 <- which(compound_tc_neg_LI$variables == "tc_0_1")
tc_index_012 <- which(compound_tc_neg_LI$variables == "tc_0_1_2")
tc_index_0123 <- which(compound_tc_neg_LI$variables == "tc_0_1_2_3") 


log_net_migration_index<- which(compound_tc_neg_LI$variables == "log_net_migration")


# Extracting the posterior samples for this coefficient
# Assuming it's the first response variable (index 1 in the third dimension if it's multidimensional)
posterior_samples_1 <- compound_tc_neg_LI$beta[, tc_index, log_net_migration_index]
posterior_samples_2 <- compound_tc_neg_LI$beta[, tc_index_01, log_net_migration_index]
posterior_samples_3 <- compound_tc_neg_LI$beta[, tc_index_012, log_net_migration_index]
posterior_samples_4 <- compound_tc_neg_LI$beta[, tc_index_0123, log_net_migration_index]



CI_lower_1 <- quantile(posterior_samples_1, probs = 0.05)
CI_upper_1 <- quantile(posterior_samples_1, probs = 0.95)

CI_lower_2 <- quantile(posterior_samples_2, probs = 0.05)
CI_upper_2 <- quantile(posterior_samples_2, probs = 0.95)

CI_lower_3 <- quantile(posterior_samples_3, probs = 0.05)
CI_upper_3 <- quantile(posterior_samples_3, probs = 0.95)

CI_lower_4 <- quantile(posterior_samples_4, probs = 0.05)
CI_upper_4 <- quantile(posterior_samples_4, probs = 0.95)



point_estimate_1 <- median(posterior_samples_1)
point_estimate_2 <- median(posterior_samples_2)
point_estimate_3 <- median(posterior_samples_3)
point_estimate_4 <- median(posterior_samples_4)


df1 <- data.frame(
  Effect = c("4-year", "3-year","2-year", "Single tc"),
  PointEstimate = c(point_estimate_4,point_estimate_3, point_estimate_2, point_estimate_1) ,
  CI_Lower = c( CI_lower_4, CI_lower_3, CI_lower_2, CI_lower_1),  
  CI_Upper = c(CI_upper_4, CI_upper_3 , CI_upper_2, CI_upper_1)
)

df1$Effect <- factor(df1$Effect, levels = c("Single tc", "2-year", "3-year", "4-year"))
print(df1)

tc_compound_neg_low <- ggplot(df1, aes(x = Effect, y = PointEstimate, group = 1)) +
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
    limits = c(-2, 2),
    breaks = seq(-2, 2, by = 0.5)
  )

tc_compound_neg_low

tc_compound_neg_high <- ggplot(df1, aes(x = Effect, y = PointEstimate, group = 1)) +
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
    limits = c(-2, 2),
    breaks = seq(-2, 2, by = 0.5)
  )

tc_compound_neg_high

# FLOOD
data_compound_flood_neg<-subset(net_neg, select = c("log_net_migration", 
                                                    "flood_threshold", "flood_threshold_1","flood_threshold_2", "flood_threshold_3", "flood_threshold_4", "flood_threshold_5",
                                                    "soilmoist_contin", "soilmoist_contin_1", "soilmoist_contin_2", "soilmoist_contin_3", "soilmoist_contin_4", "soilmoist_contin_5",
                                                    "top_cyc"  ,                    "top_cyc_1"   ,                 "top_cyc_2"  ,                  "top_cyc_3" ,                  
                                                    "top_cyc_4"   ,                 "top_cyc_5" ,
                                                    "log_armed_conflict_brd_lag1",   
                                                    "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                                    "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                                    "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                                    "shdi_lag1", "Income_Group"))
data_compound_flood_neg<-na.omit(data_compound_flood_neg)


data_compound_flood_pos<-subset(net_pos, select = c("log_net_migration", 
                                                    "flood_threshold", "flood_threshold_1","flood_threshold_2", "flood_threshold_3", "flood_threshold_4", "flood_threshold_5",
                                                    "soilmoist_contin", "soilmoist_contin_1", "soilmoist_contin_2", "soilmoist_contin_3", "soilmoist_contin_4", "soilmoist_contin_5",
                                                    "top_cyc"  ,                    "top_cyc_1"   ,                 "top_cyc_2"  ,                  "top_cyc_3" ,                  
                                                    "top_cyc_4"   ,                 "top_cyc_5" ,
                                                    "log_armed_conflict_brd_lag1",   
                                                    "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                                    "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                                    "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                                    "shdi_lag1", "Income_Group"))
data_compound_flood_pos<-na.omit(data_compound_flood_pos)


names(data_compound_flood_pos)
compound_flood_pos_low_income <- subset(data_compound_flood_pos, Income_Group %in% c("L", "LM"))
compound_flood_pos_high_income <- subset(data_compound_flood_pos, Income_Group %in% c("UM", "H"))

compound_flood_pos_low_income <- subset(compound_flood_pos_low_income, select = -Income_Group)
compound_flood_pos_high_income <- subset(compound_flood_pos_high_income, select = -Income_Group)
names(compound_flood_pos_low_income)

# Run the model
compound_flood_pos_LI <- run_bvar_with_auto_lags(compound_flood_pos_low_income, priors = priors, mh = mh)
compound_flood_pos_HI <- run_bvar_with_auto_lags(compound_flood_pos_high_income, priors = priors, mh = mh)

compound_flood_neg_low_income <- subset(data_compound_flood_neg, Income_Group %in% c("L", "LM"))
compound_flood_neg_high_income <- subset(data_compound_flood_neg, Income_Group %in% c("UM", "H"))

compound_flood_neg_low_income <- subset(compound_flood_neg_low_income, select = -Income_Group)
compound_flood_neg_high_income <- subset(compound_flood_neg_high_income, select = -Income_Group)
names(compound_flood_neg_low_income)

# Run the model
compound_flood_neg_LI <- run_bvar_with_auto_lags(compound_flood_neg_low_income, priors = priors, mh = mh)
compound_flood_neg_HI <- run_bvar_with_auto_lags(compound_flood_neg_high_income, priors = priors, mh = mh)


flood_index <- which(compound_flood_neg_LI$variables == "flood_threshold_1")
flood_index_01 <- which(compound_flood_neg_LI$variables == "flood_0_1")
flood_index_012 <- which(compound_flood_neg_LI$variables == "flood_0_1_2")
flood_index_0123 <- which(compound_flood_neg_LI$variables == "flood_0_1_2_3") 

log_net_migration_index<- which(compound_flood_neg_HI$variables == "log_net_migration")

# Extracting the posterior samples for this coefficient
# Assuming it's the first response variable (index 1 in the third dimension if it's multidimensional)
posterior_samples_1 <- compound_flood_neg_LI$beta[, flood_index, log_net_migration_index]
posterior_samples_2 <- compound_flood_neg_LI$beta[, flood_index_01, log_net_migration_index]
posterior_samples_3 <- compound_flood_neg_LI$beta[, flood_index_012, log_net_migration_index]
posterior_samples_4 <- compound_flood_neg_LI$beta[, flood_index_0123, log_net_migration_index]

CI_lower_1 <- quantile(posterior_samples_1, probs = 0.05)
CI_upper_1 <- quantile(posterior_samples_1, probs = 0.95)

CI_lower_2 <- quantile(posterior_samples_2, probs = 0.05)
CI_upper_2 <- quantile(posterior_samples_2, probs = 0.95)

CI_lower_3 <- quantile(posterior_samples_3, probs = 0.05)
CI_upper_3 <- quantile(posterior_samples_3, probs = 0.95)

CI_lower_4 <- quantile(posterior_samples_4, probs = 0.05)
CI_upper_4 <- quantile(posterior_samples_4, probs = 0.95)



point_estimate_1 <- median(posterior_samples_1)
point_estimate_2 <- median(posterior_samples_2)
point_estimate_3 <- median(posterior_samples_3)
point_estimate_4 <- median(posterior_samples_4)



df2 <- data.frame(
  Effect = c("4-year", "3-year", "2-year", "Single flood"),
  PointEstimate = c(point_estimate_4, point_estimate_3, point_estimate_2, point_estimate_1),
  CI_Lower = c(CI_lower_4, CI_lower_3, CI_lower_2, CI_lower_1),
  CI_Upper = c(CI_upper_4, CI_upper_3, CI_upper_2, CI_upper_1)
)

df2$Effect <- factor(df2$Effect, levels = c("Single flood", "2-year", "3-year", "4-year"))
print(df2)

flood_compound_neg_low <- ggplot(df2, aes(x = Effect, y = PointEstimate, group = 1)) +
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
    limits = c(-2, 2),
    breaks = seq(-2, 2, by = 0.5)
  )

flood_compound_neg_low

flood_compound_neg_high <- ggplot(df2, aes(x = Effect, y = PointEstimate, group = 1)) +
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
    limits = c(-2, 2),
    breaks = seq(-2, 2, by = 0.5)
  )

flood_compound_neg_high

# DROUGHT
data_compound_drought_neg<-subset(net_neg, select = c("log_net_migration", 
                                                      "flood_threshold", "flood_threshold_1","flood_threshold_2", "flood_threshold_3", "flood_threshold_4", "flood_threshold_5",
                                                      "soilmoist_contin", "soilmoist_contin_1", "soilmoist_contin_2", "soilmoist_contin_3", "soilmoist_contin_4", "soilmoist_contin_5",
                                                      "top_cyc"  ,                    "top_cyc_1"   ,                 "top_cyc_2"  ,                  "top_cyc_3" ,                  
                                                      "top_cyc_4"   ,                 "top_cyc_5" ,
                                                      "log_armed_conflict_brd_lag1",   
                                                      "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                                      "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                                      "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                                      "shdi_lag1", "Income_Group"))

data_compound_drought_neg<-na.omit(data_compound_drought_neg)

data_compound_drought_pos<-subset(net_pos, select = c("log_net_migration", 
                                                      "flood_threshold", "flood_threshold_1","flood_threshold_2", "flood_threshold_3", "flood_threshold_4", "flood_threshold_5",
                                                      "soilmoist_contin", "soilmoist_contin_1", "soilmoist_contin_2", "soilmoist_contin_3", "soilmoist_contin_4", "soilmoist_contin_5",
                                                      "top_cyc"  ,                    "top_cyc_1"   ,                 "top_cyc_2"  ,                  "top_cyc_3" ,                  
                                                      "top_cyc_4"   ,                 "top_cyc_5" ,
                                                      "log_armed_conflict_brd_lag1",   
                                                      "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                                      "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                                      "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                                      "shdi_lag1", "Income_Group"))

data_compound_drought_pos<-na.omit(data_compound_drought_pos)

# drought 
names(data_compound_drought_pos)
compound_drought_pos_low_income <- subset(data_compound_drought_pos, Income_Group %in% c("L", "LM"))
compound_drought_pos_high_income <- subset(data_compound_drought_pos, Income_Group %in% c("UM", "H"))

compound_drought_pos_low_income <- subset(compound_drought_pos_low_income, select = -Income_Group)
compound_drought_pos_high_income <- subset(compound_drought_pos_high_income, select = -Income_Group)
names(compound_drought_pos_low_income)

# Run the model
compound_drought_pos_LI <- run_bvar_with_auto_lags(compound_drought_pos_low_income, priors = priors, mh = mh)
compound_drought_pos_HI <- run_bvar_with_auto_lags(compound_drought_pos_high_income, priors = priors, mh = mh)

compound_drought_neg_low_income <- subset(data_compound_drought_neg, Income_Group %in% c("L", "LM"))
compound_drought_neg_high_income <- subset(data_compound_drought_neg, Income_Group %in% c("UM", "H"))

compound_drought_neg_low_income <- subset(compound_drought_neg_low_income, select = -Income_Group)
compound_drought_neg_high_income <- subset(compound_drought_neg_high_income, select = -Income_Group)
names(compound_drought_neg_low_income)

# Run the model
compound_drought_neg_LI <- run_bvar_with_auto_lags(compound_drought_neg_low_income, priors = priors, mh = mh)
compound_drought_neg_HI <- run_bvar_with_auto_lags(compound_drought_neg_high_income, priors = priors, mh = mh)

drought_index_1 <- which(compound_drought_neg_LI$variables == "soilmoist_contin_1")
drought_index_12 <- which(compound_drought_neg_LI$variables == "drought_0_1")
drought_index_123 <- which(compound_drought_neg_LI$variables == "drought_0_1_2")
drought_index_1234 <- which(compound_drought_neg_LI$variables == "drought_0_1_2_3") 

log_net_migration_index<- which(compound_drought_neg_LI$variables == "log_net_migration")


# Extracting the posterior samples for this coefficient
# Assuming it's the first response variable (index 1 in the third dimension if it's multidimensional)
posterior_samples_1 <- compound_drought_neg_LI$beta[, drought_index_1, log_net_migration_index]
posterior_samples_2 <- compound_drought_neg_LI$beta[, drought_index_12, log_net_migration_index]
posterior_samples_3 <- compound_drought_neg_LI$beta[, drought_index_123, log_net_migration_index]
posterior_samples_4 <- compound_drought_neg_LI$beta[, drought_index_1234, log_net_migration_index]



CI_lower_1 <- quantile(posterior_samples_1, probs = 0.05)
CI_upper_1 <- quantile(posterior_samples_1, probs = 0.95)

CI_lower_2 <- quantile(posterior_samples_2, probs = 0.05)
CI_upper_2 <- quantile(posterior_samples_2, probs = 0.95)

CI_lower_3 <- quantile(posterior_samples_3, probs = 0.05)
CI_upper_3 <- quantile(posterior_samples_3, probs = 0.95)

CI_lower_4 <- quantile(posterior_samples_4, probs = 0.05)
CI_upper_4 <- quantile(posterior_samples_4, probs = 0.95)


point_estimate_1 <- median(posterior_samples_1)
point_estimate_2 <- median(posterior_samples_2)
point_estimate_3 <- median(posterior_samples_3)
point_estimate_4 <- median(posterior_samples_4)


df3 <- data.frame(
  Effect = c("4-year", "3-year", "2-year", "Single drought"),
  PointEstimate = c(point_estimate_4, point_estimate_3, point_estimate_2, point_estimate_1),
  CI_Lower = c(CI_lower_4, CI_lower_3, CI_lower_2, CI_lower_1),
  CI_Upper = c(CI_upper_4, CI_upper_3, CI_upper_2, CI_upper_1)
)

df3$Effect <- factor(df3$Effect, levels = c("Single drought", "2-year", "3-year", "4-year"))
print(df3)

drought_compound_neg_low <- ggplot(df3, aes(x = Effect, y = PointEstimate, group = 1)) +
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
    limits = c(-2, 2),
    breaks = seq(-2, 2, by = 0.5)
  )

drought_compound_neg_low

drought_compound_neg_high <- ggplot(df3, aes(x = Effect, y = PointEstimate, group = 1)) +
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
    limits = c(-2, 2),
    breaks = seq(-2, 2, by = 0.5)
  )

drought_compound_neg_high

# COMBINE NEGATIVE NET

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

# posative plots (left column)
flood_compound_neg_low <- adjust_pos_plot(flood_compound_neg_low)
drought_compound_neg_low <- adjust_pos_plot(drought_compound_neg_low + labs(x = NULL))
tc_compound_neg_low <- adjust_pos_plot(tc_compound_neg_low + labs(x = "Time lag") +
                                         theme(axis.title.x = element_text(size = 24)))

# Right column, no y-axis label)
flood_compound_neg_high <- flood_compound_neg_high + labs(y = NULL, x = NULL) + theme(plot.title = element_blank())
drought_compound_neg_high <- drought_compound_neg_high + labs(y = NULL, x = NULL) + theme(plot.title = element_blank())
tc_compound_neg_high <- tc_compound_neg_high + labs(y = NULL, x = "Time lag") +
  theme(plot.title = element_blank(),
        axis.title.x = element_text(size = 24))

# Combine and label each row of plots 

row_flood <- plot_grid(
  flood_compound_neg_low, flood_compound_neg_high,
  nrow = 1, rel_widths = c(1, 1),
  align = "h",
  labels = c("a", "b"), label_size = 16, label_fontface = "bold"
)

row_drought <- plot_grid(
  drought_compound_neg_low, drought_compound_neg_high,
  nrow = 1, rel_widths = c(1, 1),
  align = "h",
  labels = c("c", "d"), label_size = 16, label_fontface = "bold"
)

row_tc <- plot_grid(
  tc_compound_neg_low, tc_compound_neg_high,
  nrow = 1, rel_widths = c(1, 1),
  align = "h",
  labels = c("e", "f"), label_size = 16, label_fontface = "bold"
)

# Add row labels

row_flood <- plot_grid(
  flood_compound_neg_low, flood_compound_neg_high,
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
  drought_compound_neg_low, drought_compound_neg_high,
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
  tc_compound_neg_low, tc_compound_neg_high,
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

#  Final full figure 

final_plot <- plot_grid(
  titles,
  row_flood,
  row_drought,
  row_tc,
  ncol = 1,
  rel_heights = c(0.1, 1, 1, 1)
)

# Save high-res image

ggsave(
  filename = "fig5_income.png",
  plot = final_plot,
  width = 13,
  height = 12,
  dpi = 300,
  bg = "white"
)
 # COMBINE POSITIVE NET 
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

# posative plots (left column)
flood_compound_pos_low <- adjust_pos_plot(flood_compound_pos_low)
drought_compound_pos_low <- adjust_pos_plot(drought_compound_pos_low + labs(x = NULL))
tc_compound_pos_low <- adjust_pos_plot(tc_compound_pos_low + labs(x = "Time lag") +
                                         theme(axis.title.x = element_text(size = 24)))

# Right column, no y-axis label
flood_compound_pos_high <- flood_compound_pos_high + labs(y = NULL, x = NULL) + theme(plot.title = element_blank())
drought_compound_pos_high <- drought_compound_pos_high + labs(y = NULL, x = NULL) + theme(plot.title = element_blank())
tc_compound_pos_high <- tc_compound_pos_high + labs(y = NULL, x = "Time lag") +
  theme(plot.title = element_blank(),
        axis.title.x = element_text(size = 24))

# Combine and label each row of plots 

row_flood <- plot_grid(
  flood_compound_pos_low, flood_compound_pos_high,
  nrow = 1, rel_widths = c(1, 1),
  align = "h",
  labels = c("a", "b"), label_size = 16, label_fontface = "bold"
)

row_drought <- plot_grid(
  drought_compound_pos_low, drought_compound_pos_high,
  nrow = 1, rel_widths = c(1, 1),
  align = "h",
  labels = c("c", "d"), label_size = 16, label_fontface = "bold"
)

row_tc <- plot_grid(
  tc_compound_pos_low, tc_compound_pos_high,
  nrow = 1, rel_widths = c(1, 1),
  align = "h",
  labels = c("e", "f"), label_size = 16, label_fontface = "bold"
)

#  Add row labels

row_flood <- plot_grid(
  flood_compound_pos_low, flood_compound_pos_high,
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
  drought_compound_pos_low, drought_compound_pos_high,
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
  tc_compound_pos_low, tc_compound_pos_high,
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

#  Final full figure

final_plot <- plot_grid(
  titles,
  row_flood,
  row_drought,
  row_tc,
  ncol = 1,
  rel_heights = c(0.1, 1, 1, 1)
)

# Save high-res image 

ggsave(
  filename = "fig5_income2.png",
  plot = final_plot,
  width = 13,
  height = 12,
  dpi = 300,
  bg = "white"
)
