data <- readRDS("data.rds")
# Compound events split by income group 
# TC

data_compound_tc_pos<-subset(net_pos, select = c("log_net_migration", 
                                                 "fld_share_pop", "fld_share_pop_1","fld_share_pop_2", "fld_share_pop_3", "fld_share_pop_4", "fld_share_pop_5",
                                                 "drt_share_pop", "drt_share_pop_1", "drt_share_pop_2", "drt_share_pop_3", "drt_share_pop_4", "drt_share_pop_5",
                                                 "cyc_share_pop"  ,                    "cyc_share_pop_1"   ,                 "cyc_share_pop_2"  ,                  "cyc_share_pop_3" ,                  
                                                 "cyc_share_pop_4"   ,                 "cyc_share_pop_5" ,
                                                 "log_armed_conflict_brd_lag1",   
                                                 "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                                 "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                                 "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                                 "shdi_lag1", "Income_Group"))

data_compound_tc_pos<-na.omit(data_compound_tc_pos)
names(data_compound_tc_pos)

data_compound_tc_pos$tc_0_1 <- ifelse(data_compound_tc_pos$cyc_share_pop > 0 & data_compound_tc_pos$cyc_share_pop_1 > 0,1, 0) 

data_compound_tc_pos$tc_0_1_2 <- ifelse(data_compound_tc_pos$cyc_share_pop > 0 & data_compound_tc_pos$cyc_share_pop_1 > 0 & data_compound_tc_pos$cyc_share_pop_2 > 0,1, 0 )

data_compound_tc_pos$tc_0_1_2_3 <- ifelse(data_compound_tc_pos$cyc_share_pop > 0 & data_compound_tc_pos$cyc_share_pop_1 > 0 & data_compound_tc_pos$cyc_share_pop_2 > 0
                                          & data_compound_tc_pos$cyc_share_pop_3 > 0,1, 0) 

summary(data_compound_tc_pos$tc_0_1_2_3)


compound_tc_pos_low_income <- subset(data_compound_tc_pos, Income_Group %in% c("L", "LM"))
compound_tc_pos_high_income <- subset(data_compound_tc_pos, Income_Group %in% c("UM", "H"))

compound_tc_pos_low_income <- subset(compound_tc_pos_low_income, select = -Income_Group)
compound_tc_pos_high_income <- subset(compound_tc_pos_high_income, select = -Income_Group)
names(compound_tc_pos_low_income)

# Run the model
compound_tc_pos_LI <- run_bvar_with_auto_lags(compound_tc_pos_low_income, priors = priors, mh = mh)
compound_tc_pos_HI <- run_bvar_with_auto_lags(compound_tc_pos_high_income, priors = priors, mh = mh)

data_compound_tc_neg<-subset(net_neg, select = c("log_net_migration", 
                                                 "fld_share_pop", "fld_share_pop_1","fld_share_pop_2", "fld_share_pop_3", "fld_share_pop_4", "fld_share_pop_5",
                                                 "drt_share_pop", "drt_share_pop_1", "drt_share_pop_2", "drt_share_pop_3", "drt_share_pop_4", "drt_share_pop_5",
                                                 "cyc_share_pop"  ,                    "cyc_share_pop_1"   ,                 "cyc_share_pop_2"  ,                  "cyc_share_pop_3" ,                  
                                                 "cyc_share_pop_4"   ,                 "cyc_share_pop_5" ,
                                                 "log_armed_conflict_brd_lag1",   
                                                 "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                                 "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                                 "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                                 "shdi_lag1", "Income_Group"))
data_compound_tc_neg<-na.omit(data_compound_tc_neg)

data_compound_tc_neg$tc_0_1 <- ifelse(data_compound_tc_neg$cyc_share_pop > 0 & data_compound_tc_neg$cyc_share_pop_1 > 0,1, 0) 

data_compound_tc_neg$tc_0_1_2 <- ifelse(data_compound_tc_neg$cyc_share_pop > 0 & data_compound_tc_neg$cyc_share_pop_1 > 0 & data_compound_tc_neg$cyc_share_pop_2 > 0,1, 0 )

data_compound_tc_neg$tc_0_1_2_3 <- ifelse(data_compound_tc_neg$cyc_share_pop > 0 & data_compound_tc_neg$cyc_share_pop_1 > 0 & data_compound_tc_neg$cyc_share_pop_2 > 0
                                          & data_compound_tc_neg$cyc_share_pop_3 > 0,1, 0) 

names(data_compound_tc_neg)
compound_tc_neg_low_income <- subset(data_compound_tc_neg, Income_Group %in% c("L", "LM"))
compound_tc_neg_high_income <- subset(data_compound_tc_neg, Income_Group %in% c("UM", "H"))

compound_tc_neg_low_income <- subset(compound_tc_neg_low_income, select = -Income_Group)
compound_tc_neg_high_income <- subset(compound_tc_neg_high_income, select = -Income_Group)
names(compound_tc_neg_low_income)

# Run the model
compound_tc_neg_LI <- run_bvar_with_auto_lags(compound_tc_neg_low_income, priors = priors, mh = mh)
compound_tc_neg_HI <- run_bvar_with_auto_lags(compound_tc_neg_high_income, priors = priors, mh = mh)

tc_index <- which(compound_tc_neg_HI$variables == "cyc_share_pop")
tc_index_01 <- which(compound_tc_neg_HI$variables == "tc_0_1")
tc_index_012 <- which(compound_tc_neg_HI$variables == "tc_0_1_2")
tc_index_0123 <- which(compound_tc_neg_HI$variables == "tc_0_1_2_3") 


log_net_migration_index<- which(compound_tc_neg_HI$variables == "log_net_migration")


# Extracting the posterior samples for this coefficient
# Assuming it's the first response variable (index 1 in the third dimension if it's multidimensional)
posterior_samples_1 <- compound_tc_neg_HI$beta[, tc_index, log_net_migration_index]
posterior_samples_2 <- compound_tc_neg_HI$beta[, tc_index_01, log_net_migration_index]
posterior_samples_3 <- compound_tc_neg_HI$beta[, tc_index_012, log_net_migration_index]
posterior_samples_4 <- compound_tc_neg_HI$beta[, tc_index_0123, log_net_migration_index]



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

tc_compound_pos_low <- ggplot(df1, aes(x = Effect, y = PointEstimate, group = 1)) +
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
    linetype = "dashed",
    color = "black",
    linewidth = 0.5
  ) +
  labs(
    x = "Time lag",
    y = "Point estimate of net migration"
  ) +
  scale_y_continuous(
    limits = c(-2, 2),
    breaks = seq(-2, 2, by = 0.5)
  ) +
  theme_nature_panel

tc_compound_pos_low

tc_compound_pos_high <- ggplot(df1, aes(x = Effect, y = PointEstimate, group = 1)) +
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
    linetype = "dashed",
    color = "black",
    linewidth = 0.5
  ) +
  labs(
    x = "Time lag",
    y = NULL
  ) +
  scale_y_continuous(
    limits = c(-2, 2),
    breaks = seq(-2, 2, by = 0.5)
  ) +
  theme_nature_panel

tc_compound_pos_high

tc_compound_neg_low <- ggplot(df1, aes(x = Effect, y = PointEstimate, group = 1)) +
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
    linetype = "dashed",
    color = "black",
    linewidth = 0.5
  ) +
  labs(
    x = "Time lag",
    y = "Point estimate of net migration"
  ) +
  scale_y_continuous(
    limits = c(-2, 2),
    breaks = seq(-2, 2, by = 0.5)
  ) +
  theme_nature_panel

tc_compound_neg_low

tc_compound_neg_high <- ggplot(df1, aes(x = Effect, y = PointEstimate, group = 1)) +
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
    linetype = "dashed",
    color = "black",
    linewidth = 0.5
  ) +
  labs(
    x = "Time lag",
    y = NULL
  ) +
  scale_y_continuous(
    limits = c(-2, 2),
    breaks = seq(-2, 2, by = 0.5)
  ) +
  theme_nature_panel

tc_compound_neg_high
# FLOOD
data_compound_flood_neg<-subset(net_neg, select = c("log_net_migration", 
                                                    "fld_share_pop", "fld_share_pop_1","fld_share_pop_2", "fld_share_pop_3", "fld_share_pop_4", "fld_share_pop_5",
                                                    "drt_share_pop", "drt_share_pop_1", "drt_share_pop_2", "drt_share_pop_3", "drt_share_pop_4", "drt_share_pop_5",
                                                    "cyc_share_pop"  ,                    "cyc_share_pop_1"   ,                 "cyc_share_pop_2"  ,                  "cyc_share_pop_3" ,                  
                                                    "cyc_share_pop_4"   ,                 "cyc_share_pop_5" ,
                                                    "log_armed_conflict_brd_lag1",   
                                                    "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                                    "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                                    "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                                    "shdi_lag1", "Income_Group"))
data_compound_flood_neg<-na.omit(data_compound_flood_neg)

data_compound_flood_neg$flood_0_1 <- ifelse(data_compound_flood_neg$fld_share_pop > 0 & data_compound_flood_neg$fld_share_pop_1 > 0,1, 0) 

data_compound_flood_neg$flood_0_1_2 <- ifelse(data_compound_flood_neg$fld_share_pop > 0 & data_compound_flood_neg$fld_share_pop_1 > 0 & data_compound_flood_neg$fld_share_pop_2 > 0,1, 0 )

data_compound_flood_neg$flood_0_1_2_3 <- ifelse(data_compound_flood_neg$fld_share_pop > 0 & data_compound_flood_neg$fld_share_pop_1 > 0 & data_compound_flood_neg$fld_share_pop_2 > 0
                                                & data_compound_flood_neg$fld_share_pop_3 > 0,1, 0) 

names(data_compound_flood_neg)
compound_flood_neg_low_income <- subset(data_compound_flood_neg, Income_Group %in% c("L", "LM"))
compound_flood_neg_high_income <- subset(data_compound_flood_neg, Income_Group %in% c("UM", "H"))

compound_flood_neg_low_income <- subset(compound_flood_neg_low_income, select = -Income_Group)
compound_flood_neg_high_income <- subset(compound_flood_neg_high_income, select = -Income_Group)
names(compound_flood_neg_low_income)

data_compound_flood_pos<-subset(net_pos, select = c("log_net_migration", 
                                                    "fld_share_pop", "fld_share_pop_1","fld_share_pop_2", "fld_share_pop_3", "fld_share_pop_4", "fld_share_pop_5",
                                                    "drt_share_pop", "drt_share_pop_1", "drt_share_pop_2", "drt_share_pop_3", "drt_share_pop_4", "drt_share_pop_5",
                                                    "cyc_share_pop"  ,                    "cyc_share_pop_1"   ,                 "cyc_share_pop_2"  ,                  "cyc_share_pop_3" ,                  
                                                    "cyc_share_pop_4"   ,                 "cyc_share_pop_5" ,
                                                    "log_armed_conflict_brd_lag1",   
                                                    "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                                    "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                                    "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                                    "shdi_lag1", "Income_Group"))
data_compound_flood_pos<-na.omit(data_compound_flood_pos)

data_compound_flood_pos$flood_0_1 <- ifelse(data_compound_flood_pos$fld_share_pop > 0 & data_compound_flood_pos$fld_share_pop_1 > 0,1, 0) 

data_compound_flood_pos$flood_0_1_2 <- ifelse(data_compound_flood_pos$fld_share_pop > 0 & data_compound_flood_pos$fld_share_pop_1 > 0 & data_compound_flood_pos$fld_share_pop_2 > 0,1, 0 )

data_compound_flood_pos$flood_0_1_2_3 <- ifelse(data_compound_flood_pos$fld_share_pop > 0 & data_compound_flood_pos$fld_share_pop_1 > 0 & data_compound_flood_pos$fld_share_pop_2 > 0
                                                & data_compound_flood_pos$fld_share_pop_3 > 0,1, 0) 



names(data_compound_flood_pos)
compound_flood_pos_low_income <- subset(data_compound_flood_pos, Income_Group %in% c("L", "LM"))
compound_flood_pos_high_income <- subset(data_compound_flood_pos, Income_Group %in% c("UM", "H"))

compound_flood_pos_low_income <- subset(compound_flood_pos_low_income, select = -Income_Group)
compound_flood_pos_high_income <- subset(compound_flood_pos_high_income, select = -Income_Group)
names(compound_flood_pos_low_income)

# Run the model
compound_flood_pos_LI <- run_bvar_with_auto_lags(compound_flood_pos_low_income, priors = priors, mh = mh)
compound_flood_pos_HI <- run_bvar_with_auto_lags(compound_flood_pos_high_income, priors = priors, mh = mh)

# Run the model
compound_flood_neg_LI <- run_bvar_with_auto_lags(compound_flood_neg_low_income, priors = priors, mh = mh)
compound_flood_neg_HI <- run_bvar_with_auto_lags(compound_flood_neg_high_income, priors = priors, mh = mh)


flood_index <- which(compound_flood_neg_HI$variables == "fld_share_pop")
flood_index_01 <- which(compound_flood_neg_HI$variables == "flood_0_1")
flood_index_012 <- which(compound_flood_neg_HI$variables == "flood_0_1_2")
flood_index_0123 <- which(compound_flood_neg_HI$variables == "flood_0_1_2_3") 

log_net_migration_index<- which(compound_flood_neg_HI$variables == "log_net_migration")

# Extracting the posterior samples for this coefficient
# Assuming it's the first response variable (index 1 in the third dimension if it's multidimensional)
posterior_samples_1 <- compound_flood_neg_HI$beta[, flood_index, log_net_migration_index]
posterior_samples_2 <- compound_flood_neg_HI$beta[, flood_index_01, log_net_migration_index]
posterior_samples_3 <- compound_flood_neg_HI$beta[, flood_index_012, log_net_migration_index]
posterior_samples_4 <- compound_flood_neg_HI$beta[, flood_index_0123, log_net_migration_index]

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

## Flood – compound, POSITIVE net-migration (LOW income)
flood_compound_pos_low <- ggplot(df2, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(color = "orange", linewidth = 0.7) +
  geom_point(color = "lightblue", size = 2) +
  geom_errorbar(
    aes(ymin = CI_Lower, ymax = CI_Upper),
    width = 0.15, color = "lightblue", linewidth = 0.6
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black",
    linewidth = 0.5
  ) +
  labs(
    x = "Time lag",
    y = "Point estimate of net migration"
  ) +
  scale_y_continuous(
    limits = c(-2, 2),
    breaks = seq(-2, 2, by = 0.5)
  ) +
  theme_nature_panel

flood_compound_pos_low

## Flood – compound, NEGATIVE net-migration (HIGH income)
flood_compound_pos_high <- ggplot(df2, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(color = "orange", linewidth = 0.7) +
  geom_point(color = "lightblue", size = 2) +
  geom_errorbar(
    aes(ymin = CI_Lower, ymax = CI_Upper),
    width = 0.15, color = "lightblue", linewidth = 0.6
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black",
    linewidth = 0.5
  ) +
  labs(
    x = "Time lag",
    y = NULL
  ) +
  scale_y_continuous(
    limits = c(-2, 2),
    breaks = seq(-2, 2, by = 0.5)
  ) +
  theme_nature_panel

flood_compound_pos_high
## Flood – compound, NEGATIVE net-migration 
flood_compound_neg_low <- ggplot(df2, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(color = "orange", linewidth = 0.7) +
  geom_point(color = "lightblue", size = 2) +
  geom_errorbar(
    aes(ymin = CI_Lower, ymax = CI_Upper),
    width = 0.15, color = "lightblue", linewidth = 0.6
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black",
    linewidth = 0.5
  ) +
  labs(
    x = "Time lag",
    y = "Point estimate of net migration"
  ) +
  scale_y_continuous(
    limits = c(-2, 2),
    breaks = seq(-2, 2, by = 0.5)
  ) +
  theme_nature_panel

flood_compound_neg_low

## Flood – compound, NEGATIVE net-migration (HIGH income)
flood_compound_neg_high <- ggplot(df2, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(color = "orange", linewidth = 0.7) +
  geom_point(color = "lightblue", size = 2) +
  geom_errorbar(
    aes(ymin = CI_Lower, ymax = CI_Upper),
    width = 0.15, color = "lightblue", linewidth = 0.6
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black",
    linewidth = 0.5
  ) +
  labs(
    x = "Time lag",
    y = NULL
  ) +
  scale_y_continuous(
    limits = c(-2, 2),
    breaks = seq(-2, 2, by = 0.5)
  ) +
  theme_nature_panel

flood_compound_neg_high


# DROUGHT
data_compound_drought_neg<-subset(net_neg, select = c("log_net_migration", 
                                                      "fld_share_pop", "fld_share_pop_1","fld_share_pop_2", "fld_share_pop_3", "fld_share_pop_4", "fld_share_pop_5",
                                                      "drt_share_pop", "drt_share_pop_1", "drt_share_pop_2", "drt_share_pop_3", "drt_share_pop_4", "drt_share_pop_5",
                                                      "cyc_share_pop"  ,                    "cyc_share_pop_1"   ,                 "cyc_share_pop_2"  ,                  "cyc_share_pop_3" ,                  
                                                      "cyc_share_pop_4"   ,                 "cyc_share_pop_5" ,
                                                      "log_armed_confHIct_brd_lag1",   
                                                      "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                                      "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                                      "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                                      "shdi_lag1", "Income_Group"))

data_compound_drought_neg<-na.omit(data_compound_drought_neg)

data_compound_drought_neg$drought_0_1 <- ifelse(data_compound_drought_neg$drt_share_pop > 0 & data_compound_drought_neg$drt_share_pop_1 > 0,1, 0) 

data_compound_drought_neg$drought_0_1_2 <- ifelse(data_compound_drought_neg$drt_share_pop > 0 & data_compound_drought_neg$drt_share_pop_1 > 0 & data_compound_drought_neg$drt_share_pop_2 > 0,1, 0 )

data_compound_drought_neg$drought_0_1_2_3 <- ifelse(data_compound_drought_neg$drt_share_pop > 0 & data_compound_drought_neg$drt_share_pop_1 > 0 & data_compound_drought_neg$drt_share_pop_2 > 0
                                                    & data_compound_drought_neg$drt_share_pop_3 > 0,1, 0) 


data_compound_drought_pos<-subset(net_pos, select = c("log_net_migration", 
                                                      "fld_share_pop", "fld_share_pop_1","fld_share_pop_2", "fld_share_pop_3", "fld_share_pop_4", "fld_share_pop_5",
                                                      "drt_share_pop", "drt_share_pop_1", "drt_share_pop_2", "drt_share_pop_3", "drt_share_pop_4", "drt_share_pop_5",
                                                      "cyc_share_pop"  ,                    "cyc_share_pop_1"   ,                 "cyc_share_pop_2"  ,                  "cyc_share_pop_3" ,                  
                                                      "cyc_share_pop_4"   ,                 "cyc_share_pop_5" ,
                                                      "log_armed_conflict_brd_lag1",   
                                                      "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                                      "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                                      "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                                      "shdi_lag1", "Income_Group"))

data_compound_drought_pos<-na.omit(data_compound_drought_pos)
data_compound_drought_pos$drought_0_1 <- ifelse(data_compound_drought_pos$drt_share_pop > 0 & data_compound_drought_pos$drt_share_pop_1 > 0,1, 0) 

data_compound_drought_pos$drought_0_1_2 <- ifelse(data_compound_drought_pos$drt_share_pop > 0 & data_compound_drought_pos$drt_share_pop_1 > 0 & data_compound_drought_pos$drt_share_pop_2 > 0,1, 0 )

data_compound_drought_pos$drought_0_1_2_3 <- ifelse(data_compound_drought_pos$drt_share_pop > 0 & data_compound_drought_pos$drt_share_pop_1 > 0 & data_compound_drought_pos$drt_share_pop_2 > 0
                                                    & data_compound_drought_pos$drt_share_pop_3 > 0,1, 0) 


# drought 
names(data_compound_drought_pos)
compound_drought_pos_low_income <- subset(data_compound_drought_pos, Income_Group %in% c("L", "LM"))
compound_drought_pos_high_income <- subset(data_compound_drought_pos, Income_Group %in% c("UM", "H"))

compound_drought_pos_low_income <- subset(compound_drought_pos_low_income, select = -Income_Group)
compound_drought_pos_high_income <- subset(compound_drought_pos_high_income, select = -Income_Group)
names(compound_drought_pos_low_income)



compound_drought_neg_low_income <- subset(data_compound_drought_neg, Income_Group %in% c("L", "LM"))
compound_drought_neg_high_income <- subset(data_compound_drought_neg, Income_Group %in% c("UM", "H"))

compound_drought_neg_low_income <- subset(compound_drought_neg_low_income, select = -Income_Group)
compound_drought_neg_high_income <- subset(compound_drought_neg_high_income, select = -Income_Group)
names(compound_drought_neg_low_income)

# Run the model
compound_drought_pos_LI <- run_bvar_with_auto_lags(compound_drought_pos_low_income, priors = priors, mh = mh)
compound_drought_pos_HI <- run_bvar_with_auto_lags(compound_drought_pos_high_income, priors = priors, mh = mh)

# Run the model
compound_drought_neg_LI <- run_bvar_with_auto_lags(compound_drought_neg_low_income, priors = priors, mh = mh)
compound_drought_neg_HI <- run_bvar_with_auto_lags(compound_drought_neg_high_income, priors = priors, mh = mh)

drought_index_1 <- which(compound_drought_neg_HI$variables == "drt_share_pop")
drought_index_12 <- which(compound_drought_neg_HI$variables == "drought_0_1")
drought_index_123 <- which(compound_drought_neg_HI$variables == "drought_0_1_2")
drought_index_1234 <- which(compound_drought_neg_HI$variables == "drought_0_1_2_3") 

log_net_migration_index<- which(compound_drought_neg_HI$variables == "log_net_migration")


# Extracting the posterior samples for this coefficient
# Assuming it's the first response variable (index 1 in the third dimension if it's multidimensional)
posterior_samples_1 <- compound_drought_neg_HI$beta[, drought_index_1, log_net_migration_index]
posterior_samples_2 <- compound_drought_neg_HI$beta[, drought_index_12, log_net_migration_index]
posterior_samples_3 <- compound_drought_neg_HI$beta[, drought_index_123, log_net_migration_index]
posterior_samples_4 <- compound_drought_neg_HI$beta[, drought_index_1234, log_net_migration_index]



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

# positive
drought_compound_pos_low <- ggplot(df3, aes(x = Effect, y = PointEstimate, group = 1)) +
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
    linetype = "dashed",
    color = "black",
    linewidth = 0.5
  ) +
  labs(
    x = "Time lag",
    y = "Point estimate of net migration",
    title = ""
  ) +
  scale_y_continuous(
    limits = c(-2, 2),
    breaks = seq(-2, 2, by = 0.5)
  ) +
  theme_nature_panel

drought_compound_pos_low

drought_compound_pos_high <- ggplot(df3, aes(x = Effect, y = PointEstimate, group = 1)) +
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
    linetype = "dashed",
    color = "black",
    linewidth = 0.5
  ) +
  labs(
    x = "Time lag",
    y = NULL,
    title = ""
  ) +
  scale_y_continuous(
    limits = c(-2, 2),
    breaks = seq(-2, 2, by = 0.5)
  ) +
  theme_nature_panel

drought_compound_pos_high
# negative
drought_compound_neg_low <- ggplot(df3, aes(x = Effect, y = PointEstimate, group = 1)) +
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
    linetype = "dashed",
    color = "black",
    linewidth = 0.5
  ) +
  labs(
    x = "Time lag",
    y = "Point estimate of net migration",
    title = ""
  ) +
  scale_y_continuous(
    limits = c(-2, 2),
    breaks = seq(-2, 2, by = 0.5)
  ) +
  theme_nature_panel

drought_compound_neg_low

drought_compound_neg_high <- ggplot(df3, aes(x = Effect, y = PointEstimate, group = 1)) +
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
    linetype = "dashed",
    color = "black",
    linewidth = 0.5
  ) +
  labs(
    x = "Time lag",
    y = NULL,
    title = ""
  ) +
  scale_y_continuous(
    limits = c(-2, 2),
    breaks = seq(-2, 2, by = 0.5)
  ) +
  theme_nature_panel

drought_compound_neg_high


# COMBINE NEGATIVE NET

library(ggplot2)
library(cowplot)


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

# Extract legend grob
shared_legend <- cowplot::get_legend(legend_plot)


adjust_left_plot <- function(p, xlab = NULL) {
  p + labs(
    y = "Point estimate of net migration",
    x = xlab
  )
}


adjust_right_plot <- function(p, xlab = NULL) {
  p + labs(
    y = NULL,
    x = xlab
  )
}



# Left column (Low-to-Lower-Middle)
flood_low_p   <- adjust_left_plot(flood_compound_neg_low,   xlab = NULL)
drought_low_p <- adjust_left_plot(drought_compound_neg_low, xlab = NULL)
tc_low_p      <- adjust_left_plot(tc_compound_neg_low,      xlab = "Time lag")

# Right column (Upper-Middle-to-High)
flood_high_p   <- adjust_right_plot(flood_compound_neg_high,   xlab = NULL)
drought_high_p <- adjust_right_plot(drought_compound_neg_high, xlab = NULL)
tc_high_p      <- adjust_right_plot(tc_compound_neg_high,      xlab = "Time lag")


## Build rows with panel labels 

row_flood <- plot_grid(
  flood_low_p, flood_high_p,
  nrow           = 1,
  rel_widths     = c(1, 1),
  align          = "h",
  labels         = c("a", "b"),
  label_size     = 7,
  label_fontface = "bold",
  label_y        = 1.05
)

row_drought <- plot_grid(
  drought_low_p, drought_high_p,
  nrow           = 1,
  rel_widths     = c(1, 1),
  align          = "h",
  labels         = c("c", "d"),
  label_size     = 7,
  label_fontface = "bold",
  label_y        = 1.05
)

row_tc <- plot_grid(
  tc_low_p, tc_high_p,
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
    "Low-to-Lower-Middle",
    fontface = "bold",
    size     = 7,
    hjust    = 0.5
  ),
  ggdraw() + draw_label(
    "Upper-Middle-to-High",
    fontface = "bold",
    size     = 7,
    hjust    = 0.5
  ),
  nrow       = 1,
  rel_widths = c(1, 1)
)



neg_compound_body <- plot_grid(
  titles,
  row_flood,
  row_drought,
  row_tc,
  ncol        = 1,
  rel_heights = c(0.18, 1, 1, 1)
)

neg_compound_with_legend <- plot_grid(
  neg_compound_body,
  shared_legend,
  ncol        = 1,
  rel_heights = c(1, 0.12)
)


##  Save as PDF

w_in <- 188 / 25.4  
h_in <- 180 / 25.4  

quartz(
  file  = "FigS16.pdf",
  type  = "pdf",
  width = w_in,
  height = h_in
)

print(neg_compound_with_legend)
dev.off()

# POSITIVE

library(ggplot2)
library(cowplot)

##  Shared legend

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

# Extract legend grob
shared_legend <- cowplot::get_legend(legend_plot)




# Left column: show y-axis label
adjust_left_plot <- function(p, xlab = NULL) {
  p + labs(
    y = "Point estimate of net migration",
    x = xlab
  )
}

# Right column: no y-axis label
adjust_right_plot <- function(p, xlab = NULL) {
  p + labs(
    y = NULL,
    x = xlab
  )
}


## Apply to each panel


flood_low_p   <- adjust_left_plot(flood_compound_pos_low,   xlab = NULL)
drought_low_p <- adjust_left_plot(drought_compound_pos_low, xlab = NULL)
tc_low_p      <- adjust_left_plot(tc_compound_pos_low,      xlab = "Time lag")


flood_high_p   <- adjust_right_plot(flood_compound_pos_high,   xlab = NULL)
drought_high_p <- adjust_right_plot(drought_compound_pos_high, xlab = NULL)
tc_high_p      <- adjust_right_plot(tc_compound_pos_high,      xlab = "Time lag")


## Build rows with panel labels

row_flood_pos <- plot_grid(
  flood_low_p, flood_high_p,
  nrow           = 1,
  rel_widths     = c(1, 1),
  align          = "h",
  labels         = c("a", "b"),
  label_size     = 7,
  label_fontface = "bold",
  label_y        = 1.05
)

row_drought_pos <- plot_grid(
  drought_low_p, drought_high_p,
  nrow           = 1,
  rel_widths     = c(1, 1),
  align          = "h",
  labels         = c("c", "d"),
  label_size     = 7,
  label_fontface = "bold",
  label_y        = 1.05
)

row_tc_pos <- plot_grid(
  tc_low_p, tc_high_p,
  nrow           = 1,
  rel_widths     = c(1, 1),
  align          = "h",
  labels         = c("e", "f"),
  label_size     = 7,
  label_fontface = "bold",
  label_y        = 1.05
)


## Add left-hand row labels 

row_flood_pos <- ggdraw() +
  draw_plot(row_flood_pos, x = 0.08, y = 0, width = 0.92, height = 1) +
  draw_label(
    "Flood",
    x        = 0.015,
    y        = 0.5,
    angle    = 90,
    size     = 7,
    fontface = "bold",
    hjust    = 0.5
  )

row_drought_pos <- ggdraw() +
  draw_plot(row_drought_pos, x = 0.08, y = 0, width = 0.92, height = 1) +
  draw_label(
    "Drought",
    x        = 0.015,
    y        = 0.5,
    angle    = 90,
    size     = 7,
    fontface = "bold",
    hjust    = 0.5
  )

row_tc_pos <- ggdraw() +
  draw_plot(row_tc_pos, x = 0.08, y = 0, width = 0.92, height = 1) +
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

titles_pos <- plot_grid(
  ggdraw() + draw_label(
    "Low-to-Lower-Middle",
    fontface = "bold",
    size     = 7,
    hjust    = 0.5
  ),
  ggdraw() + draw_label(
    "Upper-Middle-to-High",
    fontface = "bold",
    size     = 7,
    hjust    = 0.5
  ),
  nrow       = 1,
  rel_widths = c(1, 1)
)


## Final layout with legend 

pos_compound_body <- plot_grid(
  titles_pos,
  row_flood_pos,
  row_drought_pos,
  row_tc_pos,
  ncol        = 1,
  rel_heights = c(0.18, 1, 1, 1)
)

pos_compound_with_legend <- plot_grid(
  pos_compound_body,
  shared_legend,
  ncol        = 1,
  rel_heights = c(1, 0.12)
)


## Save as PDF 

w_in <- 188 / 25.4   
h_in <- 180 / 25.4   

quartz(
  file  = "FigS17.pdf", 
  type  = "pdf",
  width = w_in,
  height = h_in
)

print(pos_compound_with_legend)
dev.off()

