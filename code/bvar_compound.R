
################################################
# BVAR compound tropical cyclone
################################################

data_compound_tc<-subset(data, select = c("log_net_migration", 
                                          "fld_share_pop", "fld_share_pop_1","fld_share_pop_2", "fld_share_pop_3", "fld_share_pop_4", "fld_share_pop_5",
                                          "drt_share_pop", "drt_share_pop_1", "drt_share_pop_2", "drt_share_pop_3", "drt_share_pop_4", "drt_share_pop_5",
                                          "cyc_share_pop"  ,                    "cyc_share_pop_1"   ,                 "cyc_share_pop_2"  ,                  "cyc_share_pop_3" ,                  
                                          "cyc_share_pop_4"   ,                 "cyc_share_pop_5" ,
                                          "log_armed_conflict_brd_lag1",   
                                          "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                          "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                          "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                          "shdi_lag1"))

data_compound_tc<-na.omit(data_compound_tc)

data_compound_tc_pos<-subset(net_pos, select = c("log_net_migration", 
                                                 "fld_share_pop", "fld_share_pop_1","fld_share_pop_2", "fld_share_pop_3", "fld_share_pop_4", "fld_share_pop_5",
                                                 "drt_share_pop", "drt_share_pop_1", "drt_share_pop_2", "drt_share_pop_3", "drt_share_pop_4", "drt_share_pop_5",
                                                 "cyc_share_pop"  ,                    "cyc_share_pop_1"   ,                 "cyc_share_pop_2"  ,                  "cyc_share_pop_3" ,                  
                                                 "cyc_share_pop_4"   ,                 "cyc_share_pop_5" ,
                                                 "log_armed_conflict_brd_lag1",   
                                                 "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                                 "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                                 "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                                 "shdi_lag1"))

data_compound_tc_pos<-na.omit(data_compound_tc_pos)

data_compound_tc_neg<-subset(net_neg, select = c("log_net_migration", 
                                                 "fld_share_pop", "fld_share_pop_1","fld_share_pop_2", "fld_share_pop_3", "fld_share_pop_4", "fld_share_pop_5",
                                                 "drt_share_pop", "drt_share_pop_1", "drt_share_pop_2", "drt_share_pop_3", "drt_share_pop_4", "drt_share_pop_5",
                                                 "cyc_share_pop"  ,                    "cyc_share_pop_1"   ,                 "cyc_share_pop_2"  ,                  "cyc_share_pop_3" ,                  
                                                 "cyc_share_pop_4"   ,                 "cyc_share_pop_5" ,
                                                 "log_armed_conflict_brd_lag1",   
                                                 "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                                 "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                                 "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                                 "shdi_lag1"))
data_compound_tc_neg<-na.omit(data_compound_tc_neg)

# Create compound variables
data_compound_tc$tc_0_1 <- ifelse(data_compound_tc$cyc_share_pop > 0 & data_compound_tc$cyc_share_pop_1 > 0,1, 0) 

data_compound_tc$tc_0_1_2 <- ifelse(data_compound_tc$cyc_share_pop > 0 & data_compound_tc$cyc_share_pop_1 > 0 & data_compound_tc$cyc_share_pop_2 > 0,1, 0 )

data_compound_tc$tc_0_1_2_3 <- ifelse(data_compound_tc$cyc_share_pop > 0 & data_compound_tc$cyc_share_pop_1 > 0 & data_compound_tc$cyc_share_pop_2 > 0
                                      & data_compound_tc$cyc_share_pop_3 > 0,1, 0) 


data_compound_tc_neg$tc_0_1 <- ifelse(data_compound_tc_neg$cyc_share_pop > 0 & data_compound_tc_neg$cyc_share_pop_1 > 0,1, 0) 

data_compound_tc_neg$tc_0_1_2 <- ifelse(data_compound_tc_neg$cyc_share_pop > 0 & data_compound_tc_neg$cyc_share_pop_1 > 0 & data_compound_tc_neg$cyc_share_pop_2 > 0,1, 0 )

data_compound_tc_neg$tc_0_1_2_3 <- ifelse(data_compound_tc_neg$cyc_share_pop > 0 & data_compound_tc_neg$cyc_share_pop_1 > 0 & data_compound_tc_neg$cyc_share_pop_2 > 0
                                          & data_compound_tc_neg$cyc_share_pop_3 > 0,1, 0) 

data_compound_tc_pos$tc_0_1 <- ifelse(data_compound_tc_pos$cyc_share_pop > 0 & data_compound_tc_pos$cyc_share_pop_1 > 0,1, 0) 

data_compound_tc_pos$tc_0_1_2 <- ifelse(data_compound_tc_pos$cyc_share_pop > 0 & data_compound_tc_pos$cyc_share_pop_1 > 0 & data_compound_tc_pos$cyc_share_pop_2 > 0,1, 0 )

data_compound_tc_pos$tc_0_1_2_3 <- ifelse(data_compound_tc_pos$cyc_share_pop > 0 & data_compound_tc_pos$cyc_share_pop_1 > 0 & data_compound_tc_pos$cyc_share_pop_2 > 0
                                          & data_compound_tc_pos$cyc_share_pop_3 > 0,1, 0) 

summary(data_compound_tc_neg$tc_0_1_2_3)
# Percentage of compound events for each variable
percent_tc_0 <- mean(data_compound_tc$cyc_share_pop > 0) * 100
percent_tc_0_1 <- mean(data_compound_tc$tc_0_1 > 0) * 100
percent_tc_0_1_2 <- mean(data_compound_tc$tc_0_1_2 > 0) * 100
percent_tc_0_1_2_3 <- mean(data_compound_tc$tc_0_1_2_3 > 0) * 100

# Display the results
percent_tc_0_1
# 1.029597
percent_tc_0_1_2
# 0.3763774
percent_tc_0_1_2_3
# 0.1317037



# Install and load plotrix package for donut chart
#install.packages("plotrix")
library(plotrix)
#install.packages("RColorBrewer")
library(RColorBrewer)

categories <- c("1-year","2-year", "3-year", "4-year")
values <- c(3.63, 1, 0.37, 0.13)
colors <- brewer.pal(length(values), "Set2")  # Set2 is a nice pastel palette

pdf("des_compound_tropical_cyclone.pdf", width = 8, height = 6)
# Create a donut chart
pie(values, labels = paste(categories, round(values, 1), "%"), 
    col = colors, main = "Compound Tropical Cyclone", 
    cex.main = 2.7,  # Enlarge the title
    cex = 2)
library(plotrix)
draw.circle(0, 0, 0.5)  
dev.off()


# Load the BVAR package
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
run_bvar_with_auto_lags <- function(data_interpolated, n_draw = 10000, n_burn = 1000, n_thin = 1, priors, mh, verbose = TRUE) {
  # Infer the maximum number of lags from the data_interpolatedset
  # This assumes your data_interpolatedset has a specific naming convention for lagged variables, e.g., var1_lag1, var1_lag2, etc.
  lag_columns <- grep("_lag", names(data_interpolated), value = TRUE)
  max_lags <- max(as.integer(sub(".*_lag", "", lag_columns)))
  
  # If no lagged columns are found, set max_lags to a default value (e.g., 1)
  if (length(lag_columns) == 0) {
    max_lags <- 1
  }
  
  # Run the bvar function with the determined number of lags
  bvar_result <- bvar(data_interpolated, lags = max_lags, n_draw = n_draw, n_burn = n_burn, n_thin = n_thin, priors = priors, mh = mh, verbose = verbose)
  
  return(bvar_result)
}

names(data_compound_tc)
# Run the full and split sample models
compound_tc <- run_bvar_with_auto_lags(data_compound_tc, priors = priors, mh = mh)

compound_tc_pos <- run_bvar_with_auto_lags(data_compound_tc_pos, priors = priors, mh = mh)
compound_tc_neg <- run_bvar_with_auto_lags(data_compound_tc_neg, priors = priors, mh = mh)


summary(compound_tc_pos)

tc_index <- which(compound_tc_pos$variables == "cyc_share_pop")
tc_index_01 <- which(compound_tc_pos$variables == "tc_0_1")
tc_index_012 <- which(compound_tc_pos$variables == "tc_0_1_2")
tc_index_0123 <- which(compound_tc_pos$variables == "tc_0_1_2_3") 


log_net_migration_index<- which(compound_tc_pos$variables == "log_net_migration")

# Extracting the posterior samples for this coefficient
# Assuming it's the first response variable (index 1 in the third dimension if it's multidimensional)
posterior_samples_1 <- compound_tc_pos$beta[, tc_index, log_net_migration_index]
posterior_samples_2 <- compound_tc_pos$beta[, tc_index_01, log_net_migration_index]
posterior_samples_3 <- compound_tc_pos$beta[, tc_index_012, log_net_migration_index]
posterior_samples_4 <- compound_tc_pos$beta[, tc_index_0123, log_net_migration_index]



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

tc_full <- ggplot(df1, aes(x = Effect, y = PointEstimate, group = 1)) +
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
  scale_y_continuous(limits = c(-0.8, 0.8), breaks = seq(-0.8, 0.8, by = 0.2))

tc_full



################################################
# BVAR compound flood events
################################################

data_compound_flood<-subset(data, select = c("log_net_migration", 
                                             "fld_share_pop", "fld_share_pop_1","fld_share_pop_2", "fld_share_pop_3", "fld_share_pop_4", "fld_share_pop_5",
                                             "drt_share_pop", "drt_share_pop_1", "drt_share_pop_2", "drt_share_pop_3", "drt_share_pop_4", "drt_share_pop_5",
                                             "cyc_share_pop"  ,                    "cyc_share_pop_1"   ,                 "cyc_share_pop_2"  ,                  "cyc_share_pop_3" ,                  
                                             "cyc_share_pop_4"   ,                 "cyc_share_pop_5" ,
                                                          "log_armed_conflict_brd_lag1",   
                                                          "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                                          "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                                          "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                                          "shdi_lag1"))
data_compound_flood<-na.omit(data_compound_flood)

data_compound_flood_neg<-subset(net_neg, select = c("log_net_migration", 
                                                    "fld_share_pop", "fld_share_pop_1","fld_share_pop_2", "fld_share_pop_3", "fld_share_pop_4", "fld_share_pop_5",
                                                    "drt_share_pop", "drt_share_pop_1", "drt_share_pop_2", "drt_share_pop_3", "drt_share_pop_4", "drt_share_pop_5",
                                                    "cyc_share_pop"  ,                    "cyc_share_pop_1"   ,                 "cyc_share_pop_2"  ,                  "cyc_share_pop_3" ,                  
                                                    "cyc_share_pop_4"   ,                 "cyc_share_pop_5" ,
                                                    "log_armed_conflict_brd_lag1",   
                                                    "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                                    "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                                    "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                                    "shdi_lag1"))
data_compound_flood_neg<-na.omit(data_compound_flood_neg)


data_compound_flood_pos<-subset(net_pos, select = c("log_net_migration", 
                                                    "fld_share_pop", "fld_share_pop_1","fld_share_pop_2", "fld_share_pop_3", "fld_share_pop_4", "fld_share_pop_5",
                                                    "drt_share_pop", "drt_share_pop_1", "drt_share_pop_2", "drt_share_pop_3", "drt_share_pop_4", "drt_share_pop_5",
                                                    "cyc_share_pop"  ,                    "cyc_share_pop_1"   ,                 "cyc_share_pop_2"  ,                  "cyc_share_pop_3" ,                  
                                                    "cyc_share_pop_4"   ,                 "cyc_share_pop_5" ,
                                                    "log_armed_conflict_brd_lag1",   
                                                    "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                                    "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                                    "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                                    "shdi_lag1"))
data_compound_flood_pos<-na.omit(data_compound_flood_pos)

data_compound_flood<-na.omit(data_compound_flood)
names(data_compound_flood)

data_compound_flood$flood_0_1 <- ifelse(data_compound_flood$fld_share_pop > 0 & data_compound_flood$fld_share_pop_1 > 0,1, 0) 

data_compound_flood$flood_0_1_2 <- ifelse(data_compound_flood$fld_share_pop > 0 & data_compound_flood$fld_share_pop_1 > 0 & data_compound_flood$fld_share_pop_2 > 0,1, 0 )

data_compound_flood$flood_0_1_2_3 <- ifelse(data_compound_flood$fld_share_pop > 0 & data_compound_flood$fld_share_pop_1 > 0 & data_compound_flood$fld_share_pop_2 > 0
                                      & data_compound_flood$fld_share_pop_3 > 0,1, 0) 



data_compound_flood_pos$flood_0_1 <- ifelse(data_compound_flood_pos$fld_share_pop > 0 & data_compound_flood_pos$fld_share_pop_1 > 0,1, 0) 

data_compound_flood_pos$flood_0_1_2 <- ifelse(data_compound_flood_pos$fld_share_pop > 0 & data_compound_flood_pos$fld_share_pop_1 > 0 & data_compound_flood_pos$fld_share_pop_2 > 0,1, 0 )

data_compound_flood_pos$flood_0_1_2_3 <- ifelse(data_compound_flood_pos$fld_share_pop > 0 & data_compound_flood_pos$fld_share_pop_1 > 0 & data_compound_flood_pos$fld_share_pop_2 > 0
                                            & data_compound_flood_pos$fld_share_pop_3 > 0,1, 0) 

data_compound_flood_neg$flood_0_1 <- ifelse(data_compound_flood_neg$fld_share_pop > 0 & data_compound_flood_neg$fld_share_pop_1 > 0,1, 0) 

data_compound_flood_neg$flood_0_1_2 <- ifelse(data_compound_flood_neg$fld_share_pop > 0 & data_compound_flood_neg$fld_share_pop_1 > 0 & data_compound_flood_neg$fld_share_pop_2 > 0,1, 0 )

data_compound_flood_neg$flood_0_1_2_3 <- ifelse(data_compound_flood_neg$fld_share_pop > 0 & data_compound_flood_neg$fld_share_pop_1 > 0 & data_compound_flood_neg$fld_share_pop_2 > 0
                                                & data_compound_flood_neg$fld_share_pop_3 > 0,1, 0) 

summary(data_compound_flood_neg$flood_0_1_2_3)
# Percentage of compound events for each variable
percent_flood_0 <- mean(data_compound_flood$fld_share_pop > 0) * 100
percent_flood_0_1 <- mean(data_compound_flood$flood_0_1 > 0) * 100
percent_flood_0_1_2 <- mean(data_compound_flood$flood_0_1_2 > 0) * 100
percent_flood_0_1_2_3 <- mean(data_compound_flood$flood_0_1_2_3 > 0) * 100
percent_flood_0_1_2_3_4 <- mean(data_compound_flood$flood_0_1_2_3_4 > 0) * 100
percent_flood_0_1_2_3_4_5 <- mean(data_compound_flood$flood_0_1_2_3_4_5 > 0) * 100


# Display the results
percent_flood_0
# 24
percent_flood_0_1
# 17
percent_flood_0_1_2
# 15
percent_flood_0_1_2_3
# 14

percent_flood_0_1_2_3_4
# 4.5

percent_flood_0_1_2_3_4_5
# 3.6

library(plotrix)
#install.packages("RColorBrewer")
library(RColorBrewer)

categories <- c("1-year","2-year", "3-year", "4-year")
values <- c(20, 11, 8, 6)
colors <- brewer.pal(length(values), "Set2")  # Set2 is a nice pastel palette

pdf("des_compound_flood.pdf", width = 8, height = 6)
# Create a donut chart
pie(values, labels = paste(categories, round(values, 1), "%"), 
    col = colors, main = "Compound Flood Events", 
    cex.main = 2.7,  # Enlarge the title
    cex = 2)
library(plotrix)
draw.circle(0, 0, 0.5)  
dev.off()

names(data_compound_flood)

# Load the BVAR package
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
run_bvar_with_auto_lags <- function(data_interpolated, n_draw = 10000, n_burn = 1000, n_thin = 1, priors, mh, verbose = TRUE) {
  # Infer the maximum number of lags from the data_interpolatedset
  # This assumes your data_interpolatedset has a specific naming convention for lagged variables, e.g., var1_lag1, var1_lag2, etc.
  lag_columns <- grep("_lag", names(data_interpolated), value = TRUE)
  max_lags <- max(as.integer(sub(".*_lag", "", lag_columns)))
  
  # If no lagged columns are found, set max_lags to a default value (e.g., 1)
  if (length(lag_columns) == 0) {
    max_lags <- 1
  }
  
  # Run the bvar function with the determined number of lags
  bvar_result <- bvar(data_interpolated, lags = max_lags, n_draw = n_draw, n_burn = n_burn, n_thin = n_thin, priors = priors, mh = mh, verbose = verbose)
  
  return(bvar_result)
}
# Run bvar models
compound_flood<- run_bvar_with_auto_lags(data_compound_flood, priors = priors, mh = mh)

compound_flood_neg <- run_bvar_with_auto_lags(data_compound_flood_neg, priors = priors, mh = mh)
compound_flood_pos <- run_bvar_with_auto_lags(data_compound_flood_pos, priors = priors, mh = mh)

summary(compound_flood)

flood_index <- which(compound_flood_pos$variables == "fld_share_pop")
flood_index_01 <- which(compound_flood_pos$variables == "flood_0_1")
flood_index_012 <- which(compound_flood_pos$variables == "flood_0_1_2")
flood_index_0123 <- which(compound_flood_pos$variables == "flood_0_1_2_3") 

log_net_migration_index<- which(compound_flood_pos$variables == "log_net_migration")

# Extracting the posterior samples for this coefficient
# Assuming it's the first response variable (index 1 in the third dimension if it's multidimensional)
posterior_samples_1 <- compound_flood_pos$beta[, flood_index, log_net_migration_index]
posterior_samples_2 <- compound_flood_pos$beta[, flood_index_01, log_net_migration_index]
posterior_samples_3 <- compound_flood_pos$beta[, flood_index_012, log_net_migration_index]
posterior_samples_4 <- compound_flood_pos$beta[, flood_index_0123, log_net_migration_index]

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

# Create the plot
flood_full <- ggplot(df2, aes(x = Effect, y = PointEstimate, group = 1)) +
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
  scale_y_continuous(limits = c(-0.8, 0.8), breaks = seq(-0.8, 0.8, by = 0.2))

flood_full
################################################
# BVAR compound drought events
################################################

data_compound_drought<-subset(data, select = c("log_net_migration", 
                                                            "fld_share_pop", "fld_share_pop_1","fld_share_pop_2", "fld_share_pop_3", "fld_share_pop_4", "fld_share_pop_5",
                                                            "drt_share_pop", "drt_share_pop_1", "drt_share_pop_2", "drt_share_pop_3", "drt_share_pop_4", "drt_share_pop_5",
                                                            "cyc_share_pop"  ,                    "cyc_share_pop_1"   ,                 "cyc_share_pop_2"  ,                  "cyc_share_pop_3" ,                  
                                                            "cyc_share_pop_4"   ,                 "cyc_share_pop_5" ,
                                                            "log_armed_conflict_brd_lag1",   
                                                            "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                                            "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                                            "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                                            "shdi_lag1"))
data_compound_drought<-na.omit(data_compound_drought)


data_compound_drought_neg<-subset(net_neg, select = c("log_net_migration", 
                                                      "fld_share_pop", "fld_share_pop_1","fld_share_pop_2", "fld_share_pop_3", "fld_share_pop_4", "fld_share_pop_5",
                                                      "drt_share_pop", "drt_share_pop_1", "drt_share_pop_2", "drt_share_pop_3", "drt_share_pop_4", "drt_share_pop_5",
                                                      "cyc_share_pop"  ,                    "cyc_share_pop_1"   ,                 "cyc_share_pop_2"  ,                  "cyc_share_pop_3" ,                  
                                                      "cyc_share_pop_4"   ,                 "cyc_share_pop_5" ,
                                                      "log_armed_conflict_brd_lag1",   
                                                      "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                                      "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                                      "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                                      "shdi_lag1"))

data_compound_drought_neg<-na.omit(data_compound_drought_neg)

data_compound_drought_pos<-subset(net_pos, select = c("log_net_migration", 
                                                      "fld_share_pop", "fld_share_pop_1","fld_share_pop_2", "fld_share_pop_3", "fld_share_pop_4", "fld_share_pop_5",
                                                      "drt_share_pop", "drt_share_pop_1", "drt_share_pop_2", "drt_share_pop_3", "drt_share_pop_4", "drt_share_pop_5",
                                                      "cyc_share_pop"  ,                    "cyc_share_pop_1"   ,                 "cyc_share_pop_2"  ,                  "cyc_share_pop_3" ,                  
                                                      "cyc_share_pop_4"   ,                 "cyc_share_pop_5" ,
                                                      "log_armed_conflict_brd_lag1",   
                                                      "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                                      "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                                      "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                                      "shdi_lag1"))

data_compound_drought_pos<-na.omit(data_compound_drought_pos)

data_compound_drought$drought_0_1 <- ifelse(data_compound_drought$drt_share_pop > 0 & data_compound_drought$drt_share_pop_1 > 0,1, 0) 

data_compound_drought$drought_0_1_2 <- ifelse(data_compound_drought$drt_share_pop > 0 & data_compound_drought$drt_share_pop_1 > 0 & data_compound_drought$drt_share_pop_2 > 0,1, 0 )

data_compound_drought$drought_0_1_2_3 <- ifelse(data_compound_drought$drt_share_pop > 0 & data_compound_drought$drt_share_pop_1 > 0 & data_compound_drought$drt_share_pop_2 > 0
                                            & data_compound_drought$drt_share_pop_3 > 0,1, 0) 



data_compound_drought_pos$drought_0_1 <- ifelse(data_compound_drought_pos$drt_share_pop > 0 & data_compound_drought_pos$drt_share_pop_1 > 0,1, 0) 

data_compound_drought_pos$drought_0_1_2 <- ifelse(data_compound_drought_pos$drt_share_pop > 0 & data_compound_drought_pos$drt_share_pop_1 > 0 & data_compound_drought_pos$drt_share_pop_2 > 0,1, 0 )

data_compound_drought_pos$drought_0_1_2_3 <- ifelse(data_compound_drought_pos$drt_share_pop > 0 & data_compound_drought_pos$drt_share_pop_1 > 0 & data_compound_drought_pos$drt_share_pop_2 > 0
                                                    & data_compound_drought_pos$drt_share_pop_3 > 0,1, 0) 


data_compound_drought_neg$drought_0_1 <- ifelse(data_compound_drought_neg$drt_share_pop > 0 & data_compound_drought_neg$drt_share_pop_1 > 0,1, 0) 

data_compound_drought_neg$drought_0_1_2 <- ifelse(data_compound_drought_neg$drt_share_pop > 0 & data_compound_drought_neg$drt_share_pop_1 > 0 & data_compound_drought_neg$drt_share_pop_2 > 0,1, 0 )

data_compound_drought_neg$drought_0_1_2_3 <- ifelse(data_compound_drought_neg$drt_share_pop > 0 & data_compound_drought_neg$drt_share_pop_1 > 0 & data_compound_drought_neg$drt_share_pop_2 > 0
                                                    & data_compound_drought_neg$drt_share_pop_3 > 0,1, 0) 

summary(data_compound_drought_pos$drought_0_1_2_3)
# Percentage of compound events for each variable
percent_drought_0 <- mean(data_compound_drought$drt_share_pop > 0) * 100
percent_drought_0_1 <- mean(data_compound_drought$drought_0_1 > 0) * 100
percent_drought_0_1_2 <- mean(data_compound_drought$drought_0_1_2 > 0) * 100
percent_drought_0_1_2_3 <- mean(data_compound_drought$drought_0_1_2_3 > 0) * 100
percent_drought_0_1_2_3_4 <- mean(data_compound_drought$drought_0_1_2_3_4 > 0) * 100
percent_drought_0_1_2_3_4_5 <- mean(data_compound_drought$drought_0_1_2_3_4_5 > 0) * 100


# Display the results
percent_drought_0
# 55
# 40
percent_drought_0_1
# 30
percent_drought_0_1_2
# 24
percent_drought_0_1_2_3
# 20


library(plotrix)
#install.packages("RColorBrewer")
library(RColorBrewer)

categories <- c("1-year","2-year", "3-year", "4-year")
values <- c(55, 40, 30, 24)
colors <- brewer.pal(length(values), "Set2")  # Set2 is a nice pastel palette

pdf("des_compound_drought.pdf", width = 8, height = 6)
# Create a donut chart
pie(values, labels = paste(categories, round(values, 1), "%"), 
    col = colors, main = "Compound Drought", 
    cex.main = 2.7,  # Enlarge the title
    cex = 2)
library(plotrix)
draw.circle(0, 0, 0.5)  
dev.off()


# Load the BVAR package
library(BVAR)
# Setting up priors

mn <- bv_minnesota(
  lambda = bv_lambda(mode = 0.2, sd = 0.4, min = 0.0001, max = 5),
  alpha = bv_alpha(mode = 2), var = 1e07)

# the sum-of-coefficient
# saying that  the combined effect of all lagged values of a variable on itself (or on another variable) should sum up to a certain value, reflecting long-term stability or a specific long-term impact.
#  Setting mode = 1 might reflect a belief that, over the long term, the total impact of all lagged values of a variable on itself (or another variable) should be neutral or balanced
soc <- bv_soc(mode = 1, sd = 1, min = 1e-04, max = 50)

# single-unit-root priors - stablity of the VAR model
# mode 1 where the system is neither too sluggish nor too prone to overshooting in its return to equilibrium
sur <- bv_sur(mode = 1, sd = 1, min = 1e-04, max = 50)


# Once the priors are defined, we provide them to bv_priors() Via hyper we choose which hyperparameters should be treated hierarchically. Its default setting ("auto") includes
priors <- bv_priors(hyper = "auto", mn = mn, soc = soc, sur = sur)



mh <- bv_metropolis(scale_hess = c(0.05, 0.0001, 0.0001),
                    adjust_acc = TRUE, acc_lower = 0.25, acc_upper = 0.45)


# Wrapper function to automatically determine lags and run BVAR
run_bvar_with_auto_lags <- function(data_interpolated, n_draw = 10000, n_burn = 1000, n_thin = 1, priors, mh, verbose = TRUE) {
  # Infer the maximum number of lags from the data_interpolatedset
  # This assumes your data_interpolatedset has a specific naming convention for lagged variables, e.g., var1_lag1, var1_lag2, etc.
  lag_columns <- grep("_lag", names(data_interpolated), value = TRUE)
  max_lags <- max(as.integer(sub(".*_lag", "", lag_columns)))
  
  # If no lagged columns are found, set max_lags to a default value (e.g., 1)
  if (length(lag_columns) == 0) {
    max_lags <- 1
  }
  
  # Run the bvar function with the determined number of lags
  bvar_result <- bvar(data_interpolated, lags = max_lags, n_draw = n_draw, n_burn = n_burn, n_thin = n_thin, priors = priors, mh = mh, verbose = verbose)
  
  return(bvar_result)
}

# Run bvar models
compound_drought<- run_bvar_with_auto_lags(data_compound_drought, priors = priors, mh = mh)

compound_drought_neg <- run_bvar_with_auto_lags(data_compound_drought_neg, priors = priors, mh = mh)
compound_drought_pos <- run_bvar_with_auto_lags(data_compound_drought_pos, priors = priors, mh = mh)


drought_index_1 <- which(compound_drought_neg$variables == "drt_share_pop")
drought_index_12 <- which(compound_drought_neg$variables == "drought_0_1")
drought_index_123 <- which(compound_drought_neg$variables == "drought_0_1_2")
drought_index_1234 <- which(compound_drought_neg$variables == "drought_0_1_2_3") 

log_net_migration_index<- which(compound_drought_neg$variables == "log_net_migration")






# Extracting the posterior samples for this coefficient
# Assuming it's the first response variable (index 1 in the third dimension if it's multidimensional)
posterior_samples_1 <- compound_drought_neg$beta[, drought_index_1, log_net_migration_index]
posterior_samples_2 <- compound_drought_neg$beta[, drought_index_12, log_net_migration_index]
posterior_samples_3 <- compound_drought_neg$beta[, drought_index_123, log_net_migration_index]
posterior_samples_4 <- compound_drought_neg$beta[, drought_index_1234, log_net_migration_index]



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

drought_full <- ggplot(df3, aes(x = Effect, y = PointEstimate, group = 1)) +
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
  scale_y_continuous(limits = c(-0.8, 0.8), breaks = seq(-0.8, 0.8, by = 0.2))

drought_full
# Create the full compound model
# Figure S.11

# Libraries
library(ggplot2)
library(cowplot)

#  white background 
theme_set(
  theme_minimal(base_size = 12) +
    theme(plot.background = element_rect(fill = "white", color = NA))
)

# consistent formatting 
adjust_full_plot <- function(p, show_x = FALSE) {
  p +
    labs(
      y = "Point Estimate of Net Migration",
      x = if (show_x) "Time lag" else NULL
    ) +
    theme(
      plot.title = element_blank(),
      axis.title.y = element_text(size = 15, margin = margin(r = 6)),
      axis.title.x = element_text(size = 15)
    )
}


flood_full_adj   <- adjust_full_plot(flood_full, show_x = FALSE)
drought_full_adj <- adjust_full_plot(drought_full, show_x = FALSE)
tc_full_adj      <- adjust_full_plot(tc_full, show_x = TRUE)

#  Add row labels 
row_flood <- ggdraw() +
  draw_plot(flood_full_adj, x = 0.08, y = 0, width = 0.92, height = 1) +
  draw_label("Flood", x = 0.01, y = 0.5, angle = 90, size = 20, hjust = 0.5)

row_drought <- ggdraw() +
  draw_plot(drought_full_adj, x = 0.08, y = 0, width = 0.92, height = 1) +
  draw_label("Drought", x = 0.01, y = 0.5, angle = 90, size = 20, hjust = 0.5)

row_tc <- ggdraw() +
  draw_plot(tc_full_adj, x = 0.08, y = 0, width = 0.92, height = 1) +
  draw_label("Trop cyclone", x = 0.01, y = 0.5, angle = 90, size = 20, hjust = 0.5)

# Stack the three rows 
rows_single <- plot_grid(
  row_flood,
  row_drought,
  row_tc,
  ncol = 1,
  labels = c("a)", "b)", "c)"),
  label_size = 18,
  label_fontface = "bold",
  label_x = 0.02,
  label_y = 1.03,
  rel_heights = c(1, 1, 1)
)

#  Add single top title 
title_top <- ggdraw() +
  draw_label("Full Compound Model", fontface = "bold", size = 24, hjust = 0.5, vjust = 0.5)

# Combine title and body 
final_plot <- plot_grid(
  title_top,
  rows_single,
  ncol = 1,
  rel_heights = c(0.12, 1)
)

# Preview
final_plot

# Save 
ggsave(
  filename = "compound_full.png",
  plot = final_plot,
  width = 9,
  height = 12,
  dpi = 300,
  bg = "white"
)




# Create Fig. 5 
flood_neg_compound <- ggplot(df2, aes(x = Effect, y = PointEstimate, group = 1)) +
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
    limits = c(-2, 2),
    breaks = seq(-2, 2, by = 0.5)
  )

flood_neg_compound

drought_neg_compound <- ggplot(df3, aes(x = Effect, y = PointEstimate, group = 1)) +
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
    limits = c(-2, 2),
    breaks = seq(-2, 2, by = 0.5)
  )

drought_neg_compound

tc_neg_compound <- ggplot(df1, aes(x = Effect, y = PointEstimate, group = 1)) +
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

tc_neg_compound

# positive net 
flood_pos_compound <- ggplot(df2, aes(x = Effect, y = PointEstimate, group = 1)) +
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
    limits = c(-2, 2),
    breaks = seq(-2, 2, by = 0.5)
  )

flood_pos_compound

drought_pos_compound <- ggplot(df3, aes(x = Effect, y = PointEstimate, group = 1)) +
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
    limits = c(-2, 2),
    breaks = seq(-2, 2, by = 0.5)
  )

drought_pos_compound

tc_pos_compound <- ggplot(df1, aes(x = Effect, y = PointEstimate, group = 1)) +
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

tc_pos_compound

# Combine plots 
library(ggplot2)
library(cowplot)

# Set global white background 
theme_set(theme_minimal(base_size = 12) +
            theme(plot.background = element_rect(fill = "white", color = NA)))


adjust_neg_plot <- function(p) {
  p + labs(y = "Point Estimate of Net Migration") +
    theme(
      plot.title = element_blank(),
      axis.title.y = element_text(size = 15, margin = margin(r = 1))
    )
}

# Adjust individual plots 

# Negative plots 
flood_neg_compound <- adjust_neg_plot(flood_neg_compound)
drought_neg_compound <- adjust_neg_plot(drought_neg_compound + labs(x = NULL))
tc_neg_compound <- adjust_neg_plot(tc_neg_compound + labs(x = "Time lag") +
                                     theme(axis.title.x = element_text(size = 24)))

# Positive plots 
flood_pos_compound <- flood_pos_compound + labs(y = NULL, x = NULL) + theme(plot.title = element_blank())
drought_pos_compound <- drought_pos_compound + labs(y = NULL, x = NULL) + theme(plot.title = element_blank())
tc_pos_compound <- tc_pos_compound + labs(y = NULL, x = "Time lag") +
  theme(plot.title = element_blank(),
        axis.title.x = element_text(size = 24))

# Combine and label each row of plots 

row_flood <- plot_grid(
  flood_neg_compound, flood_pos_compound,
  nrow = 1, rel_widths = c(1, 1),
  align = "h",
  labels = c("a", "b"), label_size = 16, label_fontface = "bold"
)

row_drought <- plot_grid(
  drought_neg_compound, drought_pos_compound,
  nrow = 1, rel_widths = c(1, 1),
  align = "h",
  labels = c("c", "d"), label_size = 16, label_fontface = "bold"
)

row_tc <- plot_grid(
  tc_neg_compound, tc_pos_compound,
  nrow = 1, rel_widths = c(1, 1),
  align = "h",
  labels = c("e", "f"), label_size = 16, label_fontface = "bold"
)

# Add row labels

row_flood <- plot_grid(
  flood_neg_compound, flood_pos_compound,
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
  drought_neg_compound, drought_pos_compound,
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
  tc_neg_compound, tc_pos_compound,
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


#  Add column titles 

titles <- plot_grid(
  ggdraw() + draw_label("Negative net-migration", fontface = "bold", size = 24, hjust = 0.5),
  ggdraw() + draw_label("Positive net-migration", fontface = "bold", size = 24, hjust = 0.5),
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

# Save 

ggsave(
  filename = "fig5.png",
  plot = final_plot,
  width = 13,
  height = 12,
  dpi = 300,
  bg = "white"
)