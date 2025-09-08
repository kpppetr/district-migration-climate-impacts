
################################################
# BVAR compound tropical cyclone
################################################

data_compound_tc<-subset(data, select = c("log_net_migration", 
                                          "flood_threshold", "flood_threshold_1","flood_threshold_2", "flood_threshold_3", "flood_threshold_4", "flood_threshold_5",
                                          "soilmoist_contin", "soilmoist_contin_1", "soilmoist_contin_2", "soilmoist_contin_3", "soilmoist_contin_4", "soilmoist_contin_5",
                                          "top_cyc"  ,                    "top_cyc_1"   ,                 "top_cyc_2"  ,                  "top_cyc_3" ,                  
                                          "top_cyc_4"   ,                 "top_cyc_5" ,
                                          "log_armed_conflict_brd_lag1",   
                                          "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                          "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                          "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                          "shdi_lag1"))

data_compound_tc<-na.omit(data_compound_tc)

data_compound_tc_pos<-subset(net_pos, select = c("log_net_migration", 
                                                 "flood_threshold", "flood_threshold_1","flood_threshold_2", "flood_threshold_3", "flood_threshold_4", "flood_threshold_5",
                                                 "soilmoist_contin", "soilmoist_contin_1", "soilmoist_contin_2", "soilmoist_contin_3", "soilmoist_contin_4", "soilmoist_contin_5",
                                                 "top_cyc"  ,                    "top_cyc_1"   ,                 "top_cyc_2"  ,                  "top_cyc_3" ,                  
                                                 "top_cyc_4"   ,                 "top_cyc_5" ,
                                                 "log_armed_conflict_brd_lag1",   
                                                 "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                                 "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                                 "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                                 "shdi_lag1"))

data_compound_tc_pos<-na.omit(data_compound_tc_pos)

data_compound_tc_neg<-subset(net_neg, select = c("log_net_migration", 
                                                 "flood_threshold", "flood_threshold_1","flood_threshold_2", "flood_threshold_3", "flood_threshold_4", "flood_threshold_5",
                                                 "soilmoist_contin", "soilmoist_contin_1", "soilmoist_contin_2", "soilmoist_contin_3", "soilmoist_contin_4", "soilmoist_contin_5",
                                                 "top_cyc"  ,                    "top_cyc_1"   ,                 "top_cyc_2"  ,                  "top_cyc_3" ,                  
                                                 "top_cyc_4"   ,                 "top_cyc_5" ,
                                                 "log_armed_conflict_brd_lag1",   
                                                 "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                                 "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                                 "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                                 "shdi_lag1"))
data_compound_tc_neg<-na.omit(data_compound_tc_neg)

# Create compound variables
data_compound_tc$tc_0_1 <- ifelse(data_compound_tc$top_cyc > 0 & data_compound_tc$top_cyc_1 > 0,1, 0) 

data_compound_tc$tc_0_1_2 <- ifelse(data_compound_tc$top_cyc > 0 & data_compound_tc$top_cyc_1 > 0 & data_compound_tc$top_cyc_2 > 0,1, 0 )

data_compound_tc$tc_0_1_2_3 <- ifelse(data_compound_tc$top_cyc > 0 & data_compound_tc$top_cyc_1 > 0 & data_compound_tc$top_cyc_2 > 0
                                      & data_compound_tc$top_cyc_3 > 0,1, 0) 


data_compound_tc_neg$tc_0_1 <- ifelse(data_compound_tc_neg$top_cyc > 0 & data_compound_tc_neg$top_cyc_1 > 0,1, 0) 

data_compound_tc_neg$tc_0_1_2 <- ifelse(data_compound_tc_neg$top_cyc > 0 & data_compound_tc_neg$top_cyc_1 > 0 & data_compound_tc_neg$top_cyc_2 > 0,1, 0 )

data_compound_tc_neg$tc_0_1_2_3 <- ifelse(data_compound_tc_neg$top_cyc > 0 & data_compound_tc_neg$top_cyc_1 > 0 & data_compound_tc_neg$top_cyc_2 > 0
                                          & data_compound_tc_neg$top_cyc_3 > 0,1, 0) 

data_compound_tc_pos$tc_0_1 <- ifelse(data_compound_tc_pos$top_cyc > 0 & data_compound_tc_pos$top_cyc_1 > 0,1, 0) 

data_compound_tc_pos$tc_0_1_2 <- ifelse(data_compound_tc_pos$top_cyc > 0 & data_compound_tc_pos$top_cyc_1 > 0 & data_compound_tc_pos$top_cyc_2 > 0,1, 0 )

data_compound_tc_pos$tc_0_1_2_3 <- ifelse(data_compound_tc_pos$top_cyc > 0 & data_compound_tc_pos$top_cyc_1 > 0 & data_compound_tc_pos$top_cyc_2 > 0
                                          & data_compound_tc_pos$top_cyc_3 > 0,1, 0) 

summary(data_compound_tc_neg$tc_0_1_2_3)
# Percentage of compound events for each variable
percent_tc_0 <- mean(data_compound_tc$top_cyc > 0) * 100
percent_tc_0_1 <- mean(data_compound_tc$tc_0_1 > 0) * 100
percent_tc_0_1_2 <- mean(data_compound_tc$tc_0_1_2 > 0) * 100
percent_tc_0_1_2_3 <- mean(data_compound_tc$tc_0_1_2_3 > 0) * 100
percent_tc_0_1_2_3_4 <- mean(data_compound_tc$tc_0_1_2_3_4 > 0) * 100

# Display the results
percent_tc_0_1
# 0.604843
percent_tc_0_1_2
# 0.1740081
percent_tc_0_1_2_3
# 0.05396568

percent_tc_0_1_2_3_4
# only 0.01%


# Install and load plotrix package for donut chart
#install.packages("plotrix")
library(plotrix)
#install.packages("RColorBrewer")
library(RColorBrewer)

categories <- c("1-year","2-year", "3-year", "4-year")
values <- c(3, 0.604843, 0.1740081, 0.05396568)
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

tc_index <- which(compound_tc$variables == "top_cyc_1")
tc_index_01 <- which(compound_tc$variables == "tc_0_1")
tc_index_012 <- which(compound_tc$variables == "tc_0_1_2")
tc_index_0123 <- which(compound_tc$variables == "tc_0_1_2_3") 


log_net_migration_index<- which(compound_tc$variables == "log_net_migration")

# Extracting the posterior samples for this coefficient
# Assuming it's the first response variable (index 1 in the third dimension if it's multidimensional)
posterior_samples_1 <- compound_tc$beta[, tc_index, log_net_migration_index]
posterior_samples_2 <- compound_tc$beta[, tc_index_01, log_net_migration_index]
posterior_samples_3 <- compound_tc$beta[, tc_index_012, log_net_migration_index]
posterior_samples_4 <- compound_tc$beta[, tc_index_0123, log_net_migration_index]



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

library(ggplot2)
#png("compound_tc.png", width = 1200, height = 800) # Width and height in pixels
pdf("compound_tc.pdf", width = 16, height = 8) # Width and height in inches

# Create the ggplot
ggplot(df1, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(aes(color = "Trend"), size = 2) + # Make the trend line thicker
  geom_point(aes(color = "Point Estimates"), size = 4) + # Make the points larger
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "lightblue", size = 1) + # Add error bars for confidence intervals, with specified line thickness
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) + # Add a black dashed line at y = 0
  scale_color_manual(values = c("Trend" = "orange", "Point Estimates" = "lightblue")) + # Customize colors for the trend and point estimates/error bars
  labs(
    title = "",
    x = "",
    y = "Point Estimate of Net Migration"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 40), # Increase X axis label size
    axis.title.y = element_text(size = 34), # Increase Y axis label size
    axis.text.x = element_text(size = 30), # Increase X axis tick label size
    axis.text.y = element_text(size = 30) # Increase Y axis tick label size
  ) +
  theme(legend.position = "none") + # Remove the legend
  scale_y_continuous(limits = c(-0.8, 0.8), breaks = seq(-0.8, 0.8, by = 0.2)) # Set y-axis limits and breaks

# Close the PNG device to save the file
dev.off()

################################################
# BVAR compound flood events
################################################

data_compound_flood<-subset(data, select = c("log_net_migration", 
                                                          "flood_threshold", "flood_threshold_1","flood_threshold_2", "flood_threshold_3", "flood_threshold_4", "flood_threshold_5",
                                                          "soilmoist_contin", "soilmoist_contin_1", "soilmoist_contin_2", "soilmoist_contin_3", "soilmoist_contin_4", "soilmoist_contin_5",
                                                          "top_cyc"  ,                    "top_cyc_1"   ,                 "top_cyc_2"  ,                  "top_cyc_3" ,                  
                                                          "top_cyc_4"   ,                 "top_cyc_5" ,
                                                          "log_armed_conflict_brd_lag1",   
                                                          "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                                          "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                                          "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                                          "shdi_lag1"))
data_compound_flood<-na.omit(data_compound_flood)

data_compound_flood_neg<-subset(net_neg, select = c("log_net_migration", 
                                                    "flood_threshold", "flood_threshold_1","flood_threshold_2", "flood_threshold_3", "flood_threshold_4", "flood_threshold_5",
                                                    "soilmoist_contin", "soilmoist_contin_1", "soilmoist_contin_2", "soilmoist_contin_3", "soilmoist_contin_4", "soilmoist_contin_5",
                                                    "top_cyc"  ,                    "top_cyc_1"   ,                 "top_cyc_2"  ,                  "top_cyc_3" ,                  
                                                    "top_cyc_4"   ,                 "top_cyc_5" ,
                                                    "log_armed_conflict_brd_lag1",   
                                                    "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                                    "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                                    "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                                    "shdi_lag1"))
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
                                                    "shdi_lag1"))
data_compound_flood_pos<-na.omit(data_compound_flood_pos)

data_compound_flood<-na.omit(data_compound_flood)
names(data_compound_flood)

data_compound_flood$flood_0_1 <- ifelse(data_compound_flood$flood_threshold > 0 & data_compound_flood$flood_threshold_1 > 0,1, 0) 

data_compound_flood$flood_0_1_2 <- ifelse(data_compound_flood$flood_threshold > 0 & data_compound_flood$flood_threshold_1 > 0 & data_compound_flood$flood_threshold_2 > 0,1, 0 )

data_compound_flood$flood_0_1_2_3 <- ifelse(data_compound_flood$flood_threshold > 0 & data_compound_flood$flood_threshold_1 > 0 & data_compound_flood$flood_threshold_2 > 0
                                      & data_compound_flood$flood_threshold_3 > 0,1, 0) 



data_compound_flood_pos$flood_0_1 <- ifelse(data_compound_flood_pos$flood_threshold > 0 & data_compound_flood_pos$flood_threshold_1 > 0,1, 0) 

data_compound_flood_pos$flood_0_1_2 <- ifelse(data_compound_flood_pos$flood_threshold > 0 & data_compound_flood_pos$flood_threshold_1 > 0 & data_compound_flood_pos$flood_threshold_2 > 0,1, 0 )

data_compound_flood_pos$flood_0_1_2_3 <- ifelse(data_compound_flood_pos$flood_threshold > 0 & data_compound_flood_pos$flood_threshold_1 > 0 & data_compound_flood_pos$flood_threshold_2 > 0
                                            & data_compound_flood_pos$flood_threshold_3 > 0,1, 0) 

data_compound_flood_neg$flood_0_1 <- ifelse(data_compound_flood_neg$flood_threshold > 0 & data_compound_flood_neg$flood_threshold_1 > 0,1, 0) 

data_compound_flood_neg$flood_0_1_2 <- ifelse(data_compound_flood_neg$flood_threshold > 0 & data_compound_flood_neg$flood_threshold_1 > 0 & data_compound_flood_neg$flood_threshold_2 > 0,1, 0 )

data_compound_flood_neg$flood_0_1_2_3 <- ifelse(data_compound_flood_neg$flood_threshold > 0 & data_compound_flood_neg$flood_threshold_1 > 0 & data_compound_flood_neg$flood_threshold_2 > 0
                                                & data_compound_flood_neg$flood_threshold_3 > 0,1, 0) 

summary(data_compound_flood_neg$flood_0_1_2_3)
# Percentage of compound events for each variable
percent_flood_0 <- mean(data_compound_flood$flood_threshold > 0) * 100
percent_flood_0_1 <- mean(data_compound_flood$flood_0_1 > 0) * 100
percent_flood_0_1_2 <- mean(data_compound_flood$flood_0_1_2 > 0) * 100
percent_flood_0_1_2_3 <- mean(data_compound_flood$flood_0_1_2_3 > 0) * 100
percent_flood_0_1_2_3_4 <- mean(data_compound_flood$flood_0_1_2_3_4 > 0) * 100
percent_flood_0_1_2_3_4_5 <- mean(data_compound_flood$flood_0_1_2_3_4_5 > 0) * 100


# Display the results
percent_flood_0
# 20
percent_flood_0_1
# 11
percent_flood_0_1_2
# 8
percent_flood_0_1_2_3
# 6

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

flood_index <- which(compound_flood$variables == "flood_threshold_1")
flood_index_01 <- which(compound_flood$variables == "flood_0_1")
flood_index_012 <- which(compound_flood$variables == "flood_0_1_2")
flood_index_0123 <- which(compound_flood$variables == "flood_0_1_2_3") 

log_net_migration_index<- which(compound_flood$variables == "log_net_migration")

# Extracting the posterior samples for this coefficient
# Assuming it's the first response variable (index 1 in the third dimension if it's multidimensional)
posterior_samples_1 <- compound_flood$beta[, flood_index, log_net_migration_index]
posterior_samples_2 <- compound_flood$beta[, flood_index_01, log_net_migration_index]
posterior_samples_3 <- compound_flood$beta[, flood_index_012, log_net_migration_index]
posterior_samples_4 <- compound_flood$beta[, flood_index_0123, log_net_migration_index]

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


library(ggplot2)
#png("compound_flood_neg.png", width = 1200, height = 800) # Width and height in pixels
pdf("compound_flood.pdf", width = 16, height = 8) # Width and height in inches

# Create the ggplot
ggplot(df2, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(aes(color = "Trend"), size = 2) + # Make the trend line thicker
  geom_point(aes(color = "Point Estimates"), size = 4) + # Make the points larger
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "lightblue", size = 1) + # Add error bars for confidence intervals, with specified line thickness
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) + # Add a black dashed line at y = 0
  scale_color_manual(values = c("Trend" = "orange", "Point Estimates" = "lightblue")) + # Customize colors for the trend and point estimates/error bars
  labs(
    title = "",
    x = "",
    y = "Point Estimate of Net Migration"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 40), # Increase X axis label size
    axis.title.y = element_text(size = 34), # Increase Y axis label size
    axis.text.x = element_text(size = 30), # Increase X axis tick label size
    axis.text.y = element_text(size = 30) # Increase Y axis tick label size
  ) +
  theme(legend.position = "none") + # Remove the legend
  scale_y_continuous(limits = c(-1.5, 1.5), breaks = seq(-1.5, 1.5, by = 0.5))

# Close the PNG device to save the file
dev.off()

################################################
# BVAR compound drought events
################################################

data_compound_drought<-subset(data, select = c("log_net_migration", 
                                                            "flood_threshold", "flood_threshold_1","flood_threshold_2", "flood_threshold_3", "flood_threshold_4", "flood_threshold_5",
                                                            "soilmoist_contin", "soilmoist_contin_1", "soilmoist_contin_2", "soilmoist_contin_3", "soilmoist_contin_4", "soilmoist_contin_5",
                                                            "top_cyc"  ,                    "top_cyc_1"   ,                 "top_cyc_2"  ,                  "top_cyc_3" ,                  
                                                            "top_cyc_4"   ,                 "top_cyc_5" ,
                                                            "log_armed_conflict_brd_lag1",   
                                                            "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                                            "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                                            "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                                            "shdi_lag1"))
data_compound_drought<-na.omit(data_compound_drought)


data_compound_drought_neg<-subset(net_neg, select = c("log_net_migration", 
                                                      "flood_threshold", "flood_threshold_1","flood_threshold_2", "flood_threshold_3", "flood_threshold_4", "flood_threshold_5",
                                                      "soilmoist_contin", "soilmoist_contin_1", "soilmoist_contin_2", "soilmoist_contin_3", "soilmoist_contin_4", "soilmoist_contin_5",
                                                      "top_cyc"  ,                    "top_cyc_1"   ,                 "top_cyc_2"  ,                  "top_cyc_3" ,                  
                                                      "top_cyc_4"   ,                 "top_cyc_5" ,
                                                      "log_armed_conflict_brd_lag1",   
                                                      "log_com_conflict_brd_lag1" ,  "log_osv_conflict_brd_lag1"  ,
                                                      "v2x_libdem_lag1",   "log_world_pop_sum_l1" ,
                                                      "ethnic_status_bin_size_lag1",    "log_gdp_pc_lag1","log_area",
                                                      "shdi_lag1"))

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
                                                      "shdi_lag1"))

data_compound_drought_pos<-na.omit(data_compound_drought_pos)

data_compound_drought$drought_0_1 <- ifelse(data_compound_drought$soilmoist_contin > 0 & data_compound_drought$soilmoist_contin_1 > 0,1, 0) 

data_compound_drought$drought_0_1_2 <- ifelse(data_compound_drought$soilmoist_contin > 0 & data_compound_drought$soilmoist_contin_1 > 0 & data_compound_drought$soilmoist_contin_2 > 0,1, 0 )

data_compound_drought$drought_0_1_2_3 <- ifelse(data_compound_drought$soilmoist_contin > 0 & data_compound_drought$soilmoist_contin_1 > 0 & data_compound_drought$soilmoist_contin_2 > 0
                                            & data_compound_drought$soilmoist_contin_3 > 0,1, 0) 



data_compound_drought_pos$drought_0_1 <- ifelse(data_compound_drought_pos$soilmoist_contin > 0 & data_compound_drought_pos$soilmoist_contin_1 > 0,1, 0) 

data_compound_drought_pos$drought_0_1_2 <- ifelse(data_compound_drought_pos$soilmoist_contin > 0 & data_compound_drought_pos$soilmoist_contin_1 > 0 & data_compound_drought_pos$soilmoist_contin_2 > 0,1, 0 )

data_compound_drought_pos$drought_0_1_2_3 <- ifelse(data_compound_drought_pos$soilmoist_contin > 0 & data_compound_drought_pos$soilmoist_contin_1 > 0 & data_compound_drought_pos$soilmoist_contin_2 > 0
                                                    & data_compound_drought_pos$soilmoist_contin_3 > 0,1, 0) 


data_compound_drought_neg$drought_0_1 <- ifelse(data_compound_drought_neg$soilmoist_contin > 0 & data_compound_drought_neg$soilmoist_contin_1 > 0,1, 0) 

data_compound_drought_neg$drought_0_1_2 <- ifelse(data_compound_drought_neg$soilmoist_contin > 0 & data_compound_drought_neg$soilmoist_contin_1 > 0 & data_compound_drought_neg$soilmoist_contin_2 > 0,1, 0 )

data_compound_drought_neg$drought_0_1_2_3 <- ifelse(data_compound_drought_neg$soilmoist_contin > 0 & data_compound_drought_neg$soilmoist_contin_1 > 0 & data_compound_drought_neg$soilmoist_contin_2 > 0
                                                    & data_compound_drought_neg$soilmoist_contin_3 > 0,1, 0) 


# Percentage of compound events for each variable
percent_drought_0 <- mean(data_compound_drought$soilmoist_contin > 0) * 100
percent_drought_0_1 <- mean(data_compound_drought$drought_0_1 > 0) * 100
percent_drought_0_1_2 <- mean(data_compound_drought$drought_0_1_2 > 0) * 100
percent_drought_0_1_2_3 <- mean(data_compound_drought$drought_0_1_2_3 > 0) * 100
percent_drought_0_1_2_3_4 <- mean(data_compound_drought$drought_0_1_2_3_4 > 0) * 100
percent_drought_0_1_2_3_4_5 <- mean(data_compound_drought$drought_0_1_2_3_4_5 > 0) * 100


# Display the results
#57
percent_drought_0_1
# 38
percent_drought_0_1_2
# 27
percent_drought_0_1_2_3
# 20

percent_drought_0_1_2_3_4
# 16

library(plotrix)
#install.packages("RColorBrewer")
library(RColorBrewer)

categories <- c("1-year","2-year", "3-year", "4-year")
values <- c(57, 38, 27, 20)
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


drought_index_1 <- which(compound_drought$variables == "soilmoist_contin_1")
drought_index_12 <- which(compound_drought$variables == "drought_0_1")
drought_index_123 <- which(compound_drought$variables == "drought_0_1_2")
drought_index_1234 <- which(compound_drought$variables == "drought_0_1_2_3") 

log_net_migration_index<- which(compound_drought$variables == "log_net_migration")






# Extracting the posterior samples for this coefficient
# Assuming it's the first response variable (index 1 in the third dimension if it's multidimensional)
posterior_samples_1 <- compound_drought$beta[, drought_index_1, log_net_migration_index]
posterior_samples_2 <- compound_drought$beta[, drought_index_12, log_net_migration_index]
posterior_samples_3 <- compound_drought$beta[, drought_index_123, log_net_migration_index]
posterior_samples_4 <- compound_drought$beta[, drought_index_1234, log_net_migration_index]



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



library(ggplot2)
#png("compound_drought_pos.png", width = 1200, height = 800) # Width and height in pixels
pdf("compound_drought.pdf", width = 16, height = 8) # Width and height in inches

# Create the ggplot
ggplot(df3, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(aes(color = "Trend"), size = 2) + # Make the trend line thicker
  geom_point(aes(color = "Point Estimates"), size = 4) + # Make the points larger
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "lightblue", size = 1) + # Add error bars for confidence intervals, with specified line thickness
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) + # Add a black dashed line at y = 0
  scale_color_manual(values = c("Trend" = "orange", "Point Estimates" = "lightblue")) + # Customize colors for the trend and point estimates/error bars
  labs(
    title = "",
    x = "",
    y = "Point Estimate of Net Migration"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 40), # Increase X axis label size
    axis.title.y = element_text(size = 34), # Increase Y axis label size
    axis.text.x = element_text(size = 30), # Increase X axis tick label size
    axis.text.y = element_text(size = 30) # Increase Y axis tick label size
  ) +
  theme(legend.position = "none") + # Remove the legend
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.2)) # Set y-axis limits and breaks

dev.off()

# Create the plot
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

# --- Set global white background ---
theme_set(theme_minimal(base_size = 12) +
            theme(plot.background = element_rect(fill = "white", color = NA)))

# --- Helper: y-axis label formatting ---
adjust_neg_plot <- function(p) {
  p + labs(y = "Point Estimate of Net Migration") +
    theme(
      plot.title = element_blank(),
      axis.title.y = element_text(size = 15, margin = margin(r = 1))
    )
}

# Adjust individual plots 

# Negative plots (left column)
flood_neg_compound <- adjust_neg_plot(flood_neg_compound)
drought_neg_compound <- adjust_neg_plot(drought_neg_compound + labs(x = NULL))
tc_neg_compound <- adjust_neg_plot(tc_neg_compound + labs(x = "Time lag") +
                                     theme(axis.title.x = element_text(size = 24)))

# Positive plots (right column, no y-axis label)
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

# Add row labels, nudged inside plot area 

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

# Save high-res image 

ggsave(
  filename = "fig5.png",
  plot = final_plot,
  width = 13,
  height = 12,
  dpi = 300,
  bg = "white"
)