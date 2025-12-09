
################################################
# BVAR compound tropical cyclone
################################################
data <- readRDS("data.rds")

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

tc_index <- which(compound_tc$variables == "cyc_share_pop")
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

flood_index <- which(compound_flood$variables == "fld_share_pop")
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


drought_index_1 <- which(compound_drought$variables == "drt_share_pop")
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

# Create Fig. 4
flood_neg_compound <- ggplot(df2, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(color = "orange", linewidth = 0.7) +
  geom_point(color = "lightblue", size = 2) +
  geom_errorbar(
    aes(ymin = CI_Lower, ymax = CI_Upper),
    width    = 0.15,
    color    = "lightblue",
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
    limits = c(-2, 2),
    breaks = seq(-2, 2, by = 0.5)
  ) +
  theme_nature_panel

drought_neg_compound <- ggplot(df3, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(color = "orange", linewidth = 0.7) +
  geom_point(color = "lightblue", size = 2) +
  geom_errorbar(
    aes(ymin = CI_Lower, ymax = CI_Upper),
    width    = 0.15,
    color    = "lightblue",
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
    limits = c(-2, 2),
    breaks = seq(-2, 2, by = 0.5)
  ) +
  theme_nature_panel

tc_neg_compound <- ggplot(df1, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(color = "orange", linewidth = 0.7) +
  geom_point(color = "lightblue", size = 2) +
  geom_errorbar(
    aes(ymin = CI_Lower, ymax = CI_Upper),
    width    = 0.15,
    color    = "lightblue",
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
    limits = c(-2, 2),
    breaks = seq(-2, 2, by = 0.5)
  ) +
  theme_nature_panel


# positive net 

flood_pos_compound <- ggplot(df2, aes(x = Effect, y = PointEstimate, group = 1)) +
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
    limits = c(-2, 2),
    breaks = seq(-2, 2, by = 0.5)
  ) +
  theme_nature_panel

flood_pos_compound

drought_pos_compound <- ggplot(df3, aes(x = Effect, y = PointEstimate, group = 1)) +
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
    limits = c(-2, 2),
    breaks = seq(-2, 2, by = 0.5)
  ) +
  theme_nature_panel
drought_pos_compound

tc_pos_compound <- ggplot(df1, aes(x = Effect, y = PointEstimate, group = 1)) +
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
    limits = c(-2, 2),
    breaks = seq(-2, 2, by = 0.5)
  ) +
  theme_nature_panel

tc_pos_compound

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


# Negative panels: show y-axis label
adjust_neg_plot <- function(p, xlab = NULL) {
  p + labs(
    y = "Point estimate of net migration",
    x = xlab
  )
}

# Positive panels: no y-axis label
adjust_pos_plot <- function(p, xlab = NULL) {
  p + labs(
    y = NULL,
    x = xlab
  )
}



# Negative compound plots
flood_neg_p_comp   <- adjust_neg_plot(flood_neg_compound,   xlab = NULL)
drought_neg_p_comp <- adjust_neg_plot(drought_neg_compound, xlab = NULL)
tc_neg_p_comp      <- adjust_neg_plot(tc_neg_compound,      xlab = "Time lag")

# Positive compound plots
flood_pos_p_comp   <- adjust_pos_plot(flood_pos_compound,   xlab = NULL)
drought_pos_p_comp <- adjust_pos_plot(drought_pos_compound, xlab = NULL)
tc_pos_p_comp      <- adjust_pos_plot(tc_pos_compound,      xlab = "Time lag")


## Build rows with panel labels

row_flood_comp <- plot_grid(
  flood_neg_p_comp, flood_pos_p_comp,
  nrow           = 1,
  rel_widths     = c(1, 1),
  align          = "h",
  labels         = c("a", "b"),
  label_size     = 7,
  label_fontface = "bold",
  label_y        = 1.05
)

row_drought_comp <- plot_grid(
  drought_neg_p_comp, drought_pos_p_comp,
  nrow           = 1,
  rel_widths     = c(1, 1),
  align          = "h",
  labels         = c("c", "d"),
  label_size     = 7,
  label_fontface = "bold",
  label_y        = 1.05
)

row_tc_comp <- plot_grid(
  tc_neg_p_comp, tc_pos_p_comp,
  nrow           = 1,
  rel_widths     = c(1, 1),
  align          = "h",
  labels         = c("e", "f"),
  label_size     = 7,
  label_fontface = "bold",
  label_y        = 1.05
)


## Add left-hand row labels 

row_flood_comp <- ggdraw() +
  draw_plot(row_flood_comp, x = 0.08, y = 0, width = 0.92, height = 1) +
  draw_label(
    "Flood",
    x        = 0.015,
    y        = 0.5,
    angle    = 90,
    size     = 7,
    fontface = "bold",
    hjust    = 0.5
  )

row_drought_comp <- ggdraw() +
  draw_plot(row_drought_comp, x = 0.08, y = 0, width = 0.92, height = 1) +
  draw_label(
    "Drought",
    x        = 0.015,
    y        = 0.5,
    angle    = 90,
    size     = 7,
    fontface = "bold",
    hjust    = 0.5
  )

row_tc_comp <- ggdraw() +
  draw_plot(row_tc_comp, x = 0.08, y = 0, width = 0.92, height = 1) +
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

titles_comp <- plot_grid(
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




final_plot_comp <- plot_grid(
  titles_comp,
  row_flood_comp,
  row_drought_comp,
  row_tc_comp,
  ncol        = 1,
  rel_heights = c(0.18, 1, 1, 1)
)

final_plot_comp_with_legend <- plot_grid(
  final_plot_comp,
  shared_legend,
  ncol        = 1,
  rel_heights = c(1, 0.12)
)


## 8. Save as PDF 

w_in <- 188 / 25.4
h_in <- 180 / 25.4  

quartz(
  file  = "fig4.pdf",
  type  = "pdf",
  width = w_in,
  height = h_in
)

print(final_plot_comp_with_legend)
dev.off()

# Create Figure S13
# Full compound model

library(ggplot2)

## Tropical cyclone – full model
tc_full <- ggplot(df1, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(color = "orange", linewidth = 0.7) +
  geom_point(color = "lightblue", size = 2) +
  geom_errorbar(
    aes(ymin = CI_Lower, ymax = CI_Upper),
    width    = 0.15,
    color    = "lightblue",
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
    limits = c(-0.8, 0.8),
    breaks = seq(-0.8, 0.8, by = 0.2)
  ) +
  theme_nature_panel


## Flood – full model
flood_full <- ggplot(df2, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(color = "orange", linewidth = 0.7) +
  geom_point(color = "lightblue", size = 2) +
  geom_errorbar(
    aes(ymin = CI_Lower, ymax = CI_Upper),
    width    = 0.15,
    color    = "lightblue",
    linewidth = 0.6
  ) +
  geom_hline(
    yintercept = 0,
    linetype   = "dashed",
    color      = "black",
    linewidth  = 0.5
  ) +
  labs(
    x = "",
    y = "Point estimate of net migration",
    title = ""
  ) +
  scale_y_continuous(
    limits = c(-0.8, 0.8),
    breaks = seq(-0.8, 0.8, by = 0.2)
  ) +
  theme_nature_panel


## Drought – full model
drought_full <- ggplot(df3, aes(x = Effect, y = PointEstimate, group = 1)) +
  geom_line(color = "orange", linewidth = 0.7) +
  geom_point(color = "lightblue", size = 2) +
  geom_errorbar(
    aes(ymin = CI_Lower, ymax = CI_Upper),
    width    = 0.15,
    color    = "lightblue",
    linewidth = 0.6
  ) +
  geom_hline(
    yintercept = 0,
    linetype   = "dashed",
    color      = "black",
    linewidth  = 0.5
  ) +
  labs(
    x = "",
    y = "Point estimate of net migration",
    title = ""
  ) +
  scale_y_continuous(
    limits = c(-0.8, 0.8),
    breaks = seq(-0.8, 0.8, by = 0.2)
  ) +
  theme_nature_panel

library(ggplot2)
library(cowplot)


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
  theme_minimal(base_family = "Arial") +
  theme(
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.text      = element_text(size = 6),
    panel.grid       = element_blank(),
    panel.background = element_blank(),
    axis.text        = element_blank(),
    axis.title       = element_blank(),
    axis.ticks       = element_blank()
  )

shared_legend <- cowplot::get_legend(legend_plot)


adjust_full_plot <- function(p, xlab = NULL) {
  p +
    labs(
      x = xlab,
      y = "Point estimate of net migration"
    )
}

flood_full_p   <- adjust_full_plot(flood_full,   xlab = NULL)
drought_full_p <- adjust_full_plot(drought_full, xlab = NULL)
tc_full_p      <- adjust_full_plot(tc_full,      xlab = "Time lag")


row_flood_full <- ggdraw() +
  draw_plot(flood_full_p, x = 0.08, width = 0.92, height = 1) +
  draw_label(
    "Flood",
    x = 0.015, y = 0.5,
    angle = 90,
    size = 7, fontface = "bold"
  )

row_drought_full <- ggdraw() +
  draw_plot(drought_full_p, x = 0.08, width = 0.92, height = 1) +
  draw_label(
    "Drought",
    x = 0.015, y = 0.5,
    angle = 90,
    size = 7, fontface = "bold"
  )

row_tc_full <- ggdraw() +
  draw_plot(tc_full_p, x = 0.08, width = 0.92, height = 1) +
  draw_label(
    "Trop. cyclone",
    x = 0.015, y = 0.5,
    angle = 90,
    size = 7, fontface = "bold"
  )



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
h_in <- 170 / 25.4 

quartz(
  file  = "Fig.S13.pdf",
  type  = "pdf",
  width = w_in,
  height = h_in
)

print(full_3panel_with_legend)
dev.off()
