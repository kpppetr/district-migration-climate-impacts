##############################
# Descriptive statistics
#################################
names(data)



# Select variables
data_des <- subset(data, select = c(log_net_migration,
   fld_share_pop, drt_share_pop, cyc_share_pop, crops_0_dev_neg,
  log_armed_conflict_brd, log_com_conflict_brd, log_osv_conflict_brd,
  shdi, ethnic_status_bin_size, 
  log_world_pop_sum, log_gdp_pc, log_area, v2x_libdem
))

# Convert to data.frame 
data_des <- as.data.frame(data_des)

# Define matching variable labels
variable_labels <- c("Log (net migration)", 
  "Floods", "Drought", "Tropical Cyclones", "Crop failure",
  "Log Armed Conflict Brd", "Log Communal Conflict Brd", "Log One-sided Violence Brd",
  "Human Development Index", "Ethnic Group Size", 
  "Log Population", "Log GDP pc", "Log Area", "Liberal Democracy Index"
)

# Export summary table
stargazer(
  data_des, 
  type = "latex", 
  covariate.labels = variable_labels, 
  out = "Summary.tex",
  summary.stat = c("min", "p25", "median", "mean", "p75", "max", "sd")
)

summary(data_des$fld_share_pop)

##################################################
# Plot Distribution of a Variable
##################################################

plot_variable <- function(data, var_name, x_label, x_limits = NULL, y_limits = NULL) {
  var_data <- dplyr::select(data, dplyr::all_of(var_name)) %>% tidyr::drop_na()
  
  p <- ggplot(var_data, aes_string(x = var_name)) +
    geom_density(fill = "skyblue", color = "skyblue", alpha = 0.5, size = 1.5) +
    labs(x = x_label, y = "Density") +
    theme_minimal() +
    theme(
      plot.title = element_blank(),
      axis.title.x = element_text(size = 24),
      axis.title.y = element_text(size = 24),
      axis.text.x  = element_text(size = 22),
      axis.text.y  = element_text(size = 22)
    )
  
  if (!is.null(x_limits)) p <- p + xlim(x_limits)
  if (!is.null(y_limits)) p <- p + ylim(y_limits)
  
  print(p)
}


# ==========================
# Set Variable to Plot
# ==========================
# Select variables
data_des <- subset(data, select = c(log_net_migration,
                                    log_armed_conflict_brd, log_com_conflict_brd, log_osv_conflict_brd,
                                    shdi, ethnic_status_bin_size, 
                                    log_world_pop_sum, log_gdp_pc, log_area, v2x_libdem
))

# Convert to data.frame 
data_des <- as.data.frame(data_des)

variable_names <- names(data_des)

variable_labels <- c(
  "Log Net Migration", 
  "Log Armed Conflict Brd", "Log Non-state Conflict Brd", "Log One-sided Violence Brd",
  "Human Development Index", "Ethnic Group Size", 
  "Log (Population)", "Log (GDP pc)", "Log (area)", "Liberal Democracy Index"
)

# --- Step 1: Long format for ggplot ---
data_long <- data_des %>%
  pivot_longer(cols = all_of(variable_names), names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable, levels = variable_names, labels = variable_labels))

# --- Step 2: Density plot ---
p <- ggplot(data_long, aes(x = value)) +
  geom_density(fill = "skyblue", alpha = 0.6, color = "skyblue") +
  facet_wrap(~ variable, scales = "free", ncol = 4) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 9),     # Reduce facet title size
    axis.text = element_text(size = 8),      # Smaller tick labels
    axis.title = element_text(size = 9),     # Smaller axis titles
    plot.title = element_text(size = 11, face = "bold", hjust = 0)
  ) +
  labs(x = NULL, y = "Density", title = "")


ggsave("density_plots.pdf", plot = p, width = 8.27, height = 11.69, units = "in")

# desnity for climate variables
plot_exposure_density <- function(data, var, title, x_max = 0.10, bw_adjust = 1.3) {
  ggplot(data, aes(x = .data[[var]])) +
    geom_density(fill = "#377eb8", color = "#377eb8", alpha = 0.6,
                 adjust = bw_adjust, na.rm = TRUE) +
    coord_cartesian(xlim = c(0, x_max)) +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    labs(title = title, x = "Share exposed", y = "Density") +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
}

# 1) Flood
p_fld <- plot_exposure_density(
  data, "fld_share_pop",
  "Flood"
)
ggsave("density_flood_zoom_0_10.pdf", p_fld, width = 6.5, height = 4.5, dpi = 300)

# 2) Drought
p_drt <- plot_exposure_density(
  data, "drt_share_pop",
  "Drought"
)
ggsave("density_drought_zoom_0_10.pdf", p_drt, width = 6.5, height = 4.5, dpi = 300)

# 3) Tropical cyclones
p_cyc <- plot_exposure_density(
  data, "cyc_share_pop",
  "Tropical cyclone"
)
ggsave("density_tc_zoom_0_10.pdf", p_cyc, width = 6.5, height = 4.5, dpi = 300)

library(patchwork)
p_exposures <- (p_fld | p_drt | p_cyc) +
  plot_annotation(tag_levels = "a", tag_suffix = ".")
ggsave("density_climate.pdf", p_exposures, width = 16, height = 4.8, dpi = 300)

######################################
# climate events over time
######################################

library(tidyverse)
data_des$soilmoist_bin <- ifelse(data_des$soilmoist_contin > 0, 1, 0)
data_des$floods_bin <- ifelse(data_des$flood_threshold > 0, 1, 0)


# Replace NA with 0 in the specified columns and filter the data for the years 2000 to 2019
data2 <- data_des %>%
  mutate(
    soilmoist_bin = coalesce(soilmoist_bin, 0),
    floods_bin = coalesce(floods_bin, 0),
    top_cyc = coalesce(top_cyc, 0),
    crops_20pctneg = coalesce(crops_20pctneg, 0),
  ) %>%
  filter(year >= 2000, year <= 2019)

# Sum the binary values within each year to get the total count of events per year
data_summed <- data2 %>%
  group_by(year) %>%
  summarize(
    total_droughts = sum(soilmoist_bin, na.rm = TRUE),
    total_floods = sum(floods_bin, na.rm = TRUE),
    total_tc_cat = sum(top_cyc, na.rm = TRUE),
    total_crop = sum(crops_20pctneg, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = c(total_droughts, total_floods, total_tc_cat, total_crop),
    names_to = "event_type",
    values_to = "event_count"
  )

# Plot the data using ggplot2 with a stacked area chart
data_summed$event_type <- factor(data_summed$event_type, levels = c("total_floods", "total_droughts", "total_crop", "total_tc_cat"))

# Plotting with ggplot
ggplot(data_summed, aes(x = year, y = event_count, fill = event_type)) + 
  geom_area(position = 'stack', color = "black", size = 0.25) + 
  scale_fill_manual(values = c("total_floods" = "skyblue", "total_droughts" = "orange", "total_crop" = "pink", "total_tc_cat" = "purple"),
                    labels = c("Floods", "Drought", "Crop Failure", "Tropical Cyclone")) +
  theme_bw() + 
  labs(y = "Number of Events", x = "Year", title = "", fill = "Event Type") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )


summary(data$change_net_migration_1)

# Linear trend plots
# Load libraries
library(dplyr)
library(ggplot2)
library(tidyr)


variable_names <- names(data_des)

# Define shorter labels 
variable_labels <- c(
  "Log Net Migration", 
  "Floods", "Drought", "Tropical Cyclones",
  "Log Armed Conflict", "Log Non-state Conflict", "Log One-sided Violence",
  "Human Dev. Index", "Ethnic Group Size", 
  "Log Population", "Log GDP per Capita", "Log Area", "Lib. Democracy"
)

# Aggregate means by year 
aggregated_data <- data %>%
  dplyr::select(year, dplyr::all_of(variable_names)) %>%
  group_by(year) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  ungroup()

#  Convert to long format 
aggregated_long <- aggregated_data %>%
  pivot_longer(cols = -year, names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable, levels = variable_names, labels = variable_labels))

#  Plot 
p_trend <- ggplot(aggregated_long, aes(x = year, y = value)) +
  geom_line(color = "steelblue", size = 1.2) +
  facet_wrap(~ variable, scales = "free_y", ncol = 3) +
  theme_minimal() +
  labs(x = "Year", y = "Mean Value", title = "") +
  theme(
    strip.text = element_text(size = 14),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    plot.title = element_text(size = 24, face = "bold", hjust = 0)
  )

# --- Save ---
ggsave("trend_plots.pdf", p_trend, width = 11, height = 13, units = "in")

################################################
# Histogram on the distribution of district with positive and negative net 
####################################################
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
  filter(positive_years > 10) %>%
  pull(GID_2)

negative_districts <- summary_table %>%
  filter(negative_years > 10) %>%
  pull(GID_2)

# Step 3: Subset the original data based on the filtered districts
net_pos <- data %>%
  filter(GID_2 %in% positive_districts)

net_neg <- data %>%
  filter(GID_2 %in% negative_districts)


library(ggplot2)

# Combine positive and negative summaries
summary_long <- summary_table %>%
  pivot_longer(cols = c(positive_years, negative_years), names_to = "type", values_to = "years")

# Filter only the relevant districts
summary_long_filtered <- summary_long %>%
  filter(GID_2 %in% positive_districts | GID_2 %in% negative_districts)

# Plot the histogram
ggplot(summary_long_filtered, aes(x = years, fill = type)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(title = "",
       x = "Number of Years",
       y = "Number of Districts") +
  theme_minimal()

############################
# correlation matrix
#############################
require(ggpubr)
require(tidyverse)
require(Hmisc)
require(corrplot)

names(data)


cor_data <- subset(data, select = c(log_net_migration,
                                    fld_share_pop, drt_share_pop, cyc_share_pop,
                                    log_armed_conflict_brd, log_com_conflict_brd, log_osv_conflict_brd,
                                    shdi, ethnic_status_bin_size, 
                                    log_world_pop_sum, log_gdp_pc, log_area, v2x_libdem
))

# Rename columns for clarity
colnames(cor_data) <- c(
  "Log Net Migration", "Floods", "Drought", "Tropical Cyclones", 
  "Log Armed Conflict", "Log Non-state Conflict", 
  "Log One-sided Violence", "Human Dev. Index", "Ethnic Group Size", 
  "Log Population", "Log GDP per Capita", "Log Area", "Lib. Democracy"
)

# Compute correlation matrix with complete observations
M <- cor(cor_data, use = "complete.obs")

# Color palette
col <- colorRampPalette(c("darkblue", "white", "darkorange"))(100)
breaks <- seq(-1, 1, length.out = length(col) + 1)

# Save high-res heatmap
png(filename = "heatmap.png", width = 1600, height = 1600)
heatmap(
  x = M, 
  col = col, 
  breaks = breaks, 
  symm = TRUE, 
  margins = c(35, 35), 
  cexRow = 4, 
  cexCol = 4
)
dev.off()

# Combining maps for Fig.1 

# install.packages("magick")
library(magick)

# Read images 
a <- image_read("top_cyc_mapped_mean.png")  
b <- image_read("floods_mapped_mean.png")
c <- image_read("droughts_mapped_mean.png")
d <- image_read("crops_mapped_mean.png")

# Make them the same size
target_w <- 1600  
target_h <- 1000

resize_crop <- function(im) {
  im %>%
    image_resize(paste0(target_w, "x", target_h)) %>%              # fit inside (no zoom)
    image_resize("85%") %>%                                       # gentle zoom in (tweak 105–115%)
    image_extent(paste0(target_w, "x", target_h), color = "white") %>% 
    image_trim(fuzz = 5)
}


a <- resize_crop(a)
image_info(a)
b <- resize_crop(b)
c <- resize_crop(c)
d <- resize_crop(d)

# Step 2: Add bold corner labels with slight margin
label_map <- function(im, lab) {
  image_annotate(
    im, lab,
    size = 35, weight = 700,
    gravity = "northwest", location = "+40-5",
    color = "black"
  )
}

a <- label_map(a, "a.")
b <- label_map(b, "b.")
c <- label_map(c, "c.")
d <- label_map(d, "d.")

gap <- 20  # pixels of white border between images
add_gap <- function(im) image_border(im, "white", paste0(gap, "x", gap))

a <- add_gap(a)
b <- add_gap(b)
c <- add_gap(c)
d <- add_gap(d)


# Step 3: Combine tightly
top    <- image_append(c(a, b))           # side by side
bottom <- image_append(c(c, d))           # side by side
panel  <- image_append(c(top, bottom), stack = TRUE)  # stack rows

# Optional: add small border for neatness
panel <- image_border(panel, "white", "20x20")

image_write(panel, "maps_2x2_panel.png", density = 300)

