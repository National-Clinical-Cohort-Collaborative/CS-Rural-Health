library(ggplot2)
library(dplyr)
library(cowplot)
library(scales) 
library(tidyr)
library(forestploter)
library(grid)

# Figure 2

# Load the data
data <- read.csv('/path/to/csv')

# Extract values and CIs
extract_values_and_cis <- function(column) {
  values <- gsub(" \\(.*\\)", "", column)
  ci_lower <- gsub(".*\\((.*), .*\\)", "\\1", column)
  ci_upper <- gsub(".*, (.*)\\)", "\\1", column)
  values <- as.numeric(gsub(",", "", values))
  ci_lower <- as.numeric(gsub(",", "", ci_lower))
  ci_upper <- as.numeric(gsub(",", "", ci_upper))
  return(data.frame(values, ci_lower, ci_upper))
}

# Extract values and CIs for Event Rate and Risk Difference
event_rate_data <- extract_values_and_cis(data$Event_Rate)
risk_diff_data <- extract_values_and_cis(data$Risk_Difference)

# Combine the extracted data back into the main dataframe
data <- data %>%
  mutate(
    EventRate_Value = event_rate_data$values,
    EventRate_LowerCI = event_rate_data$ci_lower,
    EventRate_UpperCI = event_rate_data$ci_upper,
    RiskDifference_Value = risk_diff_data$values,
    RiskDifference_LowerCI = risk_diff_data$ci_lower,
    RiskDifference_UpperCI = risk_diff_data$ci_upper
  )

# Ensure the order of the categories
data$Rurality <- factor(data$Rurality, levels = c("Nonurban-Adjacent Rural", "Urban-Adjacent Rural", "Urban"))

# Define base colors for the plots
base_colors <- c(
  "Urban" = "#04A0D2BF",
  "Urban-Adjacent Rural" = "#AF4745BF",
  "Nonurban-Adjacent Rural" = "#78AF96BF"
)

darkened_colors <- sapply(base_colors, function(col) adjustcolor(col, 0.8))

# Function to create plots with smaller legend text size
create_plot <- function(data, measure, lower_ci, upper_ci) {
  ggplot(data, aes(x = Rurality, y = get(paste0(measure, "_Value")), fill = Rurality)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
    geom_errorbar(aes(ymin = get(lower_ci), ymax = get(upper_ci), color = Rurality), width = 0.2, position = position_dodge(0.7)) +
    scale_fill_manual(values = base_colors) +
    scale_color_manual(values = darkened_colors) +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 8),  # Adjust legend text size
          legend.title = element_text(size = 9), # Adjust legend title size
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_blank()) +
    coord_flip() +
    scale_y_continuous(labels = scales::comma)
}

# Create individual plots and arrange them in a grid with a shared title for each time point
plots <- list()
time_points <- c("1 Month", "3 Months", "1 Year", "2 Years")

# Modify the function that adds the title with smaller font size
for (time in time_points) {
  event_rate_plot <- create_plot(data %>% filter(Time == time), "EventRate", "EventRate_LowerCI", "EventRate_UpperCI")
  risk_diff_plot <- create_plot(data %>% filter(Time == time), "RiskDifference", "RiskDifference_LowerCI", "RiskDifference_UpperCI")
  combined_plot <- plot_grid(event_rate_plot,
                             risk_diff_plot,
                             ncol = 2)
  title <- ggdraw() + draw_label(paste("Death Within", time), fontface = 'bold', size = 8)  
  plots[[time]] <- plot_grid(title, combined_plot, ncol = 1, rel_heights = c(0.1, 1))
}

final_plot <- plot_grid(plots[["1 Month"]],
                        plots[["3 Months"]],
                        plots[["1 Year"]],
                        plots[["2 Years"]],
                        ncol = 1)

legend <- get_legend(create_plot(data %>% filter(Time == "1 Month"),
                                 "EventRate",
                                 "EventRate_LowerCI",
                                 "EventRate_UpperCI"))

column_header <- plot_grid(
  ggdraw() + draw_label("Mortality Rate per\n100,000 Persons (95% CI)", hjust = 0.5, fontface = 'bold', size = 8),  
  ggdraw() + draw_label("Excess Mortality per \n100,000 Persons (95% CI)", hjust = 0.5, fontface = 'bold', size = 8),  
  ncol = 2
)

# Combine everything into the final plot
final_combined_plot <- plot_grid(column_header, final_plot, legend, ncol = 1, rel_heights = c(0.05, 1, 0.1))



# Figure 3

hazard_data <- read.csv('path/to/csv')

# Extract values and CIs
extract_values_and_cis <- function(column) {
  values <- gsub(" \\(.*\\)", "", column)
  ci_lower <- gsub(".*\\((.*), .*\\)", "\\1", column)
  ci_upper <- gsub(".*, (.*)\\)", "\\1", column)
  values <- as.numeric(values)
  ci_lower <- as.numeric(ci_lower)
  ci_upper <- as.numeric(ci_upper)
  return(data.frame(values, ci_lower, ci_upper))
}

# Extract values and CIs for Unadjusted and Adjusted Hazard Ratios
Unadjusted_data <- extract_values_and_cis(hazard_data$Unadjusted)
Adjusted_data <- extract_values_and_cis(hazard_data$Adjusted)

# Combine the extracted data back into the main dataframe
hazard_data <- hazard_data %>%
  mutate(
    Unadjusted_Value = Unadjusted_data$values,
    Unadjusted_LowerCI = Unadjusted_data$ci_lower,
    Unadjusted_UpperCI = Unadjusted_data$ci_upper,
    Adjusted_Value = Adjusted_data$values,
    Adjusted_LowerCI = Adjusted_data$ci_lower,
    Adjusted_UpperCI = Adjusted_data$ci_upper
  )

hazard_data$Rurality <- factor(hazard_data$Rurality, levels = c("Nonurban-Adjacent Rural", "Urban-Adjacent Rural", "Urban (Reference)"))

base_colors <- c(
  "Urban (Reference)" = "#04A0D2",
  "Urban-Adjacent Rural" = "#AF4745",
  "Nonurban-Adjacent Rural" = "#78AF96"
)

darkened_colors <- sapply(base_colors, function(col) adjustcolor(col, 0.8))

# Function to create hazard ratio plots
create_hr_plot <- function(data, measure, lower_ci, upper_ci, show_legend = TRUE) {
  ggplot(data, aes(x = Rurality, y = get(paste0(measure, "_Value")), color = Rurality)) +
    geom_point(aes(color = Rurality), size = 3, show.legend = show_legend) +
    geom_errorbar(aes(ymin = get(lower_ci), ymax = get(upper_ci), color = Rurality), width = 0.1, size = 0.8, show.legend = show_legend) +
    scale_color_manual(values = darkened_colors, name = "Error Bar Color") +
    scale_fill_manual(values = base_colors, name = "Point Color") +
    theme_minimal() +
    theme(legend.position = "bottom", axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_blank()) +
    coord_flip() +
    scale_y_continuous(trans = 'log10', limits = c(1, 2.0), breaks = c(1, 1.5, 2.0), labels = c("1", "1.5", "2.0"))
}

time_points <- unique(hazard_data$Time)

plots <- list()

for (time in time_points) {
  Unadjusted_plot <- create_hr_plot(hazard_data %>% filter(Time == time), "Unadjusted", "Unadjusted_LowerCI", "Unadjusted_UpperCI", show_legend = FALSE)
  Adjusted_plot <- create_hr_plot(hazard_data %>% filter(Time == time), "Adjusted", "Adjusted_LowerCI", "Adjusted_UpperCI", show_legend = FALSE)
  combined_plot <- plot_grid(Unadjusted_plot, Adjusted_plot, ncol = 2)
  title <- ggdraw() + draw_label(paste(time), fontface = 'bold')
  plots[[time]] <- plot_grid(title, combined_plot, ncol = 1, rel_heights = c(0.1, 1))
}

final_plot <- plot_grid(plotlist = plots, ncol = 1)

legend <- get_legend(create_hr_plot(hazard_data %>% filter(Time == time_points[1]),
                                    "Unadjusted",
                                    "Unadjusted_LowerCI",
                                    "Unadjusted_UpperCI", show_legend = TRUE) + theme(legend.position = "bottom"))

column_header <- plot_grid(
  ggdraw() + draw_label("Unadjusted Hazard Ratio (95% CI)", hjust = 0.5, fontface = 'bold'),
  ggdraw() + draw_label("Adjusted Hazard Ratio (95% CI)", hjust = 0.5, fontface = 'bold'),
  ncol = 2
)

# Combine everything into the final plot
final_combined_plot <- plot_grid(column_header, final_plot, legend, ncol = 1, rel_heights = c(0.05, 1, 0.1))

# 700 x 900
print(final_combined_plot)




# Figure 4
post_covid_condition_overall <- read.csv('/path/to/csv')

base_colors <- c(
  "Urban" = "#04A0D2",
  "Urban-Adjacent Rural" = "#AF4745",
  "Nonurban-Adjacent Rural" = "#78AF96"
)

extract_percentages <- function(column) {
  as.numeric(gsub(".*\\((\\d+\\.?\\d*)%\\).*", "\\1", column))
}

post_covid_condition_overall <- post_covid_condition_overall %>%
  mutate(Urban = extract_percentages(Urban),
         Urban_Adjacent_Rural = extract_percentages(Urban_Adjacent_Rural),
         Nonurban_Adjacent_Rural = extract_percentages(Nonurban_Adjacent_Rural))

data_long <- post_covid_condition_overall %>%
  pivot_longer(cols = c("Urban", "Urban_Adjacent_Rural", "Nonurban_Adjacent_Rural"),
               names_to = "Category",
               values_to = "Percentage")

data_long <- data_long %>%
  mutate(Category = factor(recode(Category,
                                  "Urban_Adjacent_Rural" = "Urban-Adjacent Rural",
                                  "Nonurban_Adjacent_Rural" = "Nonurban-Adjacent Rural"),
                           levels = c("Nonurban-Adjacent Rural", "Urban-Adjacent Rural", "Urban")))

event_order <- rev(c(
  "Any Post-Acute-COVID-19 Condition",
  "Long COVID",
  "SARS-CoV-2 Reinfection",
  "Abnormality of the Respiratory System",
  "Abnormality of the Cardiovascular System",
  "Abnormality of the Immune System",
  "Abnormality of the Nervous System",
  "Constitutional Symptom",
  "Abnormality of Metabolism Homeostasis",
  "Abnormality of Blood And Blood Forming Tissues",
  "Abnormality of the Digestive System",
  "Abnormality of the Endocrine System",
  "Abnormality of the Musculoskeletal System",
  "Abnormality of the Integument",
  "Abnormality of the Genitourinary System",
  "Malignant Neoplasm"
))

data_long$Event <- factor(data_long$Event, levels = event_order)

# Create the grouped bar chart with percentages to the left of each bar
ggplot(data_long, aes(x = Event, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = paste0(Percentage, "%"), color = Category),
            position = position_dodge(width = 0.9), hjust = -0.1, size = 3.5) + 
  labs(title = NULL,
       x = NULL, y = "Percentage (%) New-Onset Event or Abnormality") +
  scale_fill_manual(values = base_colors,
                    labels = c("Nonurban-Adjacent Rural", "Urban-Adjacent Rural", "Urban")) +
  scale_color_manual(values = base_colors, guide = "none") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",  # Move the legend to the bottom
        legend.title = element_blank()) +
  coord_flip() +  # Flip the plot horizontally
  expand_limits(y = max(data_long$Percentage) + 5) +  
  scale_y_continuous(breaks = seq(0, 100, by = 10),  
                     labels = function(x) paste0(x, "%"))  





# Figure 5
hazard_data <- read.csv('/path/to/csv')

# Extract values and CIs
extract_values_and_cis <- function(column) {
  values <- gsub(" \\(.*\\)", "", column)
  ci_lower <- gsub(".*\\((.*), .*\\)", "\\1", column)
  ci_upper <- gsub(".*, (.*)\\)", "\\1", column)
  values <- as.numeric(values)
  ci_lower <- as.numeric(ci_lower)
  ci_upper <- as.numeric(ci_upper)
  return(data.frame(values, ci_lower, ci_upper))
}

# Extract values and CIs for Unadjusted and Adjusted Hazard Ratios
Unadjusted_data <- extract_values_and_cis(hazard_data$Unadjusted)
Adjusted_data <- extract_values_and_cis(hazard_data$Adjusted)

hazard_data <- hazard_data %>%
  mutate(
    Unadjusted_Value = Unadjusted_data$values,
    Unadjusted_LowerCI = Unadjusted_data$ci_lower,
    Unadjusted_UpperCI = Unadjusted_data$ci_upper,
    Adjusted_Value = Adjusted_data$values,
    Adjusted_LowerCI = Adjusted_data$ci_lower,
    Adjusted_UpperCI = Adjusted_data$ci_upper
  )

hazard_data$Rurality <- factor(hazard_data$Rurality, levels = c("Nonurban-Adjacent Rural", "Urban-Adjacent Rural", "Urban (Reference)"))

base_colors <- c(
  "Urban (Reference)" = "#04A0D2",
  "Urban-Adjacent Rural" = "#AF4745",
  "Nonurban-Adjacent Rural" = "#78AF96"
)

darkened_colors <- sapply(base_colors, function(col) adjustcolor(col, 0.8))

create_hr_plot <- function(data, measure, lower_ci, upper_ci, show_legend = TRUE) {
  ggplot(data, aes(x = Rurality, y = get(paste0(measure, "_Value")), color = Rurality)) +
    geom_point(aes(color = Rurality), size = 3, show.legend = show_legend) +
    geom_errorbar(aes(ymin = get(lower_ci), ymax = get(upper_ci), color = Rurality), width = 0.1, size = 0.8, show.legend = show_legend) +
    scale_color_manual(values = darkened_colors, name = "Error Bar Color") +
    scale_fill_manual(values = base_colors, name = "Point Color") +
    theme_minimal() +
    theme(legend.position = "bottom", axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_blank()) +
    coord_flip() +
    scale_y_continuous(trans = 'log10', limits = c(1, 1.5), breaks = c(1, 1.5), labels = c("1", "1.5"))
}

time_points <- unique(hazard_data$Time)

plots <- list()

for (time in time_points) {
  Unadjusted_plot <- create_hr_plot(hazard_data %>% filter(Time == time), "Unadjusted", "Unadjusted_LowerCI", "Unadjusted_UpperCI", show_legend = FALSE)
  Adjusted_plot <- create_hr_plot(hazard_data %>% filter(Time == time), "Adjusted", "Adjusted_LowerCI", "Adjusted_UpperCI", show_legend = FALSE)
  combined_plot <- plot_grid(Unadjusted_plot, Adjusted_plot, ncol = 2)
  title <- ggdraw() + draw_label(paste(time), fontface = 'bold')
  plots[[time]] <- plot_grid(title, combined_plot, ncol = 1, rel_heights = c(0.1, 1))
}

final_plot <- plot_grid(plotlist = plots, ncol = 1)

legend <- get_legend(create_hr_plot(hazard_data %>% filter(Time == time_points[1]),
                                    "Unadjusted",
                                    "Unadjusted_LowerCI",
                                    "Unadjusted_UpperCI", show_legend = TRUE) + theme(legend.position = "bottom"))

column_header <- plot_grid(
  ggdraw() + draw_label("Unadjusted Hazard Ratio (95% CI)", hjust = 0.5, fontface = 'bold'),
  ggdraw() + draw_label("Adjusted Hazard Ratio (95% CI)", hjust = 0.5, fontface = 'bold'),
  ncol = 2
)

# Combine everything into the final plot
final_combined_plot <- plot_grid(column_header, final_plot, legend, ncol = 1, rel_heights = c(0.05, 1, 0.1))

# 700 x 900
print(final_combined_plot)

# Figure 6A

data <- read.csv('/path/to/csv')

data$Rural_CI <- sprintf("%.2f (%.2f, %.2f)", data$Rural_HR, data$Rural_LowerCI, data$Rural_UpperCI)
data$Urban_CI <- sprintf("%.2f (%.2f, %.2f)", data$Urban_HR, data$Urban_LowerCI, data$Urban_UpperCI)

data$Blank_Rural <- paste(rep(" ", 20), collapse = " ")
data$Blank_Urban <- paste(rep(" ", 20), collapse = " ")

tm <- forest_theme(
  base_size = 9,
  ci_pch = c(16, 16),
  ci_col = "black",
  ci_fill = "black",
  ci_alpha = 0.8,
  ci_lty = 1,
  ci_lwd = 1.5,
  ci_Theight = 0.2, # Set a T end at the end of CI
  # Reference line width/type/color
  refline_lwd = gpar(lwd = 1, lty = "dashed", col = "grey20"),
  # Vertical line width/type/color
  vertline_lwd = 1,
  vertline_lty = "dashed",
  vertline_col = "grey20",
  footnote_cex = 1,
  legend_name = "Group",  # Legend for rural and urban
  legend_value = c("Rural", "Urban"),
  core = list(padding = unit(c(4, 3), "mm")) 
)


p_combined <- forest(
  data[, c("Covariate", "Rural_CI", "Blank_Rural", "Urban_CI", "Blank_Urban")], 
  est = list(data$Rural_HR, data$Urban_HR),  
  lower = list(data$Rural_LowerCI, data$Urban_LowerCI),  
  upper = list(data$Rural_UpperCI, data$Urban_UpperCI),  
  ci_column = c(3, 5),  
  ref_line = 1,  
  xlim = c(0.25, 5),  
  ticks_at = c(0.25, 0.5, 1, 1.5, 2, 5), 
  x_trans = "log",  
  theme = tm 
)

# 1500 width
plot(p_combined)

# Figure 6B

data <- read.csv('/path/to/csv')

data$Rural_CI <- sprintf("%.2f (%.2f, %.2f)", data$Rural_HR, data$Rural_LowerCI, data$Rural_UpperCI)
data$Urban_CI <- sprintf("%.2f (%.2f, %.2f)", data$Urban_HR, data$Urban_LowerCI, data$Urban_UpperCI)

data$Blank_Rural <- paste(rep(" ", 20), collapse = " ")
data$Blank_Urban <- paste(rep(" ", 20), collapse = " ")

tm <- forest_theme(
  base_size = 9,
  ci_pch = c(16, 16),  
  ci_col = "black",
  ci_fill = "black",
  ci_alpha = 0.8,
  ci_lty = 1,
  ci_lwd = 1.5,
  ci_Theight = 0.2, 
  refline_lwd = gpar(lwd = 1, lty = "dashed", col = "grey20"),
  vertline_lwd = 1,
  vertline_lty = "dashed",
  vertline_col = "grey20",
  footnote_cex = 1,
  legend_name = "Group",  
  legend_value = c("Rural", "Urban"),
  core = list(padding = unit(c(4, 3), "mm"))  
)


p_combined <- forest(
  data[, c("Covariate", "Rural_CI", "Blank_Rural", "Urban_CI", "Blank_Urban")],  
  est = list(data$Rural_HR, data$Urban_HR), 
  lower = list(data$Rural_LowerCI, data$Urban_LowerCI),  
  upper = list(data$Rural_UpperCI, data$Urban_UpperCI), 
  ci_column = c(3, 5),  
  ref_line = 1, 
  xlim = c(0.25, 5), 
  ticks_at = c(0.25, 0.5, 1, 1.5, 2, 5),  
  x_trans = "log", 
  theme = tm  
)

# 1500 width
plot(p_combined)
