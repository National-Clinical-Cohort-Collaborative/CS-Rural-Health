required_packages <- c(
  "gtsummary", "dplyr", "survival", "ggsurvfit", "survminer", "ggplot2",
  "cowplot", "ggpubr", "purrr", "nnet", "tidyr", "forcats", "coxme",
  "broom", "scales", "usmap", "stringr"
)

# Install any missing packages
installed <- rownames(installed.packages())
to_install <- setdiff(required_packages, installed)
if (length(to_install) > 0) install.packages(to_install)

# Load all required packages
invisible(lapply(required_packages, library, character.only = TRUE))

# -------------------------------------------------------------------------
# Step 0: Filter to patients without a documented SARS-CoV-2 infection.
# -------------------------------------------------------------------------

# COVID-19 status
local_df <- synthetic_data %>%
  mutate(
    covid_status = factor(covid_status, levels = c("Non-COVID", "COVID")),
    covid_status = relevel(covid_status, ref = "Non-COVID")
  )

local_df <- synthetic_data %>%
  filter(covid_status == "Non-COVID")

# -------------------------------------------------------------------------
# Step 1: Set factor levels and reference categories for key variables
# -------------------------------------------------------------------------

# Sex
local_df$sex <- factor(local_df$sex, levels = c("Female", "Male"))
local_df$sex <- relevel(local_df$sex, ref = "Female")

# Age group
local_df$age_group <- factor(local_df$age_group, 
                             levels = c("<30", "30-49", "50-64", "65-74", "75+"))
local_df$age_group <- relevel(local_df$age_group, ref = "30-49")

# Race/Ethnicity
local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c(
  "White Non-Hispanic",
  "Black or African American Non-Hispanic",
  "Hispanic or Latino Any Race",
  "Other",
  "Unknown"
))
local_df$race_ethnicity <- relevel(local_df$race_ethnicity, 
                                   ref = "White Non-Hispanic")

# Subregion
local_df$subregion <- factor(local_df$subregion, levels = c(
  "East North Central", "East South Central", "Middle Atlantic",
  "Mountain", "New England", "Pacific", "South Atlantic",
  "West North Central", "West South Central"
))
local_df$subregion <- relevel(local_df$subregion, ref = "East North Central")

# Rurality (three-level)
local_df$rurality <- factor(local_df$rurality, levels = c(
  "Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"
))
local_df$rurality <- relevel(local_df$rurality, ref = "Urban")

# Rurality (binary coded as 0/1)
local_df$rural_binary <- factor(local_df$rural_binary, levels = c("0", "1"))
levels(local_df$rural_binary)[levels(local_df$rural_binary) == "0"] <- "Urban"
levels(local_df$rural_binary)[levels(local_df$rural_binary) == "1"] <- "Rural"
local_df$rural_binary <- relevel(local_df$rural_binary, ref = "Urban")


# -------------------------------------------------------------------------
# Step 2: Characteristics of COVID-negative patients by rurality
# (Supplemental Table 12)
# -------------------------------------------------------------------------

table1 <- local_df %>%
  select(
    sex, age_group, race_ethnicity,  SVI,
    OBESITY, HYPERTENSION, MI, CHF, PVD, CVD, RD, PULMONARY, PUD, HEMI_PARA,
    diabetes, DEMENTIA, liver, RENAL, cancer, HIV, TOBACCO, SUBSTANCE,
    subregion, rurality
  ) %>%
  tbl_summary(by = rurality, missing = "no") %>%
  add_overall() %>%
  add_p()

# Display the table
print(table1)

# Convert binary variable in models to categorical for modeling output format 

# liver
local_df$liver <- factor(local_df$liver, levels=c("0","1"))
levels(local_df$liver)[levels(local_df$liver)=="0"] <- "No Hx of Liver Disease"
levels(local_df$liver)[levels(local_df$liver)=="1"] <- "Hx of Liver Disease"
local_df$liver=relevel(as.factor(local_df$liver),ref="No Hx of Liver Disease")

# cancer
local_df$cancer <- factor(local_df$cancer, levels=c("0","1"))
levels(local_df$cancer)[levels(local_df$cancer)=="0"] <- "No Hx of Cancer"
levels(local_df$cancer)[levels(local_df$cancer)=="1"] <- "Hx of Cancer"
local_df$cancer=relevel(as.factor(local_df$cancer),ref="No Hx of Cancer")

# diabetes
local_df$diabetes <- factor(local_df$diabetes, levels=c("0","1"))
levels(local_df$diabetes)[levels(local_df$diabetes)=="0"] <- "No Hx of Diabetes"
levels(local_df$diabetes)[levels(local_df$diabetes)=="1"] <- "Hx of Diabetes"
local_df$diabetes=relevel(as.factor(local_df$diabetes),ref="No Hx of Diabetes")

# OBESITY
local_df$OBESITY <- factor(local_df$OBESITY, levels=c("0","1"))
levels(local_df$OBESITY)[levels(local_df$OBESITY)=="0"] <- "No Hx of Obesity"
levels(local_df$OBESITY)[levels(local_df$OBESITY)=="1"] <- "Hx of Obesity"
local_df$OBESITY=relevel(as.factor(local_df$OBESITY),ref="No Hx of Obesity")

# RD
local_df$RD <- factor(local_df$RD, levels=c("0","1"))
levels(local_df$RD)[levels(local_df$RD)=="0"] <- "No Hx of Rheumatic Disease"
levels(local_df$RD)[levels(local_df$RD)=="1"] <- "Hx of Rheumatic Disease"
local_df$RD=relevel(as.factor(local_df$RD),ref="No Hx of Rheumatic Disease")

# PUD
local_df$PUD <- factor(local_df$PUD, levels=c("0","1"))
levels(local_df$PUD)[levels(local_df$PUD)=="0"] <- "No Hx of PUD"
levels(local_df$PUD)[levels(local_df$PUD)=="1"] <- "Hx of PUD"
local_df$PUD=relevel(as.factor(local_df$PUD),ref="No Hx of PUD")

# MI
local_df$MI <- factor(local_df$MI, levels=c("0","1"))
levels(local_df$MI)[levels(local_df$MI)=="0"] <- "No Hx of MI"
levels(local_df$MI)[levels(local_df$MI)=="1"] <- "Hx of MI"
local_df$MI=relevel(as.factor(local_df$MI),ref="No Hx of MI")

# CHF
local_df$CHF <- factor(local_df$CHF, levels=c("0","1"))
levels(local_df$CHF)[levels(local_df$CHF)=="0"] <- "No Hx of CHF"
levels(local_df$CHF)[levels(local_df$CHF)=="1"] <- "Hx of CHF"
local_df$CHF=relevel(as.factor(local_df$CHF),ref="No Hx of CHF")

# CVD
local_df$CVD <- factor(local_df$CVD, levels=c("0","1"))
levels(local_df$CVD)[levels(local_df$CVD)=="0"] <- "No Hx of CVD"
levels(local_df$CVD)[levels(local_df$CVD)=="1"] <- "Hx of CVD"
local_df$CVD=relevel(as.factor(local_df$CVD),ref="No Hx of CVD")

# HEMI_PARA
local_df$HEMI_PARA <- factor(local_df$HEMI_PARA, levels=c("0","1"))
levels(local_df$HEMI_PARA)[levels(local_df$HEMI_PARA)=="0"] <- "No Hx of HEMI_PARA"
levels(local_df$HEMI_PARA)[levels(local_df$HEMI_PARA)=="1"] <- "Hx of HEMI_PARA"
local_df$HEMI_PARA=relevel(as.factor(local_df$HEMI_PARA),ref="No Hx of HEMI_PARA")

# PVD
local_df$PVD <- factor(local_df$PVD, levels=c("0","1"))
levels(local_df$PVD)[levels(local_df$PVD)=="0"] <- "No Hx of PVD"
levels(local_df$PVD)[levels(local_df$PVD)=="1"] <- "Hx of PVD"
local_df$PVD=relevel(as.factor(local_df$PVD),ref="No Hx of PVD")

# RENAL
local_df$RENAL <- factor(local_df$RENAL, levels=c("0","1"))
levels(local_df$RENAL)[levels(local_df$RENAL)=="0"] <- "No Hx of RENAL"
levels(local_df$RENAL)[levels(local_df$RENAL)=="1"] <- "Hx of RENAL"
local_df$RENAL=relevel(as.factor(local_df$RENAL),ref="No Hx of RENAL")

# HYPERTENSION
local_df$HYPERTENSION <- factor(local_df$HYPERTENSION, levels=c("0","1"))
levels(local_df$HYPERTENSION)[levels(local_df$HYPERTENSION)=="0"] <- "No Hx of HYPERTENSION"
levels(local_df$HYPERTENSION)[levels(local_df$HYPERTENSION)=="1"] <- "Hx of HYPERTENSION"
local_df$HYPERTENSION=relevel(as.factor(local_df$HYPERTENSION),ref="No Hx of HYPERTENSION")

# TOBACCO
local_df$TOBACCO <- factor(local_df$TOBACCO, levels=c("0","1"))
levels(local_df$TOBACCO)[levels(local_df$TOBACCO)=="0"] <- "No Hx of TOBACCO"
levels(local_df$TOBACCO)[levels(local_df$TOBACCO)=="1"] <- "Hx of TOBACCO"
local_df$TOBACCO=relevel(as.factor(local_df$TOBACCO),ref="No Hx of TOBACCO")

# SUBSTANCE
local_df$SUBSTANCE <- factor(local_df$SUBSTANCE, levels=c("0","1"))
levels(local_df$SUBSTANCE)[levels(local_df$SUBSTANCE)=="0"] <- "No Hx of SUBSTANCE"
levels(local_df$SUBSTANCE)[levels(local_df$SUBSTANCE)=="1"] <- "Hx of SUBSTANCE"
local_df$SUBSTANCE=relevel(as.factor(local_df$SUBSTANCE),ref="No Hx of SUBSTANCE")

# HIV
local_df$HIV <- factor(local_df$HIV, levels=c("0","1"))
levels(local_df$HIV)[levels(local_df$HIV)=="0"] <- "No Hx of HIV"
levels(local_df$HIV)[levels(local_df$HIV)=="1"] <- "Hx of HIV"
local_df$HIV=relevel(as.factor(local_df$HIV),ref="No Hx of HIV")

# DEMENTIA
local_df$DEMENTIA <- factor(local_df$DEMENTIA, levels=c("0","1"))
levels(local_df$DEMENTIA)[levels(local_df$DEMENTIA)=="0"] <- "No Hx of Dementia"
levels(local_df$DEMENTIA)[levels(local_df$DEMENTIA)=="1"] <- "Hx of Dementia"
local_df$DEMENTIA=relevel(as.factor(local_df$DEMENTIA),ref="No Hx of Dementia")

# PULMONARY
local_df$PULMONARY <- factor(local_df$PULMONARY, levels=c("0","1"))
levels(local_df$PULMONARY)[levels(local_df$PULMONARY)=="0"] <- "No Hx of PULMONARY"
levels(local_df$PULMONARY)[levels(local_df$PULMONARY)=="1"] <- "Hx of PULMONARY"
local_df$PULMONARY=relevel(as.factor(local_df$PULMONARY),ref="No Hx of PULMONARY")

# -------------------------------------------------------------------------
# Step 3: Generate Univariable and Multivariable Cox Models 
# (Supplemental Table 14)
# -------------------------------------------------------------------------

# Define covariates to include in all models
covariates <- c("sex", "age_group", "race_ethnicity", 
                "OBESITY", "HYPERTENSION", "MI", "CHF", "PVD", "CVD", "RD", "PULMONARY", "PUD", 
                "HEMI_PARA", "diabetes", "DEMENTIA", "liver", "RENAL", "cancer", "HIV", 
                "TOBACCO", "SUBSTANCE", "subregion", "rurality", "SVI")

# Helper to run and format models
run_models <- function(data, label) {
  m1 <- coxph(as.formula(paste("Surv(time_to_death, death) ~", paste(covariates, collapse = " + "))), data = data) %>%
    tbl_regression(exponentiate = TRUE) %>%
    add_nevent(location = "level") %>%
    add_n(location = "level") %>%
    modify_table_body(
      ~ .x %>%
        mutate(
          stat_nevent_rate = ifelse(!is.na(stat_nevent),
                                    paste0(style_sigfig(stat_nevent / stat_n, scale = 100), "%"), NA),
          .after = stat_nevent
        )
    ) %>%
    modify_column_merge(
      pattern = "{stat_nevent} / {stat_n} ({stat_nevent_rate})",
      rows = !is.na(stat_nevent)
    ) %>%
    modify_header(stat_nevent = "**Event Rate**") %>%
    modify_caption(paste0("**Multivariable Model: ", label, "**"))
  
  m3 <- tbl_uvregression(
    data[c("time_to_death", "death", covariates)],
    method = coxph,
    y = Surv(time_to_death, death),
    exponentiate = TRUE
  ) %>%
    modify_caption(paste0("**Univariable Model: ", label, "**"))
  
  tbl_merge(tbls = list(m3, m1), tab_spanner = c("**Univariable**", "**Multivariable**"))
}

# Define datasets for each model
model_list <- list(
  "T30_model" = local_df %>% 
    mutate(death = ifelse(time_to_death > 30, 0, death),
           time_to_death = pmin(time_to_death, 30)),
  
  "T91_model" = local_df %>% 
    mutate(death = ifelse(time_to_death > 91, 0, death),
           time_to_death = pmin(time_to_death, 91)),
  
  "T365_model" = local_df %>% 
    mutate(death = ifelse(time_to_death > 365, 0, death),
           time_to_death = pmin(time_to_death, 365)),
  
  "T730_model" = local_df %>% 
    mutate(death = ifelse(time_to_death > 730, 0, death),
           time_to_death = pmin(time_to_death, 730)),
  
  "Post30_model" = local_df %>%
    filter(time_to_death > 30),
  
  "Post91_model" = local_df %>%
    filter(time_to_death > 91),
  
  "Post365_model" = local_df %>%
    filter(time_to_death > 365)
)

# Run models and store results
cox_tables <- map2(model_list, names(model_list), run_models)


# Stack all model tables together
final_combined_table <- tbl_stack(
  tbls = cox_tables,
  group_header = c(
    "30-Day Mortality (All Patients)",
    "91-Day Mortality (All Patients)",
    "365-Day Mortality (All Patients)",
    "730-Day Mortality (All Patients)",
    "Mortality After Day 30 (Survivors)",
    "Mortality After Day 91 (Survivors)",
    "Mortality After Day 365 (Survivors)"
  )
)

# Print table 
print(final_combined_table)

# -------------------------------------------------------------------------
# Step 4: Estimate Stabilized IPT Weights for Multinomial Rurality Variable
# -------------------------------------------------------------------------

# Fit multinomial logistic regression model to estimate propensity scores
ps_model <- multinom(rurality ~ sex + age_group + race_ethnicity, 
                     data = local_df, trace = FALSE)

# Predict class probabilities (each row corresponds to predicted P[rurality = class | covariates])
ps_matrix <- predict(ps_model, type = "probs")

# Store as new columns for inspection (optional)
local_df <- bind_cols(local_df, as.data.frame(ps_matrix))

# Estimate marginal (unconditional) probabilities of each rurality class for stabilized weighting
p_marginal <- prop.table(table(local_df$rurality))

# Function to compute stabilized weights for each individual
calculate_stabilized_weight <- function(row, rurality_class, p_marginal) {
  num <- as.numeric(p_marginal[[rurality_class]])
  denom <- row[[rurality_class]]
  return(num / denom)
}

# Apply stabilized weight calculation across rows
local_df$weight <- mapply(
  FUN = calculate_stabilized_weight,
  row = split(local_df[, names(p_marginal)], seq(nrow(local_df))),
  rurality_class = local_df$rurality,
  MoreArgs = list(p_marginal = p_marginal)
)

# Ensure column names of ps_matrix are aligned
# ps_colnames <- gsub("ps_", "", colnames(ps_df))  # e.g., "Urban", "Urban_Adjacent_Rural"
# colnames(ps_matrix) <- ps_colnames

# Assign individual-level PS using matrix indexing
local_df$individual_ps <- ps_matrix[cbind(seq_len(nrow(local_df)), local_df$rurality)]

# ----------------------------
# Step 4b: Summary Statistics of Stabilized Weights
# ----------------------------

# Summary statistics
cat("Summary of Stabilized Weights:\n")
print(summary(local_df$weight))

cat("\nAdditional Quantile Summary:\n")
cat("Min: ", min(local_df$weight, na.rm = TRUE), "\n")
cat("1st Quartile: ", quantile(local_df$weight, 0.25, na.rm = TRUE), "\n")
cat("Median: ", median(local_df$weight, na.rm = TRUE), "\n")
cat("Mean: ", mean(local_df$weight, na.rm = TRUE), "\n")
cat("3rd Quartile: ", quantile(local_df$weight, 0.75, na.rm = TRUE), "\n")
cat("Max: ", max(local_df$weight, na.rm = TRUE), "\n")
cat("Standard Deviation: ", sd(local_df$weight, na.rm = TRUE), "\n")

cat("\nExtreme Weights:\n")
cat("Number of Weights > 10: ", sum(local_df$weight > 10, na.rm = TRUE), "\n")
cat("Number of Weights > 50: ", sum(local_df$weight > 50, na.rm = TRUE), "\n")
cat("Number of Weights > 100: ", sum(local_df$weight > 100, na.rm = TRUE), "\n")

# Histogram of stabilized weights
ggplot(local_df, aes(x = weight)) +
  geom_histogram(bins = 100, fill = "steelblue", color = "white") +
  labs(
    title = "Histogram of Stabilized Weights",
    x = "Stabilized Weight",
    y = "Count"
  ) +
  theme_minimal()

# Boxplot of weights by rurality (exclude extreme outliers)
ggplot(local_df, aes(x = rurality, y = weight, fill = rurality)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  coord_cartesian(ylim = quantile(local_df$weight, c(0.01, 0.99), na.rm = TRUE)) +
  labs(
    title = "Stabilized Weights by Rurality",
    x = "Rurality Group",
    y = "Stabilized Weight"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# ------------------------------------------
# Step 4c: Propensity Score Diagnostics (Predictions)
# ------------------------------------------

# Predict class probabilities (each row corresponds to predicted P[rurality = class | covariates])
ps_matrix <- predict(ps_model, type = "probs")

# Store as new columns for inspection
ps_df <- as.data.frame(ps_matrix)
colnames(ps_df) <- paste0("ps_", colnames(ps_df))
local_df <- bind_cols(local_df, ps_df)

ps_colnames <- gsub("ps_", "", colnames(ps_df))  # e.g., "Urban", "Urban_Adjacent_Rural"
colnames(ps_matrix) <- ps_colnames

# Long format for faceted plots
ps_long <- local_df %>%
  select(person_id, rurality, starts_with("ps_")) %>%
  pivot_longer(
    cols = starts_with("ps_"),
    names_to = "predicted_group",
    values_to = "predicted_probability"
  )

# Clean group labels
ps_long$predicted_group <- gsub("ps_", "", ps_long$predicted_group)

# Plot: Boxplot of individual-level propensity scores (assigned class only)
ggplot(local_df, aes(x = rurality, y = individual_ps, fill = rurality)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 0.7) +
  labs(
    title = "Propensity Scores for Assigned Rurality Group",
    x = "Rurality Group",
    y = "Propensity Score"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Plot: Density of predicted probabilities by group and rurality
ggplot(ps_long, aes(x = predicted_probability, fill = rurality)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~predicted_group, scales = "free_y") +
  labs(
    title = "Predicted Probabilities by Group (Faceted)",
    x = "Predicted Probability",
    y = "Density"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())


# ------------------------------------------
# Step 4d: Generate Love Plot 
# (Supplemental Figure 6)
# ------------------------------------------

# Covariates
covariates <- c("sex", "age_group", "race_ethnicity")

# Max pairwise SMD function
compute_max_smd_multinomial <- function(data, variable, exposure, weight_var) {
  X <- model.matrix(as.formula(paste0("~", variable)), data = data)[, -1, drop = FALSE]
  levels <- colnames(X)
  groups <- unique(data[[exposure]])
  group_pairs <- combn(groups, 2, simplify = FALSE)
  
  smd_df <- lapply(levels, function(lvl) {
    x <- X[, lvl]
    smd_un_list <- c()
    smd_wt_list <- c()
    
    for (pair in group_pairs) {
      g1 <- data[[exposure]] == pair[1]
      g2 <- data[[exposure]] == pair[2]
      
      # Unweighted
      m1 <- mean(x[g1], na.rm = TRUE)
      m2 <- mean(x[g2], na.rm = TRUE)
      sd_pooled <- sqrt((var(x[g1], na.rm = TRUE) + var(x[g2], na.rm = TRUE)) / 2)
      smd_un <- (m1 - m2) / sd_pooled
      smd_un_list <- c(smd_un_list, smd_un)
      
      # Weighted
      w1 <- data[[weight_var]][g1]
      w2 <- data[[weight_var]][g2]
      m1_w <- weighted.mean(x[g1], w1, na.rm = TRUE)
      m2_w <- weighted.mean(x[g2], w2, na.rm = TRUE)
      var1_w <- sum(w1 * (x[g1] - m1_w)^2) / sum(w1)
      var2_w <- sum(w2 * (x[g2] - m2_w)^2) / sum(w2)
      sd_pooled_w <- sqrt((var1_w + var2_w) / 2)
      smd_wt <- (m1_w - m2_w) / sd_pooled_w
      smd_wt_list <- c(smd_wt_list, smd_wt)
    }
    
    tibble(Variable = paste0(variable, "_", lvl),
           Unweighted = max(abs(smd_un_list), na.rm = TRUE),
           Weighted = max(abs(smd_wt_list), na.rm = TRUE))
  }) %>% bind_rows()
  
  return(smd_df)
}

# Apply function to covariates
smd_table <- bind_rows(lapply(covariates, compute_max_smd_multinomial,
                              data = local_df,
                              exposure = "rurality",
                              weight_var = "weight"))

# Add "Overall" row
overall_smd <- smd_table %>%
  summarize(
    Variable = "Overall",
    Unweighted = mean(abs(Unweighted), na.rm = TRUE),
    Weighted = mean(abs(Weighted), na.rm = TRUE)
  )

final_smd_table <- bind_rows(smd_table, overall_smd)

#  Prepare data for plotting
plot_data <- final_smd_table %>%
  pivot_longer(cols = c(Unweighted, Weighted), names_to = "Type", values_to = "SMD") %>%
  mutate(Label = Variable) %>%
  mutate(Label = fct_rev(fct_inorder(Label)))  # Maintain top-to-bottom order

# Love Plot
ggplot(plot_data, aes(x = abs(SMD), y = Label, shape = Type, color = Type)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0.1, linetype = "dashed") +
  scale_color_manual(values = c("Unweighted" = "#E64B35", "Weighted" = "#4DBBD5")) +
  scale_shape_manual(values = c("Unweighted" = 17, "Weighted" = 16)) +
  labs(
    x = "Absolute Standardized Mean Difference (SMD)",
    y = "Covariates",
    color = "Sample",
    shape = "Sample"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 12),
    legend.position = c(0.85, 0.5),
    panel.grid.major.y = element_blank()
  )

# -------------------------------------------------------------------------
# Step 5: Run Weighted Multilevel Cox Models at Multiple Time Horizons
# (Supplemental Table 13)
# -------------------------------------------------------------------------

# Custom function to tidy coxme output
tidy.coxme <- function(x, exponentiate = FALSE, conf.int = 0.95, ...){
  beta <- x$coefficients
  nvar <- length(beta)
  nfrail <- nrow(x$var) - nvar
  nn <- c("estimate", "exp()", "std.error", "statistic", "p.value")
  se <- sqrt(diag(as.matrix(x$var))[nfrail + 1:nvar])
  z <- qnorm((1 + conf.int)/2, 0, 1)
  ret <- data.frame(
    "term"      = names(beta),
    "estimate"  = beta,
    "std.error" = se,
    "statistic" = beta/se,
    "p.value"   = 1 - pchisq((beta/se)^2, 1),
    "conf.low"  =  beta - z * se,
    "conf.high" =  beta + z * se
  )
  if (exponentiate) {
    ret$estimate <- exp(ret$estimate)
    ret$conf.low <- exp(ret$conf.low)
    ret$conf.high <- exp(ret$conf.high)
  }
  rownames(ret) <- c(1:nrow(ret))
  ret
}

#' Glance coxme
#'
#' Glance method for coxme objects
#'
#' @param x coxme object
#' @param ... other params
#' @importFrom tibble as_tibble
#' @export
#' @examples
#' library(broom)
#' library(coxme)
#' fit <- coxme(Surv(y, uncens) ~ trt + (1|center), eortc)
#' glance(fit)
#' 

glance.coxme <- function(x, ...){
  loglik <- x$loglik + c(0, 0, x$penalty)
  chi1 <- 2 * diff(loglik[1:2]) 
  chi2 <- 2 * diff(loglik[c(1,3)])
  temp0 <- as.list(c(
    x$n[2], x$n[1],
    chi1, x$df[1],
    as.numeric(loglik[3]),
    1 - pchisq(chi1, x$df[1]),
    AIC(x),
    BIC(x)))
  names(temp0) <- c("n", "events" , "Chisq", "df", "logLik", "p", "AIC", "BIC")
  
  ## random effects
  random <- VarCorr(x)
  nrow <-  sapply(random, 
                  function(x) if (is.matrix(x)) nrow(x) else length(x))
  maxcol <-max(sapply(random,
                      function(x) if (is.matrix(x)) 1 + ncol(x) else 2))
  temp1 <- matrix(NA, nrow=sum(nrow), ncol=maxcol)
  indx <- 0
  for (term in  random) {
    if (is.matrix(term)) {
      k <- nrow(term)
      nc <- ncol(term)
      for (j in 1:k) {
        temp1[j + indx, 1] <- sqrt(term[j,j])
        temp1[j + indx, 2] <- term[j,j]
        if (nc > j) {
          indx2 <- (j + 1):nc
          temp1[j + indx, 1 + indx2] <- term[j, indx2]
        }
      }
    }
    else {
      k <- length(term)
      temp1[1:k + indx, 1] <- sqrt(term)
      temp1[1:k + indx, 2] <- term
    }
    indx <- indx + k
  }
  
  indx <- cumsum(c(1, nrow))   # starting row of each effect
  temp3 <- rep("", nrow(temp1))
  temp3[indx[-length(indx)]] <- names(random)
  xname <- unlist(lapply(random, 
                         function(x) if (is.matrix(x)) dimnames(x)[[1]] else names(x)))
  
  b <- as.vector(temp1)
  names(b) <- paste(rep(c("random_sd", "random_variance"), each = length(b)/2), temp3, sep = "_")
  
  # groups
  grps <- unlist(lapply(x$frail, length))
  names(grps) <- paste0("random_n_", names(grps))
  
  ret <- as.list(c(temp0, grps, b))
  tibble::as_tibble(ret)
  
}

#' Augment coxme
#' 
#' Augment coxme object
#' 
#' @param x coxme object
#' @param data original data for augment
#' @param newdata new data on which to do predictions
#' @param type.predict type of prediction value linear predictor or risk
#' @param ... Extra arguments, not used
#' @export
#' @examples
#' library(broom)
#' library(coxme)
#' fit <- coxme(Surv(y, uncens) ~ trt + (1|center), eortc)
#' augment(fit, type.predict = "risk")
#' 

augment.coxme <- function(x, data = survival:::model.frame.coxph(x), newdata = NULL,
                          type.predict = "lp", 
                          ...) {
  pred <- predict_coxme(x, newdata = newdata, type = type.predict, se.fit = TRUE)
  data$.fitted <- pred$fit
  data$.se.fit <- pred$se.fit
  data
}

#' Predict relative risk
#' 
#' predict centered relative risk from coxph and coxme objects
#' 
#' @param model coxph or coxme object
#' @param ... others
#' @export


rr_pred <- function(model, ...) UseMethod("rr_pred", model)


#' Predict relative risk
#' 
#' predict centered relative risk from coxph object
#' 
#' @param model coxph object
#' @param newdata data.frame 
#' @param center center variable
#' @param conf.level The confidence level to use for the confidence interval.
#' @export
#' @importFrom stringr str_detect
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @examples 
#' 
#' library(survival)
#' fit <- coxph(Surv(time, status) ~ age + ph.ecog + strata(inst), lung)
#' newdata <- expand.grid(age = 50, ph.ecog = 0:3, inst = 11)
#' 
#' rr_pred(fit, newdata, center = c(ph.ecog = 0))
#' 

rr_pred.coxph <- function(model, newdata, center, conf.level = .95){
  
  
  if(conf.level >= 1 | conf.level <= 0)
    stop("conf.level must be between 0 and 1")
  
  terms <- Terms <- delete.response(terms(model))
  
  mm <- stats::model.frame(terms,newdata, na.action = na.pass, xlev = model$xlevels)
  mm2 <- model.matrix(model, mm)
  mm3 <- mm2
  # center
  mm3[,str_detect(colnames(mm2), names(center)) ] <- center
  mm_c <- mm2 - mm3
  
  # extract coefficients
  coef <- coef(model)
  # calculate predicted
  lp <- mm_c %*% coef 
  # approximate standard error
  se <- sqrt(rowSums((mm_c %*% vcov(model)) * mm_c))
  
  alpha <- 1-conf.level
  crit <- -qnorm(alpha/2)
  
  nd <- as_tibble(newdata) 
  mutate(nd,
         lp = as.vector(lp),
         se = se,
         rr = exp(lp),
         rr_l = exp(lp-(crit*se)),
         rr_h = exp(lp+(crit*se))
  )
}



#' Predict relative risk
#' 
#' predict centered relative risk from  coxme object
#' 
#' @param model coxme object
#' @param newdata data.frame 
#' @param center center variable
#' @param conf.level The confidence level to use for the confidence interval.
#' @export
#' @importFrom stringr str_detect
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @examples 
#' 
#' library(coxme)
#' fit <- coxme(Surv(time, status) ~ age + ph.ecog + (1 | inst), data = lung)
#' newdata <- expand.grid(age = 50, ph.ecog = 0:3, inst = 11)
#' rr_pred(fit, newdata, center = c(ph.ecog = 0))

rr_pred.coxme <- function(model, 
                          newdata = NULL, 
                          center, 
                          conf.level = .95){
  
  if(conf.level >= 1 | conf.level <= 0)
    stop("conf.level must be between 0 and 1")
  
  
  n <- model$n[2]
  Terms <- delete.response(terms(model))
  has_strata <- !is.null(attr(Terms, "specials")$strata) 
  
  if (has_strata) 
    has_strata <- ifelse(length(attr(Terms, "specials")$strata) == 0, FALSE, has_strata)
  has_newdata  <- !is.null(newdata)
  
  
  # if strata update terms
  if (has_strata){
    strata_terms <- untangle.specials(Terms, "strata")
    Terms2 <- Terms[-strata_terms$terms]
  } else {
    Terms2 <- Terms
  }
  
  # Extract model.matrix
  mf <- survival:::model.frame.coxph(model)
  m <- model.frame(Terms, newdata)
  mm <- model.matrix(Terms2, m)
  mm <- mm[ ,-1]
  
  # Center mm
  mm3 <- mm
  mm3[ ,str_detect(colnames(mm3), names(center))] <- center
  mm_c <- mm - mm3
  
  # Extract coefficients
  coef <- fixed.effects(model)
  
  # centered predicted
  if (length(coef) == 1){
    lp <- mm_c * coef
  } else {
    lp <- (mm_c %*% coef)
  }
  
  # Approximate standard error
  se <- sqrt(rowSums((mm_c %*% vcov(model)) * mm_c))
  
  alpha <- 1-conf.level
  crit <- -qnorm(alpha/2)
  
  nd <- as_tibble(newdata) 
  mutate(
    nd,
    lp = as.vector(lp),
    se = se,
    rr = exp(lp),
    rr_l = exp(lp-(crit*se)),
    rr_h = exp(lp+(crit*se))
  )
}

#' Predict coxme 
#' 
#' Prediction method for coxme objects
#' 
#' @param object coxme object.
#' @param newdata data.frame new data set.
#' @param type type of prediction, linear predictor ("lp") or relative risk ("risk").
#' @param se.fit if TRUE, pointwise standard errors are produced for the predictions.
#' @param strata_ref logical, use strata as reference.
#' @export
#' @import coxme
#' @examples 
#' library(coxme)
#' data(eortc)
#' fit <- coxme(Surv(y, uncens) ~ trt + (1|center), eortc)
#' predict_coxme(fit)
#' 

predict_coxme <- function(object, 
                          newdata = NULL, 
                          type = c("lp", "risk"), 
                          se.fit = FALSE,
                          strata_ref = TRUE){
  
  if (!inherits(object, 'coxme'))
    stop("Primary argument much be a coxme object")
  
  type <- match.arg(type)
  n <- object$n[2]
  Terms <- delete.response(terms(object))
  has_strata <- !is.null(attr(Terms, "specials")$strata) 
  if (has_strata) 
    has_strata <- ifelse(length(attr(Terms, "specials")$strata) == 0, FALSE, has_strata)
  has_newdata  <- !is.null(newdata)
  
  if (!se.fit & type == "lp" & !has_newdata) return(object$linear.predictor)
  
  coef <- fixed.effects(object)
  mf <- survival:::model.frame.coxph(object)
  
  # boot.ci
  
  
  
  if (has_newdata){
    m <- model.frame(Terms, newdata)
  } else {
    m <- mf
  }
  
  # if strata update terms
  if (has_strata){
    strata_terms <- untangle.specials(Terms, "strata")
    Terms2 <- Terms[-strata_terms$terms]
  } else {
    Terms2 <- Terms
  }
  
  if (has_newdata){
    mm <- model.matrix(Terms2, m)
    mm <- mm[ ,-1]
  }
  
  # has strata and reference is strata
  # calculate strata means
  if (has_strata & strata_ref){
    # Full model matrix
    x <- model.matrix(Terms, data = mf)
    
    oldstrat <- mf[[strata_terms$vars]]
    xmeans <- rowsum(x, oldstrat)/as.vector(table(oldstrat))
  }
  
  if (!has_newdata){
    # extract all cols in x which matches Terms
    mm <- model.matrix(Terms2, data =mf)[ ,-1]
    m <- mf
  }
  
  if (has_strata & strata_ref){
    newstrat <- m[[strata_terms$vars]]
    mm <- mm - xmeans[match(newstrat, row.names(xmeans)), colnames(mm)]
  } else {
    mm <- mm - rep(object$means, each = nrow(m))
  }
  
  # if (!has_newdata & !has_strata){
  #   pred <- object$linear.predictor
  # }
  if (length(coef) == 1){
    pred <- mm * coef
  } else {
    pred <- (mm %*% coef)
  }
  
  
  if (se.fit) se <- sqrt(rowSums((mm %*% vcov(object)) * mm))
  if (type == "risk"){
    pred <- exp(pred)
    if (se.fit) se <- se * sqrt(pred)
  }
  if (se.fit) list(fit = pred, se.fit = se)
  else pred
}

# Define time horizons and labels
time_points <- list(
  list(label = "Death_30d", censor_day = 30, subset = NULL),
  list(label = "Death_91d", censor_day = 91, subset = NULL),
  list(label = "Death_365d", censor_day = 365, subset = NULL),
  list(label = "Death_730d", censor_day = 730, subset = NULL),
  list(label = "Death_31to730d", censor_day = 730, subset = quote(time_to_death > 30)),
  list(label = "Death_92to730d", censor_day = 730, subset = quote(time_to_death > 91)),
  list(label = "Death_366to730d", censor_day = 730, subset = quote(time_to_death > 365))
)

# Define model formula
cox_formula <- as.formula(Surv(time_to_death, death) ~ 
                            OBESITY + HYPERTENSION + MI + CHF + PVD + CVD + RD + PULMONARY + PUD + HEMI_PARA +
                            diabetes + DEMENTIA + liver + RENAL + cancer + HIV + TOBACCO + SUBSTANCE +
                            subregion + rurality + SVI + (1 | data_partner_id))

# Initialize list to store model results
model_results_list <- list()

for (tp in time_points) {
  cat("Running model for:", tp$label, "\n")
  
  # Clone original dataset
  local_df_copy <- local_df
  
  # Subset if needed
  if (!is.null(tp$subset)) {
    local_df_copy <- local_df_copy %>% filter(!!tp$subset)
  }
  
  # Censor deaths after specified day
  local_df_copy <- local_df_copy %>%
    mutate(
      death = ifelse(time_to_death > tp$censor_day, 0, death),
      time_to_death = ifelse(time_to_death > tp$censor_day, tp$censor_day, time_to_death),
      data_partner_id = as.factor(data_partner_id)
    )
  
  # Fit Cox model with stabilized weights
  model_fit <- coxme(cox_formula, weights = weight, data = local_df_copy)
  
  # Extract results and label
  model_tidy <- broom::tidy(model_fit, exponentiate = TRUE)
  model_tidy$Model_Outcome <- tp$label
  
  # Append to results
  model_results_list[[tp$label]] <- model_tidy
}

# Combine all models
combined_models_df <- bind_rows(model_results_list)


