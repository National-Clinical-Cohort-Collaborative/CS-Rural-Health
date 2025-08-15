set.seed(20250721)

required_packages <- c(
  "dplyr", "tibble", "stringr", "purrr", "readr", "here"
)

# Install any missing packages
installed <- rownames(installed.packages())
to_install <- setdiff(required_packages, installed)
if (length(to_install) > 0) install.packages(to_install)

# Load all required packages
invisible(lapply(required_packages, library, character.only = TRUE))

# -------------------------------
# Define constants and functions
# -------------------------------

n_total <- 100000
n_covid <- n_total / 2
n_non_covid <- n_total / 2

sample_rurality <- function(n) {
  sample(c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"),
         size = n, replace = TRUE, prob = c(0.79, 0.16, 0.05))
}

generate_covariate_by_rurality <- function(rurality, probs_by_group) {
  sapply(rurality, function(r) {
    probs <- probs_by_group[[r]]
    sample(names(probs), 1, prob = probs)
  })
}

get_death_probability <- function(covid, rural) {
  base <- ifelse(covid == "COVID", 0.04, 0.024)
  adj <- case_when(
    rural == "Urban" ~ 0.028,
    rural == "Urban-Adjacent Rural" ~ 0.044,
    rural == "Nonurban-Adjacent Rural" ~ 0.045
  )
  mean(c(base, adj))
}

# -------------------------------
# Build synthetic dataset
# -------------------------------

synthetic_data <- tibble(
  person_id = sprintf("SYN%05d", 1:n_total),
  covid_status = rep(c("COVID", "Non-COVID"), each = n_total / 2),
  rurality = sample_rurality(n_total)
)

# Sex
sex_probs <- list(
  "Urban" = c(Female = 0.59, Male = 0.41),
  "Urban-Adjacent Rural" = c(Female = 0.58, Male = 0.42),
  "Nonurban-Adjacent Rural" = c(Female = 0.57, Male = 0.43)
)
synthetic_data$sex <- generate_covariate_by_rurality(synthetic_data$rurality, sex_probs)

# Define age group sampling probabilities by rurality
age_probs <- list(
  "Urban" = c("<30" = 0.18, "30-49" = 0.34, "50-64" = 0.24, "65-74" = 0.13, "75+" = 0.11),
  "Urban-Adjacent Rural" = c("<30" = 0.15, "30-49" = 0.29, "50-64" = 0.24, "65-74" = 0.16, "75+" = 0.16),
  "Nonurban-Adjacent Rural" = c("<30" = 0.14, "30-49" = 0.27, "50-64" = 0.25, "65-74" = 0.18, "75+" = 0.18)
)

# Define numeric age bounds for each group
age_ranges <- list(
  "<30" = c(18, 29),
  "30-49" = c(30, 49),
  "50-64" = c(50, 64),
  "65-74" = c(65, 74),
  "75+" = c(75, 99)
)

# Function to assign age group and numeric age
assign_age_group_and_age <- function(rurality) {
  probs <- age_probs[[rurality]]
  chosen_group <- sample(names(probs), size = 1, prob = probs)
  bounds <- age_ranges[[chosen_group]]
  chosen_age <- sample(seq(bounds[1], bounds[2]), size = 1)
  tibble(age_group = chosen_group, age_at_covid = chosen_age)
}

# Apply function to each row and bind result
synthetic_data <- synthetic_data %>%
  bind_cols(
    pmap_dfr(list(rurality = synthetic_data$rurality), assign_age_group_and_age)
  )

# Race/ethnicity
race_probs <- list(
  "Urban" = c("White Non-Hispanic" = 0.66, "Black or African American Non-Hispanic" = 0.15,
              "Hispanic or Latino Any Race" = 0.10, "Other" = 0.04, "Unknown" = 0.05),
  "Urban-Adjacent Rural" = c("White Non-Hispanic" = 0.83, "Black or African American Non-Hispanic" = 0.07,
                             "Hispanic or Latino Any Race" = 0.06, "Other" = 0.02, "Unknown" = 0.02),
  "Nonurban-Adjacent Rural" = c("White Non-Hispanic" = 0.89, "Black or African American Non-Hispanic" = 0.05,
                                "Hispanic or Latino Any Race" = 0.03, "Other" = 0.02, "Unknown" = 0.01)
)
synthetic_data$race_ethnicity <- generate_covariate_by_rurality(synthetic_data$rurality, race_probs)

# Subregion (COVID+ Table 1 distribution)
subregion_levels <- c("East North Central", "East South Central", "Middle Atlantic", "Mountain",
                      "New England", "Pacific", "South Atlantic", "West North Central", "West South Central")
subregion_probs <- c(0.50, 0.061, 0.012, 0.080, 0.018, 0.042, 0.18, 0.063, 0.050)
synthetic_data$subregion <- sample(subregion_levels, size = n_total, replace = TRUE, prob = subregion_probs)

# Data partner ID
synthetic_data$data_partner_id <- sample(1:25, size = n_total, replace = TRUE)

# Vaccination and variant (COVID+ only)
variant_levels <- c("Ancestral COVID-19", "Alpha (B.1.1.7), Beta (B.1.351), Gamma (P.1)", "Delta (B.1.617.2)", 
                    "Omicron (B.1.1.529, BA.2, BA.2.12.1)", "Omicron (BA.5, BQ.1.1, XBB.1.5)")
synthetic_data$vaccination_status <- ifelse(
  synthetic_data$covid_status == "COVID",
  sample(c("Non-Breakthrough Infection", "VAX2 Breakthrough Infection", "VAX3 Breakthrough Infection"), 
         size = n_total, replace = TRUE, prob = c(0.74, 0.15, 0.11)),
  NA
)
synthetic_data$variant_period <- ifelse(
  synthetic_data$covid_status == "COVID",
  sample(variant_levels, size = n_total, replace = TRUE, prob = c(0.25, 0.12, 0.19, 0.30, 0.14)),
  NA
)

# Binary rurality
synthetic_data$rural_binary <- ifelse(synthetic_data$rurality == "Urban", 0, 1)

# SVI
svi_means <- c("Urban" = 0.44, "Urban-Adjacent Rural" = 0.46, "Nonurban-Adjacent Rural" = 0.42)
svi_sd <- 0.12
synthetic_data$SVI <- pmin(pmax(rnorm(n_total, mean = svi_means[synthetic_data$rurality], sd = svi_sd), 0), 1)

# Comorbidities (Table 1 COVID+)
comorbidity_prevalence <- list(
  OBESITY = c(0.39, 0.37, 0.37), HYPERTENSION = c(0.28, 0.28, 0.28),
  MI = c(0.025, 0.032, 0.033), CHF = c(0.033, 0.037, 0.039),
  PVD = c(0.030, 0.032, 0.032), CVD = c(0.032, 0.033, 0.034),
  RD = c(0.050, 0.049, 0.051), PULMONARY = c(0.21, 0.19, 0.19),
  PUD = c(0.011, 0.011, 0.011), HEMI_PARA = c(0.008, 0.008, 0.008),
  diabetes = c(0.13, 0.14, 0.14), DEMENTIA = c(0.014, 0.016, 0.015),
  liver = c(0.045, 0.044, 0.042), RENAL = c(0.084, 0.089, 0.090),
  cancer = c(0.063, 0.059, 0.068), HIV = c(0.005, 0.002, 0.002),
  TOBACCO = c(0.097, 0.092, 0.092), SUBSTANCE = c(0.028, 0.025, 0.022)
)
for (var in names(comorbidity_prevalence)) {
  synthetic_data[[var]] <- mapply(function(rural) {
    idx <- match(rural, c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
    rbinom(1, 1, comorbidity_prevalence[[var]][idx])
  }, synthetic_data$rurality)
}

abnormality_prevalence <- list(
  Abnormality_of_the_respiratory_system = c(0.27, 0.23, 0.23),
  Abnormality_of_the_cardiovascular_system = c(0.33, 0.29, 0.29),
  Abnormality_of_the_immune_system = c(0.21, 0.17, 0.17),
  Abnormality_of_the_nervous_system = c(0.29, 0.24, 0.24),
  Constitutional_symptom = c(0.17, 0.14, 0.14),
  Abnormality_of_metabolism_homeostasis = c(0.31, 0.26, 0.26),
  Abnormality_of_blood_and_blood_forming_tissues = c(0.2, 0.17, 0.16),
  Abnormality_of_the_digestive_system = c(0.22, 0.18, 0.18),
  Abnormality_of_the_endocrine_system = c(0.096, 0.08, 0.08),
  Abnormality_of_the_musculoskeletal_system = c(0.22, 0.18, 0.18),
  Abnormality_of_the_integument = c(0.12, 0.095, 0.095),
  Abnormality_of_the_genitourinary_system = c(0.18, 0.16, 0.16),
  Neoplasm = c(0.059, 0.051, 0.047)
)

# Add abnormalities based on rurality-specific prevalence
for (var in names(abnormality_prevalence)) {
  synthetic_data[[var]] <- mapply(function(rural) {
    idx <- match(rural, c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
    rbinom(1, 1, abnormality_prevalence[[var]][idx])
  }, synthetic_data$rurality)
}

# Long COVID and reinfection
synthetic_data$long_covid <- ifelse(
  synthetic_data$covid_status == "COVID",
  mapply(function(rural) rbinom(1, 1, 0.013), synthetic_data$rurality),
  0
)
synthetic_data$reinfection <- ifelse(
  synthetic_data$covid_status == "COVID",
  mapply(function(rural) rbinom(1, 1, 0.048), synthetic_data$rurality),
  0
)

# Mortality and time-to-death assignment with 66% of censored cases at 730 days
synthetic_data <- synthetic_data %>%
  rowwise() %>%
  mutate(
    death = rbinom(1, 1, get_death_probability(covid_status, rurality))
  ) %>%
  ungroup() %>%
  mutate(
    time_to_death = case_when(
      death == 1 ~ round(runif(n(), 1, 730)),  # deceased: uniform survival times
      death == 0 & runif(n()) < 0.66 ~ 730,    # 66% of censored: fixed at 730
      death == 0 ~ round(rbeta(n(), 5, 1.5) * 730)  # rest of censored: right-skewed
    )
  )

# Load and process RUCC data to assign county fips randomly based on population
rucc_df <- readr::read_csv("RUCC_2023.csv", show_col_types = FALSE)

# Clean population column
rucc_df <- rucc_df %>%
  mutate(Population_2020 = as.numeric(gsub(",", "", Population_2020))) %>%
  filter(!is.na(Population_2020))

# Normalize population weights
rucc_df <- rucc_df %>%
  mutate(weight = Population_2020 / sum(Population_2020))

# Assign FIPS based on population weights

# Sample FIPS codes for each individual in synthetic_data
synthetic_data$FIPS <- sample(
  rucc_df$FIPS,
  size = nrow(synthetic_data),
  replace = TRUE,
  prob = rucc_df$weight
)

# Merge RUCC info into synthetic_data
synthetic_data <- synthetic_data %>%
  left_join(rucc_df %>% select(FIPS, RUCC_2023), by = "FIPS")

# Add hospitalization variable (15% prevalence, higher death rate)

# Assign 15% hospitalized status randomly
synthetic_data$hospitalization <- rbinom(nrow(synthetic_data), 1, 0.15)

# Force approx. 50% of deaths to be hospitalized
death_idx <- which(synthetic_data$death == 1)

# Target 50% of deaths to be among hospitalized
target_n_hosp_deaths <- round(length(death_idx) * 0.5)

# Identify which of the deaths are already hospitalized
current_hosp_deaths <- which(synthetic_data$death == 1 & synthetic_data$hospitalization == 1)

# If too few, flip some non-hospitalized deaths to hospitalized
if (length(current_hosp_deaths) < target_n_hosp_deaths) {
  deficit <- target_n_hosp_deaths - length(current_hosp_deaths)
  flip_idx <- sample(setdiff(death_idx, current_hosp_deaths), size = deficit)
  synthetic_data$hospitalization[flip_idx] <- 1
}

# If too many, reverse some hospitalized deaths
if (length(current_hosp_deaths) > target_n_hosp_deaths) {
  excess <- length(current_hosp_deaths) - target_n_hosp_deaths
  unflip_idx <- sample(current_hosp_deaths, size = excess)
  synthetic_data$hospitalization[unflip_idx] <- 0
}
