community_prevalance_adjusted <- function(updated_hr_breakthrough) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(survival)
  library(survminer)

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    obesity_before_covid,
    smoking_status,
    CCI_INDEX,
    CCI_CAT,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    any_cancer,
    obesity_before_covid,
    isc_status,
    vaccine_manufacturer,
    breakthrough_case,
    Delta,
    pre_vaccine_infection,
    quarter_of_vaccination,
    region,
    rurality,
    RUCA1,
    sdi_score,
    sdi_categories,
    hesitancy,
    hestiancy_categories,
    days_to_censor,
    i_rural,
    community_cumulative_incidence,
    comorbditity_cats
  )

  local_df$quarter_of_vaccination <- factor(local_df$quarter_of_vaccination, levels = c("Dec-Feb", "Mar-May", ">May"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$CCI_CAT <- factor(local_df$CCI_CAT, levels = c("0", "1-3", ">3"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West", "Missing"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$i_rural <- relevel(as.factor(local_df$i_rural), ref = "Urban")
  local_df$RUCA1 <- factor(local_df$RUCA1, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
  local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels = c("Low", "Medium", "High"))

  local_df$community_cumulative_incidence <- relevel(as.factor(local_df$community_cumulative_incidence), ref = "Low")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$CCI_CAT <- relevel(as.factor(local_df$CCI_CAT), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$Delta <- relevel(as.factor(local_df$Delta), ref = "Pre-Delta")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_group <- relevel(as.factor(local_df$age_group), ref = "30-49")
  local_df$sdi_categories <- relevel(as.factor(local_df$sdi_categories), ref = "High")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")

  m1 <- coxph(Surv(days_to_censor, breakthrough_case) ~ age_group + gender_concept_name + race_ethnicity + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_before_covid + isc_status + region + obesity_before_covid + vaccine_manufacturer + Delta + pre_vaccine_infection + community_cumulative_incidence, data = local_df) %>%
    tbl_regression(exponentiate = TRUE)

  print(m1)

  table1 <- as_tibble(m1, col_labels = FALSE)

  return(table1)
}


figure_3 <- function(updated_hr_breakthrough) {
  library(ggplot2)
  library(dplyr)
  require(gridExtra)
  require(scales)

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    person_id, dose_2_date, breakthrough_date, i_rural
  )

  temperatureColor <- c("#69b3a2", "#ad122a")

  datebreaks <- seq(as.Date("2020-01-01"), as.Date("2022-01-01"), by = "1 month")

  local_df1 <- filter(local_df, i_rural == "Urban")
  local_df2 <- filter(local_df, i_rural == "Rural")

  p1 <- ggplot(local_df, aes(breakthrough_date, fill = i_rural)) +
    geom_bar() +
    theme(panel.background = element_rect(fill = NA, colour = NA))

  p <- ggplot() +
    geom_histogram(local_df, mapping = aes(x = dose_2_date, fill = i_rural), binwidth = 7, colour = "black") +
    geom_histogram(local_df1, mapping = aes(x = dose_2_date), binwidth = 7, colour = "black", fill = "#00BFC4") +
    geom_density(local_df1, mapping = aes(x = breakthrough_date, y = ..density.. * 4000000), binwidth = 7, colour = "blue", fill = "#00BFC4", alpha = 0.5) +
    geom_density(local_df2, mapping = aes(x = breakthrough_date, y = ..density.. * 4000000), binwidth = 7, colour = "blue", fill = "#F8766D", alpha = 0.5) +
    scale_y_continuous(
      # Features of the first axis
      name = "Number of Persons Receiving Second Vaccination in Each Week",
      labels = comma,

      # Add a second axis and specify its features
      sec.axis = sec_axis(~ . / 4000, name = "Breakthrough Incidence Rate (per 1000-person week)") # , labels = scales::percent)
    ) +
    xlab("Calendar time") +
    scale_fill_discrete(name = "Rurality", labels = c("Rural", "Urban")) +
    theme(legend.position = "top") +
    theme(
      panel.grid.major = element_line(
        size = 0.5, linetype = "solid",
        colour = "grey75"
      ),
      panel.background = element_rect(fill = "grey99", colour = "black"),
      plot.background = element_rect(fill = "transparent", colour = NA),
      axis.title = element_text(size = 16),
      legend.text = element_text(size = 14)
    ) +
    scale_x_date(breaks = datebreaks)

  # image: svg
  plot(p)

  return(NULL)
}

glm_all_adverse <- function(outcomes_comparison_dataset) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(tidyverse)

  local_df <- outcomes_comparison_dataset %>% dplyr::select(
    covid_positive_test,
    prior_covid_infection,
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    i_rural,
    rurality,
    hesitancy,
    hestiancy_categories,
    # sdi_score,
    # sdi_categories,
    SVI,
    SVI_category,
    Series_Complete_18PlusPop_Pct,
    county_vaccination_percentage,
    period_incidence,
    community_cumulative_incidence,
    region,
    CCI_INDEX,
    comorbditity_cats,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_documented,
    isc_status,
    cohort,
    outcomes,
    hospitalized,
    adverse
  )

  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))
  local_df$i_rural <- factor(local_df$i_rural, levels = c("Urban", "Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))
  local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels = c("Low", "Medium", "High"))
  local_df$outcomes <- factor(local_df$outcomes, levels = c("adverse", "hospitalized", "non-hospitalized"))
  local_df$cohort <- factor(local_df$cohort, levels = c("vaccinated", "unvaccinated"))

  local_df$community_cumulative_incidence <- relevel(as.factor(local_df$community_cumulative_incidence), ref = "Low")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$county_vaccination_percentage <- relevel(as.factor(local_df$county_vaccination_percentage), ref = "High")
  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$SVI_category <- relevel(as.factor(local_df$SVI_category), ref = "Low")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_group <- relevel(as.factor(local_df$age_group), ref = "30-49")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")
  local_df$cohort <- relevel(as.factor(local_df$cohort), ref = "vaccinated")

  local_df <- filter(local_df, covid_positive_test == 1)

  m1 <- glm(adverse ~ age_group + gender_concept_name + race_ethnicity + prior_covid_infection + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_documented + obesity_documented + isc_status + region + SVI_category + county_vaccination_percentage + community_cumulative_incidence + hestiancy_categories + rurality + cohort, data = local_df, family = binomial(link = "logit"))

  table1 <- m1 %>%
    tbl_regression(exponentiate = TRUE) %>%
    add_nevent(location = "level")

  print(table1)

  table1 <- as_tibble(table1, col_labels = FALSE)
  return(table1)
}


glm_all_hospitalized <- function(outcomes_comparison_dataset) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(tidyverse)

  local_df <- outcomes_comparison_dataset %>% dplyr::select(
    covid_positive_test,
    prior_covid_infection,
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    i_rural,
    rurality,
    hesitancy,
    hestiancy_categories,
    # sdi_score,
    # sdi_categories,
    SVI,
    SVI_category,
    Series_Complete_18PlusPop_Pct,
    county_vaccination_percentage,
    period_incidence,
    community_cumulative_incidence,
    region,
    CCI_INDEX,
    comorbditity_cats,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_documented,
    isc_status,
    cohort,
    outcomes,
    hospitalized
  )

  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))
  local_df$i_rural <- factor(local_df$i_rural, levels = c("Urban", "Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))
  local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels = c("Low", "Medium", "High"))
  local_df$outcomes <- factor(local_df$outcomes, levels = c("adverse", "hospitalized", "non-hospitalized"))
  local_df$cohort <- factor(local_df$cohort, levels = c("vaccinated", "unvaccinated"))

  local_df$community_cumulative_incidence <- relevel(as.factor(local_df$community_cumulative_incidence), ref = "Low")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$county_vaccination_percentage <- relevel(as.factor(local_df$county_vaccination_percentage), ref = "High")
  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$SVI_category <- relevel(as.factor(local_df$SVI_category), ref = "Low")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_group <- relevel(as.factor(local_df$age_group), ref = "30-49")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")
  local_df$cohort <- relevel(as.factor(local_df$cohort), ref = "vaccinated")

  local_df <- filter(local_df, covid_positive_test == 1)

  m1 <- glm(hospitalized ~ age_group + gender_concept_name + race_ethnicity + prior_covid_infection + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_documented + obesity_documented + isc_status + region + SVI_category + county_vaccination_percentage + community_cumulative_incidence + hestiancy_categories + rurality + cohort, data = local_df, family = binomial(link = "logit"))

  table1 <- m1 %>%
    tbl_regression(exponentiate = TRUE) %>%
    add_nevent()
  table1 <- as_tibble(table1, col_labels = FALSE)
  return(table1)
}


hr_adjusted_all_exposures <- function(updated_hr_breakthrough) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(survival)
  library(car)

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    obesity_before_covid,
    smoking_status,
    CCI_INDEX,
    CCI_CAT,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_before_covid,
    isc_status,
    vaccine_manufacturer,
    breakthrough_case,
    Delta,
    pre_vaccine_infection,
    quarter_of_vaccination,
    region,
    rurality,
    sdi_score,
    sdi_categories,
    hesitancy,
    hestiancy_categories,
    days_to_censor,
    i_rural,
    SVI_category,
    county_vaccination_percentage,
    comorbditity_cats,
    community_cumulative_incidence
  )

  local_df$quarter_of_vaccination <- factor(local_df$quarter_of_vaccination, levels = c("Dec-Feb", "Mar-May", ">May"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$CCI_CAT <- factor(local_df$CCI_CAT, levels = c("0", "1-3", ">3"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))
  local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels = c("Low", "Medium", "High"))

  local_df$community_cumulative_incidence <- relevel(as.factor(local_df$community_cumulative_incidence), ref = "Low")
  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$SVI_category <- relevel(as.factor(local_df$SVI_category), ref = "Low")
  local_df$county_vaccination_percentage <- relevel(as.factor(local_df$county_vaccination_percentage), ref = "High")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$CCI_CAT <- relevel(as.factor(local_df$CCI_CAT), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$Delta <- relevel(as.factor(local_df$Delta), ref = "Pre-Delta")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_group <- relevel(as.factor(local_df$age_group), ref = "30-49")
  local_df$sdi_categories <- relevel(as.factor(local_df$sdi_categories), ref = "High")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")
  local_df$i_rural <- relevel(as.factor(local_df$i_rural), ref = "Urban")

  m1 <- coxph(Surv(days_to_censor, breakthrough_case) ~ age_group + gender_concept_name + race_ethnicity + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_before_covid + isc_status + region + obesity_before_covid + vaccine_manufacturer + Delta + pre_vaccine_infection + SVI_category + county_vaccination_percentage + community_cumulative_incidence + hestiancy_categories + rurality, data = local_df) %>%
    tbl_regression(exponentiate = TRUE)

  print(m1)

  table1 <- as_tibble(m1, col_labels = FALSE)

  return(table1)
}


hr_adjusted_all_exposures_altered_hesitancy_categories <- function(updated_hr_breakthrough) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(survival)
  library(car)

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    obesity_before_covid,
    smoking_status,
    CCI_INDEX,
    CCI_CAT,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_before_covid,
    isc_status,
    vaccine_manufacturer,
    breakthrough_case,
    Delta,
    pre_vaccine_infection,
    quarter_of_vaccination,
    region,
    rurality,
    sdi_score,
    sdi_categories,
    hesitancy,
    hesitancy_sensitivity_category,
    days_to_censor,
    i_rural,
    SVI_category,
    county_vaccination_percentage,
    comorbditity_cats,
    community_cumulative_incidence
  )

  local_df$quarter_of_vaccination <- factor(local_df$quarter_of_vaccination, levels = c("Dec-Feb", "Mar-May", ">May"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$CCI_CAT <- factor(local_df$CCI_CAT, levels = c("0", "1-3", ">3"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hesitancy_sensitivity_category <- factor(local_df$hesitancy_sensitivity_category, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))
  local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels = c("Low", "Medium", "High"))

  local_df$community_cumulative_incidence <- relevel(as.factor(local_df$community_cumulative_incidence), ref = "Low")
  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$SVI_category <- relevel(as.factor(local_df$SVI_category), ref = "Low")
  local_df$county_vaccination_percentage <- relevel(as.factor(local_df$county_vaccination_percentage), ref = "High")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$CCI_CAT <- relevel(as.factor(local_df$CCI_CAT), ref = "0")
  local_df$hesitancy_sensitivity_category <- relevel(as.factor(local_df$hesitancy_sensitivity_category), ref = "Low")
  local_df$Delta <- relevel(as.factor(local_df$Delta), ref = "Pre-Delta")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_group <- relevel(as.factor(local_df$age_group), ref = "30-49")
  local_df$sdi_categories <- relevel(as.factor(local_df$sdi_categories), ref = "High")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")
  local_df$i_rural <- relevel(as.factor(local_df$i_rural), ref = "Urban")

  local_df <- filter(local_df, pre_vaccine_infection == 0)

  m1 <- coxph(Surv(days_to_censor, breakthrough_case) ~ age_group + gender_concept_name + race_ethnicity + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_before_covid + isc_status + region + obesity_before_covid + vaccine_manufacturer + Delta + SVI_category + county_vaccination_percentage + community_cumulative_incidence + hesitancy_sensitivity_category + rurality, data = local_df) %>%
    tbl_regression(exponentiate = TRUE)

  print(m1)

  table1 <- as_tibble(m1, col_labels = FALSE)

  return(table1)
}


hr_adjusted_all_exposures_mixed_effects <- function(updated_hr_breakthrough) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(survival)
  library(car)
  library(coxme)
  library(broom)

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    age,
    age_group,
    data_partner_id,
    gender_concept_name,
    race_ethnicity,
    obesity_before_covid,
    smoking_status,
    CCI_INDEX,
    CCI_CAT,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_before_covid,
    isc_status,
    vaccine_manufacturer,
    breakthrough_case,
    Delta,
    pre_vaccine_infection,
    quarter_of_vaccination,
    region,
    rurality,
    sdi_score,
    sdi_categories,
    hesitancy,
    hestiancy_categories,
    days_to_censor,
    i_rural,
    SVI_category,
    county_vaccination_percentage,
    comorbditity_cats,
    community_cumulative_incidence
  )

  local_df$quarter_of_vaccination <- factor(local_df$quarter_of_vaccination, levels = c("Dec-Feb", "Mar-May", ">May"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$CCI_CAT <- factor(local_df$CCI_CAT, levels = c("0", "1-3", ">3"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))
  local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels = c("Low", "Medium", "High"))

  local_df$community_cumulative_incidence <- relevel(as.factor(local_df$community_cumulative_incidence), ref = "Low")
  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$SVI_category <- relevel(as.factor(local_df$SVI_category), ref = "Low")
  local_df$county_vaccination_percentage <- relevel(as.factor(local_df$county_vaccination_percentage), ref = "High")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$CCI_CAT <- relevel(as.factor(local_df$CCI_CAT), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$Delta <- relevel(as.factor(local_df$Delta), ref = "Pre-Delta")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_group <- relevel(as.factor(local_df$age_group), ref = "30-49")
  local_df$sdi_categories <- relevel(as.factor(local_df$sdi_categories), ref = "High")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")
  local_df$i_rural <- relevel(as.factor(local_df$i_rural), ref = "Urban")

  m1 <- coxme(Surv(days_to_censor, breakthrough_case) ~ age_group + gender_concept_name + race_ethnicity + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_before_covid + isc_status + region + obesity_before_covid + vaccine_manufacturer + Delta + pre_vaccine_infection + SVI_category + county_vaccination_percentage + community_cumulative_incidence + hestiancy_categories + rurality + (1 | data_partner_id), data = local_df)

  m2 <- Anova(m1, type = "III")

  print(summary(m1))

  print(m2)

  confint.coxme <- function(object, parm = NULL, level = 0.95, ..., more = TRUE) {
    if (!is.null(parm)) warning("[confint.coxme] argument 'parm' doesn't do anything for this method")
    if (level != 0.95) warning("[confint.coxme] 'level' will be 0.95 regardless of what argument you give it. Ha!")
    co <- exp(coef(object))
    se <- sqrt(diag(stats::vcov(object)))
    m <- matrix(c(co - 2 * se, co + 2 * se), ncol = 2)
    colnames(m) <- c("2.5 %", "97.5 %")
    rownames(m) <- names(co)
    if (more) {
      p <- 2 * stats::pnorm(abs(co / se), lower.tail = F)
      m <- cbind(m, co, p)
      rownames(m)[3:4] <- c("coef", "p")
    }
    return(m)
  }

  m3 <- confint.coxme(m1)

  print(m3)

  return(NULL)
}


hr_adjusted_all_exposures_mixed_effects_rural_only <- function(updated_hr_breakthrough) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(survival)
  library(car)
  library(coxme)
  library(broom)

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    age,
    age_group,
    data_partner_id,
    gender_concept_name,
    race_ethnicity,
    obesity_before_covid,
    smoking_status,
    CCI_INDEX,
    CCI_CAT,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_before_covid,
    isc_status,
    vaccine_manufacturer,
    breakthrough_case,
    Delta,
    pre_vaccine_infection,
    quarter_of_vaccination,
    region,
    rurality,
    sdi_score,
    sdi_categories,
    hesitancy,
    hestiancy_categories,
    days_to_censor,
    i_rural,
    SVI_category,
    county_vaccination_percentage,
    comorbditity_cats,
    community_cumulative_incidence
  )

  local_df$quarter_of_vaccination <- factor(local_df$quarter_of_vaccination, levels = c("Dec-Feb", "Mar-May", ">May"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$CCI_CAT <- factor(local_df$CCI_CAT, levels = c("0", "1-3", ">3"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA"))
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))
  local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels = c("Low", "Medium", "High"))

  local_df$community_cumulative_incidence <- relevel(as.factor(local_df$community_cumulative_incidence), ref = "Low")
  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$SVI_category <- relevel(as.factor(local_df$SVI_category), ref = "Low")
  local_df$county_vaccination_percentage <- relevel(as.factor(local_df$county_vaccination_percentage), ref = "High")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$CCI_CAT <- relevel(as.factor(local_df$CCI_CAT), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$Delta <- relevel(as.factor(local_df$Delta), ref = "Pre-Delta")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_group <- relevel(as.factor(local_df$age_group), ref = "30-49")
  local_df$sdi_categories <- relevel(as.factor(local_df$sdi_categories), ref = "High")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")
  local_df$i_rural <- relevel(as.factor(local_df$i_rural), ref = "Urban")

  local_df1 <- filter(local_df, i_rural == "Urban")

  m1 <- coxme(Surv(days_to_censor, breakthrough_case) ~ age_group + gender_concept_name + race_ethnicity + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_before_covid + isc_status + region + obesity_before_covid + vaccine_manufacturer + Delta + pre_vaccine_infection + SVI_category + county_vaccination_percentage + community_cumulative_incidence + hestiancy_categories + (1 | data_partner_id), data = local_df1)

  m2 <- Anova(m1, type = "III")

  print(summary(m1))

  print(m2)

  confint.coxme <- function(object, parm = NULL, level = 0.95, ..., more = TRUE) {
    if (!is.null(parm)) warning("[confint.coxme] argument 'parm' doesn't do anything for this method")
    if (level != 0.95) warning("[confint.coxme] 'level' will be 0.95 regardless of what argument you give it. Ha!")
    co <- exp(coef(object))
    se <- sqrt(diag(stats::vcov(object)))
    m <- matrix(c(co - 2 * se, co + 2 * se), ncol = 2)
    colnames(m) <- c("2.5 %", "97.5 %")
    rownames(m) <- names(co)
    if (more) {
      p <- 2 * stats::pnorm(abs(co / se), lower.tail = F)
      m <- cbind(m, co, p)
      rownames(m)[3:4] <- c("coef", "p")
    }
    return(m)
  }

  m3 <- confint.coxme(m1)

  print(m3)

  local_df2 <- filter(local_df, i_rural == "Rural")

  m4 <- coxme(Surv(days_to_censor, breakthrough_case) ~ age_group + gender_concept_name + race_ethnicity + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_before_covid + isc_status + region + obesity_before_covid + vaccine_manufacturer + Delta + pre_vaccine_infection + SVI_category + county_vaccination_percentage + community_cumulative_incidence + hestiancy_categories + (1 | data_partner_id), data = local_df2)

  m3 <- Anova(m1, type = "III")

  print(summary(m4))

  print(m2)

  confint.coxme <- function(object, parm = NULL, level = 0.95, ..., more = TRUE) {
    if (!is.null(parm)) warning("[confint.coxme] argument 'parm' doesn't do anything for this method")
    if (level != 0.95) warning("[confint.coxme] 'level' will be 0.95 regardless of what argument you give it. Ha!")
    co <- exp(coef(object))
    se <- sqrt(diag(stats::vcov(object)))
    m <- matrix(c(co - 2 * se, co + 2 * se), ncol = 2)
    colnames(m) <- c("2.5 %", "97.5 %")
    rownames(m) <- names(co)
    if (more) {
      p <- 2 * stats::pnorm(abs(co / se), lower.tail = F)
      m <- cbind(m, co, p)
      rownames(m)[3:4] <- c("coef", "p")
    }
    return(m)
  }

  m6 <- confint.coxme(m4)

  print(m4)

  return(NULL)
}


hr_adjusted_all_exposures_mixed_effects_test <- function(updated_hr_breakthrough) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(survival)
  library(car)
  library(coxme)
  library(magrittr)

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    age,
    age_group,
    data_partner_id,
    gender_concept_name,
    race_ethnicity,
    obesity_before_covid,
    smoking_status,
    CCI_INDEX,
    CCI_CAT,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_before_covid,
    isc_status,
    vaccine_manufacturer,
    breakthrough_case,
    Delta,
    pre_vaccine_infection,
    quarter_of_vaccination,
    region,
    rurality,
    sdi_score,
    sdi_categories,
    hesitancy,
    hestiancy_categories,
    days_to_censor,
    i_rural,
    SVI_category,
    county_vaccination_percentage,
    comorbditity_cats,
    community_cumulative_incidence
  )

  local_df$quarter_of_vaccination <- factor(local_df$quarter_of_vaccination, levels = c("Dec-Feb", "Mar-May", ">May"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$CCI_CAT <- factor(local_df$CCI_CAT, levels = c("0", "1-3", ">3"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA"))
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))
  local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels = c("Low", "Medium", "High"))

  local_df$community_cumulative_incidence <- relevel(as.factor(local_df$community_cumulative_incidence), ref = "Low")
  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$SVI_category <- relevel(as.factor(local_df$SVI_category), ref = "Low")
  local_df$county_vaccination_percentage <- relevel(as.factor(local_df$county_vaccination_percentage), ref = "High")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$CCI_CAT <- relevel(as.factor(local_df$CCI_CAT), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$Delta <- relevel(as.factor(local_df$Delta), ref = "Pre-Delta")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_group <- relevel(as.factor(local_df$age_group), ref = "30-49")
  local_df$sdi_categories <- relevel(as.factor(local_df$sdi_categories), ref = "High")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")
  local_df$i_rural <- relevel(as.factor(local_df$i_rural), ref = "Urban")

  m1 <- coxme(Surv(days_to_censor, breakthrough_case) ~ age_group + gender_concept_name + race_ethnicity + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_before_covid + isc_status + region + obesity_before_covid + vaccine_manufacturer + Delta + pre_vaccine_infection + SVI_category + county_vaccination_percentage + community_cumulative_incidence + hestiancy_categories + rurality + (1 | data_partner_id), data = local_df)

  m2 <- Anova(m1, type = "III")

  print(summary(m1))

  print(m2)

  coef_fixed_coxme <- function(x) {
    d <-
      tibble::tibble(
        beta = x$coefficients
      )

    nvar <- nrow(d)
    nfrail <- nrow(x$var) - nvar
    fixed_effects_indexes <- nfrail + seq_len(nvar)

    d %>%
      dplyr::mutate(
        exp_coef  = exp(beta),
        se_coef   = sqrt(diag(x$var)[fixed_effects_indexes]),
        z         = beta / se_coef,
        p         = 1 - pchisq((beta / se_coef)^2, 1),
      )
  }

  m3 <- coef_fixed_coxme(m1)

  print(m3)

  return(m3)
}


hr_adjusted_all_exposures_no_prior_covid <- function(updated_hr_breakthrough) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(survival)
  library(car)

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    obesity_before_covid,
    smoking_status,
    CCI_INDEX,
    CCI_CAT,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_before_covid,
    isc_status,
    vaccine_manufacturer,
    breakthrough_case,
    Delta,
    pre_vaccine_infection,
    quarter_of_vaccination,
    region,
    rurality,
    sdi_score,
    sdi_categories,
    hesitancy,
    hestiancy_categories,
    days_to_censor,
    i_rural,
    SVI_category,
    county_vaccination_percentage,
    comorbditity_cats,
    community_cumulative_incidence
  )

  local_df$quarter_of_vaccination <- factor(local_df$quarter_of_vaccination, levels = c("Dec-Feb", "Mar-May", ">May"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$CCI_CAT <- factor(local_df$CCI_CAT, levels = c("0", "1-3", ">3"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))
  local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels = c("Low", "Medium", "High"))

  local_df$community_cumulative_incidence <- relevel(as.factor(local_df$community_cumulative_incidence), ref = "Low")
  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$SVI_category <- relevel(as.factor(local_df$SVI_category), ref = "Low")
  local_df$county_vaccination_percentage <- relevel(as.factor(local_df$county_vaccination_percentage), ref = "High")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$CCI_CAT <- relevel(as.factor(local_df$CCI_CAT), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$Delta <- relevel(as.factor(local_df$Delta), ref = "Pre-Delta")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_group <- relevel(as.factor(local_df$age_group), ref = "30-49")
  local_df$sdi_categories <- relevel(as.factor(local_df$sdi_categories), ref = "High")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")
  local_df$i_rural <- relevel(as.factor(local_df$i_rural), ref = "Urban")

  local_df <- filter(local_df, pre_vaccine_infection == 0)

  m1 <- coxph(Surv(days_to_censor, breakthrough_case) ~ age_group + gender_concept_name + race_ethnicity + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_before_covid + isc_status + region + obesity_before_covid + vaccine_manufacturer + Delta + SVI_category + county_vaccination_percentage + community_cumulative_incidence + hestiancy_categories + rurality, data = local_df) %>%
    tbl_regression(exponentiate = TRUE)

  print(m1)

  table1 <- as_tibble(m1, col_labels = FALSE)

  return(table1)
}

hr_adjusted_hesitancy_cats <- function(updated_hr_breakthrough) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(survival)
  library(car)

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    obesity_before_covid,
    smoking_status,
    CCI_INDEX,
    CCI_CAT,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_before_covid,
    isc_status,
    vaccine_manufacturer,
    breakthrough_case,
    Delta,
    pre_vaccine_infection,
    quarter_of_vaccination,
    region,
    rurality,
    sdi_score,
    sdi_categories,
    hesitancy,
    hestiancy_categories,
    rural_hesitancy_categories,
    days_to_censor,
    i_rural,
    comorbditity_cats
  )

  local_df$quarter_of_vaccination <- factor(local_df$quarter_of_vaccination, levels = c("Dec-Feb", "Mar-May", ">May"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$CCI_CAT <- factor(local_df$CCI_CAT, levels = c("0", "1-3", ">3"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$rural_hesitancy_categories <- factor(local_df$rural_hesitancy_categories, levels = c("Urban, Low Hesitancy", "Urban, Medium Hesitancy", "Urban, High Hesitancy", "Rural, Low Hesitancy", "Rural, Medium Hesitancy", "Rural, High Hesitancy"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))

  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$CCI_CAT <- relevel(as.factor(local_df$CCI_CAT), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$Delta <- relevel(as.factor(local_df$Delta), ref = "Pre-Delta")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_group <- relevel(as.factor(local_df$age_group), ref = "30-49")
  local_df$sdi_categories <- relevel(as.factor(local_df$sdi_categories), ref = "High")
  local_df$rural_hesitancy_categories <- relevel(as.factor(local_df$rural_hesitancy_categories), ref = "Urban, Low Hesitancy")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")
  local_df$i_rural <- relevel(as.factor(local_df$i_rural), ref = "Urban")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")

  m1 <- coxph(Surv(days_to_censor, breakthrough_case) ~ age_group + gender_concept_name + race_ethnicity + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_before_covid + isc_status + region + obesity_before_covid + vaccine_manufacturer + Delta + pre_vaccine_infection + hestiancy_categories, data = local_df) %>%
    tbl_regression(exponentiate = TRUE)

  table1 <- as_tibble(m1, col_labels = FALSE)

  return(table1)
}

hr_adjusted_irural <- function(updated_hr_breakthrough) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(survival)

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    obesity_before_covid,
    smoking_status,
    CCI_INDEX,
    CCI_CAT,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_before_covid,
    isc_status,
    vaccine_manufacturer,
    breakthrough_case,
    Delta,
    pre_vaccine_infection,
    quarter_of_vaccination,
    region,
    rurality,
    sdi_score,
    sdi_categories,
    hesitancy,
    hestiancy_categories,
    days_to_censor,
    i_rural,
    comorbditity_cats
  )

  local_df$quarter_of_vaccination <- factor(local_df$quarter_of_vaccination, levels = c("Dec-Feb", "Mar-May", ">May"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$CCI_CAT <- factor(local_df$CCI_CAT, levels = c("0", "1-3", ">3"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))

  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$CCI_CAT <- relevel(as.factor(local_df$CCI_CAT), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$Delta <- relevel(as.factor(local_df$Delta), ref = "Pre-Delta")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_group <- relevel(as.factor(local_df$age_group), ref = "30-49")
  local_df$sdi_categories <- relevel(as.factor(local_df$sdi_categories), ref = "High")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")
  local_df$i_rural <- relevel(as.factor(local_df$i_rural), ref = "Urban")

  m1 <- coxph(Surv(days_to_censor, breakthrough_case) ~ age_group + gender_concept_name + race_ethnicity + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_before_covid + isc_status + region + obesity_before_covid + vaccine_manufacturer + Delta + pre_vaccine_infection + i_rural, data = local_df) %>%
    tbl_regression(exponentiate = TRUE)

  table1 <- as_tibble(m1, col_labels = FALSE)

  return(table1)
}


hr_adjusted_rurality <- function(updated_hr_breakthrough) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(survival)

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    obesity_before_covid,
    smoking_status,
    CCI_INDEX,
    CCI_CAT,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_before_covid,
    isc_status,
    vaccine_manufacturer,
    breakthrough_case,
    Delta,
    pre_vaccine_infection,
    quarter_of_vaccination,
    region,
    rurality,
    sdi_score,
    sdi_categories,
    hesitancy,
    hestiancy_categories,
    days_to_censor,
    i_rural,
    comorbditity_cats
  )

  local_df$quarter_of_vaccination <- factor(local_df$quarter_of_vaccination, levels = c("Dec-Feb", "Mar-May", ">May"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$CCI_CAT <- factor(local_df$CCI_CAT, levels = c("0", "1-3", ">3"))
  # local_df$smoking_status <- factor(local_df$smoking_status, levels=c("Non smoker", "Current or Former"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
  # local_df$lifestyle_cat <- factor(local_df$lifestyle_cat, levels=c("No Lifestyle ISC Conditions", "Multiple Lifestyle ISC Conditions", "Nutritional Deficiency", "AUD", "Obesity", "Sleep Disorder", "Current or Former Smoker"))
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))

  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$CCI_CAT <- relevel(as.factor(local_df$CCI_CAT), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$Delta <- relevel(as.factor(local_df$Delta), ref = "Pre-Delta")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_group <- relevel(as.factor(local_df$age_group), ref = "30-49")
  local_df$sdi_categories <- relevel(as.factor(local_df$sdi_categories), ref = "High")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")
  local_df$i_rural <- relevel(as.factor(local_df$i_rural), ref = "Urban")

  m1 <- coxph(Surv(days_to_censor, breakthrough_case) ~ age_group + gender_concept_name + race_ethnicity + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_before_covid + isc_status + region + obesity_before_covid + vaccine_manufacturer + Delta + pre_vaccine_infection + rurality, data = local_df) %>%
    tbl_regression(exponentiate = TRUE)

  table1 <- as_tibble(m1, col_labels = FALSE)

  return(table1)
}

hr_adjusted_sa <- function(updated_hr_breakthrough) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(survival)
  library(car)

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    obesity_before_covid,
    smoking_status,
    CCI_INDEX,
    CCI_CAT,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_before_covid,
    isc_status,
    vaccine_manufacturer,
    breakthrough_case,
    Delta,
    pre_vaccine_infection,
    quarter_of_vaccination,
    region,
    rurality,
    sdi_score,
    sdi_categories,
    hesitancy,
    hestiancy_categories,
    days_to_censor,
    i_rural,
    SVI_category,
    county_vaccination_percentage,
    comorbditity_cats,
    community_cumulative_incidence
  )

  local_df$quarter_of_vaccination <- factor(local_df$quarter_of_vaccination, levels = c("Dec-Feb", "Mar-May", ">May"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$CCI_CAT <- factor(local_df$CCI_CAT, levels = c("0", "1-3", ">3"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))
  local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels = c("Low", "Medium", "High"))

  local_df$community_cumulative_incidence <- relevel(as.factor(local_df$community_cumulative_incidence), ref = "Low")
  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$SVI_category <- relevel(as.factor(local_df$SVI_category), ref = "Low")
  local_df$county_vaccination_percentage <- relevel(as.factor(local_df$county_vaccination_percentage), ref = "High")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$CCI_CAT <- relevel(as.factor(local_df$CCI_CAT), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$Delta <- relevel(as.factor(local_df$Delta), ref = "Pre-Delta")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_group <- relevel(as.factor(local_df$age_group), ref = "30-49")
  local_df$sdi_categories <- relevel(as.factor(local_df$sdi_categories), ref = "High")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")
  local_df$i_rural <- relevel(as.factor(local_df$i_rural), ref = "Urban")

  m1 <- coxph(Surv(days_to_censor, breakthrough_case) ~ age_group + gender_concept_name + race_ethnicity + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_before_covid + isc_status + region + obesity_before_covid + vaccine_manufacturer + Delta + pre_vaccine_infection + SVI_category + county_vaccination_percentage + community_cumulative_incidence + rurality * hestiancy_categories, data = local_df) %>%
    tbl_regression(exponentiate = TRUE)

  table1 <- as_tibble(m1, col_labels = FALSE)

  return(table1)
}

hr_adjusted_vaccine_rate <- function(updated_hr_breakthrough) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(survival)

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    obesity_before_covid,
    smoking_status,
    CCI_INDEX,
    CCI_CAT,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_before_covid,
    isc_status,
    vaccine_manufacturer,
    breakthrough_case,
    Delta,
    pre_vaccine_infection,
    quarter_of_vaccination,
    region,
    rurality,
    sdi_score,
    sdi_categories,
    hesitancy,
    hestiancy_categories,
    days_to_censor,
    i_rural,
    county_vaccination_percentage,
    comorbditity_cats
  )

  local_df$quarter_of_vaccination <- factor(local_df$quarter_of_vaccination, levels = c("Dec-Feb", "Mar-May", ">May"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$CCI_CAT <- factor(local_df$CCI_CAT, levels = c("0", "1-3", ">3"))
  # local_df$smoking_status <- factor(local_df$smoking_status, levels=c("Non smoker", "Current or Former"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
  # local_df$lifestyle_cat <- factor(local_df$lifestyle_cat, levels=c("No Lifestyle ISC Conditions", "Multiple Lifestyle ISC Conditions", "Nutritional Deficiency", "AUD", "Obesity", "Sleep Disorder", "Current or Former Smoker"))
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))

  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$county_vaccination_percentage <- relevel(as.factor(local_df$county_vaccination_percentage), ref = "High")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$CCI_CAT <- relevel(as.factor(local_df$CCI_CAT), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$Delta <- relevel(as.factor(local_df$Delta), ref = "Pre-Delta")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_group <- relevel(as.factor(local_df$age_group), ref = "30-49")
  local_df$sdi_categories <- relevel(as.factor(local_df$sdi_categories), ref = "High")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")
  local_df$i_rural <- relevel(as.factor(local_df$i_rural), ref = "Urban")

  m1 <- coxph(Surv(days_to_censor, breakthrough_case) ~ age_group + gender_concept_name + race_ethnicity + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_before_covid + isc_status + region + obesity_before_covid + vaccine_manufacturer + Delta + pre_vaccine_infection + county_vaccination_percentage, data = local_df) %>%
    tbl_regression(exponentiate = TRUE)

  table1 <- as_tibble(m1, col_labels = FALSE)

  return(table1)
}

hr_sdi_adjusted <- function(updated_hr_breakthrough) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(survival)
  library(survminer)
  library(car)

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    obesity_before_covid,
    smoking_status,
    CCI_INDEX,
    CCI_CAT,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_before_covid,
    isc_status,
    vaccine_manufacturer,
    breakthrough_case,
    Delta,
    pre_vaccine_infection,
    quarter_of_vaccination,
    region,
    rurality,
    sdi_score,
    sdi_categories,
    hesitancy,
    hestiancy_categories,
    days_to_censor,
    i_rural,
    county_vaccination_percentage,
    SVI_category,
    comorbditity_cats
  )

  local_df$quarter_of_vaccination <- factor(local_df$quarter_of_vaccination, levels = c("Dec-Feb", "Mar-May", ">May"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$CCI_CAT <- factor(local_df$CCI_CAT, levels = c("0", "1-3", ">3"))
  # local_df$smoking_status <- factor(local_df$smoking_status, levels=c("Non smoker", "Current or Former"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
  # local_df$lifestyle_cat <- factor(local_df$lifestyle_cat, levels=c("No Lifestyle ISC Conditions", "Multiple Lifestyle ISC Conditions", "Nutritional Deficiency", "AUD", "Obesity", "Sleep Disorder", "Current or Former Smoker"))
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West", "Missing"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$i_rural <- relevel(as.factor(local_df$i_rural), ref = "Urban")
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))

  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$SVI_category <- relevel(as.factor(local_df$SVI_category), ref = "Medium")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$county_vaccination_percentage <- relevel(as.factor(local_df$county_vaccination_percentage), ref = "Medium")
  local_df$CCI_CAT <- relevel(as.factor(local_df$CCI_CAT), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$Delta <- relevel(as.factor(local_df$Delta), ref = "Pre-Delta")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_group <- relevel(as.factor(local_df$age_group), ref = "30-49")
  local_df$sdi_categories <- relevel(as.factor(local_df$sdi_categories), ref = "High")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")

  m1 <- coxph(Surv(days_to_censor, breakthrough_case) ~ age_group + gender_concept_name + race_ethnicity + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_before_covid + isc_status + region + obesity_before_covid + vaccine_manufacturer + Delta + pre_vaccine_infection + sdi_categories, data = local_df) %>%
    tbl_regression(exponentiate = TRUE)

  print(m1)

  table1 <- as_tibble(m1, col_labels = FALSE)

  return(table1)
}

hr_svi_adjusted <- function(updated_hr_breakthrough) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(survival)
  library(survminer)

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    obesity_before_covid,
    smoking_status,
    CCI_INDEX,
    CCI_CAT,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_before_covid,
    isc_status,
    vaccine_manufacturer,
    breakthrough_case,
    Delta,
    pre_vaccine_infection,
    quarter_of_vaccination,
    region,
    rurality,
    sdi_score,
    sdi_categories,
    hesitancy,
    hestiancy_categories,
    days_to_censor,
    i_rural,
    county_vaccination_percentage,
    SVI_category,
    comorbditity_cats
  )

  local_df$quarter_of_vaccination <- factor(local_df$quarter_of_vaccination, levels = c("Dec-Feb", "Mar-May", ">May"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$CCI_CAT <- factor(local_df$CCI_CAT, levels = c("0", "1-3", ">3"))
  # local_df$smoking_status <- factor(local_df$smoking_status, levels=c("Non smoker", "Current or Former"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
  # local_df$lifestyle_cat <- factor(local_df$lifestyle_cat, levels=c("No Lifestyle ISC Conditions", "Multiple Lifestyle ISC Conditions", "Nutritional Deficiency", "AUD", "Obesity", "Sleep Disorder", "Current or Former Smoker"))
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West", "Missing"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$i_rural <- relevel(as.factor(local_df$i_rural), ref = "Urban")
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))

  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$SVI_category <- relevel(as.factor(local_df$SVI_category), ref = "Low")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$county_vaccination_percentage <- relevel(as.factor(local_df$county_vaccination_percentage), ref = "Medium")
  local_df$CCI_CAT <- relevel(as.factor(local_df$CCI_CAT), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$Delta <- relevel(as.factor(local_df$Delta), ref = "Pre-Delta")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_group <- relevel(as.factor(local_df$age_group), ref = "30-49")
  local_df$sdi_categories <- relevel(as.factor(local_df$sdi_categories), ref = "High")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")

  m1 <- coxph(Surv(days_to_censor, breakthrough_case) ~ age_group + gender_concept_name + race_ethnicity + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_before_covid + isc_status + region + obesity_before_covid + vaccine_manufacturer + Delta + pre_vaccine_infection + SVI_category, data = local_df) %>%
    tbl_regression(exponentiate = TRUE)

  print(m1)

  table1 <- as_tibble(m1, col_labels = FALSE)

  return(table1)
}

hr_univariate_all_exposures <- function(updated_hr_breakthrough) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(survival)
  library(car)

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    obesity_before_covid,
    smoking_status,
    CCI_INDEX,
    CCI_CAT,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_before_covid,
    isc_status,
    vaccine_manufacturer,
    breakthrough_case,
    Delta,
    pre_vaccine_infection,
    quarter_of_vaccination,
    region,
    rurality,
    sdi_score,
    sdi_categories,
    hesitancy,
    hestiancy_categories,
    days_to_censor,
    i_rural,
    SVI_category,
    county_vaccination_percentage,
    comorbditity_cats,
    community_cumulative_incidence
  )

  local_df$quarter_of_vaccination <- factor(local_df$quarter_of_vaccination, levels = c("Dec-Feb", "Mar-May", ">May"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$CCI_CAT <- factor(local_df$CCI_CAT, levels = c("0", "1-3", ">3"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))
  local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels = c("Low", "Medium", "High"))

  local_df$community_cumulative_incidence <- relevel(as.factor(local_df$community_cumulative_incidence), ref = "Low")
  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$SVI_category <- relevel(as.factor(local_df$SVI_category), ref = "Low")
  local_df$county_vaccination_percentage <- relevel(as.factor(local_df$county_vaccination_percentage), ref = "High")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$CCI_CAT <- relevel(as.factor(local_df$CCI_CAT), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$Delta <- relevel(as.factor(local_df$Delta), ref = "Pre-Delta")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_group <- relevel(as.factor(local_df$age_group), ref = "30-49")
  local_df$sdi_categories <- relevel(as.factor(local_df$sdi_categories), ref = "High")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")
  local_df$i_rural <- relevel(as.factor(local_df$i_rural), ref = "Urban")

  m1 <- local_df %>%
    select(days_to_censor, breakthrough_case, age_group, gender_concept_name, race_ethnicity, heart_disease, PVD, PUD, liver_disease, diabetes_combined, paralysis, stroke, renal, any_cancer, obesity_before_covid, isc_status, region, obesity_before_covid, vaccine_manufacturer, Delta, pre_vaccine_infection, SVI_category, county_vaccination_percentage, community_cumulative_incidence, hestiancy_categories, rurality) %>%
    tbl_uvregression(
      method = coxph,
      y = Surv(days_to_censor, breakthrough_case),
      exponentiate = TRUE,
      hide_n = TRUE
    )

  print(m1)

  table1 <- as_tibble(m1, col_labels = FALSE)

  return(table1)
}

km_plot_county_cumulative_incidence <- function(updated_hr_breakthrough) {
    library(tidyverse)
    library(dplyr)
    library(survminer)
    library(ggplot2)
    library(viridis)
    library(survival)

palette_low_medium_high <- c(
  "#17a0b0", # blue for "low"
  "#815d9a", # orange/tan for "medium"
  "#cc2127"  # red for "high"
  # "#815d9a", # purple for "medium" if the orange text is too faint
  # "#ffc3b8", # pink
)

local_df <- updated_hr_breakthrough %>% dplyr::select(
age,
age_group,
gender_concept_name,
race_ethnicity,
obesity_before_covid,
smoking_status,
CCI_INDEX,
CCI_CAT,
MI,
CHF,
PVD,
dementia,
pulmonary,
rheumatic,
PUD,
liver_combined,
diabetes_combined,
paralysis,
stroke,
renal,
cancer,
mets,
hiv,
vaccine_manufacturer,
breakthrough_case,
Delta,
pre_vaccine_infection,
quarter_of_vaccination,
region,
rurality,
sdi_score,
sdi_categories,
hesitancy,
hestiancy_categories,
days_to_censor,
i_rural,
community_cumulative_incidence
)

local_df$quarter_of_vaccination <- factor(local_df$quarter_of_vaccination, levels=c("Dec-Feb", "Mar-May", ">May"))
local_df$age_group <- factor(local_df$age_group, levels=c("<30", "30-49", "50-64", "65-75", ">75"))
local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels=c("FEMALE", "MALE"))
local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels=c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
local_df$CCI_CAT <- factor(local_df$CCI_CAT, levels=c("0", "1-3", ">3"))
local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels=c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
local_df$rurality <- factor(local_df$rurality, levels=c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
local_df$region <- factor(local_df$region, levels=c("Northeast", "Midwest", "South", "West"))
local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels=c("Low", "Medium", "High"))
local_df$i_rural <- factor(local_df$i_rural, levels=c("Urban", "Rural"))

local_df$rurality=relevel(as.factor(local_df$rurality),ref="Urban")
local_df$CCI_CAT=relevel(as.factor(local_df$CCI_CAT),ref="0")
local_df$hestiancy_categories=relevel(as.factor(local_df$hestiancy_categories),ref="Low")
local_df$Delta=relevel(as.factor(local_df$Delta),ref="Pre-Delta")
local_df$region=relevel(as.factor(local_df$region),ref="Northeast")
local_df$gender_concept_name=relevel(as.factor(local_df$gender_concept_name),ref="FEMALE")
local_df$age_group=relevel(as.factor(local_df$age_group),ref="30-49")
local_df$sdi_categories=relevel(as.factor(local_df$sdi_categories),ref="Low")
local_df$race_ethnicity=relevel(as.factor(local_df$race_ethnicity),ref="non-Hispanic White")
local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels=c("Low", "Medium", "High"))

# image: svg
km_surv_object=survival::survfit(survival::Surv(days_to_censor,breakthrough_case)~community_cumulative_incidence,data=local_df)

ggsurv<-ggsurvplot(
    km_surv_object,
    data=local_df,
    risk.table = "nrisk_cumevents", 
    fontsize = 3,
    pval=TRUE, 
    pval.coord = c(0, 5),    
    fun = function(x) {(1-x)*1000}, 
    ylim = c(0,40),
    xlim = c(0,180),
    palette = palette_low_medium_high, 
    conf.int = 0.95, 
    legend.labs = c("Low", "Medium", "High"),
    font.main = c(18, "bold", "black"),
    font.x = c(16, "bold.italic", "black"),
    font.y = c(16, "bold.italic", "black"),
    font.tickslab = c(14, "plain", "black"),
    font.legend=c(14,'bold','black'),
    linetype ="solid",
    size=1,
    censor = FALSE,
    break.time.by = 30,
    ylab="Cumulative Incidence of COVID-19 Post-Vaccine \nBreakthrough Infection, per 1000 Persons",
    xlab="Time Since Full Vaccination (Days)") 
    
    # image: svg
    print(ggsurv)

    return(NULL)

   
}

km_plot_hesitancy <- function(updated_hr_breakthrough) {
  library(tidyverse)
  library(dplyr)
  library(survminer)
  library(ggplot2)
  library(viridis)
  library(survival)

  palette_low_medium_high <- c(
    "#17a0b0", # blue for "low"
    "#815d9a", # orange/tan for "medium"
    "#cc2127" # red for "high"
    # "#815d9a", # purple for "medium" if the orange text is too faint
    # "#ffc3b8", # pink
  )

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    obesity_before_covid,
    smoking_status,
    CCI_INDEX,
    CCI_CAT,
    MI,
    CHF,
    PVD,
    dementia,
    pulmonary,
    rheumatic,
    PUD,
    liver_combined,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    cancer,
    mets,
    hiv,
    vaccine_manufacturer,
    breakthrough_case,
    Delta,
    pre_vaccine_infection,
    quarter_of_vaccination,
    region,
    rurality,
    sdi_score,
    sdi_categories,
    hesitancy,
    hestiancy_categories,
    days_to_censor
  )

  local_df$quarter_of_vaccination <- factor(local_df$quarter_of_vaccination, levels = c("Dec-Feb", "Mar-May", ">May"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$CCI_CAT <- factor(local_df$CCI_CAT, levels = c("0", "1-3", ">3"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))

  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$CCI_CAT <- relevel(as.factor(local_df$CCI_CAT), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$Delta <- relevel(as.factor(local_df$Delta), ref = "Pre-Delta")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_group <- relevel(as.factor(local_df$age_group), ref = "30-49")
  local_df$sdi_categories <- relevel(as.factor(local_df$sdi_categories), ref = "Low")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")

  km_surv_object <- survival::survfit(survival::Surv(days_to_censor, breakthrough_case) ~ hestiancy_categories, data = local_df)
  ggsurv <- ggsurvplot(
    km_surv_object,
    data = local_df,
    risk.table = "nrisk_cumevents",
    fontsize = 3,
    pval = TRUE,
    pval.coord = c(0, 5),
    fun = function(x) {
      (1 - x) * 1000
    },
    ylim = c(0, 40),
    xlim = c(0, 180),
    palette = palette_low_medium_high,
    conf.int = 0.95,
    legend.labs = c("Low", "Med", "High"),
    font.main = c(18, "bold", "black"),
    font.x = c(16, "bold.italic", "black"),
    font.y = c(16, "bold.italic", "black"),
    font.tickslab = c(14, "plain", "black"),
    font.legend = c(14, "bold", "black"),
    linetype = "solid",
    size = 1,
    censor = FALSE,
    break.time.by = 30,
    ylab = "Cumulative Incidence of COVID-19 Post-Vaccine \nBreakthrough Infection, per 1000 Persons",
    xlab = "Time Since Full Vaccination (Days)"
  )

  # image: svg
  print(ggsurv)

  return(NULL)
}

km_plot_rurality <- function(updated_hr_breakthrough) {
  library(tidyverse)
  library(dplyr)
  library(survminer)
  library(ggplot2)
  library(viridis)
  library(survival)

  palette_low_medium_high <- c(
    "#17a0b0", # blue for "low"
    "#815d9a", # orange/tan for "medium"
    "#cc2127" # red for "high"
    # "#815d9a", # purple for "medium" if the orange text is too faint
    # "#ffc3b8", # pink
  )

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    obesity_before_covid,
    smoking_status,
    CCI_INDEX,
    CCI_CAT,
    MI,
    CHF,
    PVD,
    dementia,
    pulmonary,
    rheumatic,
    PUD,
    liver_combined,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    cancer,
    mets,
    hiv,
    vaccine_manufacturer,
    breakthrough_case,
    Delta,
    pre_vaccine_infection,
    quarter_of_vaccination,
    region,
    rurality,
    sdi_score,
    sdi_categories,
    hesitancy,
    hestiancy_categories,
    days_to_censor
  )

  local_df$quarter_of_vaccination <- factor(local_df$quarter_of_vaccination, levels = c("Dec-Feb", "Mar-May", ">May"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$CCI_CAT <- factor(local_df$CCI_CAT, levels = c("0", "1-3", ">3"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))

  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$CCI_CAT <- relevel(as.factor(local_df$CCI_CAT), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$Delta <- relevel(as.factor(local_df$Delta), ref = "Pre-Delta")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_group <- relevel(as.factor(local_df$age_group), ref = "30-49")
  local_df$sdi_categories <- relevel(as.factor(local_df$sdi_categories), ref = "Low")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")

  km_surv_object <- survival::survfit(survival::Surv(days_to_censor, breakthrough_case) ~ rurality, data = local_df)

  ggsurv <- ggsurvplot(
    km_surv_object,
    data = local_df,
    risk.table = "nrisk_cumevents",
    fontsize = 3,
    pval = TRUE,
    pval.coord = c(0, 5),
    fun = function(x) {
      (1 - x) * 1000
    },
    ylim = c(0, 40),
    xlim = c(0, 180),
    # xaxs = "s",
    palette = palette_low_medium_high,
    # conf.int = FALSE,
    conf.int = 0.95,
    legend.labs = c("Urban", "Urban-\nAdjacent\nRural", "Nonurban-\nAdjacent\nRural"),
    font.main = c(18, "bold", "black"),
    font.x = c(16, "bold.italic", "black"),
    font.y = c(16, "bold.italic", "black"),
    font.tickslab = c(14, "plain", "black"),
    font.legend = c(14, "bold", "black"),
    linetype = "solid",
    size = 1,
    censor = FALSE,
    break.time.by = 30,
    ylab = "Cumulative Incidence of COVID-19 Post-Vaccine \nBreakthrough Infection, per 1000 Persons",
    xlab = "Time Since Full Vaccination (Days)"
  )

  # image: svg
  print(ggsurv)

  return(NULL)
}

km_plot_sdi <- function(updated_hr_breakthrough) {
  library(tidyverse)
  library(dplyr)
  library(survminer)
  library(ggplot2)
  library(viridis)
  library(survival)

  palette_low_medium_high <- c(
    "#17a0b0", # blue for "low"
    "#815d9a", # orange/tan for "medium"
    "#cc2127" # red for "high"
    # "#815d9a", # purple for "medium" if the orange text is too faint
    # "#ffc3b8", # pink
  )

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    obesity_before_covid,
    smoking_status,
    CCI_INDEX,
    CCI_CAT,
    MI,
    CHF,
    PVD,
    dementia,
    pulmonary,
    rheumatic,
    PUD,
    liver_combined,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    cancer,
    mets,
    hiv,
    vaccine_manufacturer,
    breakthrough_case,
    Delta,
    pre_vaccine_infection,
    quarter_of_vaccination,
    region,
    rurality,
    sdi_score,
    sdi_categories,
    hesitancy,
    hestiancy_categories,
    days_to_censor
  )

  local_df$quarter_of_vaccination <- factor(local_df$quarter_of_vaccination, levels = c("Dec-Feb", "Mar-May", ">May"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$CCI_CAT <- factor(local_df$CCI_CAT, levels = c("0", "1-3", ">3"))
  # local_df$smoking_status <- factor(local_df$smoking_status, levels=c("Non smoker", "Current or Former"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
  # local_df$lifestyle_cat <- factor(local_df$lifestyle_cat, levels=c("No Lifestyle ISC Conditions", "Multiple Lifestyle ISC Conditions", "Nutritional Deficiency", "AUD", "Obesity", "Sleep Disorder", "Current or Former Smoker"))
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))

  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$CCI_CAT <- relevel(as.factor(local_df$CCI_CAT), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$Delta <- relevel(as.factor(local_df$Delta), ref = "Pre-Delta")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_group <- relevel(as.factor(local_df$age_group), ref = "30-49")
  local_df$sdi_categories <- relevel(as.factor(local_df$sdi_categories), ref = "Low")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")

  km_surv_object <- survival::survfit(survival::Surv(days_to_censor, breakthrough_case) ~ sdi_categories, data = local_df)
  ggsurv <- ggsurvplot(
    km_surv_object,
    data = local_df,
    risk.table = "nrisk_cumevents",
    fontsize = 3,
    pval = TRUE,
    pval.coord = c(0, 5),
    fun = function(x) {
      (1 - x) * 1000
    },
    ylim = c(0, 40),
    xlim = c(0, 180),
    palette = palette_low_medium_high,
    conf.int = 0.95,
    legend.labs = c("Low", "High"),
    font.main = c(18, "bold", "black"),
    font.x = c(16, "bold.italic", "black"),
    font.y = c(16, "bold.italic", "black"),
    font.tickslab = c(14, "plain", "black"),
    font.legend = c(14, "bold", "black"),
    linetype = "solid",
    size = 1,
    censor = FALSE,
    break.time.by = 30,
    ylab = "Cumulative Incidence of COVID-19 Post-Vaccine \nBreakthrough Infection, per 1000 Persons",
    xlab = "Time Since Full Vaccination (Days)"
  )


  print(ggsurv)

  return(NULL)
}

km_plot_svi <- function(updated_hr_breakthrough) {
  library(tidyverse)
  library(dplyr)
  library(survminer)
  library(ggplot2)
  library(viridis)
  library(survival)

  palette_low_medium_high <- c(
    "#cc2127", # blue for "low"
    "#815d9a", # orange/tan for "medium"
    "#17a0b0" # red for "high"
    # "#815d9a", # purple for "medium" if the orange text is too faint
    # "#ffc3b8", # pink  17a0b0
  )

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    obesity_before_covid,
    smoking_status,
    CCI_INDEX,
    CCI_CAT,
    MI,
    CHF,
    PVD,
    dementia,
    pulmonary,
    rheumatic,
    PUD,
    liver_combined,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    cancer,
    mets,
    hiv,
    vaccine_manufacturer,
    breakthrough_case,
    Delta,
    pre_vaccine_infection,
    quarter_of_vaccination,
    region,
    rurality,
    sdi_score,
    sdi_categories,
    hesitancy,
    hestiancy_categories,
    days_to_censor,
    county_vaccination_percentage,
    SVI_CTGY,
    SVI_category
  )

  local_df$quarter_of_vaccination <- factor(local_df$quarter_of_vaccination, levels = c("Dec-Feb", "Mar-May", ">May"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$CCI_CAT <- factor(local_df$CCI_CAT, levels = c("0", "1-3", ">3"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))

  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$CCI_CAT <- relevel(as.factor(local_df$CCI_CAT), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$Delta <- relevel(as.factor(local_df$Delta), ref = "Pre-Delta")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_group <- relevel(as.factor(local_df$age_group), ref = "30-49")
  local_df$sdi_categories <- relevel(as.factor(local_df$sdi_categories), ref = "Low")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")

  km_surv_object <- survival::survfit(survival::Surv(days_to_censor, breakthrough_case) ~ SVI_category, data = local_df)
  ggsurv <- ggsurvplot(
    km_surv_object,
    data = local_df,
    risk.table = "nrisk_cumevents",
    fontsize = 3,
    pval = TRUE,
    pval.coord = c(0, 5),
    fun = function(x) {
      (1 - x) * 1000
    },
    ylim = c(0, 40),
    xlim = c(0, 180),
    palette = palette_low_medium_high,
    conf.int = 0.95,
    legend.labs = c("Low", "Medium", "High"),
    font.main = c(18, "bold", "black"),
    font.x = c(16, "bold.italic", "black"),
    font.y = c(16, "bold.italic", "black"),
    font.tickslab = c(14, "plain", "black"),
    font.legend = c(14, "bold", "black"),
    linetype = "solid",
    size = 1,
    censor = FALSE,
    break.time.by = 30,
    ylab = "Cumulative Incidence of COVID-19 Post-Vaccine \nBreakthrough Infection, per 1000 Persons",
    xlab = "Time Since Full Vaccination (Days)"
  )

  # image: svg
  print(ggsurv)

  return(NULL)
}

km_plot_svi1 <- function(updated_hr_breakthrough) {
  library(tidyverse)
  library(dplyr)
  library(survminer)
  library(ggplot2)
  library(viridis)
  library(survival)

  palette_low_medium_high <- c(
    "#cc2127", # blue for "low"
    "#815d9a", # orange/tan for "medium"
    "#17a0b0" # red for "high"
    # "#815d9a", # purple for "medium" if the orange text is too faint
    # "#ffc3b8", # pink  17a0b0
  )

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    obesity_before_covid,
    smoking_status,
    CCI_INDEX,
    CCI_CAT,
    MI,
    CHF,
    PVD,
    dementia,
    pulmonary,
    rheumatic,
    PUD,
    liver_combined,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    cancer,
    mets,
    hiv,
    vaccine_manufacturer,
    breakthrough_case,
    Delta,
    pre_vaccine_infection,
    quarter_of_vaccination,
    region,
    rurality,
    sdi_score,
    sdi_categories,
    hesitancy,
    hestiancy_categories,
    days_to_censor,
    county_vaccination_percentage,
    SVI_CTGY,
    SVI_category
  )

  local_df$quarter_of_vaccination <- factor(local_df$quarter_of_vaccination, levels = c("Dec-Feb", "Mar-May", ">May"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$CCI_CAT <- factor(local_df$CCI_CAT, levels = c("0", "1-3", ">3"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))

  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$CCI_CAT <- relevel(as.factor(local_df$CCI_CAT), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$Delta <- relevel(as.factor(local_df$Delta), ref = "Pre-Delta")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_group <- relevel(as.factor(local_df$age_group), ref = "30-49")
  local_df$sdi_categories <- relevel(as.factor(local_df$sdi_categories), ref = "Low")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")

  km_surv_object <- survival::survfit(survival::Surv(days_to_censor, breakthrough_case) ~ SVI_category, data = local_df)

  ggsurv <- ggsurvplot(
    km_surv_object,
    data = local_df,
    risk.table = "nrisk_cumevents",
    fontsize = 3,
    pval = TRUE,
    pval.coord = c(0, 5),
    fun = function(x) {
      (1 - x) * 1000
    },
    ylim = c(0, 40),
    xlim = c(0, 180),
    palette = palette_low_medium_high,
    conf.int = 0.95,
    legend.labs = c("Low", "Medium", "High"),
    font.main = c(18, "bold", "black"),
    font.x = c(16, "bold.italic", "black"),
    font.y = c(16, "bold.italic", "black"),
    font.tickslab = c(14, "plain", "black"),
    font.legend = c(14, "bold", "black"),
    linetype = "solid",
    size = 1,
    censor = FALSE,
    break.time.by = 30,
    ylab = "Cumulative Incidence of COVID-19 Post-Vaccine \nBreakthrough Infection, per 1000 Persons",
    xlab = "Time Since Full Vaccination (Days)"
  )

  # image: svg
  print(ggsurv)

  return(NULL)
}

km_plot_vaccination_rate <- function(updated_hr_breakthrough) {
  library(tidyverse)
  library(dplyr)
  library(survminer)
  library(ggplot2)
  library(viridis)
  library(survival)

  palette_low_medium_high <- c(
    "#cc2127", # blue for "low"
    "#815d9a", # orange/tan for "medium"
    "#17a0b0" # red for "high"
    # "#815d9a", # purple for "medium" if the orange text is too faint
    # "#ffc3b8", # pink
  )

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    obesity_before_covid,
    smoking_status,
    CCI_INDEX,
    CCI_CAT,
    MI,
    CHF,
    PVD,
    dementia,
    pulmonary,
    rheumatic,
    PUD,
    liver_combined,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    cancer,
    mets,
    hiv,
    vaccine_manufacturer,
    breakthrough_case,
    Delta,
    pre_vaccine_infection,
    quarter_of_vaccination,
    region,
    rurality,
    sdi_score,
    sdi_categories,
    hesitancy,
    hestiancy_categories,
    days_to_censor,
    county_vaccination_percentage
  )

  local_df$quarter_of_vaccination <- factor(local_df$quarter_of_vaccination, levels = c("Dec-Feb", "Mar-May", ">May"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$CCI_CAT <- factor(local_df$CCI_CAT, levels = c("0", "1-3", ">3"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))

  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$CCI_CAT <- relevel(as.factor(local_df$CCI_CAT), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$Delta <- relevel(as.factor(local_df$Delta), ref = "Pre-Delta")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_group <- relevel(as.factor(local_df$age_group), ref = "30-49")
  local_df$sdi_categories <- relevel(as.factor(local_df$sdi_categories), ref = "Low")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")

  km_surv_object <- survival::survfit(survival::Surv(days_to_censor, breakthrough_case) ~ county_vaccination_percentage, data = local_df)

  ggsurv <- ggsurvplot(
    km_surv_object,
    data = local_df,
    risk.table = "nrisk_cumevents",
    fontsize = 3,
    pval = TRUE,
    pval.coord = c(0, 5),
    fun = function(x) {
      (1 - x) * 1000
    },
    ylim = c(0, 40),
    xlim = c(0, 180),
    palette = palette_low_medium_high,
    conf.int = 0.95,
    legend.labs = c("Low", "Medium", "High"),
    font.main = c(18, "bold", "black"),
    font.x = c(16, "bold.italic", "black"),
    font.y = c(16, "bold.italic", "black"),
    font.tickslab = c(14, "plain", "black"),
    font.legend = c(14, "bold", "black"),
    linetype = "solid",
    size = 1,
    censor = FALSE,
    break.time.by = 30,
    ylab = "Cumulative Incidence of COVID-19 Post-Vaccine \nBreakthrough Infection, per 1000 Persons",
    xlab = "Time Since Full Vaccination (Days)"
  )

  # image: svg
  print(ggsurv)

  return(NULL)
}

overall_summary <- function(outcomes_comparison_dataset) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(tidyverse)

  local_df <- outcomes_comparison_dataset %>% dplyr::select(
    covid_positive_test,
    prior_covid_infection,
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    i_rural,
    hesitancy,
    hestiancy_categories,
    # sdi_score,
    # sdi_categories,
    SVI,
    SVI_category,
    Series_Complete_18PlusPop_Pct,
    county_vaccination_percentage,
    period_incidence,
    community_cumulative_incidence,
    region,
    CCI_INDEX,
    comorbditity_cats,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_documented,
    isc_status,
    cohort,
    outcomes
  )

  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))
  local_df$i_rural <- factor(local_df$i_rural, levels = c("Urban", "Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))
  local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels = c("Low", "Medium", "High"))
  local_df$outcomes <- factor(local_df$outcomes, levels = c("adverse", "hospitalized", "non-hospitalized"))
  local_df$cohort <- factor(local_df$cohort, levels = c("vaccinated", "unvaccinated"))

  table1 <- local_df %>%
    tbl_summary(by = cohort) %>%
    add_p()
  print(table1)

  table1 <- as_tibble(table1, col_labels = FALSE)

  print(table1)
  return(table1)
}

rural_only_hr <- function(updated_hr_breakthrough) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(survival)

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    obesity_before_covid,
    smoking_status,
    CCI_INDEX,
    CCI_CAT,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_before_covid,
    isc_status,
    vaccine_manufacturer,
    breakthrough_case,
    Delta,
    pre_vaccine_infection,
    quarter_of_vaccination,
    region,
    rurality,
    sdi_score,
    sdi_categories,
    hesitancy,
    hestiancy_categories,
    days_to_censor,
    i_rural,
    comorbditity_cats
  )

  local_df$quarter_of_vaccination <- factor(local_df$quarter_of_vaccination, levels = c("Dec-Feb", "Mar-May", ">May"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$CCI_CAT <- factor(local_df$CCI_CAT, levels = c("0", "1-3", ">3"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))

  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$CCI_CAT <- relevel(as.factor(local_df$CCI_CAT), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$Delta <- relevel(as.factor(local_df$Delta), ref = "Pre-Delta")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_group <- relevel(as.factor(local_df$age_group), ref = "30-49")
  local_df$sdi_categories <- relevel(as.factor(local_df$sdi_categories), ref = "High")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")

  local_df <- filter(local_df, i_rural == "Rural")

  m1 <- coxph(Surv(days_to_censor, breakthrough_case) ~ age_group + gender_concept_name + race_ethnicity + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_before_covid + isc_status + region + obesity_before_covid + vaccine_manufacturer + Delta + pre_vaccine_infection + hestiancy_categories, data = local_df) %>%
    tbl_regression(exponentiate = TRUE)

  table1 <- as_tibble(m1, col_labels = FALSE)

  return(table1)
}

rural_only_hr_cumulative_incidence <- function(updated_hr_breakthrough) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(survival)

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    obesity_before_covid,
    smoking_status,
    CCI_INDEX,
    CCI_CAT,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_before_covid,
    isc_status,
    vaccine_manufacturer,
    breakthrough_case,
    Delta,
    pre_vaccine_infection,
    quarter_of_vaccination,
    region,
    rurality,
    sdi_score,
    sdi_categories,
    hesitancy,
    hestiancy_categories,
    days_to_censor,
    i_rural,
    county_vaccination_percentage,
    comorbditity_cats,
    community_cumulative_incidence
  )

  local_df$quarter_of_vaccination <- factor(local_df$quarter_of_vaccination, levels = c("Dec-Feb", "Mar-May", ">May"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$CCI_CAT <- factor(local_df$CCI_CAT, levels = c("0", "1-3", ">3"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))
  local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels = c("Low", "Medium", "High"))

  local_df$community_cumulative_incidence <- relevel(as.factor(local_df$community_cumulative_incidence), ref = "Low")
  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$county_vaccination_percentage <- relevel(as.factor(local_df$county_vaccination_percentage), ref = "High")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$CCI_CAT <- relevel(as.factor(local_df$CCI_CAT), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$Delta <- relevel(as.factor(local_df$Delta), ref = "Pre-Delta")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_group <- relevel(as.factor(local_df$age_group), ref = "30-49")
  local_df$sdi_categories <- relevel(as.factor(local_df$sdi_categories), ref = "High")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")
  local_df$i_rural <- relevel(as.factor(local_df$i_rural), ref = "Urban")

  local_df <- filter(local_df, i_rural == "Rural")

  m1 <- coxph(Surv(days_to_censor, breakthrough_case) ~ age_group + gender_concept_name + race_ethnicity + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_before_covid + isc_status + region + obesity_before_covid + vaccine_manufacturer + Delta + pre_vaccine_infection + community_cumulative_incidence, data = local_df) %>%
    tbl_regression(exponentiate = TRUE)

  table1 <- as_tibble(m1, col_labels = FALSE)

  return(table1)
}

rural_only_hr_vaccine_rates <- function(updated_hr_breakthrough) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(survival)

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    obesity_before_covid,
    smoking_status,
    CCI_INDEX,
    CCI_CAT,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_before_covid,
    isc_status,
    vaccine_manufacturer,
    breakthrough_case,
    Delta,
    pre_vaccine_infection,
    quarter_of_vaccination,
    region,
    rurality,
    sdi_score,
    sdi_categories,
    hesitancy,
    hestiancy_categories,
    days_to_censor,
    i_rural,
    county_vaccination_percentage,
    comorbditity_cats
  )

  local_df$quarter_of_vaccination <- factor(local_df$quarter_of_vaccination, levels = c("Dec-Feb", "Mar-May", ">May"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$CCI_CAT <- factor(local_df$CCI_CAT, levels = c("0", "1-3", ">3"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))

  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$county_vaccination_percentage <- relevel(as.factor(local_df$county_vaccination_percentage), ref = "High")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$CCI_CAT <- relevel(as.factor(local_df$CCI_CAT), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$Delta <- relevel(as.factor(local_df$Delta), ref = "Pre-Delta")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_group <- relevel(as.factor(local_df$age_group), ref = "30-49")
  local_df$sdi_categories <- relevel(as.factor(local_df$sdi_categories), ref = "High")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")
  local_df$i_rural <- relevel(as.factor(local_df$i_rural), ref = "Urban")

  local_df <- filter(local_df, i_rural == "Rural")

  m1 <- coxph(Surv(days_to_censor, breakthrough_case) ~ age_group + gender_concept_name + race_ethnicity + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_before_covid + isc_status + region + obesity_before_covid + vaccine_manufacturer + Delta + pre_vaccine_infection + county_vaccination_percentage, data = local_df) %>%
    tbl_regression(exponentiate = TRUE)

  table1 <- as_tibble(m1, col_labels = FALSE)

  return(table1)
}

summary_unvaccinated_hesitancy <- function(unvaccinated_for_hr) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(tidyverse)

  local_df <- unvaccinated_for_hr %>% dplyr::select(
    covid_positive_test,
    prior_covid_infection,
    age,
    age_category,
    gender_concept_name,
    race_ethnicity,
    i_rural,
    hesitancy,
    hestiancy_categories,
    # sdi_score,
    # sdi_categories,
    SVI,
    SVI_category,
    Series_Complete_18PlusPop_Pct,
    county_vaccination_percentage,
    period_incidence,
    community_cumulative_incidence,
    region,
    CCI_INDEX,
    comorbditity_cats,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_documented,
    isc_status
  )

  local_df$age_category <- factor(local_df$age_category, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))
  local_df$i_rural <- factor(local_df$i_rural, levels = c("Urban", "Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))
  local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels = c("Low", "Medium", "High"))

  table1 <- local_df %>%
    tbl_summary(by = hestiancy_categories) %>%
    add_p()
  print(table1)

  table1 <- as_tibble(table1, col_labels = FALSE)

  print(table1)
  return(table1)
}

summary_unvaccinated_irural <- function(unvaccinated_for_hr) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(tidyverse)

  local_df <- unvaccinated_for_hr %>% dplyr::select(
    covid_positive_test,
    prior_covid_infection,
    age,
    age_category,
    gender_concept_name,
    race_ethnicity,
    i_rural,
    hesitancy,
    hestiancy_categories,
    # sdi_score,
    # sdi_categories,
    SVI,
    SVI_category,
    Series_Complete_18PlusPop_Pct,
    county_vaccination_percentage,
    period_incidence,
    community_cumulative_incidence,
    region,
    CCI_INDEX,
    comorbditity_cats,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_documented,
    isc_status
  )

  local_df$age_category <- factor(local_df$age_category, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))
  local_df$i_rural <- factor(local_df$i_rural, levels = c("Urban", "Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))
  local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels = c("Low", "Medium", "High"))

  table1 <- local_df %>%
    tbl_summary(by = i_rural) %>%
    add_p()
  print(table1)

  table1 <- as_tibble(table1, col_labels = FALSE)

  print(table1)
  return(table1)
}

summary_unvaccinated_overall <- function(unvaccinated_for_hr) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(tidyverse)

  local_df <- unvaccinated_for_hr %>% dplyr::select(
    covid_positive_test,
    prior_covid_infection,
    age,
    age_category,
    gender_concept_name,
    race_ethnicity,
    i_rural,
    hesitancy,
    hestiancy_categories,
    # sdi_score,
    # sdi_categories,
    SVI,
    SVI_category,
    Series_Complete_18PlusPop_Pct,
    county_vaccination_percentage,
    period_incidence,
    community_cumulative_incidence,
    region,
    CCI_INDEX,
    comorbditity_cats,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_documented,
    isc_status
  )

  local_df$age_category <- factor(local_df$age_category, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))
  local_df$i_rural <- factor(local_df$i_rural, levels = c("Urban", "Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))
  local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels = c("Low", "Medium", "High"))

  table1 <- local_df %>% tbl_summary()
  print(table1)

  table1 <- as_tibble(table1, col_labels = FALSE)

  print(table1)
  return(table1)
}

table1_hesitancy <- function(updated_hr_breakthrough) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(tidyverse)

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    breakthrough_case,
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    i_rural,
    hesitancy,
    hestiancy_categories,
    # sdi_score,
    # sdi_categories,
    SVI,
    SVI_category,
    Series_Complete_18PlusPop_Pct,
    county_vaccination_percentage,
    period_incidence,
    community_cumulative_incidence,
    region,
    CCI_INDEX,
    comorbditity_cats,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_before_covid,
    isc_status,
    vaccine_manufacturer,
    month_of_full_vaccination
  )

  local_df$month_of_full_vaccination <- factor(local_df$month_of_full_vaccination, levels = c("Jan 2021", "Feb 2021", "Mar 2021", "Apr 2021", "May 2021", "Jun 2021", "Jul 2021", "Aug 2021", "Sep 2021"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
  local_df$i_rural <- factor(local_df$i_rural, levels = c("Urban", "Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))
  local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels = c("Low", "Medium", "High"))

  table1 <- local_df %>%
    tbl_summary(by = hestiancy_categories) %>%
    add_p()
  print(table1)

  table1 <- as_tibble(table1, col_labels = FALSE)

  print(table1)
  return(table1)
}

table1_overall <- function(updated_hr_breakthrough) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(tidyverse)

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    breakthrough_case,
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    i_rural,
    hesitancy,
    hestiancy_categories,
    hesitancy_sensitivity_category,
    # sdi_score,
    # sdi_categories,
    SVI,
    SVI_category,
    Series_Complete_18PlusPop_Pct,
    county_vaccination_percentage,
    period_incidence,
    community_cumulative_incidence,
    region,
    CCI_INDEX,
    comorbditity_cats,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_before_covid,
    isc_status,
    vaccine_manufacturer,
    month_of_full_vaccination
  )

  local_df$month_of_full_vaccination <- factor(local_df$month_of_full_vaccination, levels = c("Jan 2021", "Feb 2021", "Mar 2021", "Apr 2021", "May 2021", "Jun 2021", "Jul 2021", "Aug 2021", "Sep 2021"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
  local_df$i_rural <- factor(local_df$i_rural, levels = c("Urban", "Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))
  local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels = c("Low", "Medium", "High"))

  table1 <- local_df %>% tbl_summary()
  print(table1)

  table1 <- as_tibble(table1, col_labels = FALSE)

  print(table1)
  return(table1)
}

table1_rural <- function(updated_hr_breakthrough) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(tidyverse)

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    breakthrough_case,
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    i_rural,
    hesitancy,
    hestiancy_categories,
    # sdi_score,
    # sdi_categories,
    SVI,
    SVI_category,
    Series_Complete_18PlusPop_Pct,
    county_vaccination_percentage,
    period_incidence,
    community_cumulative_incidence,
    region,
    CCI_INDEX,
    comorbditity_cats,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_before_covid,
    isc_status,
    vaccine_manufacturer,
    month_of_full_vaccination
  )

  local_df$month_of_full_vaccination <- factor(local_df$month_of_full_vaccination, levels = c("Jan 2021", "Feb 2021", "Mar 2021", "Apr 2021", "May 2021", "Jun 2021", "Jul 2021", "Aug 2021", "Sep 2021"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
  local_df$i_rural <- factor(local_df$i_rural, levels = c("Urban", "Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))
  local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels = c("Low", "Medium", "High"))

  table1 <- local_df %>%
    tbl_summary(by = i_rural) %>%
    add_p()
  print(table1)

  table1 <- as_tibble(table1, col_labels = FALSE)

  print(table1)
  return(table1)
}

table1_rurality <- function(updated_hr_breakthrough) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(tidyverse)

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    breakthrough_case,
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    rurality,
    hesitancy,
    hestiancy_categories,
    # sdi_score,
    # sdi_categories,
    SVI,
    SVI_category,
    Series_Complete_18PlusPop_Pct,
    county_vaccination_percentage,
    period_incidence,
    community_cumulative_incidence,
    region,
    CCI_INDEX,
    comorbditity_cats,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_before_covid,
    isc_status,
    vaccine_manufacturer,
    month_of_full_vaccination
  )

  local_df$month_of_full_vaccination <- factor(local_df$month_of_full_vaccination, levels = c("Jan 2021", "Feb 2021", "Mar 2021", "Apr 2021", "May 2021", "Jun 2021", "Jul 2021", "Aug 2021", "Sep 2021"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))
  local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels = c("Low", "Medium", "High"))

  table1 <- local_df %>%
    tbl_summary(by = rurality) %>%
    add_p()
  print(table1)

  table1 <- as_tibble(table1, col_labels = FALSE)

  print(table1)
  return(table1)
}

test_adjusted_hr_community_incidence <- function(unvaccinated_for_hr) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(tidyverse)
  library(survival)

  local_df <- unvaccinated_for_hr %>% dplyr::select(
    covid_positive_test,
    rurality,
    hestiancy_categories,
    county_vaccination_percentage,
    SVI_category,
    age_category,
    gender_concept_name,
    comorbditity_cats,
    region,
    prior_covid_infection,
    obesity_documented,
    race_ethnicity,
    time_to_covid_or_censor,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_documented,
    isc_status,
    community_cumulative_incidence
  )
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))
  local_df$age_category <- factor(local_df$age_category, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West", "Missing"))
  local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels = c("Low", "Medium", "High"))

  local_df$community_cumulative_incidence <- relevel(as.factor(local_df$community_cumulative_incidence), ref = "Low")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$county_vaccination_percentage <- relevel(as.factor(local_df$county_vaccination_percentage), ref = "High")
  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$SVI_category <- relevel(as.factor(local_df$SVI_category), ref = "Low")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_category <- relevel(as.factor(local_df$age_category), ref = "30-49")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")

  m1 <- coxph(Surv(time_to_covid_or_censor, covid_positive_test) ~ age_category + gender_concept_name + race_ethnicity + prior_covid_infection + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_documented + obesity_documented + isc_status + region + community_cumulative_incidence, data = local_df)

  table1 <- m1 %>%
    tbl_regression(exponentiate = TRUE)
  table1 <- as_tibble(table1, col_labels = FALSE)
  return(table1)
}

test_adjusted_hr_multivariate_all <- function(unvaccinated_for_hr) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(tidyverse)
  library(survival)

  local_df <- unvaccinated_for_hr %>% dplyr::select(
    covid_positive_test,
    rurality,
    hestiancy_categories,
    county_vaccination_percentage,
    SVI_category,
    age_category,
    gender_concept_name,
    comorbditity_cats,
    region,
    prior_covid_infection,
    obesity_documented,
    race_ethnicity,
    time_to_covid_or_censor,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_documented,
    isc_status,
    community_cumulative_incidence
  )
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))
  local_df$age_category <- factor(local_df$age_category, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West", "Missing"))
  local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels = c("Low", "Medium", "High"))

  local_df$community_cumulative_incidence <- relevel(as.factor(local_df$community_cumulative_incidence), ref = "Low")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$county_vaccination_percentage <- relevel(as.factor(local_df$county_vaccination_percentage), ref = "High")
  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$SVI_category <- relevel(as.factor(local_df$SVI_category), ref = "Low")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_category <- relevel(as.factor(local_df$age_category), ref = "30-49")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")

  m1 <- coxph(Surv(time_to_covid_or_censor, covid_positive_test) ~ age_category + gender_concept_name + race_ethnicity + prior_covid_infection + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_documented + obesity_documented + isc_status + region + SVI_category + county_vaccination_percentage + community_cumulative_incidence + hestiancy_categories + rurality, data = local_df)

  table1 <- m1 %>%
    tbl_regression(exponentiate = TRUE)
  table1 <- as_tibble(table1, col_labels = FALSE)
  return(table1)
}

test_adjusted_hr_rates <- function(unvaccinated_for_hr) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(tidyverse)
  library(survival)

  local_df <- unvaccinated_for_hr %>% dplyr::select(
    covid_positive_test,
    rurality,
    hestiancy_categories,
    county_vaccination_percentage,
    SVI_category,
    age_category,
    gender_concept_name,
    comorbditity_cats,
    region,
    prior_covid_infection,
    obesity_documented,
    race_ethnicity,
    delta_flag,
    time_to_covid_or_censor,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_documented,
    isc_status,
    community_cumulative_incidence
  )

  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))
  local_df$county_vaccination_percentage <- relevel(as.factor(local_df$county_vaccination_percentage), ref = "High")
  local_df$age_category <- factor(local_df$age_category, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West", "Missing"))
  local_df$delta_flag <- relevel(as.factor(local_df$delta_flag), ref = "pre-delta")
  local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels = c("Low", "Medium", "High"))

  local_df$community_cumulative_incidence <- relevel(as.factor(local_df$community_cumulative_incidence), ref = "Low")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$county_vaccination_percentage <- relevel(as.factor(local_df$county_vaccination_percentage), ref = "High")
  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_category <- relevel(as.factor(local_df$age_category), ref = "30-49")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")

  m1 <- coxph(Surv(time_to_covid_or_censor, covid_positive_test) ~ age_category + gender_concept_name + race_ethnicity + prior_covid_infection + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_documented + obesity_documented + isc_status + region + county_vaccination_percentage, data = local_df)

  table1 <- m1 %>%
    tbl_regression(exponentiate = TRUE)
  table1 <- as_tibble(table1, col_labels = FALSE)
  return(table1)
}

test_adjusted_hr_rural <- function(unvaccinated_for_hr) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(tidyverse)
  library(survival)

  local_df <- unvaccinated_for_hr %>% dplyr::select(
    covid_positive_test,
    rurality,
    hestiancy_categories,
    county_vaccination_percentage,
    SVI_category,
    age_category,
    gender_concept_name,
    comorbditity_cats,
    region,
    prior_covid_infection,
    race_ethnicity,
    i_rural,
    obesity_documented,
    delta_flag,
    time_to_covid_or_censor,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_documented,
    isc_status,
    community_cumulative_incidence
  )
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))
  local_df$county_vaccination_percentage <- relevel(as.factor(local_df$county_vaccination_percentage), ref = "High")
  local_df$age_category <- factor(local_df$age_category, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West", "Missing"))
  local_df$i_rural <- factor(local_df$i_rural, levels = c("Urban", "Rural"))
  local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels = c("Low", "Medium", "High"))

  local_df$community_cumulative_incidence <- relevel(as.factor(local_df$community_cumulative_incidence), ref = "Low")
  local_df$i_rural <- relevel(as.factor(local_df$i_rural), ref = "Urban")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$county_vaccination_percentage <- relevel(as.factor(local_df$county_vaccination_percentage), ref = "High")
  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$SVI_category <- relevel(as.factor(local_df$SVI_category), ref = "Low")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_category <- relevel(as.factor(local_df$age_category), ref = "30-49")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")

  m1 <- coxph(Surv(time_to_covid_or_censor, covid_positive_test) ~ age_category + gender_concept_name + race_ethnicity + prior_covid_infection + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_documented + obesity_documented + isc_status + region + rurality, data = local_df)

  table1 <- m1 %>%
    tbl_regression(exponentiate = TRUE)
  table1 <- as_tibble(table1, col_labels = FALSE)
  return(table1)
}

test_adjusted_hr_svi <- function(unvaccinated_for_hr) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(tidyverse)
  library(survival)

  local_df <- unvaccinated_for_hr %>% dplyr::select(
    covid_positive_test,
    rurality,
    hestiancy_categories,
    county_vaccination_percentage,
    SVI_category,
    age_category,
    gender_concept_name,
    comorbditity_cats,
    region,
    prior_covid_infection,
    obesity_documented,
    race_ethnicity,
    time_to_covid_or_censor,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_documented,
    isc_status,
    community_cumulative_incidence
  )
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))
  local_df$age_category <- factor(local_df$age_category, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West", "Missing"))
  local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels = c("Low", "Medium", "High"))

  local_df$community_cumulative_incidence <- relevel(as.factor(local_df$community_cumulative_incidence), ref = "Low")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$county_vaccination_percentage <- relevel(as.factor(local_df$county_vaccination_percentage), ref = "High")
  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$SVI_category <- relevel(as.factor(local_df$SVI_category), ref = "Low")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_category <- relevel(as.factor(local_df$age_category), ref = "30-49")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")

  m1 <- coxph(Surv(time_to_covid_or_censor, covid_positive_test) ~ age_category + gender_concept_name + obesity_documented + prior_covid_infection + race_ethnicity + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_documented + obesity_documented + isc_status + region + SVI_category, data = local_df)

  table1 <- m1 %>%
    tbl_regression(exponentiate = TRUE)
  table1 <- as_tibble(table1, col_labels = FALSE)
  return(table1)
}

test_adjusted_hr_univariate_all <- function(unvaccinated_for_hr) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(tidyverse)
  library(survival)

  local_df <- unvaccinated_for_hr %>% dplyr::select(
    covid_positive_test,
    rurality,
    hestiancy_categories,
    county_vaccination_percentage,
    SVI_category,
    age_category,
    gender_concept_name,
    comorbditity_cats,
    region,
    prior_covid_infection,
    obesity_documented,
    race_ethnicity,
    time_to_covid_or_censor,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_documented,
    isc_status,
    community_cumulative_incidence
  )
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))
  local_df$age_category <- factor(local_df$age_category, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West", "Missing"))
  local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels = c("Low", "Medium", "High"))

  local_df$community_cumulative_incidence <- relevel(as.factor(local_df$community_cumulative_incidence), ref = "Low")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$county_vaccination_percentage <- relevel(as.factor(local_df$county_vaccination_percentage), ref = "High")
  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$SVI_category <- relevel(as.factor(local_df$SVI_category), ref = "Low")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_category <- relevel(as.factor(local_df$age_category), ref = "30-49")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")

  m1 <- local_df %>%
    select(time_to_covid_or_censor, covid_positive_test, age_category, gender_concept_name, race_ethnicity, heart_disease, PVD, PUD, liver_disease, diabetes_combined, paralysis, stroke, renal, any_cancer, obesity_documented, isc_status, region, prior_covid_infection, SVI_category, county_vaccination_percentage, community_cumulative_incidence, hestiancy_categories, rurality) %>%
    tbl_uvregression(
      method = coxph,
      y = Surv(time_to_covid_or_censor, covid_positive_test),
      exponentiate = TRUE,
      hide_n = TRUE
    )

  print(m1)

  table1 <- as_tibble(m1, col_labels = FALSE)

  return(table1)
}

test_ajusted_hr_hesitancy <- function(unvaccinated_for_hr) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(tidyverse)
  library(survival)

  local_df <- unvaccinated_for_hr %>% dplyr::select(
    covid_positive_test,
    rurality,
    hestiancy_categories,
    county_vaccination_percentage,
    SVI_category,
    age_category,
    gender_concept_name,
    comorbditity_cats,
    region,
    prior_covid_infection,
    race_ethnicity,
    obesity_documented,
    time_to_covid_or_censor,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_documented,
    isc_status,
    community_cumulative_incidence
  )
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))
  local_df$county_vaccination_percentage <- relevel(as.factor(local_df$county_vaccination_percentage), ref = "High")
  local_df$age_category <- factor(local_df$age_category, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West", "Missing"))
  local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels = c("Low", "Medium", "High"))

  local_df$community_cumulative_incidence <- relevel(as.factor(local_df$community_cumulative_incidence), ref = "Low")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$county_vaccination_percentage <- relevel(as.factor(local_df$county_vaccination_percentage), ref = "High")
  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$SVI_category <- relevel(as.factor(local_df$SVI_category), ref = "Low")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_category <- relevel(as.factor(local_df$age_category), ref = "30-49")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")

  m1 <- coxph(Surv(time_to_covid_or_censor, covid_positive_test) ~ age_category + gender_concept_name + race_ethnicity + prior_covid_infection + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_documented + obesity_documented + isc_status + region + hestiancy_categories, data = local_df)

  table1 <- m1 %>%
    tbl_regression(exponentiate = TRUE)
  table1 <- as_tibble(table1, col_labels = FALSE)
  return(table1)
}

test_crude_hr_irural <- function(unvaccinated_for_hr) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(tidyverse)
  library(survival)

  local_df <- unvaccinated_for_hr %>% dplyr::select(
    covid_positive_test,
    rurality,
    hestiancy_categories,
    county_vaccination_percentage,
    SVI_category,
    age_category,
    gender_concept_name,
    comorbditity_cats,
    region,
    prior_covid_infection,
    race_ethnicity,
    i_rural,
    obesity_documented,
    delta_flag,
    time_to_covid_or_censor,
    tp,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_documented,
    isc_status,
    community_cumulative_incidence
  )
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$SVI_category <- factor(local_df$SVI_category, levels = c("Low", "Medium", "High"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))
  local_df$county_vaccination_percentage <- relevel(as.factor(local_df$county_vaccination_percentage), ref = "High")
  local_df$age_category <- factor(local_df$age_category, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West", "Missing"))
  local_df$i_rural <- factor(local_df$i_rural, levels = c("Urban", "Rural"))
  local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels = c("Low", "Medium", "High"))

  local_df$community_cumulative_incidence <- relevel(as.factor(local_df$community_cumulative_incidence), ref = "Low")
  local_df$i_rural <- relevel(as.factor(local_df$i_rural), ref = "Urban")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$county_vaccination_percentage <- relevel(as.factor(local_df$county_vaccination_percentage), ref = "High")
  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$SVI_category <- relevel(as.factor(local_df$SVI_category), ref = "Low")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_category <- relevel(as.factor(local_df$age_category), ref = "30-49")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")

  m1 <- coxph(Surv(time_to_covid_or_censor, covid_positive_test) ~ age_category + gender_concept_name + race_ethnicity + prior_covid_infection + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_documented + obesity_documented + isc_status + region + i_rural, data = local_df)

  table1 <- m1 %>%
    tbl_regression(exponentiate = TRUE)
  table1 <- as_tibble(table1, col_labels = FALSE)
  return(table1)
}

urban_only_hr <- function(updated_hr_breakthrough) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(survival)

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    obesity_before_covid,
    smoking_status,
    CCI_INDEX,
    CCI_CAT,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_before_covid,
    isc_status,
    vaccine_manufacturer,
    breakthrough_case,
    Delta,
    pre_vaccine_infection,
    quarter_of_vaccination,
    region,
    rurality,
    sdi_score,
    sdi_categories,
    hesitancy,
    hestiancy_categories,
    days_to_censor,
    i_rural,
    comorbditity_cats
  )

  local_df$quarter_of_vaccination <- factor(local_df$quarter_of_vaccination, levels = c("Dec-Feb", "Mar-May", ">May"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$CCI_CAT <- factor(local_df$CCI_CAT, levels = c("0", "1-3", ">3"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))

  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$CCI_CAT <- relevel(as.factor(local_df$CCI_CAT), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$Delta <- relevel(as.factor(local_df$Delta), ref = "Pre-Delta")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_group <- relevel(as.factor(local_df$age_group), ref = "30-49")
  local_df$sdi_categories <- relevel(as.factor(local_df$sdi_categories), ref = "High")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")

  local_df <- filter(local_df, i_rural == "Urban")

  m1 <- coxph(Surv(days_to_censor, breakthrough_case) ~ age_group + gender_concept_name + race_ethnicity + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_before_covid + isc_status + region + obesity_before_covid + vaccine_manufacturer + Delta + pre_vaccine_infection + hestiancy_categories, data = local_df) %>%
    tbl_regression(exponentiate = TRUE)

  table1 <- as_tibble(m1, col_labels = FALSE)

  return(table1)
}

urban_only_hr_cumulative_incidence <- function(updated_hr_breakthrough) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(survival)

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    obesity_before_covid,
    smoking_status,
    CCI_INDEX,
    CCI_CAT,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_before_covid,
    isc_status,
    vaccine_manufacturer,
    breakthrough_case,
    Delta,
    pre_vaccine_infection,
    quarter_of_vaccination,
    region,
    rurality,
    sdi_score,
    sdi_categories,
    hesitancy,
    hestiancy_categories,
    days_to_censor,
    i_rural,
    comorbditity_cats,
    community_cumulative_incidence
  )

  local_df$quarter_of_vaccination <- factor(local_df$quarter_of_vaccination, levels = c("Dec-Feb", "Mar-May", ">May"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$CCI_CAT <- factor(local_df$CCI_CAT, levels = c("0", "1-3", ">3"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))
  local_df$community_cumulative_incidence <- factor(local_df$community_cumulative_incidence, levels = c("Low", "Medium", "High"))

  local_df$community_cumulative_incidence <- relevel(as.factor(local_df$community_cumulative_incidence), ref = "Low")
  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$CCI_CAT <- relevel(as.factor(local_df$CCI_CAT), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$Delta <- relevel(as.factor(local_df$Delta), ref = "Pre-Delta")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_group <- relevel(as.factor(local_df$age_group), ref = "30-49")
  local_df$sdi_categories <- relevel(as.factor(local_df$sdi_categories), ref = "High")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")

  local_df <- filter(local_df, i_rural == "Urban")

  m1 <- coxph(Surv(days_to_censor, breakthrough_case) ~ age_group + gender_concept_name + race_ethnicity + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_before_covid + isc_status + region + obesity_before_covid + vaccine_manufacturer + Delta + pre_vaccine_infection + community_cumulative_incidence, data = local_df) %>%
    tbl_regression(exponentiate = TRUE)

  table1 <- as_tibble(m1, col_labels = FALSE)

  return(table1)
}

urban_only_hr_vaccine_rates <- function(updated_hr_breakthrough) {
  library(gtsummary)
  library(rvest)
  library(dplyr)
  library(survival)

  local_df <- updated_hr_breakthrough %>% dplyr::select(
    age,
    age_group,
    gender_concept_name,
    race_ethnicity,
    obesity_before_covid,
    smoking_status,
    CCI_INDEX,
    CCI_CAT,
    heart_disease,
    PVD,
    PUD,
    liver_disease,
    diabetes_combined,
    paralysis,
    stroke,
    renal,
    any_cancer,
    obesity_before_covid,
    isc_status,
    vaccine_manufacturer,
    breakthrough_case,
    Delta,
    pre_vaccine_infection,
    quarter_of_vaccination,
    region,
    rurality,
    sdi_score,
    sdi_categories,
    hesitancy,
    hestiancy_categories,
    days_to_censor,
    i_rural,
    county_vaccination_percentage,
    comorbditity_cats
  )

  local_df$quarter_of_vaccination <- factor(local_df$quarter_of_vaccination, levels = c("Dec-Feb", "Mar-May", ">May"))
  local_df$age_group <- factor(local_df$age_group, levels = c("<30", "30-49", "50-64", "65-75", ">75"))
  local_df$gender_concept_name <- factor(local_df$gender_concept_name, levels = c("FEMALE", "MALE"))
  local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels = c("non-Hispanic White", "non-Hispanic Black", "Hispanic", "Asian American/Pacific Islander", "Other", "Missing Race/Ethnicity"))
  local_df$CCI_CAT <- factor(local_df$CCI_CAT, levels = c("0", "1-3", ">3"))
  local_df$vaccine_manufacturer <- factor(local_df$vaccine_manufacturer, levels = c("PFIZER_BIONTECH", "MODERNA", "JANSSEN"))
  local_df$rurality <- factor(local_df$rurality, levels = c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
  local_df$region <- factor(local_df$region, levels = c("Northeast", "Midwest", "South", "West"))
  local_df$hestiancy_categories <- factor(local_df$hestiancy_categories, levels = c("Low", "Medium", "High"))
  local_df$county_vaccination_percentage <- factor(local_df$county_vaccination_percentage, levels = c("Low", "Medium", "High"))
  local_df$comorbditity_cats <- factor(local_df$comorbditity_cats, levels = c("0", "1", "2", ">=3"))

  local_df$comorbditity_cats <- relevel(as.factor(local_df$comorbditity_cats), ref = "0")
  local_df$county_vaccination_percentage <- relevel(as.factor(local_df$county_vaccination_percentage), ref = "High")
  local_df$rurality <- relevel(as.factor(local_df$rurality), ref = "Urban")
  local_df$CCI_CAT <- relevel(as.factor(local_df$CCI_CAT), ref = "0")
  local_df$hestiancy_categories <- relevel(as.factor(local_df$hestiancy_categories), ref = "Low")
  local_df$Delta <- relevel(as.factor(local_df$Delta), ref = "Pre-Delta")
  local_df$region <- relevel(as.factor(local_df$region), ref = "Northeast")
  local_df$gender_concept_name <- relevel(as.factor(local_df$gender_concept_name), ref = "FEMALE")
  local_df$age_group <- relevel(as.factor(local_df$age_group), ref = "30-49")
  local_df$sdi_categories <- relevel(as.factor(local_df$sdi_categories), ref = "High")
  local_df$race_ethnicity <- relevel(as.factor(local_df$race_ethnicity), ref = "non-Hispanic White")
  local_df$i_rural <- relevel(as.factor(local_df$i_rural), ref = "Urban")

  local_df <- filter(local_df, i_rural == "Urban")

  m1 <- coxph(Surv(days_to_censor, breakthrough_case) ~ age_group + gender_concept_name + race_ethnicity + heart_disease + PVD + PUD + liver_disease + diabetes_combined + paralysis + stroke + renal + any_cancer + obesity_before_covid + isc_status + region + obesity_before_covid + vaccine_manufacturer + Delta + pre_vaccine_infection + county_vaccination_percentage, data = local_df) %>%
    tbl_regression(exponentiate = TRUE)

  table1 <- as_tibble(m1, col_labels = FALSE)

  return(table1)
}

