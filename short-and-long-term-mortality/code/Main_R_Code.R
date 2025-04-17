library(survival)
library(ggplot2)
library(dplyr)
library(ggsurvfit)
library(gtsummary)
library(dplyr)
library(ggsci)
library(twang)
library(cobalt)
library(nnet)
library(tidyr)

#### Set Factor Levels ####

# Sex
local_df$sex <- factor(local_df$sex, levels=c("Female", "Male"))

# Age Group
local_df$age_group <- factor(local_df$age_group, levels=c("<30", "30-49", "50-64", "65-74", "75+"))
local_df$age_group=relevel(as.factor(local_df$age_group),ref="30-49")

# SVI_category
local_df$SVI_category <- factor(local_df$SVI_category, levels=c("Low", "Medium", "High"))
local_df$SVI_category=relevel(as.factor(local_df$SVI_category),ref="Medium")

# Race/Ethnicity
local_df$race_ethnicity <- factor(local_df$race_ethnicity, levels=c("White Non-Hispanic", "Black or African American Non-Hispanic",
                                                                    "Hispanic or Latino Any Race", "Other", "Unknown"))

# Variant Period
local_df$variant_period <- factor(local_df$variant_period, levels=c("Ancestral COVID-19", "Alpha (B.1.1.7), Beta (B.1.351), Gamma (P.1)", "Delta (B.1.617.2)", "Omicron (B.1.1.529, BA.2, BA.2.12.1)"))
local_df$variant_period=relevel(as.factor(local_df$variant_period),ref="Alpha (B.1.1.7), Beta (B.1.351), Gamma (P.1)")

# Vaccination Status
local_df$vaccination_status <- factor(local_df$vaccination_status, levels=c( "Non-Breakthrough Infection", "VAX2 Breakthrough Infection", "VAX3 Breakthrough Infection"))

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

# MLD_LIVER
local_df$MLD_LIVER <- factor(local_df$MLD_LIVER, levels=c("0","1"))
levels(local_df$MLD_LIVER)[levels(local_df$MLD_LIVER)=="0"] <- "No Hx of MLD_LIVER"
levels(local_df$MLD_LIVER)[levels(local_df$MLD_LIVER)=="1"] <- "Hx of MLD_LIVER"
local_df$MLD_LIVER=relevel(as.factor(local_df$MLD_LIVER),ref="No Hx of MLD_LIVER")

# MOD_SEV_LIVER
local_df$MOD_SEV_LIVER <- factor(local_df$MOD_SEV_LIVER, levels=c("0","1"))
levels(local_df$MOD_SEV_LIVER)[levels(local_df$MOD_SEV_LIVER)=="0"] <- "No Hx of MOD_SEV_LIVER"
levels(local_df$MOD_SEV_LIVER)[levels(local_df$MOD_SEV_LIVER)=="1"] <- "Hx of MOD_SEV_LIVER"
local_df$MOD_SEV_LIVER=relevel(as.factor(local_df$MOD_SEV_LIVER),ref="No Hx of MOD_SEV_LIVER")

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

# METS
local_df$METS <- factor(local_df$METS, levels=c("0","1"))
levels(local_df$METS)[levels(local_df$METS)=="0"] <- "No Hx of METS"
levels(local_df$METS)[levels(local_df$METS)=="1"] <- "Hx of METS"
local_df$METS=relevel(as.factor(local_df$METS),ref="No Hx of METS")

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

# DIA_UNCOMP
local_df$DIA_UNCOMP <- factor(local_df$DIA_UNCOMP, levels=c("0","1"))
levels(local_df$DIA_UNCOMP)[levels(local_df$DIA_UNCOMP)=="0"] <- "No Hx of DIA_UNCOMP"
levels(local_df$DIA_UNCOMP)[levels(local_df$DIA_UNCOMP)=="1"] <- "Hx of DIA_UNCOMP"
local_df$DIA_UNCOMP=relevel(as.factor(local_df$DIA_UNCOMP),ref="No Hx of DIA_UNCOMP")

# DIA_COMP
local_df$DIA_COMP <- factor(local_df$DIA_COMP, levels=c("0","1"))
levels(local_df$DIA_COMP)[levels(local_df$DIA_COMP)=="0"] <- "No Hx of DIA_COMP"
levels(local_df$DIA_COMP)[levels(local_df$DIA_COMP)=="1"] <- "Hx of DIA_COMP"
local_df$DIA_COMP=relevel(as.factor(local_df$DIA_COMP),ref="No Hx of DIA_COMP")

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

#Region
local_df$region <- factor(local_df$region, levels=c("Northeast", "South", "Midwest", "West"))
local_df$region=relevel(as.factor(local_df$region),ref="Midwest")

#Subregion
local_df$subregion <- factor(local_df$subregion, levels=c("East North Central", "East South Central", "Middle Atlantic", "Mountain", "New England", "Pacific", "South Atlantic", "West North Central", "West South Central"))
local_df$subregion=relevel(as.factor(local_df$subregion),ref="East North Central")

#Rurality
local_df$rurality <- factor(local_df$rurality, levels=c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))
local_df$rurality=relevel(as.factor(local_df$rurality),ref="Urban")

# rural_binary
local_df$rural_binary <- factor(local_df$rural_binary, levels=c("0","1"))
levels(local_df$rural_binary)[levels(local_df$rural_binary)=="0"] <- "Urban"
levels(local_df$rural_binary)[levels(local_df$rural_binary)=="1"] <- "Rural"
local_df$rural_binary=relevel(as.factor(local_df$rural_binary),ref="Urban")

#### Main Descriptive Statistics ####
local_df <- from_rdata(local)

local_df <- dplyr::select(input_dataset_filtered,
                          sex,
                          age_at_covid,
                          age_group,
                          race_ethnicity,
                          variant_period,
                          vaccination_status,
                          SVI,
                          SVI_category,
                          OBESITY,
                          HYPERTENSION,
                          MI,
                          CHF,
                          PVD,
                          CVD,
                          RD,
                          PULMONARY,
                          PUD,
                          HEMI_PARA,
                          diabetes,
                          DEMENTIA,
                          liver,
                          RENAL,
                          cancer,
                          HIV,
                          TOBACCO,
                          SUBSTANCE,
                          subregion,
                          rural_binary,
                          death,
                          time_to_death
)

# Add new variables indicating death within specific time frames
local_df <- local_df %>%
  mutate(
    death_within_30_days = ifelse(time_to_death <= 30 & death == 1, 1, 0),  # Death within 30 days
    death_within_91_days = ifelse(time_to_death <= 91 & death == 1, 1, 0),  # Death within 91 days
    death_within_365_days = ifelse(time_to_death <= 365 & death == 1, 1, 0),  # Death within 365 days
    death_within_730_days = ifelse(time_to_death <= 730 & death == 1, 1, 0)  # Death within 730 days
  )

local_df <- local_df %>%
  select(-death)

table1 <- local_df %>% tbl_summary(by=rural_binary)%>% add_overall() %>% add_p()
print(table1)

#### Cumulative Incidence Plot by Rurality ####
local_df <- from_rdata(local)

km_surv_object=survfit2(Surv(time_to_death, death)~rurality,data=local_df) %>%
  ggsurvfit(type = "risk", linewidth = 0.8) +
  add_confidence_interval() +
  scale_x_continuous(n.breaks = 8) +
  add_risktable(
    risktable_stats = "{n.risk} ({cum.event})"
  )

km_surv_object2 <-
  km_surv_object +
  coord_cartesian(xlim = c(0, 775)) +
  scale_y_continuous(
    limits = c(0.0, 0.05),
    labels = scales::percent,
    expand = c(0.01, 0)
  ) +
  scale_x_continuous(breaks = seq(91.25, 730, by = 91.25),
                     labels = c("3 months", "6 months", "9 months", "12 months", "15 months",
                                "18 months", "21 months", "24 months")) +
  scale_color_manual(values = c('#04A0D2', '#AF4745', '#78AF96')) +
  scale_fill_manual(values = c('#04A0D2', '#AF4745', '#78AF96')) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 30)) +
  labs(
    y = "Cumulative Incidence",
    x = "Months from SARS-CoV-2 Infection to Death/Censor"
  ) +
  add_pvalue(caption = "Log-rank {p.value}")

# build plot (which constructs the risktable)
built_p <- ggsurvfit_build(km_surv_object2)

t1 <- survfit(Surv(time_to_death, death) ~ rurality, data = local_df) %>%
  tbl_survfit(
    times = c(30, 91, 365, 730),
    label_header = "**Survival (95% CI)**",
    reverse = TRUE,
    estimate_fun = function(x) style_number(x, digits = 2, scale = 100)
  )

table1 <- as_tibble(t1, col_labels = FALSE)

#### Main models without IPW by rurality ####

# One-month deaths
local_df <- from_rdata(local)
# Adjust the dataset by censoring deaths that occur after 30 days
local_df <- local_df %>%
  mutate(death = ifelse(time_to_death > 30, 0, death),  # Change death to 0 if time_to_death is > 30 days
         time_to_death = ifelse(time_to_death > 30, 30, time_to_death))  

m1 <- coxph(Surv(time_to_death, death)~ sex + age_group + race_ethnicity + variant_period + vaccination_status + OBESITY + HYPERTENSION + MI + CHF + PVD + CVD + RD + PULMONARY + PUD + HEMI_PARA + diabetes + DEMENTIA + liver + RENAL + cancer + HIV + TOBACCO + SUBSTANCE + subregion + rurality + SVI, data=local_df) %>%
  tbl_regression(exponentiate = TRUE)   %>%
  add_nevent(location = "level") %>%
  add_n(location = "level") %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(
        stat_nevent_rate =
          ifelse(
            !is.na(stat_nevent),
            paste0(style_sigfig(stat_nevent / stat_n, scale = 100), "%"),
            NA
          ),
        .after = stat_nevent
      )
  ) %>%
  modify_cols_merge(
    pattern = "{stat_nevent} / {stat_n} ({stat_nevent_rate})",
    rows = !is.na(stat_nevent)
  ) %>%
  modify_header(stat_nevent = "**Event Rate**")


m3 <-
  tbl_uvregression(
    local_df[c("time_to_death", "death", "sex", "age_group", "race_ethnicity", "variant_period", "vaccination_status", "OBESITY",  "HYPERTENSION", "MI", "CHF", "PVD", "CVD", "RD", "PULMONARY", "PUD", "HEMI_PARA", "diabetes", "DEMENTIA", "liver", "RENAL", "cancer", "HIV", "TOBACCO", "SUBSTANCE", "subregion", "rurality", "SVI")],
    method = coxph,
    y = Surv(time_to_death, death),
    exponentiate = TRUE
  )

merged_tbl <-
  tbl_merge(
    tbls = list(m3, m1)
  )

table1 <- as_tibble(merged_tbl, col_labels = FALSE)

# Three-month deaths
local_df <- from_rdata(local)
# Adjust the dataset by censoring deaths that occur after 91 days
local_df <- local_df %>%
  mutate(death = ifelse(time_to_death > 91, 0, death),  # Change death to 0 if time_to_death is > 91 days
         time_to_death = ifelse(time_to_death > 91, 91, time_to_death))  # Cap time_to_death at 91 days for those > 91 days


m1 <- coxph(Surv(time_to_death, death)~ sex + age_group + race_ethnicity + variant_period + vaccination_status + OBESITY + HYPERTENSION + MI + CHF + PVD + CVD + RD + PULMONARY + PUD + HEMI_PARA + diabetes + DEMENTIA + liver + RENAL + cancer + HIV + TOBACCO + SUBSTANCE + subregion + rurality + SVI, data=local_df) %>%
  tbl_regression(exponentiate = TRUE)   %>%
  add_nevent(location = "level") %>%
  add_n(location = "level") %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(
        stat_nevent_rate =
          ifelse(
            !is.na(stat_nevent),
            paste0(style_sigfig(stat_nevent / stat_n, scale = 100), "%"),
            NA
          ),
        .after = stat_nevent
      )
  ) %>%
  modify_cols_merge(
    pattern = "{stat_nevent} / {stat_n} ({stat_nevent_rate})",
    rows = !is.na(stat_nevent)
  ) %>%
  modify_header(stat_nevent = "**Event Rate**")


m3 <-
  tbl_uvregression(
    local_df[c("time_to_death", "death", "sex", "age_group", "race_ethnicity", "variant_period", "vaccination_status", "OBESITY",  "HYPERTENSION", "MI", "CHF", "PVD", "CVD", "RD", "PULMONARY", "PUD", "HEMI_PARA", "diabetes", "DEMENTIA", "liver", "RENAL", "cancer", "HIV", "TOBACCO", "SUBSTANCE", "subregion", "rurality", "SVI")],
    method = coxph,
    y = Surv(time_to_death, death),
    exponentiate = TRUE
  )

merged_tbl <-
  tbl_merge(
    tbls = list(m3, m1)
  )

# One-year deaths
local_df <- from_rdata(local)
# Adjust the dataset by censoring deaths that occur after 365 days
local_df <- local_df %>%
  mutate(death = ifelse(time_to_death > 365, 0, death),  # Change death to 0 if time_to_death is > 365 days
         time_to_death = ifelse(time_to_death > 365, 365, time_to_death))  # Cap time_to_death at 365 days for those > 365 days


m1 <- coxph(Surv(time_to_death, death)~ sex + age_group + race_ethnicity + variant_period + vaccination_status + OBESITY + HYPERTENSION + MI + CHF + PVD + CVD + RD + PULMONARY + PUD + HEMI_PARA + diabetes + DEMENTIA + liver + RENAL + cancer + HIV + TOBACCO + SUBSTANCE + subregion + rurality + SVI, data=local_df) %>%
  tbl_regression(exponentiate = TRUE)   %>%
  add_nevent(location = "level") %>%
  add_n(location = "level") %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(
        stat_nevent_rate =
          ifelse(
            !is.na(stat_nevent),
            paste0(style_sigfig(stat_nevent / stat_n, scale = 100), "%"),
            NA
          ),
        .after = stat_nevent
      )
  ) %>%
  modify_cols_merge(
    pattern = "{stat_nevent} / {stat_n} ({stat_nevent_rate})",
    rows = !is.na(stat_nevent)
  ) %>%
  modify_header(stat_nevent = "**Event Rate**")


m3 <-
  tbl_uvregression(
    local_df[c("time_to_death", "death", "sex", "age_group", "race_ethnicity", "variant_period", "vaccination_status", "OBESITY",  "HYPERTENSION", "MI", "CHF", "PVD", "CVD", "RD", "PULMONARY", "PUD", "HEMI_PARA", "diabetes", "DEMENTIA", "liver", "RENAL", "cancer", "HIV", "TOBACCO", "SUBSTANCE", "subregion", "rurality", "SVI")],
    method = coxph,
    y = Surv(time_to_death, death),
    exponentiate = TRUE
  )


merged_tbl <-
  tbl_merge(
    tbls = list(m3, m1)
  )

# Two-year deaths
local_df <- from_rdata(local)
m1 <- coxph(Surv(time_to_death, death)~ sex + age_group + race_ethnicity + variant_period + vaccination_status + OBESITY + HYPERTENSION + MI + CHF + PVD + CVD + RD + PULMONARY + PUD + HEMI_PARA + diabetes + DEMENTIA + liver + RENAL + cancer + HIV + TOBACCO + SUBSTANCE + subregion + rurality + SVI, data=local_df) %>%
  tbl_regression(exponentiate = TRUE)   %>%
  add_nevent(location = "level") %>%
  add_n(location = "level") %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(
        stat_nevent_rate =
          ifelse(
            !is.na(stat_nevent),
            paste0(style_sigfig(stat_nevent / stat_n, scale = 100), "%"),
            NA
          ),
        .after = stat_nevent
      )
  ) %>%
  modify_cols_merge(
    pattern = "{stat_nevent} / {stat_n} ({stat_nevent_rate})",
    rows = !is.na(stat_nevent)
  ) %>%
  modify_header(stat_nevent = "**Event Rate**")


m3 <-
  tbl_uvregression(
    local_df[c("time_to_death", "death", "sex", "age_group", "race_ethnicity", "variant_period", "vaccination_status", "OBESITY",  "HYPERTENSION", "MI", "CHF", "PVD", "CVD", "RD", "PULMONARY", "PUD", "HEMI_PARA", "diabetes", "DEMENTIA", "liver", "RENAL", "cancer", "HIV", "TOBACCO", "SUBSTANCE", "subregion", "rurality", "SVI")],
    method = coxph,
    y = Surv(time_to_death, death),
    exponentiate = TRUE
  )

merged_tbl <-
  tbl_merge(
    tbls = list(m3, m1)
  )

#### Propesensity Score Estimation for Rurality ####
ps_model <- multinom(rurality ~ sex + age_group + race_ethnicity + variant_period + vaccination_status,
                     data = local_df)
local_df$propensity_scores <- predict(ps_model, type = "probs")
local_df$weight <- with(local_df, ifelse(rurality == "Urban-Adjacent Rural",
                                         1 / propensity_scores[,"Urban-Adjacent Rural"],
                                         ifelse(rurality == "Nonurban-Adjacent Rural",
                                                1 / propensity_scores[,"Nonurban-Adjacent Rural"],
                                                1 / propensity_scores[,"Urban"])))

# Balance Table
bal_table <- bal.tab(rurality ~ sex + age_group + race_ethnicity + variant_period + vaccination_status,
                     data = local_df,
                     weights = "weight",
                     method = "weighting",
                     un = TRUE,
                     disp = c("means", "sds"),
                     stats = c("mean.diffs", "variance.ratios"))

# New Variable Names
new.names <- c(
  "sex_Male" = "Male Sex",
  "age_at_covid" = "Age at COVID-19 Diagnosis",
  "variant_period_Omicron (B.1.1.529, BA.2, BA.2.12.1)" = "Omicron (B.1.1.529, BA.2, BA.2.12.1)",
  "variant_period_Ancestral COVID-19" = "Ancestral COVID-19",
  "variant_period_Delta (B.1.617.2)" = "Delta (B.1.617.2)",
  "variant_period_Alpha (B.1.1.7), Beta (B.1.351), Gamma (P.1)" = "Alpha (B.1.1.7), Beta (B.1.351), Gamma (P.1)",
  "variant_period_Omicron (BA.5, BQ.1.1, XBB.1.5)" = "Omicron (BA.5, BQ.1.1, XBB.1.5)",
  "vaccination_status_Non-Breakthrough Infection" = "No COVID-19 Vaccine Series Completed",
  "vaccination_status_VAX2 Breakthrough Infection" = "Primary COVID-19 Vaccine Series Completed",
  "vaccination_status_VAX3 Breakthrough Infection" = "Primary+ COVID-19 Vaccine Series Completed",
  "race_ethnicity_White Non-Hispanic" = "White Non-Hispanic",
  "race_ethnicity_Black or African American Non-Hispanic" = "Black or African American Non-Hispanic",
  "race_ethnicity_Hispanic or Latino Any Race" = "Hispanic or Latino Any Race",
  "race_ethnicity_Other" = "Other",
  "race_ethnicity_Unknown" = "Unknown",
  "CCI_BEFORE_COVID" = "Charlson Comorbidity Index",
  "age_group_18-45" = "Age: 18-45",
  "age_group_46-65" = "Age: 46-65",
  "age_group_>65" = "Age: >65",
  "obesity_Hx of Obesity" = "Hx of Obesity",
  "hypertension_Hx of Hypertension" = "Hx of Hypertension",
  "ckd_dialysis_Hx of ckd/dialysis" = "Hx of CKD/Dialysis",
  "diabetes_Hx of Diabetes" = "Hx of Diabetes",
  "copd_asthma_Hx of COPD/asthma" = "Hx of COPD/Asthma",
  "cancer_Hx of Cancer" = "Hx of Cancer",
  "cad_Hx of CAD" = "Hx of CAD",
  "chf_Hx of CHF" = "Hx of CHF",
  "pvd_Hx of PVD" = "Hx of PVD",
  "liver_Hx of Liver Disease" = "Hx of Liver Disease",
  "transplant_type_Kidney" = "Kidney Transplant",
  "transplant_type_Liver" = "Liver Transplant",
  "transplant_type_Lung" = "Lung Transplant",
  "transplant_type_Heart" = "Heart Transplant",
  "transplant_type_Multiple" = "Multiple Transplant",
  "subregion_Midwest" = "Midwest Region",
  "subregion_Northeast" = "Northeast Region",
  "subregion_South" = "South Region",
  "subregion_West" = "West Region",
  "subregion_East North Central" = "East North Central",
  "subregion_East South Central" = "East South Central",
  "subregion_Middle Atlantic" = "Middle Atlantic",
  "subregion_Mountain" = "Mountain",
  "subregion_New England" = "New England",
  "subregion_Pacific" = "Pacific",
  "subregion_South Atlantic" = "South Atlantic",
  "subregion_West North Central" = "West North Central",
  "subregion_West South Central" = "West South Central"
)

# Love Plot
p <- love.plot(bal_table,
               threshold = 0.1,
               abs = TRUE,
               colors = pal_npg("nrc")(2),
               shapes = c("triangle filled", "circle filled"),
               var.names = new.names) +
  theme(legend.justification = c(0.9, 0.5),
        legend.position = c(0.9, 0.5),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14)) +
  xlab("Absolute Standardized Mean Difference (SMD)") +
  ylab("Covariates")

# image: svg
plot(p)

# Prepare SMD Table
smd_data <- as.data.frame(bal_table$Balance)
smd_data$Variable <- rownames(smd_data)  # Add row names as a column
smd_data <- smd_data %>%
  select(Variable, Max.Diff.Un, Max.Diff.Adj) %>%
  rename(
    Unweighted_SMD = Max.Diff.Un,
    Weighted_SMD = Max.Diff.Adj
  ) %>%
  as_tibble()

# Calculate Overall SMDs
overall_smd <- smd_data %>%
  summarize(
    Variable = "Overall",
    Unweighted_SMD = mean(abs(Unweighted_SMD), na.rm = TRUE),
    Weighted_SMD = mean(abs(Weighted_SMD), na.rm = TRUE)
  )

# Combine Overall SMDs with Individual SMDs
smd_table <- bind_rows(smd_data, overall_smd)

summary_stats <- local_df %>%
  group_by(rurality) %>%
  summarize(mean_ps = mean(propensity_scores, na.rm = TRUE),
            sd_ps = sd(propensity_scores, na.rm = TRUE),
            median_ps = median(propensity_scores, na.rm = TRUE),
            iqr_ps = IQR(propensity_scores, na.rm = TRUE))

print(summary_stats)

#### Coxme models with IPW by rurality ####

# Custom Function for Coxme Tidying
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

# One-month deaths
local_df <- from_rdata(ipw_local_cat)

# Adjust the dataset by censoring deaths that occur after 30 days
local_df <- local_df %>%
  mutate(death = ifelse(time_to_death > 30, 0, death),  # Change death to 0 if time_to_death is > 30 days
         time_to_death = ifelse(time_to_death > 30, 30, time_to_death))  # Cap time_to_death at 30 days for those > 30 days

# Death rurality
m1 <- coxme(Surv(time_to_death, death)~ sex + age_group + race_ethnicity + variant_period + vaccination_status + OBESITY + HYPERTENSION + MI + CHF + PVD + CVD + RD + PULMONARY + PUD + HEMI_PARA + diabetes + DEMENTIA + liver + RENAL + cancer + HIV + TOBACCO + SUBSTANCE + subregion + rurality + SVI + (1 | data_partner_id), weights = weight, data=local_df)

m1_results <- broom::tidy(m1, exponentiate = TRUE)

# Three-month deaths
local_df <- from_rdata(ipw_local_cat)

# Adjust the dataset by censoring deaths that occur after 91 days
local_df <- local_df %>%
  mutate(death = ifelse(time_to_death > 91, 0, death),  # Change death to 0 if time_to_death is > 91 days
         time_to_death = ifelse(time_to_death > 91, 91, time_to_death))  # Cap time_to_death at 91 days for those > 91 days

# Death rurality
m1 <- coxme(Surv(time_to_death, death)~ sex + age_group + race_ethnicity + variant_period + vaccination_status + OBESITY + HYPERTENSION + MI + CHF + PVD + CVD + RD + PULMONARY + PUD + HEMI_PARA + diabetes + DEMENTIA + liver + RENAL + cancer + HIV + TOBACCO + SUBSTANCE + subregion + rurality + SVI + (1 | data_partner_id), weights = weight, data=local_df)

m1_results <- broom::tidy(m1, exponentiate = TRUE)

# One-year deaths
local_df <- from_rdata(ipw_local_cat)

# Adjust the dataset by censoring deaths that occur after 365 days
local_df <- local_df %>%
  mutate(death = ifelse(time_to_death > 365, 0, death),  # Change death to 0 if time_to_death is > 365 days
         time_to_death = ifelse(time_to_death > 365, 365, time_to_death))  # Cap time_to_death at 365 days for those > 365 days

# Death rurality
m1 <- coxme(Surv(time_to_death, death)~ sex + age_group + race_ethnicity + variant_period + vaccination_status + OBESITY + HYPERTENSION + MI + CHF + PVD + CVD + RD + PULMONARY + PUD + HEMI_PARA + diabetes + DEMENTIA + liver + RENAL + cancer + HIV + TOBACCO + SUBSTANCE + subregion + rurality + SVI + (1 | data_partner_id), weights = weight, data=local_df)

# Two-year deaths
local_df <- from_rdata(ipw_local_cat)
# Death rurality
m1 <- coxme(Surv(time_to_death, death)~ sex + age_group + race_ethnicity + variant_period + vaccination_status + OBESITY + HYPERTENSION + MI + CHF + PVD + CVD + RD + PULMONARY + PUD + HEMI_PARA + diabetes + DEMENTIA + liver + RENAL + cancer + HIV + TOBACCO + SUBSTANCE + subregion + rurality + SVI + (1 | data_partner_id), weights = weight, data=local_df)

m1_results <- broom::tidy(m1, exponentiate = TRUE)
m1_results$Model_Outcome <- "Death Multilevel"

#### Models with deaths only occuring at intervals one month, three months, or one year after infection by rurality ####

local_df <- from_rdata(local)

s2 <- Sys.time()
cat("Step 2: Run model 1. Timestamp:", as.character(s2, TZ="America/Chicago"), ".\n Time Difference From Previous Step: ", difftime(s2, s1, units = "secs"), " seconds. \n")

# Only include deaths occurring at > one month after infection
local_df <- from_rdata(local)
local_df <- local_df %>% filter(time_to_death > 30)

m1 <- coxph(Surv(time_to_death, death)~ sex + age_group + race_ethnicity + variant_period + vaccination_status + OBESITY + HYPERTENSION + MI + CHF + PVD + CVD + RD + PULMONARY + PUD + HEMI_PARA + diabetes + DEMENTIA + liver + RENAL + cancer + HIV + TOBACCO + SUBSTANCE + subregion + rurality + SVI, data=local_df) %>%
  tbl_regression(exponentiate = TRUE)   %>%
  add_nevent(location = "level") %>%
  add_n(location = "level") %>%
  # adding event rate
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(
        stat_nevent_rate =
          ifelse(
            !is.na(stat_nevent),
            paste0(style_sigfig(stat_nevent / stat_n, scale = 100), "%"),
            NA
          ),
        .after = stat_nevent
      )
  ) %>%
  # merge the colums into a single column
  modify_cols_merge(
    pattern = "{stat_nevent} / {stat_n} ({stat_nevent_rate})",
    rows = !is.na(stat_nevent)
  ) %>%
  # update header to event rate
  modify_header(stat_nevent = "**Event Rate**")

m3 <-
  tbl_uvregression(
    local_df[c("time_to_death", "death", "sex", "age_group", "race_ethnicity", "variant_period", "vaccination_status", "OBESITY",  "HYPERTENSION", "MI", "CHF", "PVD", "CVD", "RD", "PULMONARY", "PUD", "HEMI_PARA", "diabetes", "DEMENTIA", "liver", "RENAL", "cancer", "HIV", "TOBACCO", "SUBSTANCE", "subregion", "rurality", "SVI")],
    method = coxph,
    y = Surv(time_to_death, death),
    exponentiate = TRUE
  )

# Only include deaths occurring at > three months after infection
local_df <- from_rdata(local)
local_df <- local_df %>% filter(time_to_death > 91)

m1 <- coxph(Surv(time_to_death, death)~ sex + age_group + race_ethnicity + variant_period + vaccination_status + OBESITY + HYPERTENSION + MI + CHF + PVD + CVD + RD + PULMONARY + PUD + HEMI_PARA + diabetes + DEMENTIA + liver + RENAL + cancer + HIV + TOBACCO + SUBSTANCE + subregion + rurality + SVI, data=local_df) %>%
  tbl_regression(exponentiate = TRUE)   %>%
  add_nevent(location = "level") %>%
  add_n(location = "level") %>%
  # adding event rate
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(
        stat_nevent_rate =
          ifelse(
            !is.na(stat_nevent),
            paste0(style_sigfig(stat_nevent / stat_n, scale = 100), "%"),
            NA
          ),
        .after = stat_nevent
      )
  ) %>%
  # merge the colums into a single column
  modify_cols_merge(
    pattern = "{stat_nevent} / {stat_n} ({stat_nevent_rate})",
    rows = !is.na(stat_nevent)
  ) %>%
  # update header to event rate
  modify_header(stat_nevent = "**Event Rate**")


m3 <-
  tbl_uvregression(
    local_df[c("time_to_death", "death", "sex", "age_group", "race_ethnicity", "variant_period", "vaccination_status", "OBESITY",  "HYPERTENSION", "MI", "CHF", "PVD", "CVD", "RD", "PULMONARY", "PUD", "HEMI_PARA", "diabetes", "DEMENTIA", "liver", "RENAL", "cancer", "HIV", "TOBACCO", "SUBSTANCE", "subregion", "rurality", "SVI")],
    method = coxph,
    y = Surv(time_to_death, death),
    exponentiate = TRUE
  )


merged_tbl <-
  tbl_merge(
    tbls = list(m3, m1)
  )

# Only include deaths occurring at > one year after infection
local_df <- from_rdata(local)
local_df <- local_df %>% filter(time_to_death > 365)

m1 <- coxph(Surv(time_to_death, death)~ sex + age_group + race_ethnicity + variant_period + vaccination_status + OBESITY + HYPERTENSION + MI + CHF + PVD + CVD + RD + PULMONARY + PUD + HEMI_PARA + diabetes + DEMENTIA + liver + RENAL + cancer + HIV + TOBACCO + SUBSTANCE + subregion + rurality + SVI, data=local_df) %>%
  tbl_regression(exponentiate = TRUE)   %>%
  add_nevent(location = "level") %>%
  add_n(location = "level") %>%
  # adding event rate
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(
        stat_nevent_rate =
          ifelse(
            !is.na(stat_nevent),
            paste0(style_sigfig(stat_nevent / stat_n, scale = 100), "%"),
            NA
          ),
        .after = stat_nevent
      )
  ) %>%
  # merge the colums into a single column
  modify_cols_merge(
    pattern = "{stat_nevent} / {stat_n} ({stat_nevent_rate})",
    rows = !is.na(stat_nevent)
  ) %>%
  # update header to event rate
  modify_header(stat_nevent = "**Event Rate**")


m3 <-
  tbl_uvregression(
    local_df[c("time_to_death", "death", "sex", "age_group", "race_ethnicity", "variant_period", "vaccination_status", "OBESITY",  "HYPERTENSION", "MI", "CHF", "PVD", "CVD", "RD", "PULMONARY", "PUD", "HEMI_PARA", "diabetes", "DEMENTIA", "liver", "RENAL", "cancer", "HIV", "TOBACCO", "SUBSTANCE", "subregion", "rurality", "SVI")],
    method = coxph,
    y = Surv(time_to_death, death),
    exponentiate = TRUE
  )

merged_tbl <-
  tbl_merge(
    tbls = list(m3, m1)
  )

#### Models with IPW and Mixed Effects modeling with deaths only occuring at intervals one month, three months, or one year after infection by rurality ####

# Only include deaths occurring at > one month after infection
local_df <- from_rdata(ipw_local_cat)
local_df <- local_df %>% filter(time_to_death > 30)

# Death rurality
m1 <- coxme(Surv(time_to_death, death)~ sex + age_group + race_ethnicity + variant_period + vaccination_status + OBESITY + HYPERTENSION + MI + CHF + PVD + CVD + RD + PULMONARY + PUD + HEMI_PARA + diabetes + DEMENTIA + liver + RENAL + cancer + HIV + TOBACCO + SUBSTANCE + subregion + rurality + SVI + (1 | data_partner_id), weights = weight, data=local_df)

m1_results <- broom::tidy(m1, exponentiate = TRUE)
m1_results$Model_Outcome <- "Death Multilevel"

# Only include deaths occurring at > three months after infection
local_df <- from_rdata(ipw_local_cat)
local_df <- local_df %>% filter(time_to_death > 91)

# Death rurality
m1 <- coxme(Surv(time_to_death, death)~ sex + age_group + race_ethnicity + variant_period + vaccination_status + OBESITY + HYPERTENSION + MI + CHF + PVD + CVD + RD + PULMONARY + PUD + HEMI_PARA + diabetes + DEMENTIA + liver + RENAL + cancer + HIV + TOBACCO + SUBSTANCE + subregion + rurality + SVI + (1 | data_partner_id), weights = weight, data=local_df)

m1_results <- broom::tidy(m1, exponentiate = TRUE)
m1_results$Model_Outcome <- "Death Multilevel"

# Only include deaths occurring at > one year after infection

local_df <- from_rdata(ipw_local_cat)
local_df <- local_df %>% filter(time_to_death > 365)

# Death rurality
m1 <- coxme(Surv(time_to_death, death)~ sex + age_group + race_ethnicity + variant_period + vaccination_status + OBESITY + HYPERTENSION + MI + CHF + PVD + CVD + RD + PULMONARY + PUD + HEMI_PARA + diabetes + DEMENTIA + liver + RENAL + cancer + HIV + TOBACCO + SUBSTANCE + subregion + rurality + SVI + (1 | data_partner_id), weights = weight, data=local_df)

m1_results <- broom::tidy(m1, exponentiate = TRUE)
m1_results$Model_Outcome <- "Death Multilevel"

#### Main event rate and risk difference calculation ####
## Event rate and risk difference per person months
# Define age groups in the dataset
local_df <- local_df %>%
  mutate(age_group = case_when(
    age_at_covid < 1 ~ "<1",
    age_at_covid >= 1 & age_at_covid <= 4 ~ "1-4",
    age_at_covid >= 5 & age_at_covid <= 14 ~ "5-14",
    age_at_covid >= 15 & age_at_covid <= 24 ~ "15-24",
    age_at_covid >= 25 & age_at_covid <= 34 ~ "25-34",
    age_at_covid >= 35 & age_at_covid <= 44 ~ "35-44",
    age_at_covid >= 45 & age_at_covid <= 54 ~ "45-54",
    age_at_covid >= 55 & age_at_covid <= 64 ~ "55-64",
    age_at_covid >= 65 & age_at_covid <= 74 ~ "65-74",
    age_at_covid >= 75 & age_at_covid <= 84 ~ "75-84",
    age_at_covid >= 85 ~ "85+",
    TRUE ~ NA_character_
  ))

# Define the list of outcomes and corresponding time-to-event columns
outcomes_and_times <- list(
  death = "time_to_death" 
)

# Standard population proportions for age adjustment (2020 U.S. Standard Population)
standard_population <- data.frame(
  age_group = c("<1", "1-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+"),
  proportion = c(0.012800, 0.051600, 0.134933, 0.132583, 0.134900, 0.135573, 0.122932, 0.122293, 0.087247, 0.044842, 0.020297)
)

# Separate data into different rurality groups
urban_df <- filter(local_df, rurality == "Urban")
UAR_df <- filter(local_df, rurality == "Urban-Adjacent Rural")
NAR_df <- filter(local_df, rurality == "Nonurban-Adjacent Rural")
rural_df <- filter(local_df, rural_binary == "Rural")
overall_df <- local_df

# Function to calculate crude event rate per 100,000 person-months and confidence intervals
calculate_event_rate <- function(df, time_col, event_col) {
  total_time_at_risk_months <- sum(df[[time_col]], na.rm = TRUE) / 30.44 # Convert days to months
  total_events <- sum(df[[event_col]], na.rm = TRUE)
  event_rate_per_100000_pm <- (total_events / total_time_at_risk_months) * 100000
  ci <- poisson.test(total_events, total_time_at_risk_months)$conf.int * 100000
  return(data.frame(
    Crude_Event_Rate_per_100000_PM = event_rate_per_100000_pm,
    Crude_CI_Lower = ci[1],
    Crude_CI_Upper = ci[2],
    Total_Time_at_Risk_Months = total_time_at_risk_months,
    Total_Events = total_events
  ))
}

# Function to calculate age-adjusted event rate per 100,000 person-months
calculate_age_adjusted_event_rate <- function(df, time_col, event_col, standard_population) {
  age_specific_rates <- df %>%
    group_by(age_group) %>%
    summarise(
      total_events = sum(.data[[event_col]], na.rm = TRUE),
      total_time_at_risk_months = sum(.data[[time_col]], na.rm = TRUE) / 30.44,
      event_rate_per_100000_pm = (total_events / total_time_at_risk_months) * 100000,
      variance = (total_events / (total_time_at_risk_months^2)) * (100000^2),  
      .groups = "drop"
    ) %>%
    left_join(standard_population, by = "age_group") %>%
    mutate(weighted_rate = event_rate_per_100000_pm * proportion,
           weighted_variance = variance * (proportion^2))

  # Compute age-adjusted rate
  age_adjusted_rate <- sum(age_specific_rates$weighted_rate, na.rm = TRUE)

  # Compute standard error
  standard_error <- sqrt(sum(age_specific_rates$weighted_variance, na.rm = TRUE))  
  
  # Compute confidence intervals
  z_value <- qnorm(0.975)  # 95% CI
  ci_lower <- max(0, age_adjusted_rate - z_value * standard_error)  
  ci_upper <- age_adjusted_rate + z_value * standard_error

  return(data.frame(
    Adj_Event_Rate_per_100000_PM = age_adjusted_rate,
    Adj_CI_Lower = ci_lower,
    Adj_CI_Upper = ci_upper
  ))
}

# Function to calculate crude risk differences per 100,000 person-months
calculate_crude_risk_difference <- function(group_df, overall_df, time_col, event_col) {
  group_rates <- group_df %>%
    summarise(
      Events = sum(.data[[event_col]], na.rm = TRUE),
      Time_at_risk_months = sum(.data[[time_col]], na.rm = TRUE) / 30.44
    )

  overall_rates <- overall_df %>%
    summarise(
      Events = sum(.data[[event_col]], na.rm = TRUE),
      Time_at_risk_months = sum(.data[[time_col]], na.rm = TRUE) / 30.44
    )

  group_event_rate <- group_rates$Events / group_rates$Time_at_risk_months
  overall_event_rate <- overall_rates$Events / overall_rates$Time_at_risk_months

  rate_diff <- (group_event_rate - overall_event_rate) * 100000
  se_diff <- sqrt(
    (group_event_rate / group_rates$Time_at_risk_months) +
    (overall_event_rate / overall_rates$Time_at_risk_months)
  ) * 100000
  z_value <- qnorm(0.975)
  ci_lower <- rate_diff - z_value * se_diff
  ci_upper <- rate_diff + z_value * se_diff

  return(data.frame(
    Crude_Risk_Difference_per_100000_PM = rate_diff,
    Crude_RD_CI_Lower = ci_lower,
    Crude_RD_CI_Upper = ci_upper
  ))
}

# Function to calculate age-adjusted risk differences per 100,000 person-months
calculate_age_adjusted_risk_difference <- function(group_df, overall_df, time_col, event_col, standard_population) {
  group_rate <- calculate_age_adjusted_event_rate(group_df, time_col, event_col, standard_population)
  overall_rate <- calculate_age_adjusted_event_rate(overall_df, time_col, event_col, standard_population)

  risk_difference <- group_rate$Adj_Event_Rate_per_100000_PM - overall_rate$Adj_Event_Rate_per_100000_PM
  standard_error <- sqrt(
    (group_rate$Adj_Event_Rate_per_100000_PM - group_rate$Adj_CI_Lower)^2 +
    (overall_rate$Adj_Event_Rate_per_100000_PM - overall_rate$Adj_CI_Lower)^2
  )
  z_value <- qnorm(0.975)
  rd_ci_lower <- risk_difference - z_value * standard_error
  rd_ci_upper <- risk_difference + z_value * standard_error

  return(data.frame(
    Adj_Risk_Difference_per_100000_PM = risk_difference,
    Adj_RD_CI_Lower = rd_ci_lower,
    Adj_RD_CI_Upper = rd_ci_upper
  ))
}


# Analyze data for urban, UAR, NAR, rural, and overall groups
analyze_data <- function() {
  results <- lapply(names(outcomes_and_times), function(event_col) {
    time_col <- outcomes_and_times[[event_col]]

    # Crude event rates
    urban_event_rate <- calculate_event_rate(urban_df, time_col, event_col)
    UAR_event_rate <- calculate_event_rate(UAR_df, time_col, event_col)
    NAR_event_rate <- calculate_event_rate(NAR_df, time_col, event_col)
    rural_event_rate <- calculate_event_rate(rural_df, time_col, event_col)
    overall_event_rate <- calculate_event_rate(overall_df, time_col, event_col)

    # Age-adjusted event rates
    urban_adj_event_rate <- calculate_age_adjusted_event_rate(urban_df, time_col, event_col, standard_population)
    UAR_adj_event_rate <- calculate_age_adjusted_event_rate(UAR_df, time_col, event_col, standard_population)
    NAR_adj_event_rate <- calculate_age_adjusted_event_rate(NAR_df, time_col, event_col, standard_population)
    rural_adj_event_rate <- calculate_age_adjusted_event_rate(rural_df, time_col, event_col, standard_population)
    overall_adj_event_rate <- calculate_age_adjusted_event_rate(overall_df, time_col, event_col, standard_population)

    # Crude risk differences (Comparing to Urban instead of Overall)
    UAR_crude_risk_diff <- calculate_crude_risk_difference(UAR_df, urban_df, time_col, event_col)
    NAR_crude_risk_diff <- calculate_crude_risk_difference(NAR_df, urban_df, time_col, event_col)
    rural_crude_risk_diff <- calculate_crude_risk_difference(rural_df, urban_df, time_col, event_col)

    # Adjusted risk differences (Comparing to Urban instead of Overall)
    UAR_adj_risk_diff <- calculate_age_adjusted_risk_difference(UAR_df, urban_df, time_col, event_col, standard_population)
    NAR_adj_risk_diff <- calculate_age_adjusted_risk_difference(NAR_df, urban_df, time_col, event_col, standard_population)
    rural_adj_risk_diff <- calculate_age_adjusted_risk_difference(rural_df, urban_df, time_col, event_col, standard_population)

    # Combine results
    rbind(
      cbind(data.frame(Rurality = "Urban", Outcome = event_col), 
            urban_event_rate, urban_adj_event_rate, 
            data.frame(Crude_Risk_Difference_per_100000_PM = NA, Crude_RD_CI_Lower = NA, Crude_RD_CI_Upper = NA, 
                       Adj_Risk_Difference_per_100000_PM = NA, Adj_RD_CI_Lower = NA, Adj_RD_CI_Upper = NA)),   
      
      cbind(data.frame(Rurality = "Urban-Adjacent Rural", Outcome = event_col), 
            UAR_event_rate, UAR_adj_event_rate, UAR_crude_risk_diff, UAR_adj_risk_diff),
      
      cbind(data.frame(Rurality = "Nonurban-Adjacent Rural", Outcome = event_col), 
            NAR_event_rate, NAR_adj_event_rate, NAR_crude_risk_diff, NAR_adj_risk_diff),
      
      cbind(data.frame(Rurality = "Rural", Outcome = event_col), 
            rural_event_rate, rural_adj_event_rate, rural_crude_risk_diff, rural_adj_risk_diff),
      
      cbind(data.frame(Rurality = "Overall", Outcome = event_col), 
            overall_event_rate, overall_adj_event_rate, 
            data.frame(Crude_Risk_Difference_per_100000_PM = NA, Crude_RD_CI_Lower = NA, Crude_RD_CI_Upper = NA, 
                       Adj_Risk_Difference_per_100000_PM = NA, Adj_RD_CI_Lower = NA, Adj_RD_CI_Upper = NA)) 
    )
  })

  return(do.call(rbind, results))
}

# Analyze the data
final_results <- analyze_data()



## Event rate and risk difference per person years
# Define age groups in the dataset
local_df <- local_df %>%
  mutate(age_group = case_when(
    age_at_covid < 1 ~ "<1",
    age_at_covid >= 1 & age_at_covid <= 4 ~ "1-4",
    age_at_covid >= 5 & age_at_covid <= 14 ~ "5-14",
    age_at_covid >= 15 & age_at_covid <= 24 ~ "15-24",
    age_at_covid >= 25 & age_at_covid <= 34 ~ "25-34",
    age_at_covid >= 35 & age_at_covid <= 44 ~ "35-44",
    age_at_covid >= 45 & age_at_covid <= 54 ~ "45-54",
    age_at_covid >= 55 & age_at_covid <= 64 ~ "55-64",
    age_at_covid >= 65 & age_at_covid <= 74 ~ "65-74",
    age_at_covid >= 75 & age_at_covid <= 84 ~ "75-84",
    age_at_covid >= 85 ~ "85+",
    TRUE ~ NA_character_
  ))

# Define the list of outcomes and corresponding time_to_event columns
outcomes_and_times <- list(
  death = "time_to_death" 
)

# Standard population proportions for age adjustment (2020 U.S. Standard Population)
standard_population <- data.frame(
  age_group = c("<1", "1-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+"),
  proportion = c(0.012800, 0.051600, 0.134933, 0.132583, 0.134900, 0.135573, 0.122932, 0.122293, 0.087247, 0.044842, 0.020297)
)

# Separate data into different rurality groups
urban_df <- filter(local_df, rurality == "Urban")
UAR_df <- filter(local_df, rurality == "Urban-Adjacent Rural")
NAR_df <- filter(local_df, rurality == "Nonurban-Adjacent Rural")
rural_df <- filter(local_df, rural_binary == "Rural")
overall_df <- local_df

# Function to calculate crude event rate per 100,000 person-years and confidence intervals
calculate_event_rate <- function(df, time_col, event_col) {
  total_time_at_risk_years <- sum(df[[time_col]], na.rm = TRUE) / 365.25 # Convert days to years
  total_events <- sum(df[[event_col]], na.rm = TRUE)
  event_rate_per_100000_py <- (total_events / total_time_at_risk_years) * 100000
  ci <- poisson.test(total_events, total_time_at_risk_years)$conf.int * 100000
  return(data.frame(
    Crude_Event_Rate_per_100000_PY = event_rate_per_100000_py,
    Crude_CI_Lower = ci[1],
    Crude_CI_Upper = ci[2],
    Total_Time_at_Risk_Years = total_time_at_risk_years,
    Total_Events = total_events
  ))
}

# Function to calculate age-adjusted event rate per 100,000 person-years
calculate_age_adjusted_event_rate <- function(df, time_col, event_col, standard_population) {
  age_specific_rates <- df %>%
    group_by(age_group) %>%
    summarise(
      total_events = sum(.data[[event_col]], na.rm = TRUE),
      total_time_at_risk_years = sum(.data[[time_col]], na.rm = TRUE) / 365.25,
      event_rate_per_100000_py = (total_events / total_time_at_risk_years) * 100000,
      variance = (total_events / (total_time_at_risk_years^2)) * (100000^2),  
      .groups = "drop"
    ) %>%
    left_join(standard_population, by = "age_group") %>%
    mutate(
      weighted_rate = event_rate_per_100000_py * proportion,
      weighted_variance = variance * (proportion^2)
    )

  # Compute age-adjusted rate
  age_adjusted_rate <- sum(age_specific_rates$weighted_rate, na.rm = TRUE)

  # Compute standard error
  standard_error <- sqrt(sum(age_specific_rates$weighted_variance, na.rm = TRUE))  
  
  # Compute confidence intervals
  z_value <- qnorm(0.975)  # 95% CI
  ci_lower <- max(0, age_adjusted_rate - z_value * standard_error) 
  ci_upper <- age_adjusted_rate + z_value * standard_error

  return(data.frame(
    Adj_Event_Rate_per_100000_PY = age_adjusted_rate,
    Adj_CI_Lower = ci_lower,
    Adj_CI_Upper = ci_upper
  ))
}

# Function to calculate crude risk differences per 100,000 person-years
calculate_crude_risk_difference <- function(group_df, overall_df, time_col, event_col) {
  group_rates <- group_df %>%
    summarise(
      Events = sum(.data[[event_col]], na.rm = TRUE),
      Time_at_risk_years = sum(.data[[time_col]], na.rm = TRUE) / 365.25
    )

  overall_rates <- overall_df %>%
    summarise(
      Events = sum(.data[[event_col]], na.rm = TRUE),
      Time_at_risk_years = sum(.data[[time_col]], na.rm = TRUE) / 365.25
    )

  group_event_rate <- group_rates$Events / group_rates$Time_at_risk_years
  overall_event_rate <- overall_rates$Events / overall_rates$Time_at_risk_years

  rate_diff <- (group_event_rate - overall_event_rate) * 100000
  se_diff <- sqrt(
    (group_event_rate / group_rates$Time_at_risk_years) +
    (overall_event_rate / overall_rates$Time_at_risk_years)
  ) * 100000
  z_value <- qnorm(0.975)
  ci_lower <- rate_diff - z_value * se_diff
  ci_upper <- rate_diff + z_value * se_diff

  return(data.frame(
    Crude_Risk_Difference_per_100000_PY = rate_diff,
    Crude_RD_CI_Lower = ci_lower,
    Crude_RD_CI_Upper = ci_upper
  ))
}

# Function to calculate age-adjusted risk differences per 100,000 person-years
calculate_age_adjusted_risk_difference <- function(group_df, overall_df, time_col, event_col, standard_population) {
  group_rate <- calculate_age_adjusted_event_rate(group_df, time_col, event_col, standard_population)
  overall_rate <- calculate_age_adjusted_event_rate(overall_df, time_col, event_col, standard_population)

  risk_difference <- group_rate$Adj_Event_Rate_per_100000_PY - overall_rate$Adj_Event_Rate_per_100000_PY
  standard_error <- sqrt(
    (group_rate$Adj_Event_Rate_per_100000_PY - group_rate$Adj_CI_Lower)^2 +
    (overall_rate$Adj_Event_Rate_per_100000_PY - overall_rate$Adj_CI_Lower)^2
  )
  z_value <- qnorm(0.975)
  rd_ci_lower <- risk_difference - z_value * standard_error
  rd_ci_upper <- risk_difference + z_value * standard_error

  return(data.frame(
    Adj_Risk_Difference_per_100000_PY = risk_difference,
    Adj_RD_CI_Lower = rd_ci_lower,
    Adj_RD_CI_Upper = rd_ci_upper
  ))
}


# Analyze data for urban, UAR, NAR, rural, and overall groups
analyze_data <- function() {
  results <- lapply(names(outcomes_and_times), function(event_col) {
    time_col <- outcomes_and_times[[event_col]]

    # Crude event rates
    urban_event_rate <- calculate_event_rate(urban_df, time_col, event_col)
    UAR_event_rate <- calculate_event_rate(UAR_df, time_col, event_col)
    NAR_event_rate <- calculate_event_rate(NAR_df, time_col, event_col)
    rural_event_rate <- calculate_event_rate(rural_df, time_col, event_col)
    overall_event_rate <- calculate_event_rate(overall_df, time_col, event_col)

    # Age-adjusted event rates
    urban_adj_event_rate <- calculate_age_adjusted_event_rate(urban_df, time_col, event_col, standard_population)
    UAR_adj_event_rate <- calculate_age_adjusted_event_rate(UAR_df, time_col, event_col, standard_population)
    NAR_adj_event_rate <- calculate_age_adjusted_event_rate(NAR_df, time_col, event_col, standard_population)
    rural_adj_event_rate <- calculate_age_adjusted_event_rate(rural_df, time_col, event_col, standard_population)
    overall_adj_event_rate <- calculate_age_adjusted_event_rate(overall_df, time_col, event_col, standard_population)

    # Crude risk differences (Comparing to Urban instead of Overall)
    UAR_crude_risk_diff <- calculate_crude_risk_difference(UAR_df, urban_df, time_col, event_col)
    NAR_crude_risk_diff <- calculate_crude_risk_difference(NAR_df, urban_df, time_col, event_col)
    rural_crude_risk_diff <- calculate_crude_risk_difference(rural_df, urban_df, time_col, event_col)

    # Adjusted risk differences (Comparing to Urban instead of Overall)
    UAR_adj_risk_diff <- calculate_age_adjusted_risk_difference(UAR_df, urban_df, time_col, event_col, standard_population)
    NAR_adj_risk_diff <- calculate_age_adjusted_risk_difference(NAR_df, urban_df, time_col, event_col, standard_population)
    rural_adj_risk_diff <- calculate_age_adjusted_risk_difference(rural_df, urban_df, time_col, event_col, standard_population)

    # Combine results
    rbind(
      cbind(data.frame(Rurality = "Urban", Outcome = event_col), 
            urban_event_rate, urban_adj_event_rate, 
            data.frame(Crude_Risk_Difference_per_100000_PY = NA, Crude_RD_CI_Lower = NA, Crude_RD_CI_Upper = NA, 
                       Adj_Risk_Difference_per_100000_PY = NA, Adj_RD_CI_Lower = NA, Adj_RD_CI_Upper = NA)),  
      
      cbind(data.frame(Rurality = "Urban-Adjacent Rural", Outcome = event_col), 
            UAR_event_rate, UAR_adj_event_rate, UAR_crude_risk_diff, UAR_adj_risk_diff),
      
      cbind(data.frame(Rurality = "Nonurban-Adjacent Rural", Outcome = event_col), 
            NAR_event_rate, NAR_adj_event_rate, NAR_crude_risk_diff, NAR_adj_risk_diff),
      
      cbind(data.frame(Rurality = "Rural", Outcome = event_col), 
            rural_event_rate, rural_adj_event_rate, rural_crude_risk_diff, rural_adj_risk_diff),
      
      cbind(data.frame(Rurality = "Overall", Outcome = event_col), 
            overall_event_rate, overall_adj_event_rate, 
            data.frame(Crude_Risk_Difference_per_100000_PY = NA, Crude_RD_CI_Lower = NA, Crude_RD_CI_Upper = NA, 
                       Adj_Risk_Difference_per_100000_PY = NA, Adj_RD_CI_Lower = NA, Adj_RD_CI_Upper = NA)) 
    )
  })

  return(do.call(rbind, results))
}

# Analyze the data
final_results <- analyze_data()

## Event rate and risk difference per person
# Define age groups in the dataset
local_df <- local_df %>%
  mutate(age_group = case_when(
    age_at_covid < 1 ~ "<1",
    age_at_covid >= 1 & age_at_covid <= 4 ~ "1-4",
    age_at_covid >= 5 & age_at_covid <= 14 ~ "5-14",
    age_at_covid >= 15 & age_at_covid <= 24 ~ "15-24",
    age_at_covid >= 25 & age_at_covid <= 34 ~ "25-34",
    age_at_covid >= 35 & age_at_covid <= 44 ~ "35-44",
    age_at_covid >= 45 & age_at_covid <= 54 ~ "45-54",
    age_at_covid >= 55 & age_at_covid <= 64 ~ "55-64",
    age_at_covid >= 65 & age_at_covid <= 74 ~ "65-74",
    age_at_covid >= 75 & age_at_covid <= 84 ~ "75-84",
    age_at_covid >= 85 ~ "85+",
    TRUE ~ NA_character_
  ))

# Standard population proportions for age adjustment (2020 U.S. Standard Population)
standard_population <- data.frame(
  age_group = c("<1", "1-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+"),
  proportion = c(0.012800, 0.051600, 0.134933, 0.132583, 0.134900, 0.135573, 0.122932, 0.122293, 0.087247, 0.044842, 0.020297)
)

# Specify the list of outcomes
outcomes <- c("death") 

# Subset data into different rurality groups
urban_df <- filter(local_df, rurality == "Urban")
UAR_df <- filter(local_df, rurality == "Urban-Adjacent Rural")
NAR_df <- filter(local_df, rurality == "Nonurban-Adjacent Rural")
rural_df <- filter(local_df, rural_binary == "Rural")
overall_df <- local_df

# Function to calculate crude event rates with confidence intervals
calculate_event_rate_per_100000_persons <- function(df, event_col, population_size) {
  total_events <- sum(df[[event_col]], na.rm = TRUE)
  if (population_size > 0 && total_events >= 0) {
    event_rate_per_100000_persons <- (total_events / population_size) * 100000
    ci <- poisson.test(total_events, T = population_size)$conf.int * 100000
    return(data.frame(
      Event_Rate = event_rate_per_100000_persons,
      CI_Lower = ci[1],
      CI_Upper = ci[2],
      Total_Events = total_events
    ))
  } else {
    return(data.frame(Event_Rate = NA, CI_Lower = NA, CI_Upper = NA, Total_Events = total_events))
  }
}

# Function to calculate age-adjusted rates with confidence intervals
calculate_age_adjusted_rate_with_ci <- function(df, event_col, standard_population) {
  age_specific_rates <- df %>%
    group_by(age_group) %>%
    summarise(
      total_events = sum(.data[[event_col]], na.rm = TRUE),
      population_size = n(),
      event_rate_per_100000 = (total_events / population_size) * 100000,
      variance = (total_events / (population_size^2)) * (100000^2), 
      .groups = "drop"
    ) %>%
    left_join(standard_population, by = "age_group") %>%
    mutate(
      weighted_rate = event_rate_per_100000 * proportion,
      weighted_variance = variance * (proportion^2) 
    )

  # Compute age-adjusted rate
  age_adjusted_rate <- sum(age_specific_rates$weighted_rate, na.rm = TRUE)

  # Compute standard error with correct variance weighting
  standard_error <- sqrt(sum(age_specific_rates$weighted_variance, na.rm = TRUE)) 

  # Compute confidence intervals
  z_value <- qnorm(0.975)
  ci_lower <- max(0, age_adjusted_rate - z_value * standard_error) 
  ci_upper <- age_adjusted_rate + z_value * standard_error

  return(data.frame(
    Age_Adjusted_Rate = age_adjusted_rate,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper
  ))
}

# Function to calculate crude risk differences with confidence intervals
calculate_crude_risk_difference <- function(group_df, overall_df, event_col, group_population, overall_population) {
  group_events <- sum(group_df[[event_col]], na.rm = TRUE)
  overall_events <- sum(overall_df[[event_col]], na.rm = TRUE)
  
  group_rate <- (group_events / group_population) * 100000
  overall_rate <- (overall_events / overall_population) * 100000
  
  risk_difference <- group_rate - overall_rate
  se_diff <- sqrt(
    (group_events / (group_population^2)) +
    (overall_events / (overall_population^2))
  ) * 100000
  z_value <- qnorm(0.975)
  ci_lower <- risk_difference - z_value * se_diff
  ci_upper <- risk_difference + z_value * se_diff
  
  return(data.frame(
    Crude_Risk_Difference = risk_difference,
    Crude_RD_CI_Lower = ci_lower,
    Crude_RD_CI_Upper = ci_upper
  ))
}

# Function to calculate age-adjusted risk differences with confidence intervals
calculate_age_adjusted_risk_difference <- function(group_df, overall_df, event_col, standard_population) {
  group_rate <- calculate_age_adjusted_rate_with_ci(group_df, event_col, standard_population)
  overall_rate <- calculate_age_adjusted_rate_with_ci(overall_df, event_col, standard_population)

  risk_difference <- group_rate$Age_Adjusted_Rate - overall_rate$Age_Adjusted_Rate
  standard_error <- sqrt(
    (group_rate$Age_Adjusted_Rate - group_rate$CI_Lower)^2 +
    (overall_rate$Age_Adjusted_Rate - overall_rate$CI_Lower)^2
  )
  z_value <- qnorm(0.975)
  rd_ci_lower <- risk_difference - z_value * standard_error
  rd_ci_upper <- risk_difference + z_value * standard_error

  return(data.frame(
    Risk_Difference = risk_difference,
    RD_CI_Lower = rd_ci_lower,
    RD_CI_Upper = rd_ci_upper
  ))
}


analyze_data <- function() {
  results <- lapply(outcomes, function(event_col) {
    # Crude rates
    urban_rate_info <- calculate_event_rate_per_100000_persons(urban_df, event_col, nrow(urban_df)) %>%
      rename_with(~paste0("Crude_", .))
    UAR_rate_info <- calculate_event_rate_per_100000_persons(UAR_df, event_col, nrow(UAR_df)) %>%
      rename_with(~paste0("Crude_", .))
    NAR_rate_info <- calculate_event_rate_per_100000_persons(NAR_df, event_col, nrow(NAR_df)) %>%
      rename_with(~paste0("Crude_", .))
    rural_rate_info <- calculate_event_rate_per_100000_persons(rural_df, event_col, nrow(rural_df)) %>%
      rename_with(~paste0("Crude_", .))
    overall_rate_info <- calculate_event_rate_per_100000_persons(overall_df, event_col, nrow(overall_df)) %>%
      rename_with(~paste0("Crude_", .))

    # Age-adjusted rates
    urban_age_adjusted_info <- calculate_age_adjusted_rate_with_ci(urban_df, event_col, standard_population) %>%
      rename_with(~paste0("Adj_", .))
    UAR_age_adjusted_info <- calculate_age_adjusted_rate_with_ci(UAR_df, event_col, standard_population) %>%
      rename_with(~paste0("Adj_", .))
    NAR_age_adjusted_info <- calculate_age_adjusted_rate_with_ci(NAR_df, event_col, standard_population) %>%
      rename_with(~paste0("Adj_", .))
    rural_age_adjusted_info <- calculate_age_adjusted_rate_with_ci(rural_df, event_col, standard_population) %>%
      rename_with(~paste0("Adj_", .))
    overall_age_adjusted_info <- calculate_age_adjusted_rate_with_ci(overall_df, event_col, standard_population) %>%
      rename_with(~paste0("Adj_", .))

    # Crude risk differences (Comparing to Urban instead of Overall)
    UAR_crude_risk_diff_info <- calculate_crude_risk_difference(UAR_df, urban_df, event_col, nrow(UAR_df), nrow(urban_df))
    NAR_crude_risk_diff_info <- calculate_crude_risk_difference(NAR_df, urban_df, event_col, nrow(NAR_df), nrow(urban_df))
    rural_crude_risk_diff_info <- calculate_crude_risk_difference(rural_df, urban_df, event_col, nrow(rural_df), nrow(urban_df))

    # Adjusted risk differences (Comparing to Urban instead of Overall)
    UAR_risk_diff_info <- calculate_age_adjusted_risk_difference(UAR_df, urban_df, event_col, standard_population) %>%
      rename_with(~paste0("RD_", .))
    NAR_risk_diff_info <- calculate_age_adjusted_risk_difference(NAR_df, urban_df, event_col, standard_population) %>%
      rename_with(~paste0("RD_", .))
    rural_risk_diff_info <- calculate_age_adjusted_risk_difference(rural_df, urban_df, event_col, standard_population) %>%
      rename_with(~paste0("RD_", .))

    # Combine results into a single data frame
    combined_results <- rbind(
      cbind(data.frame(Rurality = "Urban", Outcome = event_col), urban_rate_info, urban_age_adjusted_info, 
            data.frame(Crude_Risk_Difference = NA, Crude_RD_CI_Lower = NA, Crude_RD_CI_Upper = NA, 
                       RD_Risk_Difference = NA, RD_RD_CI_Lower = NA, RD_RD_CI_Upper = NA)), 
      cbind(data.frame(Rurality = "Urban-Adjacent Rural", Outcome = event_col), UAR_rate_info, UAR_age_adjusted_info, 
            UAR_crude_risk_diff_info, UAR_risk_diff_info),
      cbind(data.frame(Rurality = "Nonurban-Adjacent Rural", Outcome = event_col), NAR_rate_info, NAR_age_adjusted_info, 
            NAR_crude_risk_diff_info, NAR_risk_diff_info),
      cbind(data.frame(Rurality = "Rural", Outcome = event_col), rural_rate_info, rural_age_adjusted_info, 
            rural_crude_risk_diff_info, rural_risk_diff_info),
      cbind(data.frame(Rurality = "Overall", Outcome = event_col), overall_rate_info, overall_age_adjusted_info, 
            data.frame(Crude_Risk_Difference = NA, Crude_RD_CI_Lower = NA, Crude_RD_CI_Upper = NA, 
                       RD_Risk_Difference = NA, RD_RD_CI_Lower = NA, RD_RD_CI_Upper = NA)) 
    )
    return(combined_results)
  })

  return(do.call(rbind, results))
}

# Analyze data
final_results <- analyze_data()


#### Phenotypic abnormalities before death by rurality ####
input_dataset_filtered <- from_rdata(local)

input_dataset_filtered <- filter(input_dataset_filtered, death == 1)

# Filter to relevant data
local_df <- dplyr::select(input_dataset_filtered,
                          rurality,
                          long_covid,
                          reinfection,
                          Abnormality_of_the_respiratory_system,
                          Abnormality_of_the_cardiovascular_system,
                          Abnormality_of_the_immune_system,
                          Abnormality_of_the_nervous_system,
                          Constitutional_symptom,
                          Abnormality_of_metabolism_homeostasis,
                          Abnormality_of_blood_and_blood_forming_tissues,
                          Abnormality_of_the_digestive_system,
                          Abnormality_of_the_endocrine_system,
                          Abnormality_of_the_musculoskeletal_system,
                          Abnormality_of_the_integument,
                          Abnormality_of_the_genitourinary_system,
                          Neoplasm
)

# Create binary variable for any post-covid condition
local_df <- local_df %>%
  mutate(any_post_covid_condition = if_else(
    long_covid == 1 | reinfection == 1 |
      Abnormality_of_the_respiratory_system == 1 | Abnormality_of_the_cardiovascular_system == 1 |
      Abnormality_of_the_immune_system == 1 | Abnormality_of_the_nervous_system == 1 |
      Constitutional_symptom == 1 | Abnormality_of_metabolism_homeostasis == 1 |
      Abnormality_of_blood_and_blood_forming_tissues == 1 | Abnormality_of_the_digestive_system == 1 |
      Abnormality_of_the_endocrine_system == 1 | Abnormality_of_the_musculoskeletal_system == 1 |
      Abnormality_of_the_integument == 1 | Abnormality_of_the_genitourinary_system == 1 |
      Neoplasm == 1, 1, 0
  ))

table1 <- local_df %>% tbl_summary(by=rurality)%>% add_overall() %>% add_p()
print(table1)

