library(gtsummary)
library(rvest)
library(dplyr)
library(tidyverse)

@transform_pandas(
    Output(rid="ri.vector.main.execute.8b7042b4-4993-4da1-a548-1b15dbfc0dc4"),
    survival_data_hosp=Input(rid="ri.foundry.main.dataset.ecb6b1db-103d-4ed0-b69d-fb45b5c13cd2")
)
ROC_data <- function(survival_data_hosp) {
library(survivalROC)
  library(survival)
  library(ranger)
  library(ggplot2)
  library(dplyr)
  library(ggfortify)
  library(viridis)
  library(survminer)
df <- survival_data_hosp
## Define a helper functio nto evaluate at various t
survivalROC_helper <- function(t) {
    survivalROC(Stime        = df$time,
                status       = df$status,
                marker       = ovarian$in_death_table,
                predict.time = t,
                method       = "NNE",
                span = 0.25 * nrow(df)^(-0.20))
}
## Evaluate every 180 days
survivalROC_data <- df() %>% #df(t = 5 * c(1,2,3,4,5,6))
    mutate(survivalROC = map(t, survivalROC_helper),
           ## Extract scalar AUC
           auc = map_dbl(survivalROC, magrittr::extract2, "AUC"),
           ## Put cut off dependent values in a data_frame
           df_survivalROC = map(survivalROC, function(obj) {
               as_data_frame(obj[c("cut.values","TP","FP")])
           })) %>%
    dplyr::select(-survivalROC) %>%
    unnest() %>%
    arrange(t, FP, TP)
## Plot
survivalROC_data %>%
    ggplot(mapping = aes(x = TP, y = FP)) +
    geom_point() +
    geom_line() +
    geom_label(data = survivalROC_data %>% dplyr::select(t,auc) %>% unique,
               mapping = aes(label = sprintf("%.3f", auc)), x = 0.5, y = 0.5) +
    facet_wrap( ~ t) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.key = element_blank(),
          plot.title = element_text(hjust = 0.5),
          strip.background = element_blank())

plot(survivalROC_data)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.e777315c-3472-4f83-bc17-85093faf2761"),
    survival_data_hosp=Input(rid="ri.foundry.main.dataset.ecb6b1db-103d-4ed0-b69d-fb45b5c13cd2")
)
aa_fit <- function(survival_data_hosp) {
  library(survival)
  library(ranger)
  library(ggplot2)
  library(dplyr)
  library(ggfortify)
  print(survival_data_hosp %>% summary())
  local_df <- survival_data_hosp %>% dplyr::select(three_category_ruca, gender, age, age_Group, Race, Ethnicity, BMI_Group, BMI, Q_Score_Categories, Q_Score, Diabetes, MI, CHF, PVD, stroke, dementia, pulmonary, rheumatic, Liver, paralysis, renal, cancer, mets, hiv, Severity_Type, ECMO, MACE, Mechnical_Ventilation, in_death_table, ttdeath, quarter_of_diagnosis, time, status, CCI_INDEX, CCI_Categories)

  # Filter out missing variables
  local_df <- filter(local_df, age_Group != "Unknown/Missing")
  local_df <- filter(local_df, gender != "Other")
  local_df <- filter(local_df, gender != "Unknown")
  local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")

  # Set reference categories
  local_df$Race=relevel(as.factor(local_df$Race),ref="White")
  local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
  local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
  local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
  local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q1 2020")

  aa_fit <-aareg(Surv(time, status) ~ three_category_ruca + factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX + factor(quarter_of_diagnosis) , data = local_df)
  str(aa_fit)

  aa_fit_plot <- autoplot(aa_fit, type = "est_ridge") +
    viridis::scale_fill_viridis(discrete = TRUE) +
    viridis::scale_colour_viridis(discrete = TRUE)

  plot(aa_fit_plot)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.80d572c0-cbdb-4e02-993e-9596d9a9f378")
)
all_adjusted_OR <- function() {
library(ggplot2)
library(dplyr)

# Create labels for plot
df <- data.frame(boxLabels = c("Male vs. Female", "Asian vs. White", "Black or AA vs. White", "Other vs. White", "Race Missing vs. White", "Hispanic or Latino vs. Not Hispanic or Latino", "Ethnicity Missing vs. Not Hispanic or Latino", "Underweight vs. Normal Weight", "Overweight vs. Normal Weight", "Obese vs. Normal Weight", "BMI Missing vs. Normal Weight", "Age", "Charlson Comorbidity Index", "Large Rural vs. Urban", "Small Rural vs. Urban", "Isolated vs. Urban"),
                 boxOdds = c(1.44, 1.05, 1.07, 1.57, 1.21, 0.94, 1.4, 1.3, 0.92, 1.07, 0.53, 1.05, 1.09, 1.87, 2.17, 1.95),
                 boxCILow = c(1.38, 0.93, 1.02, 1.32, 1.21, 0.87, 1.28, 1.14, 0.86, 1, 0.50, 1.05, 1.09, 1.75, 1.99, 1.74),
                 boxCIHigh = c(1.50, 1.18, 1.13, 1.86, 1.31, 1.02, 1.52, 1.48, 0.99, 1.14, 0.57, 1.05, 1.10, 2, 2.36, 2.17)
                 )

p <- df %>%
  mutate(name = fct_relevel(boxLabels,
            "Male vs. Female", "Asian vs. White", "Black or AA vs. White", "Other vs. White", "Race Missing vs. White", "Hispanic or Latino vs. Not Hispanic or Latino", "Ethnicity Missing vs. Not Hispanic or Latino", "Underweight vs. Normal Weight", "Overweight vs. Normal Weight", "Obese vs. Normal Weight", "BMI Missing vs. Normal Weight", "Age", "Charlson Comorbidity Index", "Large Rural vs. Urban", "Small Rural vs. Urban", "Isolated vs. Urban")) %>%
    ggplot( aes(y=boxLabels, x=boxOdds)) +
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
    geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
    geom_point(size = 3.5, color = "red") +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    coord_fixed(ratio=.3) +
      ylab('') +
    xlab('Odds Ratio') +
    #facet_wrap(~cat, ncol=1)+
    annotate(geom='text', y =1.1, x=3.5, label ='',
           size=3.5, hjust=0) + ggtitle('Estimated Odds of All-Cause Mortality') +
  theme(plot.title=element_text(hjust=.5, size=20))
plot(p)
#print(p)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.90182b2c-2543-40d4-8dec-f658b68b9f32"),
    summary_all_for_analysis=Input(rid="ri.foundry.main.dataset.246fccb0-8f84-4ed9-a296-405e603a75e3")
)
all_log_model_only_rurality <- function(summary_all_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary_all_for_analysis %>% summary())
local_df <- summary_all_for_analysis %>% dplyr::select(three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, CCI_INDEX, in_death_table, age, i_rural, quarter_of_diagnosis)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ three_category_ruca, data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.84cf9ca3-81a4-4173-ba79-14abc983274a"),
    summary_table_for_analysis=Input(rid="ri.foundry.main.dataset.46b4856c-26c0-4f80-a469-ccf41839f3b5")
)
all_n3c_summary <- function(summary_table_for_analysis) {
local_df <- summary_table_for_analysis %>% dplyr::select(three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score_Categories, Diabetes, MI, CHF, PVD, stroke, dementia, pulmonary, rheumatic, Liver, paralysis, renal, cancer, mets, hiv, Severity_Type, ECMO, MACE, Mechnical_Ventilation, in_death_table, quarter_of_diagnosis)
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")

table1 <- local_df %>% tbl_summary(by = three_category_ruca) %>% add_p()
table1 <- as_tibble(table1, col_labels = FALSE)
print(table1)
return(table1)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.7b2101e4-5f60-4cc3-9780-21fd5d2f7933")
)
all_unadjusted_OR <- function() {
  library(ggplot2)
  library(dplyr)

  # Create labels for plot
  df <-
    tibble::tibble(
      boxLabels = c("Large Rural vs. Urban", "Small Rural vs. Urban", "Isolated vs. Urban"),
      boxOdds = c(1.8, 2.11, 2.03),
      boxCILow = c(1.69, 1.95, 1.82),
      boxCIHigh = c(1.91, 2.28, 2.25)
    ) %>%
    mutate(
      boxLabels = factor(
        boxLabels,
        c(
          "Large Rural vs. Urban",
          "Small Rural vs. Urban",
          "Isolated vs. Urban"
        )
      ),
      boxLabels = forcats::fct_rev(boxLabels)
    )

  p <-
    df %>%
    ggplot(aes(y=boxLabels, x=boxOdds)) +
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
    geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
    geom_point(size = 3.5, color = "red") +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    coord_fixed(ratio=.3) +
    ylab('') +
    xlab('Odds Ratio') +
    #facet_wrap(~cat, ncol=1)+
    annotate(geom='text', y =1.1, x=3.5, label ='',
             size=3.5, hjust=0) + ggtitle('Estimated Odds of All-Cause Mortality in Entire Population') +
    theme(plot.title=element_text(hjust=.5, size=20))
  plot(p)
  #return(null)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.d4702606-6210-4f8f-847b-338a2d5e48bc"),
    survival_data_hosp=Input(rid="ri.foundry.main.dataset.ecb6b1db-103d-4ed0-b69d-fb45b5c13cd2")
)
cox_fit <- function(survival_data_hosp) {
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
print(survival_data_hosp %>% summary())
local_df <- survival_data_hosp %>% dplyr::select(three_category_ruca, gender, age, age_Group, Race, Ethnicity, BMI_Group, BMI, Q_Score_Categories, Q_Score, Diabetes, MI, CHF, PVD, stroke, dementia, pulmonary, rheumatic, Liver, paralysis, renal, cancer, mets, hiv, Severity_Type, ECMO, MACE, Mechnical_Ventilation, in_death_table, ttdeath, quarter_of_diagnosis, time, status)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Missing")
local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")

cox <- coxph(Surv(time, status) ~ three_category_ruca + factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + Q_Score + factor(quarter_of_diagnosis) , data = local_df)

cox_sum <- summary(cox)

cox_fit <- survfit(cox)

cox_fit_plot <- autoplot(cox_fit)
plot(cox_fit_plot)

}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.50cb3522-2016-4884-af8b-01d080a61674"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
##Main Model

dta_residuals <- function(summary2_for_analysis) {

#all variavles:
#("ttdeath","four_category_ruca","age","i_rural","gender","age_Group","Race","Ethnicity","BMI_Group","BMI","Q_Score_Categories","Q_Score","Diabetes","MI","CHF","PVD","stroke","dementia","pulmonary","rheumatic","Liver","paralysis","renal","cancer","mets","hiv","Severity_Type","ECMO","MACE","Mechnical_Ventilation","in_death_table")
##print(summary2_for_analysis %>% summary())

local_df <- summary2_for_analysis
# %>% dplyr::select(four_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age_Group, i_rural)
#subset(local_df, gender !="Other")

local_df %>% mutate(BMI = ifelse(BMI>200, 200, BMI))

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, quarter_of_diagnosis != "Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$four_category_ruca=relevel(as.factor(local_df$four_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q1 2020")

#m1 <- glm(in_death_table ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + Q_Score + i_rural, data = local_df, family = binomial, maxit = 100)
m1 <- glm(in_death_table ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_Score_Categories) + factor(quarter_of_diagnosis), data = local_df, family = binomial, maxit = 100)
print(summary(m1))

table2 <- local_df %>% tbl_summary(by = four_category_ruca) %>% add_p()
table2 <- as_tibble(table2, col_labels = FALSE)

#summary(m1)$coefficients

#tbl_regression(m1, exponentiate = TRUE)
#table1 <- m1 %>%
#  tbl_regression(exponentiate = TRUE) %>%
#  as_gt() %>%
#  gt::tab_source_note(gt::md("*This data is simulated*"))
#print(table1)

# This section is causing probelms - not sure how to resolve
local_df$rs <- residuals(m1,type=c("deviance"))

return(local_df)
#return(NULL)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3ca80ba2-c251-4504-9f5f-e89e4492b6f9"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
##Main Model

dta_residuals_original <- function(summary2_for_analysis) {

#all variavles:
#("ttdeath","four_category_ruca","age","i_rural","gender","age_Group","Race","Ethnicity","BMI_Group","BMI","Q_Score_Categories","Q_Score","Diabetes","MI","CHF","PVD","stroke","dementia","pulmonary","rheumatic","Liver","paralysis","renal","cancer","mets","hiv","Severity_Type","ECMO","MACE","Mechnical_Ventilation","in_death_table")
##print(summary2_for_analysis %>% summary())

local_df <- summary2_for_analysis
# %>% dplyr::select(four_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age_Group, i_rural)
#subset(local_df, gender !="Other")

local_df %>% mutate(BMI = ifelse(BMI>200, 200, BMI))

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, quarter_of_diagnosis != "Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$four_category_ruca=relevel(as.factor(local_df$four_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q1 2020")

#m1 <- glm(in_death_table ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + Q_Score + i_rural, data = local_df, family = binomial, maxit = 100)
m1 <- glm(in_death_table ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_Score_Categories) + factor(quarter_of_diagnosis), data = local_df, family = binomial, maxit = 100)
print(summary(m1))

table2 <- local_df %>% tbl_summary(by = four_category_ruca) %>% add_p()
table2 <- as_tibble(table2, col_labels = FALSE)

#summary(m1)$coefficients

#tbl_regression(m1, exponentiate = TRUE)
#table1 <- m1 %>%
#  tbl_regression(exponentiate = TRUE) %>%
#  as_gt() %>%
#  gt::tab_source_note(gt::md("*This data is simulated*"))
#print(table1)

local_df$rs <- residuals(m1,type=c("deviance"))

return(local_df)
#return(NULL)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.e05f633b-c5c8-4e0b-91af-2b982546062c"),
    dta_residuals=Input(rid="ri.foundry.main.dataset.50cb3522-2016-4884-af8b-01d080a61674")
)
##Graphs of residuals vs selected vars (both included and excluded)

graph_resid_copied <- function(dta_residuals) {
    fn_resid_1 <- function(dta, variable) {
    dta$variable <- dta[, variable]
    #dta$ACE_ARB <- as.factor(dta$ACE_ARB)
    ggplot(dta, aes(x = variable , y = rs, color = in_death_table)) +
        geom_point(alpha = 0.2, size = 1.5) +
        xlab(variable) +
        ylab("Residuals") +
        theme_bw()
    }
fn_resid_2 <- function(dta, variable) {
    dta$variable <- as.factor(dta[, variable])
    ggplot(dta, aes(x = variable , y = rs, color = "red")) +
        geom_point(alpha = 0.2, size = 1.5) +
        xlab(variable) +
        ylab("Residuals") +
        theme_bw()
    }

local_df2 <-dta_residuals

grid.arrange(
    fn_resid_2(local_df2, "gender") + theme(legend.position = "none"),
    fn_resid_2(local_df2, "Race"),
    fn_resid_2(local_df2, "Ethnicity"),
    fn_resid_2(local_df2, "BMI_Group"),
    fn_resid_1(local_df2, "age"),
    fn_resid_2(local_df2, "Q_Score_Categories"),

    fn_resid_2(local_df2, "age_Group"),
    fn_resid_1(local_df2, "Q_Score"),
    fn_resid_2(local_df2, "four_category_ruca"),
    fn_resid_1(local_df2, "BMI"),
    fn_resid_2(local_df2, "Severity_Type"),
    fn_resid_2(local_df2, "ECMO"),
    fn_resid_2(local_df2, "MACE"),
    fn_resid_2(local_df2, "Mechnical_Ventilation"),

    nrow = 8, widths = c(1,1,1))
    layout_matrix = rbind(c(1,2,3),c(4,5,6),c(NA,NA,NA),c(7,8,9),c(10,11,12),c(13,14,NA))
return (NULL)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.e4c24d7d-a47f-4733-b834-6b2e99b22cc2"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
library(survminer)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(three_category_ruca, gender, age_Group, Race, Ethnicity, BMI, BMI_Group, Q_Score, in_death_table, age, i_rural, quarter_of_diagnosis, Severity_Type, CCI_INDEX, age_Category)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$age_Category=relevel(as.factor(local_df$age_Category),ref="30-49")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q3 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ factor(gender) + factor(Race) + factor(Ethnicity) + BMI + age + CCI_INDEX + factor(quarter_of_diagnosis) + factor(three_category_ruca), data = local_df, family = binomial, maxit = 100)

summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)

return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.d0c5ee58-c360-4070-be66-f8278772e59f"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_bmi <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
library(survminer)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, quarter_of_diagnosis, Severity_Type, CCI_INDEX, age_Category)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
#local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$age_Category=relevel(as.factor(local_df$age_Category),ref="30-49")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q3 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ factor(BMI_Group), data = local_df, family = binomial, maxit = 100)

summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)

return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.4daffff8-bf0d-4de7-b16b-66b84daa7bb4"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_data_partner_only <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(four_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, CCI_INDEX, in_death_table, age, i_rural, quarter_of_diagnosis, Severity_Type, data_partner_id)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$four_category_ruca=relevel(as.factor(local_df$four_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")

m1 <- glm(in_death_table ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX + factor(quarter_of_diagnosis) + factor(data_partner_id), data = local_df, family = binomial, maxit = 100)

# Add a random effect for data partner id?

summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3b591b7a-e8a8-431a-bffc-6a147fa38e8e"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_no_BMI <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, CCI_INDEX, in_death_table, age, i_rural, quarter_of_diagnosis)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

# Model
m1 <- glm(in_death_table ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX + three_category_ruca + factor(quarter_of_diagnosis), data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)

# Store model as table
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.fb0b222b-1ecb-4fb3-8b95-540ccc8c3841"),
    summary_for_analysis_on_hospitalization=Input(rid="ri.foundry.main.dataset.58239702-c61d-4026-9e36-2e75ccd2da01")
)
hosp_log_model_on_admission <- function(summary_for_analysis_on_hospitalization) {
library(gtsummary)
library(rvest)
library(dplyr)
library(survminer)
library(lme4)
print(summary_for_analysis_on_hospitalization %>% summary())
local_df <- summary_for_analysis_on_hospitalization %>% dplyr::select(data_partner_id, three_category_ruca, gender, age_Group, Race, Ethnicity, BMI, BMI_Group, Q_Score, in_death_table, age, i_rural, quarter_of_diagnosis, Severity_Type, CCI_INDEX, age_Category, hospitalized)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$age_Category=relevel(as.factor(local_df$age_Category),ref="30-49")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q3 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glmer(hospitalized ~ factor(gender) + factor(Race) + factor(Ethnicity) + BMI + age + CCI_INDEX + factor(quarter_of_diagnosis) + ( 1 | data_partner_id) + factor(three_category_ruca), data = local_df,  na.action = "na.omit",family = binomial(logit))

summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)

return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.68eb2b56-7bcd-4c8a-9e1d-d0622f76a736"),
    summary_for_analysis_on_hospitalization=Input(rid="ri.foundry.main.dataset.58239702-c61d-4026-9e36-2e75ccd2da01")
)
hosp_log_model_on_admission_only_rurality <- function(summary_for_analysis_on_hospitalization) {
library(gtsummary)
library(rvest)
library(dplyr)
library(survminer)
library(lme4)
print(summary_for_analysis_on_hospitalization %>% summary())
local_df <- summary_for_analysis_on_hospitalization %>% dplyr::select(data_partner_id, three_category_ruca, gender, age_Group, Race, Ethnicity, BMI, BMI_Group, Q_Score, in_death_table, age, i_rural, quarter_of_diagnosis, Severity_Type, CCI_INDEX, age_Category, hospitalized)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$age_Category=relevel(as.factor(local_df$age_Category),ref="30-49")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q3 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glmer(hospitalized ~ ( 1 | data_partner_id) + factor(three_category_ruca), data = local_df,  na.action = "na.omit",family = binomial(logit))

summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)

return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.2a181d1a-4ce8-4413-b1a3-2364cb702d8e"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_only_rurality <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, Severity_Type, quarter_of_diagnosis, CCI_INDEX)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ three_category_ruca, data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.e9bb97d8-2d9b-4a39-8059-929f54492c86"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_only_rurality_[redacted] <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(data_partner_id, three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, Severity_Type, quarter_of_diagnosis, CCI_INDEX)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, data_partner_id == [redacted])

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ three_category_ruca, data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.7e3cd4da-d9ac-4ac1-966d-a019c5fdee9d"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_only_rurality_[redacted] <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(data_partner_id, three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, Severity_Type, quarter_of_diagnosis, CCI_INDEX)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, data_partner_id == [redacted])

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ three_category_ruca, data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.56f3d455-9361-43e5-947d-2351da8e648a"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_only_rurality_[redacted]<- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(data_partner_id, three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, Severity_Type, quarter_of_diagnosis, CCI_INDEX)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, data_partner_id == [redacted])

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ three_category_ruca, data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.caba6f68-3b87-4b20-982b-4d4f7a2a049d"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_only_rurality_[redacted] <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(data_partner_id, three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, Severity_Type, quarter_of_diagnosis, CCI_INDEX)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, data_partner_id == [redacted])

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ three_category_ruca, data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.0bb3d884-e236-452e-9335-d322c917aadf"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_only_rurality_[redacted] <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(data_partner_id, three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, Severity_Type, quarter_of_diagnosis, CCI_INDEX)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, data_partner_id == [redacted])

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ three_category_ruca, data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.675b5cb3-12b6-4481-aaf7-dc8a04c1a538"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_only_rurality_[redacted] <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(data_partner_id, three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, Severity_Type, quarter_of_diagnosis, CCI_INDEX)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, data_partner_id == [redacted])

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ three_category_ruca, data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.d5785f01-1cd1-4056-a5a2-e0891ba4df96"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_only_rurality_[redacted] <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(data_partner_id, three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, Severity_Type, quarter_of_diagnosis, CCI_INDEX)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, data_partner_id == [redacted])

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ three_category_ruca, data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.8f1fd6b0-b2a4-4981-93da-396d81599448"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_only_rurality_[redacted] <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(data_partner_id, three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, Severity_Type, quarter_of_diagnosis, CCI_INDEX)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, data_partner_id == [redacted])

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ three_category_ruca, data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.c7a41a3c-c217-402c-af81-0c69f33c442b"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_only_rurality_[redacted] <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(data_partner_id, three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, Severity_Type, quarter_of_diagnosis, CCI_INDEX)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, data_partner_id == [redacted])

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ three_category_ruca, data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.41bf1b5f-a8e2-4267-9be6-c8f02e11246e"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_only_rurality_[redacted] <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(data_partner_id, three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, Severity_Type, quarter_of_diagnosis, CCI_INDEX)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, data_partner_id == [redacted])

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ three_category_ruca, data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.78fc0fea-8a5f-42d3-9516-4b1bbc899130"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_only_rurality_[redacted] <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(data_partner_id, three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, Severity_Type, quarter_of_diagnosis, CCI_INDEX)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, data_partner_id == [redacted])

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ three_category_ruca, data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.5c77d0eb-d8de-4ca4-9511-dd25a30f76a0"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_only_rurality_[redacted] <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(data_partner_id, three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, Severity_Type, quarter_of_diagnosis, CCI_INDEX)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, data_partner_id == [redacted])

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ three_category_ruca, data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.362b4e31-9404-4ebe-8c02-eac603534743"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_only_rurality_[redacted] <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(data_partner_id, three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, Severity_Type, quarter_of_diagnosis, CCI_INDEX)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, data_partner_id == [redacted])

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ three_category_ruca, data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.158b04a8-36dd-43c8-89ab-6a2a0b6a97ae"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_only_rurality_[redacted] <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(data_partner_id, three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, Severity_Type, quarter_of_diagnosis, CCI_INDEX)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, data_partner_id == [redacted])

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ three_category_ruca, data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f7591870-79eb-4f33-a94e-5769ab2cf843"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_only_rurality_[redacted] <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(data_partner_id, three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, Severity_Type, quarter_of_diagnosis, CCI_INDEX)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, data_partner_id == [redacted])

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ three_category_ruca, data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.768c0faa-d283-4218-a35f-4342a05599d0"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_only_rurality_[redacted] <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(data_partner_id, three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, Severity_Type, quarter_of_diagnosis, CCI_INDEX)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, data_partner_id == [redacted])

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ three_category_ruca, data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.9a450af8-8e74-4384-9517-495fcd8adb15"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_only_rurality_[redacted] <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(data_partner_id, three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, Severity_Type, quarter_of_diagnosis, CCI_INDEX)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, data_partner_id == [redacted])

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ three_category_ruca, data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.44e79c59-20fa-44fd-8665-47eb6b1f6ca4"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_only_rurality_[redacted] <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(data_partner_id, three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, Severity_Type, quarter_of_diagnosis, CCI_INDEX)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, data_partner_id == [redacted])

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ three_category_ruca, data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.52a52182-fb46-44c5-afe7-a638569a47c5"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_only_rurality_[redacted] <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(data_partner_id, three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, Severity_Type, quarter_of_diagnosis, CCI_INDEX)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, data_partner_id == [redacted])

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ three_category_ruca, data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.d2acef97-b569-4d13-91d1-1de2a89fe0de"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_only_rurality_[redacted] <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(data_partner_id, three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, Severity_Type, quarter_of_diagnosis, CCI_INDEX)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, data_partner_id == [redacted])

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ three_category_ruca, data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.feff5cee-e03c-45e9-be58-cec264080213"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_only_rurality_[redacted] <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(data_partner_id, three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, Severity_Type, quarter_of_diagnosis, CCI_INDEX)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, data_partner_id == [redacted])

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ three_category_ruca, data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.7987d250-38a5-4615-ad49-0189ba2bf1a5"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_only_rurality_[redacted] <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(data_partner_id, three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, Severity_Type, quarter_of_diagnosis, CCI_INDEX)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, data_partner_id == [redacted])

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ three_category_ruca, data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3830f0f0-9d16-4d03-83bf-9b42b6ab6828"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_only_rurality_[redacted] <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(data_partner_id, three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, Severity_Type, quarter_of_diagnosis, CCI_INDEX)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, data_partner_id == [redacted])

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ three_category_ruca, data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3603e68f-599a-4abb-b5e2-bd1879b0ddcf"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_only_rurality_[redacted] <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(data_partner_id, three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, Severity_Type, quarter_of_diagnosis, CCI_INDEX)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, data_partner_id == [redacted])

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ three_category_ruca, data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f82a34fa-777b-4fa3-b83c-9a8715fbc9f0"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_only_rurality_[redacted] <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(data_partner_id, three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, Severity_Type, quarter_of_diagnosis, CCI_INDEX)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, data_partner_id == [redacted])

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ three_category_ruca, data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.0ca39769-8020-49ac-a18b-66cf44158de7"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_only_rurality_[redacted] <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(data_partner_id, three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, Severity_Type, quarter_of_diagnosis, CCI_INDEX)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, data_partner_id == [redacted])

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ three_category_ruca, data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.794b5362-840e-4d66-9446-c1877097807c"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_rural_percentage <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
library(survminer)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(three_category_ruca, gender, age_Group, Race, Ethnicity, BMI, BMI_Group, Q_Score, in_death_table, age, i_rural, quarter_of_diagnosis, Severity_Type, CCI_INDEX, age_Category, rural_percentage)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$age_Category=relevel(as.factor(local_df$age_Category),ref="30-49")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q3 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

m1 <- glm(in_death_table ~ factor(gender) + factor(Race) + factor(Ethnicity) + BMI + age + CCI_INDEX + rural_percentage + factor(quarter_of_diagnosis), data = local_df, family = binomial, maxit = 100)

summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)

return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.9d9851b3-d3c0-41ea-bbc8-159fbf6085ff"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_log_model_rurality_only_random_effect <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
library(lme4)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(three_category_ruca, gender, age_Group, Race, Ethnicity, BMI, BMI_Group, Q_Score, in_death_table, age, i_rural, quarter_of_diagnosis, Severity_Type, CCI_INDEX, age_Category, data_partner_id)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$age_Category=relevel(as.factor(local_df$age_Category),ref="30-49")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q3 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

#m1 <- glmer(in_death_table ~ gender + Race + Ethnicity + BMI_Group + age + CCI_INDEX + quarter_of_diagnosis +
# 1 | rural_percentage), data = local_df, family = binomial, maxit = 100)

m1 <- glmer(in_death_table ~  factor(three_category_ruca) + ( 1 | data_partner_id), data = local_df,  na.action = "na.omit", family = binomial(logit))

# Add a random effect for data partner id?

summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.90a328ed-0f1d-41df-b5c0-6e4192eb874b"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
hosp_model_data_partner_random_effect <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
library(lme4)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(three_category_ruca, gender, age_Group, Race, Ethnicity, BMI, BMI_Group, Q_Score, in_death_table, age, i_rural, quarter_of_diagnosis, Severity_Type, CCI_INDEX, age_Category, data_partner_id)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$age_Category=relevel(as.factor(local_df$age_Category),ref="30-49")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q3 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

#m1 <- glmer(in_death_table ~ gender + Race + Ethnicity + BMI_Group + age + CCI_INDEX + quarter_of_diagnosis +
# 1 | rural_percentage), data = local_df, family = binomial, maxit = 100)

m1 <- glmer(in_death_table ~ factor(gender) + factor(Race) + factor(Ethnicity) + BMI + age + CCI_INDEX + factor(quarter_of_diagnosis) + ( 1 | data_partner_id) + factor(three_category_ruca), data = local_df,  na.action = "na.omit",family = binomial(logit))

# Add a random effect for data partner id?

summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.ff5170d6-6037-4554-8c2f-6d1bbe951c81")
)
hosp_unadjusted_OR <- function() {
  library(ggplot2)
  library(dplyr)

  # Create labels for plot
  df <-
    tibble::tibble(
      boxLabels = c("Large Rural vs. Urban", "Small Rural vs. Urban", "Isolated vs. Urban"),
      boxOdds = c(1.83, 2.04, 2.03),
      boxCILow = c(1.73, 1.89, 1.83),
      boxCIHigh = c(1.94, 2.20, 2.25)
    ) %>%
    mutate(
      boxLabels = factor(
        boxLabels,
        c(
          "Large Rural vs. Urban",
          "Small Rural vs. Urban",
          "Isolated vs. Urban"
        )
      ),
      boxLabels = forcats::fct_rev(boxLabels)
    )

  p <-
    df %>%
    ggplot(aes(y=boxLabels, x=boxOdds)) +
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
    geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
    geom_point(size = 3.5, color = "red") +
    rightlabs = c("g","95% CI","p-value") +
    leftlabs = c("Covariate") +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    coord_fixed(ratio=.3) +
    ylab('') +
    xlab('Odds Ratio') +
    #facet_wrap(~cat, ncol=1)+
    annotate(geom='text', y =1.1, x=3.5, label ='',
             size=3.5, hjust=0) + ggtitle('Estimated Odds of All-Cause Mortality in Hospitalized Population') +
    theme(plot.title=element_text(hjust=.5, size=20))
  plot(p)
  #return(null)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.71a5a6f7-7589-4b34-a0a0-ba1f8a95bfbb"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
inpatient_summary <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
library(tidyverse)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(three_category_ruca, gender, age_Category, Race, Ethnicity, BMI_Group, BMI, hypertension, Diabetes, MI, CHF, PVD, stroke, dementia, pulmonary, rheumatic, Liver, paralysis, renal, cancer, mets, hiv, multiple, smoking_status, Severity_Type, oxygen_saturation_lab_value, supplemental_oxygen, Mechnical_Ventilation, Invasive_Ventilation, ECMO, MACE, ecmo_mace_invasive_vent_aki, Mechnical_Ventilation, in_death_table, ttdeath, readmission,
quarter_of_diagnosis, CCI_INDEX, CCI_Categories, Dexamethasone, Hydrocortisone, Prednisone_Methylprednisolone, Prednisolone, Chloroquine_Hydroxychloroquine)

# Filter out missing variables
local_df <- filter(local_df, age_Category != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

table1 <- local_df %>% tbl_summary(by = three_category_ruca) %>% add_p()
table1 <- as_tibble(table1, col_labels = FALSE)
print(table1)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.8a7b57b6-318d-49f9-b65c-c3653b8fba2e"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
inpatient_summary_no_missing_BMI <- function(summary2_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
library(tidyverse)
print(summary2_for_analysis %>% summary())
local_df <- summary2_for_analysis %>% dplyr::select(three_category_ruca, gender, age_Group, age_Category, Race, Ethnicity, BMI_Group, BMI, Q_Score_Categories, Q_Score, hypertension, Diabetes, MI, CHF, PVD, stroke, dementia, pulmonary, rheumatic, Liver, paralysis, renal, cancer, mets, hiv, alcoholism, opioid, smoking_status, smoking, substance_abuse, supplemental_oxygen, Mechnical_Ventilation, readmission, ECMO, MACE, ecmo_mace_invasive_vent_aki, in_death_table, ttdeath, quarter_of_diagnosis, CCI_INDEX, CCI_Categories)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, age_Group != "Unknown")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Missing")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
#local_df <- filter(local_df, BMI_Group != "Unknown/Missing")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

# Produce and store summary table
table1 <- local_df %>% tbl_summary(by = three_category_ruca) %>% add_p()
table1 <- as_tibble(table1, col_labels = FALSE)
print(table1)
return(table1)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.c47915a6-6ab5-4dac-9274-2d6d09501670"),
    survival_data_hosp=Input(rid="ri.foundry.main.dataset.ecb6b1db-103d-4ed0-b69d-fb45b5c13cd2")
)
km_AG_fit <- function(survival_data_hosp) {
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)

print(survival_data_hosp %>% summary())
local_df <- survival_data_hosp %>% dplyr::select(three_category_ruca, gender, age, age_Group, Race, Ethnicity, BMI_Group, BMI, Q_Score_Categories, Q_Score, Diabetes, MI, CHF, PVD, stroke, dementia, pulmonary, rheumatic, Liver, paralysis, renal, cancer, mets, hiv, Severity_Type, ECMO, MACE, Mechnical_Ventilation, in_death_table, ttdeath, quarter_of_diagnosis, time, status, age_Category)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")

#local_df <- filter(local_df, age_Group != "Unknown/Missing")
#local_df <- filter(local_df, gender != "Other")
#local_df <- filter(local_df, gender != "Unknown")
#local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$age_Category <- factor(local_df$age_Category, levels=c("<18", "18-29", "30-49", "50-64", ">=65"))

km <- with(local_df, Surv(time, status))
head(km,80)

km_fit <- survfit(Surv(time, status) ~ 1, data=local_df)
summary(km_fit, times = c(1,5,10,15,20,25,30))

km_plot <- autoplot(km_fit)
#plot(km_plot)

km_trt_fit <- survfit(Surv(time, status) ~ three_category_ruca, data=local_df)
km_trt_fit_plot <- autoplot(km_trt_fit)
#plot(km_trt_fit_plot)

km_AG_fit <- survfit(Surv(time, status) ~ age_Category, data=local_df)
#km_trt_fit$panel$layout$strata <- c("<18", "18-29", "30-39", "40-49", "50-59", "60-69", "70-79", ">80")

str(km_AG_fit)

km_AG_fit_plot <- autoplot(km_AG_fit)

km_AG_fit_plot <- autoplot(km_AG_fit, type = "est_ridge", CI=TRUE, pval=TRUE, plotTable=TRUE, ylim = c(0.75,1), divideTime=5,
                           legendLabs=c("<18", "18-29", "30-49", "50-64", ">=65")) +
  theme(
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 14)
  ) +
  viridis::scale_fill_viridis(discrete = TRUE) +
  viridis::scale_colour_viridis(discrete = TRUE) +
 labs(x = "\n Survival Time (Days) ", y = "Survival Probabilities \n")

plot(km_AG_fit_plot)

}

@transform_pandas(
    Output(rid="ri.vector.main.execute.fc60e7a3-bbed-49f9-9880-c626622ce2c9"),
    survival_data_hosp=Input(rid="ri.foundry.main.dataset.ecb6b1db-103d-4ed0-b69d-fb45b5c13cd2")
)
km_AG_fit_copied <- function(survival_data_hosp) {
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)

print(survival_data_hosp %>% summary())
local_df <- survival_data_hosp %>% dplyr::select(three_category_ruca, gender, age, age_Group, Race, Ethnicity, BMI_Group, BMI, Q_Score_Categories, Q_Score, Diabetes, MI, CHF, PVD, stroke, dementia, pulmonary, rheumatic, Liver, paralysis, renal, cancer, mets, hiv, Severity_Type, ECMO, MACE, Mechnical_Ventilation, in_death_table, ttdeath, quarter_of_diagnosis, time, status, age_Category)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")

km <- with(local_df, Surv(time, status))
head(km,80)

km_fit <- survfit(Surv(time, status) ~ 1, data=local_df)
summary(km_fit, times = c(1,5,10,15,20,25,30))

km_plot <- autoplot(km_fit)
#plot(km_plot)

km_trt_fit <- survfit(Surv(time, status) ~ quarter_of_diagnosis, data=local_df)
km_trt_fit_plot <- autoplot(km_trt_fit)
#plot(km_trt_fit_plot)

km_AG_fit <- survfit(Surv(time, status) ~ quarter_of_diagnosis, data=local_df)
km_trt_fit$panel$layout$strata <- c("Q1 2020", "Q1 2021", "Q2 2020", "Q3 2020", "Q4 2020")

str(km_AG_fit)

km_AG_fit_plot <- autoplot(km_AG_fit)

km_AG_fit_plot <- autoplot(km_AG_fit, type = "est_ridge", CI=TRUE, pval=TRUE, plotTable=TRUE, divideTime=5,
                           legendLabs=cc("Q1 2020", "Q1 2021", "Q2 2020", "Q3 2020", "Q4 2020"),
                           title="Time to all-cause mortality \n
                           by quarter of diagnosis, for COVID-19 positive patients") +
                           viridis::scale_fill_viridis(discrete = TRUE) +
                           viridis::scale_colour_viridis(discrete = TRUE)

plot(km_AG_fit_plot, pval=TRUE)

}

@transform_pandas(
    Output(rid="ri.vector.main.execute.e5e1c61b-3477-4e9d-a494-fe9f82fd0061"),
    survival_data_hosp=Input(rid="ri.foundry.main.dataset.ecb6b1db-103d-4ed0-b69d-fb45b5c13cd2")
)
km_fit <- function(survival_data_hosp) {
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
print(survival_data_hosp %>% summary())
local_df <- survival_data_hosp %>% dplyr::select(three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, BMI, Q_Score_Categories, Q_Score, Diabetes, MI, CHF, PVD, stroke, dementia, pulmonary, rheumatic, Liver, paralysis, renal, cancer, mets, hiv, Severity_Type, ECMO, MACE, Mechnical_Ventilation, in_death_table, ttdeath, quarter_of_diagnosis, time, status)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")

km <- with(local_df, Surv(time, status))
head(km,80)

km_fit <- survfit(Surv(time, status) ~ 1, data=local_df)
summary(km_fit, times = c(1,5,10,15,20,25,30))

km_plot <- autoplot(km_fit)
plot(km_plot)

km_trt_fit <- survfit(Surv(time, status) ~ three_category_ruca, data=local_df)
km_trt_fit_plot <- autoplot(km_trt_fit)
#plot(km_trt_fit_plot)

}

@transform_pandas(
    Output(rid="ri.vector.main.execute.dc92df20-da83-42e5-9edf-73c97731a42e"),
    survival_data_hosp=Input(rid="ri.foundry.main.dataset.ecb6b1db-103d-4ed0-b69d-fb45b5c13cd2")
)
km_fit_plot <- function(survival_data_hosp) {
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(wesanderson)
library(survminer)

print(survival_data_hosp %>% summary())
local_df <- survival_data_hosp %>% dplyr::select(three_category_ruca, gender, age, age_Group, Race, Ethnicity, BMI_Group, BMI, Q_Score_Categories, Q_Score, Diabetes, MI, CHF, PVD, stroke, dementia, pulmonary, rheumatic, Liver, paralysis, renal, cancer, mets, hiv, Severity_Type, ECMO, MACE, Mechnical_Ventilation, in_death_table, ttdeath, quarter_of_diagnosis, time, status)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, quarter_of_diagnosis != "Missing")
local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")

km <- with(local_df, Surv(time, status))
head(km,80)

km_fit <- survfit(Surv(time, status) ~ 1, data=local_df)
summary(km_fit, times = c(1,5,10,15,20,25,30))

km_plot <- autoplot(km_fit)
#plot(km_plot)

pal <- wes_palette("Zissou1", 100, type = "continuous")

fit <- survfit(Surv(time, status) ~ three_category_ruca + factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + Q_Score + factor(quarter_of_diagnosis), data=local_df)

ggsurv <- ggsurvplot(fit, fun = "event", conf.int = TRUE,
                     ggtheme = theme_bw())

ggsurv$plot +theme_bw() +
  theme (legend.position = "right")+
  facet_grid(rx ~ adhere)


plot(ggsurv)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.1ac9f016-7b83-4c88-a22a-ec93eb69f0e4"),
    survival_data_hosp=Input(rid="ri.foundry.main.dataset.ecb6b1db-103d-4ed0-b69d-fb45b5c13cd2")
)
km_trt_fit <- function(survival_data_hosp) {
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(wesanderson)
library(survminer)

print(survival_data_hosp %>% summary())
local_df <- survival_data_hosp %>% dplyr::select(three_category_ruca, gender, age, irural, age_Group, Race, Ethnicity, BMI_Group, BMI, Q_Score_Categories, Q_Score, Diabetes, MI, CHF, PVD, stroke, dementia, pulmonary, rheumatic, Liver, paralysis, renal, cancer, mets, hiv, Severity_Type, ECMO, MACE, Mechnical_Ventilation, in_death_table, ttdeath, quarter_of_diagnosis, time, status, rural_categories)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")

#local_df <- filter(local_df, age_Group != "Unknown")
#local_df <- filter(local_df, gender != "Unknown")
#local_df <- filter(local_df, gender != "Other")
#local_df <- filter(local_df, quarter_of_diagnosis != "Missing")
#local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
#local_df$rural_categories=relevel(as.factor(local_df$rural_categories),ref="Urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$rural_categories <- factor(local_df$rural_categories, levels=c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))

km <- with(local_df, Surv(time, status))
head(km,80)

km_fit <- survfit(Surv(time, status) ~ 1, data=local_df)
summary(km_fit, times = c(1,5,10,15,20,25,30))

km_plot <- autoplot(km_fit, pVal=TRUE)
#plot(km_plot)

#km_trt_fit <- survfit(Surv(time, status) ~ three_category_ruca, data=local_df)
km_trt_fit <- survfit(Surv(time, status) ~ rural_categories, data=local_df)

km_trt_fit$panel$layout$names <- c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural")
str(km_trt_fit)

km_trt_fit_plot <- autoplot(km_trt_fit)
km_trt_fit_plot <- autoplot(km_trt_fit, type = "est_ridge", CI=TRUE, pval=TRUE, plotTable=TRUE,  ylim = c(0.75,1), divideTime=5,
 legendLabs=c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjance Rural")) +
 #theme(legend.position = c(0.125,0.125)) +
  theme(
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 14)
  ) +
  viridis::scale_fill_viridis(discrete = TRUE) +
  viridis::scale_colour_viridis(discrete = TRUE) +
 labs(x = "\n Survival Time (Days) ", y = "Survival Probabilities \n")
 #title = "Survival Curves by Rural Category in Hospitalized Patients over 30 Days from Day of Admission")

plot(km_trt_fit_plot)
print(km_trt_fit)
return(km_trt_fit_plot)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.002b58ba-747e-4980-8aac-73bdd7c7e2f3"),
    survival_data_hosp=Input(rid="ri.foundry.main.dataset.ecb6b1db-103d-4ed0-b69d-fb45b5c13cd2")
)
km_trt_fit_age <- function(survival_data_hosp) {
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(wesanderson)
library(survminer)

print(survival_data_hosp %>% summary())
local_df <- survival_data_hosp %>% dplyr::select(three_category_ruca, gender, age, irural, age_Group, Race, Ethnicity, BMI_Group, BMI, Q_Score_Categories, Q_Score, Diabetes, MI, CHF, PVD, stroke, dementia, pulmonary, rheumatic, Liver, paralysis, renal, cancer, mets, hiv, Severity_Type, ECMO, MACE, Mechnical_Ventilation, in_death_table, ttdeath, quarter_of_diagnosis, time, status, rural_categories, CCI_Categories)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, quarter_of_diagnosis != "Missing")
local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
#local_df$rural_categories=relevel(as.factor(local_df$rural_categories),ref="Urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$age_Group <- factor(local_df$age_Group, levels=c("<18", "18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))

km <- with(local_df, Surv(time, status))
head(km,80)

km_fit <- survfit(Surv(time, status) ~ 1, data=local_df)
summary(km_fit, times = c(1,5,10,15,20,25,30))

km_plot <- autoplot(km_fit, pVal=TRUE)
#plot(km_plot)

#km_trt_fit <- survfit(Surv(time, status) ~ three_category_ruca, data=local_df)
km_trt_fit <- survfit(Surv(time, status) ~ age_Group, data=local_df)

km_trt_fit$panel$layout$names <- c("<18", "18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
str(km_trt_fit)

km_trt_fit_plot <- autoplot(km_trt_fit)
km_trt_fit_plot <- autoplot(km_trt_fit, type = "est_ridge", CI=TRUE, pval=TRUE, plotTable=TRUE,  ylim = c(0.75,1), divideTime=5,
 legendLabs=c("<18", "18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")) +
 theme(legend.position = c(0.15,0.15)) +
 theme(
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 14)
  ) +
  viridis::scale_fill_viridis(discrete = TRUE) +
  viridis::scale_colour_viridis(discrete = TRUE) +
 labs(x = "\n Survival Time (Days) ", y = "Survival Probabilities \n")
 #title = "Survival Curves by CCI Category in Hospitalized Patients over 30 Days from Day of Admission")

plot(km_trt_fit_plot)
print(km_trt_fit)
return(km_trt_fit_plot)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.7d607193-1c07-417b-be61-cfffb5aa4d2b"),
    survival_data_hosp=Input(rid="ri.foundry.main.dataset.ecb6b1db-103d-4ed0-b69d-fb45b5c13cd2")
)
km_trt_fit_bmi <- function(survival_data_hosp) {
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(wesanderson)
library(survminer)

print(survival_data_hosp %>% summary())
local_df <- survival_data_hosp %>% dplyr::select(three_category_ruca, gender, age, irural, age_Group, Race, Ethnicity, BMI_Group, BMI, Q_Score_Categories, Q_Score, Diabetes, MI, CHF, PVD, stroke, dementia, pulmonary, rheumatic, Liver, paralysis, renal, cancer, mets, hiv, Severity_Type, ECMO, MACE, Mechnical_Ventilation, in_death_table, ttdeath, quarter_of_diagnosis, time, status, rural_categories, CCI_Categories)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")

#local_df <- filter(local_df, age_Group != "Unknown")
#local_df <- filter(local_df, gender != "Other")
#local_df <- filter(local_df, gender != "Unknown")
#local_df <- filter(local_df, quarter_of_diagnosis != "Missing")
#local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
#local_df <- filter(local_df, BMI_Group != "Unknown/Missing")
local_df$BMI_Group <- factor(local_df$BMI_Group, levels=c("<18.5", "18.5-24.9", "25-29.9", ">30", "Unknown/Missing"))

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
#local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
#local_df$rural_categories=relevel(as.factor(local_df$rural_categories),ref="Urban")
#local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
#local_df$quarter_of_diagnosis <- factor(local_df$quarter_of_diagnosis, levels=c("Q1 2020", "Q2 2020", "Q3 2020", "Q4 2020", "Q1 2021"))

km <- with(local_df, Surv(time, status))
head(km,80)

km_fit <- survfit(Surv(time, status) ~ 1, data=local_df)
summary(km_fit, times = c(1,5,10,15,20,25,30))

km_plot <- autoplot(km_fit, pVal=TRUE)
#plot(km_plot)

#km_trt_fit <- survfit(Surv(time, status) ~ three_category_ruca, data=local_df)
km_trt_fit <- survfit(Surv(time, status) ~ BMI_Group, data=local_df)

km_trt_fit$panel$layout$names <- c("<18.5", "18.5-24.9", "25-29.9", ">30", "Unknown/Missing")
str(km_trt_fit)

km_trt_fit_plot <- autoplot(km_trt_fit)
km_trt_fit_plot <- autoplot(km_trt_fit, type = "est_ridge", CI=TRUE, pval=TRUE, plotTable=TRUE,  ylim = c(0.75,1), divideTime=5,
 legendLabs=c("<18.5", "18.5-24.9", "25-29.9", ">30", "Unknown/Missing")) +
 #theme(legend.position = c(0.15,0.15)) +
 theme(
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 14)
  ) +
  viridis::scale_fill_viridis(discrete = TRUE) +
  viridis::scale_colour_viridis(discrete = TRUE) +
 labs(x = "\n Survival Time (Days) ", y = "Survival Probabilities \n")
 #title = "Survival Curves by CCI Category in Hospitalized Patients over 30 Days from Day of Admission")

plot(km_trt_fit_plot)
print(km_trt_fit)
return(km_trt_fit_plot)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.e7aed3a7-1213-4361-975d-fd0fcdea2af3"),
    survival_data_hosp=Input(rid="ri.foundry.main.dataset.ecb6b1db-103d-4ed0-b69d-fb45b5c13cd2")
)
km_trt_fit_cci <- function(survival_data_hosp) {
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(wesanderson)
library(survminer)

print(survival_data_hosp %>% summary())
local_df <- survival_data_hosp %>% dplyr::select(three_category_ruca, gender, age, irural, age_Group, Race, Ethnicity, BMI_Group, BMI, Q_Score_Categories, Q_Score, Diabetes, MI, CHF, PVD, stroke, dementia, pulmonary, rheumatic, Liver, paralysis, renal, cancer, mets, hiv, Severity_Type, ECMO, MACE, Mechnical_Ventilation, in_death_table, ttdeath, quarter_of_diagnosis, time, status, rural_categories, CCI_Categories)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")

#local_df <- filter(local_df, age_Group != "Unknown")
#local_df <- filter(local_df, gender != "Other")
#local_df <- filter(local_df, quarter_of_diagnosis != "Missing")
#local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
#local_df <- filter(local_df, BMI_Group != "Unknown/Missing")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
#local_df$rural_categories=relevel(as.factor(local_df$rural_categories),ref="Urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$CCI_Categories <- factor(local_df$CCI_Categories, levels=c("<1.0", "1.0-2.0", ">2.0"))

km <- with(local_df, Surv(time, status))
head(km,80)

km_fit <- survfit(Surv(time, status) ~ 1, data=local_df)
summary(km_fit, times = c(1,5,10,15,20,25,30))

km_plot <- autoplot(km_fit, pVal=TRUE)
#plot(km_plot)

#km_trt_fit <- survfit(Surv(time, status) ~ three_category_ruca, data=local_df)
km_trt_fit <- survfit(Surv(time, status) ~ CCI_Categories, data=local_df)

km_trt_fit$panel$layout$names <- c("<1.0", "1.0-2.0", ">2.0")
str(km_trt_fit)

km_trt_fit_plot <- autoplot(km_trt_fit)
km_trt_fit_plot <- autoplot(km_trt_fit, type = "est_ridge", CI=TRUE, pval=TRUE, plotTable=TRUE,  ylim = c(0.75,1), divideTime=5,
 legendLabs=c("<1.0", "1.0-2.0", ">2.0")) +
 risk.table = TRUE,       # show risk table.
    pval = TRUE,             # show p-value of log-rank test.
    conf.int = TRUE +
 #theme(legend.position = c(0.125,0.125)) +
  theme(
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 14)
  ) +
  viridis::scale_fill_viridis(discrete = TRUE) +
  viridis::scale_colour_viridis(discrete = TRUE) +
 labs(x = "\n Survival Time (Days) ", y = "Survival Probabilities \n") +
 #title = "Survival Curves by CCI Category in Hospitalized Patients over 30 Days from Day of Admission")

fit <- survival::survfit(Surv(time, status) ~ sex, data = lung)
autotab_risk(fit, 50) %>%
ggplot(aes(x = time, y = strata_id, label = n.risk, group= strata)) +
geom_text()

plot(km_trt_fit_plot)
print(km_trt_fit)
return(km_trt_fit_plot)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.6572de3c-cc16-4928-84ef-a89ae1894e65"),
    survival_data_hosp=Input(rid="ri.foundry.main.dataset.ecb6b1db-103d-4ed0-b69d-fb45b5c13cd2")
)
km_trt_fit_cci_copied_1 <- function(survival_data_hosp) {
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(wesanderson)
library(survminer)

print(survival_data_hosp %>% summary())
local_df <- survival_data_hosp %>% dplyr::select(three_category_ruca, gender, age, irural, age_Group, Race, Ethnicity, BMI_Group, BMI, Q_Score_Categories, Q_Score, Diabetes, MI, CHF, PVD, stroke, dementia, pulmonary, rheumatic, Liver, paralysis, renal, cancer, mets, hiv, Severity_Type, ECMO, MACE, Mechnical_Ventilation, in_death_table, ttdeath, quarter_of_diagnosis, time, status, rural_categories, CCI_Categories)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, quarter_of_diagnosis != "Missing")
local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
#local_df$rural_categories=relevel(as.factor(local_df$rural_categories),ref="Urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$CCI_Categories <- factor(local_df$CCI_Categories, levels=c("<1.0", "1.0-2.0", ">2.0"))

km <- with(local_df, Surv(time, status))
head(km,80)

km_fit <- survfit(Surv(time, status) ~ 1, data=local_df)
summary(km_fit, times = c(1,5,10,15,20,25,30))

km_plot <- autoplot(km_fit, pVal=TRUE)
#plot(km_plot)

#km_trt_fit <- survfit(Surv(time, status) ~ three_category_ruca, data=local_df)
km_trt_fit <- survfit(Surv(time, status) ~ CCI_Categories, data=local_df)

km_trt_fit$panel$layout$names <- c("<1.0", "1.0-2.0", ">2.0")
str(km_trt_fit)

km_trt_fit_plot <- autoplot(km_trt_fit)
km_trt_fit_plot <- autoplot(km_trt_fit, type = "est_ridge", CI=TRUE, pval=TRUE, plotTable=TRUE,  ylim = c(0.75,1), divideTime=5,
 legendLabs=c("<1.0", "1.0-2.0", ">2.0")) +
 theme(legend.position = c(0.1,0.1)) +
  viridis::scale_fill_viridis(discrete = TRUE) +
  viridis::scale_colour_viridis(discrete = TRUE) +
 labs(x = "\n Survival Time (Days) ", y = "Survival Probabilities \n")
 #title = "Survival Curves by CCI Category in Hospitalized Patients over 30 Days from Day of Admission")

plot(km_trt_fit_plot)
print(km_trt_fit)
return(km_trt_fit_plot)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.33b7183c-3137-4e8b-b264-ec2cb3300f2f"),
    survival_data_hosp=Input(rid="ri.foundry.main.dataset.ecb6b1db-103d-4ed0-b69d-fb45b5c13cd2")
)
km_trt_fit_quarter_of_diagnosis <- function(survival_data_hosp) {
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(wesanderson)
library(survminer)

print(survival_data_hosp %>% summary())
local_df <- survival_data_hosp %>% dplyr::select(three_category_ruca, gender, age, irural, age_Group, Race, Ethnicity, BMI_Group, BMI, Q_Score_Categories, Q_Score, Diabetes, MI, CHF, PVD, stroke, dementia, pulmonary, rheumatic, Liver, paralysis, renal, cancer, mets, hiv, Severity_Type, ECMO, MACE, Mechnical_Ventilation, in_death_table, ttdeath, quarter_of_diagnosis, time, status, rural_categories, CCI_Categories)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")

#local_df <- filter(local_df, age_Group != "Unknown")
#local_df <- filter(local_df, gender != "Other")
#local_df <- filter(local_df, gender != "Unknown")
#local_df <- filter(local_df, quarter_of_diagnosis != "Missing")
#local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
#local_df <- filter(local_df, BMI_Group != "Unknown/Missing")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
#local_df$rural_categories=relevel(as.factor(local_df$rural_categories),ref="Urban")
#local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$quarter_of_diagnosis <- factor(local_df$quarter_of_diagnosis, levels=c("Q1 2020", "Q2 2020", "Q3 2020", "Q4 2020", "Q1 2021"))

km <- with(local_df, Surv(time, status))
head(km,80)

km_fit <- survfit(Surv(time, status) ~ 1, data=local_df)
summary(km_fit, times = c(1,5,10,15,20,25,30))

km_plot <- autoplot(km_fit, pVal=TRUE)
#plot(km_plot)

#km_trt_fit <- survfit(Surv(time, status) ~ three_category_ruca, data=local_df)
km_trt_fit <- survfit(Surv(time, status) ~ quarter_of_diagnosis, data=local_df)

km_trt_fit$panel$layout$names <- c("Q1 2020", "Q2 2020", "Q3 2020", "Q4 2020", "Q1 2021")
str(km_trt_fit)

km_trt_fit_plot <- autoplot(km_trt_fit)
km_trt_fit_plot <- autoplot(km_trt_fit, type = "est_ridge", CI=TRUE, pval=TRUE, plotTable=TRUE,  ylim = c(0.75,1), divideTime=5,
 legendLabs=c("Q1 2020", "Q2 2020", "Q3 2020", "Q4 2020", "Q1 2021")) +
 #theme(legend.position = c(0.15,0.15)) +
 theme(
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 14)
  ) +
  viridis::scale_fill_viridis(discrete = TRUE) +
  viridis::scale_colour_viridis(discrete = TRUE) +
 labs(x = "\n Survival Time (Days) ", y = "Survival Probabilities \n")
 #title = "Survival Curves by CCI Category in Hospitalized Patients over 30 Days from Day of Admission")

plot(km_trt_fit_plot)
print(km_trt_fit)
return(km_trt_fit_plot)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.2d175d15-5d7c-4bef-a082-a3d965727de9"),
    summary_all_for_analysis=Input(rid="ri.foundry.main.dataset.246fccb0-8f84-4ed9-a296-405e603a75e3")
)
log_all <- function(summary_all_for_analysis) {

print(summary_all_for_analysis %>% summary())
local_df <- summary_all_for_analysis %>% dplyr::select(three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, quarter_of_diagnosis, Severity_Type, CCI_INDEX)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
m1 <- glm(in_death_table ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX + factor(three_category_ruca) + factor(quarter_of_diagnosis), data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.77940589-f666-4e40-9319-901dd1ab02fb"),
    summary_all_for_analysis=Input(rid="ri.foundry.main.dataset.246fccb0-8f84-4ed9-a296-405e603a75e3")
)
log_all_adm_outcome <- function(summary_all_for_analysis) {

print(summary_all_for_analysis %>% summary())
local_df <- summary_all_for_analysis %>% dplyr::select(three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, quarter_of_diagnosis, Severity_Type, admission_status)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, Severity_Type != "Mild")
local_df <- filter(local_df, Severity_Type != "Mild_ED")
local_df <- filter(local_df, Race != "Asian")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
#local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q1 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

# Model with admission_status as outcome
m1 <- glm(admission_status ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + Q_Score + factor(three_category_ruca) + factor(quarter_of_diagnosis), data = local_df, family = binomial, maxit = 100)
summary(m1)$coefficients
print (summary(m1)$coefficients)

# Save model to dataset
table1 <- m1 %>%
  tbl_regression(exponentiate = TRUE)
table1 <- as_tibble(table1, col_labels = FALSE)
return(table1)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.2cc2b17e-415e-44bb-87a1-718f528613d5")
)
log_model_plot3 <- function() {
  library(ggplot2)
  library(dplyr)

  # Create labels for plot
  df <-
    tibble::tibble(
      boxLabels = c("Male vs. Female",
          "Asian vs. White",
          "Black or AA vs. White",
          "Other vs. White",
          "Race Missing vs. White",
          "Hispanic or Latino vs. Not Hispanic or Latino",
          "Ethnicity Missing vs. Not Hispanic or Latino",
          "Underweight vs. Normal Weight",
          "Overweight vs. Normal Weight",
          "Obese vs. Normal Weight",
          "BMI Missing vs. Normal Weight",
          "Age",
          "Charlson Comorbidity Index",
		  "Q2 2020 vs. Q1 2020",
		  "Q3 2020 vs. Q1 2020",
		  "Q4 2020 vs. Q1 2020",
		  "Q1 2021 vs. Q1 2020",
          "Large Rural vs. Urban",
          "Small Rural vs. Urban",
          "Isolated vs. Urban"),
      boxOdds = c(1.43, 0.97, 1.00, 1.51, 1.15, 0.88, 1.38, 1.32, 0.93, 1.09, 0.55, 1.05, 1.10, 0.84, 0.59, 0.55, 0.43, 2.05, 2.31, 1.97),
      boxCILow = c(1.38, 0.86, 0.95, 1.27, 1.07, 0.81, 1.27, 1.15, 0.87, 1.02, 0.52, 1.05, 1.09, 0.76, 0.53, 0.49, 0.38, 1.83, 2.12, 1.85),
      boxCIHigh = c(1.50, 1.10, 1.06, 1.79, 1.25, 0.95, 1.51, 1.51, 1.00, 1.17, 0.59, 1.05, 1.11, 0.94, 0.66, 0.60, 0.47, 2.29, 2.51, 2.11)
    ) %>%
    mutate(
      boxLabels = factor(
        boxLabels,
        c(
          "Male vs. Female",
          "Asian vs. White",
          "Black or AA vs. White",
          "Other vs. White",
          "Race Missing vs. White",
          "Hispanic or Latino vs. Not Hispanic or Latino",
          "Ethnicity Missing vs. Not Hispanic or Latino",
          "Underweight vs. Normal Weight",
          "Overweight vs. Normal Weight",
          "Obese vs. Normal Weight",
          "BMI Missing vs. Normal Weight",
          "Age",
          "Charlson Comorbidity Index",
		  "Q2 2020 vs. Q1 2020",
		  "Q3 2020 vs. Q1 2020",
		  "Q4 2020 vs. Q1 2020",
		  "Q1 2021 vs. Q1 2020",
          "Large Rural vs. Urban",
          "Small Rural vs. Urban",
          "Isolated vs. Urban"
        )
      ),
      boxLabels = forcats::fct_rev(boxLabels)
    )

  p <-
    df %>%
    ggplot(aes(y=boxLabels, x=boxOdds)) +
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
    geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
    geom_point(size = 3.5, color = "red") +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    coord_fixed(ratio=.3) +
    ylab('') +
    xlab('Odds Ratio') +
    #facet_wrap(~cat, ncol=1)+
    annotate(geom='text', y =1.1, x=3.5, label ='',
             size=3.5, hjust=0) + ggtitle('Estimated Odds of All-Cause Mortality in Entire Population') +
    theme(plot.title=element_text(hjust=.5, size=20))
  plot(p)
  #return(null)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.8ee25103-43bf-4ec3-a5d2-67018a472ef6"),
    survival_data_hosp=Input(rid="ri.foundry.main.dataset.ecb6b1db-103d-4ed0-b69d-fb45b5c13cd2")
)
log_rank_surv <- function(survival_data_hosp) {
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(viridis)
library(wesanderson)
library(survminer)

print(survival_data_hosp %>% summary())
local_df <- survival_data_hosp %>% dplyr::select(three_category_ruca, irural, gender, age, age_Group, Race, Ethnicity, BMI_Group, BMI, Q_Score_Categories, Q_Score, Diabetes, MI, CHF, PVD, stroke, dementia, pulmonary, rheumatic, Liver, paralysis, renal, cancer, mets, hiv, Severity_Type, ECMO, MACE, Mechnical_Ventilation, in_death_table, ttdeath, quarter_of_diagnosis, time, status)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, quarter_of_diagnosis != "Missing")
local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$irural=relevel(as.factor(local_df$irural),ref="urban")

fit <- survfit(Surv(time, status) ~ irural, data = local_df)

splots <- list()
splots[[1]] <- ggsurvplot(
           fit,                     # survfit object with calculated statistics.
           data = local_df,             # data used to fit survival curves.
           risk.table = TRUE,       # show risk table.
           pval = TRUE,             # show p-value of log-rank test.
           conf.int = TRUE,         # show confidence intervals for
                                    # point estimates of survival curves.
              palette = c("#404788FF", "#95D840FF"),#palette = scale_fill_viridis(option="A"),  # pass the active palette
   #ggtheme      = NULL, # disable adding custom survminer theme
   #font.x       = NULL, # disable adding custom survminer font for the x axis
   #font.y       = NULL, # disable adding custom survminer font for the y axis
   #font.main    = NULL, # disable adding custom survminer font for the title
   #font.submain = NULL, # disable adding custom survminer font for the subtitle
   #font.caption = NULL,  # disable adding custom survminer font for the caption
           xlim = c(0,30),         # present narrower X axis, but not affect
                                    # survival estimates.
           xlab = "Time in Days From Hospital Admission",   # customize X axis label.
           ylab = "Survival Probabaility",
           ylim = c(0,1),
           break.time.by = 5,     # break X axis in time intervals by 500.
           ggtheme = theme_grey(),
           #ggtheme = theme_light(), # customize plot and risk table with a theme.
          risk.table.y.text.col = T,# colour risk table text annotations.
          risk.table.height = 0.25, # the height of the risk table
          risk.table.type = c("absolute", "percentage", "abs_pct", "nrisk_cumcensor",
    "nrisk_cumevents"),
          risk.table.y.text = TRUE,# show bars instead of names in text annotations
                                    # in legend of risk table.
          ncensor.plot = FALSE,      # plot the number of censored subjects at time t
          #ncensor.plot.height = 0.25,
          #conf.int.style = "step",  # customize style of confidence intervals
          surv.median.line = "hv",  # add the median survival pointer.
          legend.labs =
            c("Urban", "Rural"),    # change legend labels.
            font.main = c(16, "bold", "navy"),
font.submain = c(18, "bold.italic", "navy"),
font.caption = c(14, "plain", "orange"),
font.x = c(12, "bold.italic", "black"),
font.y = c(14, "bold.italic", "navy"),
font.tickslab = c(14, "bold", "black")
        )
splots[[2]] <- ggsurvplot(
           fit,                     # survfit object with calculated statistics.
           data = local_df,             # data used to fit survival curves.
           risk.table = FALSE,       # show risk table.
                     ncensor.plot = TRUE,      # plot the number of censored subjects at time t
          ncensor.plot.height = 0.25,
           pval = TRUE,             # show p-value of log-rank test.
           pval.method = TRUE,
           conf.int = TRUE,         # show confidence intervals for
                                    # point estimates of survival curves.
              palette = c("#404788FF", "#95D840FF"),#palette = scale_fill_viridis(option="A"),  # pass the active palette
   #ggtheme      = NULL, # disable adding custom survminer theme
   #font.x       = NULL, # disable adding custom survminer font for the x axis
   #font.y       = NULL, # disable adding custom survminer font for the y axis
   #font.main    = NULL, # disable adding custom survminer font for the title
   #font.submain = NULL, # disable adding custom survminer font for the subtitle
   #font.caption = NULL,  # disable adding custom survminer font for the caption
           xlim = c(0,30),         # present narrower X axis, but not affect
                                    # survival estimates.
           xlab = "Time in Days From Hospital Admission",   # customize X axis label.
           ylab = "Survival Probabaility (Range: 0.75-1.00)",
           ylim = c(0.75,1),
           break.time.by = 5,     # break X axis in time intervals by 500.
           ggtheme = theme_grey(),
           #ggtheme = theme_light(), # customize plot and risk table with a theme.
          risk.table.y.text.col = T,# colour risk table text annotations.
          risk.table.height = 0.25, # the height of the risk table
          risk.table.type = c("absolute", "percentage", "abs_pct", "nrisk_cumcensor",
    "nrisk_cumevents"),
          risk.table.y.text = TRUE,# show bars instead of names in text annotations
                                    # in legend of risk table.
          #ncensor.plot = FALSE,      # plot the number of censored subjects at time t
          #ncensor.plot.height = 0.25,
          #conf.int.style = "step",  # customize style of confidence intervals
          surv.median.line = "hv",  # add the median survival pointer.
          legend.labs =
            c("Urban", "Rural"),    # change legend labels.
            font.main = c(16, "bold", "navy"),
font.submain = c(18, "bold.italic", "navy"),
font.caption = c(14, "plain", "orange"),
font.x = c(12, "bold.italic", "black"),
font.y = c(14, "bold.italic", "navy"),
font.tickslab = c(14, "bold", "black")
        )

ggsurv <- arrange_ggsurvplots(splots, print = TRUE,
  ncol = 2, nrow = 1, risk.table.height = 0.25, ncensor.plot.height = 0.25)

surv_diff <- survdiff(Surv(time, status) ~ irural, data = local_df)

#ggsurv <- ggsurvplot(fit, fun = "event", conf.int = TRUE,
#                     ggtheme = theme_bw())

#ggsurv$plot +theme_bw() +
#  theme (legend.position = "right")+
#  facet_grid(rx ~ adhere)

print(surv_diff)

print(ggsurv)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.0ab0329c-ea79-4dba-8dc5-bcd5395a4da3"),
    dta_residuals=Input(rid="ri.foundry.main.dataset.50cb3522-2016-4884-af8b-01d080a61674")
)

m1_rural_vars_copied <- function(dta_residuals) {

    ##tab_rural vs. other included vars
    local_df <- dta_residuals %>% dplyr::select(four_category_ruca, gender, Race, Ethnicity, BMI_Group,Q_Score_Categories,age,rs )
##tab rural vs. other included vars
    clab <-c("isolated", "large rural","small rural","urban")
    table1 <- local_df %>% tbl_summary(by = four_category_ruca) %>% add_p()
    table1 <- as_tibble(table1, col_labels = FALSE)
    print(table1)

    tb2 <- dta_residuals %>% group_by(four_category_ruca,gender) %>% summarise(rs_m=mean(rs))
    wide <- reshape(tb2,idvar=c("four_category_ruca"),direction = "wide")
    print(wide)

    return(table1)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.69bb7d6a-e2ff-4625-a961-1079725be7cc"),
    dta_residuals=Input(rid="ri.foundry.main.dataset.50cb3522-2016-4884-af8b-01d080a61674")
)
m2_rural_copied <- function(dta_residuals) {

    local_df <-dta_residuals
    print("================================================================Model m2================================================================")
    m2 <- glm(in_death_table ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_Score_Categories) +factor(four_category_ruca), data = local_df, family = binomial, maxit = 100)
    print(summary(m2))
    local_df$rs2 <- residuals(m2)

    return(local_df)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.35d85d44-7f19-45df-baec-75704c471409")
)
m2_rural_vars_copied <- function(m2_rural) {
    local_df <-m2_rural
    #%>% dplyr::select(four_category_ruca, gender, Race, Ethnicity, BMI_Group,Q_Score_Categories,age,rs )

    print("================================================================Model table2================================================================")
    table2 <- local_df %>% tbl_summary(by = four_category_ruca) %>% add_p()
    table2 <- as_tibble(table2, col_labels = FALSE)

}

@transform_pandas(
    Output(rid="ri.vector.main.execute.9e6854a0-fad5-44e5-a73e-c2fa330e8156"),
    dta_residuals=Input(rid="ri.foundry.main.dataset.50cb3522-2016-4884-af8b-01d080a61674")
)
m3_rural_int_copied <- function(dta_residuals) {
    #library(lmtest)

    local_df <-dta_residuals
    print("================================================================Model m2================================================================")
    m2<- glm(in_death_table ~factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_Score_Categories) +factor(four_category_ruca) , data = local_df, family = binomial, maxit = 100)
    m3_gender <- glm(in_death_table ~factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_Score_Categories) +factor(four_category_ruca)+ four_category_ruca*gender, data = local_df, family = binomial, maxit = 100)
    m3_Race <- glm(in_death_table ~factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_Score_Categories) +factor(four_category_ruca)+ four_category_ruca*Race, data = local_df, family = binomial, maxit = 100)
    m3_Q_Score_Categories<- glm(in_death_table ~factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_Score_Categories) +factor(four_category_ruca)+ four_category_ruca*Q_Score_Categories, data = local_df, family = binomial, maxit = 100)



    print("================================================================Model m3_Q_Score_Categoriesr================================================================")
    #print(m3_Q_Score_Categories)
    print(anova(m2,m3_Q_Score_Categories,test="LRT"))

    print("================================================================Model m3_Race================================================================")
    #print(m3_Race)
    print(anova(m2,m3_Race,test="LRT"))

     print("================================================================Model m3_Race================================================================")
    #print(m3_Race)
    print(anova(m2,m3_Race,test="LRT"))
    #lrtest(m2,m3)




    return(NULL)

}

@transform_pandas(
    Output(rid="ri.vector.main.execute.e98b0f75-974f-4155-b87d-e3e04bc6aa85")
)
m3_ruralint_vars_copied <- function(m3_rural_int) {
    local_df <-m3_rural_int
    #%>% dplyr::select(four_category_ruca, gender, Race, Ethnicity, BMI_Group,Q_Score_Categories,age,rs )

    print("================================================================Model table2================================================================")
    table3 <- local_df %>% tbl_summary(by = four_category_ruca) %>% add_p()
    table3 <- as_tibble(table3, col_labels = FALSE)

    return(table3)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.d2336563-5902-4185-9b03-88f8c370382a"),
    summary_all_for_analysis=Input(rid="ri.foundry.main.dataset.246fccb0-8f84-4ed9-a296-405e603a75e3")
)
margin_log_all <- function(summary_all_for_analysis) {
library(margins)
library(ggplot2)

print(summary_all_for_analysis %>% summary())
local_df <- summary_all_for_analysis %>% dplyr::select(three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, Q_Score, in_death_table, age, i_rural, quarter_of_diagnosis, Severity_Type, CCI_INDEX)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
m1 <- glm(in_death_table ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX + factor(three_category_ruca) + factor(quarter_of_diagnosis), data = local_df, family = binomial, maxit = 100)

#summary(m1)$coefficients
#print (summary(m1)$coefficients)
#tbl_regression(m1, exponentiate = TRUE)

# Save model to table
#table1 <- m1 %>%
#  tbl_regression(exponentiate = TRUE)
#table1 <- as_tibble(table1, col_labels = FALSE)

# Estimate margins
margins1 <- summary(margins(m1))
margins1 <- as_tibble(margins1, col_labels = TRUE)

# Plot marginal effect of gender on all-cause mortality
#gender_plot <- cplot(m1, "gender", what = "prediction", main = "Predicted Mortality, Given Gender")

#plot(gender_plot)

return(margins1)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.8de3defa-e180-469d-bf0d-9d010567d33a")
)
#not working

mean1_rural_vars_copied <- function(dta_residuals) {
library(plyr)
    ##tab_rural vs. other included vars
    local_df <- dta_residuals %>% dplyr::select(four_category_ruca, gender, Race, Ethnicity, BMI_Group,Q_Score_Categories,age,rs )

##tab rural vs. other included vars
    table1 <- local_df %>% tbl_summary(by = four_category_ruca) %>% add_p()

    #table1 <- as_tibble(table1, col_labels = FALSE)

    #table1.mean <- local_df %>% group_by(four_category_ruca,gender) %>% summarize_at(vars(rs),funs(mean.,na.nm=TRUE))

    ##ddply(local_df, vars(four_category_ruca,gender), summarize,  rsm=mean(rs))

table11 <- local_df%>% group_by(four_category_ruca,gender) %>%
    summarise(mean_rs_isolated =mean(rs[factor(gender)*factor(four_category_ruca)]))


    #=="isolated"])
    #    ,mean_rs_large_rural =mean(rs[factor(gender),four_category_ruca=="large rural"])
    #    ,mean_rs_small_rural =mean(rs[factor(gender),four_category_ruca=="small rural"])
    #    ,mean_rs_urban =mean(rs[factor(gender),four_category_ruca=="urban"])
    #)
    # %>%
print ("gender")
print(table11)

    table12 <- local_df%>% group_by(factor(Race)) %>%
        summarise(mean_rs_isolated =mean(rs[four_category_ruca=="urban"])
       # ,mean_rs_large_rural =mean(rs[four_category_ruca=="large rural"])
       # ,mean_rs_small_rural =mean(rs[four_category_ruca=="small rural"])
       # ,mean_rs_urban =mean(rs[four_category_ruca=="urban"])
    )
print("Race")
print(table12)
#residuals by ethnicty for each rural category
    table13 <- local_df%>% group_by(Ethnicity) %>%
    summarise(mean_rs_isolated =mean(rs[four_category_ruca=="isolated"])
        ,mean_rs_large_rural =mean(rs[four_category_ruca=="large rural"])
        ,mean_rs_small_rural =mean(rs[four_category_ruca=="small rural"])
        ,mean_rs_urban =mean(rs[four_category_ruca=="urban"])
    )
print("Ethnicty")
print(table13)

    table14 <- local_df%>% group_by(BMI_Group) %>%
    summarise(mean_rs_isolated =mean(rs[four_category_ruca=="isolated"])
        ,mean_rs_large_rural =mean(rs[four_category_ruca=="large rural"])
        ,mean_rs_small_rural =mean(rs[four_category_ruca=="small rural"])
        ,mean_rs_urban =mean(rs[four_category_ruca=="urban"])
    )
print("BMI_Group")
print(table14)

    table15 <- local_df%>% group_by(Q_Score_Categories) %>%
    summarise(mean_rs_isolated =mean(rs[four_category_ruca=="isolated"])
        ,mean_rs_large_rural =mean(rs[four_category_ruca=="large rural"])
        ,mean_rs_small_rural =mean(rs[four_category_ruca=="small rural"])
        ,mean_rs_urban =mean(rs[four_category_ruca=="urban"])
    )
print("Q_Score_Categories")
print(table15)

    table16 <- local_df%>% group_by(age) %>%
    summarise(mean_rs_isolated =mean(rs[four_category_ruca=="isolated"])
        ,mean_rs_large_rural =mean(rs[four_category_ruca=="large rural"])
        ,mean_rs_small_rural =mean(rs[four_category_ruca=="small rural"])
        ,mean_rs_urban =mean(rs[four_category_ruca=="urban"])
    )
print("age")
print(table16)

    #spread(four_category_ruca, mean_rs)
        #%>% ungroup() %>%
       #transmute(country = country,  female_percent = F / (F+M), male_percent = M /(F+M))

return(NULL)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.1913971c-0701-48c5-8a6e-0f64c3e07802"),
    dta_residuals=Input(rid="ri.foundry.main.dataset.50cb3522-2016-4884-af8b-01d080a61674")
)
resid_in_m_copied <- function(dta_residuals) {

local_df <- dta_residuals
##local_df2 <-local_df%>% dplyr::select(rs)
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$age_Group=relevel(as.factor(local_df$age_Group),ref="30-39")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$four_category_ruca=relevel(as.factor(local_df$four_category_ruca),ref="urban")


##("ttdeath","four_category_ruca","age","i_rural","gender","age_Group","Race","Ethnicity","BMI_Group","BMI","Q_Score_Categories","Q_Score","Diabetes","MI","CHF","PVD","stroke","dementia","pulmonary","rheumatic","Liver","paralysis","renal","cancer","mets","hiv","Severity_Type","ECMO","MACE","Mechnical_Ventilation","in_death_table","rs")


#local_df$gender =  as.factor(local_df$gender)
#local_df$Race =  as.factor(local_df$Race)
#local_df$Ethnicity =  as.factor(local_df$Ethnicity)
#local_df$BMI_Group =  as.factor(local_df$BMI_Group)
#local_df$age_Group =  as.factor(local_df$age_Group)
#local_df$age_Group =  as.factor(local_df$Q_Score_Categories)

fn_resid_i <- function(dta, variable,as_factor=0) {
   # if (as_factor==1){
   #     dta$variable <-  as.factor(dta[, variable])
   # }
   # else{
   #     dta$variable <-dta[, variable]
   # }
    dta$variable <-dta[, variable]
    #dta$ACE_ARB <- as.factor(dta$ACE_ARB)
    m2 <- glm(rs ~   i_rural*variable, data = dta,  maxit = 100)
    print(summary(m2))
}
#table1 <- as_tibble(fn_resid_i, col_labels = FALSE)

print("===============================================================================================")
varlist <-list("gender","Race" ,"Ethnicity", "BMI_Group", "age_Group","Q_Score_Categories", "i_rural")
for (vvv in varlist){
    fn_resid_i(local_df,vvv,1)
}

print("===============================================================================================")
varlist <-list("Severity_Type","ECMO","MACE","Mechnical_Ventilation")
for (vvv in varlist){
    fn_resid_i(local_df,vvv)
}

#print(exp(cbind(OR = coef(m2), confint(m2))))


#table2 <- local_df %>% tbl_summary(by = Q_Score_Categories) %>% add_p()
#table2 <- as_tibble(table2, col_labels = FALSE)
#print(table2)




##table1 <- m1 %>%
##  tbl_regression(exponentiate = TRUE) %>%
##  as_gt() %>%
##  gt::tab_source_note(gt::md("*This data is simulated*"))
##print(table1)
##################################################################################
##@Jerrod: How can I store the summary table to use later? Please help!
##################################################################################
##local_df2$coefficients<-summary(m1)$coefficients



return (NULL)
#return (local_df2)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.2d8e9a32-cd91-4081-a9a1-b388c25729a5"),
    summary_all_for_analysis=Input(rid="ri.foundry.main.dataset.246fccb0-8f84-4ed9-a296-405e603a75e3")
)
summary_all <- function(summary_all_for_analysis) {
library(gtsummary)
library(rvest)
library(dplyr)
library(tidyverse)
print(summary_all_for_analysis %>% summary())
local_df <- summary_all_for_analysis %>% dplyr::select(three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, BMI, Q_Score_Categories, Q_Score, Diabetes, MI, CHF, PVD, stroke, dementia, pulmonary, rheumatic, Liver, paralysis, renal, cancer, mets, hiv, Severity_Type, ECMO, MACE, Mechnical_Ventilation, in_death_table, ttdeath, quarter_of_diagnosis, CCI_INDEX, CCI_Categories)
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")

# Create table and save to dataset
table1 <- local_df %>% tbl_summary(by = three_category_ruca) %>% add_p()
table1 <- as_tibble(table1, col_labels = FALSE)
print(table1)
return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.e62b14e0-dcae-48d0-bf89-1378ebfd7bac"),
    summary_for_analysis_on_hospitalization=Input(rid="ri.foundry.main.dataset.58239702-c61d-4026-9e36-2e75ccd2da01")
)
summary_all_filtered_to_final_analysis <- function(summary_for_analysis_on_hospitalization) {
    library(gtsummary)
library(rvest)
library(dplyr)
library(tidyverse)
print(summary_for_analysis_on_hospitalization %>% summary())
local_df <- summary_for_analysis_on_hospitalization %>% dplyr::select(three_category_ruca, gender, age_Category, Race, Ethnicity, BMI_Group, BMI, hypertension, Diabetes, MI, CHF, PVD, stroke, dementia, pulmonary, rheumatic, liver, paralysis, renal, cancer, mets, hiv, multiple, smoking_status, Severity_Type, oxygen_saturation_lab_value, hospitalized, Mechnical_Ventilation, Invasive_Ventilation, ECMO, MACE, Mechnical_Ventilation, in_death_table, ttdeath, quarter_of_diagnosis, CCI_INDEX, CCI_Categories)

# Filter out missing variables
local_df <- filter(local_df, age_Category != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

table1 <- local_df %>% tbl_summary(by = three_category_ruca) %>% add_p()
table1 <- as_tibble(table1, col_labels = FALSE)
print(table1)
return(table1)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.4bbfe080-47d6-434b-b6e7-4620508229c7"),
    summary_for_analysis_on_hospitalization=Input(rid="ri.foundry.main.dataset.58239702-c61d-4026-9e36-2e75ccd2da01")
)
survival_admission_on_rurality <- function(summary_for_analysis_on_hospitalization) {
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(wesanderson)
library(survminer)

print(summary_for_analysis_on_hospitalization %>% summary())

local_df <- summary_for_analysis_on_hospitalization %>% dplyr::select(three_category_ruca, gender, age, age_Group, Race, Ethnicity, BMI_Group, BMI, Q_Score_Categories, Q_Score, Diabetes, MI, CHF, PVD, stroke, dementia, pulmonary, rheumatic, Liver, paralysis, renal, cancer, mets, hiv, Severity_Type, ECMO, MACE, Mechnical_Ventilation, in_death_table, ttdeath, quarter_of_diagnosis, time, status)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")

#local_df <- filter(local_df, age_Group != "Unknown")
#local_df <- filter(local_df, gender != "Unknown")
#local_df <- filter(local_df, gender != "Other")
#local_df <- filter(local_df, quarter_of_diagnosis != "Missing")
#local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
#local_df$rural_categories=relevel(as.factor(local_df$rural_categories),ref="Urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"))

#km <- with(local_df, Surv(time, status))
#head(km,80)

#km_fit <- survfit(Surv(time, status) ~ 1, data=local_df)
#summary(km_fit, times = c(1,5,10,15,20,25,30))

#km_plot <- autoplot(km_fit, pVal=TRUE)
#plot(km_plot)

#km_trt_fit <- survfit(Surv(time, status) ~ three_category_ruca, data=local_df)
km_trt_fit <- survfit(Surv(time, status) ~ three_category_ruca, data=local_df)

km_trt_fit$panel$layout$names <- c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural")
str(km_trt_fit)

km_trt_fit_plot <- autoplot(km_trt_fit)
km_trt_fit_plot <- autoplot(km_trt_fit, type = "est_ridge", CI=TRUE, pval=TRUE, plotTable=TRUE ) + #,  ylim = c(0.75,1), divideTime=30,
 #legendLabs=c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjance Rural")) +
 #theme(legend.position = c(0.125,0.125)) +
  theme(
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 14)
  ) +
  viridis::scale_fill_viridis(discrete = TRUE) +
  viridis::scale_colour_viridis(discrete = TRUE) +
 labs(x = "\n Survival Time (Days) ", y = "Survival Probabilities \n")
 #title = "Survival Curves by Rural Category in Hospitalized Patients over 30 Days from Day of Admission")

plot(km_trt_fit_plot)
print(km_trt_fit)
return(km_trt_fit_plot)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.c23f6a6e-faef-4c78-8dd3-62bd868b2e41"),
    survival_data_hosp=Input(rid="ri.foundry.main.dataset.ecb6b1db-103d-4ed0-b69d-fb45b5c13cd2")
)
survival_final <- function(survival_data_hosp) {
  library(survival)
  library(ranger)
  library(ggplot2)
  library(dplyr)
  library(ggfortify)
  library(viridis)
  library(survminer)

  print(survival_data_hosp %>% summary())
  local_df <- survival_data_hosp %>% dplyr::select(three_category_ruca, irural, gender, age, age_Group, Race, Ethnicity, BMI_Group, BMI, Q_Score_Categories, Q_Score, Diabetes, MI, CHF, PVD, stroke, dementia, pulmonary, rheumatic, Liver, paralysis, renal, cancer, mets, hiv, Severity_Type, ECMO, MACE, Mechnical_Ventilation, in_death_table, ttdeath, quarter_of_diagnosis, time, status)

  # Filter out missing variables
  local_df <- filter(local_df, age_Group != "Unknown")
  local_df <- filter(local_df, gender != "Unknown")
  local_df <- filter(local_df, gender != "Other")
  local_df <- filter(local_df, quarter_of_diagnosis != "Missing")
  local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

  # Set reference categories
  local_df$Race=relevel(as.factor(local_df$Race),ref="White")
  local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
  local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
  local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
  local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
  local_df$irural=relevel(as.factor(local_df$irural),ref="urban")

  fit <- survfit(Surv(time, status) ~ irural, data = local_df)

  ggsurv <- ggsurvplot(
    fit,                     # survfit object with calculated statistics.
    data = local_df,             # data used to fit survival curves.
    risk.table = TRUE,       # show risk table.
    pval = TRUE,             # show p-value of log-rank test.
    conf.int = TRUE,         # show confidence intervals for
    # point estimates of survival curves.
    palette = c("#E7B800", "#2E9FDF"),
    xlim = c(0,30),         # present narrower X axis, but not affect
    # survival estimates.
    xlab = "Time in days",   # customize X axis label.
    break.time.by = 5,     # break X axis in time intervals by 500.
    ggtheme = theme_light(), # customize plot and risk table with a theme.
    risk.table.y.text.col = T,# colour risk table text annotations.
    risk.table.height = 0.25, # the height of the risk table
    risk.table.y.text = FALSE,# show bars instead of names in text annotations
    # in legend of risk table.
    ncensor.plot = TRUE,      # plot the number of censored subjects at time t
    ncensor.plot.height = 0.25,
    conf.int.style = "step",  # customize style of confidence intervals
    surv.median.line = "hv",  # add the median survival pointer.
    legend.labs =
      c("Urban", "Rural")    # change legend labels.
  )

  print(ggsurv)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.fbc13f63-a980-4e36-ae77-4b83462d99bc"),
    survival_data_hosp=Input(rid="ri.foundry.main.dataset.ecb6b1db-103d-4ed0-b69d-fb45b5c13cd2")
)
survival_final_copied <- function(survival_data_hosp) {
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(viridis)
library(wesanderson)
library(survminer)

print(survival_data_hosp %>% summary())
local_df <- survival_data_hosp %>% dplyr::select(three_category_ruca, irural, gender, age, age_Group, Race, Ethnicity, BMI_Group, BMI, Q_Score_Categories, Q_Score, Diabetes, MI, CHF, PVD, stroke, dementia, pulmonary, rheumatic, Liver, paralysis, renal, cancer, mets, hiv, Severity_Type, ECMO, MACE, Mechnical_Ventilation, in_death_table, ttdeath, quarter_of_diagnosis, time, status)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Missing")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, BMI_Group != "Unknown/Missing")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$irural=relevel(as.factor(local_df$irural),ref="urban")

fit <- survfit(Surv(time, status) ~ three_category_ruca, data = local_df)

ggsurv <- ggsurvplot(
           fit,                     # survfit object with calculated statistics.
           data = local_df,             # data used to fit survival curves.
           risk.table = TRUE,       # show risk table.
           pval = TRUE,             # show p-value of log-rank test.
           pval.method.size = TRUE,
           pval.method.coord = TRUE,
           conf.int = TRUE,         # show confidence intervals for
                                    # point estimates of survival curves.
             palette = c("#404788FF", "#95D840FF", "#FDE725FF"),  # pass the active palette
   ggtheme      = NULL, # disable adding custom survminer theme
   font.x       = NULL, # disable adding custom survminer font for the x axis
   font.y       = NULL, # disable adding custom survminer font for the y axis
   font.main    = NULL, # disable adding custom survminer font for the title
   font.submain = NULL, # disable adding custom survminer font for the subtitle
   font.caption = NULL,  # disable adding custom survminer font for the caption
           xlim = c(0,30),         # present narrower X axis, but not affect
                                    # survival estimates.
           xlab = "Time in days",   # customize X axis label.
           break.time.by = 5,     # break X axis in time intervals by 500.
           #ggtheme = theme_light(), # customize plot and risk table with a theme.
          risk.table.y.text.col = T,# colour risk table text annotations.
          risk.table.height = 0.25, # the height of the risk table
          risk.table.y.text = FALSE,# show bars instead of names in text annotations
                                    # in legend of risk table.
          ncensor.plot = FALSE,      # plot the number of censored subjects at time t
          #ncensor.plot.height = 0.25,
          conf.int.style = "step",  # customize style of confidence intervals
          surv.median.line = "hv",  # add the median survival pointer.
          legend.labs =
            c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural")    # change legend labels.
        )

print(ggsurv)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.6d9c9ac9-05c2-488b-85b0-78c8954b8d45"),
    survival_data_hosp=Input(rid="ri.foundry.main.dataset.ecb6b1db-103d-4ed0-b69d-fb45b5c13cd2")
)
survival_final_copied_zoomed <- function(survival_data_hosp) {
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(viridis)
library(wesanderson)
library(survminer)

print(survival_data_hosp %>% summary())
local_df <- survival_data_hosp %>% dplyr::select(three_category_ruca, irural, gender, age, age_Group, Race, Ethnicity, BMI_Group, BMI, Q_Score_Categories, Q_Score, Diabetes, MI, CHF, PVD, stroke, dementia, pulmonary, rheumatic, Liver, paralysis, renal, cancer, mets, hiv, Severity_Type, ECMO, MACE, Mechnical_Ventilation, in_death_table, ttdeath, quarter_of_diagnosis, time, status, CCI_Categories)

# Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, quarter_of_diagnosis != "Missing")
local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$irural=relevel(as.factor(local_df$irural),ref="urban")

#fit <- survfit(Surv(time, status) ~ irural, data = local_df)
fit <- survfit(Surv(time, status) ~ three_category_ruca, data = local_df)
fit2 <- survfit(Surv(time, status) ~ CCI_Categories, data = local_df)
fit3 <- survfit(Surv(time, status) ~ age_Group, data = local_df)

#m1 <- glm(in_death_table ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX + factor(four_category_ruca) + factor(quarter_of_diagnosis), data = local_df, family = binomial, maxit = 100)

splots <- list()
splots[[1]] <- ggsurvplot(
           fit,                     # survfit object with calculated statistics.
           data = local_df,             # data used to fit survival curves.
           risk.table = FALSE,       # show risk table.
           pval = TRUE,             # show p-value of log-rank test.
           conf.int = TRUE,         # show confidence intervals for
                                    # point estimates of survival curves.
              palette = c("#DCE319FF", "#440154FF", "#39568CFF"), #palette = scale_fill_viridis(option="A"),  # pass the active palette
   #ggtheme      = NULL, # disable adding custom survminer theme
   #font.x       = NULL, # disable adding custom survminer font for the x axis
   #font.y       = NULL, # disable adding custom survminer font for the y axis
   #font.main    = NULL, # disable adding custom survminer font for the title
   #font.submain = NULL, # disable adding custom survminer font for the subtitle
   #font.caption = NULL,  # disable adding custom survminer font for the caption
           xlim = c(0,30),         # present narrower X axis, but not affect
                                    # survival estimates.
           xlab = "Time in Days From Hospital Admission",   # customize X axis label.
           ylab = "Survival Probabaility",
           ylim = c(0,1),
           break.time.by = 5,     # break X axis in time intervals by 500.
           ggtheme = theme_grey(),
           #ggtheme = theme_light(), # customize plot and risk table with a theme.
          #risk.table.y.text.col = T,# colour risk table text annotations.
          #risk.table.height = 0.25, # the height of the risk table
          #risk.table.type = c("absolute", "percentage", "abs_pct", "nrisk_cumcensor", "nrisk_cumevents"),
          #risk.table.y.text = TRUE,# show bars instead of names in text annotations
                                    # in legend of risk table.
          ncensor.plot = FALSE,      # plot the number of censored subjects at time t
          #ncensor.plot.height = 0.25,
          #conf.int.style = "step",  # customize style of confidence intervals
          surv.median.line = "hv",  # add the median survival pointer.
          legend.labs =
            c("Urban", "Urban-Adjacent Rural", "Nonurban-Adjacent Rural"),    # change legend labels.
            font.main = c(16, "bold", "navy"),
font.submain = c(18, "bold.italic", "navy"),
font.caption = c(14, "plain", "orange"),
font.x = c(12, "bold.italic", "black"),
font.y = c(14, "bold.italic", "navy"),
font.tickslab = c(14, "bold", "black")
        )
splots[[2]] <- ggsurvplot(
           fit2,                     # survfit object with calculated statistics.
           data = local_df,             # data used to fit survival curves.
           risk.table = FALSE,       # show risk table.
                     ncensor.plot = TRUE,      # plot the number of censored subjects at time t
          ncensor.plot.height = 0.25,
           pval = TRUE,             # show p-value of log-rank test.
           conf.int = TRUE,         # show confidence intervals for
                                    # point estimates of survival curves.
              palette = c("#404788FF", "#95D840FF", "#DCE319FF", "FDE725FF", "20A387FF", "453781FF", "29AF7FFF"),
              #palette = c("#404788FF", "#95D840FF", "#DCE319FF"),#palette = scale_fill_viridis(option="A"),  # pass the active palette
   #ggtheme      = NULL, # disable adding custom survminer theme
   #font.x       = NULL, # disable adding custom survminer font for the x axis
   #font.y       = NULL, # disable adding custom survminer font for the y axis
   #font.main    = NULL, # disable adding custom survminer font for the title
   #font.submain = NULL, # disable adding custom survminer font for the subtitle
   #font.caption = NULL,  # disable adding custom survminer font for the caption
           xlim = c(0,30),         # present narrower X axis, but not affect
                                    # survival estimates.
           xlab = "Time in Days From Hospital Admission",   # customize X axis label.
           ylab = "Survival Probabaility (Range: 0.75-1.00)",
           ylim = c(0.75,1),
           break.time.by = 5,     # break X axis in time intervals by 500.
           ggtheme = theme_grey(),
           #ggtheme = theme_light(), # customize plot and risk table with a theme.
          risk.table.y.text.col = T,# colour risk table text annotations.
          risk.table.height = 0.25, # the height of the risk table
          risk.table.type = c("absolute", "percentage", "abs_pct", "nrisk_cumcensor",
    "nrisk_cumevents"),
          risk.table.y.text = TRUE,# show bars instead of names in text annotations
                                    # in legend of risk table.
          #ncensor.plot = FALSE,      # plot the number of censored subjects at time t
          #ncensor.plot.height = 0.25,
          #conf.int.style = "step",  # customize style of confidence intervals
          surv.median.line = "hv",  # add the median survival pointer.
          legend.labs =
            c("<18", "18-29", "30-39", "40-49", "50-59", "60-69", "70-79", ">=80"),    # change legend labels.
            font.main = c(16, "bold", "navy"),
font.submain = c(18, "bold.italic", "navy"),
font.caption = c(14, "plain", "orange"),
font.x = c(12, "bold.italic", "black"),
font.y = c(14, "bold.italic", "navy"),
font.tickslab = c(14, "bold", "black")
        )

ggsurv <- arrange_ggsurvplots(splots, print = TRUE,
  ncol = 2, nrow = 1, risk.table.height = 0.25, ncensor.plot.height = 0.25)

print(ggsurv)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.3ee9ee4e-f29b-4370-b57c-ddb943eeef02"),
    dta_residuals=Input(rid="ri.foundry.main.dataset.50cb3522-2016-4884-af8b-01d080a61674")
)
tb_resid_copied <- function(dta_residuals) {

local_df <- dta_residuals
##local_df2 <-local_df%>% dplyr::select(rs)
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$age_Group=relevel(as.factor(local_df$age_Group),ref="30-39")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")

##("ttdeath","four_category_ruca","age","i_rural","gender","age_Group","Race","Ethnicity","BMI_Group","BMI","Q_Score_Categories","Q_Score","Diabetes","MI","CHF","PVD","stroke","dementia","pulmonary","rheumatic","Liver","paralysis","renal","cancer","mets","hiv","Severity_Type","ECMO","MACE","Mechnical_Ventilation","in_death_table","rs")

m1 <- glm(rs ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + factor(Q_Score_Categories) + i_rural , data = local_df,  maxit = 100)
print(summary(m1))

local_df$gender =  as.factor(local_df$gender)
local_df$Race =  as.factor(local_df$Race)
m2 <- glm(rs ~   i_rural*Q_Score_Categories, data = local_df,  maxit = 100)

#print(exp(cbind(OR = coef(m2), confint(m2))))
print(summary(m2))

table2 <- local_df %>% tbl_summary(by = Q_Score_Categories) %>% add_p()
table2 <- as_tibble(table2, col_labels = FALSE)
#print(table2)

##table1 <- m1 %>%
##  tbl_regression(exponentiate = TRUE) %>%
##  as_gt() %>%
##  gt::tab_source_note(gt::md("*This data is simulated*"))
##print(table1)
##################################################################################
##@Jerrod: How can I store the summary table to use later? Please help!
##################################################################################
##local_df2$coefficients<-summary(m1)$coefficients

return (NULL)
#return (local_df2)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.c1b41359-bbc0-4ec3-900a-a7cf260427f3")
)
unnamed <- function(summar) {

}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.bc670a47-324b-4814-be0d-f63015ecb980"),
    hospitalized_survived=Input(rid="ri.foundry.main.dataset.0272d167-f133-43df-8d5b-87c2c1ce1b2d")
)
unnamed_9 <- function(hospitalized_survived) {
library(gtsummary)
library(rvest)
library(dplyr)
library(tidyverse)
print(hospitalized_survived %>% summary())
local_df <- hospitalized_survived %>% dplyr::select(three_category_ruca, gender, age_Group, Race, Ethnicity, BMI_Group, BMI, Q_Score_Categories, Q_Score, Diabetes, MI, CHF, PVD, stroke, dementia, pulmonary, rheumatic, Liver, paralysis, renal, cancer, mets, hiv, Severity_Type, ECMO, MACE, Mechnical_Ventilation, covid_peak, length_of_stay)
table1 <- local_df %>% tbl_summary(by = three_category_ruca) %>% add_p()
table1 <- as_tibble(table1, col_labels = FALSE)
print(table1)
return(table1)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.5d3050e9-7698-4b58-8b79-759f225f7944"),
    dta_residuals=Input(rid="ri.foundry.main.dataset.50cb3522-2016-4884-af8b-01d080a61674")
)
unnamed_9_copied <- function(dta_residuals) {
    df<-dta_residuals
    aggregate(df$rs, list(df$gender), FUN=mean)
}
