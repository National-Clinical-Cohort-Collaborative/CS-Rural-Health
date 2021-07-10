library(gtsummary)
library(rvest)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(MatchIt)


str_md2 <- "glm(in_death_table ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) + i_rural + any_disease + ip_rate , data = local_df, family = binomial())"
allcols <- c("ttdeath","in_death_table","ruca_cat","i_rural","gender","age_Group","age","Race","Ethnicity","BMI_Group","BMI","Q_cat","Q_Score","Diabetes","MI","CHF","PVD","stroke","dementia","pulmonary","rheumatic","Liver","paralysis","renal","cancer","mets","hiv","any_disease","Severity_Type","ECMO","MACE","Mechnical_Ventilation","covid_peak","los","quarter_dx","data_partner_id","ip_rate","pred2","rs2","rs2_self","rs2_x")

stinlist_cat <- list("gender","Race", "Ethnicity","BMI_Group","Q_cat", "quarter_dx","any_disease")

stinlist_cont <- list("age","ip_rate")

stNOTinlist_cat <- list("ruca_cat","data_partner_id","age_Group","Q_Score","Diabetes","MI","CHF","PVD","stroke","dementia","pulmonary","rheumatic","Liver","paralysis","renal","cancer","mets","hiv")

stNOTinlist_cont <- list("ttdeath","los")
#covid_peak: all missing

stOutcome_cat <- list("in_death_table","Severity_Type","ECMO","MACE","Mechnical_Ventilation")

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.2b38f775-6bda-4b43-9093-9b20a2d1c865"),
    s1_pre_analysis=Input(rid="ri.foundry.main.dataset.64f9d685-6967-4af5-9656-9d3044b0493e")
)
s1_data_residuals_1 <- function(s1_pre_analysis) {
   local_df <- s1_pre_analysis %>% dplyr::select(ttdeath,in_death_table,ruca_cat,i_rural,gender,age_Group,age,Race,Ethnicity,BMI_Group,BMI,Q_cat,Q_Score,Diabetes,MI,CHF,PVD,stroke,dementia,pulmonary,rheumatic,Liver,paralysis,renal,cancer,mets,hiv,any_disease,Severity_Type,ECMO,MACE,Mechnical_Ventilation,covid_peak,los,quarter_dx,data_partner_id,ip_rate)

    md2 <- glm(in_death_table ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) + i_rural + any_disease + ip_rate, data = local_df, family = binomial())


    local_df$pred2 <- predict(md2,type=c('response'))
    local_df$rs2 <- residuals(md2)

    local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    print('---------------------------------------------------------------------------------')
    print('Check sum residuals')

    mean_all <- mean(local_df$rs2)
    print(paste("Checksum rs2_default", mean_all))
    local_df$rs2_x <- residuals(md2,local_df,type=c("deviance"))
    mean_all <- mean(local_df$rs2_x)
    print(paste("Checksum rs2_deviance", mean_all))
    local_df$rs2_x <- residuals(md2,local_df,type=c("response"))
    mean_all <- mean(local_df$rs2_x)
    print(paste("Checksum rs2_response", mean_all))
    local_df$rs2_x <- residuals(md2,local_df,type=c("pearson"))
    mean_all <- mean(local_df$rs2_x)
    print(paste("Checksum rs2_pearson", mean_all))

    mean_all <- mean(local_df$rs2_self)
    print(paste("San's Checksum rs2_self", mean_all))
    hist(local_df$rs2_self)

return(local_df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.baa3369c-5e0f-4a99-953d-73a04a39970d"),
    s_resid_i_r_rural=Input(rid="ri.foundry.main.dataset.3ea07984-9c9e-4365-92e5-efa838846c4b")
)
s1_data_residuals_1_1_1 <- function(s_resid_i_r_rural) {
   local_df <- s_resid_i_r_rural
   # %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    md2 <- glm(i_rural ~factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease +  factor(data_partner_id), family=binomial, data = local_df)

    print(summary(md2))

    local_df$pred2 <- predict(md2,type=c('response'))
    #local_df$pred2 <- predict(md2 , type="response")



    mod_match <- matchit(i_rural ~ pred2,method = "nearest", data = local_df)
    dta_m <- match.data(mod_match)
    print(summary(dta_m))
    return(dta_m)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.364605a9-2f0f-424b-927e-4ab3e71a1686"),
    s_resid_i_r_rural=Input(rid="ri.foundry.main.dataset.3ea07984-9c9e-4365-92e5-efa838846c4b")
)
s1_data_residuals_1_1_1_1 <- function(s_resid_i_r_rural) {
   local_df <- unnamed_2
   # %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    md2 <- glm(i_rural ~factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease +  factor(data_partner_id), family=binomial, data = local_df)

    print(summary(md2))

    local_df$pred2 <- predict(md2,type=c('response'))
    #local_df$pred2 <- predict(md2 , type="response")



    return(local_df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.437408bb-054b-46ca-854c-8a84d0333730"),
    s_resid_i_r_rural=Input(rid="ri.foundry.main.dataset.3ea07984-9c9e-4365-92e5-efa838846c4b")
)
s1_data_residuals_1_1_1_2 <- function(s_resid_i_r_rural) {
   local_df <- s_resid_i_r_rural
   # %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    md2 <- glm(i_rural ~factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)

    print(summary(md2))

    local_df$pred2 <- predict(md2,type=c('response'))
    #local_df$pred2 <- predict(md2 , type="response")

    mod_match <- matchit(i_rural ~ pred2,method = "nearest", data = local_df)
    dta_m <- match.data(mod_match)
    print(summary(dta_m))
    return(dta_m)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.0e516f2d-b7d2-44f3-9668-524b341ac6f7"),
    s_resid_i_r_rural=Input(rid="ri.foundry.main.dataset.3ea07984-9c9e-4365-92e5-efa838846c4b")
)
s1_data_residuals_1_1_1_2_1 <- function(s_resid_i_r_rural) {
   local_df <- s_resid_i_r_rural
   # %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)
    mod_match <- matchit(i_rural ~factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease, method = "nearest", data = local_df)
    dta_m <- match.data(mod_match)
    print(summary(dta_m))
    return(dta_m)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.81e52adc-9c6c-42a3-a71c-c55bb9d77bb1"),
    s_3cats=Input(rid="ri.foundry.main.dataset.8c767db8-591e-4e1c-a921-ced14db04ba3")
)
s1_data_residuals_1_1_1_2_2_1 <- function(s_3cats) {
   local_df <- s1_data_residuals_1_1_1_2_2

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

    tb2 <-NA
    stdlist <- list("four_category_ruca","three_category_ruca","age_Group","Q_cat","Diabetes","MI","CHF","PVD","stroke","dementia","pulmonary","rheumatic","Liver","paralysis","renal","cancer","mets","hiv","any_disease","Severity_Type","ECMO","MACE","Mechnical_Ventilation","data_partner_id")

      #"covid_peak",

    for (x in  stdlist){

        print(paste("x=",x))

        local_df$x <- local_df[,x]
        a <- glm( rs2_self ~ i_rural+ factor(x) , data = local_df)

        print(summary(a))
        #tb1 <- a %>% tbl_regression(exponentiate = TRUE)
        #print(tb1)
        #tb1 <- local_df %>% tbl_summary() %>% add_p()
        #tb1 <- as_tibble(tb1, col_labels = FALSE)
        #tb2 <- rbind(tb2,tb1)
    }

    stdlist <- list("ttdeath","BMI","npats","ip_rate","los")
    for (x in  stdlist){
        print(paste("x=",x))
        local_df$x <- local_df[,x]
        a <- glm( rs2_self ~ i_rural+ x , data = local_df)

        print(summary(a))
        #tb1 <- a %>% tbl_regression(exponentiate = TRUE)
        #print(tb1)
        #tb1 <- local_df %>% tbl_summary() %>% add_p()
        #tb1 <- as_tibble(tb1, col_labels = FALSE)
        #tb2 <- rbind(tb2,tb1)
    }

    return(NULL)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.aef11030-0c07-4b57-8aa7-3a9d87db7fd6"),
    unnamed_8=Input(rid="ri.foundry.main.dataset.6269482e-6137-4c64-97bc-9264db84b63a")
)
s1_data_residuals_1_1_1_2_2_2 <- function(unnamed_8) {
   local_df <- unnamed_8

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

   # %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San

    #md2 <- glm( in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease +factor(data_partner_id), family=binomial, data = local_df)
    local_df$i_rural_orig <- local_df$i_rural

    md2 <- glm( i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)


    mod_match <- matchit(i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX + factor(quarter_of_diagnosis) , method = "nearest", exact ~ factor(data_partner_id) , data = local_df)
    dta_m <- match.data(mod_match)

    md2 <- glm(in_death_table ~ i_rural, data = dta_m, family = binomial, maxit = 100)

    print(summary(md2))

    dta_m$pred2 <- predict(md2,type=c('response'))
    dta_m$rs2_self <- local_df$in_death_table - local_df$pred2

    return(dta_m)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.b2088b2d-b900-4bf9-bb36-b70f76c0fa3f"),
    unnamed_8=Input(rid="ri.foundry.main.dataset.6269482e-6137-4c64-97bc-9264db84b63a")
)
s1_data_residuals_1_1_1_2_2_2_1 <- function(unnamed_8) {
   local_df <- unnamed_8

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
#local_df$three_category_ruca=relevel(as.factor(local_df$quarter_of_diagnosis),ref="urban")
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))
local_df$i_rural_orig <- local_df$i_rural
local_df$i_rural <- strtoi(local_df$i_rural_orig,base =0L)

   # %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San

    md2 <- glm( i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , family=binomial, data = local_df)

    local_df$pred2 <- predict(md2,type=c('response'))

    mod_match <- matchit(i_rural ~ pred2 , method = "nearest", exact = ~ factor(data_partner_id) , data = local_df)
    dta_m <- match.data(mod_match)

    md2 <- glm(in_death_table ~ i_rural, data = dta_m, family = binomial, maxit = 100)

    print(summary(md2))

    dta_m$pred2 <- predict(md2,type=c('response'))
    dta_m$rs2_self <- local_df$in_death_table - local_df$pred2

    return(dta_m)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.fd727812-4abb-4b68-a4a3-86cd43736054"),
    unnamed_8=Input(rid="ri.foundry.main.dataset.6269482e-6137-4c64-97bc-9264db84b63a")
)
s1_data_residuals_1_1_1_2_2_2_1_1 <- function(unnamed_8) {
   local_df <- unnamed_8

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
#local_df$three_category_ruca=relevel(as.factor(local_df$quarter_of_diagnosis),ref="urban")
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))
local_df$i_rural_orig <- local_df$i_rural
local_df$i_rural <- strtoi(local_df$i_rural_orig,base =0L)

print(dim(local_df))
   # %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San

    md2 <- glm( i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , family=binomial, data = local_df)

    local_df$pred2 <- predict(md2,type=c('response'))
    local_df$pred2cat <- ifelse(local_df$pred2 >=.2, 1, 0)

    #local_df %>% mutate(pred2cat = case_when(local_df$pred2  >= .2 ~ 1,local_df$pred2 <.2 ~ 0))
    print("matching")
    mod_match <- matchit(i_rural ~ pred2cat +factor(data_partner_id), method = "exact", data = local_df)
    dta_m <- match.data(mod_match,drop.unmatched=FALSE)

    print("PSC")
    md2 <- glm(in_death_table ~ i_rural, data = dta_m,family = binomial,weights= weights, maxit = 100)
    print(summary(md2))

    dta_m$pred2 <- predict(md2,type=c('response'))
    dta_m$rs2_self <- local_df$in_death_table - local_df$pred2

    return(dta_m)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.8cf2b8e6-787f-47aa-bf6d-a7fe6ffd9379"),
    unnamed_8=Input(rid="ri.foundry.main.dataset.6269482e-6137-4c64-97bc-9264db84b63a")
)
s1_data_residuals_1_1_1_2_2_2_1_1_1 <- function(unnamed_8) {
   local_df <- unnamed_8

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
#local_df$three_category_ruca=relevel(as.factor(local_df$quarter_of_diagnosis),ref="urban")
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))
local_df$i_rural_orig <- local_df$i_rural
local_df$i_rural <- strtoi(local_df$i_rural_orig,base =0L)

print(dim(local_df))
   # %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San

    md2 <- glm( i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , family=binomial, data = local_df)

    local_df$pred2 <- predict(md2,type=c('response'))
    #local_df$pred2cat <- ifelse(local_df$pred2 >=.2, 1, 0)
    local_df <- local_df %>% mutate(pred2cat = case_when(local_df$pred2 <.055 ~ 0.2,0.055<=local_df$pred2 & local_df$pred2 <.110 ~ .4,.110<=local_df$pred2 & local_df$pred2 <.165 ~ .6,.165<=local_df$pred2 & local_df$pred2<.220 ~ .8, local_df$pred2 >=.220 ~ 1))

    #local_df %>% mutate(pred2cat = case_when(local_df$pred2  >= .2 ~ 1,local_df$pred2 <.2 ~ 0))
    print("matching")
    mod_match <- matchit(i_rural ~ pred2cat +factor(data_partner_id), method = "exact", data = local_df)
    dta_m <- match.data(mod_match,drop.unmatched=FALSE)
    #dta_m <- get_matches(mod_match)

    print("PSC")
    md2 <- glm(in_death_table ~ i_rural, data = dta_m,family = binomial,weights= weights, maxit = 100)
    print(summary(md2))

    dta_m$pred2 <- predict(md2,type=c('response'))
    dta_m$rs2_self <- local_df$in_death_table - local_df$pred2

    return(dta_m)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.df49aa68-34d8-4f30-a2af-fab82d002bd8"),
    unnamed_8=Input(rid="ri.foundry.main.dataset.6269482e-6137-4c64-97bc-9264db84b63a")
)
s1_data_residuals_1_1_1_2_2_2_1_1_1_1 <- function(unnamed_8) {
   local_df <- unnamed_8

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
#local_df$three_category_ruca=relevel(as.factor(local_df$quarter_of_diagnosis),ref="urban")
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))
local_df$i_rural_orig <- local_df$i_rural
local_df$i_rural <- strtoi(local_df$i_rural_orig,base =0L)

print(dim(local_df))
   # %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San

    md2 <- glm( i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , family=binomial, data = local_df)

    local_df$pred2 <- predict(md2,type=c('response'))
    local_df$pred2cat <- ifelse(local_df$pred2 >=.2, 1, 0)

    #local_df %>% mutate(pred2cat = case_when(local_df$pred2  >= .2 ~ 1,local_df$pred2 <.2 ~ 0))
    print("matching")
    mod_match <- matchit(i_rural ~ pred2cat +factor(data_partner_id), method = "exact", data = local_df)
    dta_m <- match.data(mod_match,drop.unmatched=FALSE)
    dta_m <- get_matches(mod_match)

    print("PSC")
    md2 <- glm(in_death_table ~ i_rural, data = dta_m,family = binomial,weights= weights, maxit = 100)
    print(summary(md2))

    dta_m$pred2 <- predict(md2,type=c('response'))
    dta_m$rs2_self <- local_df$in_death_table - local_df$pred2

    return(dta_m)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.3740bc25-a75a-492a-afbe-874b1c1346a1"),
    unnamed_8=Input(rid="ri.foundry.main.dataset.6269482e-6137-4c64-97bc-9264db84b63a")
)
s1_data_residuals_1_1_1_2_2_2_1_1_1_2 <- function(unnamed_8) {
   local_df <- unnamed_8

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
#local_df$three_category_ruca=relevel(as.factor(local_df$quarter_of_diagnosis),ref="urban")
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))
local_df$i_rural_orig <- local_df$i_rural
local_df$i_rural <- strtoi(local_df$i_rural_orig,base =0L)

print(dim(local_df))
   # %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San

    md2 <- glm( i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , family=binomial, data = local_df)

    local_df$pred2 <- predict(md2,type=c('response'))
    local_df$pred2cat <- ifelse(local_df$pred2 >=.2, 1, 0)

    #local_df %>% mutate(pred2cat = case_when(local_df$pred2  >= .2 ~ 1,local_df$pred2 <.2 ~ 0))
    print("matching")
    mod_match <- matchit(i_rural ~ pred2cat +factor(data_partner_id), method = "exact", data = local_df)
    dta_m <- match.data(mod_match,drop.unmatched=FALSE)
    #dta_m <- get_matches(mod_match)

    print("PSC")
    md2 <- glm(in_death_table ~ i_rural, data = dta_m,family = binomial,weights= weights, maxit = 100)
    print(summary(md2))

    dta_m$pred2 <- predict(md2,type=c('response'))
    dta_m$rs2_self <- local_df$in_death_table - local_df$pred2

    return(dta_m)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.d9c57356-22b9-4093-8a02-b34a0367d8e7"),
    unnamed_8=Input(rid="ri.foundry.main.dataset.6269482e-6137-4c64-97bc-9264db84b63a")
)
s1_data_residuals_1_1_1_2_2_2_1_1_1_3 <- function(unnamed_8) {
   local_df <- unnamed_8

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
#local_df$three_category_ruca=relevel(as.factor(local_df$quarter_of_diagnosis),ref="urban")
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))
local_df$i_rural_orig <- local_df$i_rural
local_df$i_rural <- strtoi(local_df$i_rural_orig,base =0L)

print(dim(local_df))
   # %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San

    md2 <- glm( i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , family=binomial, data = local_df)

    local_df$pred2 <- predict(md2,type=c('response'))

    g <- qplot(local_df$pred2, geom="histogram", binwidth = .05,)

    plot(g)

    return(NULL)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.b777fc13-7ecf-4ccc-83f1-57374e51c4b0"),
    unnamed_8=Input(rid="ri.foundry.main.dataset.6269482e-6137-4c64-97bc-9264db84b63a")
)
s1_data_residuals_1_1_1_2_2_2_1_1_1_4 <- function(unnamed_8) {
   local_df <- unnamed_8

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
#local_df$three_category_ruca=relevel(as.factor(local_df$quarter_of_diagnosis),ref="urban")
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))
local_df$i_rural_orig <- local_df$i_rural
local_df$i_rural <- strtoi(local_df$i_rural_orig,base =0L)

print(dim(local_df))
   # %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San

    md2 <- glm( i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , family=binomial, data = local_df)

    local_df$pred2 <- predict(md2,type=c('response'))
    #local_df$pred2cat <- ifelse(local_df$pred2 >=.2, 1, 0)

    local_df <- local_df %>% mutate(pred2cat = case_when(local_df$pred2 <.055 ~ 0.2,0.55<=local_df$pred2 & local_df$pred2 <.110 ~ .4,.110<=local_df$pred2 & local_df$pred2 <.165 ~ .6,.165<=local_df$pred2 & local_df$pred2<.220 ~ .8, local_df$pred2 >=.220 ~ 1))



    return(local_df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.1a0818e9-40e1-4cef-bc3b-4ee05b5b690e"),
    s1_pre_analysis=Input(rid="ri.foundry.main.dataset.64f9d685-6967-4af5-9656-9d3044b0493e")
)
s1_summ_data_residuals <- function(s1_pre_analysis) {

local_df <- s1_pre_analysis
#%>% dplyr::select(ipat,ttdeath,in_death_table,ruca_cat,i_rural,gender,age_Group,age,Race,Ethnicity,BMI_Group,BMI,Q_cat,Q_Score,Diabetes,MI,CHF,PVD,stroke,dementia,pulmonary,rheumatic,Liver,paralysis,renal,cancer,mets,hiv,any_disease,Severity_Type,ECMO,MACE,Mechnical_Ventilation,covid_peak,los,quarter_dx,data_partner_id,npats,ip_rate)

##print(summ_df <- df %>% select(gender,Race, Ethnicity, BMI_Group, Q_cat, quarter_dx,age, i_rural,age_Group,BMI,Q_Score) %>% tbl_summary(by = i_rural) %>% add_p())

# Filter out missing variables
#local_df <- filter(local_df, age_Group != "Unknown)
#local_df <- filter(local_df, gender != "Other")
#local_df <- filter(local_df, quarter_dx != "Missing")
#local_df <- filter(local_df, 29<age & age<80)

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$ruca_cat=relevel(as.factor(local_df$ruca_cat),ref="urban")
local_df$quarter_dx=relevel(as.factor(local_df$quarter_dx),ref="2020q1")

#local_df$data_partner_id <- as.factor(local_df$data_partner_id)
#local_df$any_disease <- pmax(local_df$Diabetes ,local_df$MI ,local_df$CHF ,local_df$PVD ,local_df$stroke ,local_df$dementia ,local_df$pulmonary ,local_df$rheumatic ,local_df$Liver ,local_df$paralysis ,local_df$renal ,local_df$cancer ,local_df$mets ,local_df$hiv )

print('---------------------------------------------------------------------------------')
print('Summary data group by i_rural')

table1 <- local_df %>% tbl_summary(by = i_rural) %>% add_p()
table1 <- as_tibble(table1, col_labels = FALSE)
print(table1)

return(table1)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.5c10d929-64fd-4c54-b579-9787ee58f8eb"),
    s1_data_residuals_1=Input(rid="ri.foundry.main.dataset.2b38f775-6bda-4b43-9093-9b20a2d1c865")
)
## collapse residuals by variables

s1_summ_resid_NOTinvars_1 <- function(s1_data_residuals_1) {

    local_df <- s1_data_residuals_1 %>%
        dplyr::select(ttdeath,in_death_table,ruca_cat,age_Group,Race,Ethnicity,BMI_Group,Q_Score,Diabetes,MI,CHF,PVD,stroke,dementia,pulmonary,rheumatic,Liver,paralysis,renal,cancer,mets,hiv,Severity_Type,ECMO,MACE,Mechnical_Ventilation,covid_peak,data_partner_id,pred2,rs2,rs2_self)

    mean_all <- mean(local_df$rs2)
    print(paste("Checksum ", mean_all))
    mean_all <- mean(local_df$rs2_self)
    print(paste("San's Checksum ", mean_all))

    #stdlist_cont <- list("age","ip_rate")
    #for (x in  stdlist_cont) {
    #    print("------------------------------------------------------------------------------------")
    #    print(paste("---Set 1a: Group Residuals of continuos variable x=",x,"---"))
    #    local_df$x <- local_df[,x]
    #    mean_x <- mean(local_df$x)
    #    local_df$x <- mean_x
    #    ###tb1 <- local_df %>% group_by(x) %>% summarise(npats = n(), rs2_m=mean(rs2), std_err = sd(rs2)/sqrt(npats))
    #    tb2 <- local_df %>% group_by(x) %>% summarise(npats = n(),percent =100, rs2_m=mean(rs2_self))
    #    tb2 <- as_tibble(tb2, col_labels = FALSE)
    #    print(tb2)
    #    tb2$varname <- x
    #}

    print('------------------------------------------------------------------------------------')
    print(paste('---Set 1c: Group Residuals all in one category---'))
    local_df$x <- 'OneCategory'
    ###tb1 <- local_df %>% group_by(x) %>% summarise(npats = n(), rs2_m=mean(rs2), std_err = sd(rs2)/sqrt(npats))
    tb2 <- local_df %>% group_by(x) %>% summarise(npats = n(),percent =100, rs2_m=mean(rs2_self))
    tb2 <- as_tibble(tb2, col_labels = FALSE)
    print(tb2)
    tb2$varname <- 'OneCategory'
    nAll <-tb2$npats[1]
    print(nAll)

    nAll <-tb2$npats[1]
    print(nAll)
    stdlist_cat <- list("gender","Race", "Ethnicity","BMI_Group","Q_cat", "quarter_dx","any_disease")
    stdlist_cat <- list("in_death_table","ruca_cat","data_partner_id","age_Group","Q_Score","Diabetes","MI","CHF","PVD","stroke","dementia","pulmonary","rheumatic","Liver","paralysis","renal","cancer","mets","hiv","Severity_Type","ECMO","MACE","Mechnical_Ventilation")
    #stdlist_cat <- list("gender")

   for (x in  stdlist_cat){
        print("------------------------------------------------------------------------------------")
        print(paste("---Set 1b: Group Residuals for each cat variable in md2 models - variable x = ",x,"---"))
        local_df$x <- local_df[,x]
        ###tb1 <- local_df %>% group_by(x) %>% summarise(npats = n(), rs2_m=mean(rs2), std_err = sd(rs2)/sqrt(npats))
        tb1 <- local_df %>% group_by(x) %>% summarise(npats = n(), percent = 100*npats/nAll,rs2_m=mean(rs2_self))
        tb1 <- as_tibble(tb1, col_labels = FALSE)
        #tb1 <- local_df %>% tbl_summary( type = all_continuous() ~ "continuous2",
        #       statistic = all_continuous() ~ c("{median} ({p25}, {p75})", "{min}, {max}"),
        #        missing = "no" )
        print(tb1)
        tb1$varname <-x
        tb2 <- rbind(tb2,tb1)
    }

    barplot(tb2$rs2_m,name.arg=tb2$varname, col="red")
    #, horiz=TRUE)

    return(tb2)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.2ab9fd05-f750-4723-b632-6dac408addf5"),
    s1_data_residuals_1=Input(rid="ri.foundry.main.dataset.2b38f775-6bda-4b43-9093-9b20a2d1c865")
)
## collapse residuals by variables

s1_summ_resid_invars <- function(s1_data_residuals_1) {

    fn_resid_1 <- function(local_df, x) {
        print("------------------------------------------------------------------------------------")
        print(paste("---Set 1b: Group Residuals for each cat variable in md2 models - variable x = ",x,"---"))
        local_df$x <- local_df[,x]
        ###tb1 <- local_df %>% group_by(x) %>% summarise(npats = n(), rs2_m=mean(rs2), std_err = sd(rs2)/sqrt(npats))
        tb1 <- local_df %>% group_by(x) %>% summarise(npats = n(), percent = 100*npats/nAll,rs2_m=mean(rs2_self))
        tb1 <- as_tibble(tb1, col_labels = FALSE)
        print(tb1)
        tb1$varname <-x

        #dta$ACE_ARB <- as.factor(dta$ACE_ARB)
        barplot(tb1$rs2_m,name.arg=tb1$varname, col="light blue")
        #M <- cbind(tb1$varname,tb1$rs2_m)
        #f<-plot(M)
        abline(0,0,add=TRUE, col='red')
        #plot(tb1$percent,tb1$rs2_m, add=TRUE)
        #plot(tb1$rs2_m)

    }

    local_df <- s1_data_residuals_1 %>%
        dplyr::select(in_death_table, gender, Race, Ethnicity,BMI,BMI_Group,Q_Score, Q_cat,age,age_Group, quarter_dx,i_rural, any_disease, rs2,rs2_self )

    mean_all <- mean(local_df$rs2)
    print(paste("Checksum ", mean_all))
    mean_all <- mean(local_df$rs2_self)
    print(paste("San's Checksum ", mean_all))
    nAll <- nrow(local_df)

    par(mfrow=c(1,8))

    #grid.arrange(
    fn_resid_1(local_df, "gender")
    fn_resid_1(local_df, "Race")
    fn_resid_1(local_df, "Ethnicity")
    fn_resid_1(local_df, "BMI_Group")
    fn_resid_1(local_df, "Q_cat")
    fn_resid_1(local_df, "quarter_dx")
    fn_resid_1(local_df, "any_disease")


    return(NULL)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.89db4e4e-3cd6-4b3e-8cc4-d62a06d36108"),
    s1_data_residuals_1=Input(rid="ri.foundry.main.dataset.2b38f775-6bda-4b43-9093-9b20a2d1c865")
)
## collapse residuals by variables

s1_summ_resid_invars_1 <- function(s1_data_residuals_1) {

    ##Graphs of residuals vs selected vars (both included and excluded)

    fn_resid_1 <- function(dta, variable) {
    dta$variable <- dta[, variable]
    #dta$ACE_ARB <- as.factor(dta$ACE_ARB)
    ggplot(dta, aes(x = variable , y = rs2_self, color = in_death_table)) +
        geom_point(alpha = 0.2, size = 1.5) +
        xlab(variable) +
        ylab("Residuals") +
        theme_bw()
     }

fn_resid_2 <- function(dta, variable) {
    dta$variable <- as.factor(dta[, variable])
    ggplot(dta, aes(x = variable , y = rs2_self, color = 1-in_death_table)) +
        geom_point(alpha = 0.2, size = 1.5) +
        xlab(variable) +
        ylab("Residuals") +
        theme_bw()
    }

local_df2 <-s1_data_residuals_1

grid.arrange(
    fn_resid_2(local_df2, "gender") + theme(legend.position = "none"),
    fn_resid_2(local_df2, "Race") + theme(legend.position = "none"),
    fn_resid_2(local_df2, "Ethnicity")+ theme(legend.position = "none"),
    fn_resid_2(local_df2, "BMI_Group")+ theme(legend.position = "none"),
    fn_resid_2(local_df2, "Q_cat")+ theme(legend.position = "none"),
    fn_resid_2(local_df2, "quarter_dx")+ theme(legend.position = "none"),
    fn_resid_2(local_df2, "any_disease")+ theme(legend.position = "none"),

    fn_resid_1(local_df2, "age")+ theme(legend.position = "none"),
    fn_resid_1(local_df2, "ip_rate")+ theme(legend.position = "none"),

    fn_resid_2(local_df2, "age_Group")+ theme(legend.position = "none"),
    fn_resid_2(local_df2, "ruca_cat")+ theme(legend.position = "none"),
    fn_resid_2(local_df2, "data_partner_id")+ theme(legend.position = "none"),
    fn_resid_2(local_df2, "Diabetes")+ theme(legend.position = "none"),
    fn_resid_2(local_df2, "MI")+ theme(legend.position = "none"),
    fn_resid_2(local_df2, "CHF")+ theme(legend.position = "none"),
    fn_resid_2(local_df2, "PVD")+ theme(legend.position = "none"),
    fn_resid_2(local_df2, "stroke")+ theme(legend.position = "none"),
    fn_resid_2(local_df2, "dementia")+ theme(legend.position = "none"),
    fn_resid_2(local_df2, "pulmonary")+ theme(legend.position = "none"),
    fn_resid_2(local_df2, "rheumatic")+ theme(legend.position = "none"),
    fn_resid_2(local_df2, "Liver")+ theme(legend.position = "none"),
    fn_resid_2(local_df2, "paralysis")+ theme(legend.position = "none"),
    fn_resid_2(local_df2, "renal")+ theme(legend.position = "none"),
    fn_resid_2(local_df2, "cancer")+ theme(legend.position = "none"),
    fn_resid_2(local_df2, "mets")+ theme(legend.position = "none"),
    fn_resid_2(local_df2, "hiv")+ theme(legend.position = "none"),

    fn_resid_1(local_df2, "Q_Score")+ theme(legend.position = "none"),
    fn_resid_1(local_df2, "BMI")+ theme(legend.position = "none"),
    fn_resid_1(local_df2, "los")+ theme(legend.position = "none"),
    fn_resid_2(local_df2, "covid_peak")+ theme(legend.position = "none"),

    fn_resid_2(local_df2, "Severity_Type")+ theme(legend.position = "none"),
    fn_resid_2(local_df2, "ECMO")+ theme(legend.position = "none"),
    fn_resid_2(local_df2, "MACE")+ theme(legend.position = "none"),
    fn_resid_2(local_df2, "Mechnical_Ventilation")+ theme(legend.position = "none"),

    nrow = 10, widths = c(1,1,1,1,1))

    #layout_matrix = rbind(c(1,2,3,4),c(5,6,7,NA),c(8,9,NA,NA),c(10,11,NA,NA),c(12,13,14,15),c(16,17,18,19,20))
    return (NULL)

}



@transform_pandas(
    Output(rid="ri.vector.main.execute.1bd718a3-e1a2-4218-b122-8e9518e20131"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
#All BMI cats kept
#md2:  glm(in_death_table ~ i_rural + r_rural + factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 100)

s2_dt_i_r_rural <- function(unnamed_17) {

   local_df <- unnamed_17
      #  %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    # Filter out missing variables
    local_df <- filter(local_df, age_Group != "Unknown/Missing")
    local_df <- filter(local_df, gender != "Other")
    local_df <- filter(local_df, gender != "Unknown")
    local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
    #local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

    # Set reference categories
    local_df$Race=relevel(as.factor(local_df$Race),ref="White")
    local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
    local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
    local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
    local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
    local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))


    md1 <- glm(in_death_table ~ i_rural + r_rural + factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 20)
    print(summary(md1))

    local_df$pred2 <- predict(md1,type=c('response'))
    local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md1 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)

    return(local_df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.4ab41c9d-bbab-4ad5-af15-6c2b878e7389"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
#demos adjust
#All BMI cats kept, combined
#md1: glm(in_death_table ~ r_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 100)
s2_dt_i_rural_adj <- function(unnamed_17) {

   local_df <- unnamed_17
      #  %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    # Filter out missing variables
    local_df <- filter(local_df, age_Group != "Unknown/Missing")
    local_df <- filter(local_df, gender != "Other")
    local_df <- filter(local_df, gender != "Unknown")
    local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
    #local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

    # Set reference categories
    local_df$Race=relevel(as.factor(local_df$Race),ref="White")
    local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
    local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
    local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
    local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
    local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))
    # local_df$i_rural <- itostr(local_df$i_rural_orig,base =0L)


    md1 <- glm(in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX  + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 20)
    print(summary(md1))

    local_df$pred2 <- predict(md1,type=c('response'))
    local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md1 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)

    return(local_df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.566964b0-8add-4f1e-9037-d861158e54be"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
#All BMI cats kept
#md0:glm(in_death_table ~ i_rural   + factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 100)

s2_dt_i_rural_adj_0 <- function(unnamed_17) {
    local_df <- unnamed_17
      #  %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    # Filter out missing variables
    local_df <- filter(local_df, age_Group != "Unknown/Missing")
    local_df <- filter(local_df, gender != "Other")
    local_df <- filter(local_df, gender != "Unknown")
    local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
    #local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

    # Set reference categories
    local_df$Race=relevel(as.factor(local_df$Race),ref="White")
    local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
    local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
    local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
    local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
    local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San
    #md2 <- glm( in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease +factor(data_partner_id), family=binomial, data = local_df)


    md1 <- glm(in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 20)
    print(summary(md1))

    local_df$pred2 <- predict(md1,type=c('response'))
    local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md1 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)

    return(local_df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.774ebfc9-21ab-43c0-b3f3-c1a9f6a2a466"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
#demos adjust
#All BMI cats kept, combined
#md1: glm(in_death_table ~ r_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 100)
s2_dt_i_rural_dp_fe <- function(unnamed_17) {

   local_df <- unnamed_17
      #  %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    # Filter out missing variables
    local_df <- filter(local_df, age_Group != "Unknown/Missing")
    local_df <- filter(local_df, gender != "Other")
    local_df <- filter(local_df, gender != "Unknown")
    local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
    #local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

    # Set reference categories
    local_df$Race=relevel(as.factor(local_df$Race),ref="White")
    local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
    local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
    local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
    local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
    local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

    md1 <- glm(in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX  + factor(quarter_of_diagnosis) + factor(data_partner_id), data = local_df, family = binomial, maxit = 20)
    print(summary(md1))

    local_df$pred2 <- predict(md1,type=c('response'))
    local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md1 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)

    return(local_df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.8d0ee150-6af0-4d87-a9cc-10538f75ce90"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
#demos adjust
#All BMI cats kept, combined
#md1: glm(in_death_table ~ r_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) +factor(BMI_Group)*factor(age_Group) , data = local_df, family = binomial, maxit = 100)
s2_dt_i_rural_dp_fe_1 <- function(unnamed_17) {

   local_df <- unnamed_17
      #  %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    # Filter out missing variables
    local_df <- filter(local_df, age_Group != "Unknown/Missing")
    local_df <- filter(local_df, gender != "Other")
    local_df <- filter(local_df, gender != "Unknown")
    local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
    #local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

    # Set reference categories
    local_df$Race=relevel(as.factor(local_df$Race),ref="White")
    local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
    local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
    local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
    local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
    local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

    md1 <- glm(in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group)  + CCI_INDEX  + factor(quarter_of_diagnosis) + factor(BMI_Group)*factor(age_Group) + factor(data_partner_id), data = local_df, family = binomial, maxit = 20)
    print(summary(md1))

    local_df$pred2 <- predict(md1,type=c('response'))
    local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md1 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)

    return(local_df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.cf5ebbfa-9246-4934-a418-2e6780cb38fd"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
#demos adjust
#All BMI cats kept, combined
#md1: glm(in_death_table ~ r_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) +i_rural*factor(quarter_of_diagnosis)+i_rural*CCI_INDEX + i_rural*factor(BMI_Group), data = local_df, family = binomial, maxit = 100)
s2_dt_i_rural_dp_fe_intact3 <- function(unnamed_17) {

   local_df <- unnamed_17
      #  %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    # Filter out missing variables
    local_df <- filter(local_df, age_Group != "Unknown/Missing")
    local_df <- filter(local_df, gender != "Other")
    local_df <- filter(local_df, gender != "Unknown")
    local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
    #local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

    # Set reference categories
    local_df$Race=relevel(as.factor(local_df$Race),ref="White")
    local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
    local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
    local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
    local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
    #local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

    md1 <- glm(in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX  + factor(quarter_of_diagnosis) + factor(data_partner_id) + i_rural*factor(quarter_of_diagnosis)+i_rural*CCI_INDEX + i_rural*factor(BMI_Group), data = local_df, family = binomial, maxit = 20)
    print(summary(md1))

    local_df$pred2 <- predict(md1,type=c('response'))
    local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md1 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)

    return(local_df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.4df0c4a0-cfa9-4030-96c7-e3078e818d81"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
s2_dt_i_rural_dp_psc <- function(unnamed_17) {
   local_df <- unnamed_17

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
#local_df$three_category_ruca=relevel(as.factor(local_df$quarter_of_diagnosis),ref="urban")
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))
local_df$i_rural_orig <- local_df$i_rural
#ocal_df$i_rural <- strtoi(local_df$i_rural_orig,base =0L)

print(dim(local_df))
   # %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San

    md2 <- glm( i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX  + factor(quarter_of_diagnosis) , family=binomial, data = local_df)

    local_df$pred2 <- predict(md2,type=c('response'))
    #local_df$pred2cat <- ifelse(local_df$pred2 >=.2, 1, 0)
    g <- qplot(local_df$pred2, geom="histogram")
    plot(g)

    local_df <- local_df %>% mutate(pred2cat = case_when(local_df$pred2 <.055 ~ 0.2,0.055<=local_df$pred2 & local_df$pred2 <.110 ~ .4,.110<=local_df$pred2 & local_df$pred2 <.165 ~ .6,.165<=local_df$pred2 & local_df$pred2<.220 ~ .8, local_df$pred2 >=.220 ~ 1))

    #local_df %>% mutate(pred2cat = case_when(local_df$pred2  >= .2 ~ 1,local_df$pred2 <.2 ~ 0))
    print("matching")
    mod_match <- matchit(i_rural ~ pred2cat +factor(data_partner_id), method = "exact", data = local_df)
    dta_m <- match.data(mod_match,drop.unmatched=FALSE)
    #dta_m <- get_matches(mod_match)

    print("PSC")
    md2 <- glm(in_death_table ~ i_rural, data = dta_m,family = binomial,weights= weights, maxit = 100)
    print(summary(md2))

    dta_m$pred2 <- predict(md2,type=c('response'))
    dta_m$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md2 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)


    return(dta_m)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.be86a591-a2c8-4467-9238-7d08b5a25874"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
library(mice)

#library(VIM)

s2_dt_i_rural_dp_psc_imputeBMI <- function(unnamed_17) {

local_df <- unnamed_17 %>%
    dplyr::select(in_death_table,i_rural,gender,age_Group,Race,Ethnicity,BMI_Group,quarter_of_diagnosis,CCI_INDEX,data_partner_id,Severity_Type,ECMO,MACE,Mechnical_Ventilation)

    #drop ttdeath, los, BMI, age, Q_Score, Q_Score_Categories, CCI_Categories, all diseases, three_category_ruca,four_category_ruca,ip_rate,r_rural
    #summary(local_df)
    #gp2: replace cat (gender, BMI_Group, age_Group, quarter_of_diagnosis, Race, Ethnicity) Unknown/missing back to missing, impute


    local_df$gender[local_df$gender == "Unknown"] <- NA
    local_df$BMI_Group[local_df$BMI_Group == "Unknown/Missing"] <- NA
    local_df$age_Group[local_df$age_Group == "Unknown/Missing"] <- NA
    local_df$quarter_of_diagnosis[local_df$quarter_of_diagnosis == "Unknown/Missing"] <- NA

    local_df$Race[local_df$Race == "Missing/Unknown"] <- NA
    local_df$Ethnicity[local_df$Ethnicity == "Missing/Unknown"] <- NA

    #local_df <- head(local_df,100)
    #str(local_df)

    local_df$Race <- as.factor(local_df$Race)
    local_df$gender <- as.factor(local_df$gender)
    local_df$Ethnicity <- as.factor(local_df$Ethnicity)
    local_df$age_Group <- as.factor(local_df$age_Group)
    local_df$BMI_Group <- as.factor(local_df$BMI_Group)
    local_df$quarter_of_diagnosis <- as.factor(local_df$quarter_of_diagnosis)

    local_df$data_partner_id <- as.factor(local_df$data_partner_id)
    #local_df$Severity_Type <- as.factor(local_df$Severity_Type)

    #print(md.pattern(local_df))
    #print(head(local_df,20))
    tempData <- mice(local_df, m=5, maxit = 40)

    #local_df_imputed <- complete(tempData,1)
    #fit a linear model on all datasets together
    #lm_5_model=with(mice_imputes,lm(chl~age+bmi+hyp))


    lm_5_model=with(tempData,lm(i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX  + factor(quarter_of_diagnosis) , family=binomial))
    #print(summary(lm_5_model))

    #Use the pool() function to combine the results of all the models
    combo_5_model <- pool(lm_5_model)
    #str(combo_5_model)

    tb1 <- combo_5_model$pooled
    #print('------------------OR------------------------------------')
    #tb1 <- combo_5_model %>% tbl_regression(exponentiate = TRUE)
    #tb1 <- as_tibble(tb1, col_labels = FALSE)
    #print(tb1)

    #return(NULL)
    return(tb1)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.5f2e9cd7-ef6b-4689-b0b4-08ac20a03a36"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
library(mice)

#library(VIM)

s2_dt_i_rural_dp_psc_imputeBMI_1 <- function(unnamed_17) {

local_df <- unnamed_17 %>%
    dplyr::select(in_death_table,i_rural,gender,age_Group,Race,Ethnicity,BMI_Group,quarter_of_diagnosis,CCI_INDEX,data_partner_id,Severity_Type,ECMO,MACE,Mechnical_Ventilation,ip_rate,r_rural)

    #drop ttdeath, los, BMI, age, Q_Score, Q_Score_Categories, CCI_Categories, all diseases
    #summary(local_df)
    #gp2: replace cat (gender, BMI_Group, age_Group, quarter_of_diagnosis, Race, Ethnicity) Unknown/missing back to missing, impute

    str(local_df)

    local_df$gender[local_df$gender == "Unknown"] <- NA
    local_df$BMI_Group[local_df$BMI_Group == "Unknown/Missing"] <- NA
    local_df$age_Group[local_df$age_Group == "Unknown/Missing"] <- NA
    local_df$quarter_of_diagnosis[local_df$quarter_of_diagnosis == "Unknown/Missing"] <- NA

    local_df$Race[local_df$Race == "Missing/Unknown"] <- NA
    local_df$Ethnicity[local_df$Ethnicity == "Missing/Unknown"] <- NA

    local_df <- head(local_df,100)

    local_df$Race <- as.factor(local_df$Race)
    local_df$gender <- as.factor(local_df$gender)
    local_df$Ethnicity <- as.factor(local_df$Ethnicity)
    local_df$age_Group <- as.factor(local_df$age_Group)
    local_df$BMI_Group <- as.factor(local_df$BMI_Group)
    local_df$quarter_of_diagnosis <- as.factor(local_df$quarter_of_diagnosis)


    print(md.pattern(local_df))

    print("before")
    print(head(local_df,20))

    tempData <- mice(local_df, m=5, maxit = 40)

    local_df_imputed <- complete(tempData,1)


    # Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
#local_df$three_category_ruca=relevel(as.factor(local_df$quarter_of_diagnosis),ref="urban")
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))
local_df$i_rural_orig <- local_df$i_rural
#ocal_df$i_rural <- strtoi(local_df$i_rural_orig,base =0L)

print(dim(local_df))

    md2 <- glm( i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX  + factor(quarter_of_diagnosis) , family=binomial, data = local_df)

    local_df$pred2 <- predict(md2,type=c('response'))
    #local_df$pred2cat <- ifelse(local_df$pred2 >=.2, 1, 0)
    g <- qplot(local_df$pred2, geom="histogram")
    plot(g)


    local_df <- local_df %>% mutate(pred2cat = case_when(local_df$pred2 <.055 ~ 0.2,0.055<=local_df$pred2 & local_df$pred2 <.110 ~ .4,.110<=local_df$pred2 & local_df$pred2 <.165 ~ .6,.165<=local_df$pred2 & local_df$pred2<.220 ~ .8, local_df$pred2 >=.220 ~ 1))

    #local_df %>% mutate(pred2cat = case_when(local_df$pred2  >= .2 ~ 1,local_df$pred2 <.2 ~ 0))
    print("matching")
    mod_match <- matchit(i_rural ~ pred2cat +factor(data_partner_id), method = "exact", data = local_df)
    dta_m <- match.data(mod_match,drop.unmatched=FALSE)
    #dta_m <- get_matches(mod_match)

    print("PSC")
    md2 <- glm(in_death_table ~ i_rural, data = dta_m,family = binomial,weights= weights, maxit = 20)
    print(summary(md2))

    dta_m$pred2 <- predict(md2,type=c('response'))
    dta_m$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md2 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)



    #local_df <- filter(local_df, age_Group == NA)
    #local_df <- local_df_impute %>% dplyr::select(age_Group,gender,Race,Ethnicity,BMI_Group,quarter_of_diagnosis,CCI_INDEX,data_partner_id,r_rural,i_rural, in_death_table)
    #print("after")
    #print(head(local_df_impute,20))

    return(local_df_imputed)
    #return(tb1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.415eb984-8ba4-4c87-8bfc-f841e5f39839"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
library(mice)

#library(VIM)

s2_dt_i_rural_dp_psc_imputeBMI_2 <- function(unnamed_17) {

local_df <- unnamed_17 %>%
    dplyr::select(in_death_table,i_rural,gender,age_Group,Race,Ethnicity,BMI_Group,quarter_of_diagnosis,CCI_INDEX,data_partner_id,Severity_Type,ECMO,MACE,Mechnical_Ventilation)

    #drop ttdeath, los, BMI, age, Q_Score, Q_Score_Categories, CCI_Categories, all diseases, three_category_ruca,four_category_ruca,ip_rate,r_rural
    #summary(local_df)
    #gp2: replace cat (gender, BMI_Group, age_Group, quarter_of_diagnosis, Race, Ethnicity) Unknown/missing back to missing, impute


    local_df$gender[local_df$gender == "Unknown"] <- NA
    local_df$BMI_Group[local_df$BMI_Group == "Unknown/Missing"] <- NA
    local_df$age_Group[local_df$age_Group == "Unknown/Missing"] <- NA
    local_df$quarter_of_diagnosis[local_df$quarter_of_diagnosis == "Unknown/Missing"] <- NA

    local_df$Race[local_df$Race == "Missing/Unknown"] <- NA
    local_df$Ethnicity[local_df$Ethnicity == "Missing/Unknown"] <- NA

    #local_df <- head(local_df,100)
    #str(local_df)

    local_df$Race <- as.factor(local_df$Race)
    local_df$gender <- as.factor(local_df$gender)
    local_df$Ethnicity <- as.factor(local_df$Ethnicity)
    local_df$age_Group <- as.factor(local_df$age_Group)
    local_df$BMI_Group <- as.factor(local_df$BMI_Group)
    local_df$quarter_of_diagnosis <- as.factor(local_df$quarter_of_diagnosis)

    local_df$data_partner_id <- as.factor(local_df$data_partner_id)
    #local_df$Severity_Type <- as.factor(local_df$Severity_Type)

    #print(md.pattern(local_df))
    #print(head(local_df,20))
    tempData <- mice(local_df, m=5, maxit = 40)

    #local_df_imputed <- complete(tempData,1)
    #fit a linear model on all datasets together
    #lm_5_model=with(mice_imputes,lm(chl~age+bmi+hyp))


    lm_5_model=with(tempData,lm(i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX  + factor(quarter_of_diagnosis) + factor(data_partner_id) , family=binomial))
    #print(summary(lm_5_model))

    #Use the pool() function to combine the results of all the models
    combo_5_model <- pool(lm_5_model)
    #str(combo_5_model)

    tb1 <- combo_5_model$pooled
    #print('------------------OR------------------------------------')
    #tb1 <- combo_5_model %>% tbl_regression(exponentiate = TRUE)
    #tb1 <- as_tibble(tb1, col_labels = FALSE)
    #print(tb1)

    #return(NULL)
    return(tb1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.a428e859-6890-4cc5-ab95-a48a9e16611f"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
library(mice)
library(lme4)

#library(VIM)

s2_dt_i_rural_dp_psc_imputeBMI_2_1 <- function(unnamed_17) {

local_df <- unnamed_17 %>%
    dplyr::select(in_death_table,i_rural,gender,age_Group,Race,Ethnicity,BMI_Group,quarter_of_diagnosis,CCI_INDEX,data_partner_id,Severity_Type,ECMO,MACE,Mechnical_Ventilation)

    #drop ttdeath, los, BMI, age, Q_Score, Q_Score_Categories, CCI_Categories, all diseases, three_category_ruca,four_category_ruca,ip_rate,r_rural
    #summary(local_df)
    #gp2: replace cat (gender, BMI_Group, age_Group, quarter_of_diagnosis, Race, Ethnicity) Unknown/missing back to missing, impute


    local_df$gender[local_df$gender == "Unknown"] <- NA
    local_df$BMI_Group[local_df$BMI_Group == "Unknown/Missing"] <- NA
    local_df$age_Group[local_df$age_Group == "Unknown/Missing"] <- NA
    local_df$quarter_of_diagnosis[local_df$quarter_of_diagnosis == "Unknown/Missing"] <- NA

    local_df$Race[local_df$Race == "Missing/Unknown"] <- NA
    local_df$Ethnicity[local_df$Ethnicity == "Missing/Unknown"] <- NA

    #local_df <- head(local_df,100)
    #str(local_df)

    local_df$Race <- as.factor(local_df$Race)
    local_df$gender <- as.factor(local_df$gender)
    local_df$Ethnicity <- as.factor(local_df$Ethnicity)
    local_df$age_Group <- as.factor(local_df$age_Group)
    local_df$BMI_Group <- as.factor(local_df$BMI_Group)
    local_df$quarter_of_diagnosis <- as.factor(local_df$quarter_of_diagnosis)

    local_df$data_partner_id <- as.factor(local_df$data_partner_id)
    #local_df$Severity_Type <- as.factor(local_df$Severity_Type)

    #print(md.pattern(local_df))
    #print(head(local_df,20))
    tempData <- mice(local_df, m=5, maxit = 40)

    #local_df_imputed <- complete(tempData,1)
    #fit a linear model on all datasets together
    #lm_5_model=with(mice_imputes,lm(chl~age+bmi+hyp))


    lm_5_model=with(tempData,glmer(formula = in_death_table ~ 1 + i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX  + factor(quarter_of_diagnosis) + (1|data_partner_id), data = ., family = "binomial"))
    #print(summary(lm_5_model))

    #Use the pool() function to combine the results of all the models
    combo_5_model <- pool(lm_5_model)
    #str(combo_5_model)

    tb1 <- combo_5_model$pooled
    #print('------------------OR------------------------------------')
    #tb1 <- combo_5_model %>% tbl_regression(exponentiate = TRUE)
    #tb1 <- as_tibble(tb1, col_labels = FALSE)
    #print(tb1)

    #return(NULL)
    return(tb1)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.94280d60-1a6c-4d6b-b0d9-5a8bbe8e02b1"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
library(mice)
library(lme4)

#library(VIM)

s2_dt_i_rural_dp_psc_imputeBMI_2_1_1 <- function(unnamed_17) {

local_df <- unnamed_17 %>%
    dplyr::select(in_death_table,i_rural,gender,age_Group,Race,Ethnicity,BMI_Group,quarter_of_diagnosis,CCI_INDEX,data_partner_id,Severity_Type,ECMO,MACE,Mechnical_Ventilation)

    #drop ttdeath, los, BMI, age, Q_Score, Q_Score_Categories, CCI_Categories, all diseases, three_category_ruca,four_category_ruca,ip_rate,r_rural
    #summary(local_df)
    #gp2: replace cat (gender, BMI_Group, age_Group, quarter_of_diagnosis, Race, Ethnicity) Unknown/missing back to missing, impute


    local_df$gender[local_df$gender == "Unknown"] <- NA
    local_df$BMI_Group[local_df$BMI_Group == "Unknown/Missing"] <- NA
    local_df$age_Group[local_df$age_Group == "Unknown/Missing"] <- NA
    local_df$quarter_of_diagnosis[local_df$quarter_of_diagnosis == "Unknown/Missing"] <- NA

    local_df$Race[local_df$Race == "Missing/Unknown"] <- NA
    local_df$Ethnicity[local_df$Ethnicity == "Missing/Unknown"] <- NA

    local_df <- head(local_df,10000)
    #str(local_df)

    local_df$Race <- as.factor(local_df$Race)
    local_df$gender <- as.factor(local_df$gender)
    local_df$Ethnicity <- as.factor(local_df$Ethnicity)
    local_df$age_Group <- as.factor(local_df$age_Group)
    local_df$BMI_Group <- as.factor(local_df$BMI_Group)
    local_df$quarter_of_diagnosis <- as.factor(local_df$quarter_of_diagnosis)

    local_df$data_partner_id <- as.factor(local_df$data_partner_id)
    #local_df$Severity_Type <- as.factor(local_df$Severity_Type)

    #print(md.pattern(local_df))
    #print(head(local_df,20))
    tempData <- mice(local_df, m=5, maxit = 40)

    #local_df_imputed <- complete(tempData,1)
    #fit a linear model on all datasets together
    #lm_5_model=with(mice_imputes,lm(chl~age+bmi+hyp))


    lm_5_model=with(tempData,glmer(formula = in_death_table ~ 1 + i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX  + factor(quarter_of_diagnosis) + (1|data_partner_id), data = ., family = "binomial"))
    #print(summary(lm_5_model))

    #Use the pool() function to combine the results of all the models
    combo_5_model <- pool(lm_5_model)
    #str(combo_5_model)

    tb1 <- combo_5_model$pooled
    #print('------------------OR------------------------------------')
    #tb1 <- combo_5_model %>% tbl_regression(exponentiate = TRUE)
    #tb1 <- as_tibble(tb1, col_labels = FALSE)
    #print(tb1)

    #return(NULL)
    return(tb1)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.7f6c3132-d7ce-4852-996d-7ea5659ad252"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
library(mice)

#library(VIM)

s2_dt_i_rural_dp_psc_imputeBMI_2_2 <- function(unnamed_17) {

local_df <- unnamed_17 %>%
    dplyr::select(in_death_table,i_rural,gender,age_Group,Race,Ethnicity,BMI_Group,quarter_of_diagnosis,CCI_INDEX,data_partner_id,Severity_Type,ECMO,MACE,Mechnical_Ventilation)

    #drop ttdeath, los, BMI, age, Q_Score, Q_Score_Categories, CCI_Categories, all diseases, three_category_ruca,four_category_ruca,ip_rate,r_rural
    #summary(local_df)
    #gp2: replace cat (gender, BMI_Group, age_Group, quarter_of_diagnosis, Race, Ethnicity) Unknown/missing back to missing, impute


    local_df$gender[local_df$gender == "Unknown"] <- NA
    local_df$BMI_Group[local_df$BMI_Group == "Unknown/Missing"] <- NA
    local_df$age_Group[local_df$age_Group == "Unknown/Missing"] <- NA
    local_df$quarter_of_diagnosis[local_df$quarter_of_diagnosis == "Unknown/Missing"] <- NA

    local_df$Race[local_df$Race == "Missing/Unknown"] <- NA
    local_df$Ethnicity[local_df$Ethnicity == "Missing/Unknown"] <- NA

    local_df <- head(local_df,10000)
    #str(local_df)

    local_df$Race <- as.factor(local_df$Race)
    local_df$gender <- as.factor(local_df$gender)
    local_df$Ethnicity <- as.factor(local_df$Ethnicity)
    local_df$age_Group <- as.factor(local_df$age_Group)
    local_df$BMI_Group <- as.factor(local_df$BMI_Group)
    local_df$quarter_of_diagnosis <- as.factor(local_df$quarter_of_diagnosis)

    local_df$data_partner_id <- as.factor(local_df$data_partner_id)
    #local_df$Severity_Type <- as.factor(local_df$Severity_Type)
    str(local_df)

    #print(md.pattern(local_df))
    #print(head(local_df,20))
    tempData <- mice(local_df, m=5, maxit = 40)
    str(tempData)
    #local_df_imputed <- complete(tempData,1)
    #fit a linear model on all datasets together
    #lm_5_model=with(mice_imputes,lm(chl~age+bmi+hyp))


    #lm_5_model=with(tempData,lm(i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX + factor(quarter_of_diagnosis) + factor(data_partner_id) , family=binomial))
    lm_5_model=with(tempData,lm(i_rural ~ gender + Race + Ethnicity + BMI_Group + age_Group + CCI_INDEX + quarter_of_diagnosis + data_partner_id , family=binomial))
    #print(summary(lm_5_model))

    #Use the pool() function to combine the results of all the models
    combo_5_model <- pool(lm_5_model)
    #str(combo_5_model)

    tb1 <- combo_5_model$pooled
    #print('------------------OR------------------------------------')
    #tb1 <- combo_5_model %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)

    #return(NULL)
    return(tb1)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.5329b29d-96ec-4331-ab60-309cf83b6117"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
library(mice)

#library(VIM)

s2_dt_i_rural_dp_psc_imputeBMI_3 <- function(unnamed_17) {

local_df <- unnamed_17 %>%
    dplyr::select(in_death_table,i_rural,gender,age_Group,Race,Ethnicity,BMI_Group,quarter_of_diagnosis,CCI_INDEX,data_partner_id,Severity_Type,ECMO,MACE,Mechnical_Ventilation)

    #drop ttdeath, los, BMI, age, Q_Score, Q_Score_Categories, CCI_Categories, all diseases, three_category_ruca,four_category_ruca,ip_rate,r_rural
    #summary(local_df)
    #gp2: replace cat (gender, BMI_Group, age_Group, quarter_of_diagnosis, Race, Ethnicity) Unknown/missing back to missing, impute


    local_df$gender[local_df$gender == "Unknown"] <- NA
    local_df$BMI_Group[local_df$BMI_Group == "Unknown/Missing"] <- NA
    local_df$age_Group[local_df$age_Group == "Unknown/Missing"] <- NA
    local_df$quarter_of_diagnosis[local_df$quarter_of_diagnosis == "Unknown/Missing"] <- NA

    local_df$Race[local_df$Race == "Missing/Unknown"] <- NA
    local_df$Ethnicity[local_df$Ethnicity == "Missing/Unknown"] <- NA

    local_df <- head(local_df,100)
    #str(local_df)

    local_df$Race <- as.factor(local_df$Race)
    local_df$gender <- as.factor(local_df$gender)
    local_df$Ethnicity <- as.factor(local_df$Ethnicity)
    local_df$age_Group <- as.factor(local_df$age_Group)
    local_df$BMI_Group <- as.factor(local_df$BMI_Group)
    local_df$quarter_of_diagnosis <- as.factor(local_df$quarter_of_diagnosis)

    local_df$data_partner_id <- as.factor(local_df$data_partner_id)
    #local_df$Severity_Type <- as.factor(local_df$Severity_Type)

    #print(md.pattern(local_df))
    #print(head(local_df,20))
    tempData <- mice(local_df, m=5, maxit = 40)

    #local_df_imputed <- complete(tempData,1)
    #fit a linear model on all datasets together
    #lm_5_model=with(mice_imputes,lm(chl~age+bmi+hyp))


    lm_5_model=with(tempData,lm(i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX  + factor(quarter_of_diagnosis) , family=binomial))
    #print(summary(lm_5_model))

    #Use the pool() function to combine the results of all the models
    combo_5_model <- pool(lm_5_model)
    #str(combo_5_model)

    tb1 <- combo_5_model$pooled
    #print('------------------OR------------------------------------')
    #tb1 <- combo_5_model %>% tbl_regression(exponentiate = TRUE)
    #tb1 <- as_tibble(tb1, col_labels = FALSE)
    #print(tb1)

    #return(NULL)
    return(tb1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.30ef8c73-75b1-4409-aabf-6928b10a2807"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
#demos adjust
#All BMI cats kept, combined
#md1: glm(in_death_table ~ r_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 100)
s2_dt_i_rural_dp_re <- function(unnamed_17) {

   local_df <- unnamed_17
      #  %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    # Filter out missing variables
    local_df <- filter(local_df, age_Group != "Unknown/Missing")
    local_df <- filter(local_df, gender != "Other")
    local_df <- filter(local_df, gender != "Unknown")
    local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
    #local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

    # Set reference categories
    local_df$Race=relevel(as.factor(local_df$Race),ref="White")
    local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
    local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
    local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
    local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
    local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))
    #local_df$data_partner_id <- itostr(local_df$data_partner_id,  base = 10L)

    md1 <- glm(in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX  + factor(quarter_of_diagnosis) + (1|data_partner_id), data = local_df, family = binomial, maxit = 20)
    print(summary(md1))

    local_df$pred2 <- predict(md1,type=c('response'))
    local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md1 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)

    return(local_df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.30207fb2-1292-4105-8adf-181320848f4e"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
#demos adjust
#All BMI cats kept, combined
#md1: glm(in_death_table ~ r_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 100)
s2_dt_i_rural_dp_re_slopes <- function(unnamed_17) {

   local_df <- unnamed_17
      #  %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    # Filter out missing variables
    local_df <- filter(local_df, age_Group != "Unknown/Missing")
    local_df <- filter(local_df, gender != "Other")
    local_df <- filter(local_df, gender != "Unknown")
    local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
    #local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

    # Set reference categories
    local_df$Race=relevel(as.factor(local_df$Race),ref="White")
    local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
    local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
    local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
    local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
    local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))
    #local_df$data_partner_id <- as.factor(local_df$data_partner_id)


    md1 <- glm(in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX  + factor(quarter_of_diagnosis) + (1|data_partner_id) + (0 + r_rural|data_partner_id), data = local_df, family = binomial, maxit = 20)
    print(summary(md1))

    local_df$pred2 <- predict(md1,type=c('response'))
    local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md1 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)

    return(local_df)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.ff204ba9-400a-4c77-be97-dcf6e1197fc9"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
#All BMI cats kept
#md3:glm(in_death_table ~ i_rural + r_rural  + i_rural*r_rural   + factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 100)

s2_dt_int_i_r_rural <- function(unnamed_17) {

   local_df <- unnamed_17
      #  %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    # Filter out missing variables
    local_df <- filter(local_df, age_Group != "Unknown/Missing")
    local_df <- filter(local_df, gender != "Other")
    local_df <- filter(local_df, gender != "Unknown")
    local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
    #local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

    # Set reference categories
    local_df$Race=relevel(as.factor(local_df$Race),ref="White")
    local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
    local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
    local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
    local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
    local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))



    md1 <- glm(in_death_table ~ i_rural + r_rural  + i_rural*r_rural   + factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 20)
    print(summary(md1))

    local_df$pred2 <- predict(md1,type=c('response'))
    local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md1 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)

    return(local_df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.2614cacf-c819-441a-99dd-d1f0861fcb10"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
#demos adjust
#All BMI cats kept, combined
#md1: glm(in_death_table ~ r_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 100)
s2_dt_r_rural_adj <- function(unnamed_17) {

   local_df <- unnamed_17
      #  %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    # Filter out missing variables
    local_df <- filter(local_df, age_Group != "Unknown/Missing")
    local_df <- filter(local_df, gender != "Other")
    local_df <- filter(local_df, gender != "Unknown")
    local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
    #local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

    # Set reference categories
    local_df$Race=relevel(as.factor(local_df$Race),ref="White")
    local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
    local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
    local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
    local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
    local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San
    #md2 <- glm( in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease +factor(data_partner_id), family=binomial, data = local_df)


    md1 <- glm(in_death_table ~ r_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 100)
    print(summary(md1))

    local_df$pred2 <- predict(md1,type=c('response'))
    local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md1 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)

    return(local_df)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.83131250-4729-40e8-bed0-f8c71230c2d2"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
#r_rural fix effects + adjust
#All BMI cats kept, combined
#md1: glm(in_death_table ~ r_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 100)
s2_dt_r_rural_dp_fe <- function(unnamed_17) {

   local_df <- unnamed_17
      #  %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    # Filter out missing variables
    local_df <- filter(local_df, age_Group != "Unknown/Missing")
    local_df <- filter(local_df, gender != "Other")
    local_df <- filter(local_df, gender != "Unknown")
    local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
    #local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

    # Set reference categories
    local_df$Race=relevel(as.factor(local_df$Race),ref="White")
    local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
    local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
    local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
    local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
    local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San
    #md2 <- glm( in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease +factor(data_partner_id), family=binomial, data = local_df)


    md1 <- glm(in_death_table ~ r_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) +factor(data_partner_id), data = local_df, family = binomial, maxit = 100)
    print(summary(md1))

    local_df$pred2 <- predict(md1,type=c('response'))
    local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md1 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)

    return(local_df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3654c222-11f1-4668-a46a-cbf5488a1409"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
s2_dt_r_rural_dp_psc_age <- function(unnamed_17) {
   local_df <- unnamed_17

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
#local_df$three_category_ruca=relevel(as.factor(local_df$quarter_of_diagnosis),ref="urban")
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))
local_df$i_rural_orig <- local_df$i_rural
#local_df$i_rural <- strtoi(local_df$i_rural_orig,base =0L)

print(dim(local_df))
   # %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San

    md2 <- glm( i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , family=binomial, data = local_df)

    local_df$pred2 <- predict(md2,type=c('response'))
    #local_df$pred2cat <- ifelse(local_df$pred2 >=.2, 1, 0)
    g <- qplot(local_df$pred2, geom="histogram")
    plot(g)

    local_df <- local_df %>% mutate(pred2cat = case_when(local_df$pred2 <.055 ~ 0.2,0.055<=local_df$pred2 & local_df$pred2 <.110 ~ .4,.110<=local_df$pred2 & local_df$pred2 <.165 ~ .6,.165<=local_df$pred2 & local_df$pred2<.220 ~ .8, local_df$pred2 >=.220 ~ 1))

    #local_df %>% mutate(pred2cat = case_when(local_df$pred2  >= .2 ~ 1,local_df$pred2 <.2 ~ 0))
    print("matching")
    mod_match <- matchit(i_rural ~ pred2cat +factor(data_partner_id), method = "exact", data = local_df)
    dta_m <- match.data(mod_match,drop.unmatched=FALSE)
    #dta_m <- get_matches(mod_match)

    print("PSC")
    md2 <- glm(in_death_table ~ i_rural, data = dta_m,family = binomial,weights= weights, maxit = 100)
    print(summary(md2))

    dta_m$pred2 <- predict(md2,type=c('response'))
    dta_m$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md2 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)


    return(dta_m)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.6b75d4f6-742b-441c-8493-d7f9f2b79885"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
#r_rural fix effects + adjust
#All BMI cats kept, combined
#md1: glm(in_death_table ~ r_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 100)
s2_dt_r_rural_dp_re <- function(unnamed_17) {

   local_df <- unnamed_17
      #  %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    # Filter out missing variables
    local_df <- filter(local_df, age_Group != "Unknown/Missing")
    local_df <- filter(local_df, gender != "Other")
    local_df <- filter(local_df, gender != "Unknown")
    local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
    #local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

    # Set reference categories
    local_df$Race=relevel(as.factor(local_df$Race),ref="White")
    local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
    local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
    local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
    local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
    local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San
    #md2 <- glm( in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease +factor(data_partner_id), family=binomial, data = local_df)


    md1 <- glm(in_death_table ~ r_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) + (1|data_partner_id), data = local_df, family = binomial, maxit = 100)
    print(summary(md1))

    local_df$pred2 <- predict(md1,type=c('response'))
    local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md1 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)

    return(local_df)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.9713fc7f-5087-41fb-b239-390772cb7dc9"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
#r_rural fix effects + adjust
#All BMI cats kept, combined
#md1: glm(in_death_table ~ r_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 100)
s2_dt_r_rural_dp_re_slopes <- function(unnamed_17) {

   local_df <- unnamed_17
      #  %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    # Filter out missing variables
    local_df <- filter(local_df, age_Group != "Unknown/Missing")
    local_df <- filter(local_df, gender != "Other")
    local_df <- filter(local_df, gender != "Unknown")
    local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
    #local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

    # Set reference categories
    local_df$Race=relevel(as.factor(local_df$Race),ref="White")
    local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
    local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
    local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
    local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
    local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San
    #md2 <- glm( in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease +factor(data_partner_id), family=binomial, data = local_df)


    md1 <- glm(in_death_table ~ r_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) + (1|data_partner_id) + (0 + r_rural|data_partner_id), data = local_df, family = binomial, maxit = 100)
    print(summary(md1))

    local_df$pred2 <- predict(md1,type=c('response'))
    local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md1 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)

    return(local_df)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.9f0e6cec-e0ca-4f9c-b8df-d2c747045d56"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
#only r_rural
#All BMI cats kept, combined
#md1: glm(in_death_table ~ r_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 100)
s2_dt_r_rural_only <- function(unnamed_17) {

   local_df <- unnamed_17
      #  %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    # Filter out missing variables
    local_df <- filter(local_df, age_Group != "Unknown/Missing")
    local_df <- filter(local_df, gender != "Other")
    local_df <- filter(local_df, gender != "Unknown")
    local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
    #local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

    # Set reference categories
    local_df$Race=relevel(as.factor(local_df$Race),ref="White")
    local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
    local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
    local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
    local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
    local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San
    #md2 <- glm( in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease +factor(data_partner_id), family=binomial, data = local_df)


    md1 <- glm(in_death_table ~ r_rural, data = local_df, family = binomial, maxit = 100)
    print(summary(md1))

    local_df$pred2 <- predict(md1,type=c('response'))
    local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md1 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)

    return(local_df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3035f7ef-e9ec-439a-9519-6dcee27977b5"),
    s2_dt_r_rural_adj=Input(rid="ri.foundry.main.dataset.2614cacf-c819-441a-99dd-d1f0861fcb10")
)
#Rural
s2_dt_r_rural_rural <- function(s2_dt_r_rural_adj) {
   local_df <- s2_dt_r_rural_adj
 #    %>% dplyr::select(in_death_table,r_rural, i_rural, three_category_ruca, four_category_ruca, gender,age,Race,Ethnicity,BMI_Group, CCI_INDEX, quarter_of_diagnosis)

   # Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, i_rural == 1)
local_df <- filter(local_df, r_rural > .01)
#local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
#local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))



    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San
    #md2 <- glm( in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease +factor(data_partner_id), family=binomial, data = local_df)

    print(min(local_df$r_rural))
    print(max(local_df$r_rural))

    md2 <- glm(in_death_table ~ r_rural + factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis)  , data = local_df, family = binomial, maxit = 20)
    print(summary(md2))

    local_df$pred2 <- predict(md2,type=c('response'))
    local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md2 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)

    return(local_df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.df2aa63e-7daf-4da5-8d75-a8c0902e97e8"),
    s2_dt_r_rural_adj=Input(rid="ri.foundry.main.dataset.2614cacf-c819-441a-99dd-d1f0861fcb10")
)
s2_dt_r_rural_set3 <- function(s2_md1, s2_dt_r_rural_adj) {

local_df <- s2_dt_r_rural_adj

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
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

#find_idx <- function(val)df$index[match(index[val],df$value)]

    tb2 <-NA
    #"Ethnicity","i_rural","BMI_Group","age","CCI_INDEX","quarter_of_diagnosis","gender","Race"
    stdlist <- list("r_rural","Race","Ethnicity","age","CCI_INDEX","quarter_of_diagnosis","gender")
    stdlist_cont <- list("age","CCI_INDEX")



    for (x in  stdlist_cont){
        x_index <- which(stdlist==x) [1]
        print(paste('x index',x_index))
        local_df$x <- local_df[,x]

        md_prep <- glm(in_death_table ~ x, family=binomial, data = local_df)
        local_df$predprep <- predict(md_prep,type=c('response'))
        max_predprep <- max(local_df$predprep)
        min_predprep <- min(local_df$predprep)
        step_prepprep <- (max_predprep - min_predprep)/5

        local_df <- local_df %>% mutate(cont2cat = case_when(local_df$predprep <step_prepprep ~ 2,step_prepprep<=local_df$predprep & local_df$predprep <2*step_prepprep ~ 4,2*step_prepprep<=local_df$predprep & local_df$predprep <3*step_prepprep ~ 6,3*step_prepprep<=local_df$predprep & local_df$predprep<4*step_prepprep ~ 8, local_df$predprep >=2*step_prepprep ~ 10))

        x_factor <- paste(x,'_factor', sep = "")
        #local_df$x_factor <- as.factor(local_df$cont2cat)
        local_df[,x_factor] <- local_df$cont2cat

        print(paste("------------Continuous to category variable x=",x,"----------------"))

        a <- glm(rs2_self ~ r_rural + factor(cont2cat), data = local_df)
        print(summary(a))

        tb1 <- a
        tb1 <- a %>% tbl_regression(exponentiate = FALSE)
        tb1 <- as_tibble(tb1, col_labels = FALSE)
        tb1$x <- x
        tb1$md <- paste('r_rural + factor(', x,'_cont2cat)')
        #print(tb1)
        tb2 <- rbind(tb2,tb1)
    }
    print(tb2)
    #tb2_signigicant <- filter(tb2, p_value<.05)

    return(local_df)
    #return(NULL)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.4f903875-0c20-422b-8a2a-37d183464d5d"),
    s2_dt_r_rural_adj=Input(rid="ri.foundry.main.dataset.2614cacf-c819-441a-99dd-d1f0861fcb10")
)
#Urban
s2_dt_r_rural_urban <- function(s2_dt_r_rural_adj) {
   local_df <- s2_dt_r_rural_adj
 #  %>% dplyr::select(in_death_table,r_rural, i_rural, three_category_ruca, four_category_ruca, gender,age,Race,Ethnicity,BMI_Group, CCI_INDEX, quarter_of_diagnosis)

   # Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, i_rural != 1)
local_df <- filter(local_df, r_rural > .01)
#local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

   # %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San
    #md2 <- glm( in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease +factor(data_partner_id), family=binomial, data = local_df)

    print(min(local_df$r_rural))
    print(max(local_df$r_rural))

    md2 <- glm(in_death_table ~ r_rural + factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis)  , data = local_df, family = binomial, maxit = 100)
    print(summary(md2))

    local_df$pred2 <- predict(md2,type=c('response'))
    local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md2 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)

    return(local_df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.86d6abde-1b44-41c7-b037-cc607e95261e"),
    s2_dt_r_rural_adj=Input(rid="ri.foundry.main.dataset.2614cacf-c819-441a-99dd-d1f0861fcb10")
)
#Urban
s2_dt_r_rural_urban_2 <- function(s2_dt_r_rural_adj) {
   local_df <- s2_dt_r_rural_adj
 #  %>% dplyr::select(in_death_table,r_rural, i_rural, three_category_ruca, four_category_ruca, gender,age,Race,Ethnicity,BMI_Group, CCI_INDEX, quarter_of_diagnosis)
print(dim(local_df))
   # Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, i_rural != 1)
local_df <- filter(local_df, r_rural > .01)
#local_df <- filter(local_df, BMI_Group != "Unknown/Missing")
print(dim(local_df))

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
#local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

   # %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San
    #md2 <- glm( in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease +factor(data_partner_id), family=binomial, data = local_df)

    print(min(local_df$r_rural))
    print(max(local_df$r_rural))

    md2 <- glm(in_death_table ~ r_rural + factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis)  , data = local_df, family = binomial, maxit = 100)
    print(summary(md2))

    local_df$pred2 <- predict(md2,type=c('response'))
    local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md2 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)

    return(local_df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.a4753810-13c7-44b7-8961-30b33fa961ba"),
    s2_dt_i_r_rural=Input(rid="ri.vector.main.execute.1bd718a3-e1a2-4218-b122-8e9518e20131")
)
s2_md_i_r_rural_set1 <- function(s2_md1, s2_dt_i_r_rural) {

local_df <- s2_dt_i_r_rural

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
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

#find_idx <- function(val)df$index[match(index[val],df$value)]

    tb2 <-NA
    #"Ethnicity","i_rural","BMI_Group","age","CCI_INDEX","quarter_of_diagnosis","gender","Race"
    stdlist <- list("r_rural","Race","Ethnicity","age","CCI_INDEX","quarter_of_diagnosis","gender")
    stdlist_cat <- list("gender","Race","Ethnicity","BMI_Group","quarter_of_diagnosis")

    for (x in  stdlist){
        x_index <- which(stdlist==x) [1]
        print(paste('x index',x_index))
        for (y in  stdlist){
            y_index <- which(stdlist==y) [1]
            print(paste('y index',y_index))
            if (x_index < y_index) {
                print(paste("------------x*y=",x,"*",y,"----------------"))
                local_df$x <- local_df[,x]
                local_df$y <- local_df[,y]
                if (x %in% stdlist_cat) {
                    local_df$x <- as.factor(local_df[,x])
                }
                if (y %in% stdlist_cat) {
                    local_df$y <- as.factor(local_df[,y])
                }
                a <- glm(rs2_self ~ r_rural + x*y, data = local_df)
                print(summary(a))

                tb1 <- a
                tb1 <- a %>% tbl_regression(exponentiate = FALSE)
                tb1 <- as_tibble(tb1, col_labels = FALSE)
                tb1$x <- x
                tb1$y <- y
                tb1$md <- paste('r_rural + ', x,'*',y)
                #print(tb1)
                tb2 <- rbind(tb2,tb1)
            }
        }
        return (tb2)
    }

    tb2_signigicant <- filter(tb2, p_value<.05)
    print(tb2_signigicant)
    return(tb2)
    #return(NULL)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.832e76e9-af37-403c-8cd0-7a5f3c5c75b8"),
    s2_dt_i_rural_adj_0=Input(rid="ri.foundry.main.dataset.566964b0-8add-4f1e-9037-d861158e54be")
)
s2_md_i_rural_0_set1 <- function(s2_md1, s2_dt_i_rural_adj_0) {

local_df <- s2_dt_i_rural_adj_0

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
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

#find_idx <- function(val)df$index[match(index[val],df$value)]
st4sw <- 'i_rural'
    tb2 <-NA
    #"Ethnicity","i_rural","BMI_Group","age","CCI_INDEX","quarter_of_diagnosis","gender","Race"
    stdlist <- list("gender","Race","Ethnicity","age","CCI_INDEX","BMI_Group","quarter_of_diagnosis")
    stdlist_cat <- list("gender","Race","Ethnicity","BMI_Group","quarter_of_diagnosis")

    for (x in  stdlist){
        x_index <- which(stdlist==x) [1]
        print(paste('x index',x_index))
        for (y in  stdlist){
            y_index <- which(stdlist==y) [1]
            print(paste('y index',y_index))
            if (x_index < y_index) {
                print(paste("------------x*y=",x,"*",y,"----------------"))

                if (x %in% stdlist_cat) {
                    local_df$x <- as.factor(local_df[,x])
                    st_x <- paste('+ factor(',x,') ')
                }
                else {
                    local_df$x <- local_df[,x]
                    st_x <- paste('+ x ')
                }
                if (y %in% stdlist_cat) {
                    local_df$y <- as.factor(local_df[,y])
                    st_y <- paste('*factor(',y,') ')
                }
                else {
                    local_df$y <- local_df[,y]
                    st_y <- paste('*',y)
                }
                st4sw <- paste(st4sw, st_x , st_y)
                a <- glm(rs2_self ~ i_rural + x*y, data = local_df)
                print(summary(a))

                tb1 <- a
                tb1 <- a %>% tbl_regression(exponentiate = FALSE)
                tb1 <- as_tibble(tb1, col_labels = FALSE)
                tb1$x <- x
                tb1$y <- y
                tb1$md <- paste('i_rural + ', x,'*',y)
                #print(tb1)
                tb2 <- rbind(tb2,tb1)
            }
        }
        print(st4sw)
        return (tb2)
    }

    tb2_signigicant <- filter(tb2, p_value<.05)

    print(tb2)
    return(tb2_signigicant)
    #return(NULL)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.61332d9f-c3cf-4625-9d93-1be0923325ff"),
    s2_dt_i_rural_adj_0=Input(rid="ri.foundry.main.dataset.566964b0-8add-4f1e-9037-d861158e54be")
)
s2_md_i_rural_0_set1_sw <- function(s2_md1, s2_dt_i_rural_adj_0) {

local_df <- s2_dt_i_rural_adj_0
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
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

    a <- glm(rs2_self ~ i_rural+ i_rural*factor(gender) + i_rural*factor(Race) + i_rural*factor(Ethnicity) + i_rural*factor(BMI_Group) + i_rural*age + i_rural*CCI_INDEX  + i_rural*factor(quarter_of_diagnosis) , data = local_df,maxit = 100)

    print(summary(a))
    tb1 <- a
    tb1 <- a %>% tbl_regression(exponentiate = FALSE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)

    return(tb1)
    #return(NULL)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.c051eb4b-f3c5-455b-9ffc-f9d9f6c04d2c"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
#demos adjust
#All BMI cats kept, combined
#md1: glm(in_death_table ~ r_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 100)
s2_md_i_rural_adj <- function(unnamed_17) {

   local_df <- unnamed_17
      #  %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    # Filter out missing variables
    local_df <- filter(local_df, age_Group != "Unknown/Missing")
    local_df <- filter(local_df, gender != "Other")
    local_df <- filter(local_df, gender != "Unknown")
    local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
    #local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

    # Set reference categories
    local_df$Race=relevel(as.factor(local_df$Race),ref="White")
    local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
    local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
    local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
    local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
    local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))


    md1 <- glm(in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX  + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 20)
    print(summary(md1))

    #local_df$pred2 <- predict(md1,type=c('response'))
    #local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md1 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)

    return(tb1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.65e1e4f5-d12a-4412-84ed-ddc4dd8ce852"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
#demos adjust
#All BMI cats kept, combined
#md1: glm(in_death_table ~ r_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 100)
s2_md_i_rural_dp_fe <- function(unnamed_17) {

   local_df <- unnamed_17
      #  %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    # Filter out missing variables
    local_df <- filter(local_df, age_Group != "Unknown/Missing")
    local_df <- filter(local_df, gender != "Other")
    local_df <- filter(local_df, gender != "Unknown")
    local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
    #local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

    # Set reference categories
    local_df$Race=relevel(as.factor(local_df$Race),ref="White")
    local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
    local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
    local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
    local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
    local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San
    #md2 <- glm( in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease +factor(data_partner_id), family=binomial, data = local_df)


    md1 <- glm(in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX  + factor(quarter_of_diagnosis) + factor(data_partner_id) , data = local_df, family = binomial, maxit = 20)
    print(summary(md1))

    #local_df$pred2 <- predict(md1,type=c('response'))
    #local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md1 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)

    return(tb1)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.9afdb1ab-cb11-4519-b010-59399eac97c1"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
s2_md_i_rural_dp_psc <- function(unnamed_17) {
   local_df <- unnamed_17

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
#local_df$three_category_ruca=relevel(as.factor(local_df$quarter_of_diagnosis),ref="urban")
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))
local_df$i_rural_orig <- local_df$i_rural
#ocal_df$i_rural <- strtoi(local_df$i_rural_orig,base =0L)

print(dim(local_df))
   # %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San

    md2 <- glm( i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX  + factor(quarter_of_diagnosis) , family=binomial, data = local_df)

    local_df$pred2 <- predict(md2,type=c('response'))
    #local_df$pred2cat <- ifelse(local_df$pred2 >=.2, 1, 0)
    g <- qplot(local_df$pred2, geom="histogram")
    plot(g)


    local_df <- local_df %>% mutate(pred2cat = case_when(local_df$pred2 <.055 ~ 0.2,0.055<=local_df$pred2 & local_df$pred2 <.110 ~ .4,.110<=local_df$pred2 & local_df$pred2 <.165 ~ .6,.165<=local_df$pred2 & local_df$pred2<.220 ~ .8, local_df$pred2 >=.220 ~ 1))

    #local_df %>% mutate(pred2cat = case_when(local_df$pred2  >= .2 ~ 1,local_df$pred2 <.2 ~ 0))
    print("matching")
    mod_match <- matchit(i_rural ~ pred2cat +factor(data_partner_id), method = "exact", data = local_df)
    dta_m <- match.data(mod_match,drop.unmatched=FALSE)
    #dta_m <- get_matches(mod_match)

    print("PSC")
    md2 <- glm(in_death_table ~ i_rural, data = dta_m,family = binomial,weights= weights, maxit = 20)
    print(summary(md2))

    dta_m$pred2 <- predict(md2,type=c('response'))
    dta_m$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md2 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)


    return(tb1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.0b179ae9-db1f-49a9-a3e7-f3178013099b"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
s2_md_i_rural_dp_psc_drop_ukBMIcat <- function(unnamed_17) {
   local_df <- unnamed_17

   # Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, BMI_Group != "Unknown/Missing")
dim(local_df)
# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
#local_df$three_category_ruca=relevel(as.factor(local_df$quarter_of_diagnosis),ref="urban")
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))
local_df$i_rural_orig <- local_df$i_rural
#ocal_df$i_rural <- strtoi(local_df$i_rural_orig,base =0L)

print(dim(local_df))
   # %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San

    md2 <- glm( i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX  + factor(quarter_of_diagnosis) , family=binomial, data = local_df)

    local_df$pred2 <- predict(md2,type=c('response'))
    #local_df$pred2cat <- ifelse(local_df$pred2 >=.2, 1, 0)
    g <- qplot(local_df$pred2, geom="histogram")
    plot(g)


    local_df <- local_df %>% mutate(pred2cat = case_when(local_df$pred2 <.055 ~ 0.2,0.055<=local_df$pred2 & local_df$pred2 <.110 ~ .4,.110<=local_df$pred2 & local_df$pred2 <.165 ~ .6,.165<=local_df$pred2 & local_df$pred2<.220 ~ .8, local_df$pred2 >=.220 ~ 1))

    #local_df %>% mutate(pred2cat = case_when(local_df$pred2  >= .2 ~ 1,local_df$pred2 <.2 ~ 0))
    print("matching")
    mod_match <- matchit(i_rural ~ pred2cat +factor(data_partner_id), method = "exact", data = local_df)
    dta_m <- match.data(mod_match,drop.unmatched=FALSE)
    #dta_m <- get_matches(mod_match)

    print("PSC")
    md2 <- glm(in_death_table ~ i_rural, data = dta_m,family = binomial,weights= weights, maxit = 20)
    print(summary(md2))

    dta_m$pred2 <- predict(md2,type=c('response'))
    dta_m$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md2 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)


    return(tb1)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.6cde0274-50e5-4d33-8c6d-b14ea5a0c5ef"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
s2_md_i_rural_dp_psc_drop_ukBMIcat_1 <- function(unnamed_17) {
   local_df <- unnamed_17

   # Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, BMI_Group != "Unknown/Missing")
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, Race != "Missing/Unknown")
local_df <- filter(local_df, Ethnicity != "Missing/Unknown")

dim(local_df)
# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
#local_df$three_category_ruca=relevel(as.factor(local_df$quarter_of_diagnosis),ref="urban")
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))
local_df$i_rural_orig <- local_df$i_rural
#ocal_df$i_rural <- strtoi(local_df$i_rural_orig,base =0L)

print(dim(local_df))
   # %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San

    md2 <- glm( i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX  + factor(quarter_of_diagnosis) , family=binomial, data = local_df)

    local_df$pred2 <- predict(md2,type=c('response'))
    #local_df$pred2cat <- ifelse(local_df$pred2 >=.2, 1, 0)
    g <- qplot(local_df$pred2, geom="histogram")
    plot(g)


    local_df <- local_df %>% mutate(pred2cat = case_when(local_df$pred2 <.055 ~ 0.2,0.055<=local_df$pred2 & local_df$pred2 <.110 ~ .4,.110<=local_df$pred2 & local_df$pred2 <.165 ~ .6,.165<=local_df$pred2 & local_df$pred2<.220 ~ .8, local_df$pred2 >=.220 ~ 1))

    #local_df %>% mutate(pred2cat = case_when(local_df$pred2  >= .2 ~ 1,local_df$pred2 <.2 ~ 0))
    print("matching")
    mod_match <- matchit(i_rural ~ pred2cat +factor(data_partner_id), method = "exact", data = local_df)
    dta_m <- match.data(mod_match,drop.unmatched=FALSE)
    #dta_m <- get_matches(mod_match)

    print("PSC")
    md2 <- glm(in_death_table ~ i_rural, data = dta_m,family = binomial,weights= weights, maxit = 20)
    print(summary(md2))

    dta_m$pred2 <- predict(md2,type=c('response'))
    dta_m$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md2 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)


    return(tb1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.5db3b58e-9314-4cb0-9473-ecc2693b497f"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
library(mice)
#library(VIM)

s2_md_i_rural_dp_psc_imputeBMI <- function(unnamed_17) {

local_df <- unnamed_17 %>%
    dplyr::select(in_death_table,four_category_ruca,three_category_ruca,i_rural,gender,age_Group,Race,Ethnicity,BMI_Group,quarter_of_diagnosis,CCI_INDEX,data_partner_id,Severity_Type,ECMO,MACE,Mechnical_Ventilation,ip_rate,r_rural)
    #drop ttdeath, los, BMI, age, Q_Score, Q_Score_Categories, CCI_Categories, all diseases

#gp2: replace cat (gender, BMI_Group, age_Group, quarter_of_diagnosis, Race, Ethnicity) Unknown/missing back to missing, impute
local_df$gender[local_df$gender == "Unknown"] <- NA
local_df$BMI_Group[local_df$BMI_Group == "Unknown/Missing"] <- NA
local_df$age_Group[local_df$age_Group == "Unknown/Missing"] <- NA
local_df$quarter_of_diagnosis[local_df$quarter_of_diagnosis == "Unknown/Missing"] <- NA

local_df$Race[local_df$Race == "Missing/Unknown"] <- NA
local_df$Ethnicity[local_df$Ethnicity == "Missing/Unknown"] <- NA

local_df$Race <- as.factor(local_df$Race)
local_df$gender <- as.factor(local_df$gender)
local_df$Ethnicity <- as.factor(local_df$Ethnicity)
local_df$age_Group <- as.factor(local_df$age_Group)
local_df$BMI_Group <- as.factor(local_df$BMI_Group)
local_df$quarter_of_diagnosis <- as.factor(local_df$quarter_of_diagnosis)

print(md.pattern(local_df))

tempData <- mice(local_df,m=10,maxit=20,seed=500)
summary(tempData)
local_df <- complete(tempData,1)
#local_df <- filter(local_df, age_Group == NA)

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
#local_df$three_category_ruca=relevel(as.factor(local_df$quarter_of_diagnosis),ref="urban")
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))
local_df$i_rural_orig <- local_df$i_rural
#ocal_df$i_rural <- strtoi(local_df$i_rural_orig,base =0L)

print(dim(local_df))

    md2 <- glm( i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX  + factor(quarter_of_diagnosis) , family=binomial, data = local_df)

    local_df$pred2 <- predict(md2,type=c('response'))
    #local_df$pred2cat <- ifelse(local_df$pred2 >=.2, 1, 0)
    g <- qplot(local_df$pred2, geom="histogram")
    plot(g)


    local_df <- local_df %>% mutate(pred2cat = case_when(local_df$pred2 <.055 ~ 0.2,0.055<=local_df$pred2 & local_df$pred2 <.110 ~ .4,.110<=local_df$pred2 & local_df$pred2 <.165 ~ .6,.165<=local_df$pred2 & local_df$pred2<.220 ~ .8, local_df$pred2 >=.220 ~ 1))

    #local_df %>% mutate(pred2cat = case_when(local_df$pred2  >= .2 ~ 1,local_df$pred2 <.2 ~ 0))
    print("matching")
    mod_match <- matchit(i_rural ~ pred2cat +factor(data_partner_id), method = "exact", data = local_df)
    dta_m <- match.data(mod_match,drop.unmatched=FALSE)
    #dta_m <- get_matches(mod_match)

    print("PSC")
    md2 <- glm(in_death_table ~ i_rural, data = dta_m,family = binomial,weights= weights, maxit = 20)
    print(summary(md2))

    dta_m$pred2 <- predict(md2,type=c('response'))
    dta_m$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md2 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)


    return(tb1)

}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.6607434d-0510-4559-acee-d4034250793f"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
#demos adjust
#All BMI cats kept, combined
#md1: glm(in_death_table ~ r_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 100)
s2_md_i_rural_dp_re <- function(unnamed_17) {

   local_df <- unnamed_17
      #  %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    # Filter out missing variables
    local_df <- filter(local_df, age_Group != "Unknown/Missing")
    local_df <- filter(local_df, gender != "Other")
    local_df <- filter(local_df, gender != "Unknown")
    local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
    #local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

    # Set reference categories
    local_df$Race=relevel(as.factor(local_df$Race),ref="White")
    local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
    local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
    local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
    local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
    local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))
    #local_df$data_partner_id <- as.factor(local_df$data_partner_id)

    md1 <- glm(in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX  + factor(quarter_of_diagnosis) + (1|data_partner_id), data = local_df, family = binomial, maxit = 20)
    print(summary(md1))

    #local_df$pred2 <- predict(md1,type=c('response'))
    #local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md1 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)

    return(tb1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.d30878f7-dc49-479c-ba4a-0a932357858d"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
library(lme4)
#library(magrittr)
#> Loading required package: Matrix

#demos adjust
#All BMI cats kept, combined
#md1: glm(in_death_table ~ r_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 100)
s2_md_i_rural_dp_re_lmer <- function(unnamed_17) {

   local_df <- unnamed_17
    #  %>% dplyr::select(in_death_table,i_rural,gender,age_Group,Race,Ethnicity,BMI_Group,Q_cat,CCI_INDEX,quarter_dx,data_partner_id)

    # Filter out missing variables
    local_df <- filter(local_df, age_Group != "Unknown/Missing")
    local_df <- filter(local_df, gender != "Other")
    local_df <- filter(local_df, gender != "Unknown")
    local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
    #local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

    # Set reference categories
    local_df$Race=relevel(as.factor(local_df$Race),ref="White")
    local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
    local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
    local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
    local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
    local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))
    #local_df$data_partner_id <- as.factor(local_df$data_partner_id)
    #m2 <- ds %>%
    #glmer(
    #formula = y ~ 1 + Days + rural + (1 | Subject),
    #family  = "binomial",
    #data    = .  )
    md1 <- local_df %>% glmer(formula = in_death_table ~ 1 + i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX  + factor(quarter_of_diagnosis) + (1|data_partner_id), data = ., family = "binomial")
    print(summary(md1))

    #local_df$pred2 <- predict(md1,type=c('response'))
    #local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md1 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)

    return(tb1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.fb64454f-65d9-4ad3-80ba-571fe687a292"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
#demos adjust
#All BMI cats kept, combined
#md1: glm(in_death_table ~ r_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 100)
s2_md_i_rural_dp_re_slopes <- function(unnamed_17) {

   local_df <- unnamed_17
      #  %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    # Filter out missing variables
    local_df <- filter(local_df, age_Group != "Unknown/Missing")
    local_df <- filter(local_df, gender != "Other")
    local_df <- filter(local_df, gender != "Unknown")
    local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
    #local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

    # Set reference categories
    local_df$Race=relevel(as.factor(local_df$Race),ref="White")
    local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
    local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
    local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
    local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
    local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))
    #local_df$data_partner_id <- as.factor(local_df$data_partner_id)

    md1 <- glm(in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX  + factor(quarter_of_diagnosis) + (1|data_partner_id) + (0 + r_rural|data_partner_id), data = local_df, family = binomial, maxit = 20)
    print(summary(md1))

    #local_df$pred2 <- predict(md1,type=c('response'))
    #local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md1 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)

    return(tb1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.44ac363d-91de-43f3-be0f-fb1e8c59d876"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
#demos adjust
#All BMI cats kept, combined
#md1: glm(in_death_table ~ r_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 100)
s2_md_i_rural_only <- function(unnamed_17) {

   local_df <- unnamed_17
      #  %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    # Filter out missing variables
    local_df <- filter(local_df, age_Group != "Unknown/Missing")
    local_df <- filter(local_df, gender != "Other")
    local_df <- filter(local_df, gender != "Unknown")
    local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
    #local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

    # Set reference categories
    local_df$Race=relevel(as.factor(local_df$Race),ref="White")
    local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
    local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
    local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
    local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
    local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San
    #md2 <- glm( in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease +factor(data_partner_id), family=binomial, data = local_df)


    md1 <- glm(in_death_table ~ i_rural, data = local_df, family = binomial, maxit = 100)
    print(summary(md1))

    local_df$pred2 <- predict(md1,type=c('response'))
    local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md1 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)

    return(tb1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.205b617b-0aa0-489c-adf2-7274f8032969"),
    s2_dt_i_rural_adj=Input(rid="ri.foundry.main.dataset.4ab41c9d-bbab-4ad5-af15-6c2b878e7389")
)
s2_md_i_rural_set1 <- function(s2_dt_i_rural_adj) {

local_df <- s2_dt_i_rural_adj

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
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

#find_idx <- function(val)df$index[match(index[val],df$value)]
st4sw <- 'i_rural'
    tb2 <-NA
    #"Ethnicity","i_rural","BMI_Group","age","CCI_INDEX","quarter_of_diagnosis","gender","Race"
    stdlist <- list("gender","Race","Ethnicity","age_Group","CCI_INDEX","BMI_Group","quarter_of_diagnosis")
    stdlist_cat <- list("gender","Race","Ethnicity","age_Group","BMI_Group","quarter_of_diagnosis")

    for (x in  stdlist){
        x_index <- which(stdlist==x) [1]
        print(paste('x index',x_index))
        for (y in  stdlist){
            y_index <- which(stdlist==y) [1]
            print(paste('y index',y_index))
            if (x_index < y_index) {
                print(paste("------------x*y=",x,"*",y,"----------------"))

                if (x %in% stdlist_cat) {
                    local_df$x <- as.factor(local_df[,x])
                    st_x <- paste('+ factor(',x,') ')
                }
                else {
                    local_df$x <- local_df[,x]
                    st_x <- paste('+ x ')
                }
                if (y %in% stdlist_cat) {
                    local_df$y <- as.factor(local_df[,y])
                    st_y <- paste('*factor(',y,') ')
                }
                else {
                    local_df$y <- local_df[,y]
                    st_y <- paste('*',y)
                }
                st4sw <- paste(st4sw, st_x , st_y)
                a <- glm(rs2_self ~ i_rural + x*y, data = local_df)
                print(summary(a))

                tb1 <- a
                tb1 <- a %>% tbl_regression(exponentiate = FALSE)
                tb1 <- as_tibble(tb1, col_labels = FALSE)
                tb1$x <- x
                tb1$y <- y
                tb1$md <- paste('i_rural + ', x,'*',y)
                #print(tb1)
                tb2 <- rbind(tb2,tb1)
            }#if
        }#for_x
        print(st4sw)

    }

    #tb2_signigicant <- filter(tb2, p_value<.05)

    print(tb2)
    return(tb2)
    #return(tb2_signigicant)
    #return(NULL)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.cbad3e16-51da-4366-9d5c-351af9beaada"),
    s2_dt_i_rural_adj=Input(rid="ri.foundry.main.dataset.4ab41c9d-bbab-4ad5-af15-6c2b878e7389")
)
s2_md_i_rural_set1_intact_i_rural <- function(s2_dt_i_rural_adj) {
s2_md_i_rural_set1_intact_i_rural <- function(s2_dt_i_rural_adj) {

local_df <- s2_dt_i_rural_adj

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
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

#find_idx <- function(val)df$index[match(index[val],df$value)]
st4sw <- 'i_rural'
    tb2 <-NA
    #"Ethnicity","i_rural","BMI_Group","age","CCI_INDEX","quarter_of_diagnosis","gender","Race"
    xstdlist <- list("i_rural")
    stdlist <- list("gender","Race","Ethnicity","age_Group","CCI_INDEX","BMI_Group","quarter_of_diagnosis")
    stdlist_cat <- list("gender","Race","Ethnicity","age_Group","BMI_Group","quarter_of_diagnosis")

    local_df_input <- local_df
    #local_df <-head(local_df,100)
    for (x in  xstdlist){
        #x_index <- which(stdlist==x) [1]
        #print(paste('x index',x_index))
        for (y in  stdlist){
            y_index <- which(stdlist==y) [1]
            print(paste('y index',y_index))
            #if (x_index < y_index) {
                print(paste("------------i_rural*y = i_rural*",y,"----------------"))
                if (y %in% stdlist_cat) {
                    local_df$y <- as.factor(local_df[,y])
                    st_y <- paste('*factor(',y,') ')
                }
                else {
                    local_df$y <- local_df[,y]
                    st_y <- paste('*',y)
                }
                #st4sw <- paste(st4sw, st_x , st_y)
                a <- glm(rs2_self ~  i_rural*y, data = local_df)
                print(summary(a))

                tb1 <- a
                tb1 <- a %>% tbl_regression(exponentiate = FALSE)
                tb1 <- as_tibble(tb1, col_labels = FALSE)

                tb1$y <- y
                tb1$md <- paste('i_rural *',y)
                #print(tb1)
                tb2 <- rbind(tb2,tb1)
            #}#if
        }#for_y
        #print(st4sw)
    }#for_x

    #tb2_signigicant <- filter(tb2, p_value<.05)

    print(tb2)
    return(tb2)
    #return(tb2_signigicant)
    #return(NULL)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.1d266e0d-463d-4cf8-a636-8598fc46745c"),
    s2_dt_i_rural_dp_fe=Input(rid="ri.foundry.main.dataset.774ebfc9-21ab-43c0-b3f3-c1a9f6a2a466")
)
s2_md_i_rural_set1_intact_ratio <- function(s2_dt_i_rural_dp_fe) {

    fn_diff_odd <- function(local_df, md, st_md){
        #print(dim(local_df))
        print(summary(md))
        local_df$yhat <- predict(md,type=c('response'))
        local_df$rs <- local_df$in_death_table - local_df$yhat
        #print(mean(local_df$i_rural))
        #print(mean(local_df$yhat))
        #keep rural only and then pretend it is urban
        local_df$i_rural <- 0
        local_df$yhat_0 <- predict(md,type=c('response'))
        local_df$rs_0 <- local_df$in_death_table - local_df$yhat_0
        #print(mean(local_df$i_rural))
        #print(mean(local_df$yhat_0))

        local_df <- filter(local_df,i_rural_org==1)
    print(dim(local_df))
        local_df$diff <- local_df$yhat - local_df$yhat_0
        #print(mean(local_df$diff))
        m_diff <- mean(local_df$diff)
        m_yhat <- mean(local_df$yhat)
        m_yhat_0 <- mean(local_df$yhat_0)

        tb <- data.frame(md_name=c(st_md),my_diff=c(1.0*m_diff), my_ratio = c(1.0*(1.0*m_yhat/(1.0-m_yhat))/(1.0*m_yhat_0/(1.0-m_yhat_0))))
        print(tb)
        return(tb)
    }

    local_df <- s2_dt_i_rural_dp_fe

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
    #local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))
    local_df$i_rural_org <- local_df$i_rural
    local_df_org <- local_df

    print(dim(local_df_org))
    ####calc fe

    md <- glm(in_death_table ~ i_rural + factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX  + factor(quarter_of_diagnosis) + factor(data_partner_id), data = local_df, family = binomial, maxit = 20)
    tb2 <- fn_diff_odd(local_df, md, "fe")
#======================================================
    local_df <- local_df_org
    mdfe_int3 <- glm(in_death_table ~ i_rural + factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX  + factor(quarter_of_diagnosis) + factor(data_partner_id) + i_rural*factor(quarter_of_diagnosis) + i_rural*factor(BMI_Group)+ i_rural*CCI_INDEX, data = local_df, family = binomial, maxit = 20)
    tb1 <- fn_diff_odd(local_df, mdfe_int3, "fe_int_3")
    tb2 <- rbind(tb2,tb1)
    print(tb2)
 #======================================================
#"Ethnicity","i_rural","BMI_Group","age","CCI_INDEX","quarter_of_diagnosis","gender","Race"
    xstdlist <- list("i_rural")
    stdlist <- list("gender","Race","Ethnicity","age_Group","CCI_INDEX","BMI_Group","quarter_of_diagnosis")
    stdlist_cat <- list("gender","Race","Ethnicity","age_Group","BMI_Group","quarter_of_diagnosis")
    for (x in  xstdlist){
        #x_index <- which(stdlist==x) [1]
        #print(paste('x index',x_index))
        for (y in  stdlist){
            local_df <- local_df_org
            y_index <- which(stdlist==y) [1]
            print(paste('y index',y_index))
            #if (x_index < y_index) {
                print(paste("------------i_rural*y = i_rural*",y,"----------------"))
                if (y %in% stdlist_cat) {
                    local_df$y <- as.factor(local_df[,y])
                    st_y <- paste('*factor(',y,') ')
                }
                else {
                    local_df$y <- local_df[,y]
                    st_y <- paste('*',y)
                }
                #st4sw <- paste(st4sw, st_x , st_y)
                mdfe_int <- glm(in_death_table ~ i_rural + factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX  + factor(quarter_of_diagnosis) + factor(data_partner_id) + i_rural*y, data = local_df, family = binomial, maxit = 20)
                tb1 <- fn_diff_odd(local_df, mdfe_int, paste("fe_i_rural*",y,sep=""))
                tb2 <- rbind(tb2,tb1)
                print(tb2)
            #}#if
        }#for_y
        #print(st4sw)
    }#for_x

    return(tb2)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ac816572-8358-4010-833f-3569b4ca21da"),
    s2_dt_i_rural_adj=Input(rid="ri.foundry.main.dataset.4ab41c9d-bbab-4ad5-af15-6c2b878e7389")
)
s2_md_i_rural_set1_sw_1 <- function( s2_dt_i_rural_adj) {

local_df <- s2_dt_i_rural_adj
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
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

    a <- glm(rs2_self ~ i_rural+ i_rural*factor(gender) + i_rural*factor(Race) + i_rural*factor(Ethnicity) + i_rural*factor(BMI_Group) + i_rural*factor(age_Group) + i_rural*CCI_INDEX  + i_rural*factor(quarter_of_diagnosis) , data = local_df,maxit = 20)

    print(summary(a))
    tb1 <- a
    tb1 <- a %>% tbl_regression(exponentiate = FALSE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)

    return(tb1)
    #return(NULL)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.a4e83696-0314-41d6-afcf-2c88d182a422"),
    s2_dt_i_rural_adj=Input(rid="ri.foundry.main.dataset.4ab41c9d-bbab-4ad5-af15-6c2b878e7389")
)
s2_md_i_rural_set2 <- function(s2_dt_i_rural_adj) {

local_df <- s2_dt_i_rural_adj

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
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

#find_idx <- function(val)df$index[match(index[val],df$value)]

    tb2 <-NA
    #"Ethnicity","i_rural","BMI_Group","age","CCI_INDEX","quarter_of_diagnosis","gender","Race"

    stdlist <- list("data_partner_id","four_category_ruca","three_category_ruca","i_rural","age","CCI_Categories","BMI","Q_Score_Categories","Q_Score","Diabetes","MI","CHF","PVD","stroke","dementia","pulmonary","rheumatic","Liver","paralysis","renal","cancer","mets","hiv","ttdeath","Severity_Type","ECMO","MACE","Mechnical_Ventilation","length_of_stay","ip_rate")

    stdlist_cat <- list("data_partner_id","four_category_ruca","three_category_ruca","i_rural","CCI_Categories","Q_Score_Categories","Q_Score","Diabetes","MI","CHF","PVD","stroke","dementia","pulmonary","rheumatic","Liver","paralysis","renal","cancer","mets","hiv","ttdeath","Severity_Type","ECMO","MACE","Mechnical_Ventilation")
    for (x in  stdlist){
        #x_index <- which(stdlist==x)[1]
        #print(paste('x index',x_index))
        print(paste("------------ Var NOT in model x=",x,"----------------"))
                local_df$x <- local_df[,x]
                if (x %in% stdlist_cat) {
                    local_df$x <- as.factor(local_df[,x])
                }
                a <- glm(rs2_self ~ i_rural + x, data = local_df)
                print(summary(a))

                tb1 <- a
                tb1 <- a %>% tbl_regression(exponentiate = FALSE)
                tb1 <- as_tibble(tb1, col_labels = FALSE)
                tb1$x <- x
                tb1$md <- paste('i_rural + ', x)
                #print(tb1)
                tb2 <- rbind(tb2,tb1)
    }
    print(tb2)
    #tb2_signigicant <- filter(tb2, p_value<.05)

    #return(tb2_signigicant)
    return(tb2)
    #return(NULL)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.44a603df-9e35-4a80-a537-eb898c7916e7"),
    s2_dt_i_rural_adj=Input(rid="ri.foundry.main.dataset.4ab41c9d-bbab-4ad5-af15-6c2b878e7389")
)
s2_md_i_rural_set2_1 <- function(s2_dt_i_rural_adj) {

local_df <- s2_dt_i_rural_adj

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
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

#find_idx <- function(val)df$index[match(index[val],df$value)]

    tb2 <-NA
    #"Ethnicity","i_rural","BMI_Group","age","CCI_INDEX","quarter_of_diagnosis","gender","Race"

    stdlist <- list("r_rural","data_partner_id","four_category_ruca","three_category_ruca","i_rural","r_rural", "age","CCI_Categories","BMI","Q_Score_Categories","Q_Score","Diabetes","MI","CHF","PVD","stroke","dementia","pulmonary","rheumatic","Liver","paralysis","renal","cancer","mets","hiv","ttdeath","Severity_Type","ECMO","MACE","Mechnical_Ventilation","length_of_stay","ip_rate")

    stdlist_cat <- list("data_partner_id","four_category_ruca","three_category_ruca","i_rural","CCI_Categories","Q_Score_Categories","Q_Score","Diabetes","MI","CHF","PVD","stroke","dementia","pulmonary","rheumatic","Liver","paralysis","renal","cancer","mets","hiv","ttdeath","Severity_Type","ECMO","MACE","Mechnical_Ventilation")
    for (x in  stdlist){
        #x_index <- which(stdlist==x)[1]
        #print(paste('x index',x_index))
        print(paste("------------ Var NOT in model x=",x,"----------------"))
                local_df$x <- local_df[,x]
                if (x %in% stdlist_cat) {
                    local_df$x <- as.factor(local_df[,x])
                }
                a <- glm(rs2_self ~ i_rural + x, data = local_df)
                print(summary(a))

                tb1 <- a
                tb1 <- a %>% tbl_regression(exponentiate = FALSE)
                tb1 <- as_tibble(tb1, col_labels = FALSE)
                tb1$x <- x
                tb1$md <- paste('i_rural + ', x)
                #print(tb1)
                tb2 <- rbind(tb2,tb1)
    }
    print(tb2)
    #tb2_signigicant <- filter(tb2, p_value<.05)

    #return(tb2_signigicant)
    return(tb2)
    #return(NULL)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.c081501f-2750-4b54-a237-584db81dd481"),
    s2_dt_i_rural_adj=Input(rid="ri.foundry.main.dataset.4ab41c9d-bbab-4ad5-af15-6c2b878e7389")
)
s2_md_i_rural_set3 <- function(s2_dt_i_rural_adj) {

local_df <- s2_dt_i_rural_adj

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
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

#find_idx <- function(val)df$index[match(index[val],df$value)]

    tb2 <-NA
    #"Ethnicity","i_rural","BMI_Group","age","CCI_INDEX","quarter_of_diagnosis","gender","Race"
    stdlist <- list("BMI_Group","age","CCI_INDEX","i_rural","Race","Ethnicity","quarter_of_diagnosis","gender")
    stdlist_cont <- list("CCI_INDEX", "age")



    for (x in  stdlist_cont){
        x_index <- which(stdlist==x) [1]
        print(paste('x index',x_index))
        local_df$x <- local_df[,x]

        md_prep <- glm(in_death_table ~ x, family=binomial, data = local_df)
        local_df$predprep <- predict(md_prep,type=c('response'))
        max_predprep <- max(local_df$predprep)
        min_predprep <- min(local_df$predprep)
        step_prepprep <- (max_predprep - min_predprep)/5

        local_df <- local_df %>% mutate(cont2cat = case_when(local_df$predprep <step_prepprep ~ 2,step_prepprep<=local_df$predprep & local_df$predprep <2*step_prepprep ~ 4,2*step_prepprep<=local_df$predprep & local_df$predprep <3*step_prepprep ~ 6,3*step_prepprep<=local_df$predprep & local_df$predprep<4*step_prepprep ~ 8, local_df$predprep >=2*step_prepprep ~ 10))

        x_factor <- paste(x,'_factor', sep = "")
        #local_df$x_factor <- as.factor(local_df$cont2cat)
        local_df[,x_factor] <- local_df$cont2cat

        print(paste("------------Continuous to category variable x=",x,"----------------"))

        a <- glm(rs2_self ~ i_rural + factor(cont2cat), data = local_df)
        print(summary(a))

        tb1 <- a
        tb1 <- a %>% tbl_regression(exponentiate = FALSE)
        tb1 <- as_tibble(tb1, col_labels = FALSE)
        tb1$x <- x
        tb1$md <- paste('r_rural + factor(', x,'_cont2cat)')
        #print(tb1)
        tb2 <- rbind(tb2,tb1)
    }
    print(tb2)
    #tb2_signigicant <- filter(tb2, p_value<.05)

    return(tb2)
    #return(NULL)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.0e46bae2-49ca-4715-baeb-9cefb0c0039a"),
    s2_dt_int_i_r_rural=Input(rid="ri.vector.main.execute.ff204ba9-400a-4c77-be97-dcf6e1197fc9")
)
s2_md_int_i_r_rural_set1 <- function(s2_md1, s2_dt_int_i_r_rural) {

local_df <- s2_dt_int_i_r_rural

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
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

#find_idx <- function(val)df$index[match(index[val],df$value)]

    tb2 <-NA
    #"Ethnicity","i_rural","BMI_Group","age","CCI_INDEX","quarter_of_diagnosis","gender","Race"
    stdlist <- list("r_rural","Race","Ethnicity","age","CCI_INDEX","quarter_of_diagnosis","gender")
    stdlist_cat <- list("gender","Race","Ethnicity","BMI_Group","quarter_of_diagnosis")

    for (x in  stdlist){
        x_index <- which(stdlist==x) [1]
        print(paste('x index',x_index))
        for (y in  stdlist){
            y_index <- which(stdlist==y) [1]
            print(paste('y index',y_index))
            if (x_index < y_index) {
                print(paste("------------x*y=",x,"*",y,"----------------"))
                local_df$x <- local_df[,x]
                local_df$y <- local_df[,y]
                if (x %in% stdlist_cat) {
                    local_df$x <- as.factor(local_df[,x])
                }
                if (y %in% stdlist_cat) {
                    local_df$y <- as.factor(local_df[,y])
                }
                a <- glm(rs2_self ~ r_rural + x*y, data = local_df)
                print(summary(a))

                tb1 <- a
                tb1 <- a %>% tbl_regression(exponentiate = FALSE)
                tb1 <- as_tibble(tb1, col_labels = FALSE)
                tb1$x <- x
                tb1$y <- y
                tb1$md <- paste('r_rural + ', x,'*',y)
                #print(tb1)
                tb2 <- rbind(tb2,tb1)
            }
        }
        return (tb2)
    }

    tb2_signigicant <- filter(tb2, p_value<.05)
    print(tb2_signigicant)
    return(tb2)
    #return(NULL)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.7bc49f42-247f-4b59-83c3-1aa5c15f896f"),
    s2_dt_r_rural_adj=Input(rid="ri.foundry.main.dataset.2614cacf-c819-441a-99dd-d1f0861fcb10")
)
s2_md_r_rural_set1 <- function(s2_md1, s2_dt_r_rural_adj) {

local_df <- s2_dt_r_rural_adj

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
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

#find_idx <- function(val)df$index[match(index[val],df$value)]

    tb2 <-NA
    #"Ethnicity","i_rural","BMI_Group","age","CCI_INDEX","quarter_of_diagnosis","gender","Race"
    stdlist <- list("r_rural","Race","Ethnicity","age","CCI_INDEX","quarter_of_diagnosis","gender")
    stdlist_cat <- list("gender","Race","Ethnicity","BMI_Group","quarter_of_diagnosis")

    for (x in  stdlist){
        x_index <- which(stdlist==x) [1]
        print(paste('x index',x_index))
        for (y in  stdlist){
            y_index <- which(stdlist==y) [1]
            print(paste('y index',y_index))
            if (x_index < y_index) {
                print(paste("------------x*y=",x,"*",y,"----------------"))
                local_df$x <- local_df[,x]
                local_df$y <- local_df[,y]
                if (x %in% stdlist_cat) {
                    local_df$x <- as.factor(local_df[,x])
                }
                if (y %in% stdlist_cat) {
                    local_df$y <- as.factor(local_df[,y])
                }
                a <- glm(rs2_self ~ r_rural + x*y, data = local_df)
                print(summary(a))

                tb1 <- a
                tb1 <- a %>% tbl_regression(exponentiate = FALSE)
                tb1 <- as_tibble(tb1, col_labels = FALSE)
                tb1$x <- x
                tb1$y <- y
                tb1$md <- paste('r_rural + ', x,'*',y)
                #print(tb1)
                tb2 <- rbind(tb2,tb1)
            }
        }
        return (tb2)
    }
    print(tb2)
    tb2_signigicant <- filter(tb2, p_value<.05)

    return(tb2_signigicant)
    #return(NULL)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.64ed035f-f52f-4d84-97e1-f064169e09a2"),
    s2_dt_r_rural_adj=Input(rid="ri.foundry.main.dataset.2614cacf-c819-441a-99dd-d1f0861fcb10")
)
s2_md_r_rural_set1_sw_1 <- function(s2_md1, s2_dt_r_rural_adj) {

local_df <- s2_dt_r_rural_adj

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
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

    a <- glm(rs2_self ~ r_rural+ r_rural*factor(gender) + r_rural*factor(Race) + r_rural*factor(Ethnicity) + r_rural*factor(BMI_Group) + r_rural*age + r_rural*CCI_INDEX  + r_rural*factor(quarter_of_diagnosis) , data = local_df,maxit = 100)

    print(summary(a))
    tb1 <- a
    tb1 <- a %>% tbl_regression(exponentiate = FALSE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)

    return(tb1)
    #return(NULL)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.cbcd04f4-62f6-47e8-8266-9ca890bb00b1"),
    s2_dt_r_rural_adj=Input(rid="ri.foundry.main.dataset.2614cacf-c819-441a-99dd-d1f0861fcb10")
)
s2_md_r_rural_set2 <- function(s2_md1, s2_dt_r_rural_adj) {

local_df <- s2_dt_r_rural_adj

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
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

#find_idx <- function(val)df$index[match(index[val],df$value)]

    tb2 <-NA
    #"Ethnicity","i_rural","BMI_Group","age","CCI_INDEX","quarter_of_diagnosis","gender","Race"

    stdlist <- list("four_category_ruca","three_category_ruca","i_rural","age_Group","CCI_Categories","data_partner_id","BMI","Q_Score_Categories","Q_Score","Diabetes","MI","CHF","PVD","stroke","dementia","pulmonary","rheumatic","Liver","paralysis","renal","cancer","mets","hiv","ttdeath","Severity_Type","ECMO","MACE","Mechnical_Ventilation","length_of_stay","ip_rate")

    stdlist_cat <- list("four_category_ruca","three_category_ruca","i_rural","age_Group","CCI_Categories","data_partner_id","Q_Score_Categories","Q_Score","Diabetes","MI","CHF","PVD","stroke","dementia","pulmonary","rheumatic","Liver","paralysis","renal","cancer","mets","hiv","ttdeath","Severity_Type","ECMO","MACE","Mechnical_Ventilation")
    for (x in  stdlist){
        #x_index <- which(stdlist==x)[1]
        #print(paste('x index',x_index))
        print(paste("------------ Var NOT in model x=",x,"----------------"))
                local_df$x <- local_df[,x]
                if (x %in% stdlist_cat) {
                    local_df$x <- as.factor(local_df[,x])
                }
                a <- glm(rs2_self ~ r_rural + x, data = local_df)
                print(summary(a))

                tb1 <- a
                tb1 <- a %>% tbl_regression(exponentiate = FALSE)
                tb1 <- as_tibble(tb1, col_labels = FALSE)
                tb1$x <- x
                tb1$md <- paste('r_rural + ', x)
                #print(tb1)
                tb2 <- rbind(tb2,tb1)
    }
    print(tb2)
    #tb2_signigicant <- filter(tb2, p_value<.05)

    #return(tb2_signigicant)
    return(tb2)
    #return(NULL)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.4b23793b-92ba-4fea-9acb-e6f55eb5f2cf"),
    s2_dt_r_rural_adj=Input(rid="ri.foundry.main.dataset.2614cacf-c819-441a-99dd-d1f0861fcb10")
)
s2_md_r_rural_set2_sw_1 <- function(s2_md1, s2_dt_r_rural_adj) {

local_df <- s2_dt_r_rural_adj

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
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

    a <- glm(rs2_self ~ r_rural + factor(four_category_ruca)+ factor(three_category_ruca)+factor(i_rural)+factor(age_Group)+factor(CCI_Categories)+factor(data_partner_id)+BMI+factor(Q_Score_Categories)+Q_Score+factor(Diabetes)+factor(MI)+factor(CHF)+factor(PVD)+factor(stroke)+factor(dementia)+factor(pulmonary)+factor(rheumatic)+factor(Liver)+factor(paralysis)+factor(renal)+factor(cancer)+factor(mets)+factor(hiv)+ttdeath+factor(Severity_Type)+factor(ECMO)+factor(MACE)+factor(Mechnical_Ventilation)+length_of_stay+ip_rate , data = local_df,maxit = 100)

    print(summary(a))
    tb1 <- a
    tb1 <- a %>% tbl_regression(exponentiate = FALSE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)

    return(tb1)
    #return(NULL)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.b2bb4208-0de1-4830-a7ab-46f3bf23ce3c"),
    s2_dt_r_rural_adj=Input(rid="ri.foundry.main.dataset.2614cacf-c819-441a-99dd-d1f0861fcb10")
)
s2_md_r_rural_set3 <- function(s2_md1, s2_dt_r_rural_adj) {

local_df <- s2_dt_r_rural_adj

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
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

#find_idx <- function(val)df$index[match(index[val],df$value)]

    tb2 <-NA
    #"Ethnicity","i_rural","BMI_Group","age","CCI_INDEX","quarter_of_diagnosis","gender","Race"
    stdlist <- list("r_rural","Race","Ethnicity","age","CCI_INDEX","quarter_of_diagnosis","gender")
    stdlist_cont <- list("age","CCI_INDEX")



    for (x in  stdlist_cont){
        x_index <- which(stdlist==x) [1]
        print(paste('x index',x_index))
        local_df$x <- local_df[,x]

        md_prep <- glm(in_death_table ~ x, family=binomial, data = local_df)
        local_df$predprep <- predict(md_prep,type=c('response'))
        max_predprep <- max(local_df$predprep)
        min_predprep <- min(local_df$predprep)
        step_prepprep <- (max_predprep - min_predprep)/5

        local_df <- local_df %>% mutate(cont2cat = case_when(local_df$predprep <step_prepprep ~ 2,step_prepprep<=local_df$predprep & local_df$predprep <2*step_prepprep ~ 4,2*step_prepprep<=local_df$predprep & local_df$predprep <3*step_prepprep ~ 6,3*step_prepprep<=local_df$predprep & local_df$predprep<4*step_prepprep ~ 8, local_df$predprep >=2*step_prepprep ~ 10))

        x_factor <- paste(x,'_factor', sep = "")
        #local_df$x_factor <- as.factor(local_df$cont2cat)
        local_df[,x_factor] <- local_df$cont2cat

        print(paste("------------Continuous to category variable x=",x,"----------------"))

        a <- glm(rs2_self ~ r_rural + factor(cont2cat), data = local_df)
        print(summary(a))

        tb1 <- a
        tb1 <- a %>% tbl_regression(exponentiate = FALSE)
        tb1 <- as_tibble(tb1, col_labels = FALSE)
        tb1$x <- x
        tb1$md <- paste('r_rural + factor(', x,'_cont2cat)')
        #print(tb1)
        tb2 <- rbind(tb2,tb1)
    }
    print(tb2)
    #tb2_signigicant <- filter(tb2, p_value<.05)

    return(tb2)
    #return(NULL)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f1f4b8a3-1b33-4f4b-9607-7ad6f98deafc"),
    s2_dt_r_rural_set3=Input(rid="ri.foundry.main.dataset.df2aa63e-7daf-4da5-8d75-a8c0902e97e8")
)
s2_md_r_rural_set3_sw <- function(s2_md1, s2_dt_r_rural_set3) {

local_df <- s2_dt_r_rural_set3

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
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

    a <- glm(rs2_self ~ r_rural + factor(age_factor)+factor(CCI_INDEX_factor) , data = local_df,maxit = 100)

    print(summary(a))
    tb1 <- a
    tb1 <- a %>% tbl_regression(exponentiate = FALSE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)

    return(tb1)
    #return(NULL)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.148af6cc-d12a-4eb6-9170-213cc1ddc0bd"),
    s2_dt_i_rural_dp_fe=Input(rid="ri.foundry.main.dataset.774ebfc9-21ab-43c0-b3f3-c1a9f6a2a466")
)
s2_mdfe_i_rural_set1_intact_i_rural <- function(s2_dt_i_rural_dp_fe) {

local_df <- s2_dt_i_rural_dp_fe

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
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

#find_idx <- function(val)df$index[match(index[val],df$value)]
st4sw <- 'i_rural'
    tb2 <-NA
    #"Ethnicity","i_rural","BMI_Group","age","CCI_INDEX","quarter_of_diagnosis","gender","Race"
    xstdlist <- list("i_rural")
    stdlist <- list("gender","Race","Ethnicity","age_Group","CCI_INDEX","BMI_Group","quarter_of_diagnosis")
    stdlist_cat <- list("gender","Race","Ethnicity","age_Group","BMI_Group","quarter_of_diagnosis")

    local_df_input <- local_df
    #local_df <-head(local_df,100)
    for (x in  xstdlist){
        #x_index <- which(stdlist==x) [1]
        #print(paste('x index',x_index))
        for (y in  stdlist){
            y_index <- which(stdlist==y) [1]
            print(paste('y index',y_index))
            #if (x_index < y_index) {
                print(paste("------------i_rural*y = i_rural*",y,"----------------"))
                if (y %in% stdlist_cat) {
                    local_df$y <- as.factor(local_df[,y])
                    st_y <- paste('*factor(',y,') ')
                }
                else {
                    local_df$y <- local_df[,y]
                    st_y <- paste('*',y)
                }
                #st4sw <- paste(st4sw, st_x , st_y)
                a <- glm(rs2_self ~  i_rural*y, data = local_df)
                print(summary(a))

                tb1 <- a
                tb1 <- a %>% tbl_regression(exponentiate = FALSE)
                tb1 <- as_tibble(tb1, col_labels = FALSE)

                tb1$y <- y
                tb1$md <- paste('i_rural *',y)
                #print(tb1)
                tb2 <- rbind(tb2,tb1)
            #}#if
        }#for_y
        #print(st4sw)
    }#for_x

    #tb2_signigicant <- filter(tb2, p_value<.05)

    print(tb2)
    return(tb2)
    #return(tb2_signigicant)
    #return(NULL)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.0559ce82-846a-41d7-a3f1-3add570eb3ce"),
    s2_dt_i_rural_dp_psc=Input(rid="ri.foundry.main.dataset.4df0c4a0-cfa9-4030-96c7-e3078e818d81")
)
s2_mdfe_i_rural_set1_intact_i_rural_1 <- function(s2_dt_i_rural_dp_psc) {

local_df <- s2_dt_i_rural_dp_psc

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
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

#find_idx <- function(val)df$index[match(index[val],df$value)]
st4sw <- 'i_rural'
    tb2 <-NA
    #"Ethnicity","i_rural","BMI_Group","age","CCI_INDEX","quarter_of_diagnosis","gender","Race"
    xstdlist <- list("i_rural")
    stdlist <- list("gender","Race","Ethnicity","age_Group","CCI_INDEX","BMI_Group","quarter_of_diagnosis")
    stdlist_cat <- list("gender","Race","Ethnicity","age_Group","BMI_Group","quarter_of_diagnosis")

    local_df_input <- local_df
    #local_df <-head(local_df,100)
    for (x in  xstdlist){
        #x_index <- which(stdlist==x) [1]
        #print(paste('x index',x_index))
        for (y in  stdlist){
            y_index <- which(stdlist==y) [1]
            print(paste('y index',y_index))
            #if (x_index < y_index) {
                print(paste("------------i_rural*y = i_rural*",y,"----------------"))
                if (y %in% stdlist_cat) {
                    local_df$y <- as.factor(local_df[,y])
                    st_y <- paste('*factor(',y,') ')
                }
                else {
                    local_df$y <- local_df[,y]
                    st_y <- paste('*',y)
                }
                #st4sw <- paste(st4sw, st_x , st_y)
                a <- glm(rs2_self ~  i_rural*y, data = local_df)
                print(summary(a))

                tb1 <- a
                tb1 <- a %>% tbl_regression(exponentiate = FALSE)
                tb1 <- as_tibble(tb1, col_labels = FALSE)

                tb1$y <- y
                tb1$md <- paste('i_rural *',y)
                #print(tb1)
                tb2 <- rbind(tb2,tb1)
            #}#if
        }#for_y
        #print(st4sw)
    }#for_x

    #tb2_signigicant <- filter(tb2, p_value<.05)

    a <- glm(rs2_self ~  i_rural*factor(quarter_of_diagnosis)+i_rural*CCI_INDEX + i_rural*factor(BMI_Group), data = local_df)
    print(summary(a))

    tb1 <- a
    tb1 <- a %>% tbl_regression(exponentiate = FALSE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)

    tb1$y <- '3intacts'
    tb1$md <- 'psc-3intacts'
    #print(tb1)
    tb2 <- rbind(tb2,tb1)
    print(tb2)

    return(tb2)
    #return(tb2_signigicant)
    #return(NULL)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.82d4eea7-b01d-4543-8c6c-f7a4531a5e3d"),
    s2_dt_i_rural_dp_fe=Input(rid="ri.foundry.main.dataset.774ebfc9-21ab-43c0-b3f3-c1a9f6a2a466")
)
s2_mdfe_i_rural_set1_intact_i_rural_3 <- function(s2_dt_i_rural_dp_fe) {

local_df <- s2_dt_i_rural_dp_fe

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
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

    a <- glm(rs2_self ~  i_rural*factor(quarter_of_diagnosis)+i_rural*CCI_INDEX + i_rural*factor(BMI_Group), data = local_df)
    print(summary(a))

    tb1 <- a
    tb1 <- a %>% tbl_regression(exponentiate = FALSE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)


    #tb2_signigicant <- filter(tb2, p_value<.05)

    print(tb1)
    return(tb1)
    #return(tb2_signigicant)
    #return(NULL)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.68794068-8f3a-4fa1-ba27-55362a1cc2e7"),
    unnamed_8=Input(rid="ri.foundry.main.dataset.6269482e-6137-4c64-97bc-9264db84b63a")
)
#Remove missing BMI
s_2cats_Mdrop_BMI_1_1 <- function(unnamed_8) {
   local_df <- unnamed_8

   # Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

   # %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San
    #md2 <- glm( in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease +factor(data_partner_id), family=binomial, data = local_df)


    md2 <- glm(in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 100)
    print(summary(md2))

    local_df$pred2 <- predict(md2,type=c('response'))
    local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md2 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)

    return(tb1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.bf0938e0-5374-444e-a89a-660352d697a8"),
    unnamed_8=Input(rid="ri.foundry.main.dataset.6269482e-6137-4c64-97bc-9264db84b63a")
)
#Remove missing BMI
s_2cats_NM_BMI_1 <- function(unnamed_8) {
   local_df <- unnamed_8

   # Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
#local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

   # %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San
    #md2 <- glm( in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease +factor(data_partner_id), family=binomial, data = local_df)


    md2 <- glm(in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 100)
    print(summary(md2))

    local_df$pred2 <- predict(md2,type=c('response'))
    local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md2 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)

    return(local_df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.6994004d-07bc-48e0-9a1d-bfa23e17ab56"),
    unnamed_8=Input(rid="ri.foundry.main.dataset.6269482e-6137-4c64-97bc-9264db84b63a")
)
#Remove missing BMI
s_2cats_NM_BMI_1_1 <- function(unnamed_8) {
   local_df <- unnamed_8

   # Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
#local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

   # %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San
    #md2 <- glm( in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease +factor(data_partner_id), family=binomial, data = local_df)


    md2 <- glm(in_death_table ~ i_rural + factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) + factor(data_partner_id), data = local_df, family = binomial, maxit = 100)
    print(summary(md2))

    local_df$pred2 <- predict(md2,type=c('response'))
    local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    tb1 <- md2 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print('------------------OR------------------------------------')
    print(tb1)

    return(local_df)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.229c53a7-815b-4133-afcb-1f59525bd7bf"),
    unnamed_8=Input(rid="ri.foundry.main.dataset.6269482e-6137-4c64-97bc-9264db84b63a")
)
#Remove missing BMI
s_2cats_NM_BMI_1_2 <- function(unnamed_8) {
   local_df <- unnamed_8

   # Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, i_rural != 1)
local_df <- filter(local_df, r_rural > .01)
#local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

   # %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San
    #md2 <- glm( in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease +factor(data_partner_id), family=binomial, data = local_df)

    print(min(local_df$r_rural))
    print(max(local_df$r_rural))

    md2 <- glm(in_death_table ~ r_rural + factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis)  , data = local_df, family = binomial, maxit = 100)
    print(summary(md2))

    local_df$pred2 <- predict(md2,type=c('response'))
    local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md2 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)

    return(local_df)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.04f3def4-d75b-4a30-9cd8-d75b58bf8ba4"),
    unnamed_8=Input(rid="ri.foundry.main.dataset.6269482e-6137-4c64-97bc-9264db84b63a")
)
#Remove missing BMI
s_2cats_NM_BMI_1_2_1 <- function(unnamed_8) {
   local_df <- unnamed_8

   # Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, i_rural == 1)
local_df <- filter(local_df, r_rural > .01)
#local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
#local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

   # %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San
    #md2 <- glm( in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease +factor(data_partner_id), family=binomial, data = local_df)

    print(min(local_df$r_rural))
    print(max(local_df$r_rural))

    md2 <- glm(in_death_table ~ r_rural + factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis)  , data = local_df, family = binomial, maxit = 100)
    print(summary(md2))

    local_df$pred2 <- predict(md2,type=c('response'))
    local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md2 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)

    return(local_df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.8c767db8-591e-4e1c-a921-ced14db04ba3"),
    unnamed_8=Input(rid="ri.foundry.main.dataset.6269482e-6137-4c64-97bc-9264db84b63a")
)
s_3cats <- function(unnamed_8) {
   local_df <- unnamed_8

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

   # %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San
    #md2 <- glm( in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease +factor(data_partner_id), family=binomial, data = local_df)

 #+ ip_rate
    md2 <- glm(in_death_table ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX + factor(three_category_ruca) + factor(quarter_of_diagnosis), data = local_df, family = binomial, maxit = 100)


    print(summary(md2))

    local_df$pred2 <- predict(md2,type=c('response'))
    local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    print('------------------OR------------------------------------')
    tb1 <- md2 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print(tb1)

    return(local_df)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.f2f48493-9a76-4b84-a803-60ece85009b1"),
    unnamed_8=Input(rid="ri.foundry.main.dataset.6269482e-6137-4c64-97bc-9264db84b63a")
)
#Remove missing BMI
s_3cats_NM_BMI <- function(unnamed_8) {
   local_df <- unnamed_8

   # Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

   # %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San
    #md2 <- glm( in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease +factor(data_partner_id), family=binomial, data = local_df)


    md2 <- glm(in_death_table ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX + factor(three_category_ruca) + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 100)



    print(summary(md2))

    local_df$pred2 <- predict(md2,type=c('response'))
    local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    return(local_df)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.48c24b4e-d8e2-417a-8314-4f7076387cc3"),
    unnamed_8=Input(rid="ri.foundry.main.dataset.6269482e-6137-4c64-97bc-9264db84b63a")
)
#Remove missing BMI
s_3cats_NM_BMI_1 <- function(unnamed_8) {
   local_df <- unnamed_8

   # Filter out missing variables
local_df <- filter(local_df, age_Group != "Unknown/Missing")
local_df <- filter(local_df, gender != "Other")
local_df <- filter(local_df, gender != "Unknown")
local_df <- filter(local_df, quarter_of_diagnosis != "Unknown/Missing")
#local_df <- filter(local_df, BMI_Group != "Unknown/Missing")

# Set reference categories
local_df$Race=relevel(as.factor(local_df$Race),ref="White")
local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic or Latino")
local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
local_df$three_category_ruca=relevel(as.factor(local_df$three_category_ruca),ref="urban")
local_df$quarter_of_diagnosis=relevel(as.factor(local_df$quarter_of_diagnosis),ref="Q2 2020")
local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

   # %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San
    #md2 <- glm( in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease +factor(data_partner_id), family=binomial, data = local_df)


    md2 <- glm(in_death_table ~ factor(three_category_ruca) + factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 100)
    print(summary(md2))

    local_df$pred2 <- predict(md2,type=c('response'))
    local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    tb1 <- md2 %>% tbl_regression(exponentiate = TRUE)
    tb1 <- as_tibble(tb1, col_labels = FALSE)
    print('------------------OR------------------------------------')
    print(tb1)

    return(local_df)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.4ff62ef4-0477-4920-8c37-35f10fc3b8c4"),
    unnamed_8=Input(rid="ri.foundry.main.dataset.6269482e-6137-4c64-97bc-9264db84b63a")
)
s_3cats_ip_rate <- function(unnamed_8) {
   local_df <- unnamed_8

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

   # %>% dplyr::select(in_death_table,i_rural,gender,age,Race,Ethnicity,BMI,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id)

    #md2 <- glm(  i_rural ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)
    #San
    #md2 <- glm( in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease +factor(data_partner_id), family=binomial, data = local_df)

 #+ ip_rate
    md2 <- glm(in_death_table ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX + factor(three_category_ruca) + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 100)



    print(summary(md2))

    local_df$pred2 <- predict(md2,type=c('response'))
    local_df$rs2_self <- local_df$in_death_table - local_df$pred2

    return(local_df)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.b02de9a0-a29b-4a96-9154-11d167fe8cd4"),
    s1_data_residuals_1=Input(rid="ri.foundry.main.dataset.2b38f775-6bda-4b43-9093-9b20a2d1c865")
)
s_graph_pred_resid <- function(s1_data_residuals_1) {
    local_df <- s1_data_residuals_1  %>% dplyr::select(pred2,rs2_self,in_death_table)
    plot(local_df$pred2,local_df$rs2_self,col=c("blue","red")[1+local_df$in_death_table])
    lines(lowess(local_df$pred2[local_df$in_death_table==0],local_df$rs2_self[local_df$in_death_table==0]),col="blue")
    lines(lowess(local_df$pred2[local_df$in_death_table==1],local_df$rs2_self[local_df$in_death_table==1]),col="red")
    lines(lowess(local_df$pred2,local_df$rs2_self),col="black",lwd=2)
    #abline(h=0,lty=2,col="grey")

}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.a5577ae4-830d-4b27-99fc-3be3678e0686"),
    s1_data_residuals_1=Input(rid="ri.foundry.main.dataset.2b38f775-6bda-4b43-9093-9b20a2d1c865")
)
#LTR
s_interact_LRT <- function(s1_data_residuals_1) {
    local_df <- s1_data_residuals_1 %>%
          dplyr::select(in_death_table, gender, Race, Ethnicity,BMI,BMI_Group,Q_Score, Q_cat,age,age_Group, quarter_dx,i_rural, any_disease,ip_rate, rs2,rs2_self )

    md2 <- glm(in_death_table ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) + i_rural + any_disease + ip_rate , data = local_df, family = binomial())
    print(paste('Min=',min(local_df$rs2_self)))
    print(paste('Max=',max(local_df$rs2_self)))

    tb2 <-NA

    stdlist <- stinlist_cat

    for (x in  stdlist){
        print("--------------------------------------------------------------------------------------")
        print(paste("--- Set 3b1: LRT adding each interation - i_rural + variable x  = ",x))
        local_df$x <- as.factor(local_df[,x])

        md3<- glm(in_death_table ~factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) +factor(quarter_dx)+ + any_disease + ip_rate+ i_rural + i_rural*x, data = local_df, family = binomial, maxit = 100)

        tb1 <- anova(md2,md3,test="LRT")
        print(tb1)

        #tb1 <- local_df %>% group_by(i_rural,x) %>% summarise(npats = n(), rs2_m=mean(rs2_self))
        tb1 <- as_tibble(tb1, col_labels = FALSE)
        tb1$varname <- x
        tb2 <- rbind(tb2,tb1)
    }

    stdlist <- stinlist_cont

    for (x in  stdlist){
        print("--------------------------------------------------------------------------------------")
        print(paste("--- Set 3b2: LRT adding each interation - i_rural + variable x  = ",x))
        local_df$x <- local_df[,x]

        md3<- glm(in_death_table ~factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) +factor(quarter_dx)+ + any_disease + ip_rate+ i_rural + i_rural*x, data = local_df, family = binomial, maxit = 100)

        tb1 <- anova(md2,md3,test="LRT")
        print(tb1)

        #tb1 <- local_df %>% group_by(i_rural,x) %>% summarise(npats = n(), rs2_m=mean(rs2_self))
        tb1 <- as_tibble(tb1, col_labels = FALSE)
        tb1$varname <- x
        tb2 <- rbind(tb2,tb1)
    }
    return(NULL)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.7dc6d77a-edbf-4c00-834d-ac149c707e21"),
    s1_data_residuals_1=Input(rid="ri.foundry.main.dataset.2b38f775-6bda-4b43-9093-9b20a2d1c865")
)
s_interact_NOTinvars <- function(s1_data_residuals_1) {


      local_df <- s1_data_residuals_1 %>%
          dplyr::select(i_rural,in_death_table, ttdeath,in_death_table,ruca_cat,age_Group,Race,Ethnicity,BMI_Group,Q_Score,Diabetes,MI,CHF,PVD,stroke,dementia,pulmonary,rheumatic,Liver,paralysis,renal,cancer,mets,hiv,Severity_Type,ECMO,MACE,Mechnical_Ventilation,covid_peak,los,data_partner_id,pred2,rs2,rs2_self)

    tb2 <-NA
    stdlist <- stNOTinlist_cat

    for (x in  stdlist){
        print("--------------------------------------------------------------")
        print(paste("--- Set 3c1: Interact between i_rural and each variable in md2 model - variable x = ",x))
        local_df$x <- local_df[,x]

        #two.way <- aov(rs2 ~ x+ruca_cat+x*ruca_cat , data = local_df)
        two.way <- glm(in_death_table ~ i_rural + factor(x)  + factor(x)*i_rural , data = local_df)
        print(summary(two.way))

        tb1 <- local_df %>% group_by(i_rural,x) %>% summarise(npats = n(), rs2_m=mean(rs2_self))
        tb1 <- as_tibble(tb1, col_labels = FALSE)
        tb1$varname <- x


        tb2 <- rbind(tb2,tb1)
    }
    stdlist <- stNOTinlist_cont

    for (x in  stdlist){
        print("--------------------------------------------------------------")
        print(paste("--- Set 3c2: Interact between i_rural and each variable in md2 model - variable x = ",x))
        local_df$x <- local_df[,x]

        #two.way <- aov(rs2 ~ x+ruca_cat+x*ruca_cat , data = local_df)
        two.way <- glm(in_death_table ~ i_rural + x  + x*i_rural , data = local_df)
        print(summary(two.way))

        #tb1 <- local_df %>% group_by(i_rural,x) %>% summarise(npats = n(), rs2_m=mean(rs2_self))
        #tb1 <- as_tibble(tb1, col_labels = FALSE)
        #tb1$varname <- x

        #tb2 <- rbind(tb2,tb1)
    }


    return(tb2)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.7d5d467c-2cc7-468e-baeb-558fb11be0c9"),
    s1_data_residuals_1=Input(rid="ri.foundry.main.dataset.2b38f775-6bda-4b43-9093-9b20a2d1c865")
)
s_interact_NOTinvars_1 <- function(s1_data_residuals_1) {


      local_df <- s1_data_residuals_1 %>%
          dplyr::select(i_rural,in_death_table, ttdeath,in_death_table,ruca_cat,age_Group,Race,Ethnicity,BMI_Group,Q_Score,Diabetes,MI,CHF,PVD,stroke,dementia,pulmonary,rheumatic,Liver,paralysis,renal,cancer,mets,hiv,Severity_Type,ECMO,MACE,Mechnical_Ventilation,covid_peak,los,data_partner_id,pred2,rs2,rs2_self, ip_rate)

    tb2 <-NA
    stdlist <- stNOTinlist_cat

    for (x in  stdlist){
        print("--------------------------------------------------------------")
        print(paste("--- Set 3c1: Interact between i_rural and each variable in md2 model - variable x = ",x))
        local_df$x <- local_df[,x]

        #two.way <- aov(rs2 ~ x+ruca_cat+x*ruca_cat , data = local_df)
        two.way <- glm(in_death_table ~ i_rural + factor(x) , data = local_df, family="binomial")
        print(summary(two.way))

        tb1 <- local_df %>% group_by(i_rural,x) %>% summarise(npats = n(), rs2_m=mean(rs2_self))
        tb1 <- as_tibble(tb1, col_labels = FALSE)
        tb1$varname <- x


        tb2 <- rbind(tb2,tb1)
    }
    stdlist <- stNOTinlist_cont

    for (x in  stdlist){
        print("--------------------------------------------------------------")
        print(paste("--- Set 3c2: Interact between i_rural and each variable in md2 model - variable x = ",x))
        local_df$x <- local_df[,x]

        #two.way <- aov(rs2 ~ x+ruca_cat+x*ruca_cat , data = local_df)
        two.way <- glm(in_death_table ~ i_rural + x  + x*i_rural , data = local_df)
        print(summary(two.way))

        #tb1 <- local_df %>% group_by(i_rural,x) %>% summarise(npats = n(), rs2_m=mean(rs2_self))
        #tb1 <- as_tibble(tb1, col_labels = FALSE)
        #tb1$varname <- x

        #tb2 <- rbind(tb2,tb1)
    }


    return(tb2)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.56725eae-2284-4cb3-a920-ac17601fcc18"),
    s1_data_residuals_1=Input(rid="ri.foundry.main.dataset.2b38f775-6bda-4b43-9093-9b20a2d1c865")
)
##glm in_death_table ~ Interaction
s_interact_invars <- function(s1_data_residuals_1) {


      local_df <- s1_data_residuals_1 %>%
          dplyr::select(in_death_table, gender, Race, Ethnicity,BMI,BMI_Group,Q_Score, Q_cat,age,age_Group, quarter_dx,i_rural, any_disease, ip_rate, rs2,rs2_self )

    tb2 <-NA
    stdlist <- stinlist_cat

    for (x in  stdlist){
        print("--------------------------------------------------------------")
        print(paste("--- Set 3a1: Interact between i_rural and each variable in md2 model - variable x = ",x))
        local_df$x <- local_df[,x]

        #two.way <- aov(rs2 ~ x+ruca_cat+x*ruca_cat , data = local_df)
        two.way <- glm(in_death_table ~ i_rural + factor(x)  + factor(x)*i_rural , data = local_df)
        print(summary(two.way))

        tb1 <- local_df %>% group_by(i_rural,x) %>% summarise(npats = n(), rs2_m=mean(rs2_self))
        tb1 <- as_tibble(tb1, col_labels = FALSE)
        tb1$varname <- x


        tb2 <- rbind(tb2,tb1)
    }
    #tb2$invar = 1

    stdlist <- stinlist_cont

    for (x in  stdlist){
        print("--------------------------------------------------------------")
        print(paste("--- Set 3a2: Interact between i_rural and each variable in md2 model - variable x = ",x))
        local_df$x <- local_df[,x]

        #two.way <- aov(rs2 ~ x+ruca_cat+x*ruca_cat , data = local_df)
        two.way <- glm(in_death_table ~ i_rural + x  + x*i_rural , data = local_df)
        print(summary(two.way))

        #tb1 <- local_df %>% group_by(i_rural,x) %>% summarise(npats = n(), rs2_m=mean(rs2_self))
        #tb1 <- as_tibble(tb1, col_labels = FALSE)
        #tb1$varname <- x

        #tb2 <- rbind(tb2,tb1)
    }

    return(tb2)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ae0992b2-26f9-4ca4-8b68-c130ad60f6a2"),
    s1_data_residuals_1=Input(rid="ri.foundry.main.dataset.2b38f775-6bda-4b43-9093-9b20a2d1c865")
)
## print OR of md2
s_print_OR_md2 <- function(s1_data_residuals_1) {

    local_df <- s1_data_residuals_1
    # Set reference categories
    # Set reference categories
    local_df$Race=relevel(as.factor(local_df$Race),ref="White")
    local_df$Ethnicity=relevel(as.factor(local_df$Ethnicity),ref="Not Hispanic")
    local_df$BMI_Group=relevel(as.factor(local_df$BMI_Group),ref="18.5-24.9")
    local_df$ruca_cat=relevel(as.factor(local_df$ruca_cat),ref="urban")
    local_df$quarter_dx=relevel(as.factor(local_df$quarter_dx),ref="2020q1")

    md2 <- glm(in_death_table ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age  + factor(Q_cat) +factor(quarter_dx) + i_rural + any_disease + ip_rate , data = local_df, family = 'binomial', maxit = 100)

    #md2 <- glmer(in_death_table ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age  + factor(Q_cat) +factor(quarter_dx) + i_rural | data_partner_id  , data = local_df, family = binomial)
   # md2 <- glmer(in_death_table ~ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age  + factor(Q_cat) +factor(quarter_dx) + i_rural | data_partner_id  , data = local_df, family = binomial)

    #tbl_regression(m1, exponentiate = TRUE)
    table1 <- md2 %>%
        tbl_regression(exponentiate = TRUE)
    table1 <- as_tibble(table1, col_labels = FALSE)
    print(table1)

return(table1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.e6f4b601-1ad2-47d5-a169-9db890c57d74"),
    s_2cats_NM_BMI_1=Input(rid="ri.foundry.main.dataset.bf0938e0-5374-444e-a89a-660352d697a8")
)
s_r_set1 <- function(s_2cats_NM_BMI_1) {

local_df <- s_2cats_NM_BMI_1

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
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

    tb2 <-NA
    #"Ethnicity","i_rural","BMI_Group","age","CCI_INDEX","quarter_of_diagnosis","gender","Race"
    stdlist <- list("Race","Ethnicity","age","CCI_INDEX","quarter_of_diagnosis","gender")
    stdlist_cat <- list("gender","Race","Ethnicity","BMI_Group","quarter_of_diagnosis")

    for (x in  stdlist){
        for (y in  stdlist){

            if (x!=y){
                print(paste("------------x*y=",x,"*",y,"----------------"))
                local_df$x <- local_df[,x]
                local_df$y <- local_df[,y]
                if (x %in% stdlist_cat) {
                    local_df$x <- as.factor(local_df[,x])
                }
                if (y %in% stdlist_cat) {
                    local_df$y <- as.factor(local_df[,y])
                }
                a <- glm(rs2_self ~ i_rural + x*y, data = local_df)
                print(summary(a))
                #tb1 <- summary(a)
                #tb1 <- a %>% tbl_regression(exponentiate = TRUE)
                #tb1 <- as_tibble(tb1, col_labels = FALSE)
                #tb1$x <- x
                #tb1$y <- y
                #tb1$md <- paste('i_rural + ', x,'*',y)
                #print(tb1)
                #tb2 <- rbind(tb2,tb1)
            }
        }
    }


    #return(tb2)
    return(NULL)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.ba6880c0-8453-4dc8-bcc0-1802d5a5e4ca"),
    s_2cats_NM_BMI_1=Input(rid="ri.foundry.main.dataset.bf0938e0-5374-444e-a89a-660352d697a8")
)
s_r_set1_2 <- function(s_2cats_NM_BMI_1) {

local_df <- s_2cats_NM_BMI_1

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
#local_df$three_category_ruca <- factor(local_df$three_category_ruca, levels=c("urban", "urban-adjacent rural", "nonurban-adjacent rural"))

    tb2 <-""
    stdlist <- list("i_rural","gender","Race","Ethnicity","BMI_Group","age","CCI_INDEX","quarter_of_diagnosis")
    stdlist_cat <- list("gender","Race","Ethnicity","BMI_Group","quarter_of_diagnosis")

    indvars<- '0'
    nvars = length(stdlist)

    for (i in 2:nvars-1){
        x   <- stdlist[i]

        print(paste("x=",x))
        k1 <-grep(x, colnames(local_df))

        local_df$x <- local_df[,k1]
        if (class(local_df$x)=='character') { x <- paste('factor(',x,')') }

        for (j in 1+i:nvars){
            y <- f_y <- stdlist[j]

            print(paste("y=",y))
            k2 <-grep(y, colnames(local_df))
            local_df$y <- local_df[,k2]
            if (class(local_df$y)=='character') { y <- paste('factor(',y,')') }

            indvars <- paste(indvars ,"+", x,"*",y)
        }
    }
    print(indvars)
    return(NULL)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.47dbb795-485f-4cee-a908-800cc5a72fd0"),
    s_2cats_NM_BMI_1=Input(rid="ri.foundry.main.dataset.bf0938e0-5374-444e-a89a-660352d697a8")
)
s_r_set2 <- function(s_2cats_NM_BMI_1) {

local_df <- s_2cats_NM_BMI_1

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

    tb2 <-NA
    stdlist <- list("ttdeath","four_category_ruca","three_category_ruca","age_Group","BMI","Q_Score_Categories","Q_Score","Diabetes","MI","CHF","PVD","stroke","dementia","pulmonary","rheumatic","Liver","paralysis","renal","cancer","mets","hiv","Severity_Type","ECMO","MACE","Mechnical_Ventilation","covid_peak","length_of_stay","data_partner_id","CCI_Categories","ip_rate")
    stdlist_cat <- list("four_category_ruca","three_category_ruca","age_Group","Q_Score_Categories","Diabetes","MI","CHF","PVD","stroke","dementia","pulmonary","rheumatic","Liver","paralysis","renal","cancer","mets","hiv","Severity_Type","ECMO","MACE","Mechnical_Ventilation","data_partner_id","CCI_Categories")
    for (x in  stdlist){
        print(paste("------------x=",x,"----------------"))
        local_df$x <- local_df[,x]
        if (x %in% stdlist_cat) {
                    local_df$x <- as.factor(local_df[,x])
        }
        a <- glm(rs2_self ~ i_rural+ x + i_rural*x, data = local_df)
        print(summary(a))
        tb1 <- a %>% tbl_regression(exponentiate = TRUE)
        tb1 <- as_tibble(tb1, col_labels = FALSE)
        tb1$x <- x
        print(tb1)
        tb2 <- rbind(tb2,tb1)
    }
    return(tb2)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.d8e0aa68-5b3e-4c77-9129-f621d4b2a534"),
    s_2cats_NM_BMI_1=Input(rid="ri.foundry.main.dataset.bf0938e0-5374-444e-a89a-660352d697a8")
)
s_r_set2_1 <- function(s_2cats_NM_BMI_1) {

local_df <- s_2cats_NM_BMI_1

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

    tb2 <-NA
    stdlist <- list("dementia","pulmonary","rheumatic","Liver","paralysis","renal","cancer","mets","hiv","Severity_Type","ECMO","MACE","Mechnical_Ventilation","covid_peak","length_of_stay","data_partner_id","CCI_Categories","ip_rate","ttdeath","four_category_ruca","three_category_ruca","age_Group","BMI","Q_Score_Categories","Q_Score","Diabetes","MI","CHF","PVD","stroke")
    stdlist_cat <- list("four_category_ruca","three_category_ruca","age_Group","Q_Score_Categories","Diabetes","MI","CHF","PVD","stroke","dementia","pulmonary","rheumatic","Liver","paralysis","renal","cancer","mets","hiv","Severity_Type","ECMO","MACE","Mechnical_Ventilation","data_partner_id","CCI_Categories")
    for (x in  stdlist){
        print(paste("------------x=",x,"----------------"))
        local_df$x <- local_df[,x]
        if (x %in% stdlist_cat) {
                    local_df$x <- as.factor(local_df[,x])
        }
        a <- glm(rs2_self ~ i_rural+ x , data = local_df)
        print(summary(a))
        tb1 <- a %>% tbl_regression(exponentiate = TRUE)
        tb1 <- as_tibble(tb1, col_labels = FALSE)
        tb1$x <- x
        print(tb1)
        tb2 <- rbind(tb2,tb1)
    }
    return(tb2)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.9ffb5bf4-c5b2-4c48-9378-e66d48185cae"),
    s_2cats_NM_BMI_1=Input(rid="ri.foundry.main.dataset.bf0938e0-5374-444e-a89a-660352d697a8")
)
s_r_set3 <- function(s_2cats_NM_BMI_1) {

local_df <- s_2cats_NM_BMI_1

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

    tb2 <-NA
    stdlist <- list("i_rural","gender","Race","Ethnicity","BMI_Group","age","CCI_INDEX","quarter_of_diagnosis")
    stdlist_cat <- list("gender","Race","Ethnicity","BMI_Group","quarter_of_diagnosis")
    stdlist_cont <- list("age","CCI_INDEX")

for (x in  stdlist_cont){
    local_df$x <- local_df[,x] )
    a<-glm(in_death_table ~ x, family = binomial, data= local_df)
    local_df$pred2 <- predict(a,type=c('response'))
    local$cat <- x02
    x24
    x46
    x68
    x80
    return(tb2)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.0343df6a-717b-44ad-a045-c953b1a08257"),
    s_2cats_NM_BMI_1=Input(rid="ri.foundry.main.dataset.bf0938e0-5374-444e-a89a-660352d697a8"),
    s_r_set1_2=Input(rid="ri.vector.main.execute.ba6880c0-8453-4dc8-bcc0-1802d5a5e4ca")
)
unnamed_11 <- function(s_r_set1_2, s_2cats_NM_BMI_1) {
    local_df <-    s_2cats_NM_BMI_1
    a <- glm(in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis) , data = local_df, family = binomial, maxit = 100)

}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.d7bd4115-f204-4d30-8d8b-11c3b8449861"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
unnamed_23 <- function(unnamed_17) {
    local_df <- unnamed_17
    print("------------------------------------------------------------------------------------")
    tb0 <- local_df %>% tbl_summary(by = i_rural) %>% add_p()
    tb0 <- as_tibble(tb0, col_labels = FALSE)
    print(tb0)
    return(tb0)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.b22aa817-6376-4afe-8a38-4a6aa18f12ce"),
    s1_data_residuals_1_1_1=Input(rid="ri.foundry.main.dataset.baa3369c-5e0f-4a99-953d-73a04a39970d")
)
unnamed_3 <- function(s1_data_residuals_1_1_1) {
        local_df <- s1_data_residuals_1_1_1

        a <- glm(in_death_table ~ i_rural, family=binomial, data = local_df)
        print(summary(a))
        tb1 <- a %>% tbl_regression(exponentiate = TRUE)
        tb1 <- as_tibble(tb1, col_labels = FALSE)

        return(tb1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.db80fd22-a5c4-461b-ae49-84c100bd954f"),
    s1_data_residuals_1_1_1=Input(rid="ri.foundry.main.dataset.baa3369c-5e0f-4a99-953d-73a04a39970d")
)
unnamed_3_1 <- function(s1_data_residuals_1_1_1) {
        local_df <- s1_data_residuals_1_1_1

        a <- glm(in_death_table ~ i_rural + factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease +  factor(data_partner_id), family=binomial, data = local_df)
        print(summary(a))
        tb1 <- a %>% tbl_regression(exponentiate = TRUE)
        tb1 <- as_tibble(tb1, col_labels = FALSE)

        return(tb1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.acd92f20-adb0-4c13-b860-14829289f9d3"),
    s1_data_residuals_1_1_1_2=Input(rid="ri.foundry.main.dataset.437408bb-054b-46ca-854c-8a84d0333730")
)
library(lme4)

unnamed_3_1_1 <- function(s1_data_residuals_1_1_1_2) {
        local_df <- s1_data_residuals_1_1_1_2

        a <- lmer(in_death_table ~ 1 + i_rural + (1|data_partner_id), family=binomial, data = local_df)
        print(summary(a))
        tb1 <- a %>% tbl_regression(exponentiate = TRUE)
        tb1 <- as_tibble(tb1, col_labels = FALSE)

        return(tb1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.a6f75b39-1905-41ab-b9c8-f1892682e88c"),
    s1_data_residuals_1_1_1_2=Input(rid="ri.foundry.main.dataset.437408bb-054b-46ca-854c-8a84d0333730")
)
unnamed_3_2 <- function(s1_data_residuals_1_1_1_2) {
        local_df <- s1_data_residuals_1_1_1_2

        a <- glm(in_death_table ~ i_rural + factor(data_partner_id), family=binomial, data = local_df)
        print(summary(a))
        tb1 <- a %>% tbl_regression(exponentiate = TRUE)
        tb1 <- as_tibble(tb1, col_labels = FALSE)

        return(tb1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f661d122-f4fb-4b50-b90b-e2b33d025b83"),
    s1_data_residuals_1_1_1_2=Input(rid="ri.foundry.main.dataset.437408bb-054b-46ca-854c-8a84d0333730")
)
unnamed_3_2_1 <- function(s1_data_residuals_1_1_1_2) {
        local_df <- s1_data_residuals_1_1_1_2

        a <- glm(in_death_table ~ i_rural + factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease + factor(data_partner_id), family=binomial, data = local_df)

        print(summary(a))
        tb1 <- a %>% tbl_regression(exponentiate = TRUE)
        tb1 <- as_tibble(tb1, col_labels = FALSE)

        return(tb1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.77896235-a28d-4b3a-827e-1b67da602f03"),
    s1_data_residuals_1_1_1_2=Input(rid="ri.foundry.main.dataset.437408bb-054b-46ca-854c-8a84d0333730")
)
unnamed_3_2_1_1 <- function(s1_data_residuals_1_1_1_2) {
        local_df <- s1_data_residuals_1_1_1_2

        a <- glm(in_death_table ~ i_rural + factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)

        print(summary(a))
        tb1 <- a %>% tbl_regression(exponentiate = TRUE)
        tb1 <- as_tibble(tb1, col_labels = FALSE)

        return(tb1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.c239fe8e-bed4-4a5f-afac-119ce8938cb2"),
    s1_data_residuals_1_1_1_2_1=Input(rid="ri.foundry.main.dataset.0e516f2d-b7d2-44f3-9668-524b341ac6f7")
)
unnamed_3_2_1_2 <- function(s1_data_residuals_1_1_1_2_1) {
        local_df <- s1_data_residuals_1_1_1_2_1

        a <- glm(in_death_table ~ i_rural + factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease + factor(data_partner_id), family=binomial, data = local_df)

        print(summary(a))
        tb1 <- a %>% tbl_regression(exponentiate = TRUE)
        tb1 <- as_tibble(tb1, col_labels = FALSE)

        return(tb1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.7b9ec325-e704-4bb1-ad96-8247f3767b32"),
    s1_data_residuals_1_1_1_2_1=Input(rid="ri.foundry.main.dataset.0e516f2d-b7d2-44f3-9668-524b341ac6f7")
)
unnamed_6 <- function(s1_data_residuals_1_1_1_2_1) {
        local_df <- s1_data_residuals_1_1_1_2_1

        a <- glm(in_death_table ~ i_rural + factor(data_partner_id), family=binomial, data = local_df)
        print(summary(a))
        tb1 <- a %>% tbl_regression(exponentiate = TRUE)
        tb1 <- as_tibble(tb1, col_labels = FALSE)

        return(tb1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.264e3be5-3337-48f2-878c-26a4d338172b"),
    s1_data_residuals_1_1_1_2_1=Input(rid="ri.foundry.main.dataset.0e516f2d-b7d2-44f3-9668-524b341ac6f7")
)
unnamed_7 <- function(s1_data_residuals_1_1_1_2_1) {
        local_df <- s1_data_residuals_1_1_1_2_1

        a <- glm(in_death_table ~ i_rural + factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + factor(Q_cat) + factor(quarter_dx) +  any_disease , family=binomial, data = local_df)

        print(summary(a))
        tb1 <- a %>% tbl_regression(exponentiate = TRUE)
        tb1 <- as_tibble(tb1, col_labels = FALSE)

        return(tb1)
}
