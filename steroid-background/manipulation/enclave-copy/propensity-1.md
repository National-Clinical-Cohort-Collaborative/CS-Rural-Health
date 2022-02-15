Excerpts from propensity-1
==================

https://unite.nih.gov/workspace/vector/view/ri.vector.main.workbook.5a94ff22-3d02-432d-9a62-f135eeda95b2?branch=master

Global Code
------------------

```r
load_packages <- function () {    
    library(magrittr)
    library(twang)
    library(survey)
}
    
prepare_dataset <- function (d, .slice_ids) {
    d %>%
        # tibble::as_tibble() %>%
        dplyr::filter(partner_slice_id %in% .slice_ids) %>%
        dplyr::mutate(
            age_cut5             = as.factor(age_cut5),
            asthma_v1            = as.factor(asthma_v1),
            asthma_v2            = as.factor(asthma_v2),
            tx_v1                = as.factor(tx_v1),
            tx_v2                = as.factor(tx_v2),
            tx_v3                = as.factor(tx_v3),
            age                  = as.factor(age),
            gender_male          = as.factor(gender_male),
            smoking_ever         = as.factor(smoking_ever),
            bmi_cut5             = factor_bmi(bmi_cut5),
            # height               = as.numeric(height),
            # weight               = as.numeric(weight),
            race_v1    = as.factor(race_v1),
            race_v2    = as.factor(race_v2),
            partner_slice_id     = as.factor(partner_slice_id),
        )  
}

estimate_propensity <- function (d) {
    equation_propensity <- "tx_v1 ~ asthma + age_cut5 + gender_male + smoking_ever + BMI + race_ethnicity_v1"
    # ignored terms: + Height + Weight

    mnps(
        as.formula(equation_propensity), 
        data = d,
        estimand = 'ATE', #vs ATT
        train.fraction = 0.5,
        verbose = F,
        interaction.depth = 2, 
        n.trees = 1500,
        shrinkage = .1,
        stop.method = 'es.max', #minimize the maximum effect size
        version = 'xgboost' 
    )

    #multinomial propensity scores (for binomial, use ps()) --using `twang` package
    #https://onlinelibrary.wiley.com/doi/10.1002/sim.5753

    # When n.trees = 10000 # >  Error: cannot allocate vector of size 11.6 Gb
    # When n.trees =  2000 # >  Error: cannot allocate vector of size 2.3 Gb
}

graph_diagnostics <- function (propensity_model) {
    print("Full balance table")
    propensity_model %>%
        bal.table() %>%
        print()

    print("Balance collapsed to covariate")
    propensity_model %>%
        bal.table(collapse.to = 'covariate') %>%
        print()
   
   print("Balance collapsed to stop method")
    propensity_model %>%
        bal.table(collapse.to = 'stop.method') %>%
        print()

    print("Subset no tx vs. systemic")
    propensity_model %>%
        bal.table(
            subset.treat = c('no_tx_documented', 'steroid_systemic'),  
            subset.var   = c('asthma')
        )

    p1 <- plot(propensity_model, plots = 1)
    p2 <- plot(propensity_model, plots = 2)
    p3 <- plot(propensity_model, plots = 3)
    p4 <- plot(propensity_model, plots = 4)

    ggpubr::ggarrange(
        p1[[1]], p1[[2]], p1[[3]], # p1[[4]],#p1[[5]],p1[[6]],
        p2[[1]], p2[[2]], p2[[3]], # p2[[4]],#p2[[5]],p2[[6]],
        # p3[[1]], p3[[2]], p3[[3]], p3[[4]],p3[[5]],p3[[6]],
        # p4[[1]], p4[[2]], p4[[3]], p4[[4]],p4[[5]],p4[[6]],

        nrow = 2,
        ncol = 3
    ) %>%
        plot() %>%
        print()
}

factor_bmi <- function (x) {
    factor(
        x, 
        levels = c(
            "underweight", 
            "healthy", 
            "overweight", 
            "obese", 
            "missing"
        )
    )
}
```


`patient_thinned`
------------------

```sql
with person_1 as (
    -- Thin: take only 25% of patients without no-asthma
    -- select cast(right(person_id, 2) as int) , count(*) as count FROM patient GROUP BY cast(right(person_id, 2) as int)  order by count(*) asc
    --  select count(*) as count FROM patient WHERE cast(right(person_id, 2) as int) < 25 and asthma_v1 = 'none'

    SELECT
        *        
    FROM patient
    WHERE 
        (asthma_v1 = 'none' and tx_v1 = 'no_tx_documented')
        and 
        -- cast(right(person_id, 2) as int) < 50  -- Catch [0, 49]
        cast(right(person_id, 2) as int) < 25  -- Catch [0, 24]

    UNION ALL   

    -- Keep: take 100% of patients with any category of asthma
    SELECT
        *
    FROM patient 
    WHERE 
        asthma_v1 != 'none' or tx_v1 != 'no_tx_documented'
)
,person_2 as (
    SELECT
        person_id
        ,date_first_covid_dx
        ,month_first_covid_dx
        ,period_first_covid_dx
        ,first_visit_date_in_3_mos
        ,last_visit_start_date_in_3_months
        ,asthma_v1
        ,asthma_v2
        ,pulmonary
        ,tx_v1
        ,tx_v2
        ,tx_v3
        ,tx_nonsteroid_biologic
        ,tx_steroid_systemic
        ,tx_steroid_inhaled
        ,tx_steroid_nasal
        ,tx_nonsteroid_saba
        ,tx_steroid_unused
        ,in_death_table
        ,length_of_stay
        ,age
        ,age_cut5
        ,severe_dead
        ,dead_w_covid
        ,gender_male
        ,smoking_ever
        ,Severity_Type
        ,covid_fatality
        ,inpatient_ed
        ,bmi
        ,bmi_cut5
        ,height
        ,weight
        ,race_v1
        ,race_v2
        ,count_comorbidities
        ,data_partner_id
        ,partner_slice_id
        ,cast(rand() * 8 as int) + 1  as patient_slice_id
    FROM person_1
)
SELECT /*+ COALESCE(200) */   * FROM  person_2
```

`propensity_3`
-------------------

```r
propensity_3 <- function( patient_thinned) {
    slice_ids <- 1:99

    # ---- nothing below should change between slices ----------
    load_packages()
  
    ds <-
        patient_thinned %>%
        dplyr::filter(tx_v1 %in% c("no_tx_documented", "steroid_systemic")) %>%
        dplyr::filter(asthma_v2 == "severe") %>%
        prepare_dataset(slice_ids) %>%
        dplyr::mutate(
            tx_systemic = dplyr::recode(tx_v1, "no_tx_documented" = 0, "steroid_systemic" = 1),
        )

    print(table(systemic = ds$tx_systemic, ds$asthma_v2, useNA = "always"))
    print(table(male = ds$gender_male, ds$asthma_v2, useNA = "always"))
    print(table(smoking_ever = ds$smoking_ever, ds$tx_systemic, useNA = "always"))
    print(table(bmi = ds$bmi_cut5, smoking_ever = ds$smoking_ever, useNA = "always"))
    print(table(tx = ds$tx_v1, smoking_ever = ds$smoking_ever, useNA = "always"))
    print(table(race = ds$race_v2, tx = ds$tx_v1, useNA = "always"))

    # Three-way tables
    print("tx by bmi by smoking -----")
    print(table(bmi = ds$bmi_cut5, smoking_ever = ds$smoking_ever, tx = ds$tx_v1, useNA = "always"))
    print("tx by bmi by gender -----")
    print(table(bmi = ds$bmi_cut5, gender_male = ds$gender_male, tx = ds$tx_v1, useNA = "always"))
    print("tx by bmi by race -----")
    print(table(bmi = ds$bmi_cut5, race = ds$race_v2, tx = ds$tx_v1,useNA = "always"))

    ds %>%
        dplyr::select(
            x = bmi,
        ) %>%
        tidyr::drop_na(x) %>%
        dplyr::summarize(
            n    = dplyr::n(),
            mean = mean(x, na.rm = TRUE),
            min  = min(x, na.rm = TRUE),
            # q01  = quantile(x, probs = .01),
            # q05  = quantile(x, probs = .05),
            q10  = quantile(x, probs = .10),
            q25  = quantile(x, probs = .25),
            q40  = quantile(x, probs = .40),
            q45  = quantile(x, probs = .45),
            q50  = quantile(x, probs = .50),
            # q55  = quantile(x, probs = .55),
            q60  = quantile(x, probs = .60),
            q75  = quantile(x, probs = .75),
            q90  = quantile(x, probs = .90),
            q95  = quantile(x, probs = .95),
            q98  = quantile(x, probs = .98),
            q99  = quantile(x, probs = .99),
            max  = max(x, na.rm = TRUE),
        )

    # equation_propensity <- "tx_systemic ~ asthma_v1 + age_cut5 + gender_male + smoking_ever + bmi + race_v2"
    # equation_propensity <- "tx_systemic ~ asthma_v1 + gender_male + age_cut5  + smoking_ever + bmi + race_v2 + pulmonary" #asthma_v2, period_first_covid_dx
    # equation_propensity <- "tx_systemic ~ asthma_v2 + gender_male + age_cut5  + smoking_ever + bmi + race_v2 + pulmonary" #asthma_v2, period_first_covid_dx
    equation_propensity <- "tx_systemic ~ gender_male + age_cut5  + smoking_ever + bmi_cut5 + race_v2" #asthma_v2, period_first_covid_dx
    
    ps_mod <- twang::ps(
        as.formula(equation_propensity), 
        data = ds,
        estimand = 'ATE', #vs ATT
        # estimand = 'ATT', #vs ATT
        train.fraction = 0.5,
        verbose = FALSE,
        interaction.depth = 2, 
        n.trees = 1500,
        shrinkage = .1,
        stop.method = 'es.max', #minimize the maximum effect size
        version = 'xgboost' 
    )
    
    ggpubr::ggarrange(
        plot(ps_mod, plots = 1),
        plot(ps_mod, plots = 2),
        plot(ps_mod, plots = 3),
        plot(ps_mod, plots = 4),

        nrow = 2,
        ncol = 2
    ) %>%
        plot() %>%
        print()
        
    slim_balance <- function (l, type, suffix = "") {
        l[[type]] %>%
            tibble::rownames_to_column("estimate") %>%
            dplyr::mutate(
                se = abs(std.eff.sz),
                # se2 = (tx.mn - ct.mn) / tx.sd,
            ) %>%
            dplyr::select(
                estimate,
                tx.mn,
                # tx.sd,
                ct.mn,
                # ct.sd,
                se,
                # se          = std.eff.sz,
                # se2,
                # stat,
                # p,
                # ks,
                # ks.pval,
            ) %>%
            dplyr::rename_at(
            .funs   = function(n) paste0(n, "_", suffix),
            .vars = dplyr::vars(-estimate)
            )
    }

    compare_types <- function (l) {
        slim_balance(l, "unw", "uw") %>%
            dplyr::left_join(slim_balance(l, "es.max.ATE", "w"), by = "estimate") %>%
            dplyr::arrange(dplyr::desc(se_w))
    }

    ps_mod %>%
        twang::bal.table(collapse.to = 'covariate') %>%
        compare_types() %>%
        print()


    # print("Balance collapsed to covariate")
    # ps_mod %>%
    #     bal.table(collapse.to = 'covariate') %>%
    #     tibble::as_tibble() %>%
    #     print()
   
    # print("Balance collapsed to stop method")
    # ps_mod %>%
    #     bal.table(collapse.to = 'stop.method') %>%
    #     tibble::as_tibble() %>%
    #     print(n = 25)
    # print("")

    # print("Subset no tx vs. systemic")
    # ps_mod %>%
    #     bal.table(
    #         subset.treat = c('no_tx_documented', 'steroid_systemic'),  
    #         subset.var   = c('asthma_v2')
    #     )
    # print("")


    # print(plot(ps_mod, plots = 5))
    # ps_mod$desc %>%
    #     as.data.frame() %>%
    #     tibble::rownames_to_column("effect") %>%
    #     print()
    

    # ps_mod <- estimate_propensity(ds)
        
    # #Assign weights to original ds.    
    ds$weight_asthma_specific <- get.weights(ps_mod, stop.method = 'es.max')

    # graph_diagnostics(ps_mod)
    
    return(ds)    
}
```
