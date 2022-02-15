Excerpts from analysis-1
==================

https://unite.nih.gov/workspace/vector/view/ri.vector.main.workbook.69dc0fbc-b028-4c9f-a633-abc957c5284a?branch=master

Global Code
------------------

```r
load_packages <- function () {
    library(magrittr)
    library(ggplot2)
    library(broom)
    library(ggpubr)
    library(ggeffects)
    # library(effects)
}

factor_steroid <- function (x) {
    dplyr::recode_factor(
        x,         
        'no_steroids_documented'  = 'none documented', 
        'steroid_nasal'           = 'nasal', 
        'steroid_inhaled'         = 'inhaled', 
        'steroid_systemic'        = 'systemic'        
    )
}

factor_period <- function (x) {
    factor(
        x,
        levels = c(
            "2020H1"
            ,"2020H2"
            ,"2021H1"
            ,"2021H2"
            # ,"2022H1"
            # ,"2022H2"
        )
    )
}

factor_asthma <- function (x) {
    dplyr::recode_factor(
        x, 
        '7-none'                        = 'none',
        # '6-asthma - other'              = 'other',
        '4-mild intermittent asthma'    = 'mild intermittent',
        '3-mild persistent asthma'      = 'mild persist',
        '2-moderate persistent asthma'  = 'moderate persist',
        '1-severe persistent asthma'    = 'severe persist',
        '5-asthma - unspecified'        = 'unspecified'
    )
}

prepare_predictors <- function (d) {
    d %>%
        # dplyr::filter(
        #     hypertension == 0L &
        #     # upper_gi_bleed == 0L &
        #     MI == 0L &
        #     CHF == 0L &
        #     PVD == 0L &
        #     stroke == 0L &
        #     # dementia == 0L &
        #     # pulmonary == 0L &
        #     # rheumatic == 0L &
        #     # PUD == 0L &   # peptic ulcer
        #     # liver_mild == 0L &
        #     diabetes == 0L &
        #     dmcx == 0L &        # ?
        #     paralysis == 0L &
        #     renal == 0L &
        #     cancer == 0L &
        #     mets == 0L &   # metastatic?
        #     hiv == 0L
        # ) %>%
        dplyr::mutate(
            severe_dead    = as.integer(severe_dead),
            inpatient_ed   = as.integer(inpatient_ed),
            steroid        = factor_steroid(steroid_class),
            asthma         = factor_asthma(asthma_category_cdc), 
            period_first_covid_dx = factor_period(period_first_covid_dx),
        )  %>%
        dplyr::select(
            -steroid_class, 
            -asthma_category_cdc,
        ) %>%
        tibble::as_tibble()
}

tidy_model <- function (m, model_title) {
    m %>%
        broom::tidy() %>%
        dplyr::mutate(
            model = model_title
        ) %>%
        tibble::as_tibble() %>%
        dplyr::mutate_if(is.numeric, round, 5)
}

palette_dark <- c( # http://colrd.com/image-dna/29746/
  "none documented" = "gray50",
  "nasal"           = "#45a79e",  # green
  "inhaled"         = "#bfc269",  # yellow green
  "systemic"        = "#f46b4f",  # orange
  "title"           = "#646596"   # purple
  # "inhaled"         = "#a06e97", # mauve
)
palette_light <- scales::alpha(palette_dark, alpha = .5)
    
augment_prediction <- function (m, terms) {
    if (!inherits(m, "glm")) stop("The class of the `m` argument must be 'glm'.")

    broom::augment(
        x             = m,
        newdata       = ggeffects::new_data(m, terms = terms),
        type.predict  = "response",
        se_fit        = TRUE
    )  %>%
    dplyr::mutate(
        lower = .fitted - .se.fit * 1.96,
        upper = .fitted + .se.fit * 1.96,
    ) %>%
    dplyr::select(
        yhat  = .fitted,
        x     = tidyselect::matches(terms[1]),
        group = tidyselect::matches(terms[2]),
        lower,
        upper,
    ) 
}

plot_asthma_by_steroid <- function (
    ms, 
    outcome_label, 
    y_limits = NULL, 
    y_breaks = NULL,
    y_scale  = NULL,
    title    = NULL
) {
    #if (!inherits(ms, "glm")) stop("The class of the `m` argument must be 'glm'.")
    
    terms <- c("asthma", "steroid")
    ms %>%
        purrr::map_df(function(m) augment_prediction(m, terms), .id = "model") %>%
        ggplot(aes(x = x, y = yhat, group = group, color = group, fill = group, ymin = lower, ymax = upper)) +
        geom_line() + 
        geom_ribbon(color = NA) +
        geom_point() +
        # geom_linerange() +
        scale_y_continuous(breaks = y_breaks, labels = y_scale) +
        scale_color_manual(values = palette_dark) +
        scale_fill_manual(values = palette_light) +
        coord_flip(ylim = y_limits) +
        facet_wrap("model") +
        guides(color = guide_legend(override.aes = list(size = 6))) +
        guides(fill = guide_legend(override.aes = list(alpha = 1))) +
        theme_minimal(base_size = 20) +
        theme(legend.title     = element_text(color = palette_dark[["title"]], face = "bold")) +
        theme(strip.background = element_rect(fill  = palette_dark[["title"]], color = NA)) +
        theme(strip.text       = element_text(color = "gray97"               , face = "bold")) +
        theme(axis.title       = element_text(color = palette_dark[["title"]], face = "bold")) +
        # theme(axis.line = element_line(size = 0)) + # color doesn't work w/ ggeffects
        theme(panel.grid.major.y = element_line(color = "gray97")) +
        theme(panel.grid.minor.y = element_blank()) +
        labs(
            x     = "Asthma Category", 
            y     = outcome_label, 
            color = "Steroid Class",
            fill  = "Steroid Class",
            title = title
        )
}
```

Cell Counts
--------------------

```r
graph_counts_unfiltered_2 <- function(cell_count_unfiltered) {
    library(ggplot2)

    ds <- 
        cell_count_unfiltered %>%
        tibble::as_tibble() %>%
        dplyr::mutate(
            steroid        = factor_steroid(steroid_class),
            asthma         = factor_asthma(asthma_category_cdc), 
            pt_count       = as.integer(pt_count),                 # from a 64-bit int
        )

    {
        ggplot(ds, aes(x = asthma, y = pt_count, group = steroid, color = steroid)) +
        geom_line(size = 4, alpha = .4) +
        geom_point(size = 3) +
        # annotation_logticks() # Not w/ ggplot2 3.3.0: https://stackoverflow.com/a/68972745/1082435
        scale_y_log10(
            breaks = scales::trans_breaks("log10", function(x) 10^x),
            labels = scales::trans_format("log10", scales::math_format(10^.x))
        ) +
        scale_color_manual(values = palette_dark) +
        scale_fill_manual(values = palette_light) +
        coord_cartesian(ylim = c(10, 5000000)) +
        # coord_flip(ylim = c(10, 5000000)) +
        guides(color = guide_legend(override.aes = list(size = 6))) +
        guides(fill = guide_legend(override.aes = list(alpha = 1))) +
        theme_minimal(base_size = 20) +
        theme(legend.position = c(1, 1), legend.justification = c(1, 1)) +
        theme(legend.title     = element_text(color = palette_dark[["title"]], face = "bold")) +
        theme(strip.background = element_rect(fill  = palette_dark[["title"]], color = NA)) +
        theme(strip.text       = element_text(color = "gray97"               , face = "bold")) +
        theme(axis.title       = element_text(color = palette_dark[["title"]], face = "bold")) +
        # theme(axis.line = element_line(size = 0)) + # color doesn't work w/ ggeffects
        theme(panel.grid.major.y = element_line(color = "gray97")) +
        theme(panel.grid.minor.y = element_blank()) +
        labs(
            x     = "Asthma Category", 
            y     = "Patient Count (log 10)", 
            color = "Steroid Class",
            fill  = "Steroid Class",
            title = "Patient Counts: Asthma x Steroid Combination"
        )
    } %>%
        print()
    
    return(ds)
}
```

LOS Unfiltered
--------------------

```r
los_unfiltered <- function (Ds_patient_2) {
    load_packages()

    main_title    <- "Unfiltered"
    outcome_name  <- "length_of_stay"
    outcome_label <- "Length of Stay"

    predictors_1  <- "asthma * steroid"
    predictors_2  <- "asthma + steroid"
    model_titles  <- c("interaction",  "main effects")

    glm_link      <- "poisson"
    y_limits      <- c(0, 12)
    y_breaks      <- c(0, 3, 6, 9, 12)
    y_scale       <- scales::comma_format(accuracy = 1)

    ds <- 
        Ds_patient_2 %>%
        prepare_predictors()

    # ---- nothing below really should change ----------------------------
    predictors <- list(predictors_1, predictors_2)
    eqs <- 
        paste0(outcome_name, " ~ ", predictors) %>%
        rlang::set_names(model_titles)
        
    ds %>%
        dplyr::count(steroid, asthma) %>%
        print(n = 100)
        
    ms <-
        eqs %>%
        purrr::map(function(eq) glm(eq, data = ds, family = glm_link))

    ms %>%
        plot_asthma_by_steroid(
            outcome_label = outcome_label,
            y_limits      = y_limits, 
            y_breaks      = y_breaks,
            y_scale       = y_scale,
            title         = main_title
        ) %>%
        print()
      
    bs <-
        list(ms, model_titles) %>%
        purrr::pmap(function(m, n) tidy_model(m, n))

    # bs %>%
    #     purrr::walk(function(t) print(t, n = 100))

    bs %>%
        purrr::map_dfr(~.) %>%
        dplyr::select(model, term, estimate, std.error, statistic, p.value)
}
```

Steroid by Quarter
--------------------------

```r
explore_steroid_quarter <- function (Ds_patient_2) {
    load_packages()

    predictors <- "steroid + quarter_first_covid_dx"
    terms      <- c("quarter_first_covid_dx", "steroid")
    title      <- "Steroid by Quarter-Year"
    
    ds <- 
        Ds_patient_2 %>%
        prepare_predictors()

    outcomes <- 
        c(
            "inpatient_ed",
            "length_of_stay",
            "severe_dead"
        ) %>%
        rlang::set_names()   
    
    eqs <- 
        outcomes %>%
        purrr::map_chr(function (o) sprintf("%s ~ %s", o, predictors))

    links <- c(
        "binomial",
        "poisson",
        "binomial"
    )

    d_count <-
        ds %>%
        dplyr::count(!!! syms(terms)) %>%
        dplyr::mutate(
            yhat    = log10(n),
            outcome = "patient_count_log10",
            lower   =  NA_real_,
            upper   =  NA_real_,
        ) %>%
        dplyr::select(
            outcome,
            yhat,
            x         = terms[1],
            group     = terms[2],
            lower, 
            upper,
        )

    dgs <-                             # datasets for graphing
        list(eqs, links) %>%
        purrr::pmap(function (eq, l) glm(eq, data = ds, family = l)) %>%
        purrr::map_dfr(function (m) augment_prediction(m, terms), .id = "outcome") %>%
        dplyr::union_all(d_count) %>%
        dplyr::mutate(
            outcome = factor(
                outcome,
                levels = c("patient_count_log10"  , "inpatient_ed", "length_of_stay", "severe_dead"),
                labels = c("Patient Count (log10)", "Inpatient/ED", "Length of Stay", "Dead/Severe COVID")
            )
        )

    {
        dgs %>%
            ggplot(aes(x = x, y = yhat, group = group, color = group, fill = group, ymin = lower, ymax = upper)) +
            geom_line() +
            geom_ribbon(color = NA) +
            scale_color_manual(values = palette_dark) +
            scale_fill_manual(values = palette_light) +
            facet_wrap("outcome", scales = "free_y", ncol = 1) + # github.com/zeehio/facetscales
            guides(color = guide_legend(reverse = TRUE, override.aes = list(size = 6))) +
            guides(fill  = guide_legend(reverse = TRUE, override.aes = list(alpha = 1))) +
            theme_minimal(base_size = 20) +
            # theme(legend.position = c(1, .55), legend.justification = c(1, .5)) +
            theme(legend.title     = element_text(color = palette_dark[["title"]], face = "bold")) +
            theme(strip.background = element_rect(fill  = palette_dark[["title"]], color = NA)) +
            theme(strip.text       = element_text(color = "gray97"               , face = "bold")) +
            theme(axis.title       = element_text(color = palette_dark[["title"]], face = "bold")) +
            # theme(axis.line = element_line(size = 0)) + # color doesn't work w/ ggeffects
            theme(panel.grid.major.y = element_line(color = "gray97")) +
            theme(panel.grid.minor.y = element_blank()) +
            labs(
                x     = NULL, 
                y     = NULL, 
                color = "Steroid Class",
                fill  = "Steroid Class",
                title = title
            )
    } %>%
        print()

    return (ds)
}
```
