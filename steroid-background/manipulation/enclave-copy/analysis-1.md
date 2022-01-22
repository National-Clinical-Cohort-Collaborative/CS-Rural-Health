Exceprts from analysis-1
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

factor_asthma <- function (x) {
    dplyr::recode_factor(
        x, 
        '7-none'                        = '7-none',
        '6-asthma - other'              = '6-other',
        '5-asthma - unspecified'        = '5-unspecified',
        '4-mild intermittent asthma'    = '4-mild intermittent',
        '3-mild persistent asthma'      = '3-mild persist',
        '2-moderate persistent asthma'  = '2-moderate persist',
        '1-severe persistent asthma'    = '1-severe persist'
    )
}

prepare_predictors <- function (d) {
    d %>%
        dplyr::mutate(
            inpatient_ed   = as.integer(inpatient_ed),
            steroid        = factor_steroid(steroid_class),
            asthma         = factor_asthma(asthma_category_cdc), 
        ) %>%
        tibble::as_tibble()
}

# build_equation <- function (outcome_name, predictors) {
#     sprintf(
#         "%s ~ 1 + %s",
#         outcome_name,
#         predictors
#     )
# }

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
palette_light <- scales::alpha(palette_dark, alpha = .8)
    
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
        # geom_line() + 
        geom_ribbon() +
        geom_point() +
        geom_linerange() +
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

LOS Unfiltered
--------------------

```r
    los_unfiltered <- function(Ds_patient_2) {
    load_packages()

    outcome_name  <- "length_of_stay"
    outcome_label <- "Length of Stay"

    predictors_1  <- "asthma * steroid"
    predictors_2  <- "asthma + steroid"
    model_titles  <- c("interaction",  "main effects")

    glm_link      <- "poisson"
    y_limits      <- c(0, 12)
    y_breaks      <- c(0, 3, 6, 9, 12)
    y_scale       <- NULL

    ds <- 
        Ds_patient_2 %>%
        prepare_predictors()

    # ---- nothing below really should change ----------------------------
    predictors <- list(predictors_1, predictors_2)
    eqs <- 
        paste0(outcome_name, " ~ ", predictors) %>%
        rlang::set_names(model_titles)
        
    ms <-
        eqs %>%
        purrr::map(function(eq) glm(eq, data = ds, family = glm_link))

    ms %>%
        plot_asthma_by_steroid(
            outcome_label = outcome_label,
            y_limits      = y_limits, 
            y_breaks      = y_breaks,
            y_scale       = y_scale
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
