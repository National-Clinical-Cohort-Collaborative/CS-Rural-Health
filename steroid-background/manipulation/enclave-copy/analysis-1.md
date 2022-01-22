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
            inpatient_ed_2 = as.integer(inpatient_ed),
            steroid_class  = factor_steroid(steroid_class),
            asthma         = factor_asthma(asthma_category_cdc), 
        ) %>%
        tibble::as_tibble()
}

build_equation <- function (outcome_name, predictors) {
    sprintf(
        "%s ~ 1 + %s",
        outcome_name,
        predictors
    )
}

tidy_model <- function (m, model_name) {
    m %>%
        broom::tidy()  %>%
        dplyr::mutate(
            model = model_name
        )%>%
        tibble::as_tibble() %>%
        dplyr::mutate_if(is.numeric, round, 5)
}

plot_asthma_by_steroid <- function (
    m, 
    outcome_label, 
    y_limits = NULL, 
    y_breaks = NULL,
    title    = NULL
) {
    if (!inherits(m, "glm")) stop("The class of the `m` argument must be 'glm'.")

    palette_dark <- # http://colrd.com/image-dna/29746/
        c(
            "none documented" = "gray50",
            "nasal"           = "#45a79e", # purple
            # "nasal"           = "#646596", # purple
            # "inhaled"         = "#a06e97", # mauve
            "inhaled"         = "#bfc269", # yellow green
            "systemic"        = "#f46b4f"  # orange
        )
    
    m %>%
        ggpredict(
            terms = c("asthma", "steroid_class")
        ) %>%
        plot( # https://strengejacke.github.io/ggeffects/articles/introduction_plotcustomize.html
            ci = TRUE,
            facet = FALSE, 
            line.size = 3, 
            # limits = y_limits,
            connect.lines = TRUE
        ) +
        scale_y_continuous(breaks = y_breaks) +
        scale_color_manual(values = palette_dark) +
        coord_flip(ylim = y_limits) +
        guides(color = guide_legend(override.aes = list(size = 6))) +
        theme_ggeffects(base_size = 30) +
        theme(axis.line = element_line(size = 0)) + # color doesn't work w/ ggeffects
        theme(panel.grid.major.y = element_line(color = "gray97")) +
        theme(panel.grid.minor.y = element_blank()) +
        labs(
            x = "Asthma Category", 
            y = outcome_label, 
            color = "Steroid Class",
            title = title
        )
}
```

LOS Unfiltered
--------------------

```r
los_unfiltered_global <- function( Ds_patient_2) {
    load_packages()
    theme_set(theme_bw())

    outcome_name  <- "length_of_stay"
    outcome_label <- "Length of Stay"

    predictors_1  <- "asthma * steroid_class"
    predictors_2  <- "asthma + steroid_class"
    model_name_1  <- paste0("interaction: ", predictors_1)
    model_name_2  <- paste0("main: "       , predictors_2)

    glm_link      <- "poisson"
    y_limits      <- c(0, 9)
    y_breaks      <- c(0, 3, 6, 9)

    ds <- 
        Ds_patient_2 %>%
        prepare_predictors()
        
    m1 <- 
        build_equation(outcome_name, predictors_1) %>%
        glm(data = ds, family = glm_link)  
        
    m2 <- 
        build_equation(outcome_name, predictors_2) %>%
        glm(data = ds, family = glm_link)    
      
    t1 <-
        m1 %>%
        tidy_model(model_name_1)
    t2 <-
        m2 %>%
        tidy_model(model_name_2)

    print(t1, n = 100)
    print(t2, n = 100)
    
    m1 %>%
        plot_asthma_by_steroid(
            outcome_label = outcome_label,
            y_limits      = y_limits, 
            y_breaks      = y_breaks,
            title         = "Model 1"
        ) %>%
        print()    

    m2 %>%
        plot_asthma_by_steroid(
            outcome_label = outcome_label,
            y_limits      = y_limits, 
            y_breaks      = y_breaks,
            title         = "Model 2"
        ) %>%
        print()

    t1 %>%
        rbind(t2) %>%
        dplyr::select(model, term, estimate, std.error, statistic, p.value)
    
}
```
