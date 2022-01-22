Exceprts from analysis-1
==================

https://unite.nih.gov/workspace/vector/view/ri.vector.main.workbook.69dc0fbc-b028-4c9f-a633-abc957c5284a?branch=master

Global Code
------------------

```r
plot_asthma_by_steroid <- function (
    m, 
    outcome_label, 
    y_limits = NULL, 
    y_breaks = NULL,
    title    = NULL
) {
    if (!inherits(m, "glm")) stop("The class of the `m` argument must be 'glm'.")
    
    terms <- c("asthma", "steroid_class")
    m %>%
        broom::augment(
            newdata       = ggeffects::new_data(m, terms = terms),
            type.predict  = "response",
            se_fit        = TRUE
        ) %>%
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
        ) %>%
        ggplot(aes(x = x, y = yhat, group = group, color = group, fill = group, ymin = lower, ymax = upper)) +
        geom_ribbon() +
        # geom_linerange() +
        scale_y_continuous(breaks = y_breaks) +
        scale_color_manual(values = palette_dark) +
        scale_fill_manual(values = palette_light) +
        coord_flip(ylim = y_limits) +
        guides(color = guide_legend(override.aes = list(size = 6))) +
        guides(fill = guide_legend(override.aes = list(alpha = 1))) +
        theme_minimal(base_size = 30) +
        # theme(axis.line = element_line(size = 0)) + # color doesn't work w/ ggeffects
        theme(panel.grid.major.y = element_line(color = "gray97")) +
        theme(panel.grid.minor.y = element_blank()) +
        labs(
            x = "Asthma Category", 
            y = outcome_label, 
            color = "Steroid Class",
            fill = "Steroid Class",
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
