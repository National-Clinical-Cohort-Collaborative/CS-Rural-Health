

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.09101df7-7ecd-4507-99d1-0aae4e9d06da"),
    grades_unpivoted_data_partner=Input(rid="ri.foundry.main.dataset.0980ad6e-7a6a-43a5-b592-c007784c597e")
)
data_partner_grade_plot <- function(grades_unpivoted_data_partner) {
    library(dplyr)
    library(ggplot2)
    library(RColorBrewer)
    local_df <- grades_unpivoted_data_partner %>% dplyr::select(data_partner_id, domain, grade)

    grade_heatmap <- ggplot(data = local_df, mapping = aes(x = domain,
                                                       y = data_partner_id,
                                                       fill = grade)) +
    geom_tile() +
    scale_fill_distiller(palette = "YlOrRd") +
    xlab(label = "Domain") 

plot(grade_heatmap)
return(grades_unpivoted_data_partner)
}

