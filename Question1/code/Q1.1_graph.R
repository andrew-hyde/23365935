graph_q1.1_func <- function(df_data, title, subtitle, caption, xlabel, ylabel){

    library(tidyverse)

    # wrangle data to create variables for scores above thresholds
    df_q1.1_data <- df_covid %>% select(continent, total_cases_per_million, total_deaths_per_million) %>%
        na.omit() %>%
        group_by(continent) %>%
        summarise(across(everything(), sum)) %>% # summarise averages for each continent
        mutate(total_cases_per_million = round(total_cases_per_million, digits = 1)) %>%  # round down
        mutate(total_deaths_per_million = round(total_deaths_per_million, digits = 1)) %>%
        tail(6) %>%
        gather(label, value, -continent)

    ########################

graph <- ggplot(data = df_q1.1_data) +

        geom_bar(aes(x = continent, y = value, fill = label), stat = "identity") +
        geom_text(aes(x = continent, y = value, label = value), vjust = 0) +

        facet_wrap(~label, scales = "free_y", nrow = 2) +

        labs(title = title,
             subtitle = subtitle,
             caption = caption,
             x = xlabel,
             y = ylabel) +

        theme() +
        theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
        theme(legend.position="none")


    graph


}

