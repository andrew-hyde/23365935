graph_q1.2_func <- function(df_data, title, subtitle, caption, xlabel, ylabel){

    library(tidyverse)

    # wrangle data to get 10 poorest and 10 least poor countries
    df_q1.2_data_a <- df_covid %>%
        select(location, icu_patients_per_million, extreme_poverty ) %>%
        na.omit() %>%
        group_by(location) %>%
        summarise(across(everything(), mean)) %>% # summarise averages for each continent
        arrange(extreme_poverty) %>% head(10)

    df_q1.2_data_b <- df_covid %>% select(location, icu_patients_per_million, extreme_poverty ) %>%
               na.omit() %>%
                 group_by(location) %>%
                summarise(across(everything(), mean)) %>% # summarise averages for each continent
                 arrange(extreme_poverty) %>% tail(10)

    ######################
    # bind data frames

    df_q1.2_data <- bind_rows(df_q1.2_data_a, df_q1.2_data_b) %>%
        tidyr::gather(label, value, -location) %>%
        mutate(across(3, round, 2)) # round to 2 digits

        # mutate(total_cases= round(total_cases, digits = 1)) %>%  # round down
        # mutate(total_deaths = round(total_deaths, digits = 1)) %>%
        # tail(6) %>%
        # gather(label, value, -continent)

    ########################

    graph <- ggplot(data = df_q1.2_data) +

        geom_bar(aes(x = reorder(location, value), y = value, colour = label, fill = label), stat = "identity") +
        #geom_text(aes(x = continent, y = value, label = value), vjust = 0) +

        #facet_wrap(~label, scales = "free_y", nrow = 2) +

        labs(title = title,
             subtitle = subtitle,
             caption = caption,
             x = xlabel,
             y = ylabel) +

        theme() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme(legend.position="bottom")


    graph


}

