graph_q1.3_func <- function(df_data, title, subtitle, caption, xlabel, ylabel){

    library(tidyverse)

    # wrangle data to get 10 poorest and 10 least poor countries
    df_q1.3_data_a <- df_covid %>% filter(continent == "Asia") %>%
        mutate(date = as.Date(date)) %>%
        select(date, icu_patients_per_million, hospital_beds_per_thousand) %>%
        na.omit() %>%
        group_by(date) %>%
        summarise_at(c("icu_patients_per_million", "hospital_beds_per_thousand"), mean) %>%
        mutate(Continent = "Asia") %>%
        arrange(date)

    df_q1.3_data_b <- df_covid %>% filter(continent == "Africa") %>%
        mutate(date = as.Date(date)) %>%
        select(date, icu_patients_per_million, hospital_beds_per_thousand) %>%
        na.omit() %>%
        group_by(date) %>%
        summarise_at(c("icu_patients_per_million", "hospital_beds_per_thousand"), mean) %>%
        mutate(Continent = "Africa") %>%
        arrange(date)

    df_q1.3_data_c <- df_covid %>% filter(continent == "Europe") %>%
        mutate(date = as.Date(date)) %>%
        select(date, icu_patients_per_million, hospital_beds_per_thousand) %>%
        na.omit() %>%
        group_by(date) %>%
        summarise_at(c("icu_patients_per_million", "hospital_beds_per_thousand"), mean) %>%
        mutate(Continent = "Europe") %>%
        arrange(date)

    df_q1.3_data_d <- df_covid %>% filter(continent == "North America") %>%
        mutate(date = as.Date(date)) %>%
        select(date, icu_patients_per_million, hospital_beds_per_thousand) %>%
        na.omit() %>%
        group_by(date) %>%
        summarise_at(c("icu_patients_per_million", "hospital_beds_per_thousand"), mean) %>%
        mutate(Continent = "North America") %>%
        arrange(date)

    df_q1.3_data_e <- df_covid %>% filter(continent == "Oceania") %>%
        mutate(date = as.Date(date)) %>%
        select(date, icu_patients_per_million, hospital_beds_per_thousand) %>%
        na.omit() %>%
        group_by(date) %>%
        summarise_at(c("icu_patients_per_million", "hospital_beds_per_thousand"), mean) %>%
        mutate(Continent = "Oceania") %>%
        arrange(date)

    df_q1.3_data_f <- df_covid %>% filter(continent == "South America") %>%
        mutate(date = as.Date(date)) %>%
        select(date, icu_patients_per_million, hospital_beds_per_thousand) %>%
        na.omit() %>%
        group_by(date) %>%
        summarise_at(c("icu_patients_per_million", "hospital_beds_per_thousand"), mean) %>%
        mutate(Continent = "South America") %>%
        arrange(date)

######################
    # bind data frames

df_q1.3_data <- bind_rows(df_q1.3_data_a, df_q1.3_data_b, df_q1.3_data_c,
                          df_q1.3_data_d, df_q1.3_data_e, df_q1.3_data_f) %>%
    tidyr::gather(label, value, -date, -Continent)
    #mutate(across(4, round, 2)) # round to 2 digits


########################

graph <- df_q1.3_data %>% ggplot() +

        geom_line(aes(x = date, y = value, color = label), alpha = 0.5,
                  size = 0.2) +

    #geom_line(aes(x = date, y = value, color = label), alpha = 0.8,
             # size = 1) +

    facet_wrap(~Continent, scales = "free_y") +

        labs(title = title,
             subtitle = subtitle,
             caption = caption,
             y = ylabel,
             x = xlabel) +

        theme() +
        theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
        theme(legend.position="bottom")

        #scale_x_discrete(guide = guide_axis(n.dodge=1.2))

    graph

}



