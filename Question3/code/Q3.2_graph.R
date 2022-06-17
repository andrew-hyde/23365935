graph_q3.2_func <- function(df_data, title, subtitle, caption, xlabel, ylabel){


    library(tidyverse)
    library(lubridate)

    df_q3.2_data <- df_tennis %>%
        mutate(`1st Serve Percentage In` = (w_1stIn/w_svpt)*100) %>%
        mutate(`1st Serve Percentage Won` = (w_1stWon/w_svpt)*100) %>%
        mutate(date = ymd(tourney_date)) %>%
        filter(date >= as.Date("1999-01-01") & date <= as.Date("2022-12-31")) %>%
        filter(tourney_level == c("G","M")) %>% # gram slam of masters events
        mutate(tourney_level = as.character(gsub("G","Grand Slam",as.character(tourney_level)))) %>% # substitute for full event name
        mutate(tourney_level = as.character(gsub("M","Masters 1000",as.character(tourney_level)))) %>% # substitute for full event name
        select(date, tourney_level, `1st Serve Percentage In`) %>%
        tidyr::gather(label, value, -date, -tourney_level)

#############


graph <- df_q3.2_data %>% ggplot() +

        geom_density(aes(x = date, y = value, fill = tourney_level), stat = "identity") +
        #    geom_text(aes(x = number, y = value, label = value),  position = position_stack(vjust = 1)) +
        #scale_color_gradient(low="blue", high="red") +
        facet_wrap(~tourney_level, scales = "free_y", nrow = 2) +

         theme_bw() +

        # guides(color = FALSE, fill = FALSE, alpha = FALSE) +

        # Add titles:
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
