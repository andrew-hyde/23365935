
graph_q2.1_jitter_boxplot <- function(df_data, title, subtitle, caption, xlabel, ylabel){

    library(tidyverse)
    library(lubridate)

    df_q2.1_data <- df_london %>%
        mutate(date = ymd(date)) %>%
        # mutate(date = as.Date(date,format = "%Y-%m")) %>%
        mutate(date = format(date, "%Y")) %>%
        select(date, max_temp, min_temp, mean_temp) %>% na.omit() %>%
        gather(label, value, -date)


    # group_by(`Lead Studio`) %>% # group all movies according to the studio they belong to
    # summarise_at(vars(`Profitability`), ~mean(.)) %>% #
    # mutate(`Profitability` = round(`Profitability`, digits = 1)) %>%
    # arrange(desc(`Profitability`))

graph <- df_q2.1_data %>% ggplot() +

    geom_jitter(aes(x = date, y = `value`, color = label), alpha = 1) +
        geom_boxplot(aes(x = date, y = `value`), alpha = 0.2) +


        theme_bw() +

        # guides(color = FALSE, fill = FALSE, alpha = FALSE) +

        # Add titles:
        labs(title = title,
             subtitle = subtitle,
             caption = caption,
             x = xlabel,
             y = ylabel) +

        theme() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme(legend.position="bottom") +
    theme(legend.title=element_blank())


    # scale_color_npg() + # Now we use fill...
    # scale_fill_npg() # Now we use fill...

    graph

}

