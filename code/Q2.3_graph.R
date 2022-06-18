
graph_q2.3_func <- function(df_data, title, subtitle, caption, xlabel, ylabel){

    # for the count_if function
    library(expss)

    library(tidyverse)
    library(lubridate)


    df_q2.3_data <- df_london %>%
        mutate(date = ymd(date)) %>%
        filter(date >= as.Date("2020-01-01") & date <= as.Date("2020-12-31")) %>%
        mutate(date = format(date, "%Y %m %d")) %>%
        mutate(`Days with Rain` = ifelse(precipitation!=0.0, 1, 0)) %>% # dummy variables for it is rained that day
        mutate(`Days with no Rain` = ifelse(precipitation==0.0, 1, 0)) %>% # dummy variables for it is rained that day
        select(date, `Days with Rain`, `Days with no Rain`) %>%
        na.omit() %>%
        summarise_at(c("Days with Rain", "Days with no Rain"), sum) %>%
        mutate(`Days with Rain` = (`Days with Rain`/362)*100) %>% # only 362 observations of 2020
        mutate(`Days with no Rain` = (`Days with no Rain`/362)*100) %>%
        tidyr::gather(label, value) %>%
        head(2) %>%
        mutate(across(2, round, 2))


    ##############


graph <- df_q2.3_data %>% ggplot() +

        geom_bar(aes(x="", y=value, fill=label), stat="identity", width=1, color="white") +
        coord_polar("y") +
        geom_text(aes(x ="", y = value, label = value), color = "black", size=4, position = position_stack(vjust = 1)) +

        theme_bw() +

        # Add titles:
        labs(title = title,
             subtitle = subtitle,
             caption = caption,
             x = xlabel,
             y = ylabel) +

        theme() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme(legend.position="right") +
        theme(legend.title=element_blank()) +
        theme(axis.text.x=element_blank())


    graph

}





