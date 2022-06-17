
graph_q2.2_func <- function(df_data, title, subtitle, caption, xlabel, ylabel){


library(tidyverse)
library(lubridate)


df_q2.2_data <- df_london %>%
    mutate(date = ymd(date)) %>%
    filter(date >= as.Date("2016-01-01") & date <= as.Date("2020-12-31")) %>%
    mutate(date = format(date, "%Y %m")) %>%
    select(date, sunshine, cloud_cover) %>%
    na.omit %>%
    #mutate(cloud_cover = as.numeric(gsub("NA","",as.character(cloud_cover)))) #%>% # remove pattern NA, by replacing pattern with '0'
    group_by(date) %>%
    summarise_at(c("sunshine", "cloud_cover"), sum)


##############


graph <- df_q2.2_data %>% ggplot() +

    geom_bar(aes(x = date, y = sunshine, fill = sunshine), stat = "identity", alpha = 0.8) +


    #scale_color_gradient(low="blue", high="red") +

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
    theme(legend.position="bottom")


# scale_color_npg() + # Now we use fill...
# scale_fill_npg() # Now we use fill...

graph

}



