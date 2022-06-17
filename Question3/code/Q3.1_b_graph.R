

graph_q3.1b_func <- function(df_data, title, subtitle, caption, xlabel, ylabel){

library(tidyverse)
library(lubridate)

df_q3.1b_data <- df_tennis %>%
    mutate(date = ymd(tourney_date)) %>%
    filter(date >= as.Date("1999-01-01") & date <= as.Date("2022-12-31")) %>%
    #mutate(winner = grep("Novak Djokovic|Rafael Nadal", ignore.case=TRUE, df_q3.1_data$winner_name)) %>%
    filter(winner_name == "Rafael Nadal") %>%
    filter(round == "F") %>%
    filter(tourney_level == "G") %>% #filter for grand slam by number of sets
    mutate(number = (1:20)) %>%
    mutate(Aces = w_ace, Double_Faults = w_df) %>%
    select(number, Aces, Double_Faults) %>%
    tidyr::gather(label, value, -number)


# select(date, sunshine, cloud_cover) %>%
# na.omit %>%
# #mutate(cloud_cover = as.numeric(gsub("NA","",as.character(cloud_cover)))) #%>% # remove pattern NA, by replacing pattern with '0'
# group_by(date) %>%
# summarise_at(c("sunshine", "cloud_cover"), sum)


##############


graph <- df_q3.1b_data %>% ggplot() +

    geom_bar(aes(x = number, y = value, fill = label), stat = "identity", alpha = 2, position='dodge') +
    #    geom_text(aes(x = number, y = value, label = value),  position = position_stack(vjust = 1)) +
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
    theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
    theme(legend.position="bottom")

graph

}


