
graph_q5.1_func <- function(df_data, title, subtitle, caption, xlabel, ylabel){

    library(tidyverse)
    library(lubridate)

# CLEANING THIS WAS TOUGH
options(scipen=999) # TRICK - turn off scientific notation of Installs column

df_q5.1_data <- df_google %>% select(Category, Price, Installs) %>%
    na.omit() %>%
    filter(Price != "0") %>% # Filter for apps that charge money
    mutate(Installs = as.numeric(gsub("[+ ,]","", Installs))) %>% # remove characters
    mutate(Installs = as.numeric(Installs)) %>%
    mutate(Price = as.numeric(gsub("[$]","", Price))) %>% # remove $ by subsituting it for nothing
    mutate(Revenue = Price*Installs) %>%
    arrange(Revenue, .by_group = F) %>%
    select(Category, Price, Installs, Revenue) %>%
    group_by(Category) %>%
    dmap(mean)

# plot_orderset <- function(df, Column, Order){
#
#     df[,Column][[1]] <- factor(df[,Column][[1]], levels = Order)
#
#     df
#
# }
#
# order <- df_plot %>% arrange(Installs) %>% pull(Category)
# dfplot <- df_plot %>% plot_orderset(., Column = "Category", Order = order)

graph <- df_q5.1_data %>% ggplot() +
    geom_bar(aes(x = Category , y = Revenue,  fill = Category), stat = "identity") +
    #geom_text(aes(x = Category, y = Revenue, label = Installs), vjust = 0.1) +

theme_bw() +

    # Add titles:
    labs(title = title,
         subtitle = subtitle,
         caption = caption,
         x = xlabel,
         y = ylabel) +

    theme() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(legend.position="none")

graph

}


