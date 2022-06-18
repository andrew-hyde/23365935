
Q3_table_func <- function(df_data){

    library(tidyverse)
    library(knitr)
    library(kableExtra)
    library(lubridate)

df_q3.3_data <- df_tennis %>%
        mutate(`1st Serve Percentage In` = (w_1stIn/w_svpt)*100) %>% # create coloumn of 1st Serve Percentage in
        mutate(`1st Serve Percentage Won` = (w_1stWon/w_svpt)*100) %>% # create coloumn of 1st Serve Percentage in that was won
        mutate(date = ymd(tourney_date)) %>% # format date
        filter(date >= as.Date("1997-01-01") & date <= as.Date("2022-12-31")) %>%
        mutate(date = format(date, "%Y")) %>%  # format by year
    filter(tourney_level == c("G","M")) %>% # gram slam of masters events
        mutate(tourney_level = as.character(gsub("G","Grand Slam",as.character(tourney_level)))) %>% # substitute for full event name
        mutate(tourney_level = as.character(gsub("M","Masters 1000",as.character(tourney_level)))) %>% # substitute for full event name
        mutate(Aces = w_ace, Double_Faults = w_df) %>% # full names
    group_by(date) %>% na.omit() %>%
        select(date,`1st Serve Percentage In`, `1st Serve Percentage Won`, Aces, Double_Faults, winner_ht) %>%
        summarise(across(everything(), mean)) %>% # average each column
    mutate_at(vars(-date), funs(round(., 2))) %>% # round off
    arrange(desc(date))


df_q3.3_data %>% kbl(align = "c", caption = "Descriptive Statistics of Tennis Match Winners over the last 25 Years",
                               col.names = c("Date", "1st Serve In (%)", "1st Serve Won (%)", "Aces", "Double Faults", "Height")) %>%
    kable_styling(latex_options = "striped")
    #add_header_above(c(" " = 2)) #%>%



}

