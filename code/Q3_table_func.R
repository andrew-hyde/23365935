
Q3_table_func <- function(df_data){
    library(xtable)
    # NB to remove xtable's comments as (https://stackoverflow.com/questions/24400308/how-to-remove-the-lines-in-xtable-table-output-by-knitr):
    options(xtable.comment = FALSE)

    library(tidyverse)
    library(xtable)
    library(lubridate)
    library(purrrlyr)

result <- df_tennis %>%
        mutate(`1st Serve Percentage In` = (w_1stIn/w_svpt)*100) %>% # create coloumn of 1st Serve Percentage in
        mutate(`1st Serve Percentage Won` = (w_1stWon/w_svpt)*100) %>% # create coloumn of 1st Serve Percentage in that was won
        mutate(date = ymd(tourney_date)) %>% # format date
        filter(date >= as.Date("1997-01-01") & date <= as.Date("2022-12-31")) %>%
        mutate(date = format(date, "%Y")) %>%  # format by year
    filter(tourney_level == c("G","M")) %>% # gram slam of masters events
        mutate(tourney_level = as.character(gsub("G","Grand Slam",as.character(tourney_level)))) %>% # substitute for full event name
        mutate(tourney_level = as.character(gsub("M","Masters 1000",as.character(tourney_level)))) %>% # substitute for full event name
        mutate(Aces = w_ace, Double_Faults = w_df, Height = winner_ht) %>% # full names
    group_by(date) %>% na.omit() %>%
        select(date,`1st Serve Percentage In`, `1st Serve Percentage Won`, Aces, Double_Faults, Height) %>%
        dmap(mean)
    #%>% # average each column
    #mutate_at(vars(c()), funs(round(., 2))) %>% # round off
    #arrange(desc(date))


t <- knitr::kable(result, digits = 2, caption = "Descriptive Statistics of Tennis Match Winners over the last 25 Years",
col.names = c("Date", "1st Serve In (%)", "1st Serve Won (%)", "Aces", "Double Faults", "Height"))
#     #add_header_above(c(" " = 2)) #%>%

t

}

