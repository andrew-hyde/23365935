table_func <- function(df_data){

library(tidyverse)
library(knitr)
library(kableExtra)
library(lubridate)


# TABLE
q2_table_df <- df_london %>% mutate(date = ymd(date)) %>%
    filter(date >= as.Date("2011-01-01") & date <= as.Date("2020-12-31")) %>%
    mutate(date = format(date, "%Y")) %>%
    select(date, sunshine, cloud_cover) %>%
    na.omit %>%
    group_by(date) %>%
    summarise_at(c("sunshine"), sum)


q2_table_df %>% kbl(align = "c", caption = "Descriptive Statistics", col.names = c("Dates", "Mean")) %>%
    kable_styling(latex_options = "striped") %>%
    add_header_above(c(" " = 2)) #%>%


}
