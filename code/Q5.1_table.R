
Q5_table_func <- function(df_data){

    library(tidyverse)
    library(knitr)
    library(kableExtra)
    library(lubridate)


options(scipen=999) # turn off scientific notation of Installs column

df_q5.2_data <- df_google %>% select(Category, Price, Installs, Size) %>%
    na.omit() %>%
    filter(Price != "0") %>% # Filter for apps that charge money
    mutate(Size = as.numeric(gsub("[M]","", Size))) %>% # remove characters
    mutate(Installs = as.numeric(gsub("[+ ,]","", Installs))) %>% # remove characters
    mutate(Installs = as.numeric(Installs)) %>%
    mutate(Price = as.numeric(gsub("[$]","", Price))) %>% # remove $ by subsituting it for nothing
    mutate(Revenue = Price*Installs) %>%
    arrange(Revenue, .by_group = F) %>%
    group_by(Category) %>%
    #summarise_at(vars(c(Price, Installs, Revenue, Size)), mean)


df_q5.2_data %>% kbl(align = "c", caption = "Summary of Mean Application Statistics per Catergory",
                     col.names = c("Category", "Price ($)", "Installs", "Revenue  ($)", "Size")) %>%
    kable_styling(latex_options = "striped")
#add_header_above(c(" " = 2)) #%>%

}
