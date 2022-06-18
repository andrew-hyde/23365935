
Q5_table_func <- function(df_data){

    options(xtable.comment = FALSE)
    options(knitr.kable.NA = "")

    library(tidyverse)
    library(knitr)
    library(kableExtra)
    library(lubridate)
    library(purrrlyr)


options(scipen=999) # turn off scientific notation of Installs column

result <- df_google %>% select(Category, Price, Installs, Size) %>%
    na.omit() %>%
    filter(Price != "0") %>% # Filter for apps that charge money
    mutate(Size = suppressWarnings(as.numeric(gsub("[M]","", Size)))) %>% # remove characters
    mutate(Installs = as.numeric(gsub("[+ ,]","", Installs))) %>% # remove characters
    mutate(Installs = as.numeric(Installs)) %>%
    mutate(Price = as.numeric(gsub("[$]","", Price))) %>% # remove $ by subsituting it for nothing
    mutate(Revenue = Price*Installs) %>%
    arrange(Revenue, .by_group = F) %>%
    group_by(Category) %>% dmap(mean)


t <- knitr::kable(result, digits = 2, caption = "Summary of Mean Application Statistics per Catergory",
                     col.names = c("Category", "Price ($)", "Installs", "Revenue  ($)", "Size"))
#add_header_above(c(" " = 2)) #%>%

t

}
