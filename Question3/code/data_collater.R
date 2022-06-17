
# Write a function to collate this data into a single data frame.

# function should also suppress the ugly read csv messages.

data_collater <- function(data_directory) {

    setwd("C:/Masters Economics/Semester 1/Data Science/Practical2022/23365935/Question3/data/Tennis")

    library(data.table)
    library(tidyverse)

    data <- list.files(pattern = ".csv", ) %>%  #list all file in folder ending with ".csv"
        lapply(., fread, sep=",") %>% #function reads data and separates where a comma is present
        rbindlist(.) #only files with the same number of columns will bind

    data
}




# Data_Collating <- function(Datroot){
#
#     library(tidyverse)
#
#     # let's create a silent read function first (as it prints a load of nonsense if you use read_csv directly):
#     silentread <- function(x){
#         hushread <- purrr::quietly(read_csv)
#         df <- hushread(x)
#         df$result
#     }
#
#     datcolat <-
#         list.files(Datroot, full.names = T, recursive = T) %>%
#         # Ensure you only load the csv's, not the README.txt.
#         .[grepl("atp_matches_20|atp_matches_19", .)] %>%
#         as.list() %>%
#         map(~silentread(.)) %>%
#         bind_rows()
#     # equivalent to using map_df
#
#     datcolat
#     #"amateur|chall|doubles|futures|rankings|dictionary|players"
# }
#
# atp_matches_three_decades <- Data_Collating(Datroot = "C:/Masters Economics/Semester 1/Data Science/Practical2022/23365935/Question3/data/tennis-cut/")
#
#
# list_of_files <- list.files(path = "Question3/data/Tennis/",
#                             recursive = TRUE,
#                             pattern = "matches\\.csv",
#                             full.names = F)
#
# df <- readr::read_csv(list_of_files, id = "Tennis")
#
#
# library(data.table)
#
# tbl_fread <-
#     list.files(pattern = "*.csv") %>%
#     map_df(fread(.))
