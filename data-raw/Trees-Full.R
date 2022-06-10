## code to prepare `Trees.Full` dataset goes here

filename = "cleaned_database_full.csv"
tidyverse::Trees.File = read_csv(filename)
usethis::use_data(Trees.Full, overwrite = TRUE)
