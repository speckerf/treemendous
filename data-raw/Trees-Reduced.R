## code to prepare `Trees.Reduced` dataset goes here

filename = "cleaned_database_reduced.csv"
Trees.Reduced = tidyverse::read_csv(filename)
usethis::use_data(Trees.Reduced, overwrite = TRUE)
