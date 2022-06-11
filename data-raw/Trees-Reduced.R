## code to prepare `Trees.Reduced` dataset goes here

filename = "data-raw/cleaned_database_reduced.csv"
Trees.Reduced = readr::read_csv(filename)[2:11]
usethis::use_data(Trees.Reduced, overwrite = TRUE)
