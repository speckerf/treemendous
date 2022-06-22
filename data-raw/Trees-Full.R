## code to prepare `Trees.Full` dataset goes here

filename = "data-raw/cleaned_database_full.csv"
Trees.Full = readr::read_csv(filename)[2:28]
usethis::use_data(Trees.Full, overwrite = TRUE, compress = 'xz')
