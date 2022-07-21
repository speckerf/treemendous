## code to prepare `iucn` dataset goes here
iucn_file_path <- "data-raw/iucn.csv"

iucn <- readr::read_csv(iucn_file_path) %>%
  dplyr::select(Genus, Species) %>%
  dplyr::rename('Orig.Genus' = 'Genus',
                'Orig.Species' = 'Species')
usethis::use_data(iucn, overwrite = TRUE)
