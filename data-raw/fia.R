## code to prepare `FIA_cleaned` dataset goes here
fia_file_path <- "data-raw/fia_masterlist_v91_2021.csv"

fia_raw <- readr::read_csv(fia_file_path) %>%
  dplyr::select(Genus, Species)

to_exclude <- fia_raw %>%
  dplyr::filter(stringr::str_detect(Species, 'spp.') |
                stringr::str_detect(Species, '^x ') |
                stringr::str_detect(Species, 'ssp.') |
                stringr::str_detect(Species, 'broadleaf') |
                stringr::str_detect(Species, 'evergreen') |
                stringr::str_detect(Species, 'unknown'))

fia <- fia_raw %>% dplyr::anti_join(to_exclude) %>%
  dplyr::mutate(Species = stringr::str_to_lower(Species),
                Genus = stringr::str_to_sentence(Genus)) %>%
  dplyr::distinct(Genus, Species)


usethis::use_data(fia, overwrite = TRUE)
