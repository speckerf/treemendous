## code to prepare `sysdata` dataset goes here

internal_datasets <- c("WFO.rds", "WCVP.rds", "GBIF.rds", "edges_fuzzy_matched.rds")

paths <- fs::path('data-raw', 'add_to_sysdata', list.files(path = 'data-raw/add_to_sysdata'))
# check that all datasets listed in internal_datasets are in the folder add_to_sysdata
assertthat::assert_that(all(sapply(internal_datasets, function(x) any(stringr::str_detect(paths, pattern = x)))))

WFO <- readRDS(fs::path('data-raw', 'add_to_sysdata', 'WFO.rds'))
WCVP <- readRDS(fs::path('data-raw', 'add_to_sysdata', 'WCVP.rds'))
GBIF <- readRDS(fs::path('data-raw', 'add_to_sysdata', 'GBIF.rds'))
edges_fuzzy_matched <- readRDS(fs::path('data-raw', 'add_to_sysdata', 'edges_fuzzy_matched.rds'))

usethis::use_data(WFO, WCVP, GBIF, edges_fuzzy_matched, overwrite = TRUE, internal = TRUE)
