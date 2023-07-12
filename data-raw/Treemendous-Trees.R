## code to prepare `Treemendous.Trees` dataset goes here
packages = c("dplyr", "stringr",
             "tidyr", "purrr",
             "readr", "memoise")

devtools::load_all()

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

helper.get_tree_genera_list <- function(paths){
  BGCI <- load_BGCI(paths)
  list_genera <- base::unique(BGCI$Genus)
  return(list_genera)
}

load_BGCI <- function(paths){
  fieldnames = c('TaxonName', 'Author')
  df <- readr::read_csv(paths[['bgci']], col_select = all_of(fieldnames)) %>%
    tidyr::separate('TaxonName', sep = ' ', into = c('Genus', 'Species')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE) %>%
    tidyr::drop_na(Genus, Species) %>%
    dplyr::rename('BGCI_Authors' = 'Author')
  return(df)
}

## !! if backbones are updated: remember to update Treemendous.Trees documentation in R/data.R as well!!
paths <- yaml::read_yaml("data-raw/paths.yml")

get_tree_genera_list <- memoise::memoise(helper.get_tree_genera_list) ## remember output of tree genera list using memoise:  only needs to evaluate it once

# load precomputed GBIF, WFO, WCVP dataset
# WFO <-  WFO already visible because we loaded the package with devtools::load_all()
# data(WFO) are available already after calling devtools::load_all()
BGCI <-  load_BGCI(paths)
# WCVP <- WCVP
#data(WCVP)
# GBIF <-  GBIF
#data(GBIF)

## combine via outer join using Reduce on Genus and Species columns
names_dfs <- c('WCVP', 'GBIF', 'WFO', 'BGCI')

if(fs::dir_exists(fs::path('data-raw', 'add_to_sysdata'))){
  WCVP <- readRDS(file = fs::path('data-raw', 'add_to_sysdata', 'WCVP.rds'))
  WFO <- readRDS(file = fs::path('data-raw', 'add_to_sysdata', 'WFO.rds'))
  GBIF <- readRDS(file = fs::path('data-raw', 'add_to_sysdata', 'GBIF.rds'))
  } else if (exists('WFO') & exists('WCVP') & exists('GBIF')) {
    message("WFO, GBIF, GBIF should be first created in the folder add_too_sysdata and loaded from there. ")
  } else{
  stop("WFO, GBIF, and WCVP were not found, not in sysdata.rds and also not in data-raw/add_to_s")
}


dfs <- list(WCVP, GBIF, WFO, BGCI)

dfs_indicator <- purrr::map2(dfs, names_dfs, function(.dfs, .names_dfs){
  dplyr::mutate(.dfs, !! .names_dfs := TRUE)
})

df_merged <- purrr::reduce(dfs_indicator, function(df1, df2) dplyr::full_join(df1, df2, by = c('Genus', 'Species')))

treemendous_database <- df_merged %>%
  dplyr::mutate_at(names_dfs, ~replace_na(.,0)) %>%
  dplyr::arrange(Genus, Species) %>%
  dplyr::mutate(ID_merged = dplyr::row_number()) %>%
  dplyr::relocate('Genus', 'Species', 'BGCI', 'WFO', 'WCVP', 'GBIF', 'ID_merged')

### Tidy-up treemendous_database to match the input constraints the package is posing.

Treemendous.Trees <- treemendous_database %>%
  dplyr::mutate(Genus = stringr::str_remove(Genus, '\u00D7')) %>% # remove hybrid characters in genus name
  dplyr::mutate(Genus = stringr::str_to_sentence(Genus)) %>%
  dplyr::mutate(Species = stringr::str_to_lower(Species)) %>%
  dplyr::distinct(Genus, Species, .keep_all = TRUE) ## we loose 11 species here: but it is necessary to avoid having duplicate species in get_testset() function


usethis::use_data(Treemendous.Trees, overwrite = TRUE, compress = 'xz')

