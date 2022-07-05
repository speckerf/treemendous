## code to prepare `Trees.Full` dataset goes here

packages = c("dplyr", "stringr",
             "tidyr", "purrr",
             "V.PhyloMaker", "readr", "memoise")

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
  FIA <- load_FIA(paths)
  list_genera <- base::union(BGCI$Genus, FIA$Genus)
  return(list_genera)
}

get_tree_genera_list <- memoise::memoise(helper.get_tree_genera_list) ## remember output of tree genera list using memoise:  only needs to evaluate it once

load_BGCI <- function(paths){
  fieldnames = c('TaxonName', 'Author')
  df <- readr::read_csv(paths[['bgci']], col_select = all_of(fieldnames)) %>%
    tidyr::separate('TaxonName', sep = ' ', into = c('Genus', 'Species')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE) %>%
    tidyr::drop_na(Genus, Species) %>%
    dplyr::rename('BGCI_Authors' = 'Author')
  return(df)
}

load_FIA <- function(paths){
  fieldnames = c('Genus', 'Species', 'FIA Code')
  df <- readr::read_csv(paths[['fia']], col_select = all_of(fieldnames), col_types = "cci") %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE) %>%
    dplyr::rename('FIA_ID' = 'FIA Code')
  return(df)
}

load_WCVP <- function(paths){
  list_of_genera <- get_tree_genera_list(paths)
  fieldnames <- c('family', 'genus', 'species', 'kew_id', 'accepted_kew_id', 'accepted_authors', 'authors', 'rank', 'taxonomic_status')
  WCVP <- read_delim(paths[['wcvp']], delim = '|', col_select = all_of(fieldnames)) %>%
    dplyr::rename('WCVP_Family' = 'family',
                  'Genus' = 'genus',
                  'Species' = 'species',
                  'WCVP_ID' = 'kew_id',
                  'WCVP_accepted_ID' = 'accepted_kew_id',
                  'WCVP_accepted_Authors' = 'accepted_authors',
                  'WCVP_Authors' = 'authors',
                  'WCVP_Status' = 'taxonomic_status',
                  'Rank' = 'rank') %>%
    dplyr::filter(Rank == 'SPECIES') %>%
    tidyr::drop_na(c('Genus', 'Species'))

  WCVP_Accepted <- WCVP %>%
    dplyr::filter(Genus %in% list_of_genera &
                  WCVP_Status == 'Accepted') %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)

  WCVP_Synonyms <- WCVP %>%
    dplyr::semi_join(WCVP_Accepted, by = c('WCVP_accepted_ID' = 'WCVP_ID')) %>%
    dplyr::anti_join(WCVP_Accepted, by = c('Genus', 'Species')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)

  WCVP_merged <- WCVP_Accepted %>%
    dplyr::bind_rows(WCVP_Synonyms) %>%
    dplyr::arrange(Genus, Species) %>%
    dplyr::mutate(Rank = NULL)

  return(WCVP_merged)
}

load_WFO <- function(paths){
  list_of_genera <- get_tree_genera_list(paths)

  ## load dataset
  fieldnames <- c('family', 'genus', 'specificEpithet', 'taxonRank', 'taxonID', 'acceptedNameUsageID', 'taxonomicStatus', 'scientificNameAuthorship')
  WFO <- read_delim(paths[['wfo']], delim = '\t', col_select = all_of(fieldnames)) %>%
    dplyr::rename('WFO_Family' = 'family',
                  'Genus' = 'genus',
                  'Species' = 'specificEpithet',
                  'WFO_ID' = 'taxonID',
                  'WFO_accepted_ID' = 'acceptedNameUsageID',
                  'WFO_Authors' = 'scientificNameAuthorship',
                  'WFO_Status' = 'taxonomicStatus',
                  'Rank' = 'taxonRank') %>%
    dplyr::filter(Rank == 'SPECIES') %>%
    tidyr::drop_na(c('Genus', 'Species'))


  WFO_Accepted <- WFO %>%
    dplyr::filter(Genus %in% list_of_genera &
                    WFO_Status == 'ACCEPTED') %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)

  WFO_Synonyms <- WFO %>%
    dplyr::semi_join(WFO_Accepted, by = c('WFO_accepted_ID' = 'WFO_ID')) %>%
    dplyr::anti_join(WFO_Accepted, by = c('Genus', 'Species')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)

  WFO_merged <- WFO_Accepted %>%
    dplyr::bind_rows(WFO_Synonyms) %>%
    dplyr::arrange(Genus, Species) %>%
    dplyr::mutate(Rank = NULL)

  return(WFO_merged)
}

load_GBIF <- function(paths){
  list_of_genera <- get_tree_genera_list(paths)

  ## load dataset
  fieldnames <- c('specificEpithet', 'kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'taxonID', 'acceptedNameUsageID', 'taxonRank', 'taxonomicStatus', 'scientificNameAuthorship')
  GBIF <- read_delim(paths[['gbif']], delim = '\t', col_select = all_of(fieldnames)) %>%
    dplyr::rename('GBIF_Family' = 'family',
                  'Genus' = 'genus',
                  'Species' = 'specificEpithet',
                  'GBIF_ID' = 'taxonID',
                  'GBIF_accepted_ID' = 'acceptedNameUsageID',
                  'GBIF_Authors' = 'scientificNameAuthorship',
                  'GBIF_Status' = 'taxonomicStatus',
                  'Rank' = 'taxonRank') %>%
    dplyr::filter(kingdom == 'Plantae' &
                  phylum == 'Tracheophyta' &
                  class %in% c('Magnoliopsida', 'Pinopsida', 'Ginkgoopsida', 'Cycadopsida') &
                  Rank == 'species' ) %>%
    tidyr::drop_na(c('Genus', 'Species')) %>%
    dplyr::mutate(kingdom = NULL,
                  phylum = NULL,
                  class = NULL,
                  order = NULL)

  GBIF_Accepted <- GBIF %>%
    dplyr::filter(Genus %in% list_of_genera &
                    GBIF_Status == 'accepted') %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)

  GBIF_Synonyms <- GBIF %>%
    dplyr::semi_join(GBIF_Accepted, by = c('GBIF_accepted_ID' = 'GBIF_ID')) %>%
    dplyr::anti_join(GBIF_Accepted, by = c('Genus', 'Species')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)

  GBIF_merged <- GBIF_Accepted %>%
    dplyr::bind_rows(GBIF_Synonyms) %>%
    dplyr::arrange(Genus, Species) %>%
    dplyr::mutate(Rank = NULL)
  return(GBIF_merged)
}

load_PHYLOMAKER <- function(paths){
  list_of_genera <- get_tree_genera_list(paths)

  PM <- V.PhyloMaker::tips.info  %>% tidyr::as_tibble() %>%
    dplyr::select(species, genus, family) %>%
    tidyr::separate(species, into = c('Genus', 'Species'), sep = '_') %>%
    dplyr::filter(Genus == genus,
                  Genus %in% list_of_genera) %>%
    dplyr::mutate(genus = NULL) %>%
    dplyr::rename('PM_Family' = family) %>%
    tidyr::drop_na(c('Genus', 'Species')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE) %>%
    dplyr::arrange(Genus, Species)

    return(PM)
}

folder_raw_data <- '/Users/felixspecker/polybox/ETH_Polybox/CBB/FS22/Lab_Crowther/raw_data_treemendous/'
fia_path <- paste(folder_raw_data, 'fia_spp_list_FGver90.csv', sep = '')
bgci_path <- paste(folder_raw_data, 'BGCI_global_tree.csv', sep = '')
wcvp_path <- paste(folder_raw_data, 'WCVP.txt', sep = '')
wfo_path <- paste(folder_raw_data, 'classification.txt', sep = '')
gbif_path <- paste(folder_raw_data, 'Taxon.tsv', sep = '')
paths <- hash::hash("fia" = fia_path, "bgci" = bgci_path, "wcvp" = wcvp_path, "wfo" = wfo_path, "gbif" = gbif_path)

WFO <-  load_WFO(paths)
BGCI <-  load_BGCI(paths)
WCVP <-  load_WCVP(paths)
FIA <-  load_FIA(paths)
GBIF <-  load_GBIF(paths)
PM <-  load_PHYLOMAKER(paths)

## combine via outer join using Reduce on Genus and Species columns
names_dfs <- c('WCVP', 'FIA', 'GBIF', 'WFO', 'BGCI', 'PM')
dfs <- list(WCVP, FIA, GBIF, WFO, BGCI, PM)

dfs_indicator <- purrr::map2(dfs, names_dfs, function(.dfs, .names_dfs){
  dplyr::mutate(.dfs, !! .names_dfs := TRUE)
})

df_merged <- purrr::reduce(dfs_indicator, function(df1, df2) dplyr::full_join(df1, df2, by = c('Genus', 'Species'))) %>%
  dplyr::arrange('Genus', 'Species')

Trees.Full <- df_merged %>%
  dplyr::mutate_at(names_dfs, ~replace_na(.,0)) %>%
  dplyr::relocate('Genus', 'Species', 'BGCI', 'WFO', 'WCVP', 'GBIF', 'FIA', 'PM')

usethis::use_data(Trees.Full, overwrite = TRUE, compress = 'xz')
