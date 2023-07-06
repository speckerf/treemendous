## code to prepare `Treemendous.Trees` dataset goes here
packages = c("dplyr", "stringr",
             "tidyr", "purrr",
             "readr", "memoise")

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

load_WCVP <- function(paths){
  list_of_genera <- get_tree_genera_list(paths)
  fieldnames <- c('family', 'genus', 'kew_id', 'accepted_kew_id', 'authors', 'rank', 'taxonomic_status', 'taxon_name')
  WCVP <- read_delim(paths[['wcvp']], delim = '|', col_select = dplyr::all_of(fieldnames)) %>%
    dplyr::rename('WCVP_Family' = 'family',
                  'Genus' = 'genus',
                  'Species' = 'taxon_name',
                  'WCVP_ID' = 'kew_id',
                  'WCVP_accepted_ID' = 'accepted_kew_id',
                  'WCVP_Authors' = 'authors',
                  'WCVP_Status' = 'taxonomic_status',
                  'WCVP_Rank' = 'rank') %>%
    dplyr::mutate(Species = stringr::str_remove(Species, pattern = "^([A-Z])\\w+\\s?")) %>% # get all except Genus from taxon_name. ^\S+\h*(.*$). ".*?\\s"
    dplyr::filter(WCVP_Rank != 'GENUS') %>%
    dplyr::mutate(WCVP_Rank = stringr::str_to_sentence(WCVP_Rank)) %>%
    dplyr::filter(WCVP_Rank %in% c('Form', 'Species', 'Subspecies', 'Variety')) %>%
    tidyr::drop_na(c('Genus', 'Species'))

  # move everythin after f. | var. | subsp. into WCVP_Infraspecific
  WCVP <- WCVP %>%
    dplyr::mutate(WCVP_Infraspecific = stringr::str_extract(Species, "(.+)(\\sf\\.|var\\.|subsp\\.\\s)(.+)", group = 3)) %>%
    dplyr::mutate(WCVP_Species = stringr::str_remove(Species, "\\s.*")) %>%
  dplyr::mutate(Species = NULL) %>%
    dplyr::rename('Species' = 'WCVP_Species')

  ## Flag all the Species with potential authorship conflicts
  WCVP_conflicting_authorship <- WCVP %>% group_by(Genus, Species) %>% filter(n()>1) %>% filter(n_distinct(WCVP_Authors) > 1) %>% ungroup()
  WCVP_no_conflict <- WCVP %>% dplyr::anti_join(WCVP_conflicting_authorship)
  WCVP <- dplyr::bind_rows(dplyr::mutate(WCVP_conflicting_authorship, 'WCVP_Authors_Conflict' = TRUE), dplyr::mutate(WCVP_no_conflict, 'WCVP_Authors_Conflict' = FALSE)) %>%
    dplyr::arrange(Genus, Species)


  WCVP_Accepted <- WCVP %>%
    dplyr::filter(Genus %in% list_of_genera &
                    WCVP_Status %in% c('Accepted')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)

  WCVP_Synonyms <- WCVP %>%
    dplyr::filter(Genus %in% list_of_genera &
                    WCVP_Status %in% c('Synonym', 'Homotypic_Synonym'),
                  !is.na(WCVP_accepted_ID)) %>% # exclude all the synonyms which do not contain any information on the corresponding accepted species.
    dplyr::anti_join(WCVP_Accepted, by = c('Genus', 'Species')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)
  assertthat::assert_that(!any(is.na(WCVP_Synonyms$WCVP_accepted_ID)))

  WCVP_Accepted_Synonyms <- dplyr::bind_rows(WCVP_Accepted, WCVP_Synonyms)
  assertthat::assert_that(nrow(dplyr::distinct(WCVP_Accepted_Synonyms, Genus, Species)) == nrow(WCVP_Accepted) + nrow(WCVP_Synonyms))

  WCVP_Outgoing_Edges <- WCVP %>%
    dplyr::semi_join(WCVP_Accepted_Synonyms, by = c('WCVP_ID' = 'WCVP_accepted_ID')) %>%
    dplyr::anti_join(WCVP_Accepted_Synonyms, by = c('Genus', 'Species')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)
  assertthat::assert_that(all(is.na(WCVP_Outgoing_Edges$WCVP_accepted_ID)))

  WCVP_Incoming_Edges <- WCVP %>%
    dplyr::semi_join(WCVP_Accepted_Synonyms, by = c('WCVP_accepted_ID' = 'WCVP_ID')) %>%
    dplyr::anti_join(WCVP_Accepted_Synonyms, by = c('Genus', 'Species')) %>%
    dplyr::anti_join(WCVP_Outgoing_Edges, by = c('Genus', 'Species')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)
  assertthat::assert_that(!any(is.na(WCVP_Incoming_Edges$WCVP_accepted_ID)))

  ## Solve issue where outgoing species are not of Rank Species: see comments in load_WFO for more details
  ## comment out for version where varieties and subspecies are allowed as well
  WCVP_Synonyms_where_Accepted_not_found <- WCVP_Synonyms %>%
    dplyr::filter(!is.na(WCVP_accepted_ID)) %>%
    dplyr::anti_join(WCVP_Accepted_Synonyms, by = c('WCVP_accepted_ID' = 'WCVP_ID')) %>%
    dplyr::anti_join(WCVP_Outgoing_Edges, by = c('WCVP_accepted_ID' = 'WCVP_ID'))

  WCVP_Accepted_Synonyms_without_Conflict <- WCVP_Accepted_Synonyms %>%
    dplyr::anti_join(WCVP_Synonyms_where_Accepted_not_found, by = c('Genus', 'Species'))

  WCVP_merged <- WCVP_Accepted_Synonyms_without_Conflict %>%
    dplyr::bind_rows(WCVP_Outgoing_Edges, WCVP_Incoming_Edges) %>%
    dplyr::distinct(Genus, Species, .keep_all = T) %>%
    dplyr::arrange(Genus, Species)

  nrow_conflicting <- WCVP_merged %>% filter(!is.na(WCVP_accepted_ID)) %>% dplyr::anti_join(WCVP_merged, by = c('WCVP_accepted_ID' = 'WCVP_ID')) %>% nrow()
  if(nrow_conflicting > 0){
    warning(paste('The WCVP database is not self-contained, meaning that there are synonyms for which the accepted species is not in the database! In total, there are', nrow_conflicting, 'conflicting species.'))
  }
  return(WCVP_merged)
}

load_WFO <- function(paths){
  list_of_genera <- get_tree_genera_list(paths)

  ## load dataset
  fieldnames <- c('family', 'genus', 'specificEpithet', 'infraspecificEpithet', 'taxonRank', 'taxonID', 'acceptedNameUsageID', 'taxonomicStatus', 'scientificNameAuthorship')
  WFO <- read_delim(paths[['wfo']], delim = '\t', col_select = all_of(fieldnames)) %>%
    dplyr::rename('WFO_Family' = 'family',
                  'Genus' = 'genus',
                  'Species' = 'specificEpithet',
                  'WFO_ID' = 'taxonID',
                  'WFO_accepted_ID' = 'acceptedNameUsageID',
                  'WFO_Authors' = 'scientificNameAuthorship',
                  'WFO_Status' = 'taxonomicStatus',
                  'WFO_Rank' = 'taxonRank',
                  'WFO_Infraspecific' = 'infraspecificEpithet') %>%
    dplyr::mutate(WFO_Rank = stringr::str_to_sentence(WFO_Rank),
                  WFO_Status = stringr::str_to_sentence(WFO_Status)) %>%
    dplyr::filter(WFO_Rank %in% c('Species', 'Variety', 'Subspecies', 'Form')) %>%
    tidyr::drop_na(c('Genus', 'Species'))

  ## add a flag for possible author conflicts
  WFO_conflicting_authorship <- WFO %>% group_by(Genus, Species, WFO_Infraspecific) %>% filter(n()>1) %>% filter(n_distinct(WFO_Authors) > 1) %>% ungroup() %>% arrange(Genus, Species)
  WFO_no_conflict <- WFO %>% dplyr::anti_join(WFO_conflicting_authorship)
  WFO <- dplyr::bind_rows(dplyr::mutate(WFO_conflicting_authorship, 'WFO_Authors_Conflict' = TRUE), dplyr::mutate(WFO_no_conflict, 'WFO_Authors_Conflict' = FALSE)) %>%
    dplyr::arrange(Genus, Species)


  WFO_Accepted <- WFO %>%
    dplyr::filter(Genus %in% list_of_genera &
                    WFO_Status %in% c('Accepted')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)

  WFO_Synonyms <- WFO %>%
    dplyr::filter(Genus %in% list_of_genera &
                    WFO_Status %in% c('Synonym', 'Heterotypicsynonym', 'Homotypicsynonym')) %>%
    dplyr::anti_join(WFO_Accepted, by = c('Genus', 'Species')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)

  WFO_Accepted_Synonyms <- dplyr::bind_rows(WFO_Accepted, WFO_Synonyms)
  assertthat::assert_that(nrow(dplyr::distinct(WFO_Accepted_Synonyms, Genus, Species)) == nrow(WFO_Accepted) + nrow(WFO_Synonyms))

  WFO_Outgoing_Edges <- WFO %>%
    dplyr::semi_join(WFO_Accepted_Synonyms, by = c('WFO_ID' = 'WFO_accepted_ID')) %>%
    dplyr::anti_join(WFO_Accepted_Synonyms, by = c('Genus', 'Species')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)


  if(!all(is.na(WFO_Outgoing_Edges$WFO_accepted_ID))){
    num = sum(!is.na(WFO_Outgoing_Edges$WFO_accepted_ID))
    warning(sprintf("For an unknow reason there were %i conflicting entries in WFO_Outgoing_Edges. They got excluded. ", num))
    WFO_Outgoing_Edges <- WFO_Outgoing_Edges %>%
      filter(is.na(WFO_accepted_ID))
  }
  assertthat::assert_that(all(is.na(WFO_Outgoing_Edges$WFO_accepted_ID)))

  WFO_Incoming_Edges <- WFO %>%
    dplyr::semi_join(WFO_Accepted_Synonyms, by = c('WFO_accepted_ID' = 'WFO_ID')) %>%
    dplyr::anti_join(WFO_Accepted_Synonyms, by = c('Genus', 'Species')) %>%
    dplyr::anti_join(WFO_Outgoing_Edges, by = c('Genus', 'Species')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)

  assertthat::assert_that(!any(is.na(WFO_Incoming_Edges$WFO_accepted_ID))) ## all incoming edges need WFO_Accepted_ID column


  ## There are synonyms in WFO_Accepted_Synonyms where the accepted species is not present. Likely because it had rank 'SUBSPECIES', 'FORM', or 'VARIETY' and is therefore not present in WFO
  # We remove these to maintain a self-contained database
  # Alternatively, one could iteratively try to resolve these issues, by connecting these species with SPECIES equivalent of the other forms. This would involve changing the WFO_Accepted_ID and link the to the SPECIES equivalents.
  # For simplicity, however, we exclude the conflicting synonyms.
  WFO_Synonyms_where_Accepted_not_found <- WFO_Accepted_Synonyms %>%
    dplyr::filter(!is.na(WFO_accepted_ID)) %>%
    dplyr::anti_join(WFO_Accepted_Synonyms, by = c('WFO_accepted_ID' = 'WFO_ID')) %>%
    dplyr::anti_join(WFO_Outgoing_Edges, by = c('WFO_accepted_ID' = 'WFO_ID'))


  WFO_Accepted_Synonyms_without_Conflict <- WFO_Accepted_Synonyms %>%
    dplyr::anti_join(WFO_Synonyms_where_Accepted_not_found, by = c('Genus', 'Species'))

  WFO_merged <- WFO_Accepted_Synonyms_without_Conflict %>%
    dplyr::bind_rows(WFO_Outgoing_Edges, WFO_Incoming_Edges) %>%
    dplyr::distinct(Genus, Species, .keep_all = T) %>%
    dplyr::arrange(Genus, Species) %>%
    dplyr::mutate(WFO_Rank = NULL)

  nrow_conflicting <- WFO_merged %>% filter(!is.na(WFO_accepted_ID)) %>% dplyr::anti_join(WFO_merged, by = c('WFO_accepted_ID' = 'WFO_ID')) %>% nrow()
  if(nrow_conflicting > 0){
    warning(paste('The WFO database is not self-contained, meaning that there are synonyms for which the accepted species is not in the database! In total, there are', nrow_conflicting, 'conflicting species.'))
  }

  return(WFO_merged)
}

load_GBIF <- function(paths){
  list_of_genera <- get_tree_genera_list(paths)

  ## load dataset
  fieldnames <- c('specificEpithet', 'kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'taxonID', 'acceptedNameUsageID', 'taxonRank', 'taxonomicStatus', 'scientificNameAuthorship', 'infraspecificEpithet')
  GBIF <- read_delim(paths[['gbif']], delim = '\t', col_select = all_of(fieldnames)) %>%
    dplyr::rename('GBIF_Family' = 'family',
                  'Genus' = 'genus',
                  'Species' = 'specificEpithet',
                  'GBIF_Infraspecific' = 'infraspecificEpithet',
                  'GBIF_ID' = 'taxonID',
                  'GBIF_accepted_ID' = 'acceptedNameUsageID',
                  'GBIF_Authors' = 'scientificNameAuthorship',
                  'GBIF_Status' = 'taxonomicStatus',
                  'GBIF_Rank' = 'taxonRank') %>%
    dplyr::filter(kingdom == 'Plantae' &
                    phylum == 'Tracheophyta' &
                    class %in% c('Magnoliopsida', 'Pinopsida', 'Ginkgoopsida', 'Cycadopsida')) %>%
    tidyr::drop_na(c('Genus', 'Species')) %>%
    dplyr::mutate(GBIF_Rank = stringr::str_to_sentence(GBIF_Rank),
                  GBIF_Status = stringr::str_to_sentence(GBIF_Status)) %>%
    dplyr::filter(GBIF_Rank %in% c('Form', 'Species', 'Subspecies', 'Variety')) %>% # entries with following ranks are discared: unranked (1221) (no unranked are considered accepted)
    dplyr::mutate(kingdom = NULL,
                  phylum = NULL,
                  class = NULL,
                  order = NULL)

  ## Flag all the Species with potential authorship conflicts
  # add n_distinct(GBIF_Authors) > 1, because some entries have more than one entry for identical the same Genus, Species, Author combination. They would appear in n()>1 but not represent real author conflicts.
  GBIF_conflicting_authorship <- GBIF %>% group_by(Genus, Species) %>% filter(n()>1) %>% filter(n_distinct(GBIF_Authors) > 1) %>% ungroup() %>% arrange(Genus, Species)
  GBIF_no_conflict <- GBIF %>% dplyr::anti_join(GBIF_conflicting_authorship)
  GBIF <- dplyr::bind_rows(dplyr::mutate(GBIF_conflicting_authorship, 'GBIF_Authors_Conflict' = TRUE), dplyr::mutate(GBIF_no_conflict, 'GBIF_Authors_Conflict' = FALSE)) %>%
    dplyr::arrange(Genus, Species)

  GBIF_Accepted <- GBIF %>%
    dplyr::filter(Genus %in% list_of_genera &
                    GBIF_Status == 'Accepted') %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)

  # it is safe to discard the with status doubtful here, because they bear are not associated with information about their accepted species.
  GBIF_Synonyms <- GBIF %>%
    dplyr::filter(Genus %in% list_of_genera &
                    GBIF_Status %in% c('Synonym', 'Heterotypic synonym', 'Homotypic synonym')) %>%
    dplyr::anti_join(GBIF_Accepted, by = c('Genus', 'Species')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)

  GBIF_Accepted_Synonyms <- dplyr::bind_rows(GBIF_Accepted, GBIF_Synonyms)
  assertthat::assert_that(nrow(dplyr::distinct(GBIF_Accepted_Synonyms, Genus, Species)) == nrow(GBIF_Accepted) + nrow(GBIF_Synonyms))

  GBIF_Outgoing_Edges <- GBIF %>%
    dplyr::semi_join(GBIF_Accepted_Synonyms, by = c('GBIF_ID' = 'GBIF_accepted_ID')) %>%
    dplyr::anti_join(GBIF_Accepted_Synonyms, by = c('Genus', 'Species')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)

  assertthat::assert_that(all(is.na(GBIF_Outgoing_Edges$GBIF_accepted_ID)))

  GBIF_Incoming_Edges <- GBIF %>%
    dplyr::semi_join(GBIF_Accepted_Synonyms, by = c('GBIF_accepted_ID' = 'GBIF_ID')) %>%
    dplyr::anti_join(GBIF_Accepted_Synonyms, by = c('Genus', 'Species')) %>%
    dplyr::anti_join(GBIF_Outgoing_Edges, by = c('Genus', 'Species')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)

  assertthat::assert_that(!any(is.na(GBIF_Incoming_Edges$GBIF_accepted_ID))) ## all incoming edges need GBIF_Accepted_ID column

  ## see comments in load_WFO for details
  GBIF_Synonyms_where_Accepted_not_found <- GBIF_Accepted_Synonyms %>%
    dplyr::filter(!is.na(GBIF_accepted_ID)) %>%
    dplyr::anti_join(GBIF_Accepted_Synonyms, by = c('GBIF_accepted_ID' = 'GBIF_ID')) %>%
    dplyr::anti_join(GBIF_Outgoing_Edges, by = c('GBIF_accepted_ID' = 'GBIF_ID'))

  GBIF_Accepted_Synonyms_without_Conflict <- GBIF_Accepted_Synonyms %>%
    dplyr::anti_join(GBIF_Synonyms_where_Accepted_not_found, by = c('Genus', 'Species'))

  GBIF_merged <- GBIF_Accepted_Synonyms_without_Conflict %>%
    dplyr::bind_rows(GBIF_Outgoing_Edges, GBIF_Incoming_Edges) %>%
    dplyr::distinct(Genus, Species, .keep_all = T) %>%
    dplyr::arrange(Genus, Species) %>%
    dplyr::relocate(Genus, Species)

  GBIF_merged_non_distinct <- GBIF_Accepted_Synonyms_without_Conflict %>%
    dplyr::bind_rows(GBIF_Outgoing_Edges, GBIF_Incoming_Edges) %>%
    dplyr::arrange(Genus, Species) %>%
    dplyr::relocate(Genus, Species)

  nrow_conflicting <- GBIF_merged %>% filter(!is.na(GBIF_accepted_ID)) %>% dplyr::anti_join(GBIF_merged, by = c('GBIF_accepted_ID' = 'GBIF_ID')) %>% nrow()
  if(nrow_conflicting > 0){
    warning(paste('The GBIF database is not self-contained, meaning that there are synonyms for which the accepted species is not in the database! In total, there are', nrow_conflicting, 'conflicting species.'))
  }

  return(GBIF_merged)
}

## !! if backbones are updated: remember to update Treemendous.Trees documentation in R/data.R as well!!
folder_raw_data <- '../raw_data_treemendous/'
bgci_path <- paste(folder_raw_data, 'bgci_globaltree_v16_2022_apr.csv', sep = '')
wcvp_path <- paste(folder_raw_data, 'wcvp_v9_jun_2022.txt', sep = '')
wfo_path <- paste(folder_raw_data, 'wfo_classification_v2022_jul.txt', sep = '')
gbif_path <- paste(folder_raw_data, 'gbif_taxon_v2021_dez.tsv', sep = '')
paths <- hash::hash("bgci" = bgci_path, "wcvp" = wcvp_path, "wfo" = wfo_path, "gbif" = gbif_path)

get_tree_genera_list <- memoise::memoise(helper.get_tree_genera_list) ## remember output of tree genera list using memoise:  only needs to evaluate it once

WFO <-  load_WFO(paths)
BGCI <-  load_BGCI(paths)
WCVP <-  load_WCVP(paths)
GBIF <-  load_GBIF(paths)

## combine via outer join using Reduce on Genus and Species columns
names_dfs <- c('WCVP', 'GBIF', 'WFO', 'BGCI')
dfs <- list(WCVP, GBIF, WFO, BGCI)

dfs_indicator <- purrr::map2(dfs, names_dfs, function(.dfs, .names_dfs){
  dplyr::mutate(.dfs, !! .names_dfs := TRUE)
})

df_merged <- purrr::reduce(dfs_indicator, function(df1, df2) dplyr::full_join(df1, df2, by = c('Genus', 'Species')))

treemendous_database <- df_merged %>%
  dplyr::mutate_at(names_dfs, ~replace_na(.,0)) %>%
  dplyr::relocate('Genus', 'Species', 'BGCI', 'WFO', 'WCVP', 'GBIF') %>%
  dplyr::arrange(Genus, Species) %>%
  dplyr::mutate(ID_merged = dplyr::row_number())

### Tidy-up treemendous_database to match the input constraints the package is posing.

Treemendous.Trees <- treemendous_database %>%
  dplyr::mutate(Genus = stringr::str_remove(Genus, '\u00D7')) %>% # remove hybrid characters in genus name
  dplyr::mutate(Genus = stringr::str_to_sentence(Genus)) %>%
  dplyr::mutate(Species = stringr::str_to_lower(Species)) %>%
  dplyr::distinct(Genus, Species, .keep_all = TRUE) ## we loose 11 species here: but it is necessary to avoid having duplicate species in get_testset() function


usethis::use_data(Treemendous.Trees, overwrite = TRUE, compress = 'xz')

