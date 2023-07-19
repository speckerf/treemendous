#' @title Database used by Treemendous to standardize Species Names
#'
#' @description A dataset containing tree species assembled from four different publicly available datasets:
#'
#' - **BGCI** (Botanical Gardens Conservation Internation): _GlobalTreeSearch_, Version `1.7` (April, 2023), [Source](https://tools.bgci.org/global_tree_search.php)
#' - **WFO** (World Flora Online): _Taxonomic Backbone_, Version `v.2023.06` (June, 2023), [Source](http://www.worldfloraonline.org/downloadData)
#' - **WCVP** (World Checklist of Vascular Plants): Version `v9` (June, 2022), [Source](http://sftp.kew.org/pub/data-repositories/WCVP/Archive/)
#' - **GBIF** (Global Biodiversity Information Facility): Version (December, 2022), [Source](https://hosted-datasets.gbif.org/datasets/backbone/)
#'
#' _Treemendous_ matches and resolves synonyms according to the dataset `Treemendous.Trees`, allowing the user always to specify a subset of the backbones if desired.
#' @details
#' See the publication accompanying the _Treemendous_ package for more details. TODO: insert link to publication / pre-print.
#'
#' The code for how the backbones were curated and merged is available in the package source code under `data-raw/Treemendous-Trees.R`.
#'
#' Although all information about the taxonomic family of species (`WFO_Family`, `WCVP_Family`, `GBIF_Family`),
#' as well as the scientific authorship (`WFO_Authors`, `WCVP_Authors`, `GBIF_Authors`, `BGCI_Authors`), is not used by
#' the functionality of the package so far, we decided to keep the information out of the following reasons.
#' First, it allows a user to further investigate matched names and allows for manual a assessment of whether a match was reasonable or not.
#' Second, the unused information is likely to be used for future functionalities of the _Treemendous_ package.
#' For instance, we plan to let _Treemendous_ interact with the _V.PhyloMaker2_ package to get species phylogenies, a common analysis performed in ecological research.
#' _V.PhyloMaker2_ requires the user to input the taxonomic family, which could be resolved using the information about taxonomic families in `Treemendous.Trees`.
#' Further, if a user has access to information about the scientific authorship, future versions might consider this in cases of ambiguous matches, and could help resolving these.
#'
#'
#' @source \url{https://github.com/speckerf/treemendous}
#'
#' @format A data frame with `r nrow(Treemendous.Trees)` species and `r ncol(Treemendous.Trees)` variables:
#' \describe{
#'   \item{Genus}{Genus name of species binomial}
#'   \item{Species}{Specific epithet of species binomial}
#'   \item{BGCI}{Boolean indicator whether this species was present in the `BGCI` backbone}
#'   \item{WFO}{Boolean indicator whether this species was present in the `WFO` backbone}
#'   \item{WCVP}{Boolean indicator whether this species was present in the `WCVP` backbone}
#'   \item{GBIF}{Boolean indicator whether this species was present in the `GBIF` backbone}
#'   \item{BGCI_Authors}{Information about authors based on `BGCI`}
#'   \item{WFO_ID}{Unique ID in `WFO`}
#'   \item{WFO_accepted_ID}{Unique ID of accepted species in `WFO`}
#'   \item{WFO_Status}{Status according to `WFO`: e.g. Synonym, Accepted}
#'   \item{WFO_Authors}{Information about authors based on `WFO`}
#'   \item{WFO_Rank}{Taxonomic rank the species: one of `c('Species, Subspecies, Variety, Form)`}
#'   \item{WFO_Family}{Taxonomical family as specified by `WFO`}
#'   \item{WFO_Infraspecific}{Infraspecific epithet of the corresponding entry in `WFO`}
#'   \item{WFO_Flag}{Indicates whether multiple entries with identical latin binomials were present in the original **WFO** database, which would be resolved to different latin binomials in `resolve_synonyms()`.
#'   "Authorship ambiguity": Two or more entries at rank `Species` with different authorship would be resolved to different latin binomials.
#'   "Infraspecific ambiguituy": Two or more entries at infraspecific levels would be resolved to different latin binomials.}
#'   \item{WFO_new_linkage}{Boolean indicator whether `WCVP_accepted_ID` was relinked to another entry in `Treemendous.Trees` with the same latin binomial. This was necessary because our database design only allowed for one entry for every unique latin binomial. }
#'   \item{WCVP_ID}{Unique ID in `WCVP`}
#'   \item{WCVP_accepted_ID}{Unique ID of accepted species in `WCVP`}
#'   \item{WCVP_Status}{Status according to `WCVP`: e.g. Synonym, Accepted}
#'   \item{WCVP_Authors}{Information about authors based on `WCVP`}
#'   \item{WCVP_Rank}{Taxonomic rank the species: one of `c('Species, Subspecies, Variety, Form)`}
#'   \item{WCVP_Family}{Taxonomical family as specified by `WCVP`}
#'   \item{WCVP_Infraspecific}{Infraspecific epithet of the corresponding entry in `WCVP`}
#'   \item{WCVP_Flag}{See `WFO_Flag` above.}
#'   \item{WCVP_new_linkage}{See `WFO_new_linkage` above.}
#'   \item{GBIF_ID}{Unique ID in `GBIF`}
#'   \item{GBIF_accepted_ID}{Unique ID of accepted species in `GBIF`}
#'   \item{GBIF_Status}{Status according to `GBIF`: e.g. Synonym, Accepted}
#'   \item{GBIF_Authors}{Information about authors based on `GBIF`}
#'   \item{GBIF_Rank}{Taxonomic rank the species: one of `c('Species, Subspecies, Variety, Form)`}
#'   \item{GBIF_Family}{Taxonomical family as specified by `GBIF`}
#'   \item{GBIF_Infraspecific}{Infraspecific epithet of the corresponding entry in `GBIF`}
#'   \item{GBIF_Flag}{See `WFO_Flag` above.}
#'   \item{GBIF_new_linkage}{See `WFO_new_linkage` above.}
#'   \item{ID_merged}{Unique ID assigned to each species in `Treemendous.Trees`. Note that these ID's are currently not ensured to be consistent between subsequent versions of the package.}
#' }
"Treemendous.Trees"


#' @title Selected Trees from the IUCN red list of Threatened Species
#'
#' @description A dataset containing threatened tree species from the genus `Acer` and the families `Betulaceae`, `Nothofagaceae` and `Theaceae`.
#' The data was downloaded in June 2022 from the official webpage of the International Union for Conservation of Nature (IUCN) and is available under the following [link](https://www.iucnredlist.org/resources/spatial-data-download).
#'
#' @format A data frame with `r nrow(iucn)` rows and `r ncol(iucn)` variables:
#' \describe{
#'   \item{Orig.Genus}{Genus name of species binomial}
#'   \item{Orig.Species}{Specific epithet of the species binomial}
#' }
"iucn"

#' @title Cleaned Master Tree Species list from FIA
#'
#' @description A cleaned dataset containing trees the Forest Inventory and Analysis (FIA) program of the U.S. Forest Service. This dataset is used in the example usage section of the manuscript for the _Treemendous_ package.
#' The data was downloaded in November 2022 from the official webpage of the Forest Inventory and Analysis National Program and is available under the following [link](https://www.fia.fs.usda.gov/library/field-guides-methods-proc/index.php).
#'
#' @format A data frame with `r nrow(fia)` rows and `r ncol(fia)` variables:
#' \describe{
#'   \item{Genus}{Genus name of species binomial}
#'   \item{Species}{Specific epithet of the species binomial}
#' }
"fia"

