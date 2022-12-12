#' @title Database used by Treemendous to standardize Species Names
#'
#' @description A dataset containing tree species assembled from four different publicly available datasets:
#'
#' - **BGCI** (Botanical Gardens Conservation Internation): _GlobalTreeSearch_, Version `1.6` (April, 2022), [Source](https://tools.bgci.org/global_tree_search.php)
#' - **WFO** (World Flora Online): _Taxonomic Backbone_, Version `v.2022.07` (July, 2022), [Source](http://www.worldfloraonline.org/downloadData)
#' - **WCVP** (World Checklist of Vascular Plants): Version `v9` (June, 2022), [Source](http://sftp.kew.org/pub/data-repositories/WCVP/Archive/)
#' - **GBIF** (Global Biodiversity Information Facility): Version (November, 2021), [Source](https://hosted-datasets.gbif.org/datasets/backbone/)
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
#'   \item{BGCI}{Logical indicator whether this species was present in the `BGCI` backbone}
#'   \item{WFO}{Logical indicator whether this species was present in the `WFO` backbone}
#'   \item{WCVP}{Logical indicator whether this species was present in the `WCVP` backbone}
#'   \item{GBIF}{Logical indicator whether this species was present in the `GBIF` backbone}
#'   \item{BGCI_Authors}{Information about authors based on `BGCI`}
#'   \item{WFO_ID}{Unique ID in `WFO`}
#'   \item{WFO_accepted_ID}{Unique ID of accepted species in `WFO`}
#'   \item{WFO_Status}{Status according to `WFO`: e.g. Synonym, Accepted}
#'   \item{WFO_Authors}{Information about authors based on `WFO`}
#'   \item{WFO_Family}{Taxonomical family as specified by `WFO`}
#'   \item{WCVP_ID}{Unique ID in `WCVP`}
#'   \item{WCVP_accepted_ID}{Unique ID of accepted species in `WCVP`}
#'   \item{WCVP_Status}{Status according to `WCVP`: e.g. Synonym, Accepted}
#'   \item{WCVP_Authors}{Information about authors based on `WCVP`}
#'   \item{WCVP_Family}{Taxonomical family as specified by `WCVP`}
#'   \item{GBIF_ID}{Unique ID in `GBIF`}
#'   \item{GBIF_accepted_ID}{Unique ID of accepted species in `GBIF`}
#'   \item{GBIF_Status}{Status according to `GBIF`: e.g. Synonym, Accepted}
#'   \item{GBIF_Authors}{Information about authors based on `GBIF`}
#'   \item{GBIF_Family}{Taxonomical family as specified by `GBIF`}
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

