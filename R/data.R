#' Title: TODO.
#'
#' A dataset containing TODO.
#'
#' @format A data frame with `r nrow(Treemendous.Trees)` rows and `r ncol(Treemendous.Trees)` variables:
#' \describe{
#'   \item{Genus}{Genus name of species binomial}
#'   \item{Species}{Specific epithet of species binomial}
#'   \item{BGCI}{Logical indicator whether this species was present in the `BGCI` backbone: Version `1.6` (April, 2022), [Source](https://tools.bgci.org/global_tree_search.php)}
#'   \item{WFO}{Logical indicator whether this species was present in the `WFO` backbone: Version `v.2022.04` (April, 2022), [Source](http://www.worldfloraonline.org/downloadData)}
#'   \item{WCVP}{Logical indicator whether this species was present in the `WCVP` backbone: Version `v8` (March, 2022), [Source](https://wcvp.science.kew.org/)}
#'   \item{GBIF}{Logical indicator whether this species was present in the `GBIF` backbone: Version (November, 2021), [Source](https://hosted-datasets.gbif.org/datasets/backbone/)}
#'   \item{FIA}{Logical indicator whether this species was present in the `FIA` backbone: Version `9.1`, [Source](https://www.fia.fs.fed.us/library/field-guides-methods-proc/index.php)}
#'   \item{PM}{Logical indicator whether this species was present in the `V.PhyloMaker` backbone: Package version `0.1.0` [Source](https://github.com/jinyizju/V.PhyloMaker)}
#'   \item{BGCI_Authors}{...}
#'   \item{WFO_ID}{...}
#'   \item{WFO_accepted_ID}{...}
#'   \item{WFO_Status}{...}
#'   \item{WFO_Authors}{...}
#'   \item{WFO_Family}{...}
#'   \item{WCVP_ID}{...}
#'   \item{WCVP_accepted_ID}{...}
#'   \item{WCVP_Status}{...}
#'   \item{WCVP_Authors}{...}
#'   \item{WCVP_accepted_Authors}{...}
#'   \item{WCVP_Family}{...}
#'   \item{GBIF_ID}{...}
#'   \item{GBIF_accepted_ID}{...}
#'   \item{GBIF_Status}{...}
#'   \item{GBIF_Authors}{...}
#'   \item{GBIF_Family}{...}
#'   \item{FIA_ID}{...}
#'   \item{PM_Family}{...}
#'   \item{ID_merged}{...}
#' }
#' @source \url{https://github.com/speckerf/}
#'
#'
#'
"Treemendous.Trees"


#' Title: Selected Trees from the IUCN red list of Threatened Species
#'
#' A dataset containing threatened tree species from the genus `Acer` and the families `Betulaceae`, `Nothofagaceae` and `Theaceae`.
#' The data was downloaded in June 2022 from the official webpage of the International Union for Conservation of Nature (IUCN) and is available under the following [link](https://www.iucnredlist.org/resources/spatial-data-download).
#'
#' @format A data frame with `r nrow(iucn)` rows and `r ncol(iucn)` variables:
#' \describe{
#'   \item{Orig.Genus}{Genus name of species binomial}
#'   \item{Orig.Species}{Specific epithet of the species binomial}
#' }
"iucn"

