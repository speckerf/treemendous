#' Title: TODO.
#'
#' A dataset containing TODO.
#'
#' @format A data frame with `r nrow(Trees.Full)` rows and `r ncol(Trees.Full)` variables:
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




"Trees.Full"

#' Subset of Trees.Full, which is used as a test-set.
#'
#' A test dataset containing 100 randomly selected Species from the database Trees.Full.
#' The following modifications were performed:
#' - There were no modifications on the names performed.
#'
#' @format A data frame with `r nrow(test1)` rows and `r ncol(test1)` variables:
#' \describe{
#'   \item{Orig.Species}{Name of specific epithet}
#'   \item{Orig.Genus}{Name of Genus}
#' }
#' @source \url{https://github.com/speckerf/}
"test1"

#' Modified subset of Trees.Full, which is used as a test-set.
#'
#' A test dataset containing 100 randomly selected Species from the database.
#' The following modifications were performed:
#' - The last character of the Species column was removed and the species names should therefore be fuzzy matched.
#'
#' @format A data frame with `r nrow(test2)` rows and `r ncol(test2)` variables:
#' \describe{
#'   \item{Orig.Species}{Name of specific epithet}
#'   \item{Orig.Genus}{Name of Genus}
#' }
#' @source \url{https://github.com/speckerf/}
"test2"

#' Modified subset of Trees.Full, which is used as a test-set.
#'
#' A test dataset containing `r nrow(test3)` Species, the first 5 from test1 and the first 5 from test2.
#'
#' @format A data frame with `r nrow(test3)` rows and `r ncol(test3)` variables:
#' \describe{
#'   \item{Orig.Species}{Name of specific epithet}
#'   \item{Orig.Genus}{Name of Genus}
#' }
#' @source \url{https://github.com/speckerf/}
"test3"

#' Modified subset of Trees.Full, which is used as a test-set.
#'
#' A test dataset containing 100 randomly selected Species from the database.
#' The following modifications were performed:
#' - The last character of the Genus column was removed and the Genus names should therefore be fuzzy matched.
#'
#' @format A data frame with `r nrow(test4)` rows and `r ncol(test4)` variables:
#' \describe{
#'   \item{Orig.Species}{Name of specific epithet}
#'   \item{Orig.Genus}{Name of Genus}
#' }
#' @source \url{https://github.com/speckerf/}
"test4"

#' Modified subset of Trees.Full, which is used as a test-set.
#'
#' A test dataset containing 100 randomly selected Species from the database.
#' The following modifications were performed:
#' - The last character of the Genus column was removed and the Genus names should therefore be fuzzy matched.
#' - The last character of the Species column was removed, and therefore should be fuzzy matched.
#'
#' @format A data frame with `r nrow(test5)` rows and `r ncol(test5)` variables:
#' \describe{
#'   \item{Orig.Species}{Name of specific epithet}
#'   \item{Orig.Genus}{Name of Genus}
#' }
#' @source \url{https://github.com/speckerf/}
"test5"

#' Modified subset of Trees.Full, which is used as a test-set.
#'
#' A test dataset the first 10 species from `test1`, `test2`, `test4` and `test5`.
#' The following modifications were performed:
#' - 10: unchanged
#' - 10: last character from species removed
#' - 10: last character from genus removed
#' - 10: last character from both genus and species removed
#'
#' @format A data frame with `r nrow(test6)` rows and `r ncol(test6)` variables:
#' \describe{
#'   \item{Orig.Species}{Name of specific epithet}
#'   \item{Orig.Genus}{Name of Genus}
#' }
#' @source \url{https://github.com/speckerf/}
"test6"
