#' Title: TODO.
#'
#' A dataset containing TODO.
#'
#' @format A data frame with 331414 rows and 27 variables:
#' \describe{
#'   \item{Genus}{Genus name of species binomial}
#'   \item{Species}{Specific epithet of species binomial}
#'   \item{BGCI}{Logical indicator whether this species was present in the `BGCI` backbone: Version `1.6` (April, 2022), [Source](https://tools.bgci.org/global_tree_search.php)}
#'   \item{WFO}{Logical indicator whether this species was present in the `WFO` backbone: Version `v.2022.04` (April, 2022), [Source](http://www.worldfloraonline.org/downloadData)}
#'   \item{WCVP}{Logical indicator whether this species was present in the `WCVP` backbone: Version `v8` (March, 2022), [Source](https://wcvp.science.kew.org/)}
#'   \item{GBIF}{Logical indicator whether this species was present in the `GBIF` backbone: Version (November, 2021), [Source](https://hosted-datasets.gbif.org/datasets/backbone/)}
#'   \item{FIA}{Logical indicator whether this species was present in the `FIA` backbone: Version `9.1`, [Source](https://www.fia.fs.fed.us/library/field-guides-methods-proc/index.php)}
#'   \item{PM}{Logical indicator whether this species was present in the `V.PhyloMaker` backbone: Package version `0.1.0` [Source](https://github.com/jinyizju/V.PhyloMaker)}
#'   ...
#' }
#' @source \url{https://github.com/speckerf/}
"Trees.Full"

#' Subset of Trees.Full, which is used as a test-set.
#'
#' A test dataset containing 100 randomly selected Species from the database Trees.Full.
#' The following modifications were performed:
#' - There were no modifications on the names performed.
#'
#' @format A data frame with 100 rows and 2 variables:
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
#' @format A data frame with 100 rows and 2 variables:
#' \describe{
#'   \item{Orig.Species}{Name of specific epithet}
#'   \item{Orig.Genus}{Name of Genus}
#' }
#' @source \url{https://github.com/speckerf/}
"test2"

#' Modified subset of Trees.Full, which is used as a test-set.
#'
#' A test dataset containing 10 Species, the first 5 from test1 and the first 5 from test2.
#'
#' @format A data frame with 10 rows and 2 variables:
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
#' @format A data frame with 100 rows and 2 variables:
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
#' @format A data frame with 100 rows and 2 variables:
#' \describe{
#'   \item{Orig.Species}{Name of specific epithet}
#'   \item{Orig.Genus}{Name of Genus}
#' }
#' @source \url{https://github.com/speckerf/}
"test5"

#' Modified subset of Trees.Full, which is used as a test-set.
#'
#' A test dataset containing 10 randomly selected Species from the database.
#' The following modifications were performed:
#' - 2: unchanged
#' - 2: one character typo in species
#' - 2: changed suffix
#' - 2: Typo in Genus
#' - 2: Typo in Genus and Species name
#'
#' @format A data frame with 100 rows and 2 variables:
#' \describe{
#'   \item{Orig.Species}{Name of specific epithet}
#'   \item{Orig.Genus}{Name of Genus}
#' }
#' @source \url{https://github.com/speckerf/}
"test6"
