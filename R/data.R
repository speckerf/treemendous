#' Title: TODO.
#'
#' A dataset containing TODO.
#'
#' @format A data frame with 331414 rows and 27 variables:
#' \describe{
#'   \item{price}{price, in US dollars}
#'   \item{carat}{weight of the diamond, in carats}
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
#'   \item{Species}{Name of specific epithet}
#'   \item{Genus}{Name of Genus}
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
#'   \item{Species}{Name of specific epithet}
#'   \item{Genus}{Name of Genus}
#' }
#' @source \url{https://github.com/speckerf/}
"test2"

#' Modified subset of Trees.Full, which is used as a test-set.
#'
#' A test dataset containing 10 Species, the first 5 from test1 and the first 5 from test2.
#'
#' @format A data frame with 10 rows and 2 variables:
#' \describe{
#'   \item{Species}{Name of specific epithet}
#'   \item{Genus}{Name of Genus}
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
#'   \item{Species}{Name of specific epithet}
#'   \item{Genus}{Name of Genus}
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
#'   \item{Species}{Name of specific epithet}
#'   \item{Genus}{Name of Genus}
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
#'   \item{Species}{Name of specific epithet}
#'   \item{Genus}{Name of Genus}
#' }
#' @source \url{https://github.com/speckerf/}
"test6"
