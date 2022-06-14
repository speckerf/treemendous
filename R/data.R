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

#' Prices of 50,000 round cut diamonds TODO.
#'
#' A dataset containing the prices and other attributes of almost 54,000
#' diamonds TODO.
#'
#' @format A data frame with 331414 rows and 10 variables:
#' \describe{
#'   \item{TODO}{price, in US dollars}
#'   \item{TODO}{weight of the diamond, in carats}
#'   ...
#' }
#' @source \url{https://github.com/speckerf/}
"Trees.Reduced"

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
