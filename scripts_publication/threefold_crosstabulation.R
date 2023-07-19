devtools::load_all()
require('dplyr')
require('forcats')

lookup <- function(old, new) {
  if (length(old) != length(new)) {
    stop("`old` and `new` must be of equal length.")
  }
  lookup <- setNames(new, old)
  rlang::splice(as.list(lookup))
}

# refactor status variables to the three levels: Accepted Synonym Missing
wfo_fct <- fct_collapse(
  stringi::stri_replace_na(Treemendous.Trees$WFO_Status, replacement = 'missing'),
             'Missing' = 'missing',
             'Accepted' = c('Accepted'),
             'Not Accepted' = c('Synonym')
)

gbif_fct <- fct_collapse(
  stringi::stri_replace_na(Treemendous.Trees$GBIF_Status, replacement = 'missing'),
                  'Missing' = 'missing',
                  'Accepted' = c('Accepted'),
                  'Not Accepted'  = c('Doubtful', 'Heterotypic synonym', 'Homotypic synonym', 'Proparte synonym', 'Synonym')
)

wcvp_fct <- fct_collapse(
  stringi::stri_replace_na(Treemendous.Trees$WCVP_Status, replacement = 'missing'),
  'Missing' = 'missing',
  'Accepted' = c('Accepted'),
  'Not Accepted'  = c('Artificial Hybrid', 'Unplaced', 'Homotypic_Synonym', 'Synonym')
)


tab <- table(wfo_fct, wcvp_fct, gbif_fct)
ftable(tab)

