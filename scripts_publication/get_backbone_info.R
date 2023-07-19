## script to get backbone info in section 3.1 datasets

devtools::load_all()
require('dplyr')

# all
Treemendous.Trees %>% nrow()

# BGCI
# entries
Treemendous.Trees %>% filter(BGCI) %>% nrow()
# genera
Treemendous.Trees %>% filter(BGCI) %>% select(Genus) %>% distinct() %>% nrow()


# WFO
# entries
Treemendous.Trees %>% filter(WFO) %>% nrow()
# accepted
Treemendous.Trees %>% filter(WFO) %>% filter(WFO_Status == 'Accepted') %>%  nrow()

# WCVP
# entries
Treemendous.Trees %>% filter(WCVP) %>% nrow()
# accepted
Treemendous.Trees %>% filter(WCVP) %>% filter(WCVP_Status == 'Accepted') %>%  nrow()


# GBIF
# entries
Treemendous.Trees %>% filter(GBIF) %>% nrow()
# accepted
Treemendous.Trees %>% filter(GBIF) %>% filter(GBIF_Status == 'Accepted') %>%  nrow()
