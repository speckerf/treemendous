## code to prepare `Test-Data` dataset goes here

library(dplyr)
library(stringr)

set.seed(333)
test1 <- sample_n(Trees.Full, 100) %>%
  select(c('Genus', 'Species')) %>%
  rename(Orig.Genus = Genus, Orig.Species = Species)


test2 <- test1 %>%
  mutate(Orig.Species = gsub('.{1}$', '', Orig.Species))

test3 <- rbind(test1[1:5,], test2[1:5,])

test4 <- test1 %>%
  mutate(Orig.Genus = gsub('.{1}$', '', Orig.Genus))

assertthat::assert_that(!any(test4$Orig.Genus %in% Trees.Full$Genus)) ## test if created genera are by coincidence present in the database

test5 <- test1 %>%
  mutate(Orig.Genus = gsub('.{1}$', '', Orig.Genus),
         Orig.Species = gsub('.{1}$', '', Orig.Species))

#test6_species <- c('Juglans allardiana',
                  # 'Conyza cayennensis',
                  # 'Leandra grandyfolia',
                  # 'Chionanthus veruculatus',
                  # 'Tephrosia marianum',
                  # 'Stachytarpheta caudatoides',
                  # 'Cerdrela brunellioides',
                  # 'Cerreus mallisonii',
                  # 'Zitrus kinokunnii',
                  # 'Mirciaria mashalanta')

test6 <- list(test1, test2, test4, test5) %>%
  lapply(dplyr::slice, 1:10) %>%
  dplyr::bind_rows()

## Expected Matches
# A tibble: 10 × 2
# Genus          Species
# <chr>          <chr>
#  c('Juglans allardiana', 'Conyza cayennensis', 'Leandra grandifolia', 'Chionanthus verruculatus', 'Tephrosia mariana', 'Stachytarpheta caudata', 'Cedrela brunellioides', 'Cereus mallisonii', 'Citrus kinokuni', 'Myrciaria maschalantha')
# test6 <- test6_species %>% tibble::tibble() %>%
  #tidyr::separate(".", into = c('Orig.Genus', 'Orig.Species'), sep = ' ')

usethis::use_data(test1, overwrite = TRUE)
usethis::use_data(test2, overwrite = TRUE)
usethis::use_data(test3, overwrite = TRUE)
usethis::use_data(test4, overwrite = TRUE)
usethis::use_data(test5, overwrite = TRUE)
usethis::use_data(test6, overwrite = TRUE)
