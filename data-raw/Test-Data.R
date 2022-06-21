## code to prepare `Test-Data` dataset goes here

library(dplyr)
library(stringr)

set.seed(111)
test1 <- sample_n(Trees.Reduced, 100) %>%
  select(c('Genus', 'Species')) %>%
  rename(Orig.Genus = Genus, Orig.Species = Species)


test2 <- test1 %>%
  mutate(Orig.Species = gsub('.{1}$', '', Orig.Species))

test3 <- rbind(test1[1:5,], test2[1:5,])

test4 <- test1 %>%
  mutate(Orig.Genus = gsub('.{1}$', '', Orig.Genus))

test5 <- test1 %>%
  mutate(Orig.Genus = gsub('.{1}$', '', Orig.Genus),
         Orig.Species = gsub('.{1}$', '', Orig.Species))

test6_species <- c('Juglans allardiana',
                  'Conyza cayennensis',
                  'Leandra grandyfolia',
                  'Chionanthus veruculatus',
                  'Tephrosia marianum',
                  'Stachytarpheta caudatoides',
                  'Cerdrela brunellioides',
                  'Cerreus mallisonii',
                  'Zitrus kinokunnii',
                  'Mirciaria mashalanta')
## Expected Matches
# A tibble: 10 × 2
# Genus          Species
# <chr>          <chr>
#  c('Juglans allardiana', 'Conyza cayennensis', 'Leandra grandifolia', 'Chionanthus verruculatus', 'Tephrosia mariana', 'Stachytarpheta caudata', 'Cedrela brunellioides', 'Cereus mallisonii', 'Citrus kinokuni', 'Myrciaria maschalantha')
test6 <- test6_species %>% tibble::tibble() %>%
  dplyr::mutate(Orig.Genus = sapply(strsplit(test6_species, split = " "), "[[", 1),
                Orig.Species = sapply(strsplit(test6_species, split = " "), "[[", 2)) %>%
  dplyr::select(-c("."))

usethis::use_data(test1, overwrite = TRUE)
usethis::use_data(test2, overwrite = TRUE)
usethis::use_data(test3, overwrite = TRUE)
usethis::use_data(test4, overwrite = TRUE)
usethis::use_data(test5, overwrite = TRUE)
usethis::use_data(test6, overwrite = TRUE)
