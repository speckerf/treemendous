## code to prepare `Test-Data` dataset goes here

library(dplyr)
library(stringr)

set.seed(111)
test1 <- sample_n(Trees.Reduced, 100) %>%
  select(c('Genus', 'Species'))

test2 <- test1 %>%
  select(c('Genus', 'Species'))
test2$Species <- test2$Species %>%
  {gsub('.{1}$', '', .)}

test3 <- rbind(test1[1:5,], test2[1:5,])

test4 <- test1 %>%
  select(c('Genus', 'Species')) %>%
  mutate(Genus = gsub('.{1}$', '', Genus))


usethis::use_data(test1, overwrite = TRUE)
usethis::use_data(test2, overwrite = TRUE)
usethis::use_data(test3, overwrite = TRUE)
usethis::use_data(test4, overwrite = TRUE)
