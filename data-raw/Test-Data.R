## code to prepare `Test-Data` dataset goes here

library(dplyr)
library(stringr)

df <- data("Trees.Reduced")

if(!exists("Trees.Reduced")){
  data("Trees.Reduced")
}

set.seed(111)
test1 <- sample_n(Trees.Reduced, 100) %>%
  select(c('Genus', 'Species'))

set.seed(112)
test2 <- test1 %>%
  select(c('Genus', 'Species'))
test2$Species <- test2$Species %>%
  {gsub('.{1}$', '', .)}


usethis::use_data(test1, overwrite = TRUE)
usethis::use_data(test2, overwrite = TRUE)
