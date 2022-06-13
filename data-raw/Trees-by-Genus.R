## code to prepare `Trees.by.Genus` dataset goes here

filter_by_genus <- function(name){
  return(Trees.Full %>% dplyr::filter(Genus == name))
}
all_genera = unique(Trees.Full$Genus)
Trees.by.Genus <- setNames(lapply(all_genera, filter_by_genus), all_genera)

usethis::use_data(Trees.by.Genus, overwrite = TRUE, internal = TRUE)
