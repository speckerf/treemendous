helper.get_trees_by_genera <- function(){
  return(Trees.Full %>% base::split(.$Genus))
}

get_trees_by_genera <- memoise::memoise(helper.get_trees_by_genera)
