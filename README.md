# Treemendous: An R Package for Standardizing Taxonomical Names of Tree Species

[![R-CMD-check](https://github.com/speckerf/treemendous/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/speckerf/treemendous/actions/workflows/R-CMD-check.yml)
[![codecov](https://codecov.io/gh/speckerf/treemendous/branch/main/graph/badge.svg?token=XKTPGEMFDE)](https://codecov.io/gh/speckerf/treemendous)

## Reference Manual

The latest version of the reference manual is available [here](Reference_manual_Treemendous_1.0.0.pdf).

## Installation

Install `devtools` if necessary. Then install the package via:
```r 
devtools::install_github("speckerf/treemendous")
```

## Example

### Package Installation

```{r install, eval=FALSE}
library(devtools)
install_github("speckerf/treemendous")
```

### Species List Preparation

All functions of _Treemendous_ require the species name to be split into two columns, `Genus` and `Species`, with the former being capitalized. Assume you have two species, _Acer platanoides_ and _Fagus sylvatica_, you can create the input `tibble` by calling: 


```{r input, message=FALSE}
library(tidyverse)
species <- c('Acer platanoides', 'Fagus sylvatica')
input <- species %>%
  tibble::as_tibble_col(column_name = 'binomial') %>%
  tidyr::separate(col = 'binomial', into = c('Genus', 'Species'))
input
```

Other useful functions for creating the input `tibble` include:

```{r, eval=FALSE}
readr::read_csv('path') # import data
dplyr::select(Genus, Species) # select columns
dplyr::distinct(Genus, Species) # remove duplicate binomials
dplyr::rename('Genus' = 'old_genus_name',
                'Species' = 'old_species_name') # rename columns
dplyr::mutate(Genus = stringr::str_to_title(Genus)) # capitalize Genus
dplyr::mutate(Species = stringr::str_remove(Species, ".*?\\s")) # remove everything before first space
tidyr::drop_na(c('Genus', 'Species')) # remove rows with NA's
dplyr::arrange(Genus, Species) # sort names
dplyr::bind_rows(x, y) # concatenate two tibble's

```

### FIA: Standardize species names from the U.S. Forest Inventory and Analysis program. 

Along with the package comes an example dataset `fia` with $2171$ different tree species names. Assume that we want to standardize these species names according to a certain backbone (use the backbone argument). The function `summarize_output()` can be used to get a summary of the process. 

```{r}
library(treemendous)
```

```{r, message=FALSE}
result <- fia %>% matching(backbone = 'BGCI')
summarize_output(result)
```

From $2171$ species names in total, we were able to match $1848$ according to the backbone `WFO`, with $1800$ names matching exactly, and $48$ species names matching using fuzzy- and suffix-matching. Besides information about the matching process, the output contains the old names (prefix `Orig.`) as well as the matched names (prefix `Matched.`) as follows:

```{r}
result %>% 
  dplyr::slice_head(n=3) %>%
  dplyr::select(1:5)
```

We can further increase the number of matched species by using the functions `matching()` followed by `enforce_matching()`. Here, we specify the backbone `BGCI`. 

```{r, message=FALSE}
result <- fia %>% 
  matching(backbone = 'BGCI') %>% 
  enforce_matching(backbone = 'BGCI')
result %>% summarize_output()
```

Now, we are able to match $2086$ species names in total, with $238$ species being matched via `enforce_matching()`. Note that the number of matched distinct species names is lower with $2051$, because several input species were matched to the same species in the target database `BGCI`. 

If we choose a different backbone than `BGCI`, we can further resolve synonyms after matching the species names with the function `resolve_synonyms()`. Now, the output contains additionally the accepted species names (prefix `Accepted.`), as well as a column `Accepted.Backbone`, which states accroding to which backbone the synonym was resolved.

```{r, message=FALSE}
result <- fia %>% 
  matching('WFO') %>% 
  resolve_synonyms('WFO')
```

```{r}
result %>%
dplyr::slice_head(n=3) %>%
dplyr::select(1:6, 8)
```

Instead of using a single backbone, the user can decide to use any subset of the backbones `c('BGCI', 'WFO', 'WCVP', 'GBIF')` or use all of them by simply calling `matching()` without any argument. While `matching()` considers all backbones being equally important, the function `sequential_matching()` can be used to call `matching()` for individual backbones sequentially. For every species, the matched backbone is provided in the column `Matched.Backbone`. 

```{r, eval=F, message=FALSE}
result <- fia %>% 
  sequential_matching(sequential_backbones = c('BGCI', 'WFO', 'WCVP'))
```

### Translate species names between two databases.

Oftentimes, researches require integrating multi-modal data from
different sources for their analyses. Here, we demonstrate the use of the 
function `translate_trees()`, which allows a user directly translate names from an input database to a target database. 
First, we resolve both databases individually according to the single backbone (WFO) and compare the resolved names. Then, we use translate\_trees to translate the input species names into the target names.

```{r}
input <- tibble::tibble(
  Genus = c('Machilus', 'Quercus', 'Ocotea'),
  Species = c('velutina', 'leucotrichophora', 'citrusmioides')
)
target <- tibble::tibble(
  Genus = c('Actinodaphne', 'Quercus', 'Ocotea'),
  Species = c('magniflora', 'oblongata', 'citrusmoides')
)
```

```{r, message=FALSE}
input %>%
  matching(backbone = 'WFO') %>%
  resolve_synonyms('WFO') %>%
  dplyr::select(1:6)
```

```{r, message=FALSE}
target %>%
  matching(backbone = 'WFO') %>%
  resolve_synonyms('WFO') %>%
  dplyr::select(1:6)
```

Resolving both sets individually leads to a mismatch - _Machilus velutina_ and _Actinodaphne magniflora_ were resolved to two different names. Now let's see whether translate\_trees can be used to match all three species: 

```{r, message=FALSE}
translate_trees(df = input, target = target) %>% 
  dplyr::select(1:4) 
```

Essentially, all three species names can be translated from the input set to the target set. Incorporating the knowledge of the desired target names, the function leverages the information about synonym-accepted relations in the three backbones WFO, WCVP and GBIF and is able to translate _Machilus velutina_ into _Actinodaphne magniflora_. 


## Overview of functionality

Please refer to the documentation for a detailed description of the functions: [treemendous_1.0.0.pdf](https://github.com/speckerf/treemendous/files/11333926/treemendous_1.0.0.pdf)


<img width="953" alt="grafik" src="https://user-images.githubusercontent.com/71322309/196914510-0074d30c-52b9-4d45-a234-cfce98363f02.png">


