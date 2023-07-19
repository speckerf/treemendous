# Treemendous: An R Package for Standardizing Taxonomical Names of Tree Species

[![R-CMD-check](https://github.com/speckerf/treemendous/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/speckerf/treemendous/actions/workflows/R-CMD-check.yml)
[![codecov](https://codecov.io/gh/speckerf/treemendous/branch/main/graph/badge.svg?token=XKTPGEMFDE)](https://codecov.io/gh/speckerf/treemendous)

Treemendous is an open-source software package for the R programming environment that provides a toolset for standardizing tree species names and translating between different databases according to four publicly
available backbones [World Flora Online (WFO), the Botanical Gardens Convention International (BGCI), the World Consensus on Vascular Plants (WCVP) and the Global Biodiversity Information Facility (GBIF)]. The
package simultaneously leverages information and relationships across all these
backbones to increase matching rates and minimize data loss, while ensuring the
resulting species are accepted and consistent with a single reference backbone. The package provides a flexible workflow depending on the use case, in which users can
chain together different functionalities ranging from simple matching to a single
backbone, to graph-based iterative matching using synonym-accepted relations across
all backbones in the database. In addition, the package allows users to translate'
one tree species list into another, streamlining the assimilation of new data into
preexisting datasets or models. 
In this readme file we provide installation instructions and worked-out examples for more detailed information please refer to the reference manual or the publication associated to this package [Add link to pub]
### Reference Manual

The latest version of the reference manual is available [here](https://github.com/speckerf/treemendous/files/12092065/Reference_manual_Treemendous_1.1.0.pdf).



### Package Installation

```{r install, eval=FALSE}
library(devtools)
install_github("speckerf/treemendous")
```
If you are encountering problems installing _devtools_, try to install it instead with the package _remotes_:

```{r install, eval=FALSE}
remotes::install_github("speckerf/treemendous")
```
### Alternative Installation: Run with Docker

If for any reason the installation was not successful, we provide a Docker image with the package already preinstalled.
The Docker image is available on Dockerhub at 'speckerf/treemendous'. 
The major steps are described below:

**Download Docker Desktop Client**

- https://www.docker.com/products/docker-desktop/
- Start Docker Desktop Application

**Pull the image**
Open a terminal and navigate to the desired location. Then pull the image from Dockerhub with:
```bash
docker pull speckerf/treemendous
```
**Run the container**

Running the Docker container:

```bash
docker run --rm \  
           -p 8888:8787 \
           -e PASSWORD=password \
           treemendous
```
Go to your browser: open http://localhost:8888/ 
- this should open an rstudio interface: log in with username 'rstudio' and password 'password'

Note, the docker container cannot actually see any data on your local machine. You have to mount a repository. To mount your current working directory, use:
(if `$(pwd)` doesn't work in your terminal, you can use the absolute path)

```bash
docker run --rm \  
           -p 8888:8787 \
           -e PASSWORD=password \
           -v $(pwd):/home/rstudio \ 
           treemendous
```

## Example
### Species List Preparation

All functions of \textit{Treemendous} require the species name to be split into two columns, _Genus_ and _Species_, with the former being capitalized. Assume you have two species, \textit{Acer platanoides} and \textit{Fagus sylvatica}, you can create the input _tibble_ by calling: 


```{r input, message=FALSE}
### Species list preparation
library(tidyverse)
species <- c('Acer platanoides', 'Fagus sylvatica')
input <- species %>%
  tibble::as_tibble_col(column_name = 'binomial') %>%
  tidyr::separate(col = 'binomial', into = c('Genus', 'Species'))
input
```

Other useful functions for creating the input _tibble_ include:

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

Along with the package comes an example dataset _fia_ with $2171$ different tree species names. Assume that we want to standardize these species names according to a certain backbone (use the backbone argument). The function _summarize_output()_ can be used to get a summary of the process. 

```{r}
library(treemendous)
```

```{r, message=FALSE}
result <- fia %>% matching(backbone = 'BGCI')
summarize_output(result)
```

From $2171$ species names in total, we were able to match $1822$ according to the backbone _BGCI_, with $1779$ names matching exactly, and $43$ species names matching using fuzzy- and suffix-matching. Besides information about the matching process, the output contains the old names (prefix _Orig._) as well as the matched names (prefix _Matched._) as follows:

```{r}
result %>% 
  dplyr::slice_head(n=3) %>%
  dplyr::select(1:5)
```

We can further increase the number of matched species by using the functions _matching()_ followed by _enforce_matching()_. Here, we specify the backbone _BGCI_. 

```{r, message=FALSE}
result <- fia %>% 
  matching(backbone = 'BGCI') %>% 
  enforce_matching(backbone = 'BGCI')
result %>% summarize_output()
```

Now, we are able to match $2097$ species names in total, with $275$ species being matched via _enforce_matching()_. Note that the number of matched distinct species names is lower with $2044$, because several input species were matched to the same species in the target database _BGCI_. 

Note that if we choose a different backbone than _BGCI_, then species can matched names that are not accepted (synonyms), we can further resolve synonyms after matching the species names with the function _resolve_synonyms()_. Now, the output contains additionally the accepted species names (prefix _Accepted._), as well as a column _Accepted.Backbone_, which states according to which backbone the synonym was resolved.

```{r, message=FALSE}
result <- fia %>% 
  matching('WFO') %>% 
  resolve_synonyms('WFO')
```

```{r}
result %>% 
  dplyr::slice_head(n=3) %>% 
  dplyr::select(dplyr::matches('Orig|Matched|Accepted'), -'matched')
```
Note that a warning message is produced "Please consider calling highlight_flags() to investigate potential ambiguities upon resolving synonyms to accepted names". Potential ambiguities could have been resolved in your dataset and it is suggested to use _highlight_flags()_ to know more and decide if you want to check them manually. The _highlight_flags()_ function should be used separetely from the others as it will only return species that have some flag and not the full dataset.  

```{r, message=FALSE}
flags <- result %>% highlight_flags('WFO')
flags %>% 
  dplyr::slice_head(n=3) %>% 
  dplyr::select(dplyr::matches('Acc|Flag'))
```
Instead of using a single backbone, the user can decide to use any subset of the backbones _c('BGCI', 'WFO', 'WCVP', 'GBIF')_ or use all of them by simply calling _matching()_ without any argument. While _matching()_ considers all backbones being equally important, the function _sequential_matching()_ can be used to call _matching()_ for individual backbones sequentially. For every species, the matched backbone is provided in the column _Matched.Backbone_. 

```{r, eval=F, message=FALSE}
result <- fia %>% 
  sequential_matching(sequential_backbones = c('BGCI', 'WFO', 'WCVP'))
```
Remember that _matching()_ and _sequential_matching()_ match any species in the database and thus can provide matches to synonyms rather than accepted species. To get only accepted species returned use _resolve_synonyms()_ after the matching function.

### Translate species names between two databases.

Oftentimes, researches require integrating multi-modal data from
different sources for their analyses. Here, we demonstrate the use of the 
function _translate_trees()_, which allows a user directly translate names from an input database to a target database. 
First, we resolve both databases individually according to the single backbone (WFO) and compare the resolved names. Then, we use translate\_trees to translate the input species names into the target names.

```{r}
input <- tibble::tibble(
  Genus = c('Aria', 'Ardisia', 'Malus'),
  Species = c('umbellata', 'japonica', 'sylvestris')
)
target <- tibble::tibble(
  Genus = c('Sorbus', 'Ardisia', 'Malus'),
  Species = c('umbellata', 'montana', 'orientalis')
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
Resolving both sets individually leads to a mismatch - _Malus orientalis_ and _Malus sylvestris_ were resolved to two different names. Now let's see whether translate\_trees can be used to match all three species: 
```{r, message=FALSE}
translate_trees(df = input, target = target) %>% 
  dplyr::select(1:4) 
```

Essentially, all three species names can be translated from the input set to the target set. Incorporating the knowledge of the desired target names, the function leverages the information about synonym-accepted relations in the three backbones WFO, WCVP and GBIF and is able to translate _Malus sylvestris_ into _Malus orientalis_. 


## Overview of functionality

Please refer to the documentation for a detailed description of the functions: [treemendous_1.1.0.pdf](https://github.com/speckerf/treemendous/files/12092065/treemendous_1.1.0.pdf)


<img width="953" alt="grafik" src="https://user-images.githubusercontent.com/71322309/196914510-0074d30c-52b9-4d45-a234-cfce98363f02.png">


