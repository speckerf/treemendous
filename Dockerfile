FROM rocker/verse:latest

RUN apt-get update \
 && apt-get upgrade -y \
 && apt-get install -y \
 git \
 git-lfs \
 make \
 gzip \
 rename

RUN R -e "install.packages('magrittr');      if (!library(magrittr, logical.return=T)) quit(status=10)" 
RUN R -e "install.packages('remotes'); if (!library(remotes, logical.return=T)) quit(status=10)"
RUN R -e "install.packages('assertthat');   if (!library(assertthat, logical.return=T)) quit(status=10)" 
RUN R -e "install.packages('tibble'); if (!library(tibble, logical.return=T)) quit(status=10)"
RUN R -e "install.packages('tidyr');      if (!library(tidyr, logical.return=T)) quit(status=10)" 
RUN R -e "install.packages('dplyr'); if (!library(dplyr, logical.return=T)) quit(status=10)" 
RUN R -e "install.packages('purrr');      if (!library(purrr, logical.return=T)) quit(status=10)" 
RUN R -e "install.packages('readr');      if (!library(readr, logical.return=T)) quit(status=10)" 
RUN R -e "install.packages('stringr');   if (!library(stringr, logical.return=T)) quit(status=10)"
RUN R -e "install.packages('memoise');   if (!library(memoise, logical.return=T)) quit(status=10)" 
RUN R -e "install.packages('progress'); if (!library(progress, logical.return=T)) quit(status=10)" 
RUN R -e "install.packages('fuzzyjoin');   if (!library(fuzzyjoin, logical.return=T)) quit(status=10)" 
RUN R -e "install.packages('Matrix');      if (!library(Matrix, logical.return=T)) quit(status=10)" 
RUN R -e "install.packages('igraph'); if (!library(igraph, logical.return=T)) quit(status=10)" 
RUN R -e "remotes::install_github('speckerf/treemendous'); if (!library(treemendous, logical.return=T)) quit(status=10)"
