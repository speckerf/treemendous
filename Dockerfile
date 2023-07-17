FROM rocker/rstudio:latest

RUN apt-get update 
RUN apt-get install libxml2
RUN apt-get update \
 && apt-get upgrade -y \
 && apt-get install -y \
 git \
 git-lfs \
 make \
 gzip \
 rename

RUN Rscript -e 'install.packages(c("mvtnorm", "matrixcalc", "igraph", "gplots", "Matrix"))' 
#RUN apt-get install -y libpoppler-cpp-dev 
###RUN apt-get install libapparmor-dev
#RUN apt-get install gfortran
#RUN apt-get install libmkl-rt
# gcc-7 g++-7 libx11-dev libglu1-mesa-dev libfreetype6-dev libgmp-dev glpk-utils
#RUN apt-get install -y r-cran-igraph r-cran-rglpk
#iN apt-get install libmkl-rt 

  

#RUN R -e "install.packages('stats');      if (!library(stats, logical.return=T)) quit(status=10)" 
#RUN R -e "install.packages('magrittr');      if (!library(magrittr, logical.return=T)) quit(status=10)" 
#RUN R -e "install.packages('remotes'); if (!library(remotes, logical.return=T)) quit(status=10)"
#RUN R -e "install.packages('assertthat');   if (!library(assertthat, logical.return=T)) quit(status=10)" 
#RUN R -e "install.packages('tibble'); if (!library(tibble, logical.return=T)) quit(status=10)"
#RUN R -e "install.packages('tidyr');      if (!library(tidyr, logical.return=T)) quit(status=10)" 
#RUN R -e "install.packages('dplyr'); if (!library(dplyr, logical.return=T)) quit(status=10)" 
#RUN R -e "install.packages('purrr');      if (!library(purrr, logical.return=T)) quit(status=10)" 
#RUN R -e "install.packages('readr');      if (!library(readr, logical.return=T)) quit(status=10)" 
#RUN R -e "install.packages('stringr');   if (!library(stringr, logical.return=T)) quit(status=10)"
#RUN R -e "install.packages('memoise');   if (!library(memoise, logical.return=T)) quit(status=10)" 
#RUN R -e "install.packages('progress'); if (!library(progress, logical.return=T)) quit(status=10)" 
#RUN R -e "install.packages('fuzzyjoin');   if (!library(fuzzyjoin, logical.return=T)) quit(status=10)" 
#RUN R -e "install.packages('Matrix');      if (!library(Matrix, logical.return=T)) quit(status=10)" 
RUN R -e "install.packages('igraph'); if (!library(igraph, logical.return=T)) quit(status=10)" 
#RUN R -e "remotes::install_github('speckerf/treemendous'); if (!library(treemendous, logical.return=T)) quit(status=10)"
