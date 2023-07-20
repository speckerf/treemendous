FROM rocker/verse:latest
RUN R -e "install.packages('remotes'); if (!library(remotes, logical.return=T)) quit(status=10)"
RUN Rscript -e "remotes::install_github('speckerf/treemendous'); if (!library(treemendous, logical.return=T)) quit(status=10)"


