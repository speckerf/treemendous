# Workflow to create package in data

All interal datasets must be created first, then they have to be loaded and stored together in sysdata. 

- WFO.R, GBIF.R, WCVP.R: Create WFO, GBIF, WCVP cleaned databases and store them in add\_sysdata 
- Backbone-Trees.R: Create Backbone.Trees from WFO, GBIF, WCVP, BGCI
- Create fuzzy matching edges for every entry in Backbone.Trees by running edges\_fuzzy\_matched.R
- Save GBIF, WFO, WCVP, and edges\_fuzzy\_matched into R/sysdata.rds by running sysdata.R

