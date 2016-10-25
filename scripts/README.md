`./scripts/` Directory
=========

This directory contains scripts referenced throughout the repository. They are clustered into folders according the to context of usage.

- `functions-common.R` - scripts used throughout the repository. 

### Graphing scripts
- `graphs/graphs-general.R` - primitive graphing functions
- `graphs/graphs-presets.R` - settings used in graphs throughout the repository
- `theme-main.R` - ggplot theme used as the default in graphs 
- `graphs/graphs-specific.R` - complex particular graphs, usually unique to the repository


###  Publishing scripts
- `reports/report-starter.R`  - source file for dymanic report
- `reports/report-starter.Rmd` - form file for th dynamic report
- `reports/report-starter-blank.R` - equivalent to `report-starter.R`, but without examples. Sometimes it is easier to start with a blank page
- `reports/runMany.R` - published the specified `.Rmd` files
