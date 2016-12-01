# knitr::stitch_rmd(script="./___/___.R", output="./___/stitched-output/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.

#e.g ab_TAU_00 <- c("ab_TAU_00_est", "ab_TAU_00_se", "ab_TAU_00_wald", "ab_TAU_00_pval")
source("./sandbox/pipeline-demo-1/group-variables.R") # selected_results
# load functions that generate scripts
source("./sandbox/pipeline-demo-1/functions-to-generate-Mplus-scripts.R")
# load functions that process the output files and create a summary dataset
source("./sandbox/pipeline-demo-1/extraction-functions.R")

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>% 
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
requireNamespace("tidyr") # data manipulation
requireNamespace("dplyr") # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit")# For asserting conditions meet expected patterns.
# requireNamespace("car") # For it's `recode()` function.
requireNamespace("reshape2") # data transformations
requireNamespace("data.table") # data transformations
requireNamespace("MplusAutomation")
requireNamespace("stringr")
requireNamespace("IalsaSynthesis")

# ---- declare-globals ---------------------------------------------------------
options(width=160)
path_output        <- "./sandbox/pipeline-demo-1/outputs/"
path_generic_data  <- "./data/unshared/derived/map-1/wide-dataset.dat"
path_generic_names <- "./data/unshared/derived/map-1/wide-variable-names.txt"

# ---- load-data ---------------------------------------------------------------
ds_long <- readRDS("./data/unshared/derived/map-1/data-long.rds")
ds_wide <- readRDS("./data/unshared/derived/map-1/data-wide.rds")


testit::assert("File does not exist",file.exists(path_generic_data)) 
testit::assert("File does not exist",file.exists(path_generic_names)) 

# file.copy(from=path_generic_names,to= "./sandbox/pipeline-demo-1/outputs/",overwrite = T)

# ---- inspect-data -------------------------------------------------------------

# ---- tweak-data --------------------------------------------------------------

# ---- basic-table --------------------------------------------------------------

# ---- basic-graph --------------------------------------------------------------



## Run the lines above to load the needed functions
## Execute script snippets for each pair individually below this
# ---- create-predictor-selector -----------------------------
ls_model_number <- list(
  "univariate_flat"      = "u0",
  "univariate_linear"    = "u1",
  "univariate_quadratic" = "u2",
  "bivariate_flat"       = "b0",
  "bivariate_linear"     = "b1",
  "bivariate_quadratic"  = "b2"
)
ls_subgroup = list(
  "male" = "male",
  "female" = "female"
  #"unisex" = "unisex"
)
ls_model_type <- list( 
   "a"   = c("age_c70")
  ,"ah"  = c("age_c70","edu_c7")
  ,"aeh" = c("age_c70","edu_c7","htm_c")
  ,"aehplus" = c("age_c70","edu_c7","htm_c", "smoke","heart","diabetes")
  ,"aeplus" = c("age_c70","edu_c7", "smoke","heart","diabetes")
) 


############################################################ GRIP #####
## @knitr dummy_1
# Use the first example as the template for further pairs

wave_set_modeled <-  c(1,2,3,4,5)
subset_condition_1 <- "dementia_ever NE 1"
folder_data        = "./data/unshared/derived/map-1"
path_prototype     = "./sandbox/pipeline-demo-1/prototype-wide.inp"
folder_output      = "./sandbox/pipeline-demo-1/outputs/" 
# folder_data        = "./data/unshared/derived/map"
# folder_output      = "./output/studies/map/phys-cog/pulmonary" 


mplus_generator_bivariate(
   model_number       = "b1"
  ,subgroup           = "female"
  ,model_type         = "aehplus"
  ,process_a_name     = 'fev'# item name of process (A), goes into file name
  ,process_a_mplus    = 'fev'# identifies the variable in Mplus
  ,process_b_name     = 'bostordel'# item name of process (B), goes into file name
  ,process_b_mplus    = 'bostordel'# identifies the variable in Mplus
  ,covariate_set      = ls_model_type[["aehplus"]]
  ,wave_set_modeled   = wave_set_modeled 
  ,subset_condition_1 = subset_condition_1 # subset data to member of this group
  ,path_prototype     = path_prototype
  ,folder_data        = folder_data
  ,folder_output      = folder_output
  ,run_models         = TRUE # If TRUE then Mplus runs estimation to produce .out, .gh5, and/or, other files
)

# ---- examine-created-output ----------------
source("./scripts/mplus/mplus.R") # downloaded from http://www.statmodel.com/mplus-R/mplus.R
path_gh5 <- "./sandbox/syntax-creator/outputs/grip-mmse/male_5.gh5"

# view options: https://www.statmodel.com/mplus-R/GH5_R.shtml

mplus.list.variables(path_gh5) # variables in the gh5 file
mplus.view.plots(path_gh5)  # available graphs for this type of gh5 file
# histograms
mplus.plot.histogram(path_gh5, "SA") # slope of process A
mplus.plot.histogram(path_gh5, "SB") # slope of process B
# scatterplots
mplus.plot.scatterplot(path_gh5, "IA", "IB") # intercepts
mplus.plot.scatterplot(path_gh5, "SA", "SB") # slopes
mplus.plot.scatterplot(path_gh5, "IA", "SA") # physical
mplus.plot.scatterplot(path_gh5, "IB", "SB") # cognitive

ds <- mplus.get.data(path_gh5, "SA")

summary(ds)
head(ds)

#### ----- development ----------------------

# Grip - Boston Naming Task #
# # from "./sandbox/syntax-creator/extraction_functions.R  script
# collect_model_results(folder = "outputs/pairs/grip_bnt") # collect and save into the same folder
# ds <- readRDS(file.path(pathFolder,"grip_bnt.rds")) # load the data for outcome pair
# # from "./scripts/graphs/koval_brown_profiles.R"
# kb_profiles(ds,  vertical="wave_count",  border=5) # produces the kb_profile graph
#




