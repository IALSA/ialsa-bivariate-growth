options(width=160)
rm(list=ls())
cat("\f")

library(dplyr)
library(tidyr)
library(ggplot2)


pathRoot <- getwd()
pathFolder <- file.path(pathRoot,"sandbox/syntax-creator/outputs")

requireNamespace("ggplot2")
requireNamespace("dplyr") #Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit")
requireNamespace("reshape2") # data transformations
requireNamespace("data.table") # data transformations
requireNamespace("MplusAutomation")
requireNamespace("stringr")
requireNamespace("IalsaSynthesis")


#e.g ab_TAU_00 <- c("ab_TAU_00_est", "ab_TAU_00_se", "ab_TAU_00_wald", "ab_TAU_00_pval")
source("./sandbox/syntax-creator/group-variables.R") # selected_results
# load functions that generate scripts
source("./sandbox/syntax-creator/functions-to-generate-Mplus-scripts.R")
# load functions that process the output files and create a summary dataset
source("./sandbox/syntax-creator/extraction-functions.R")



# create a object with main_theme definition
source("./scripts/graphs/theme-main.R")
# load graphical function
source("./scripts/graphs/kb-profiles-functions.R")


# point to  the folder with datasets containing model results

run_models_on <- FALSE # TRUE - run models, FALSE - only create script





## Run the lines above to load the needed functions
## Execute script snippets for each pair individually below this


############################################################ GRIP #####
## @knitr dummy_1
# Use the first example as the template for further pairs
saved_location <- "./sandbox/syntax-creator/outputs/grip-mmse"
prototype <-  "./sandbox/syntax-creator/prototype-map-wide.inp"
wave_set_modeled <-  c(1,2,3,4,5)
# source('./sandbox/syntax-creator/look-at-data.R') # create data for this outcome pair
# function below is defined in "./sandbox/syntax-creator/functions-to-generate-Mplus-scripts.R"
mplus_generator_bivariate(
  prototype = prototype # point to the template
  # saved_location == place_in
  , saved_location = saved_location # where to store all the .inp/.out scripts
  , process_a_name = 'grip'# item name of process (A)
  # remove mplus name , no need if data are prepped by R,
  , process_a_mplus = 'gripavg'# Mplus variable of process (A)
  , process_b_name = 'mmse'# item name of process (B)
  , process_b_mplus = 'mmse'# Mplus variable of process (B)
  , subgroup_sex = "male" # subset data to members of this group
  , subset_condition_1 = "dementia_ever NE 1" # subset data to member of this group
  , covariate_set = c("age_c70","htm_c160", "edu_c7")  # list of covariates ("_c" stands for "centercd)
  , wave_set_modeled =  wave_set_modeled # Integer vector of waves considered by the model, ie c(1,2,3,5,8).
  , run_models = FALSE # If TRUE then Mplus runs estimation to produce .out, .gh5, and/or, other files
) # execute to generate script


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




