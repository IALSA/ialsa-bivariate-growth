# The purpose of this script is to create a data object (dto) which will hold all data and metadata.
# Run the lines below to stitch a basic html output.
# knitr::stitch_rmd(
#   script="./manipulation/0-ellis-island.R",
#   output="./manipulation/stitched-output/0-ellis-island.md"
# )
# The above lines are executed only when the file is run in RStudio, !! NOT when an Rmd/Rnw file calls it !!

############################
##  Land on Ellis Island  ##
############################

rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console

# ---- load-sources ------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  
# Ideally, no real operations are performed in these sourced scripts. 
source("./scripts/functions-common.R") # used in multiple reports

# ---- load-packages ----------------------------------------------
# Attach packages so their functions don't need to be qualified when used
# See more : http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # Pipes

# Functions of these packages will need to be qualified when used
# See more: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("tidyr") #  data manipulation
requireNamespace("dplyr") # f() names conflict with other packages (esp. base, stats, and plyr).
requireNamespace("sas7bdat") # for inputing SAS files
# ---- declare-globals ----------------------------------------------



# ---- load-data ------------------------------------------------
# load data objects
ds0 <- sas7bdat::read.sas7bdat("./data/unshared/raw/octo/octomult_151015.sas7bdat")


baseline_measures <- c(
   "Case"   # unique person identifier
  ,"PairID" # twin pair
  ,"TwinID" # twin number
  ,"Zygosity"
  ,"BirthDate"
  ,"DeadDate"
  ,"Female"
  ,"Educyrs"
  ,"SESgrp"
  ,"Smoke"
  ,"YTDead"
  ,"DemEver"
)

longitudinal_measures <- c(
   "time"    # Exact Years in Study Based on Entered Age
  ,"CompAge" # composite age
  ,"CVD"     # cardi-vascular disease
  ,"diabYN"  # diabetes (Yes/No)
  
  ,"height"  # (cm)
  ,"weight"  # (kg)
  ,"gait3m"  # Normal Walk & turn, 3m, in sec
  ,"gripp"   # Grip Strength in Pounds
  ,"pek"     # Pulmonary Expiratory Function
  
  ,"prose"   # prose recall
  ,"block"   # Block Design
  ,"figure"  # Figure Reasoning
  ,"digsym"  # Digit Symbol
  ,"mmse"    # mini mental status exam
)

ls <- list()
for(i in seq_along(longitudinal_measures)){
  ls[[i]] <- paste0(longitudinal_measures[i],1:5)
}
longitudinal_measures <- unlist(ls)

ds <- ds0 %>% dplyr::select_(.dots = c(baseline_measures, longitudinal_measures))
ds <- as.data.frame(ds)

table(ds$Educyrs, useNA = "always")
str(ds)

write.table(ds,"./data/unshared/derived/octo/octomult_151015.dat", row.names=F, col.names=F)
write(names(ds), "./data/unshared/derived/octo/variable-names.txt", sep=" ")


