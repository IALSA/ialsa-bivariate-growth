# the purpose of this script is to create a data catalog of all models extracted from Mplus outputs

# To stitch a basic html output from this code, run the line below. For elaborated report, run the corresponding .Rmd file
# knitr::stitch_rmd(script="./manipulation/physical-cognitive/0-parser.R", output="./manipulation/physical-cognitive/stitched-output/0-parser.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/mplus/group-variables.R")
source("./scripts/mplus/extraction-functions-auto.R")
source("./scripts/common-functions.R")

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes
library(MplusAutomation)
library(IalsaSynthesis)
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
requireNamespace("tidyr")   # data wrangling
requireNamespace("dplyr")   # avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit")  # asserting conditions meet expected patterns.
requireNamespace("readr")   # input and output of data
requireNamespace("knitr")   # input and output of data

# ---- declare-globals ---------------------------------------------------
path_folder <- "./model-output/physical-cognitive/studies/" # old scripts rely on "studies" to detect position in file address
path_save <- "./model-output/physical-cognitive/0-catalog-raw" # product of this script

# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2")
requireNamespace("tidyr")
requireNamespace("dplyr") #Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit") #For asserting conditions meet expected patterns.

# ---- utility-functions -------------------------------------------------
# functions local to this script are stored in this chunk
# takes in a vector of paths and collects model results from the .out file

collect_one_study <- function(
   lst          # list object (broken by studies) with paths to model outputs
  ,study        # name of the study to be passed for extraction
  ,column_names # character vector with names of items to be extracted from the output
  ,save_folder  # folder where the extracted results shall be placed
){
  ## Values for testing and development
  # lst          <- list_path_out
  # study        <- "nas"
  # column_names <- selected_results
  # save_folder  <- path_folder
  # paths_out    <- list_path_out[[study]]
  # str(lst$nas)
  # str(lst$octo)
  # create a file to be populated, this helps organize model output
  results <- data.frame(matrix(NA, ncol = length(column_names) ))
  names(results) <- column_names
  #  populate the `results` mold for all models of a given study
  for(i in seq_along(lst[[study]]) ){
    # for(i in 50:50){        # for testing and dev
    # i <- 3; study = "lasa"  # for testing and dev
    (collected <- collect_result(path = lst[[study]][i] ) )
    (collected_names <- names(collected))
    results[i, collected_names] <- collected
  }
  # save the parsed results near the raw outputs
  write.csv(results,  paste0(save_folder,"/0-parsed-results-",study,".csv"), row.names=F)
  return(results)
  # reacts wierdly to NAS, ivestigate
}
# Demonstrate usage:
# collect_one_study(
#    lst          = list_path_out                          
#   ,study        = "eas"                     
#   ,column_names = selected_results                       
#   ,save_folder  = path_folder                     
# )
# path <- "./model-output/physical-cognitive/studies//lasa/grip_max-coding_max/b1_female_a_grip_max_coding_max.out"
# path <- "./model-output/physical-cognitive/studies/eas/Gait/b1_female_ae_walking_executive_gait_trailsb.out"
# path <- "./model-output/physical-cognitive/studies/lasa/physical-cognitive/b1_female_aehplus_gait_codingtask.out"
# file.exists(path)
# a <- collect_result(path)
# scan(path, what='character', sep='\n') # each line of output as a char value


# path <- "./model-output/physical-cognitive/studies//map/fev-bnt/b1_male_a_fev_bnt.out"
# path <- "./model-output/physical-cognitive/studies//map/fev100-wordlistim/b1_female_a_fev100_wordlistim.out"
# path <- list_path_out[["lasa"]][1]
# path <- list_path_out[["nuage"]][1]
# path <- list_path_out[["octo"]][1]



# ---- load-data ---------------------------------------------------------------
# create a list object broken by study, containing paths to model outputs
list_path_out <-list(
  # manual estimation (outputs provided "as is" from the study drivers)
   "eas"   = list.files(file.path(path_folder,"eas")  ,full.names=T, recursive=T, pattern="out$")
  ,"elsa"  = list.files(file.path(path_folder,"elsa") ,full.names=T, recursive=T, pattern="out$")
  ,"hrs"   = list.files(file.path(path_folder,"hrs")  ,full.names=T, recursive=T, pattern="out$")
  ,"ilse"  = list.files(file.path(path_folder,"ilse") ,full.names=T, recursive=T, pattern="out$")
  ,"nas"   = list.files(file.path(path_folder,"nas"),  full.names=T, recursive=T, pattern="out$")
  ,"nuage" = list.files(file.path(path_folder,"nuage"),full.names=T, recursive=T, pattern="out$")
  ,"satsa" = list.files(file.path(path_folder,"satsa"),full.names=T, recursive=T, pattern="out$")
  ,"lasa"  = list.files(file.path(path_folder,"lasa") ,full.names=T, recursive=T, pattern="out$") # Cambridge version
  # script estimation (outputs generated using IalsaSynthesis package, using datasets provided by IALSA Study Curator) 
  ,"map"   = list.files(file.path(path_folder,"map")  ,full.names=T, recursive=T, pattern="out$")
  ,"octo"  = list.files(file.path(path_folder,"octo") ,full.names=T, recursive=T, pattern="out$")
)

# ---- inspect-data ------------------------------------------------
# list_path_out[["elsa"]]
# path <- list_path_out[["lasa"]][1]
# path <- list_path_out[["map"]][1]
# path <- list_path_out[["nuage"]][1]

# ---- tweak-data --------------------------------------------------
# remove models that did not terminate normaly
# at this point, detection is manual
# a <- list_path_out[["lasa"]]
# b <- a[grepl("b1_female_aehplus_grip_gait.out$|b1_male_aehplus_grip_gait.out$", a)]
# list_path_out[["elsa"]] <- setdiff(list_path_out[["elsa"]], b)


##########################
# N   O   T   I   C   E  #
##########################

# AT THIS POINT THE EXTRACTION BREAKS DOWN ON LASA AND MAP
# THE RAW CATALOG HAS BEEN TAKEN FROM PORTLAND-2015
# RETURN HERE WHEN HAVE MORE TIME TO DEBUG EXTRACTION


# ---- parse-model-outputs --------------------------------------------
# do not combine into a loop for convenient, disjoint usage
# manual estimation (outputs provided "as is" from the study drivers)

# NOTICE: at this point we suspect the support of extraction from manually estimated outputs
# due to MplusAutomation version change, it unfeasble to support both extraction routines
# Rely on the existed extrations products to support publication. 

# collect_one_study(list_path_out,"eas",   selected_results, path_folder)
# collect_one_study(list_path_out,"elsa",  selected_results, path_folder)
# collect_one_study(list_path_out,"hrs",   selected_results, path_folder)
# collect_one_study(list_path_out,"ilse",  selected_results, path_folder)
# collect_one_study(list_path_out,"nas",  selected_results, path_folder)
# collect_one_study(list_path_out,"nuage", selected_results, path_folder)
# collect_one_study(list_path_out,"satsa", selected_results, path_folder)
# collect_one_study(list_path_out,"lasa",  selected_results, path_folder)

# script estimation (outputs generated using IalsaSynthesis package, using datasets provided by IALSA Study Curator)
# collect_one_study(list_path_out,"lasa",  selected_results, path_folder) # not there yet
# collect_one_study(list_path_out,"map",   selected_results, path_folder)
# collect_one_study(list_path_out,"octo",  selected_results, path_folder)

# ---- assemble-catalog ---------------------------------------------------------
# create a path to the folder one step above from the study specific within project
a <- strsplit(path_folder,"/")[[1]]
b <- a[1:(length(a)-1)]
path_one_up <- paste0(paste(b,collapse="/"),"/")
# list the files that conform to a certain structure
(combine_studies <- list.files(path_folder, pattern = "^0-parsed-results-\\w+\\.csv$", full.names =T) )
# create a 
dtos <- list()
for(i in seq_along(combine_studies)){
  dtos[[i]] <- read.csv(combine_studies[i], header=T, stringsAsFactors = F)
}
# combine results files from each study
catalog <- dplyr::bind_rows(dtos)

# ---- save-to-disk ------------------------------------------------------------
write.csv(catalog,  paste0(path_save,".csv"), row.names=F)



