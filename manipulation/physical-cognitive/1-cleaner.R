# The purpose of this script is to clean the raw catalog

# run the line below to stitch a basic html output. For elaborated report, run the corresponding .Rmd file
# knitr::stitch_rmd(script="./manipulation/0-ellis-island.R", output="./manipulation/stitched-output/0-ellis-island.md")
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
library(knitr)
library(dplyr)
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
requireNamespace("tidyr")   # data wrangling
requireNamespace("dplyr")   # avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit")  # asserting conditions meet expected patterns.
requireNamespace("readr")   # input and output of data


# ---- declare-globals ---------------------------------------------------------
path_input   <- "./model-output/physical-cognitive/0-catalog-raw"
path_save  <- "./model-output/physical-cognitive/1-catalog-clean"

# ---- utility-functions -------------------------------------------------------
# functions local to this script go here. 

# repeat the definition here so that you can replace default to html=F
# adds neat styling to your knitr table
neat <- function(x, html = F , ...){
  # browser()
  # knitr.table.format = output_format
  if(!html){
    x_t <- knitr::kable(x, format = "pandoc", row.names = F,...)
  }else{
    x_t <- x %>%
      # x %>%
      # knitr::kable() %>%
      knitr::kable(format="html",row.names = F, ...) %>%
      kableExtra::kable_styling(
        bootstrap_options = c("striped", "hover", "condensed","responsive"),
        # bootstrap_options = c( "condensed"),
        full_width = F,
        position = "left"
      )
  }
  return(x_t)
}# usage:
# nt <- t %>% neat()

# ---- load-data ---------------------------------------------------------------
# catalog <- read.csv(paste0(path_input,".csv"), header = T,  stringsAsFactors=FALSE)
catalog <- readr::read_csv(paste0(path_input,".csv"),col_names = TRUE)

# ---- tweak-data --------------------------------------------------------------
colnames(catalog)
ds <- catalog %>% dplyr::arrange_("model_type", "process_a") %>%
  dplyr::select_("study_name", "model_number","subgroup","model_type","process_a", "process_b")
# assign an alias for quick reference in this script
ds <- catalog
# ----- load-rename-classify-mapping -------------------------------------
ds_rules <- read.csv("./data/public/raw/rename-classify-rules.csv", stringsAsFactors = F) %>%
  dplyr::select(-notes,-mplus_name)


# ---- spell_model_number ------------------------------------------------------
t <- table(ds$model_number, ds$study_name);t[t==0]<-".";t

# ---- spell-subgroup ---------------------------------------------------------
t <- table(ds$subgroup, ds$study_name);t[t==0]<-".";t

# ---- spell-model_type -------------------------------------------
t <- table(ds$model_type, ds$study_name);t[t==0]<-".";t # old values
# ---- correct-model_type ------------------------------------------------------
# extract the specific renaming rule
d_rule <- ds_rules %>%
  dplyr::filter(category == "model_type") %>%
  dplyr::select(entry_raw, entry_new)
d_rule
# join the model data frame to the conversion data frame.
ds <- ds %>%
  dplyr::left_join(d_rule, by=c("model_type"="entry_raw"))
# verify
t <- table(ds$entry_new, ds$study_name);t[t==0]<-".";t # what the new values would look like
t <- table(ds$model_type, ds$entry_new);t[t==0]<-".";t # raw rows, new columns
# the table above shows how raw (old) values will be transformed into new
# Replace raw entries with new entries
ds <- ds %>%
  dplyr::mutate_("model_type"="entry_new") %>%
  dplyr::select(-entry_new)
t <- table(ds$model_type, ds$study_name); t[t==0]<-"."; t

# ---- spell-process_a -------------------------------------------------
t <- table(ds$process_a, ds$study_name); t[t==0]<-"."; t
# ---- correct-process_a ------------------------------------------------
# extract the specific renaming rule
d_rule <- ds_rules %>%
  dplyr::filter(category == "physical") %>%
  dplyr::select(entry_raw, entry_new)
d_rule
# join the model data frame to the conversion data frame.
ds <- ds %>%
  dplyr::left_join(d_rule, by=c("process_a"="entry_raw")) #%>% 
  # as.data.frame()
# ---- test-process_a ------------------------------------------------
# verify
t <- table(ds$entry_new, ds$study_name);t[t==0]<-".";t
# Remove the old variable, and rename the cleaned/condensed variable.
ds <- ds %>%
  dplyr::select(-process_a) %>% #dplyr::filter(model_number == "b1") %>%
  dplyr::rename_("process_a"="entry_new") # name correction
# verify
t <- table(ds$process_a, ds$study_name); t[t==0]<-"."; t

# ---- spell-process_b -------------------------------------------------
t <- table(ds$process_b, ds$study_name); t[t==0]<-"."; t
d <- ds %>%
  dplyr::group_by_("study_name","process_b") %>%
  dplyr::summarize(count=n()) %>%
  dplyr::ungroup() #%>%
  # dplyr::arrange_("study_name")
# d %>% neat(html=F,align = c("r","r","r"))
knitr::kable(d %>% dplyr::arrange_("study_name"))
knitr::kable(d %>% dplyr::arrange_("process_b"))
# ---- correct-process_b ------------------------------------------------
# extract the specific renaming rule
d_rule <- ds_rules %>%
  dplyr::filter(category == "cognitive") %>%
  dplyr::select(entry_raw,entry_new,label_cell,label_row, domain )
d_rule # see greater context in  ./data/public/raw/rename-classify-rules.csv
# join the model data frame to the conversion data frame.
ds <- ds %>%
  dplyr::left_join(d_rule, by=c("process_b"="entry_raw"))
# verify
t <- table(ds$entry_new,  ds$study_name);t[t==0]<-".";t
t <- table(ds$label_cell, ds$study_name);t[t==0]<-".";t
t <- table(ds$label_row,  ds$study_name);t[t==0]<-".";t
t <- table(ds$domain,     ds$study_name);t[t==0]<-".";t
# count models for the combination of each:
d <- ds %>%
  dplyr::group_by_("study_name","process_b") %>%
  dplyr::summarize(count=n()) %>%
  dplyr::ungroup() 
# arrange differently for inspection
d %>% arrange_("study_name") %>% kable()
d %>% arrange_("process_b") %>% kable()

# Remove the old variable, and rename the cleaned/condensed variable.
ds <- ds %>%
  dplyr::select(-process_b) %>% # contains the raw values that we recoded
  dplyr::rename_("process_b"        = "entry_new")  %>% # replace with new
  dplyr::rename_("process_b_cell"   = "label_cell") %>% # value in the cell, name of the test
  dplyr::rename_("process_b_row"    = "label_row")  %>% # label of the row, main descriptive label identifying a measure
  dplyr::rename_("process_b_domain" = "domain")         # color of the cell
 
# ---- test-process_b --------------------------------------
# verify
t <- table(ds$process_b,        ds$study_name, useNA = "always"); t[t==0]<-"."; t 
t <- table(ds$process_b_cell,   ds$study_name, useNA = "always"); t[t==0]<-"."; t
t <- table(ds$process_b_row,    ds$study_name, useNA = "always"); t[t==0]<-"."; t
t <- table(ds$process_b_domain, ds$study_name, useNA = "always"); t[t==0]<-"."; t
# count models for the combination of each:
d <- ds %>%
  dplyr::group_by_(
     "study_name"
    ,"process_b"
    ,"process_b_cell"
    ,"process_b_row"
    ,"process_b_domain"
  ) %>%
  dplyr::summarize(count=n()) %>%
  dplyr::ungroup() 
# final inspection
d %>% arrange_("study_name") %>% kable()
d %>% arrange_("process_b") %>% kable()
d %>% arrange_("process_b_cell") %>% kable()
d %>% arrange_("process_b_row") %>% kable()
d %>% arrange_("process_b_domain") %>% kable()
# to pick up where and how the columns differ, explore the code below
#d %>% 
#   dplyr::mutate(
#     test_same = ifelse(process_b==process_b_cell,T,F)
#   ) 

# ---- standardize-names -------------------------
ds %>% glimpse(70)
catalog <- rename_columns_in_catalog(ds)
catalog %>% glimpse(70)

# ---- remove-unwanted-measures --------------------
# to avoid removing case in each report, do it here
# see https://github.com/IALSA/IALSA-2015-Portland/issues/152#issuecomment-268622007
catalog <- catalog %>%
  dplyr::filter(!(study_name == 'hrs' & process_b == "tics")) %>%
  # correct the spellings of specific tests
  dplyr::mutate(
    # some of the measure require corrections at the study level
    process_b = ifelse(study_name == "map"  & process_b == "matrices", "raven_standard", process_b),
    process_b = ifelse(study_name == "lasa" & process_b == "raven",    "raven_color_ab", process_b)
  ) #

# ---- save-to-disk ------------------------------------------------------------
write.csv(catalog,  paste0(path_save,".csv"), row.names=F)



