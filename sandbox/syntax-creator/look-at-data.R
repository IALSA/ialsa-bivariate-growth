# knitr::stitch_rmd(script="./manipulation/car-ellis.R", output="./manipulation/stitched-output/car-ellis.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
# rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
# cat("\f") # cleans console

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
base::source("./scripts/functions-common.R")
# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes

# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2")
requireNamespace("dplyr") #Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit")
requireNamespace("reshape2") # data transformations
loadNamespace("data.table") # data transformations

# ---- declare-globals ---------------------------------------------------------
# path_input  <- "./data/unshared/raw/map/ds0.rds"
path_input  <- "../MAP/data-unshared/derived/dto.rds"
# path_input  <- "./data-unshared/raw/map/ds0.rds"
# figure_path <- 'manipulation/stitched-output/te/'

# put test assert here to check the connection.


# ---- load-data ---------------------------------------------------------------
dto <- readRDS(path_input)
ds <- dto$unitData
str(ds)
ds %>% dplyr::count(study)
ds <- ds %>% dplyr::filter(study == "MAP ")# keep only MAP
ds %>% dplyr::count(study)


# ----- select_subset ------------------------------------
# select variables you will need for modeling, be conservative
selected_items <- c(
  "id", # personal identifier
  "age_bl", #Age at baseline
  "htm", # Height(meters)
  "wtkg", # Weight (kilograms)
  "msex", # Gender
  "race", # Participant's race
  "educ", # Years of education

  # time-invariant above
  "fu_year", # Follow-up year ---###---
  # time-variant below

  "age_at_visit", #Age at cycle - fractional
  "dementia", # Dementia diagnosis

  "cts_bname", # Boston naming - 2014
  "mmse",  # Mini mental status examination
  "cts_nccrtd", #  Number comparison - 2014

  "fev", # forced expiratory volume
  "gait_speed", # Gait Speed - MAP
  "gripavg" # Extremity strength
)

# ---- compute_time_difference -------------------------
d <- as.data.frame(ds[ , selected_items])
# d <- d[1:10 , c("id","age_bl", "fu_year", "age_at_visit")]
# d

d <- d %>%
  dplyr::group_by(id) %>%
  dplyr::arrange(fu_year) %>%
  dplyr::mutate(
    time_since_bl = (age_at_visit - dplyr::lag(age_at_visit, 1))
  ) %>%
  dplyr::ungroup()

# ---- compute_subsetting_conditions ---------------------

d <- d %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(dementia_ever = any(dementia==1)) %>%
  dplyr::ungroup() #%>%
#dplyr::filter(dementia_ever %in% c(FALSE, NA))

d <- as.data.frame(d)
table(d$dementia_ever, useNA="always")
d$dementia_ever <- as.numeric(d$dementia_ever)
d <- dplyr::tbl_df(d)

# ---- center_covariates ---------------------------------
d <- d %>%
  dplyr::mutate(age_c70 = age_bl - 70)  %>%
  dplyr::mutate(htm_c160 = htm - 1.6)  %>%
  dplyr::mutate(edu_c7 = educ - 7)

d %>% dplyr::glimpse()


# ---- missing_values -----------------------------
# remove observations with missing values on the time variable
table(d$fu_year, useNA = "always")
d <- d %>% dplyr::filter(!is.na(fu_year))
table(d$fu_year, useNA = "always")

# ---- long_to_wide -----------------------------------------
# long to wide conversion might rely on the classification given to the variables with respect to time : variant or invariant
# should this classification be manual or automatic?

dw <- data.table::dcast(data.table::setDT(d), id + age_bl + age_c70 + htm + htm_c160 + wtkg + msex + race + educ + edu_c7 + dementia_ever ~ fu_year, value.var = c(
  "age_at_visit", #Age at cycle - fractional
  "time_since_bl", # time elapsed since the baseline
  "dementia", # Dementia diagnosis

  "cts_bname", # Boston naming - 2014
  "mmse",  # Mini mental status examination
  "cts_nccrtd", #  Number comparison - 2014

  "fev", # forced expiratory volume
  "gait_speed", # Gait Speed - MAP
  "gripavg" # Extremity strength


))

# recode missing values


# set.seed(42)
# random_subset <- sample(unique(dw$id), size = 500)
# dw <- dw[d$id %in% random_subset, ]

dw[is.na(dw)] <- -9999
dw %>% dplyr::glimpse()
table(dw$age_centered_70, useNA = "always")



# dw %>% dplyr::glimpse()

# ---- export_data -------------------------------------
# At this point we would like to export the data in .dat format
# to be fed to Mplus for any subsequent modeling
saved_location <- "./sandbox/syntax-creator/outputs/grip-mmse/"

write.table(d,paste0(saved_location,"/long-dataset.dat"), row.names=F, col.names=F)
write(names(d), paste0(saved_location,"/long-variable-names.txt"), sep=" ")

write.table(dw, paste0(saved_location,"/wide-dataset.dat"), row.names=F, col.names=F)
write(names(dw), paste0(saved_location,"/wide-variable-names.txt"), sep=" ")
