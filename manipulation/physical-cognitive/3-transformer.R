# The purpose of this script is to transform the wide catalog into long catalog.

# run the line below to stitch a basic html output. For elaborated report, run the corresponding .Rmd file
# knitr::stitch_rmd(script="./manipulation/0-ellis-island.R", output="./manipulation/stitched-output/0-ellis-island.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/mplus/group-variables.R")
source("./scripts/mplus/model-components.R")


# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
requireNamespace("tidyr")   # data wrangling
requireNamespace("dplyr")   # avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit")  # asserting conditions meet expected patterns.
requireNamespace("readr")   # input and output of data




# ---- declare-globals ---------------------------------------------------------
path_input   <- "./model-output/physical-cognitive/2-catalog-wide"
path_save  <- "./model-output/physical-cognitive/3-catalog-long"

# the structure of coefficient suffixes
regex_general <- "^(a|b|aa|bb|ab|er|cr)_(\\w+)_(\\d{2})_(est|se|wald|pval|ci95_lower|ci95_upper)$"

# ---- utility-finctions ---------------------------------------------------------
coefficient_of_variation <- function(x)( sd(x)/mean(x) )

# ---- load-data ---------------------------------------------------------------
catalog_wide <- readr::read_csv(paste0(path_input,".csv"),col_names = TRUE)



# domain_renaming_stencil <- readr::read_csv("./reports/correlation-3/pulmonary-domain-structure-dead.csv")
domain_renaming_stencil <- readr::read_csv("./data/public/raw/pulmonary-domain-structure-dead.csv")
domain_renaming_stencil %>% glimpse()
# ---- tweak-data --------------------------------------------------------------
# extract a single model for inspection and testing
ds <- catalog_wide %>%
  dplyr::filter(
     study_name == "octo"
    ,process_a  == "grip"
    # ,process_b  == "gait"
    ,subgroup   == "female"
    ,model_type == "aehplus"
  )
ds %>% dplyr::glimpse()

# ---- elongate-catalog-wide -------------------------------------------------
# ds_long <- ds %>%         # use when testing
ds_long <- catalog_wide %>% # use when running
  dplyr::select_(
    .dots=c(
      model_components[["id"]]
      ,model_components[["process_id"]]
      ,model_components[["info"]]
      ,model_components[["fixed"]]
      ,model_components[["random"]]
      ,model_components[["estimated_r"]]
      # ,model_components[["computed_r"]]
      # ,model_components[["compute_corr"]]
    )
  )  %>%
  dplyr::filter( !is.na(process_a) & !is.na(process_b) ) %>%  # remove univariate models
  dplyr::filter( model_number %in% c("b1")) %>%    # same as above, remove univariate models, but more restrictive
  dplyr::filter( process_a!="nophys" & process_b!="nocog" ) %>% # remove univariate models
  tidyr::gather_("g", "value", c(
    model_components[["fixed"]]
    ,model_components[["random"]]
    ,model_components[["estimated_r"]]
  )
  ) %>% # BISR + covariates
  dplyr::mutate(
    process      = gsub(regex_general, "\\1", g, perl=T),
    coefficient  = gsub(regex_general, "\\2", g, perl=T),
    subindex     = gsub(regex_general, "\\3", g, perl=T),
    stat         = gsub(regex_general, "\\4", g, perl=T)
  )
ds_long %>% dplyr::glimpse()

# impose specific domain structure
ds_long <- ds_long %>%
  dplyr::left_join(domain_renaming_stencil, by = c("study_name", "process_b","process_b_domain")) %>%
  dplyr::mutate(process_b_domain = process_b_domain_new) %>% # overwrite with new values
  dplyr::select(-process_b_domain_new)   # remove dublicated columns
ds_long %>% dplyr::glimpse()

d <- ds_long %>% filter(is.na(process_b_label))



# ---- table-dynamic-long ----------------------------------------
# inspect the created object via dynamic table
# ds_long %>%
#   dplyr::mutate(
#     # study_name    = factor(study_name),
#     process_a      = factor(process_a),
#     process_b      = factor(process_b),
#     process        = factor(process),
#     subgroup       = factor(subgroup),
#     coefficient    = factor(coefficient),
#     stat           = factor(stat)
#   ) %>%
  # dplyr::filter(model_type=="aehplus" & subgroup=="female" & process_a=="grip") %>%
  # dplyr::filter(model_type=="aehplus" & subgroup=="female" & process_a=="grip" & study_name =="map") %>%
  # dplyr::select(-g, -model_type, -subgroup, -process_a, -parameter_count) %>%
  # DT::datatable(
  #   class     = 'cell-border stripe',
  #   caption   = "Growth Curve Model Solution --Long Format",
  #   filter    = "top",
  #   options   = list(pageLength = 6, autoWidth = TRUE)
  # )

# ---- remove-duplicates ----------------------------------------
define_duplicates <- c(  model_components[["id"]]
                         ,model_components[["process_id"]]
                         ,model_components[["info"]]
                         ,"process", "coefficient","subindex", "stat"
) # all, but "value"
ds_no_duplicates <- ds_long %>%
  dplyr::group_by_( .dots  = define_duplicates ) %>%
  dplyr::summarize( value  = mean(value, na.rm=T) ) %>%
  dplyr::ungroup()
# compile dataframe of dublicate rows, should have 0
ds_find_duplicates <- ds_long %>%
  dplyr::distinct() %>% #Drops it from 256 rows to 56 rows.
  dplyr::group_by_( .dots  = define_duplicates ) %>%
  dplyr::filter(!is.na(value)) %>% #Drops from 56 rows to 8 rows.  !!Careful that you don't remove legit NAs (esp, in nonduplicated rows).
  dplyr::summarize(
    count      = n(),
    values     = paste(value, collapse=";"),
    value_cv   = coefficient_of_variation(value)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(1<count) %>%
  dplyr::filter(.001 < value_cv) #Drops from 8 to 0 rows.

testit::assert("Pool contains duplicates", !nrow(ds_find_duplicates)>0L)

# testit::assert("No meaningful duplicate rows should exist.", nrow(ds_find_duplicates)==0L)
# rm(variables_part_1, variables_part_2, ds_find_duplicates)

# ---- assemble-catalog ------------------------------------------------------------------
catalog_long <- ds_no_duplicates %>%
  tidyr::spread(key=stat, value=value)# %>%

# ---- save-to-disk ------------------------------------------------------------
write.csv(catalog_long,  paste0(path_save,".csv"), row.names=F)
