# The purpose of this script is to add new variables to the clean catalog (compute new indices)

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
requireNamespace("knitr")   # input and output of data



# ---- declare-globals ---------------------------------------------------------
path_input   <- "./model-output/physical-cognitive/1-catalog-clean"
path_save  <- "./model-output/physical-cognitive/2-catalog-wide"

# Get names of groups from "./scripts/mplus/model-components.R"
# list them here for demonstration, but refer to master copy for authority (READ THIS NOTE)
# PART 1 : model identifiers
variables_part_1 <- c(
  "model_number"
  ,"study_name"        # eas, elsa ...
  ,"subgroup"          # male & female
  ,"model_type"        # 0 , a, ae, aeh, aeh+, & full
  ,"process_a"         # fev, pef, grip
  ,"process_b"         # block, digits_f
)
# PART 2 : model information indices
variables_part_2 <- c(
  "subject_count"      # sample size
  ,"parameter_count"   # number of estimated parameters
  ,"wave_count"        # number of waves
  ,'ll'                # log-likelihood
  ,"aic"               # Akaike information criterion
  ,"bic"               # Bayesian information criterion
)
# PART 3: target variables, components in correlation computation
variables_part_6 <- c(
  # levels
  "ab_tau_00_est"       # covar betweew (a) - (b)
  ,"aa_tau_00_est"       # var (a)
  ,"bb_tau_00_est"       # var (b)
  # slopes
  ,"ab_tau_11_est"       # covar betweew (a) - (b)
  ,"aa_tau_11_est"       # var (a)
  ,"bb_tau_11_est"       # var (b)
  # residuals
  ,"ab_sigma_00_est"     # covar betweew (a) - (b)
  ,"a_sigma_00_est"      # var (a)
  ,"b_sigma_00_est"      # var (b)
)

# ---- load-data ---------------------------------------------------------------
catalog <- readr::read_csv(paste0(path_input,".csv"),col_names = TRUE)
catalog %>% glimpse(70)
# rm(path_input)
# ----- bivariate_test -----_________________________________________________
is_univariate <- grepl(pattern="^u\\d$", x=catalog$model_number)
is_bivariate <- grepl(pattern="^b\\d$", x=catalog$model_number)
testit::assert("The model number should match the univariate or bivariate pattern.", is_univariate | is_bivariate)
catalog$outcome_count <- ifelse(is_univariate, 1L, 2L)

# # create a small ds for testing and development
ds_small <- catalog %>%
  dplyr::filter(
    study_name == "octo"
    ,process_a  == "gait"
    # ,process_b  == "block"
    ,subgroup   == "female"
    ,model_type == "aehplus"
  ) %>%
  dplyr::select_(
    .dots=c(
       variables_part_1
      ,variables_part_2
      ,variables_part_6
    )
  )
 
# ------ conduct-computation ----------------------
# compute correlation coefficient from raw covariances using Fisher transform
alpha <- 0.05
z_alpha <- qnorm(1 - (alpha/2))
# ds <- ds_small %>%  # use for testing
ds <- catalog %>%   # use for full implementation
  dplyr::mutate(
    
     cr_levels_est      = ab_tau_00_est  / ( sqrt(aa_tau_00_est) * sqrt(bb_tau_00_est) )
    ,cr_levels_z        = atanh(cr_levels_est)
    ,cr_levels_ztest    = cr_levels_z * sqrt(subject_count - 3)
    ,cr_levels_zpval    = pnorm(-abs(cr_levels_z))*2
    ,cr_levels_zeta_lo  = cr_levels_z -  (z_alpha * sqrt( 1 / (subject_count - 3)))
    ,cr_levels_zeta_hi  = cr_levels_z +  (z_alpha * sqrt( 1 / (subject_count - 3)))
    ,cr_levels_ci95_lo  = tanh(cr_levels_zeta_lo)
    ,cr_levels_ci95_hi  = tanh(cr_levels_zeta_hi)
    
    ,cr_slopes_est      = ab_tau_11_est  / ( sqrt(aa_tau_11_est) * sqrt(bb_tau_11_est) )
    ,cr_slopes_z        = atanh(cr_slopes_est)
    ,cr_slopes_ztest    = cr_slopes_z * sqrt(subject_count - 3)
    ,cr_slopes_zpval    = pnorm(-abs(cr_slopes_ztest))*2
    ,cr_slopes_zeta_lo  = cr_slopes_z -  (z_alpha * sqrt( 1 / (subject_count - 3)))
    ,cr_slopes_zeta_hi  = cr_slopes_z +  (z_alpha * sqrt( 1 / (subject_count - 3)))
    ,cr_slopes_ci95_lo  = tanh(cr_slopes_zeta_lo)
    ,cr_slopes_ci95_hi  = tanh(cr_slopes_zeta_hi)
    
    ,cr_resid_est       = ab_sigma_00_est/ ( sqrt(a_sigma_00_est) * sqrt(b_sigma_00_est) )
    ,cr_resid_z         = atanh(cr_resid_est)
    ,cr_resid_ztest     = cr_resid_z * sqrt(subject_count - 3)
    ,cr_resid_zpval     = pnorm(-abs(cr_resid_ztest))*2
    ,cr_resid_zeta_lo   = cr_resid_z -  (z_alpha * sqrt( 1 / (subject_count - 3)))
    ,cr_resid_zeta_hi   = cr_resid_z +  (z_alpha * sqrt( 1 / (subject_count - 3)))
    ,cr_resid_ci95_lo   = tanh(cr_resid_zeta_lo)
    ,cr_resid_ci95_hi   = tanh(cr_resid_zeta_hi)
    
  ) #%>%
# t() %>%
# print()
# update the object with new computations
catalog <- ds

# ---- save-to-disk ------------------------------------------------------------
write.csv(catalog,  paste0(path_save,".csv"), row.names=F)

