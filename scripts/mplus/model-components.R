# These object definitions are used throughout the repository
# All of these are stored in the object `model_components`

# This object is created at the end of this script:
#   component           label                         list of column names
model_components <- list(
   "id"          = NA # Model identifiers           - variables_part_1
  ,"info"        = NA # Model information           - variables_part_2
  ,"fixed"       = NA # Fixed effects (gamma)       - variables_part_3
  ,"fixed_key"   = NA # Order of predictors         - coefficient_key
  ,"random"      = NA # Random effects (tau)        - c(variables_part_4a, variables_part_4b)
  ,"residual"    = NA # Residuals (sigma)           - c(variables_part_4c, variables_part_4d)
  ,"estimated_r" = NA # (e)stimated co(r)relations  - variables_part_4e
  ,"computed_r"  = NA # (c)omputed  co(r)relations  - variables_part_5
  ,"compute_corr"= NA # components to compute corr  - variables_part_6
  ,"process_id"  = NA # domain structure and labels - variables_part_7
)

#### Model identifiers
variables_part_1 <- c(
  "model_number"       # u|b - uni|bi-variate; 0|1|2 - highest curve component
  ,"study_name"        # eas, elsa ...
  ,"subgroup"          # male & female
  ,"model_type"        # 0 , a, ae, aeh, aeh+, & full
  ,"process_a"         # fev, pef, grip
  ,"process_b"         # block, digits_f
)

#### Model information
variables_part_2 <- c(
  "subject_count"      # sample size
  ,"parameter_count"   # number of estimated parameters
  ,"wave_count"        # number of waves
  ,'ll'                # log-likelihood
  ,"aic"               # Akaike information criterion
  ,"bic"               # Bayesian information criterion
)

#######################################################################################
# Estimates of the covariates

#### Fixed effects (gamma)
# identify the possible numeric combinations (in this case linear model)
coefficients_possible <- c(
  "00", "10",
  "01", "11",
  "02", "12",
  "03", "13",
  "04", "14",
  "05", "15",
  "06", "16"
)
# the standard set of values describing estimated parameters
stats_possible        <- c("est", "se", "wald", "pval")#, "ci95_lower", "ci95_upper")
# combine the two into a data frame
ds_order_gamma <- tidyr::crossing(
  process       = c("a", "b"),
  coefficient   = factor(coefficients_possible, levels=coefficients_possible),
  stat          = factor(stats_possible       , levels=stats_possible)
)
# assembled the population of the fixed effects loadings
variables_part_3 <- sprintf(
  "%s_gamma_%s_%s",
  ds_order_gamma$process,
  ds_order_gamma$coefficient,
  ds_order_gamma$stat
)
##### The order in which predictors enter the equation
coefficient_key <- c(
  "0"  = "intercept",
  "1"  = "age",
  "2"  = "education",
  "3"  = "height",
  "4"  = "smoking",
  "5"  = "cardio",
  "6"  = "diabetes"
)

#######################################################################################
# Bivariate intercepts, slopes, and residuals (BISR)

##### Random effects (tau)
# on-diagonal elements in the matrix of random effects
variables_part_4a <- c(
   "aa_tau_00_est"
  ,"aa_tau_00_se"
  ,"aa_tau_00_wald"
  ,"aa_tau_00_pval"
  ,"aa_tau_11_est"
  ,"aa_tau_11_se"
  ,"aa_tau_11_wald"
  ,"aa_tau_11_pval"
  ,"bb_tau_00_est"
  ,"bb_tau_00_se"
  ,"bb_tau_00_wald"
  ,"bb_tau_00_pval"
  ,"bb_tau_11_est"
  ,"bb_tau_11_se"
  ,"bb_tau_11_wald"
  ,"bb_tau_11_pval"
)
# on-diagonal elements in the matrix of random effects
variables_part_4b <- c(
   "aa_tau_01_est"
  ,"aa_tau_01_se"
  ,"aa_tau_01_wald"
  ,"aa_tau_01_pval"
  ,"bb_tau_10_est"
  ,"bb_tau_10_se"
  ,"bb_tau_10_wald"
  ,"bb_tau_10_pval"
  ,"ab_tau_00_est"
  ,"ab_tau_00_se"
  ,"ab_tau_00_wald"
  ,"ab_tau_00_pval"
  ,"ab_tau_11_est"
  ,"ab_tau_11_se"
  ,"ab_tau_11_wald"
  ,"ab_tau_11_pval"
  ,"ab_tau_01_est"
  ,"ab_tau_01_se"
  ,"ab_tau_01_wald"
  ,"ab_tau_01_pval"
  ,"ab_tau_10_est"
  ,"ab_tau_10_se"
  ,"ab_tau_10_wald"
  ,"ab_tau_10_pval"
)
#### Residuals (sigma)
# on-diagonal elements in the matrix of residuals
variables_part_4c <- c(
   "a_sigma_00_est"
  ,"a_sigma_00_se"
  ,"a_sigma_00_wald"
  ,"a_sigma_00_pval"
  ,"b_sigma_00_est"
  ,"b_sigma_00_se"
  ,"b_sigma_00_wald"
  ,"b_sigma_00_pval"
)
# off-diagonal elements in the matrix of residuals
variables_part_4d <- c(
   "ab_sigma_00_est"
  ,"ab_sigma_00_se"
  ,"ab_sigma_00_wald"
  ,"ab_sigma_00_pval"
)
# additional sets of parameters
# (e)stimated co(r)relations of intercepts, slopes, and residuals
variables_part_4e <- c(
   "er_tau_00_est"
  ,"er_tau_00_se"
  ,"er_tau_00_wald"
  ,"er_tau_00_pval"
  ,"er_tau_11_est"
  ,"er_tau_11_se"
  ,"er_tau_11_wald"
  ,"er_tau_11_pval"
  ,"er_sigma_00_est"
  ,"er_sigma_00_se"
  ,"er_sigma_00_wald"
  ,"er_sigma_00_pval"
)
# (c)omputed  co(r)relations of intercepts, slopes, and residuals
variables_part_5 <- c(
   "cr_levels_est"
  ,"cr_tau_00_se"
  ,"cr_tau_00_wald"
  ,"cr_tau_00_pval"
  ,"cr_tau_11_est"
  ,"cr_tau_11_se"
  ,"cr_tau_11_wald"
  ,"cr_tau_11_pval"
  ,"cr_sigma_00_est"
  ,"cr_sigma_00_se"
  ,"cr_sigma_00_wald"
  ,"cr_sigma_00_pval"
)
#######################################################################################
# Other useful sets

# components in correlation computation
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
# domain structure and labels for process names
variables_part_7 <- c(
   "process_a"           # inique id of process A
  ,"process_b"           # unique id of process B
  ,"process_b_cell"      # custom label of process_b for the cell
  ,"process_b_row"       # A measure, that may have different specific tests
  ,"process_b_domain"    # Domain os the process B (the long process)
)
# Now putting it all together
#   component      list of column names                      label
model_components <- list(
  "id"           = variables_part_1                        # Model identifiers            
  ,"info"        = variables_part_2                        # Model information            
  ,"fixed"       = variables_part_3                        # Fixed effects (gamma)        
  ,"fixed_key"   = coefficient_key                         # Order of predictors          
  ,"random"      = c(variables_part_4a, variables_part_4b) # Random effects (tau)         
  ,"residual"    = c(variables_part_4c, variables_part_4d) # Residuals (sigma)            
  ,"estimated_r" = variables_part_4e                       # (e)stimated co(r)relations        
  ,"computed_r"  = variables_part_5                        # (c)omputed  co(r)relations       
  ,"compute_corr"= variables_part_6                        # components to compute corr       
  ,"process_id"  = variables_part_7                        # domain structure and labels      
)
