# ## This script declares the functions that generate Mplus .inp file used in model fitting.

# prototype = "sandbox/01-univariate-linear/prototype-map-wide.inp"
# saved_location = "sandbox/01-univariate-linear/example"
# process_a_name = 'numbercomp'# measure name
# process_a_mplus = 'cts_nccrtd'# Mplus variable
# subgroup_sex = "male"
# subset_condition_2 = "dementia_ever NE 1"
# wave_set_modeled =  c(1,2,3,4,5)
# run_models = FALSE


mplus_generator_bivariate <- function(
    prototype #= "./sandbox/syntax-creator/prototype-map-wide.inp" # point to the template
  , saved_location #= "sandbox/syntax-creator/example" # where to store all the .inp/.out scripts
  , process_a_name #= 'grip'# item name of process (A)
  , process_a_mplus #= 'gripavg'# Mplus variable of process (A)
  , process_b_name #= 'numbercomp'# item name of process (B)
  , process_b_mplus# = 'cts_nccrtd'# Mplus variable of process (B)
  , subgroup_sex#= "male" # subset data to members of this group
  , subset_condition_1 #= "dementia_ever NE 1" # subset data to member of this group
  , covariate_set #= c("age_c70","htm_c160", "edu_c7")  # list of covariates ("_c" stands for "centercd)
  , wave_set_modeled #=  c(1,2,3,4,5) # Integer vector of waves considered by the model, ie c(1,2,3,5,8).
  , run_models = FALSE # If TRUE then Mplus runs estimation to produce .out, .gh5, and/or, other files
){
  # input the template to work with
  proto_input <- scan(prototype, what='character', sep='\n')
    #This makes it all one (big) element, if you need it in the future.
  # proto_input <- paste(proto_input, collapse="\n")

    # declare global values
  pathVarnames <- paste0(saved_location,"/wide-variable-names.txt")
  names_are <- read.csv(pathVarnames, header = F, stringsAsFactors = F)[ ,1]
  (a <- grepl("age_at_visit_", names_are))
  (b <- names_are[a])
  (c <- gsub("age_at_visit_","",b))
  (d <- as.numeric(c))
  wave_set_possible <- d
  (wave_modeled_max <- max(wave_set_modeled))


  # after modification .inp files will be saved as:
  newFile <- paste0(saved_location,"/", subgroup_sex ,"_", wave_modeled_max,".inp")
  # newFile <- paste0(saved_location,"/","b1_",subgroup_sex ,"_", wave_modeled_max,".inp")


  # TITLE:
  # DATA:
  # File = wide_dataset.dat; # automatic object, created by `look-at-data.R`
  # VARIABLE:
  # Names are # define the variabels used in the analysis

  names_are <- paste(names_are, collapse="\n") #Collapse all the variable names to one element (seperated by line breaks).
  names_are <- stringr::str_wrap(str = names_are, width  = 80, exdent = 4)
  proto_input <- gsub(pattern = "%names_are%", replacement = names_are, x = proto_input)

  # USEVAR are # what variables are used in estimation
  (estimated_timepoints <- paste0("time",wave_set_modeled))
  (estimated_timepoints <- paste(estimated_timepoints, collapse="\n"))
  proto_input <- gsub(pattern ="%estimated_timepoints%", replacement = estimated_timepoints, x = proto_input)

  (process_a_timepoints <- paste0("a",wave_set_modeled))
  (process_a_timepoints <- paste(process_a_timepoints, collapse="\n"))
  proto_input <- gsub(pattern ="%process_a_timepoints%", replacement = process_a_timepoints, x = proto_input)

  (process_b_timepoints <- paste0("b",wave_set_modeled))
  (process_b_timepoints <- paste(process_b_timepoints, collapse="\n"))
  proto_input <- gsub(pattern ="%process_b_timepoints%", replacement = process_b_timepoints, x = proto_input)


  (covariate_set <- paste(covariate_set, collapse="\n"))
  proto_input <- gsub(pattern = "%covariate_set%", replacement = covariate_set, x = proto_input)


  # Useobservations are # select a subset of observation
  if(subgroup_sex=="male"){
    print_sex_value <- paste0("msex EQ 1")
  } else {
    print_sex_value <- paste0("msex EQ 0")
  }
  # subset
  proto_input <- gsub("msex EQ %subgroup_sex%", paste0("msex EQ ",print_sex_value), proto_input)
  proto_input <- gsub("%subset_condition_1%", subset_condition_1, proto_input)

  # DEFINE:

  (match_timepoints_process_a <- paste0("a",wave_set_modeled,"=",process_a_mplus,"_",wave_set_modeled,";"))
  match_timepoints_process_a <- paste(match_timepoints_process_a, collapse="\n")
  proto_input <- gsub(pattern ="%match_timepoints_process_a%", replacement = match_timepoints_process_a, x = proto_input)

  (match_timepoints_process_b <- paste0("b",wave_set_modeled,"=",process_b_mplus,"_",wave_set_modeled,";"))
  match_timepoints_process_b <- paste(match_timepoints_process_b, collapse="\n")
  proto_input <- gsub(pattern ="%match_timepoints_process_b%", replacement = match_timepoints_process_b, x = proto_input)

  (match_time_since_bl <- paste0("time",wave_set_modeled,"=", "time_since_bl","_",wave_set_modeled,";"))
  match_time_since_bl <- paste(match_time_since_bl, collapse="\n")
  proto_input <- gsub(pattern ="%match_timepoints%", replacement = match_time_since_bl, x = proto_input)

  # ANALYSIS:
  # MODEL:

  # define process (A) in time points
  (assing_a_to_timepoints <- paste0("ia sa | a",wave_set_modeled," AT ","time",wave_set_modeled," ;"))
  (assing_a_to_timepoints <- paste(assing_a_to_timepoints, collapse="\n"))
  proto_input <- gsub(pattern ="%assing_a_to_timepoints%", replacement = assing_a_to_timepoints, x = proto_input)

  # define process (B) in time points
  (assing_b_to_timepoints <- paste0("ib sb | b",wave_set_modeled," AT ","time",wave_set_modeled," ;"))
  (assing_b_to_timepoints <- paste(assing_b_to_timepoints, collapse="\n"))
  proto_input <- gsub(pattern ="%assing_b_to_timepoints%", replacement = assing_b_to_timepoints, x = proto_input)

  # residual covariance of process (A)
  (resid_covariance_a <- paste0("a",wave_set_modeled," (res_a);"))
  (resid_covariance_a <- paste(resid_covariance_a, collapse="\n"))
  proto_input <- gsub(pattern ="%resid_covariance_a%", replacement = resid_covariance_a, x = proto_input)

  # residual covariance of process (B)
  (resid_covariance_b <- paste0("b",wave_set_modeled," (res_b);"))
  (resid_covariance_b <- paste(resid_covariance_b, collapse="\n"))
  proto_input <- gsub(pattern ="%resid_covariance_b%", replacement = resid_covariance_b, x = proto_input)

  # residual covariances of processes
  (resid_covariances <- paste0("a",wave_set_modeled," pwith ", "b", wave_set_modeled," (res_cov);"))
  (resid_covariances <- paste(resid_covariances, collapse="\n"))
  proto_input <- gsub(pattern ="%resid_covariances%", replacement = resid_covariances, x = proto_input)





  browser()


  proto_input <- gsub(pattern ="%waves_max%", replacement = wave_modeled_max, x = proto_input)
  # MODEL CONSTRAINT:
  # OUTPUT:
  # PLOT:

  writeLines(proto_input,newFile)

  if(run_models){
    # run all models in the folder
    pathRoot <- getwd()
    saved_laction_mplus <- paste0(pathRoot,"/",saved_location)
    MplusAutomation::runModels(directory=saved_laction_mplus )#, Mplus_command = Mplus_install_path)
  }
} # close function
