# this script contains the definitions of the functions that extract the results of model estimation
# folder = "./sandbox/syntax-creator/outputs/grip-numbercomp/"
collect_model_results <- function(folder){
  # collect a vector with .out file paths
  (out_list <- list.files(file.path(folder), full.names=T, recursive=F, pattern="out$"))
  # mplus_output <- scan(out_list[i], what='character', sep='\t') # each line of output as a char value
  mplus_output <- readr::read_file(out_list[1])
  # the script `group-variables.R` creates objects with names of standard variables for easier handling
  #e.g ab_TAU_00 <- c("ab_TAU_00_est", "ab_TAU_00_se", "ab_TAU_00_wald","ab_TAU_00_pval")
  # source("./scripts/mplus/group-variables.R")

  # I. EXTRACTION
  #
  # I.A. Extract Model Summaries
  # create a dataset with model summaries
  get_msum <- function(){
    ## Declare the model descriptors we wish to record:
    msum_names <- c("Mplus.version",
                    "Title",
                    "AnalysisType",
                    "Estimator",
                    "Observations",
                    "Parameters",
                    "LL","AIC","BIC","aBIC","AICC",
                    "Filename", "filePath")
    # Create data frame to populated from model output files
    msum <- data.frame(matrix(ncol=length(msum_names)))
    names(msum) <- msum_names # columns is what we want to extract from MplusAutomation::extractModelSummaries()
    msum
    # cycle through all model output files in this study
    for(i in seq_along(out_list)){
      # get a single model summary
      ith_msum <- MplusAutomation::extractModelSummaries(target=out_list[i], recursive=T)
      # LOGICAL: is this descriptor present in the current model?
      (descriptor_exists <- names(ith_msum) %in% msum_names)
      # names of descriptors that exist in ith model
      (existing_descriptors <- names(ith_msum)[descriptor_exists])
      # populate existing fields
      msum[i, existing_descriptors] <- ith_msum[names(ith_msum) %in% msum_names]
      msum$filePath[i] <- out_list[i] # attach the file path directly from the observed list

      #       # add the filename for identification
      #       (a <- strsplit(msum$filePath[i], split="/")) # each subfolder into a char value
      #       selector <- a[[1]] %in% c("studies") # find which one says "studies"
      #       element_number <- c(1:length(selector))[selector] # get its number
      #       msum$study_name[i] <- a[[1]][element_number+1] # move one step to the right

      # add the file path to locate for debugging
      # (a <- strsplit(msum$filePath[i], split="/studies/")) # common folder
      #       msum$file_path[i] <- a # move one step to the right


    } # close loop
    return(msum)
  } # close function
  msum <- get_msum()

  # I.B. Extract Model Parameters
  # create a list which elements are datasets containing estimated coefficients
  get_mpar <- function(){
    mpar <- list()  # Create list object to populated from model output files
    # cycle through all model output files
    for(i in seq_along(out_list)){
      out_file <-  tail(strsplit(out_list[i],"/")[[1]], n=1)  # grab an output file to work with
      message("Getting model ", i, ", ",out_file) # view the file name
      mplus_output <- scan(out_list[i], what='character', sep='\n') # each line of output as a char value
      # testing for specific errors
      no_observations <- length(grep("One or more variables in the data set have no non-missing values", mplus_output))
      variance_zero <- length(grep("One or more variables have a variance of zero", mplus_output))
      # If there are no specific error, then go get the parameter solution
      if(no_observations){
        mpar[i]  <- "No observations"
      }else{

        if(variance_zero){
          mpar[i]  <- "Zero variance"
        }else{
          mpar[i] <- MplusAutomation::extractModelParameters(target=out_list[i], recursive=T, dropDimensions=T)
        }
      }
    }
    return(mpar)
  }
  mpar <- get_mpar()

  # II.ASSEMBLY
  # using (msum, mpar)  and custom parsing, populate the result mold (catalog-4)
  #
  # create empty dataset "results" that will be later populated with extracted values
  # selected_results is declared in group-variables.R script
  results <- data.frame(matrix(NA, ncol=length(selected_results), nrow=length(mpar))) # length(mpar) = number of output files
  names(results) <-  selected_results

  # II.A. Basic Results
  # extract the basic indicators about the model
  get_results_basic <- function(){
    selected_models <- seq_along(mpar)
    for(i in selected_models){
      (out_file <-  tail(strsplit(out_list[i],"/")[[1]], n=1)) # grab an output file to work with
      message("Getting model ", i, ", ",out_file)# view the file name
      mplus_output <- scan(out_list[i], what='character', sep='\n') # each line of output as a char value
      (model <- mpar[[i]])# load the extract of this model's estimates
      # testing for specific errors
      (no_observations <- length(grep("One or more variables in the data set have no non-missing values", mplus_output)))
      (variance_zero <- length(grep("One or more variables have a variance of zero", mplus_output)))
      # If there are no specific error, then go get the parameter solution
      if(no_observations){
        results[i, "Error"]  <- "No observations"
      }else{
        if(variance_zero){
          results[i,"Error"]  <- "Zero variance"
        } else {

          ## Populate admin variables
          results[i, 'software'] <- mplus_output[1]
          results[i,"version"] <- "0.1"
          results[i, c('date', 'time')] <- strsplit(mplus_output[3], '  ')[[1]]
          results[i,"data_file"] <- strsplit(mplus_output[grep("File", mplus_output, ignore.case=TRUE)], 'IS| is |=|;')[[1]][2]
          results[i, 'output_file'] <- msum[i, 'Filename']
          results[i, "file_path"] <- msum[i,"filePath"]

          ## Populate model_id variables
          # results[i,"study_name"] <- msum$study_name[i]
          # results[i,c("model_number", 'subgroup',  'model_type')] <- strsplit(msum$Filename[i], '_')[[1]][1:3]

          process_a_name = 'grip'
          process_b_name = 'numbercomp'
          subgroup_sex = "male"
          model_type = "aeh" # = must be defined programmatically, by the scan of covariates

          results[i, "process_a"] <- process_a_name # ARGUMENTS DEFINED IN mplus_generator_bivariate()
          results[i, "process_b"] <- process_b_name # ARGUMENTS DEFINED IN mplus_generator_bivariate()
          results[i, "subgroup_sex"] <- subgroup_sex # ARGUMENTS DEFINED IN mplus_generator_bivariate()
          results[i, "model_type"] <- model_type

          ## Populate model_info variables
          results[i, 'subject_count'] <- msum[i, 'Observations'] # verify this, maybe datapoints, not subjects
          results[i, 'parameter_count'] <- msum[i, 'Parameters']

          (subject <- model[model$paramHeader=='Intercepts', 'param'])
          (results[i, 'wave_count'] <- max(as.numeric(gsub("[^0-9]", '', subject)), na.rm=T)) # MUST CHANGE. COUNTS THE HIGHEST NUMBER, BUT RATHER MUST COUNT THE COUNT OF WAVES
          (results[i, c('LL')] <-  msum[i,c('LL')])
          (results[i, c('aic')] <-  msum[i,c('AIC')])
          (results[i, c('bic')] <-  msum[i,c('BIC')])
          (results[i, c('adj_bic')] <-  msum[i,c('aBIC')])
          (results[i, c('aaic')] <-  msum[i,c('AICC')])
          ## Computed values

        } # close else
      } # close else
    } # close loop for selected models
    return(results)
  } # close get_results_basic
  results <- get_results_basic()
  # results[i, a_GAMMA_00]

  # II.B. Catching Errors
  # records all relevant errors and warnings about model estimation produced by Mplus
  get_results_errors <- function(){
    selected_models <- seq_along(mpar)
    for(i in selected_models){
      (out_file <-  tail(strsplit(out_list[i],"/")[[1]], n=1)) # grab an output file to work with
      message("Getting model ", i, ", ",out_file)# view the file name
      mplus_output <- scan(out_list[i], what='character', sep='\n') # each line of output as a char value
      # testing for specific errors
      (no_observations <- length(grep("One or more variables in the data set have no non-missing values", mplus_output)))
      (variance_zero <- length(grep("One or more variables have a variance of zero", mplus_output)))
      # If there are no specific error, then go get the parameter solution
      if(no_observations){
        results[i, "Error"]  <- "No observations"
      }else{
        if(variance_zero){
          results[i,"Error"]  <- "Zero variance"
        }
        else{

          ## Check for model convergence
          line_found <-  length(grep("THE MODEL ESTIMATION TERMINATED NORMALLY", mplus_output))
          results[i, 'has_converged'] <- line_found

          line_found <- length(grep("THE COVARIANCE COVERAGE FALLS BELOW THE SPECIFIED LIMIT", mplus_output))
          results[i,"covar_covered"] <- line_found

          line_found <- grep("TRUSTWORTHY FOR SOME PARAMETERS DUE TO A NON-POSITIVE DEFINITE", mplus_output)
          results[i,"trust_all"] <- !length(line_found)==1L

          line_found <- grep("PROBLEM INVOLVING THE FOLLOWING PARAMETER:", mplus_output)
          snippet <- mplus_output[line_found+1]
          if(length(snippet)>0){
            results[i,"mistrust"] <- snippet
          }


        } # close else
      } # close else
    } # close loop for selected_models
    return(results)
  } # close get_results_errors
  results <- get_results_errors()

  # III.A. Random Effects
  # record the extracted values of the estimated random effects
  get_results_random <- function(){
    selected_models <- seq_along(mpar)
    for(i in selected_models){
      model <- mpar[[i]] # load the extract of this model's estimates

      ## covariante btw intercept of process (A) and intercept of process (B) - ab_TAU_00
      (test <- model[grep(".WITH", model$paramHeader),]) # paramHeader containing .WITH
      (test <- test[grep("^I|S", test$param),]) # param starting with I or S
      (test <- test[grep("^I", test$paramHeader),]) # paramHeader starting with I
      (test <- test[grep("^I", test$param),]) # pram starting with I
      (test <- test[ ,c("est", "se","est_se", "pval")])
      if(dim(test)[1]!=0){results[i, ab_TAU_00] <- test}

      ## covariance btw slope of process (A) and slope of process (B) - ab_TAU_11
      (test <- model[grep(".WITH", model$paramHeader),]) # paramHeader containing .WITH
      (test <- test[grep("^I|S", test$param),]) # param starting with I or S
      (test <- test[grep("^S", test$paramHeader),]) # paramHeader starting with S
      (test <- test[grep("^S", test$param),]) # pram starting with S
      (test <- test[ ,c("est", "se","est_se", "pval")])
      if(dim(test)[1]!=0) {results[i, ab_TAU_11] <- test}

      ## covariance btw intercept of process (A) and slope of process (A) - aa_TAU_01
      (test <- model[grep(".WITH", model$paramHeader),]) # paramHeader containing .WITH
      (test <- test[grep("^IA|^SA", test$param),]) # param starting NOT with I or S
      (test <- test[grep("^IA|^SA", test$paramHeader),])
      (test <- test[ ,c("est", "se","est_se", "pval")])
      if(dim(test)[1]!=0){results[i, aa_TAU_01] <- test}

      ## covariance btw intercept of process (A) and slope of process (B) - ab_TAU_01
      (test <- model[grep(".WITH", model$paramHeader),]) # paramHeader containing .WITH
      (test <- test[grep("^IA|^SB", test$param),]) # param starting NOT with I or S
      (test <- test[grep("^IA|^SB", test$paramHeader),])
      (test <- test[ ,c("est", "se","est_se", "pval")])
       if(dim(test)[1]!=0){results[i, ab_TAU_01] <- test}

      ## covariance btw intercept of process (A) and slope of process (B) - ab_TAU_10
      (test <- model[grep(".WITH", model$paramHeader),]) # paramHeader containing .WITH
      (test <- test[grep("^IB|^SA", test$param),]) # param starting NOT with I or S
      (test <- test[grep("^IB|^SA", test$paramHeader),])
      (test <- test[ ,c("est", "se","est_se", "pval")])
       if(dim(test)[1]!=0){results[i, ab_TAU_10] <- test}

      ## covariance btw slope of process (B) and intercept of process (B) - bb_TAU_10
      (test <- model[grep(".WITH", model$paramHeader),]) # paramHeader containing .WITH
      (test <- test[grep("^IB|^SB", test$param),]) # param starting NOT with I or S
      (test <- test[grep("^IB|^SB", test$paramHeader),])
      (test <- test[ ,c("est", "se","est_se", "pval")])
       if(dim(test)[1]!=0){results[i, bb_TAU_10] <- test}

      ## Variance of random intercept of process (A) - aa_TAU_00
      (test <- model[grep("Residual.Variances", model$paramHeader),])
      (test <- test[test$param=='IA', ])
      (test <- test[ ,c("est", "se","est_se", "pval")])
      if(dim(test)[1]!=0) {results[i, aa_TAU_00] <- test}

      ## Variance of random slope of process (A) - aa_TAU_11
      (test <- model[grep("Residual.Variances", model$paramHeader),])
      (test <- test[test$param=='SA', ])
      (test <- test[ ,c("est", "se","est_se", "pval")])
      if(dim(test)[1]!=0) {results[i, aa_TAU_11] <- test}

      ## Variance of random intercept of process (B) - bb_TAU_00
      (test <- model[grep("Residual.Variances", model$paramHeader),])
      (test <- test[test$param=='IB', ])
      (test <- test[ ,c("est", "se","est_se", "pval")])
      if(dim(test)[1]!=0) {results[i,bb_TAU_00] <- test}

      ## Variance of random slope of process (B) - bb_TAU_11
      (test <- model[grep("Residual.Variances", model$paramHeader),])
      (test <- test[test$param=='SB', ])
      (test <- test[ ,c("est", "se","est_se", "pval")])
      if(dim(test)[1]!=0) {results[i, bb_TAU_11] <- test}

    } # close for loop
    return(results)
  }# close get_results_random
  results <- get_results_random()

  # III.B. Residuals
  # record the extracted values of the estimated random effects
  get_results_residual <- function(){
    selected_models <- seq_along(mpar)
    for(i in selected_models){
      i <- 1
      model <- mpar[[i]] # load the extract of this model's estimates

      ## variance of residual of process (A)- a_SIGMA
      (test <- model[grep("^A", model$param), ])
      (test <- test[grep("^Residual.Variances", test$paramHeader), ])
      (test <- test[ ,c("est", "se","est_se", "pval")][1,]) # only the first line, they should be same
      if(dim(test)[1]!=0) {results[i, a_SIGMA] <- test}

      ## variance of residual of process (B) - b_SIGMA
      (test <- model[grep("^B", model$param), ])
      (test <- test[grep("^Residual.Variances", test$paramHeader), ])
      (test <- test[ ,c("est", "se","est_se", "pval")][1,]) # only the first line, they should be same
      if(dim(test)[1]!=0) {results[i, b_SIGMA] <- test}

      ## covariance btw residuals of process (A) and process (B) - ab_SIGMA
      (test <- model[grep(".WITH", model$paramHeader),]) # paramHeader containing .WITH
      (test <- test[-grep("^I|S", test$param),]) # param starting NOT with I or S
      (test <- test[ ,c("est", "se","est_se", "pval")][1,]) # only the first line, they should be same
      if(dim(test)[1]!=0){results[i, ab_SIGMA] <- test}

      ## Correlations b/w INTERCEPT of process (A)  and INTERCEPT of process (B)
      results[i,R_IAIB] <- IalsaSynthesis::extract_named_wald("R_IAIB", mplus_output)
      ## Correlations b/w SLOPE of process (A)  and SLOPE of process (B)
      results[i, R_SASB] <- IalsaSynthesis::extract_named_wald("R_SASB", mplus_output)

      ## Correlations b/w RESIDUAL of process (A)  and RESIDUAL of process (B)
      results[i,R_RES_AB] <- IalsaSynthesis::extract_named_wald("R_RES_AB",mplus_output)

    } # close for loop
    return(results)
  }# close get_results_residual
  results <- get_results_residual()


  # III.C. Fixed Effects
  # record the extracted values of the estimated random effects
  get_results_fixed <- function(){
    selected_models <- seq_along(mpar)
    for(i in selected_models){
      model <- mpar[[i]] # load the extract of this model's estimates
      ## intercept
      (int <- model[grep("Intercepts", model$paramHeader),])

      ## average initial status of process (A) - a_GAMMA_00
      (test <- int[int$param=='IA',c('est', 'se', "est_se", 'pval')])
      if(dim(test)[1]!=0) {results[i, a_GAMMA_00] <- test}

      ## average rate of change of process (A) - a_GAMMA_10
      (test <- int[int$param=='SA',c('est', 'se', "est_se", 'pval')])
      if(dim(test)[1]!=0) {results[i, a_GAMMA_10] <- test}

      ## average initial status of process (B) - b_GAMMA_00
      (test <- int[int$param=='IB',c('est', 'se', "est_se", 'pval')])
      if(dim(test)[1]!=0) {results[i, b_GAMMA_00] <- test}

      ## average rate of change of process (B) - b_GAMMA_10
      (test <- int[int$param=='SB',c('est', 'se', "est_se", 'pval')])
      if(dim(test)[1]!=0) {results[i, b_GAMMA_10] <- test}

      ## intercept of process (A) regressed on (age at baseline) centered at 70 years - a_GAMMA_01
      (test <- model[grep("IA.ON", model$paramHeader),])
      (test <- test[test$param=="AGE_C70",])
      (test <- test[c('est', 'se', "est_se", 'pval')])
      if(dim(test)[1]!=0) {results[i, a_GAMMA_01] <- test}

      ## slope of process (A) regressed on (age at baseline) centered at 70 years - a_GAMMA_11
      (test <- model[grep("SA.ON", model$paramHeader),])
      (test <- test[test$param=="AGE_C70",])
      (test <- test[c('est', 'se', "est_se", 'pval')])
      if(dim(test)[1]!=0) {results[i, a_GAMMA_11] <- test}

      ## intercept of process (A) regressed on (education) centered at 7 grades - a_GAMMA_01
      (test <- model[grep("IA.ON", model$paramHeader),])
      (test <- test[test$param=="EDU_C7",])
      (test <- test[c('est', 'se', "est_se", 'pval')])
      if(dim(test)[1]!=0) {results[i, a_GAMMA_01] <- test}

      ## slope of process (A) regressed on (education) centered at 7 graded
      (test <- model[grep("SA.ON", model$paramHeader),])
      (test <- test[test$param=="EDU_C7",])
      (test <- test[c('est', 'se', "est_se", 'pval')])
      if(dim(test)[1]!=0) {results[i, a_GAMMA_11] <- test}

      ## intercept of process (A) regressed on (height)  centered at 160 cm - a_GAMMA_01
      (test <- model[grep("IA.ON", model$paramHeader),])
      (test <- test[test$param=="HTM_C160",])
      (test <- test[c('est', 'se', "est_se", 'pval')])
      if(dim(test)[1]!=0) {results[i, a_GAMMA_01] <- test}

      ## slope of process (A) regressed on (height) centered at 160 cm  - a_GAMMA_11
      (test <- model[grep("SA.ON", model$paramHeader),])
      (test <- test[test$param=="HTM_C160",])
      (test <- test[c('est', 'se', "est_se", 'pval')])
      if(dim(test)[1]!=0) {results[i, a_GAMMA_11] <- test}



      #       ## intercept of process (B)  regressed on Age at baseline
      #       (test <- model[grep("IB.ON", model$paramHeader),])
      #       (test <- test[test$param=="AGE_C70",])
      #       (test <- test[c('est', 'se', "est_se", 'pval')])
      #       if(dim(test)[1]!=0) {results[i, b_GAMMA_01] <- test}
      #
      #       ## slope of process (B) regressed on Age at baseline
      #       (test <- model[grep("SB.ON", model$paramHeader),])
      #       (test <- test[test$param=="BAGE",])
      #       (test <- test[c('est', 'se', "est_se", 'pval')])
      #       if(dim(test)[1]!=0) {results[i, b_GAMMA_11] <- test}


    } # close for loop
    return(results)
  }# close get_results_fixed
  results <- get_results_fixed()

  # IV.A. Export results
  (a <- (strsplit(folder, "/")[[1]]))
  (a_max <- max(length(a)))
  (b <- a[a_max])
  write.csv(results, paste0(folder,"/",b,".csv") , row.names=F)
  # saveRDS(results, paste0(destination,".rds") )
}



# collect_all_results <- function(allFolder){
#   ## make scripts from the prototype and run it (run_models=TRUE)
#   # pathFolder <- allFolder # where outputs are
#   out_list_all <- list.files(pathFolder, full.names=T, recursive=T, pattern="out$")
#
#   directories <- gsub(pattern, "\\1", dto_paths, perl=T)
#
#   pair_names <- basename(directories)
#
#
#
#   for(i in allFolder){
#     collect_model_results(folder=allFolder)
#   }
#
# }
#


# collect model results into a dataset stored in "./outputs/pairs/"
# the following function is defined in extraction_functions.R  script
# collect_model_results(folder = "outputs/pairs/grip_categories")
# ds <- readRDS(file.path(pathFolder,"grip_categories.rds")) # load the data for outcome pare
# kb_pr
