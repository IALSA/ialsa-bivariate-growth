requireNamespace("ggplot2")
requireNamespace("tidyr")
requireNamespace("dplyr") #Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit")
requireNamespace("reshape2")
data <- ds_wide
class(data$male)
dslong <- readRDS("/Users/cassandrabrown/Github/ialsa-bivariate-growth/data/unshared/derived/map-1/data-long.rds")

dswide <- readRDS("/Users/cassandrabrown/Github/ialsa-bivariate-growth/data/unshared/derived/map-1/data-wide.rds")

dswide$sex <- factor(dswide$male, levels=c(TRUE, FALSE), labels=c("male","female"))
dslong$sex <- factor(dslong$male, levels=c(TRUE, FALSE), labels=c("male","female"))
dswidemen <- dswide[dswide$male==TRUE,]
dswidemen <- dswidemen[dswidemen$dementia_ever!= TRUE,]
dslongmen <- dslong[dslong$sex=="male",]

dswidewomen <- dswide[dswide$sex=="female",]

dslongwomen <- dslong[dslong$sex=="female",]


table(men$htm_c)
sum(!is.na(dswidemen$htm_c))
sum(!is.na(dswidemen$age_c70))
sum(!is.na(dswidemen$edu_c7))
sum(!is.na(dswidemen$smoke))
sum(!is.na(dswidemen$heart))
sum(!is.na(dswidemen$diabetes))
sum(is.na(dswidemen$dementia_ever))

table(dswidemen$dementia_ever)


temporal_pattern <- function(ds, measure){
  # set.seed(seed_value)
  ds_long <- ds
  (ids <- sample(unique(ds_long$id),1))
  d <-ds_long %>%
    dplyr::filter(id %in% ids ) %>%
    dplyr::select_("id","wave", measure)
  print(d)
}

temporal_pattern(dslongmen,dslongmen$fev)

# examine the descriptives over waves
over_waves <- function(ds, measure_name, exclude_values="") {
  ds <- as.data.frame(ds)
  testit::assert("No such measure in the dataset", measure_name %in% unique(names(ds)))
  # measure_name = "htval"; wave_name = "wave"; exclude_values = c(-99999, -1)
  cat("Measure : ", measure_name,"\n", sep="")
  t <- table( ds[,measure_name], ds[,"wave"], useNA = "always"); t[t==0] <- ".";t
  print(t)
  cat("\n")
  ds[,measure_name] <- as.numeric(ds[,measure_name])
  
  d <- ds[!(ds[,measure_name] %in% exclude_values), ]
  a <- lazyeval::interp(~ round(mean(var),2) , var = as.name(measure_name))
  b <- lazyeval::interp(~ round(sd(var),3),   var = as.name(measure_name))
  c <- lazyeval::interp(~ n())
  dots <- list(a,b,c)
  t <- d %>%
    dplyr::select_("id","wave", measure_name) %>%
    na.omit() %>%
    # dplyr::mutate_(measure_name = as.numeric(measure_name)) %>%
    dplyr::group_by_("wave") %>%
    dplyr::summarize_(.dots = setNames(dots, c("mean","sd","count")))
  return(as.data.frame(t))
  
}
dswidemen$f
over_waves(dslongmen,"fev")
over_waves(dslongmen, "bnt", exclude_values = (is.na("bnt")==TRUE))
over_waves(dslongmen, "bostordel", exclude_values = (is.na("bostordel")==TRUE))
over_waves(dslong)
which
dswidemen$nofev<-ifelse(is.na(dswidemen$fev_00)==TRUE & is.na(dswidemen$fev_01)==TRUE  & is.na(dswidemen$fev_02)==TRUE  & is.na(dswidemen$fev_03)==TRUE  & is.na(dswidemen$fev_04)==TRUE, c("missing"), c("somedata"))
table(dswidemen$nofev)

dswidemen$nobnt<-ifelse(is.na(dswidemen$bnt_00)==TRUE & is.na(dswidemen$bnt_01)==TRUE  & is.na(dswidemen$bnt_02)==TRUE  & is.na(dswidemen$bnt_03)==TRUE  & is.na(dswidemen$bnt_04)==TRUE, c("missing"), c("somedata"))
table(dswidemen$nobnt)
#no covariates
dswidemen$nocov<-ifelse(#is.na(dswidemen$edu_c7)==TRUE & is.na(dswidemen$age_c70)==TRUE  & 
  is.na(dswidemen$htm_c)==TRUE & is.na(dswidemen$heart)==TRUE  & is.na(dswidemen$smoke)==TRUE & is.na(dswidemen$diabetes)==TRUE, c("missing"), c("somedata"))
table(dswidemen$nocov)

dswidemen$nohtmheart <- ifelse(is.na(dswidemen$htm_c)==TRUE & is.na(dswidemen$heart)==TRUE, c("missing"), c("somedata"))

table(dswidemen$nohtmheart)

dswidemen$nohtmsmoke <- ifelse(is.na(dswidemen$htm_c)==TRUE & is.na(dswidemen$smoke)==TRUE, c("missing"), c("somedata"))
table(dswidemen$nohtmsmoke)

dswidemen$nohtmdiabetes <- ifelse(is.na(dswidemen$htm_c)==TRUE & is.na(dswidemen$diabetes)==TRUE, c("missing"), c("somedata"))
table(dswidemen$nohtmdiabetes)

dswidemen$noheartsmoke <- ifelse(is.na(dswidemen$heart)==TRUE  & is.na(dswidemen$smoke)==TRUE, c("missing"), c("somedata"))
table(dswidemen$noheartsmoke)

dswidemen$noheartdiabetes <- ifelse(is.na(dswidemen$heart)==TRUE  & is.na(dswidemen$diabetes)==TRUE, c("missing"), c("somedata"))
table(dswidemen$noheartdiabetes)

#identify rows with missing covariates
which(is.na(dswidemen$diabetes))
which(is.na(radcMAPc$heart_cum.00))
which(is.na(radcMAPc$educ))
which(is.na(radcMAPc$age_bl))
which(is.na(radcMAPc$htm.00))
which(is.na(radcMAPc$smoking))
