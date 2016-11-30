requireNamespace("ggplot2")
requireNamespace("tidyr")
requireNamespace("dplyr") #Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit")
requireNamespace("reshape2")
data <- ds_wide
class(data$male)
dslong <- readRDS("/Users/cassandrabrown/Github/ialsa-bivariate-growth/data/unshared/derived/map-1/data-long.rds")

dswide <- readRDS("/Users/cassandrabrown/Github/ialsa-bivariate-growth/data/unshared/derived/map-1/data-wide.rds")

dswide$sex <- factor(dswide$male, levels=c(TRUE, FALSE), labels=c("male","Female"))
dslong$sex <- factor(dslong$male, levels=c(TRUE, FALSE), labels=c("male","Female"))
dswidemen <- dswide[dswide$sex=="male",]
dslongmen <- dslong[dslong$sex=="male",]
dswidewomen <- data[data$sex=="Female",]

summarize(men)
table(men$htm_c)
sum(!is.na(men$htm_c))
sum(!is.na(dswidemen$age_c70))
sum(!is.na(men$edu_c7))
sum(!is.na(men$smoke))
sum(!is.na(men$stroke))
sum(!is.na(men$diabetes))
sum(!is.na(dswidemen$dementia_ever))
temporal_pattern <- function(ds, measure){
  # set.seed(seed_value)
  ds_long <- ds
  (ids <- sample(unique(ds_long$id),1))
  d <-ds_long %>%
    dplyr::filter(id %in% ids ) %>%
    dplyr::select_("id","wave", measure)
  print(d)
}

temporal_pattern(dslong,dslong$fev)

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

over_waves(dslongmen,"fev")
over_waves(dslongmen, "bnt", exclude_values = (is.na("bnt")==TRUE))
over_waves(dslongmen, "bostordel", exclude_values = (is.na("bostordel")==TRUE))
over_waves(dslong)
which
men$nofev<-ifelse(fev.00==-9999 & fev.01==-9999 & fev.02==-9999 & fev.03==-9999 & fev.04==-9999 &
                        fev.05==-9999 & fev.06==-9999 & fev.07==-9999 & fev.08==-9999 & fev.09==-9999 &
                        fev.10==-9999 & fev.11==-9999 & fev.12==-9999 & fev.13==-9999 & fev.14==-9999 &
                        fev.15==-9999 & fev.16==-9999 & fev.17==-9999, c("missing"), c("somedata"))
table(radcMAP$nofev)