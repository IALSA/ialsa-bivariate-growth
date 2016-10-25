# remove all elements for a clean start
rm(list=ls(all=TRUE))



# # Import text file with the description of the raw files
# wave5Desc <- read.fwf("./Data/Raw/wave5_and_medrec_CONTENTS.txt", 
#               widths = c(6, 13, 10, 5, 13, 100), 
#               col.names=c("Number","Variable","Type", "Length", "Format", "Label"),
#               skip=37, n = 95)
# 
# medrecDesc <- read.fwf("./Data/Raw/wave5_and_medrec_CONTENTS.txt", 
#                       widths = c(6, 13, 10, 5, 10, 100), 
#                       col.names=c("Number","Variable","Type", "Length", "Format", "Label"),
#                       skip=177, n = 15)


# Import RAW files
wave1 <- Hmisc::spss.get("./Data/Raw/wave1.sav", use.value.labels = TRUE)
wave2 <- Hmisc::spss.get("./Data/Raw/wave2.sav", use.value.labels = TRUE)
wave3 <- Hmisc::spss.get("./Data/Raw/wave3.sav", use.value.labels = TRUE)
wave4 <- Hmisc::spss.get("./Data/Raw/wave4.sav", use.value.labels = TRUE)
wave5 <- Hmisc::spss.get("./Data/Raw/wave5.sav", use.value.labels = TRUE)


# Understand the structure of the data object
class(wave1)
dim(wave1)
nrow(wave1)
ncol(wave1)

# str() produces cumbersome output
str(wave1)
# but shows the properties of each column/variable
attr(wave1, "names")
# alternatively
# names(wave1)

#  exploring the names and labels
cat("\014") # clears console
attr(wave1$v116, "label")
colname <- "v1"
varlabel <- paste(attr(wave1[,colname], "label"))
cbind(colname,varlabel)



### Optimization assignment - turn this into two-level loop
### Optimization assignment - turn this into a function

# wave 1
# Descriptors in wave 1
curWave <- paste0('wave',1)  
dsNames1 <- data.frame(row.names=c("colname","varlabel"))
colsOFwave <- names(eval(as.name(curWave)))
for (i in colsOFwave){
  colname <- i
  varlabel <- paste(attr(wave1[,colname], "label"))
  rowName <- cbind(colname,varlabel)
  dsNames1 <- data.frame(rbind(dsNames1,rowName))
}
print(dsNames1)


# wave 2
# Descriptors in wave 2
curWave <- paste0('wave',2)  
dsNames2 <- data.frame(row.names=c("colname","varlabel"))
colsOFwave <- names(eval(as.name(curWave)))
for (i in colsOFwave){
  colname <- i
  varlabel <- paste(attr(wave2[,colname], "label"))
  rowName <- cbind(colname,varlabel)
  dsNames2 <- data.frame(rbind(dsNames2,rowName))
}
print(dsNames2)


# wave 3
# Descriptors in wave 3
curWave <- paste0('wave',3)  
dsNames3 <- data.frame(row.names=c("colname","varlabel"))
colsOFwave <- names(eval(as.name(curWave)))
for (i in colsOFwave){
  colname <- i
  varlabel <- paste(attr(wave3[,colname], "label"))
  rowName <- cbind(colname,varlabel)
  dsNames3 <- data.frame(rbind(dsNames3,rowName))
}
print(dsNames1)


# wave 4
# Descriptors in wave 4
curWave <- paste0('wave',4)  
dsNames4 <- data.frame(row.names=c("colname","varlabel"))
colsOFwave <- names(eval(as.name(curWave)))
for (i in colsOFwave){
  colname <- i
  varlabel <- paste(attr(wave4[,colname], "label"))
  rowName <- cbind(colname,varlabel)
  dsNames4 <- data.frame(rbind(dsNames4,rowName))
}
print(dsNames4)


# wave 5
# Descriptors in wave 5
curWave <- paste0('wave',5)  
dsNames5 <- data.frame(row.names=c("colname","varlabel"))
colsOFwave <- names(eval(as.name(curWave)))
for (i in colsOFwave){
  colname <- i
  varlabel <- paste(attr(wave5[,colname], "label"))
  rowName <- cbind(colname,varlabel)
  dsNames5 <- data.frame(rbind(dsNames5,rowName))
}
print(dsNames5)

rm(list = c("rowName", "colname", "curWave", "i", "varlabel", "colsOFwave"))


# Assigning new names to the variables
dsNames1$varname <- ifelse(
  dsNames1$colname == "v1"   , c("CaseID"), ifelse(
    dsNames1$colname == "v2"   , c("PairID"), ifelse(
      dsNames1$colname == "v116" , c("Marital"),NA)))


# Assigning new names to the variables
dsNames2$varname <- ifelse(
  dsNames2$colname == "v1"   , c("CaseID"), ifelse(
    dsNames2$colname == "v2"   , c("PairID"), ifelse(
      dsNames2$colname == "b116" , c("Marital"),NA)))

# Assigning new names to the variables
dsNames3$varname <- ifelse(
  dsNames3$colname == "v1"   , c("CaseID"), ifelse(
    dsNames3$colname == "v2"   , c("PairID"), ifelse(
      dsNames3$colname == "c116" , c("Marital"),NA)))


a <- wave1[,c("v1",'v2',"v116")]
b <- wave2[,c("v1","v2","b116")]
c <- wave3[,c("v1","v2","c116")]
require(plyr)
mergeOn <- c("v1","v2")
allWaves <- plyr::join(a, b, mergeOn)
allWaves <- plyr::join(allWaves, c, mergeOn)


saveRDS(allWaves,"./Data/Derived/Unshared/dsW.rds")

