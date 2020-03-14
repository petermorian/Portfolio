# packages to install 
install.packages("data.table")
install.packages("gdata")
install.packages("Janitor")
install.packages("dplyr")
install.packages("sqldf")
install.packages("tidyverse")

# library of packages to load
library(data.table)
library(gdata)
library(janitor)
library(dplyr)
library(stringr)
library(sqldf)
library(tidyverse)

# --- directores of inputs and outputs
input_loc= file.path("~/Desktop/Task/Task 1 - NHS/1. Inputs/") 
output_loc = file.path("~/Desktop/Task/Task 1 - NHS/3. Outputs/") 
testfile_name = "NHS17 Testfile.csv"
testfile_loc = paste(input_loc,testfile_name,sep="")


# --- importing testfile
testfile=data.table(read.csv(testfile_loc, header=TRUE))
used_variables=data.table(colnames(testfile))[order(V1)]
used_variables%>% mutate_if(is.factor, as.character)->used_variables

# --- automated datalist importing
datalist_col_names=c("Heading","Variable","Description", "Source")
datalisting=list()
for (i in c("dl","cf","tb")) {  # <---- user enters the number of mapping files they want automated
  datalistname=assign(paste("datalist_",i,sep=""), paste("NHS17-18_datalist/4324055001",i,"001.xls",sep=""))
  datalistloc=assign(paste("datalist_",i,"_loc",sep=""), paste(input_loc,datalistname,sep=""))
  datalisttable=assign(paste("dl_",i,sep=""),data.table(read.xls(datalistloc,sheet="Index")[-(1:5),c(1,2,3)])[,Source:=i])
  setnames(datalisttable,datalist_col_names)
  datalisttable%>%mutate_if(is.factor, as.character)->datalisttable2
  datalisting[[i]]=datalisttable2
}
dl_total = do.call(rbind, datalisting) # combine all datalist variables
dl_total=data.table(dl_total)#[order(Variable)]


# --- matching variables from testlist to datalists
dl_total[, Used:= dl_total$Variable %in% used_variables$V1] #see which ones exist in test data
dl_total_unique=data.table(distinct(dl_total,Variable,.keep_all=T)) #remove duplicate variables
# INTERESTING OBS: some variables were not found in any of the datalist files
check=used_variables[, InAnyDataList:= used_variables$V1 %in% dl_total_unique$Variable]
# record the variables that are found in both the testfile and joined datalists
dl_total_used=dl_total_unique[dl_total_unique$Used=="TRUE"] #keep only matching variables
summary(dl_total_used$Used)
#279 variables out of 395 (71% match for mapping)
#276 from dl, 3 fom tb, 0 from cf

# THIS IS DUE TO SOME VARIABLES HAVING RANGES NOT SPECIFIED
# WHHOR (01 to 60)
# WPM01 (01 - 60)
# WPM02 (01 - 60)
# DBACTIO(A-G)
# DISAB(A-F)
# DISQ14(A-E)
# DISQ16(A-F)
# MEDSMEN(A-E)


#redoing datalist mappings using wildcards
dl_total = do.call(rbind, datalisting) # combine all datalist variables
#dl_total=data.table(dl_total)[order(Variable)]
dl_total%>% mutate_if(is.factor, as.character)->dl_total
for (j in 1:nrow(dl_total)){ 
  variable=dl_total$Variable[j]
  description=dl_total$Description[j]
  if (endsWith(variable,")")){
    if (grepl("\\s[:(:].*?$", variable)) {   # DEFINED RANGE OF RESPONSES "(1-K)" OR " (1-K)"
      dl_total$Variable2[j]=gsub("\\s[:(:].*?$","%",variable)
    } else {
      dl_total$Variable2[j]=gsub("[:(:].*?$","%",variable)
    }
  } else {
    if (grepl("[:<:].*?[:>:]$", description)) {  # MULTIPLE RESPONSES <...>
      dl_total$Variable2[j]=paste(variable, "%",sep="")
    } else {
      dl_total$Variable2[j]=variable
    }
  }
}

dl_total_unique=data.table(distinct(dl_total,Variable2,.keep_all=T)) 
testfile_datafile_map=sqldf("
  select A.V1 as testfile_var, B.Variable as datafile_var, B.Source as dl_source
  from used_variables as A
  left join dl_total_unique as B
  on A.V1 like B.Variable2
  ")
testfile_datafile_map=data.table(testfile_datafile_map)
# export mappings 
saveRDS(testfile_datafile_map, paste(output_loc,"mapping_pm.rds", sep=""))
write.csv(testfile_datafile_map, paste(output_loc,"mapping_pm.csv", sep=""))

# --- data checks for mapping table 
check_nulls=testfile_datafile_map[is.na(testfile_datafile_map$datafile_var)]
check_nulls
# variables unable to be mapped:
# AGEMTHS - age in months
# NHIFINWT - selected persons weight (https://www.abs.gov.au/ausstats/abs@.nsf/Latestproducts/4324.0.55.001Main%20Features502017-18?opendocument&tabname=Summary&prodno=4324.0.55.001&issue=2017-18&num=&view=)
# cannot find these by inspection of datalists
check_dl_source=testfile_datafile_map[testfile_datafile_map$dl_source!="dl"]
nrow(check_dl_source)
# 3 additonals variables were found from "tb" & "cf", but not in "dl"



dl_total=data.table(dl_total)
fill_NA <- function(x) {
  which.na <- c(which(!is.na(x)), length(x) + 1)
  values <- na.omit(x)
  
  if (which.na[1] != 1) {
    which.na <- c(1, which.na)
    values <- c(values[1], values)
  }
  
  diffs <- diff(which.na)
  return(rep(values, times = diffs))
}


