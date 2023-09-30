###preparing environment: unloaded prior packages, loading new ones----

#unload packages that were loaded before (run function twice to "catch" all pkgs)
#this is a workaround to avoid masking problems when running the scripts successively
lapply(names(sessionInfo()$otherPkgs), function(pkgs)
  detach(
    paste0('package:', pkgs),
    character.only = T,
    unload = T,
    force = T
  ))

lapply(names(sessionInfo()$otherPkgs), function(pkgs)
  detach(
    paste0('package:', pkgs),
    character.only = T,
    unload = T,
    force = T
  ))

#load libraries
library(tidyverse)

###[1] Raw data import----------------------------------------------------------
#the folders of the participants excluded before the trial contain a results file 
#with the same header but only one row NA as data

# create list of rawfiles with subdirectory info included (folder name includes ID)
raw.files <- data_frame(filename = list.files(pattern = "results.csv$", recursive = T))

#helper function to create a column with filename:
# "Create a new R function called read.csv.and.add.filename which expects to be 
# passed a path to a csv file as an input. This function reads the csv file at the
# path (converting it to a dataframe), and adds a new column containing the 
# original file path it read from. It then returns this dataframe."

read.csv.and.add.filename <- function(filename){
  read_csv(filename) %>%
    mutate(filename=filename)
}

# add the filename as a new column in the dataframe
rawdata_ID_all  <- raw.files %>%
  rowwise() %>%
  do(., read.csv.and.add.filename(.$filename))

#derive the exluded/included status from the foldername of the subjects data
#split the column "filename" into 3 columns "id", "excluded" and "time of exclusion"
rawdata_ID_all <- rawdata_ID_all  %>% 
  separate(filename, into = c(NA, NA,"id", "excl", "excl_time"))

#when the column "excl_time" contains the value "csv" it should be transformed
# to value "Included"
rawdata_ID_all$excl_time[rawdata_ID_all$excl_time == "csv"] <- "Included"


#rename column 1 into sample_nr and column 9 to "phot_lux"
names(rawdata_ID_all)[1] <- "sample_nr"
names(rawdata_ID_all)[9] <- "phot_lux"



#save the data into a R data file called "rawfiles"
save(rawdata_ID_all, file="./02_rawdata/rawfiles.rda")
