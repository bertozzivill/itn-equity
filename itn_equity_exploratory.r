###############################################################################################################
## itn_equity_exploratory.r
## Amelia Bertozzi-Villa
## November 2023
## 
## Begin exploring the ITN equity DHS data shared by Tasmin Symons of
## MAP on November 21, 2023 via Google Drive
##############################################################################################################


library(data.table)
library(ggplot2)

rm(list=ls())

# filepaths
parent_dir <- "~/Dropbox (IDM)/Malaria Team Folder/projects/map_general/itn_equity/preliminary_data"
tas_data_fname <- file.path(parent_dir, "child_table.csv")


# load data from Tas
tas_data_raw <- fread(tas_data_fname)

# hmm, these only have use data, not access. Downloaded the latest cleaned datasets from the
# ITN model workflow, let's see if they can be merged on. 

# first, which of these two files do I want to use?
from_stockflow <- fread(file.path(parent_dir, "access_from_stockflow_prep", "input_itn_hh_data_all.csv"))
hh_data_survey <- fread(file.path(parent_dir, "access_from_stockflow_prep", "input_itn_hh_survey_data.csv"))

# file input_itn_hh_data_all contains more informative columns, though it also contains rows with missing lat-longs.
rm(hh_data_survey); gc()

# compare and merge datasets
length(unique(tas_data_raw$surveyname))
length(unique(from_stockflow$SurveyId))
length(intersect(unique(tas_data_raw$surveyname), unique(from_stockflow$SurveyId)))
# looks like all the surveys Tas got are included in the stockflow data, that's good.
# what surveys are missing?
remaining_surveys <- setdiff(unique(from_stockflow$SurveyId), unique(tas_data_raw$surveyname))
remaining_surveys[!remaining_surveys %like% "DHS"]
# looks like there are 52 other surveys, 47 of which are DHS, that we could include?

# for now, subset down to only the surveys in Tas' list
from_stockflow <- from_stockflow[!SurveyId %in% remaining_surveys]

# my first interpretation of this work is that we're interested in the household level, so I'll pull 
# only household level wealth columns from Tas' data and merge on to the stockflow table
wealth_data <- unique(tas_data_raw[, list(SurveyName=surveyname,
                                       subregion,
                                       clusterid,
                                       householdid,
                                       date_of_interview,
                                       wealth_score,
                                       wealth_quintile)]
                          )



# adjust from_stockflow to match names
setnames(from_stockflow, c("SurveyId", "hhid"),
         c("SurveyName", "householdid"))
# clusterid is a character vector for some reason?
from_stockflow[, clusterid:=as.integer(clusterid)]

# sort by survey, cluster, and householdid for ease of use
from_stockflow <- from_stockflow[order(SurveyName, clusterid, householdid)]

# strip leading whitespace from household ids so merge can proceed
wealth_data[, householdid:= trimws(householdid)]
from_stockflow[, householdid:= trimws(householdid)]

# also strip leading zeros 
wealth_data[, householdid:= sub("^0+", "", householdid)]
from_stockflow[, householdid:= sub("^0+", "", householdid)]

# merge, retaining rows so we can see what if there's disconnect
itn_wealth_data <- merge(from_stockflow, wealth_data, all = T)

# trimming whitespece helped with the merge, but still some issues
# Not surprised that there's wealth data missing, 
# but I am surprised that there's so much itn data missing.
good_merge <- itn_wealth_data[complete.cases(itn_wealth_data)]
bad_right_merge <- itn_wealth_data[is.na(access)]

test_survey <- unique(bad_right_merge$SurveyName)[[1]]
test_stockflow <- from_stockflow[SurveyName==test_survey]
test_wealth <- wealth_data[SurveyName==test_survey]



