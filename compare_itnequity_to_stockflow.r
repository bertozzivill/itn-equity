###############################################################################################################
## compare_itnequity_to_stockflow.r
## Amelia Bertozzi-Villa
## December 2023
## 
## Begin exploring the ITN equity DHS data shared by Camilo Vargas of
## MAP on December 1, 2023 via Google Drive
##############################################################################################################

library(data.table)
library(ggplot2)

rm(list=ls())

# filepaths
parent_dir <- "~/Dropbox (IDM)/Malaria Team Folder/projects/map_general/itn_equity/primary_data"
raw_data_fname <- file.path(parent_dir, "itn_with_housing-0-1-0.csv")

# load data from Camilo
data_raw <- fread(raw_data_fname)

# load stock and flow data
from_stockflow <- fread(file.path(parent_dir, "../preliminary_data/access_from_stockflow_prep", "input_itn_hh_data_all.csv"))

# compare survey inclusion
survey_list_itnequity <- unique(data_raw$dhs_survey_id)
survey_list_stockflow <- unique(from_stockflow$SurveyId)
shared_surveys <- intersect(survey_list_itnequity, survey_list_stockflow)

# for now, drop all non-joint surveys so we can compare and merge datasets
data_raw <- data_raw[dhs_survey_id %in% shared_surveys]
from_stockflow <- from_stockflow[SurveyId %in% shared_surveys]

# I just discovered issues with the stock-and-flow cleaning method of TZ2007AIS:
# there are some duplicated rows, and some rows where households are duplicated
# but with small differences 
# see e.g.: from_stockflow[SurveyId=="TZ2007AIS" & hhid=="3 47"]
# dropping that survey from both datasets for now to resolve the issue
data_raw <- data_raw[dhs_survey_id !="TZ2007AIS"]
from_stockflow <- from_stockflow[SurveyId !="TZ2007AIS"]

# does from_stockflow contain duplicates? Not anymore
from_stockflow[duplicated(from_stockflow)]

# strip leading whitespace and leading zeros from household ids so merge can proceed
data_raw[, hhid:= trimws(hhid)]
from_stockflow[, hhid:= trimws(hhid)]

data_raw[, hhid:= sub("^0+", "", hhid)]
from_stockflow[, hhid:= sub("^0+", "", hhid)]

# we know there are some rows in the stockflow dataset 
# that are missing from camilo's... find the indices of these and drop them
find_indices_itnequity <- data_raw[, list(dhs_survey_id, 
                                       clusterid,
                                       hhid,
                                       from_itnequity=1)]

find_indices_stockflow <- from_stockflow[, list(dhs_survey_id=SurveyId,
                                                clusterid=as.integer(clusterid),
                                                hhid,
                                                from_stockflow=1)]
find_indices_merged <- merge(find_indices_itnequity, find_indices_stockflow, all = T)

# drop the stockflow indices that don't have a match in the itn equity data
from_stockflow <- from_stockflow[!which(is.na(find_indices_merged$from_itnequity))]
rm(find_indices_itnequity, find_indices_stockflow, find_indices_merged); gc()

# for the purposes of finding the commonalities and differences between the
# the datasets, keep only the columns in camilo's data that relate to nets
itn_cols <- names(data_raw)[1:30]
itn_data_raw <- data_raw[, ..itn_cols]
rm(data_raw); gc()
itn_data_raw[, itnequity_access:=itn_theoretical_capacity/n_defacto_pop]
itn_data_raw[is.na(itnequity_access) & n_defacto_pop==0, itnequity_access:=0]

intersect(itn_cols, names(from_stockflow))
setdiff(itn_cols, names(from_stockflow))
setdiff(names(from_stockflow), itn_cols)

# rename some stockflow variables to match their counterparts
setnames(from_stockflow, 
         c("SurveyId", "year", "month", "n_with_access"),
         c("dhs_survey_id", "interview_year", "interview_month", "itn_theoretical_capacity"))
from_stockflow[, clusterid:=as.integer(clusterid)]

# for now, worry ONLY about comparing access between the two datasets
access_data_itnequity <- itn_data_raw[, list(dhs_survey_id,
                                             clusterid,
                                             hhid,
                                             interview_month,
                                             interview_year,
                                             itnequity_access)]

access_data_stockflow <- from_stockflow[, list(dhs_survey_id,
                                             clusterid,
                                             hhid,
                                             interview_month,
                                             interview_year,
                                             stockflow_access=access)]

merged_access <- merge(access_data_stockflow, access_data_itnequity)
merged_access[, diff:=stockflow_access-itnequity_access]
summary(merged_access)

# differences occur in 3,572 cases-- 0.27% of the data. ok to move on for now.
access_discrepancy <- merged_access[abs(diff)>1e-5]



