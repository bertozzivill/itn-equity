###############################################################################################################
## clean_itn_equity_data.r
## Amelia Bertozzi-Villa
## December 2023
## 
## Cleand and prpep the ITN equity DHS data shared by Camilo Vargas of
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

# start by isolating only the main data we need: 
# hh pop, itn access, country, time, and wealth quintile/index
colnames <- names(data_raw)
which(colnames=="hhid")
data_raw[, which(colnames=="hhid")]
itn_data <- data_raw[, list(surveyid, 
                            dhs_survey_id,
                            country_name,
                            clusterid,
                            hhid,
                            latitude,
                            longitude,
                            interview_month,
                            interview_year,
                            survey_year_label,
                            hh_sample_wt,
                            hh_size,
                            n_defacto_pop,
                            n_slept_under_itn,
                            n_itn,
                            itn_theoretical_capacity,
                            wealth_quintile=wealth_index,
                            wealth_quintile_recode=wealth_index_recode,
                            wealth_index_score)]
itn_data <- itn_data[order(country_name, dhs_survey_id, clusterid, hhid)]

# some nulls in wealth index-- make sure that they only exist across 
# entire surveys, not within surveys
contains_nulls <- itn_data[is.na(wealth_quintile)]
contains_nulls_surveys <- unique(contains_nulls$dhs_survey_id)
table(itn_data[dhs_survey_id %in% contains_nulls_surveys]$dhs_survey_id)
table(contains_nulls$dhs_survey_id)

# ok, the issue is complete surveys except for one instance in 
# SN2014DHS and four instances in TZ2007AIS-- check those out
itn_data[dhs_survey_id %in% c("SN2014DHS", "TZ2007AIS") & is.na(wealth_quintile)]

# can't say, but it's a small problem... drop them?
itn_data <- itn_data[!is.na(wealth_quintile)]

# this dataset looks complete, except for some lat-longs... great! 
summary(itn_data)


# if n_slept_under_itn is never greater than n_defacto_pop,
# set n_slept_under_itn to n_defacto_pop-- 
# if someone did not sleep in the household, their net use cannot be connected to that household
itn_data[n_slept_under_itn>n_defacto_pop, n_slept_under_itn:=n_defacto_pop]

# calculate household-level itn access
itn_data[, access:= itn_theoretical_capacity/n_defacto_pop]
# set to zero for households with no people
itn_data[is.na(access) & n_defacto_pop==0, access:=0]

# and use
itn_data[, use:=n_slept_under_itn/n_defacto_pop]
# again, set to zero
itn_data[is.na(use) & n_defacto_pop==0, use:=0]

# todo: do we even want to keep households with "no" members? it's a very small proportion (0.26%)
# I guess this represents households where everyone slept somewhere else last night? hh_size is >0
# but n_defacto_pop is zero.

# save this dataset to explore in another script.
write.csv(itn_data, file=file.path(parent_dir, "itn_equity_cleaned.csv"), row.names=F)





