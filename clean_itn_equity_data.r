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
raw_data_fname <- file.path(parent_dir, "itn_with_housing-1-2-0.csv")

# load data from Camilo
data_raw <- fread(raw_data_fname)

# looks amazing, but: still no ISO3
# what does hhsize mean if it's neither dejure nor defacto pop?

# start by isolating only the main data we need: 
# hh pop, itn access, country, time, and wealth quintile/index
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
                            n_dejure_pop,
                            n_defacto_pop,
                            n_slept_under_itn,
                            n_itn,
                            itn_theoretical_capacity,
                            wealth_quintile_dhs=wealth_index,
                            wealth_index_score)]
itn_data <- itn_data[order(country_name, dhs_survey_id, clusterid, hhid)]

# no longer any nulls in  wealth index :)

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

# what does it mean when n_dejure_pop is zero?

# save this dataset to explore in another script.
write.csv(itn_data, file=file.path(parent_dir, "itn_equity_cleaned.csv"), row.names=F)





