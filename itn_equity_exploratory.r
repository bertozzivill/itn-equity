###############################################################################################################
## itn_equity_exploratory.r
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

# immediate questions:
# where is access and use, how is it calculated?
# what is "occ_hh_has_enough_itn"?
# what do columns hv226 and hv106 indicate?
# two surveyid columns

# I'd like to compare these to the datasets from the stock and flow model to see how they line up

# first, which of these two files do I want to use?
from_stockflow <- fread(file.path(parent_dir, "../preliminary_data/access_from_stockflow_prep", "input_itn_hh_data_all.csv"))

# compare survey inclusion
survey_list_camilo <- unique(data_raw$dhs_survey_id)
survey_list_stockflow <- unique(from_stockflow$SurveyId)
shared_surveys <- intersect(survey_list_camilo, survey_list_stockflow)
camilo_only_surveys <- setdiff(survey_list_camilo, survey_list_stockflow)
stockflow_only_surveys <- setdiff(survey_list_stockflow, survey_list_camilo)

#Camilo's list excludes MICS, which we expect. 
# todo: flag other surveys for camilo
# what's going on with survey 273 in camilo's dataset? lots of nulls
# can I get country iso3?

# camilo's data also includes surveys outside of Africa:
# IA? -- India
# NM? -- Namibia
# HT-- Haiti
# MM-- Myanmar
# AF -- Afghanistan
# PG -- Papua New Guinea
# TL -- Timor Leste
# PK -- Pakistan
# KH -- Cambodia
# SZ -- Eswatini
# VN -- Vietnam
# ID -- Indonesia
# GY -- Guyana
# ST -- Sao Tome & Principe


# for now, drop all non-joint surveys so we can compare and merge datasets
data_raw <- data_raw[dhs_survey_id %in% shared_surveys]
from_stockflow <- from_stockflow[SurveyId %in% shared_surveys]

# for the purposes of finding the commonalities and differences between the
# the datasets, keep only the columns in camilo's data that relate to nets
itn_cols <- names(data_raw)[1:30]
intersect(itn_cols, names(from_stockflow))
setdiff(itn_cols, names(from_stockflow))

# strip leading whitespace and leading zeros from household ids so merge can proceed
data_raw[, hhid:= trimws(hhid)]
from_stockflow[, hhid:= trimws(hhid)]

data_raw[, hhid:= sub("^0+", "", hhid)]
from_stockflow[, hhid:= sub("^0+", "", hhid)]

itn_data_raw <- data_raw[, ..itn_cols]

# to start, just explore why the from_stockflow dataset has more rows 
# than camilo's data-- keep only survey, hhid, year, month, and an indicator column
# unique to each dataset
test_merge_camilo <- data_raw[, list(dhs_survey_id, 
                                     clusterid,
                                     hhid,
                                     interview_month,
                                     interview_year,
                                     from_camilo=1)]
test_merge_camilo <- test_merge_camilo[order(dhs_survey_id, clusterid, hhid)]


test_merge_stockflow <- from_stockflow[, list(dhs_survey_id=SurveyId,
                                              clusterid=as.integer(clusterid),
                                              hhid,
                                              interview_month=month,
                                              interview_year=year,
                                              from_stockflow=1)]
test_merge_stockflow <- test_merge_stockflow[order(dhs_survey_id, clusterid, hhid)]


merged_data <- merge(test_merge_camilo, test_merge_stockflow, all = T)

bad_left_merge <- merged_data[is.na(from_stockflow)]

bad_left_example_survey <- unique(bad_left_merge$dhs_survey_id)[[1]]
bad_left_example <- bad_left_merge[dhs_survey_id==bad_left_example_survey]
bad_left_example_stockflow <- test_merge_stockflow[dhs_survey_id==bad_left_example_survey]
bad_left_example_camilo <- test_merge_camilo[dhs_survey_id==bad_left_example_survey]

# ok, only survey left in the bad left merge is ET2005DHS, which has two issues:
# 1. discrepancy in interview month between the two data sources,
# 2. additional rows in stockflow dataset-- 924 extra households:
bad_left_example_stockflow[!hhid %in% bad_left_example_camilo$hhid] 


bad_right_merge <- merged_data[is.na(from_camilo)]

bad_right_example_survey <- unique(bad_right_merge$dhs_survey_id)[[1]]
bad_right_example <- bad_right_merge[dhs_survey_id==bad_right_example_survey]
bad_right_example_stockflow <- test_merge_stockflow[dhs_survey_id==bad_right_example_survey]
bad_right_example_camilo <- test_merge_camilo[dhs_survey_id==bad_right_example_survey]

# in this example, as with ET2005DHS above, it looks like there are rows in the stockflow dataset that simply
# don't exist in the newly-extracted dataset? Do one more merge to confirm it's not a date discrepancy
are_households_missing <- merge(bad_right_merge[, list(dhs_survey_id,
                                                       clusterid,
                                                       hhid)], 
                                test_merge_camilo[, list(dhs_survey_id,
                                                         clusterid,
                                                         hhid,
                                                         from_camilo)],
                                all.x = T)
nrow(are_households_missing) == nrow(bad_right_merge)

# make sure the numbers make sense
good_merge_count <- nrow(merged_data[!is.na(from_camilo) & !is.na(from_stockflow)])
leftover_camilo <- nrow(data_raw) - good_merge_count
leftover_stockflow <- nrow(from_stockflow) - good_merge_count
# they don't make sense... going to simplify by removing dates

ggplot(are_households_missing, aes(x=dhs_survey_id)) +
  geom_bar() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))



