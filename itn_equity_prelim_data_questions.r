###############################################################################################################
## itn_equity_prelim_data_questions.r
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

# load stock and flow data
from_stockflow <- fread(file.path(parent_dir, "../preliminary_data/access_from_stockflow_prep", "input_itn_hh_data_all.csv"))

# compare survey inclusion
survey_list_itnequity <- unique(data_raw$dhs_survey_id)
survey_list_stockflow <- unique(from_stockflow$SurveyId)
shared_surveys <- intersect(survey_list_itnequity, survey_list_stockflow)
itnequity_only_surveys <- setdiff(survey_list_itnequity, survey_list_stockflow)
stockflow_only_surveys <- setdiff(survey_list_stockflow, survey_list_itnequity)

# Camilo's list excludes MICS, which we expect. 
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

# for the purposes of finding the commonalities and differences between the
# the datasets, keep only the columns in camilo's data that relate to nets
itn_cols <- names(data_raw)[1:30]
itn_data_raw <- data_raw[, ..itn_cols]

# to start, just explore why the from_stockflow dataset has more rows 
# than camilo's data-- keep only survey, hhid, and an indicator column
# unique to each dataset
# todo: add month and year back in
test_merge_itnequity <- data_raw[, list(dhs_survey_id, 
                                        clusterid,
                                        hhid,
                                        from_itnequity=1)]
test_merge_itnequity <- test_merge_itnequity[order(dhs_survey_id, clusterid, hhid)]


test_merge_stockflow <- from_stockflow[, list(dhs_survey_id=SurveyId,
                                              clusterid=as.integer(clusterid),
                                              hhid,
                                              from_stockflow=1)]
test_merge_stockflow <- test_merge_stockflow[order(dhs_survey_id, clusterid, hhid)]

# duplicates? No.
nrow(unique(test_merge_itnequity))==nrow(test_merge_itnequity)
nrow(unique(test_merge_stockflow))==nrow(test_merge_stockflow)
test_merge_stockflow[duplicated(test_merge_stockflow)]

merged_data <- merge(test_merge_itnequity, test_merge_stockflow, all = T)

# 90,268 entries are missing from the new dataset-- where did they go?
summary(merged_data)

# does something seem wrong a priori with the households that aren't included?
dropped_hhs <- from_stockflow[is.na(merged_data$from_itnequity)]
summary(dropped_hhs)

# many, but not all, are zeros... is that a reason to drop, unless n_defacto_pop is zero?
# are these refugees etc?