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
library(survey)

rm(list=ls())

# filepaths
parent_dir <- "~/Dropbox (IDM)/Malaria Team Folder/projects/map_general/itn_equity/primary_data"
raw_data_fname <- file.path(parent_dir, "itn_with_housing-2-0-1.csv")
function_fname <- "~/repos/itn-equity/itn_equity_functions.r"
source(function_fname)

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
                            urban_rural=ifelse(cluster_urban_rural=="", NA,
                                               ifelse(cluster_urban_rural=="U", 
                                                      "Urban", "Rural")),
                            interview_month,
                            interview_year,
                            year=as.integer(gsub(".*([0-9]{4}).*", "\\1", dhs_survey_id)),
                            survey_year_label,
                            hh_has_entry_in_net_tbl,
                            hh_sample_wt,
                            hh_size,
                            n_dejure_pop,
                            n_defacto_pop,
                            n_pop_u5,
                            n_slept_under_itn,
                            n_itn,
                            itn_theoretical_capacity,
                            examined_micro,
                            positive_micro,
                            examined_rdt,
                            positive_rdt,
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

# let's restrict to countries in Africa
africa_list <- c("Angola",
                 "Benin",
                 "Burkina Faso",
                 "Burundi",
                 "Cameroon",
                 "Chad",
                 "Comoros",
                 "Congo",
                 "Congo Democratic Republic",
                 "Cote d'Ivoire",
                 "Eswatini",
                 "Gabon",
                 "Gambia",
                 "Ghana",
                 "Guinea",
                 "Kenya",
                 "Liberia",
                 "Madagascar",
                 "Malawi",
                 "Mali",
                 "Mauritania",
                 "Mozambique",
                 "Namibia",
                 "Niger",
                 "Nigeria",
                 "Rwanda",
                 "Sao Tome and Principe",
                 "Senegal",
                 "Sierra Leone",
                 "Tanzania",
                 "Togo",
                 "Uganda",
                 "Zambia",
                 "Zimbabwe")
itn_data <- itn_data[country_name %in% africa_list]


# the following surveys have an erroneous final digit in the "wealth index score"... drop that digit.
extra_digit_surveys <- c("AO2011MIS",
                         "IA2020DHS",
                         "KH2005DHS",
                         "ML2012DHS",
                         "SN2012DHS",
                         "SN2015DHS",
                         "SN2016DHS",
                         "ZW2015DHS")

itn_data[dhs_survey_id %in% extra_digit_surveys,
         wealth_index_score:= as.integer(wealth_index_score/10)]

# also divide wealth index by 100,000 to get the actual value as advised by camilo
itn_data[,wealth_index_score:=wealth_index_score/100000]


# calculate wealth quintile three different ways: as it comes out of the box,
# as our way of replicating what comes out of the box, and weighting 
# only by hh_sample_weight instead of hh_sample_weight*n_dejure_pop
itn_data[, hh_sample_wt_times_hh:= hh_sample_wt*n_dejure_pop]

itn_data[, wealth_quintile_by_population:= cut(x = wealth_index_score, 
                                               breaks = wtd.quantile(x = wealth_index_score, 
                                                                     weights=hh_sample_wt_times_hh,
                                                                     probs=0:5/5),
                                               labels = FALSE, include.lowest = TRUE),
         by=dhs_survey_id]

# also calculate wealth quintile with hh sample weighting only
itn_data[, wealth_quintile_by_household:= cut(x = wealth_index_score, 
                                              breaks = wtd.quantile(x = wealth_index_score, 
                                                                    weights=hh_sample_wt,
                                                                    probs=0:5/5),
                                              labels = FALSE, include.lowest = TRUE),
         by=dhs_survey_id]

# a single null in wealth quintile, but it should just go to the lowest quintile...
# itn_data[is.na(wealth_quintile_by_household) & dhs_survey_id=="IA2020DHS", wealth_quintile_by_household:=1]

# convert all wealth quintile vals to factor
wealth_quintile_levels <- c("Poorest",
                            "Poorer",
                            "Middle",
                            "Richer",
                            "Richest")

metric_vals <- c("wealth_quintile_dhs",
                 "wealth_quintile_by_population",
                 "wealth_quintile_by_household")

itn_data[,(metric_vals):=lapply(.SD, factor, labels=wealth_quintile_levels),
         .SDcols=metric_vals]

# calculate prevalence
itn_data[,prev_micro:= positive_micro/examined_micro]
itn_data[, prev_rdt:=positive_rdt/examined_rdt]


#quick exploration of pfpr
pfpr_subset <- itn_data[!is.na(examined_micro) | !is.na(examined_rdt)]

# is there a concern that more kids were examined than there were kids in the house?
# is n_pop_u5 de facto or de jure?
# in some places, kids over 5 are tested, via camilo
pfpr_subset[examined_micro>n_pop_u5]

# what's the difference between an "examined_*" of 0 and an NA? 
# maybe a zero is an attempted test but no consent? should I drop those?
# from camilo: NA means household was not selected, 0 means no people were examined for whatever reason

pfpr_subset[, micro_tested:= examined_micro>0]
pfpr_subset[, rdt_tested:= examined_rdt>0]

# surprised that so many households got both rdt tests and microscopy tests?
# from camilo: yup, that's true! 
table(pfpr_subset$micro_tested, pfpr_subset$rdt_tested)
pfpr_subset[micro_tested==T & rdt_tested==T & micro_tested!=rdt_tested]

# save this dataset to explore in another script.
write.csv(itn_data, file=file.path(parent_dir, "../cleaned_input_data/itn_equity_cleaned.csv"), row.names=F)



