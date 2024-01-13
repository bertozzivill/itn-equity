###############################################################################################################
## itn_equity_cluster.r
## Amelia Bertozzi-Villa
## January 2024
## 
## Explore the equity between wealth, access, and pfpr at the cluster level
##############################################################################################################
library(data.table)
library(ggplot2)
library(survey)
library(wesanderson)
library(PNWColors)
library(geofacet)

options(digits=4)

rm(list=ls())

# filepaths
parent_dir <- "~/Dropbox (IDM)/Malaria Team Folder/projects/map_general/itn_equity/"
input_data_fname <- file.path(parent_dir, "cleaned_input_data/itn_equity_cleaned.csv")
function_fname <- "~/repos/itn-equity/itn_equity_functions.r"
source(function_fname)

# load itn data 
itn_data <- fread(input_data_fname)
country_survey_map <- unique(itn_data[, list(dhs_survey_id, country_name)])


weight_vals <- c("hh_sample_wt", "hh_sample_wt_times_hh")
wealth_quintile_levels <- c("Poorest",
                            "Poorer",
                            "Middle",
                            "Richer",
                            "Richest")

# convert wealth quintile columns to factor
to_convert_cols <- names(itn_data)[names(itn_data) %like% "wealth_quintile"]
itn_data[, c(to_convert_cols) := lapply(.SD, factor, levels=wealth_quintile_levels), .SDcols = to_convert_cols]

# load geofacet 
ssa_grid <- fread("~/repos/itn-equity/geofacet_ssa_itn_equity.csv")


#### explore cluster-level relationships across surveys

itn_data[, micro_value:= pmin(examined_micro, 1)]
itn_data[, rdt_value:= pmin(examined_rdt, 1)]

table(itn_data$dhs_survey_id, itn_data$rdt_value, useNA = "ifany")

# keep only those surveys where they take any malaria tests at all
pfpr_surveys <- unique(itn_data[!is.na(examined_micro) | !is.na(examined_rdt)]$dhs_survey_id)
itn_data <- itn_data[dhs_survey_id %in% pfpr_surveys]

table(itn_data$dhs_survey_id, itn_data$micro_value, useNA = "ifany")


# also open qs here about how to consider wealth quintile (which is household-level)
# as we explore the relationship with pfpr and access

# to start, just take raw cluster-level access and cluster-level pfpr

sum_vals <- c("itn_theoretical_capacity",
              "n_defacto_pop",
              "examined_micro",
              "positive_micro",
              "examined_rdt",
              "positive_rdt"
              )

itn_by_cluster <- itn_data[, lapply(.SD, sum, na.rm=T),
                   .SDcols=sum_vals,
                   by=list(dhs_survey_id,
                           country_name,
                           year,
                           clusterid
                   )]

itn_by_cluster[, access:= itn_theoretical_capacity/n_defacto_pop]
itn_by_cluster[, prev_micro:=positive_micro/examined_micro]
itn_by_cluster[, prev_rdt:=positive_rdt/examined_rdt]

ggplot(itn_by_cluster, aes(x=prev_micro, y=prev_rdt)) +
  geom_point()

ggplot(itn_by_cluster, aes(x=access, y=prev_rdt)) +
  geom_point()

ggplot(itn_by_cluster, aes(x=access, y=prev_micro)) +
  geom_point()

ggplot(itn_by_cluster, aes(x=access, y=prev_rdt)) +
  geom_point() +
  facet_wrap(~dhs_survey_id, scales="free")

# todo: map this

# to consider adding wealth: we could either calculate wealth quintile by *cluster*, 
# or we could aggregate up the households. 

# take wealth index by cluster
wealth_by_cluster <- itn_data[, lapply(.SD, mean, na.rm=T),
                           .SDcols=c("wealth_index_score",
                                     "hh_sample_wt"),
                           by=list(dhs_survey_id,
                                   country_name,
                                   year,
                                   clusterid
                           )]

# what are the quintile cutoffs for this compared to the quintile cutoffs overall?

compare_cutoffs <- rbind(wealth_by_cluster[, list(label="cluster",
                                                  cutoff_perc=seq(0, 100, 20),
                                                  cutoff=wtd.quantile(x=wealth_index_score,
                                                                      weights=hh_sample_wt,
                                                                      probs=0:5/5)),
                                           by=dhs_survey_id],
                         itn_data[, list(label="household",
                                                  cutoff_perc=seq(0, 100, 20),
                                                  cutoff=wtd.quantile(x=wealth_index_score,
                                                                      weights=hh_sample_wt,
                                                                      probs=0:5/5)),
                                           by=dhs_survey_id])

ggplot(compare_cutoffs, aes(x=cutoff_perc, y=cutoff, fill=label)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~dhs_survey_id, scales="free")

# we'll decide what cutoff to use later. for now, plot wealth along with pfpr and access:
itn_by_cluster <- merge(itn_by_cluster, wealth_by_cluster, all.x=T)

ggplot(itn_by_cluster, aes(x=wealth_index_score, y=prev_rdt, color=access)) +
  geom_point() +
  facet_wrap(~dhs_survey_id)

ggplot(itn_by_cluster, aes(x=wealth_index_score, y=access, color=prev_rdt)) +
  geom_point()+
  facet_wrap(~dhs_survey_id)




# come back to within-cluster heterogeneity later
# How much variation is there between households
# by cluster in wealth?
this_survey <- pfpr_surveys[[71]]

this_data <- itn_data[dhs_survey_id==this_survey]
cluster_quint_props_test <- prop.table(table(this_data$clusterid,
                                             this_data$wealth_quintile_by_household),
                                       margin=1)

cluster_quint_props <- this_data[, list(quint_count= .N),
                                    by=list(dhs_survey_id,
                                            country_name,
                                            year,
                                            clusterid,
                                            wealth_quintile_by_household)]
cluster_quint_props[, prop:= quint_count/sum(quint_count),
                    by=list(dhs_survey_id,
                            country_name,
                            year,
                            clusterid)]

ggplot(cluster_quint_props,
       aes(x=wealth_quintile_by_household, y=clusterid, fill=prop)) + 
  geom_tile() +
  scale_fill_distiller(palette='GnBu')

