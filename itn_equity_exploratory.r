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
library(Hmisc)
library(survey)

options(digits=3)

rm(list=ls())

# filepaths
parent_dir <- "~/Dropbox (IDM)/Malaria Team Folder/projects/map_general/itn_equity/"
input_data_fname <- file.path(parent_dir, "primary_data/itn_equity_cleaned.csv")

# load data 
itn_data <- fread(input_data_fname)
setnames(itn_data, "wealth_quintile", "wealth_quintile_dhs")

wealth_quintile_levels <- c("Poorest",
                            "Poorer",
                            "Middle",
                            "Richer",
                            "Richest")

# calculate wealth quintile three different ways: as it comes out of the box,
# as our way of replicating what comes out of the box, and weighting 
# only by hh_sample_weight instead of hh_sample_weight*hh_size
itn_data[, hh_sample_wt:=hh_sample_wt/1000000]
itn_data[, hh_sample_wt_times_hh:= hh_sample_wt*hh_size]
# todo: write a function for this
itn_data[, wealth_quintile_replicated:= cut(x = wealth_index_score, 
                                            breaks = Hmisc::wtd.quantile(x = wealth_index_score, 
                                                                         weights=hh_sample_wt_times_hh,
                                                                         probs=0:5/5, 
                                                                         normwt=FALSE),
                                            labels = FALSE, include.lowest = TRUE),
         by=dhs_survey_id]

# also calculate wealth quintile with hh sample weighting only
itn_data[, wealth_quintile_simple:= cut(x = wealth_index_score, 
                                         breaks = Hmisc::wtd.quantile(x = wealth_index_score, 
                                                                      weights=hh_sample_wt,
                                                                      probs=0:5/5, 
                                                                      normwt=FALSE),
                                         labels = FALSE, include.lowest = TRUE),
         by=dhs_survey_id]

# how good is the agreement?
itn_data[, quintile_matched:= wealth_quintile_simple==wealth_quintile_replicated]
test_match <- as.data.table(prop.table(table(itn_data$dhs_survey_id, itn_data$quintile_matched), margin=1))
names(test_match) <- c("dhs_survey_id,", "is_matched", "agreement")
test_match[, agreement:= round(agreement, 2)]
test_match[is_matched=="TRUE" & agreement<0.95]

# set wealth quint to factor
# itn_data[, wealth_quintile_recode:=factor(wealth_quintile, 
#                                              labels=wealth_quintile_levels)]

find_svymean <- function(svy_metric, svy_design){
  mean_dt <- svymean(as.formula(paste("~ as.factor(", svy_metric, ")")),
                      svy_design, na.rm=T)
  mean_dt <- as.data.table(mean_dt)
  mean_dt[, weighting_type:=svy_design$weights_name]
  mean_dt[, metric:=svy_metric]
  mean_dt[, wealth_quintile:= factor(wealth_quintile_levels, levels = wealth_quintile_levels)]
  return(mean_dt)
}

set_up_survey <- function(weights, data, ids){
  this_survey_design <- svydesign(data=data,
                                  ids= as.formula(paste("~", ids)), 
                                  weights= as.formula(paste("~", weights)))
  this_survey_design$weights_name <- weights
  return(this_survey_design)
}


summarize_survey <- function(weight_vals, data, ids, metric_vals){
  svy_designs <- lapply(weight_vals, set_up_survey, data=data, ids=ids)
  all_means <- rbindlist(lapply(svy_designs, function(this_survey_design){
    rbindlist(lapply(metric_vals, find_svymean, svy_design=this_survey_design))
  }))
  return(all_means)
}


# are 20% of households in each survey in each wealth quintile?
unique_surveys <- unique(itn_data$dhs_survey_id)
metric_vals <- c("wealth_quintile_dhs", "wealth_quintile_replicated", "wealth_quintile_simple")
weight_vals <- c("hh_sample_wt", "hh_sample_wt_times_hh")

all_svy_means <- rbindlist(lapply(unique_surveys, function(this_survey){
  these_means <- summarize_survey(data=itn_data[dhs_survey_id==this_survey], 
                                  ids = "clusterid",
                                  weight_vals = weight_vals,
                                  metric_vals = metric_vals)
  these_means[, dhs_survey_id:=this_survey]
  

}))

# check that means always sum to one
test_sums <- all_svy_means[, list(tot=sum(mean)), 
              by=.(dhs_survey_id, metric, weighting_type)]
summary(test_sums)

#ok, looks good. 
setnames(all_svy_means, "mean", "proportion")


ggplot(all_svy_means[metric=="wealth_quintile_dhs" & weighting_type=="agg_simple"], 
       aes(x=wealth_quintile, y=proportion)) +
  geom_hline(yintercept = 0.2) +
  geom_violin(aes(fill=wealth_quintile, color=wealth_quintile), alpha=0.5) +
  stat_summary(fun.data=mean_sdl, 
               geom="pointrange")+
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x="Wealth Quintile",
       y="Proportion Distribution Across Surveys",
       title="Wealth Quintile from DHS,\nAggregated by HH Sampling Weights")

ggplot(all_svy_means[metric=="wealth_quintile_dhs"], 
       aes(x=wealth_quintile, y=proportion)) +
  geom_hline(yintercept = 0.2) +
  geom_violin(aes(fill=wealth_quintile, color=wealth_quintile), alpha=0.5) +
  stat_summary(fun.data=mean_sdl, 
               geom="pointrange")+
  facet_grid(.~weighting_type)+
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x="Wealth Quintile",
       y="Proportion Distribution Across Surveys",
       title="Wealth Quintile from DHS, Comparing Aggregation Method")

ggplot(all_svy_means, aes(x=wealth_quintile, y=proportion)) +
  geom_hline(yintercept = 0.2) +
  geom_violin(aes(fill=wealth_quintile, color=wealth_quintile), alpha=0.5) +
  stat_summary(fun.data=mean_sdl, 
               geom="pointrange")+
  facet_grid(metric~weighting_type)+
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x="Wealth Quintile",
       y="Proportion Distribution Across Surveys",
       title="Comparison Across Data Type and Aggregation Type")



# according to the DHS docs: chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://dhsprogram.com/pubs/pdf/CR6/CR6.pdf
# The cut points in the wealth index at which to form the quintiles are
# calculated by obtaining a weighted frequency distribution of households,
# the weight being the product of the number of de jure members of the
# household and the sampling weight of the household.
# Thus, the distribution represents the national household population,
# where each member is given the wealth index score of his or her household.
# The persons are then ordered by the score, and the distribution is
# divided at the points that form the five 20- percent sections.
# Then the household score is recoded into the quintile variable so
# that each member of a household also receives that household’s
# quintile category. 
