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
library(stringr)
library(wesanderson)

options(digits=4)

rm(list=ls())

# filepaths
parent_dir <- "~/Dropbox (IDM)/Malaria Team Folder/projects/map_general/itn_equity/"
input_data_fname <- file.path(parent_dir, "primary_data/itn_equity_cleaned.csv")

# load data 
itn_data <- fread(input_data_fname)

wealth_quintile_levels <- c("Poorest",
                            "Poorer",
                            "Middle",
                            "Richer",
                            "Richest")

# calculate wealth quintile three different ways: as it comes out of the box,
# as our way of replicating what comes out of the box, and weighting 
# only by hh_sample_weight instead of hh_sample_weight*n_dejure_pop
itn_data[, hh_sample_wt_times_hh:= hh_sample_wt*n_dejure_pop]

# todo: write a function for this
itn_data[, wealth_quintile_by_population:= cut(x = wealth_index_score, 
                                            breaks = Hmisc::wtd.quantile(x = wealth_index_score, 
                                                                         weights=hh_sample_wt_times_hh,
                                                                         probs=0:5/5, 
                                                                         normwt=FALSE),
                                            labels = FALSE, include.lowest = TRUE),
         by=dhs_survey_id]

# also calculate wealth quintile with hh sample weighting only
itn_data[, wealth_quintile_by_household:= cut(x = wealth_index_score, 
                                         breaks = Hmisc::wtd.quantile(x = wealth_index_score, 
                                                                      weights=hh_sample_wt,
                                                                      probs=0:5/5, 
                                                                      normwt=FALSE),
                                         labels = FALSE, include.lowest = TRUE),
         by=dhs_survey_id]

# # why are there some nulls in wealth quintile?
# null_pop_wt <- itn_data[is.na(wealth_quintile_by_population), list(dhs_survey_id, wealth_quintile_dhs, hh_sample_wt_times_hh, n_dejure_pop, wealth_index_score, wealth_quintile_by_population)]
# pop_breaks <- itn_data[dhs_survey_id %in% null_pop_wt$dhs_survey_id,
#                        list(by_pop_breaks=Hmisc::wtd.quantile(x = wealth_index_score, 
#                                                   weights=hh_sample_wt_times_hh,
#                                                   probs=0:5/5, 
#                                                   normwt=F)),
#                        by=dhs_survey_id]
# 
# null_surveys <- unique(null_pop_wt$dhs_survey_id)
# example_null_survey <- null_surveys[1]
# 
# pop_breaks[dhs_survey_id==example_null_survey]
# summary(itn_data[dhs_survey_id==example_null_survey])


########## Start functions

find_svymean <- function(svy_metric, svy_design){
  mean_dt <- svymean(make.formula(svy_metric), 
                      svy_design, na.rm=T)
  mean_dt <- as.data.table(mean_dt, keep.rownames=T)
  mean_dt[, weighting_type:=svy_design$weights_name]
  mean_dt[, metric:=svy_metric]
  return(mean_dt)
}

find_svyby <- function(svy_by, svy_metric, svy_design){
  mean_dt <- svyby(make.formula(svy_metric),
                   by = make.formula(svy_by),
                   design = svy_design, svymean,
                   na.rm=T)
  mean_dt <- as.data.table(mean_dt)
  mean_dt[, weighting_type:=svy_design$weights_name]
  
  mean_dt[, metric:=svy_by]
  return(mean_dt)
}

set_up_survey <- function(weights, data, ids){
  this_survey_design <- svydesign(data=data,
                                  ids= as.formula(paste("~", ids)), 
                                  weights= as.formula(paste("~", weights)))
  this_survey_design$weights_name <- weights
  return(this_survey_design)
}

summarize_survey <- function(weight_vals, data, ids, metric_vals, by_vals=NULL){
  svy_designs <- lapply(weight_vals, set_up_survey, data=data, ids=ids)
  
  if (length(by_vals)==0){
    all_means <- rbindlist(lapply(svy_designs, 
                                  function(this_survey_design){
                                    rbindlist(lapply(metric_vals,
                                                     find_svymean,
                                                     svy_design=this_survey_design))
                                  }))
  }else{
    all_means <- rbindlist(lapply(svy_designs, 
                                  function(this_survey_design){
                                    rbindlist(lapply(by_vals,
                                                     find_svyby,
                                                     svy_metric=metric_vals,
                                                     svy_design=this_survey_design),
                                              use.names=F)
                                  }))
  }
  
  return(all_means)
}

# convert all wealth quintile vals to factor
metric_vals <- c("wealth_quintile_dhs", "wealth_quintile_by_population", "wealth_quintile_by_household")

itn_data[,(metric_vals):=lapply(.SD, factor, labels=wealth_quintile_levels),
         .SDcols=metric_vals]

unique_surveys <- unique(itn_data$dhs_survey_id)
weight_vals <- c("hh_sample_wt", "hh_sample_wt_times_hh")

##### Check dhs aggregation
# are 20% of households in each survey in each wealth quintile?

all_wealth_quintiles <- rbindlist(lapply(unique_surveys, function(this_survey){
  these_means <- summarize_survey(data=itn_data[dhs_survey_id==this_survey], 
                                  ids = "clusterid",
                                  weight_vals = weight_vals,
                                  metric_vals = metric_vals)
  these_means[, dhs_survey_id:=this_survey]
  
}))

all_wealth_quintiles[, wealth_quintile:= factor(gsub(paste(metric_vals, collapse="|"), "", rn), 
                              levels=wealth_quintile_levels)]
all_wealth_quintiles[, rn:=NULL]

# check that means always sum to one

all_wealth_quintiles[, mean:=round(mean, 4)]
test_sums <- all_wealth_quintiles[, list(tot=sum(mean)), 
              by=.(dhs_survey_id, metric, weighting_type)]
summary(test_sums)

#ok, looks good. 
setnames(all_wealth_quintiles, "mean", "proportion")

ggplot(all_wealth_quintiles, aes(x=wealth_quintile, y=proportion)) +
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

ggplot(all_wealth_quintiles[weighting_type=="hh_sample_wt_times_hh" & metric=="wealth_quintile_dhs"],
       aes(x=dhs_survey_id, y=proportion, fill=wealth_quintile)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=c(0.2, 0.4, 0.6, 0.8)) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))
unique(all_wealth_quintiles[weighting_type=="hh_sample_wt_times_hh" & metric=="wealth_quintile_dhs" & proportion!=0.2])



# Tas' question: what does median household wealth by quintile look like under the different metrics?
print("calculating average de jure pop by quintile")
hhsize_by_quintile <- rbindlist(lapply(unique_surveys, function(this_survey){
  
  these_means <- summarize_survey(data=itn_data[dhs_survey_id==this_survey], 
                                  ids = "clusterid",
                                  weight_vals = "hh_sample_wt",
                                  metric_vals = "n_dejure_pop",
                                  by_vals = c("wealth_quintile_by_household")
  )
  setnames(these_means, "wealth_quintile_by_household", "wealth_quintile")
  these_means[, dhs_survey_id:=this_survey]
  
}))

hhsize_by_quintile[ , quintile_size_rank := order(order(n_dejure_pop, decreasing = FALSE)), by=.(metric,dhs_survey_id) ]
hhsize_by_quintile[ , quintile_size_factor := factor(quintile_size_rank,
                                              labels=c("Smallest", "Smaller", "Middle", "Bigger", "Biggest"))]


# find a variable that orders from smallest to largest, starting in the "Poorest" category and moving upwards
find_row_ranking <- dcast.data.table(hhsize_by_quintile, dhs_survey_id ~ wealth_quintile, value.var = "quintile_size_rank") 
find_row_ranking <- find_row_ranking[order(Poorest, Poorer, Middle, Richer, Richest)]
find_row_ranking[, row_order:= .I]
hhsize_by_quintile <- merge(hhsize_by_quintile, find_row_ranking[, list(dhs_survey_id, row_order)])

ggplot(hhsize_by_quintile, aes(x=wealth_quintile, y=reorder(dhs_survey_id, n_dejure_pop))) +
  geom_tile(aes(fill=n_dejure_pop)) +
  facet_grid(.~metric) + 
  theme_minimal() +
  scale_fill_distiller(palette="Spectral")

ggplot(hhsize_by_quintile, aes(x=wealth_quintile, y=reorder(dhs_survey_id, row_order))) +
  geom_tile(aes(fill=quintile_size_factor)) +
  theme_minimal() +
  scale_fill_brewer(palette="BrBG", name="Household Size Rank") +
  labs(x="Wealth Quintile",
       y="Survey")


wealth_by_quintile <- rbindlist(lapply(unique_surveys, function(this_survey){
  
  these_means <- summarize_survey(data=itn_data[dhs_survey_id==this_survey], 
                                  ids = "clusterid",
                                  weight_vals = weight_vals,
                                  metric_vals = "wealth_index_score",
                                  by_vals = c("wealth_quintile_by_population", "wealth_quintile_by_household")
  )
  setnames(these_means, "wealth_quintile_by_population", "wealth_quintile")
  these_means[, dhs_survey_id:=this_survey]
  
}))

# remove inappropriate weighting types 
wealth_by_quintile <- wealth_by_quintile[(metric=="wealth_quintile_by_household" & weighting_type=="hh_sample_wt") | 
                                           (metric!="wealth_quintile_by_household" & weighting_type!="hh_sample_wt")]


wealth_by_quintile_wide <- dcast.data.table(wealth_by_quintile, dhs_survey_id + wealth_quintile ~ metric, value.var = "wealth_index_score")
wealth_by_quintile_wide[, diff:= wealth_quintile_by_population-wealth_quintile_by_household]
wealth_by_quintile_wide[, simple_greater:=wealth_quintile_by_household>wealth_quintile_by_population]

ggplot(wealth_by_quintile_wide, aes(x=wealth_quintile_by_population, y=wealth_quintile_by_household)) +
  geom_abline() +
  geom_point(aes(color=simple_greater)) +
  theme_minimal() +
  facet_wrap(~wealth_quintile, scales="free") +
  labs(x="Wealth Index When Weighting by Population",
       y="Wealth Index When Weighting by Household",
       title="Wealth Index Comparison")

strange_subset <- unique(wealth_by_quintile_wide[wealth_quintile=="Richest" & wealth_quintile_by_household>(1e+06)]$dhs_survey_id)

all_wealth_quintiles[dhs_survey_id %in% strange_subset[[1]]]







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
