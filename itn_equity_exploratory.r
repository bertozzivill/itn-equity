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

# todo: why are there some nulls in wealth quintile?

# convert all wealth quintile vals to factor
metric_vals <- c("wealth_quintile_dhs", "wealth_quintile_by_population", "wealth_quintile_by_household")

itn_data[,(metric_vals):=lapply(.SD, factor, labels=wealth_quintile_levels),
         .SDcols=metric_vals]



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

unique_surveys <- unique(itn_data$dhs_survey_id)
weight_vals <- c("hh_sample_wt", "hh_sample_wt_times_hh")

# Tas' question: what does median household wealth by quintile look like under the different metrics?
hhsize_by_quintile <- rbindlist(lapply(unique_surveys, function(this_survey){
  
  these_means <- summarize_survey(data=itn_data[dhs_survey_id==this_survey], 
                                  ids = "clusterid",
                                  weight_vals = weight_vals,
                                  metric_vals = "hh_size",
                                  by_vals = c("wealth_quintile_by_population", "wealth_quintile_by_household")
  )
  setnames(these_means, "wealth_quintile_by_population", "wealth_quintile")
  these_means[, dhs_survey_id:=this_survey]
  
}))
hhsize_by_quintile <- hhsize_by_quintile[(metric=="wealth_quintile_by_household" & weighting_type=="hh_sample_wt") | 
                                           (metric!="wealth_quintile_by_household" & weighting_type!="hh_sample_wt")]
hhsize_by_quintile[, sort_poorest:=hh_size[which(wealth_quintile=="Poorest")], by = .(metric,dhs_survey_id)]

ggplot(hhsize_by_quintile, aes(x=wealth_quintile, y=reorder(dhs_survey_id, hh_size))) +
  geom_tile(aes(fill=hh_size)) +
  facet_grid(.~metric) +
  theme_minimal() +
  scale_fill_distiller(palette="Spectral")


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

test_poorest <- wealth_by_quintile[wealth_quintile=="Poorest"]
test_poorest_wide <- dcast.data.table(test_poorest, dhs_survey_id ~ metric, value.var = "wealth_index_score")
test_poorest_wide[, diff:= wealth_quintile_by_population-wealth_quintile_by_household]
test_poorest_wide[, simple_greater:=wealth_quintile_by_household>wealth_quintile_by_population]

ggplot(test_poorest_wide[wealth_quintile_by_population>-5e+05], aes(x=wealth_quintile_by_population, y=wealth_quintile_by_household)) +
  geom_abline() +
  geom_point(aes(color=simple_greater)) +
  theme_minimal() 


ggplot(wealth_by_quintile, aes(x=wealth_quintile, y=wealth_index_score)) +
  geom_violin(aes(fill=wealth_quintile, color=wealth_quintile), alpha=0.5) +
  stat_summary(fun.data=mean_sdl, 
               geom="pointrange")+
  facet_grid(.~metric)+
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x="Wealth Quintile",
       y="Wealth Index Distribution Across Surveys",
       title="Comparison Across Data Type and Aggregation Type")


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
