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
library(survey)
# library(stringr)
library(wesanderson)

options(digits=4)

rm(list=ls())

# filepaths
parent_dir <- "~/Dropbox (IDM)/Malaria Team Folder/projects/map_general/itn_equity/"
input_data_fname <- file.path(parent_dir, "primary_data/itn_equity_cleaned.csv")
function_fname <- "~/repos/itn-equity/itn_equity_functions.r"
source(function_fname)

# load data 
itn_data <- fread(input_data_fname)
country_survey_map <- unique(itn_data[, list(dhs_survey_id, country_name)])


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
itn_data[is.na(wealth_quintile_by_household) & dhs_survey_id=="IA2020DHS", wealth_quintile_by_household:=1]

# convert all wealth quintile vals to factor
wealth_quintile_levels <- c("Poorest",
                            "Poorer",
                            "Middle",
                            "Richer",
                            "Richest")

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

# who isn't being aggregated correctly?
dhs_only_check <- all_wealth_quintiles[weighting_type=="hh_sample_wt_times_hh" & metric=="wealth_quintile_dhs"]
dhs_only_check[, proportion:= round(proportion, 2)]
mismatched_proportions <- unique(dhs_only_check[proportion!=0.2]$dhs_survey_id)


##### Tas' question: Are the poorest households usually the largest?

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


##### Tas' question: what does median household wealth by quintile look like under the different metrics?

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
  facet_wrap(~wealth_quintile) +
  labs(x="Wealth Index When Weighting by Population",
       y="Wealth Index When Weighting by Household",
       title="Wealth Index Comparison")


ggplot(wealth_by_quintile[metric=="wealth_quintile_by_household"],
       aes(x=dhs_survey_id, y=wealth_index_score, fill=wealth_quintile)) +
  geom_bar(stat="identity", position = "dodge") +
  facet_grid(metric~.) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))





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
