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
library(PNWColors)

options(digits=4)

rm(list=ls())

# filepaths
parent_dir <- "~/Dropbox (IDM)/Malaria Team Folder/projects/map_general/itn_equity/"
input_data_fname <- file.path(parent_dir, "primary_data/itn_equity_cleaned.csv")
function_fname <- "~/repos/itn-equity/itn_equity_functions.r"
source(function_fname)

# load data 
itn_data <- fread(input_data_fname)

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


wealth_by_quintile <- merge(wealth_by_quintile, country_survey_map, all.x = T)
wealth_by_quintile[, survey_count:= seq_len(.N), by=.(metric, wealth_quintile, country_name)]
surprising_surveys <- wealth_by_quintile_wide[wealth_quintile=="Richest" & wealth_quintile_by_household > 1e+06 ]$dhs_survey_id
surprising_countries <- country_survey_map[dhs_survey_id %in% surprising_surveys]$country_name

explore_surprising <- wealth_by_quintile[metric=="wealth_quintile_by_household" & country_name %in% surprising_countries]
# I suspect that the issue might be an extra trailing zero at the end of the wealth index score?
explore_surprising_full <- itn_data[dhs_survey_id %in% surprising_surveys]
explore_surprising_full[, final_digit:= wealth_index_score %% 10]
unique(explore_surprising_full$final_digit)
# it's zero everywhere except IA2020DHS, where every digit ends in one?? 


  # ggplot(explore_surprising,
  #        aes(x=wealth_quintile, y=wealth_index_score, fill=wealth_quintile)) +
  #   geom_bar(stat="identity", position = "dodge") +
  #   geom_text(data=explore_surprising[wealth_quintile=="Middle"], y=1.5e+06, aes(label=dhs_survey_id)) + 
  #   facet_grid(country_name ~ survey_count) +
  #   theme_minimal() +
  #   theme(axis.text.x = element_text(angle=45, hjust=1)) +
  #   labs(x="",
  #        y="Wealth Index Score")





##### What does ITN Access by wealth quintile look like?
access_by_quintile <- rbindlist(lapply(unique_surveys, function(this_survey){
  
  these_means <- summarize_survey(data=itn_data[dhs_survey_id==this_survey], 
                                  ids = "clusterid",
                                  weight_vals = weight_vals,
                                  metric_vals = "access",
                                  by_vals = c("wealth_quintile_by_population", "wealth_quintile_by_household", "wealth_quintile_dhs")
  )
  setnames(these_means, "wealth_quintile_by_population", "wealth_quintile")
  these_means[, dhs_survey_id:=this_survey]
  
}))

# remove inappropriate weighting types 
access_by_quintile <- access_by_quintile[(metric=="wealth_quintile_by_household" & weighting_type=="hh_sample_wt") | 
                                           (metric!="wealth_quintile_by_household" & weighting_type!="hh_sample_wt")]

access_by_quintile <-merge(access_by_quintile, country_survey_map, all.x=T)
access_by_quintile[, year:= as.integer(gsub(".*([0-9]{4}).*", "\\1", dhs_survey_id))]

compare_hh_to_dhs <- access_by_quintile[metric %in% c("wealth_quintile_by_household",
                                                      "wealth_quintile_dhs")]
compare_hh_to_dhs <- dcast.data.table(compare_hh_to_dhs, 
                                      country_name + dhs_survey_id + year + wealth_quintile ~ metric,
                                      value.var = "access")
compare_hh_to_dhs[, diff:=wealth_quintile_by_household-wealth_quintile_dhs]
compare_hh_to_dhs[, is_greater:=wealth_quintile_by_household>wealth_quintile_dhs]

ggplot(compare_hh_to_dhs, 
       aes(x=wealth_quintile_dhs,
           y=wealth_quintile_by_household,
           color=is_greater)) +
  geom_abline() +
  geom_point() +
  facet_wrap(~wealth_quintile) +
  theme_minimal() +
  labs(x="ITN Access w/ Original DHS Weighting",
       y="ITN Access w/ Household-Level Weighting")

access_by_quintile_hh <- access_by_quintile[metric=="wealth_quintile_by_household"]


ggplot(access_by_quintile_hh[country_name=="Tanzania"],
       aes(x=dhs_survey_id, y=access, fill=wealth_quintile)) +
  geom_bar(stat="identity", position = "dodge") +
  facet_grid(metric~.) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

ggplot(access_by_quintile_hh,
       aes(x=year, y=access, color=wealth_quintile)) +
  geom_line() +
  geom_pointrange(aes(ymin=access-se, ymax=access+se)) +
  facet_wrap(~country_name)+
  scale_color_manual(values = rev(pnw_palette("Bay",5)),
                     name="Wealth Quintile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x="", y="ITN Access",
       title="ITN Access by Country and Time")


ggplot(access_by_quintile_hh,
       aes(x=year, y=access, fill=wealth_quintile)) +
  geom_bar(stat="identity", position="dodge") +
  # geom_tile() +
  #geom_text(aes(label=round(access, 2))) +
  # scale_fill_distiller(palette = "YlGnBu", direction=1) +
  facet_wrap(~country_name)+
  theme_minimal() +
  labs(x="", y="")

ggplot(access_by_quintile_hh,
       aes(x=year, y=wealth_quintile, fill=access)) +
  geom_tile() +
  #geom_text(aes(label=round(access, 2))) +
  scale_fill_distiller(palette = "YlGnBu", direction=1) +
  facet_wrap(~country_name)+
  theme_minimal() +
  labs(x="", y="")

# Let's split these up by number of surveys to see if we can tell a clearer story
access_by_quintile_hh[, survey_count:=.N, by=.(country_name, wealth_quintile)]
# prettier survey names
access_by_quintile_hh[, survey_label:= paste(country_name, year)]

single_survey <- access_by_quintile_hh[survey_count==1]
ggplot(single_survey,
       aes(x=wealth_quintile, y=reorder(survey_label, access), fill=access)) +
  geom_tile() +
  geom_text(aes(label=round(access, 2))) +
  scale_fill_distiller(palette = "YlGnBu", direction=1) +
  theme_minimal() +
  labs(x="", y="",
       title="ITN Access by Wealth Quintile,\nSingle-Survey Countries")

double_survey <-  access_by_quintile_hh[survey_count==2]
ggplot(double_survey,
       aes(x=year, y=access, color=wealth_quintile)) +
  geom_line() +
  geom_pointrange(aes(ymin=access-se, ymax=access+se)) +
  facet_wrap(~country_name)+
  theme_minimal() +
  labs(x="", y="ITN Access")

more_surveys <- access_by_quintile_hh[survey_count>2]
ggplot(more_surveys,
       aes(x=year, y=access, color=wealth_quintile)) +
  geom_line() +
  geom_pointrange(aes(ymin=access-se, ymax=access+se)) +
  facet_wrap(~country_name)+
  theme_minimal() +
  labs(x="", y="ITN Access")


# Go back to basics: how are ITN access and wealth distributed in the survey itself?
ggplot(itn_data, aes(x=wealth_index_score)) +
  geom_density(aes(color=wealth_quintile_by_household)) +
  facet_wrap(~dhs_survey_id, scales="free") +
  theme_minimal()

# And access?
ggplot(itn_data, aes(x=access)) +
  # geom_density() +
  geom_density() +
  # facet_wrap(~dhs_survey_id) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x="ITN Access",
       y="Density",
       title="Distribution of ITN Access by Household Across All Surveys")

# Let's compare wealth inequality to itn access inequality
access_by_quintile_hh[ , access_rank := order(order(access, decreasing = FALSE)), 
                       by=.(dhs_survey_id) ]
access_by_quintile_hh[ , access_rank_factor := factor(access_rank,
                                                      labels=c("Lowest Access", 
                                                               "Lower Access", 
                                                               "Middle Access", 
                                                               "Higher Access",
                                                               "Highest Access"))]

# merge on wealth inequity
setnames(access_by_quintile_hh, "se", "se_access")
setnames(wealth_by_quintile, "se", "se_wealth_index_score")
setkey(access_by_quintile_hh, NULL)
setkey(wealth_by_quintile, NULL)
wealth_by_quintile[, survey_count:=NULL]

wealth_and_access <- merge(access_by_quintile_hh, wealth_by_quintile, all.x=T)

ggplot(wealth_and_access[survey_count==1],
       aes(x=wealth_quintile, y=reorder(survey_label, wealth_index_score), fill=wealth_index_score)) +
  geom_tile() +
  geom_text(aes(label=round(wealth_index_score, 2))) +
  scale_fill_distiller(palette = "YlGnBu", direction=1) +
  theme_minimal() +
  labs(x="", y="",
       title="Wealth Index by Wealth Quintile,\nSingle-Survey Countries")

ggplot(wealth_and_access[survey_count==2],
       aes(x=year, y=wealth_index_score, color=wealth_quintile)) +
  geom_hline(yintercept=0) +
  geom_line() +
  geom_point() +
  facet_wrap(~country_name)+
  theme_minimal() +
  labs(x="", y="Wealth Index")

ggplot(wealth_and_access[survey_count>2],
       aes(x=year, y=wealth_index_score, color=wealth_quintile)) +
  geom_hline(yintercept=0) +
  geom_line() +
  geom_pointrange(aes(ymin=wealth_index_score-se_wealth_index_score, ymax=wealth_index_score+se_wealth_index_score)) +
  facet_wrap(~country_name)+
  theme_minimal() +
  labs(x="", y="Wealth Index")

# compare wealth index to access--poorly informative
ggplot(wealth_and_access, aes(x=wealth_index_score, y=access)) +
  geom_point(aes(color=access_rank_factor)) +
  facet_grid(~wealth_quintile)+
  theme_minimal() 

# what is the distribution of surveys across wealth quintile and access? 
count_surveys_by_group <- wealth_and_access[, list(count=.N), 
                                            by=.(wealth_quintile, access_rank_factor)]

ggplot(count_surveys_by_group, aes(x=wealth_quintile, y=access_rank_factor)) +
  geom_tile(aes(fill=count)) +
  geom_text(aes(label=count)) +
  scale_fill_distiller(palette = "Purples", 
                       direction = 1,
                      name="No.\nof Surveys") +
  theme_minimal() +
  labs(x="",
       y="")


# How big is the discrepancy between the highest and lowest wealth quintiles?
find_wealth_gap <- dcast.data.table(
  wealth_and_access[wealth_quintile %in% c("Poorest", "Richest")],
  country_name + dhs_survey_id ~ wealth_quintile, value.var = "wealth_index_score")
find_wealth_gap[, wealth_gap:=Richest-Poorest]
ggplot(find_wealth_gap, aes(x=wealth_gap)) +
  geom_density()

find_access_gap <- dcast.data.table(
  wealth_and_access[access_rank_factor %in% c("Lowest Access", "Highest Access")],
  country_name + dhs_survey_id ~ access_rank_factor, value.var = "access"
  )
find_access_gap[, access_gap:=`Highest Access` - `Lowest Access`]
ggplot(find_access_gap, aes(x=access_gap)) +
  geom_density()


all_gaps <- merge(find_access_gap, find_wealth_gap)

# merge on wealth quintile and survey label
all_gaps <- merge(wealth_and_access[access_rank_factor %in% c("Lowest Access", "Highest Access"),
                                    list(country_name, 
                                           dhs_survey_id, 
                                           survey_label,
                                           year,
                                         wealth_quintile,
                                         access_rank_factor)],
                  all_gaps, 
                  by=c("country_name",
                       "dhs_survey_id"))

all_gaps[, access_rank_factor:= paste(access_rank_factor, " Wealth Quintile")]
all_gaps <- dcast.data.table(all_gaps, country_name + dhs_survey_id +survey_label + year + `Lowest Access` + `Highest Access` + access_gap + Poorest + Richest + wealth_gap ~ access_rank_factor, 
                 value.var = "wealth_quintile" )

ggplot(all_gaps) +
  geom_linerange(aes(x=wealth_gap, ymin=`Lowest Access`, ymax=`Highest Access`)) +
  facet_grid(`Highest Access  Wealth Quintile` ~ `Lowest Access  Wealth Quintile`) +
  theme_minimal()

ggplot(all_gaps, aes(x=`Lowest Access`, y=access_gap, color=year)) +
  geom_point(size=2) +
  scale_color_distiller(palette = "YlGn", direction=1) + 
  theme_minimal() +
  labs(y="Access Gap",
       title="Access Gap, by Lowest Access")

ggplot(all_gaps, aes(x=`Highest Access`, y=access_gap, color=year)) +
  geom_point(size=2) +
  scale_color_distiller(palette = "YlGn", direction=1) + 
  theme_minimal() +
  labs(y="Access Gap",
       title="Access Gap, by Highest Access")

ggplot(all_gaps, aes(x=access_gap,
                     ymin=`Lowest Access`,
                     ymax=`Highest Access`)) +
  geom_linerange() +
  geom_point(aes(y=`Lowest Access`, color=`Lowest Access  Wealth Quintile`), size=2)+
  geom_point(aes(y=`Highest Access`, color=`Highest Access  Wealth Quintile`), size=2)+
  scale_color_manual(values = rev(pnw_palette("Bay",5)),
                     name="Wealth Quintile") +
  facet_grid(~`Highest Access  Wealth Quintile`) +
  theme_minimal() +
  labs(x="Access Gap",
       y="Access",
       title="Access Ranges by Size of Access Gap")

ggplot(all_gaps, aes(x=year,
                     ymin=`Lowest Access`,
                     ymax=`Highest Access`)) +
  geom_linerange() +
  geom_point(aes(y=`Lowest Access`, color=`Lowest Access  Wealth Quintile`), size=2)+
  geom_point(aes(y=`Highest Access`, color=`Highest Access  Wealth Quintile`), size=2)+
  scale_color_manual(values = rev(pnw_palette("Bay",5)),
                     name="Wealth Quintile") +
  facet_wrap(~country_name) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(y="Access",
       title="Access Ranges over Country and Time")

# ok, looking at size of access gap is getting interesting.
ggplot(all_gaps, aes(x=year, y=access_gap)) +
  geom_line() + 
  geom_point(aes(color=`Highest Access  Wealth Quintile`), size=2) +
  facet_wrap(~country_name) +
  scale_color_manual(values = rev(pnw_palette("Bay",5)),
                     name="Wealth Quintile\nwith Highest Access") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(y="Access",
       title="Access Gap over Country and Time")
  
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
