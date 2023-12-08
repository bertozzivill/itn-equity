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
options(digits=2)

rm(list=ls())

# filepaths
parent_dir <- "~/Dropbox (IDM)/Malaria Team Folder/projects/map_general/itn_equity/"
input_data_fname <- file.path(parent_dir, "primary_data/itn_equity_cleaned.csv")

# load data 
itn_data <- fread(input_data_fname)

# set wealth quint to factor
itn_data[, wealth_quintile_recode:=factor(wealth_quintile, 
                                             labels=c("Poorest",
                                                      "Poorer",
                                                      "Middle",
                                                      "Richer",
                                                      "Richest"))]

# is 20% of each survey in each wealth quintile?
wealth_quints <- itn_data[, list(count=.N), by=list(dhs_survey_id,
                                                    wealth_quintile=wealth_quintile_recode)]
wealth_quints[, proportion:=count/sum(count), by=dhs_survey_id]
wealth_quints <- wealth_quints[order(dhs_survey_id, wealth_quintile)]


ggplot(wealth_quints, aes(x=wealth_quintile, y=proportion)) +
  geom_hline(yintercept = 0.2) +
  geom_violin(aes(fill=wealth_quintile, color=wealth_quintile), alpha=0.5) +
  stat_summary(fun.data=mean_sdl, 
               geom="pointrange")+
  theme_minimal() +
  theme(legend.position = "none")

# no, but 20% across the dataset appears to be? how can this be?
table(itn_data$wealth_quintile)
prop.table(table(itn_data$wealth_quintile))

# sort by proportion in quintile
wealth_quints <- dcast.data.table(wealth_quints, dhs_survey_id~wealth_quintile, value.var="proportion")
wealth_quints <- wealth_quints[order(Poorest, Poorer, Middle, Richer, Richest)]
wealth_quints[, dhs_survey_id:=reorder(dhs_survey_id, .I)]
wealth_quints <- melt(wealth_quints, id.vars = "dhs_survey_id", 
                      variable.name = "wealth_quintile", 
                      value.name="proportion")

# heatmap across surveys
# sort by lowest prop in 1st quintile
# wealth_quints[, dhs_survey_id:=reorder(dhs_survey_id, proportion)]
ggplot(wealth_quints, aes(y=wealth_quintile, x=dhs_survey_id, fill=proportion)) +
  geom_tile() +
  scale_fill_gradient2(midpoint = 0.2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

## not so beautiful, but fine for now

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

# Let's see if we can recreate this 
recreate_wealth_quint <- itn_data[, list(dhs_survey_id,
                                         country_name,
                                         clusterid,
                                         hhid,
                                         hh_sample_wt,
                                         hh_size,
                                         wealth_index_score)]

# obtaining a weighted frequency distribution of households,
# the weight being the product of the number of de jure members of the
# household and the sampling weight of the household.
# dividing by 1,000,000 to make hh sampling weights the right value
recreate_wealth_quint[, updated_weight:=hh_sample_wt*hh_size/1000000]

# try just taking the weighted quantile with this value?
recreate_wealth_quint[, wealth_quintile_test := cut(x = wealth_index_score, 
                     breaks = Hmisc::wtd.quantile(x = wealth_index_score, weights=updated_weight,
                                                  probs=0:5/5, 
                                                  normwt=FALSE),
                     labels = FALSE, include.lowest = TRUE), 
   by = .(dhs_survey_id) ]

# merge this back onto og dataset
test_wealth_quint <- merge(itn_data, recreate_wealth_quint)
ggplot(test_wealth_quint, aes(x=wealth_quintile, y=wealth_quintile_recode)) +
  geom_abline() +
  geom_point()
test_wealth_quint[wealth_quintile!=wealth_quintile_test, list(dhs_survey_id,
                                                              wealth_quintile, wealth_quintile_test, 
                                                              diff=wealth_quintile-wealth_quintile_test)]


