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

# re-calculate quintile the DHS way, weighting by household sample
# weight and household size
itn_data[, hhsize_hh_weight:= hh_sample_wt*hh_size/1000000]
itn_data[, wealth_quintile_replicated:= cut(x = wealth_index_score, 
                                            breaks = Hmisc::wtd.quantile(x = wealth_index_score, weights=hhsize_hh_weight,
                                                                         probs=0:5/5, 
                                                                         normwt=FALSE),
                                            labels = FALSE, include.lowest = TRUE),
         by=dhs_survey_id]

# also calculate wealth quintile with hh sample weighting only
itn_data[, wealth_quintile_hh_only:= cut(x = wealth_index_score, 
                                            breaks = Hmisc::wtd.quantile(x = wealth_index_score, weights=hh_sample_wt,
                                                                         probs=0:5/5, 
                                                                         normwt=FALSE),
                                            labels = FALSE, include.lowest = TRUE),
         by=dhs_survey_id]

# how big is the difference between the manual recode and dhs?  
itn_data[, quintile_discrepancy:= wealth_quintile==wealth_quintile_replicated]
prop.table(table(itn_data$dhs_survey_id, itn_data$quintile_discrepancy), margin=1)


# looks great for everything except the Angola 2011 MIS, which has only a 48% 
# accuracy rate? 
prop.table(table(itn_data[dhs_survey_id=="AO2011MIS"]$wealth_quintile_replicated))

# use the manual version for now so it's replicable.
# how big is the difference between using hh size and not?
itn_data[, quintile_discrepancy:= wealth_quintile_replicated==wealth_quintile_hh_only]
prop.table(table(itn_data$dhs_survey_id, itn_data$quintile_discrepancy), margin=1)



itn_data[,wealth_quintile_hhonly_recode:=factor(wealth_quintile_hh_only, 
                                                   labels=c("Poorest",
                                                            "Poorer",
                                                            "Middle",
                                                            "Richer",
                                                            "Richest"))]
wealth_quints_v2 <- itn_data[!is.na(wealth_quintile_hh_only), list(count=.N, type="no_hhsize_weighting"), by=list(dhs_survey_id,
                                                    wealth_quintile=wealth_quintile_hhonly_recode)]
wealth_quints_v2[, proportion:=count/sum(count), by=dhs_survey_id]
wealth_quints_v2 <- wealth_quints_v2[order(dhs_survey_id, wealth_quintile)]
wealth_quints_v2[, count:=NULL]

wealth_quints <- rbind(wealth_quints, wealth_quints_v2, fill=T)
wealth_quints[is.na(type), type:="with_hhsize_weighting"]


ggplot(wealth_quints, aes(x=wealth_quintile, y=proportion)) +
  geom_hline(yintercept = 0.2) +
  geom_violin(aes(fill=wealth_quintile, color=wealth_quintile), alpha=0.5) +
  stat_summary(fun.data=mean_sdl, 
               geom="pointrange")+
  facet_grid(.~type)+
  theme_minimal() +
  theme(legend.position = "none")
