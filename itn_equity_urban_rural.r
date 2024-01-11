

###############################################################################################################
## itn_equity_national.r
## Amelia Bertozzi-Villa
## December 2023
## 
## Aggregate surveys to the national level and explore the relationship between 
## wealth quintile and ITN access
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

# the urban/rural label is absent in some surveys--- calculate what percentage of each
urban_rural_missing_surveys <- unique(itn_data[cluster_urban_rural==""]$dhs_survey_id) 
urban_rural_missing <- itn_data[dhs_survey_id %in% urban_rural_missing_surveys]
table(urban_rural_missing$dhs_survey_id)

#oh, it's in all of these surveys... drop em
print("dropping the following surveys with missing urban/rural labels")
print(urban_rural_missing_surveys)
itn_data <- itn_data[!dhs_survey_id %in% urban_rural_missing_surveys]
itn_data[, urban_rural:= ifelse(cluster_urban_rural=="U", "Urban", "Rural")]

rm(urban_rural_missing, urban_rural_missing_surveys)

unique_surveys <- unique(itn_data$dhs_survey_id)


##### Find urban/rural-level itn access for each survey
urban_rural_access <- rbindlist(lapply(unique_surveys, function(this_survey){
  
  these_means <- summarize_survey(data=itn_data[dhs_survey_id==this_survey], 
                                  ids = "clusterid",
                                  weight_vals = "hh_sample_wt",
                                  metric_vals = "access",
                                  by_vals = "urban_rural"
  )
  these_means[, dhs_survey_id:=this_survey]
}))

urban_rural_access <-merge(urban_rural_access, country_survey_map, all.x=T)
urban_rural_access[, year:= as.integer(gsub(".*([0-9]{4}).*", "\\1", dhs_survey_id))]
urban_rural_access[, name:=country_name]

ggplot(urban_rural_access,
       aes(x=year, y=access, color=urban_rural)) +
  geom_line() +
  geom_pointrange(aes(ymin=access-se, ymax=access+se)) +
  facet_geo(~name, grid = ssa_grid, label="name") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x="", y="ITN Access",
       title="ITN Access by Country and Time")


##### What does ITN Access by wealth quintile and urbanicity look like?

access_by_quintile <- rbindlist(lapply(unique_surveys, function(this_survey){
  
  these_means <- summarize_survey(data=itn_data[dhs_survey_id==this_survey], 
                                  ids = "clusterid",
                                  weight_vals = weight_vals,
                                  metric_vals = "access",
                                  by_vals = c("~wealth_quintile_by_population+urban_rural",
                                              "~wealth_quintile_by_household+urban_rural",
                                              "~wealth_quintile_dhs+urban_rural")
  )
  setnames(these_means, "wealth_quintile_by_population", "wealth_quintile")
  these_means[, dhs_survey_id:=this_survey]
  
}))

# remove inappropriate weighting types 
access_by_quintile[, metric:=gsub("~", "", metric)]
access_by_quintile[, metric:=gsub("\\+urban_rural", "", metric)]

access_by_quintile <- access_by_quintile[((metric== "wealth_quintile_by_household") & (weighting_type=="hh_sample_wt")) | 
                                           ((metric != "wealth_quintile_by_household") & (weighting_type!="hh_sample_wt"))]

access_by_quintile <-merge(access_by_quintile, country_survey_map, all.x=T)
access_by_quintile[, year:= as.integer(gsub(".*([0-9]{4}).*", "\\1", dhs_survey_id))]

# check weighting types
access_by_quintile_wide <- dcast.data.table(access_by_quintile, 
                                            dhs_survey_id + wealth_quintile + urban_rural ~ metric, 
                                            value.var="access")

ggplot(access_by_quintile_wide, aes(x=wealth_quintile_by_household, 
                                    y=wealth_quintile_by_population)) +
  geom_abline() +
  geom_point()

ggplot(access_by_quintile_wide, aes(x=wealth_quintile_by_household, 
                                    y=wealth_quintile_dhs)) +
  geom_abline() +
  geom_point()

ggplot(access_by_quintile_wide, aes(x=wealth_quintile_by_population, 
                                    y=wealth_quintile_dhs)) +
  geom_abline() +
  geom_point()

# choose to focus on the houshold-level metric for comparability
access_by_quintile_hh <- access_by_quintile[metric %like%  "wealth_quintile_by_household"]
access_by_quintile_hh[, name:=country_name]
