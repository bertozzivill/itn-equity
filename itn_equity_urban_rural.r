

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

unique_surveys <- unique(itn_data$dhs_survey_id)
weight_vals <- c("hh_sample_wt", "hh_sample_wt_times_hh")
wealth_quintile_levels <- c("Poorest",
                            "Poorer",
                            "Middle",
                            "Richer",
                            "Richest")

# convert wealth quintile columns to factor
to_convert_cols <- names(itn_data)[names(itn_data) %like% "wealth_quintile"]
itn_data[, c(to_convert_cols) := lapply(.SD, factor, levels=wealth_quintile_levels), .SDcols = to_convert_cols]

# load national-level pfpr data
pfpr_data <- fread(file.path(parent_dir, "primary_data/National_Unit-data.csv"))
pfpr_data[ISO3=="CIV", Name:="Cote d'Ivoire"]
pfpr_data[ISO3=="COD", Name:="Congo Democratic Republic"]
pfpr_data[ISO3=="STP", Name:="Sao Tome and Principe"]
pfpr_data[ISO3=="SWZ", Name:="Eswatini"]
pfpr_data <- pfpr_data[Metric=="Infection Prevalence" & Name %in% country_survey_map$country_name,
                       list(country_name=Name,
                            iso3=ISO3,
                            name=Name,
                            year=Year,
                            pfpr=Value/100)]



# load geofacet 
ssa_grid <- fread("~/repos/itn-equity/geofacet_ssa_itn_equity.csv")

ggplot(pfpr_data, aes(x=year, y=pfpr)) +
  geom_line() +
  geom_point() +
  facet_geo(~name, grid = ssa_grid, label="name")

# use this to add iso3 to the country map
country_survey_map <- merge(country_survey_map, unique(pfpr_data[, list(country_name, iso3)]), all.x=T)

##### Find national-level itn access for each survey
national_access <- rbindlist(lapply(unique_surveys, function(this_survey){
  
  these_means <- summarize_survey(data=itn_data[dhs_survey_id==this_survey], 
                                  ids = "clusterid",
                                  weight_vals = "hh_sample_wt",
                                  metric_vals = "access"
  )
  these_means[, dhs_survey_id:=this_survey]
}))
# I have no idea why, but converting a svystat object to a data 
# table renames the "SE" column to "access"... correct this 
national_access <- national_access[, list(dhs_survey_id, 
                                          national_access=mean,
                                          national_access_se=access)]



##### What does median household wealth by quintile look like under the different metrics?

wealth_by_quintile <- rbindlist(lapply(unique_surveys, function(this_survey){
  
  these_means <- summarize_survey(data=itn_data[dhs_survey_id==this_survey], 
                                  ids = "clusterid",
                                  weight_vals = weight_vals,
                                  metric_vals = "wealth_index_score",
                                  by_vals = c("wealth_quintile_by_population",
                                              "wealth_quintile_by_household")
  )
  setnames(these_means, "wealth_quintile_by_population", "wealth_quintile")
  these_means[, dhs_survey_id:=this_survey]
  
}))

# remove inappropriate weighting types 
wealth_by_quintile <- wealth_by_quintile[(metric=="wealth_quintile_by_household" & weighting_type=="hh_sample_wt") | 
                                           (metric!="wealth_quintile_by_household" & weighting_type!="hh_sample_wt")]

wealth_by_quintile <- merge(wealth_by_quintile, country_survey_map, all.x = T)
wealth_by_quintile[, survey_count:= seq_len(.N), by=.(metric, wealth_quintile, country_name)]


##### What does ITN Access by wealth quintile look like?
access_by_quintile <- rbindlist(lapply(unique_surveys, function(this_survey){
  
  these_means <- summarize_survey(data=itn_data[dhs_survey_id==this_survey], 
                                  ids = "clusterid",
                                  weight_vals = weight_vals,
                                  metric_vals = "access",
                                  by_vals = c("wealth_quintile_by_population",
                                              "wealth_quintile_by_household",
                                              "wealth_quintile_dhs")
  )
  setnames(these_means, "wealth_quintile_by_population", "wealth_quintile")
  these_means[, dhs_survey_id:=this_survey]
  
}))

# remove inappropriate weighting types 
access_by_quintile <- access_by_quintile[(metric=="wealth_quintile_by_household" & weighting_type=="hh_sample_wt") | 
                                           (metric!="wealth_quintile_by_household" & weighting_type!="hh_sample_wt")]

access_by_quintile <-merge(access_by_quintile, country_survey_map, all.x=T)
access_by_quintile[, year:= as.integer(gsub(".*([0-9]{4}).*", "\\1", dhs_survey_id))]

# choose to focus on the houshold-level metric for comparability
access_by_quintile_hh <- access_by_quintile[metric=="wealth_quintile_by_household"]
access_by_quintile_hh[, name:=country_name]
