

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

options(digits=4)

rm(list=ls())

# filepaths
parent_dir <- "~/Dropbox (IDM)/Malaria Team Folder/projects/map_general/itn_equity/"
input_data_fname <- file.path(parent_dir, "primary_data/itn_equity_cleaned.csv")
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
pfpr_data <- pfpr_data[Metric=="Infection Prevalence" & Name %in% country_survey_map$country_name]

ggplot(pfpr_data, aes(x=Year, y=Value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Name)

##### Find national-level int access for each survey
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

# order by the highest index value for the highest quintile
sort_wealth_quintile <- wealth_by_quintile[wealth_quintile=="Richest"]
sort_wealth_quintile <- sort_wealth_quintile[order(metric, wealth_index_score)]
sort_wealth_quintile[, wealth_index_order:=seq_len(.N), by=.(metric)]
wealth_by_quintile_ordered <- merge(wealth_by_quintile, sort_wealth_quintile[, list(metric,
                                                                                    dhs_survey_id, 
                                                                                    wealth_index_order)],
                                    by=c("metric", "dhs_survey_id"))

ggplot(wealth_by_quintile_ordered[metric=="wealth_quintile_by_household"],
       aes(x=wealth_index_order, y=wealth_index_score, fill=wealth_quintile)) +
  geom_bar(stat="identity", position = "dodge") +
  facet_grid(metric~.) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))




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

# merge pfpr and national access onto this
access_and_pfpr <- merge(access_by_quintile_hh,
                         pfpr_data[, list(country_name=Name,
                                          year=Year,
                                          pfpr=Value/100)],
                         by=c("country_name", "year"),
                         all.x=T)

access_and_pfpr <- merge(access_and_pfpr, national_access, by="dhs_survey_id", all.x=T)

ggplot(access_and_pfpr, aes(x=pfpr, y=national_access)) +
  geom_point(aes(color=year)) +
  facet_wrap(~country_name)


ggplot(access_and_pfpr,
       aes(x=year, y=access, color=wealth_quintile)) +
  geom_line() +
  geom_pointrange(aes(ymin=access-se, ymax=access+se)) +
  geom_line(aes(y=pfpr), color="black") +
  geom_point(aes(y=pfpr), color="black") +
  facet_wrap(~country_name)+
  scale_color_manual(values = rev(pnw_palette("Bay",5)),
                     name="Wealth Quintile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x="", y="ITN Access",
       title="ITN Access by Country and Time")


ggplot(access_and_pfpr[!is.na(pfpr)], aes(x=wealth_quintile, y=as.factor(pfpr), fill=access)) +
  geom_tile() + 
  scale_fill_distiller(palette = "YlGnBu", direction=1)


# Let's split these up by number of surveys to see if we can tell a clearer story
access_by_quintile_hh[, survey_count:=.N, by=.(country_name, wealth_quintile)]
# prettier survey names
access_by_quintile_hh[, survey_label:= paste(country_name, year)]

ggplot(access_by_quintile_hh[survey_count==1],
       aes(x=wealth_quintile, y=reorder(survey_label, access), fill=access)) +
  geom_tile() +
  geom_text(aes(label=round(access, 2))) +
  scale_fill_distiller(palette = "YlGnBu", direction=1) +
  theme_minimal() +
  labs(x="", y="",
       title="ITN Access by Wealth Quintile,\nSingle-Survey Countries")

ggplot(access_by_quintile_hh[survey_count==2],
       aes(x=year, y=access, color=wealth_quintile)) +
  geom_line() +
  geom_pointrange(aes(ymin=access-se, ymax=access+se)) +
  facet_wrap(~country_name)+
  theme_minimal() +
  labs(x="", y="ITN Access")

ggplot(access_by_quintile_hh[survey_count>2],
       aes(x=year, y=access, color=wealth_quintile)) +
  geom_line() +
  geom_pointrange(aes(ymin=access-se, ymax=access+se)) +
  facet_wrap(~country_name)+
  theme_minimal() +
  labs(x="", y="ITN Access")


# what does this look like when you order by wealth index score? 

access_with_wealth_order <- merge(access_by_quintile_hh, sort_wealth_quintile[, list(metric,
                                                                                    dhs_survey_id, 
                                                                                    wealth_index_order)],
                                    by=c("metric", "dhs_survey_id"),
                                  all.x=T)

 
  

ggplot(access_with_wealth_order,
       aes(x=wealth_index_order, y=access, fill=wealth_quintile)) +
  geom_bar(stat="identity", position = "dodge") +
  facet_grid(metric~.) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))


### Let's compare wealth inequality to itn access inequality
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