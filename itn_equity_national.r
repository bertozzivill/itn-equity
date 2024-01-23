

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


##### Remove purchased nets from the itn dataset-- calculate access based *exclusively* 
##### on freely-provided nets

provenance_data <- fread(file.path(parent_dir, "cleaned_input_data/provenance_by_hh.csv"))
prov_surveys <- unique(provenance_data$dhs_survey_id)

# keep a record of the original number of itns (free and paid)
itn_data[, n_itn_total:= n_itn]

# keep an option to remove the surveys without provenance data, even if you don't account for paid nets
provenance_data_only <- T

if (provenance_data_only){
  # subset down only to the surveys with provenance data
  itn_data <- itn_data[dhs_survey_id %in% prov_surveys]
  unique_surveys <- prov_surveys
}

remove_paid_nets <- T

if (remove_paid_nets){
  itn_data_orig <- copy(itn_data)
  
  provenance_wide <- dcast.data.table(provenance_data, 
                                      dhs_survey_id + clusterid + hhid ~ paid_type, 
                                      value.var = "net_count")
  provenance_wide[is.na(free), free:= 0]
  provenance_wide[is.na(paid), paid:= 0]
  
  itn_data <- merge(itn_data,provenance_wide, all=T)
  
  # TEMP: drop the extra clusters from the uganda 2018 mis that don't exist in the hh data
  itn_data <- itn_data[!(dhs_survey_id=="UG2018MIS" & clusterid>320)]
  
  # data should be missing only where n_itn==0
  itn_data[is.na(free), free:= 0]
  itn_data[is.na(paid), paid:= 0]
  
  # confirm one last time that the sum of the net_counts column equals n_itn
  itn_data[, test_tot_itns:= free + paid]

  # excellent
  itn_data[n_itn!=test_tot_itns]
  itn_data[, test_tot_itns:= NULL]
  
  # ok, now replace n_itn with the number of free itns only
  itn_data[, n_itn:= free]
  
  # drop the "free" and "paid" columns
  itn_data[, c("free", "paid") := NULL]
  
  setdiff(names(itn_data), names(itn_data_orig))
  rm(itn_data_orig); gc()
}


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

ggplot(access_by_quintile_hh,
       aes(x=year, y=access, color=wealth_quintile)) +
  geom_line() +
  geom_point() +
  facet_geo(~name, grid = ssa_grid, label="name") +
  scale_color_manual(values = rev(pnw_palette("Bay",5)),
                     name="Wealth Quintile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x="", y="ITN Access")

# merge pfpr and national access onto this
access_and_pfpr <- merge(access_by_quintile_hh,
                         pfpr_data,
                         by=c("country_name", "name", "iso3", "year"),
                         all.x=T)

access_and_pfpr <- merge(access_and_pfpr, national_access, 
                         by= c("dhs_survey_id"), all.x=T)


ggplot(access_and_pfpr, aes(x=year, y=pfpr)) +
  geom_point(aes(color=national_access)) +
  scale_color_distiller(palette = "YlGn", direction=1) +
  facet_geo(~name, grid = ssa_grid, label="name") +
  theme(axis.text.x = element_text(angle=45, hjust=1))


# todo: make this story clearer
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



# explore wealth quintile specifically
ggplot(wealth_and_access[survey_count==1],
       aes(x=wealth_quintile, y=reorder(survey_label, wealth_index_score), fill=wealth_index_score)) +
  geom_tile() +
  geom_text(aes(label=round(wealth_index_score, 2))) +
  scale_fill_distiller(palette = "YlGnBu", direction=1) +
  theme_minimal() +
  labs(x="", y="",
       title="Wealth Index by Wealth Quintile,\nSingle-Survey Countries")

ggplot(wealth_and_access,
       aes(x=year, y=wealth_index_score, color=wealth_quintile)) +
  geom_hline(yintercept=0) +
  geom_line() +
  geom_point() +
  facet_geo(~name, grid = ssa_grid, label="name") + 
  theme_minimal() +
  labs(x="", y="Wealth Index")


# what does wealth vs access look like overall?
ggplot(wealth_and_access, aes(x=wealth_quintile, y=access, fill=wealth_quintile)) +
  geom_point(alpha=0.5) +
  geom_violin( alpha=0.5) +
  geom_boxplot(width=0.2) +
  scale_fill_manual(values = rev(pnw_palette("Bay",5)),
                    name="Wealth Quintile") +
  theme_minimal() +
  labs(x="",
       y="ITN Access")

ggplot(wealth_and_access, aes(x=access_rank_factor, fill=wealth_quintile)) +
  geom_bar()+
  scale_fill_manual(values = rev(pnw_palette("Bay",5)),
                              name="Wealth Quintile") +
  geom_text(aes(label=after_stat(count)), 
            stat='count', 
            position=position_stack(vjust=0.5))+
  theme_minimal() +
  labs(x="",
       y="Count")


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

find_access_gap <- dcast.data.table(
  wealth_and_access[access_rank_factor %in% c("Lowest Access", "Highest Access")],
  country_name + dhs_survey_id ~ access_rank_factor, value.var = "access"
)
find_access_gap[, access_gap:=`Highest Access` - `Lowest Access`]


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

all_gaps[, name:=country_name]
ggplot(all_gaps, aes(x=year,
                     ymin=`Lowest Access`,
                     ymax=`Highest Access`)) +
  geom_linerange() +
  geom_point(aes(y=`Lowest Access`, color=`Lowest Access  Wealth Quintile`), size=2)+
  geom_point(aes(y=`Highest Access`, color=`Highest Access  Wealth Quintile`), size=2)+
  scale_color_manual(values = rev(pnw_palette("Bay",5)),
                     name="Wealth Quintile") +
  facet_geo(~name, grid = ssa_grid, label="name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(y="Access",
       title="Access Ranges over Country and Time")

# ok, looking at size of access gap is getting interesting.
ggplot(all_gaps, aes(x=year, y=access_gap)) +
  geom_line() + 
  geom_point(aes(color=`Highest Access  Wealth Quintile`), size=2) +
  facet_geo(~name, grid = ssa_grid, label="name") +
  scale_color_manual(values = rev(pnw_palette("Bay",5)),
                     name="Wealth Quintile\nwith Highest Access") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x="",
       y="Access Gap",
       title="")


# look at access gap vs national access and prevalence 

all_gaps <- merge(all_gaps, national_access, by="dhs_survey_id", all.x=T)
all_gaps <- merge(all_gaps, pfpr_data,
      by=c("country_name", "name", "year"),
      all.x=T)

ggplot(all_gaps, aes(x=pfpr, y=access_gap)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x="PfPR", y="Access Gap")

ggplot(all_gaps, aes(x=pfpr, y=access_gap)) +
  geom_point(aes(color=`Highest Access  Wealth Quintile`)) +
  facet_grid(~`Highest Access  Wealth Quintile`) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x="PfPR", y="Access Gap")

ggplot(all_gaps, aes(x=pfpr, y=access_gap)) +
  geom_point(aes(color=`Highest Access  Wealth Quintile`)) +
  scale_color_manual(values = rev(pnw_palette("Bay",5)),
                     name="Wealth Quintile\nwith Highest Access") +
  facet_geo(~name, grid = ssa_grid, label="name") +
  #theme_minimal() +
  labs(x="PfPR", y="Access Gap",
       title="PfPR vs Access Gap, Mapped")

##### How does the access gap relate to the timing of mass campaigns?
net_dists <- fread(file.path(parent_dir, "cleaned_input_data/itn_dists_cleaned.csv"))

# keep only the countries we need, and merge on the proper country name
net_dists <- merge(unique(country_survey_map[, list(country_name, iso3)]),
                   net_dists[, list(iso3, year, nets_distributed,mass_campaign)],
                   by="iso3",
                   all.x=T)
net_dists[, name:=country_name]

net_dists[, counter:= cumsum(mass_campaign==T), by=iso3]
net_dists[, yrs_since_campaign:= ifelse(mass_campaign == F & counter>0, seq_len(.N)-1, NA_integer_), 
          by=list(iso3, counter)]
net_dists[mass_campaign==T, yrs_since_campaign:=0]

ggplot(net_dists, aes(x=year, y=nets_distributed)) +
  geom_line() +
  geom_point() +
  facet_wrap(~iso3, scales="free_y") +
  theme_minimal()

ggplot(net_dists, aes(x=year, y=nets_distributed)) +
  geom_line() +
  geom_point(aes(color=mass_campaign)) +
  facet_wrap(~iso3, scales="free_y") +
  theme_minimal()



ggplot(net_dists, aes(x=year, y=nets_distributed)) +
  geom_line() +
  geom_point(aes(color=factor(yrs_since_campaign), shape=mass_campaign)) +
  # facet_geo(~name, grid = ssa_grid, label="name", scales="free_y") 
  facet_wrap(~iso3, scales="free_y") +
  theme_minimal()

# creates nulls for Eswatini 2006, Namibia 2006, ST&P 2008
all_gaps <- merge(all_gaps, net_dists[, list(country_name, 
                                         year, 
                                         nets_distributed, 
                                         mass_campaign, 
                                         yrs_since_campaign)],
              by=c("country_name", "year"), all.x=T) 


all_gaps[is.na(yrs_since_campaign), yrs_since_campaign:=-1]

ggplot(all_gaps, aes(x=factor(yrs_since_campaign), y=access_gap, fill=factor(yrs_since_campaign))) +
  geom_point(alpha=0.5) +
  geom_violin(alpha=0.5) +
  geom_boxplot(width=0.1) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x="Years Since Last Campaign",
       y="Access Gap")

ggplot(all_gaps, aes(x=factor(yrs_since_campaign), y=access_gap, color=`Highest Access  Wealth Quintile`)) +
  # geom_violin(aes(group=yrs_since_campaign), alpha=0.5) +
  geom_boxplot(aes(group=yrs_since_campaign)) +
  geom_point() +
  facet_grid(.~`Highest Access  Wealth Quintile`) +
  scale_color_manual(values = rev(pnw_palette("Bay",5)),
                     name="Wealth Quintile\nwith Highest Access") +
  # facet_geo(~name, grid = ssa_grid, label="name", scales="free_y") 
  theme_minimal() +
  labs(x="Years Since Last Campaign",
       y="Access Gap")

ggplot(all_gaps, aes(x=year, y=access_gap)) +
  geom_line() + 
  geom_point(aes(color=factor(yrs_since_campaign)), size=2) +
  facet_geo(~name, grid = ssa_grid, label="name") +
  # scale_color_manual(values = rev(pnw_palette("Bay",5)),
  #                    name="Wealth Quintile\nwith Highest Access") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(y="Access",
       title="Access Gap over Country and Time")

