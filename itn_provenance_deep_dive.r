##############################################################################################################
## itn_provenance_deep_dive.r
## Amelia Bertozzi-Villa
## January 2024
## 
## Dig into ITN access by wealth quintile, with and without provenance data
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
parent_dir <- "~/Dropbox (IDM)/Malaria Team Folder/projects/map_general/itn_equity/results/national"
function_fname <- "~/repos/itn-equity/itn_equity_functions.r"
source(function_fname)

ssa_grid <- fread("~/repos/itn-equity/geofacet_ssa_itn_equity.csv")


# load data
all_nets <- fread(file.path(parent_dir, "wealth_and_access_prov_surveys_all_nets.csv"))
all_nets[, type:="all nets"]

free_only_nets <- fread(file.path(parent_dir, "wealth_and_access_prov_surveys_free_nets_only.csv"))
free_only_nets[, type:="free only"]

compare_nets <- rbind(all_nets, free_only_nets)

all_gaps <- fread(file.path(parent_dir, "access_gaps_prov_surveys_all_nets.csv"))
all_gaps[, type:="all nets"]

free_only_gaps <- fread(file.path(parent_dir, "access_gaps_prov_surveys_free_nets_only.csv"))
free_only_gaps[, type:="free only"]

compare_gaps <- rbind(all_gaps, free_only_gaps)

wealth_quintile_levels <- c("Poorest",
                            "Poorer",
                            "Middle",
                            "Richer",
                            "Richest")

compare_nets[, wealth_quintile:=factor(wealth_quintile, levels=wealth_quintile_levels)]
compare_gaps[, `Highest Access  Wealth Quintile`:=factor(`Highest Access  Wealth Quintile`,
                                                         levels=wealth_quintile_levels)]
compare_gaps[, `Lowest Access  Wealth Quintile`:=factor(`Lowest Access  Wealth Quintile`,
                                                         levels=wealth_quintile_levels)]

compare_nets[, access_rank_factor:= factor(access_rank_factor, levels=c("Lowest Access", 
                                                                        "Lower Access", 
                                                                        "Middle Access", 
                                                                        "Higher Access",
                                                                        "Highest Access"))]


# how different is access when you control for net provenance?

# look at the whole survey first, no wealth quintiles
compare_nets_survey_level <- unique(compare_nets[, list(dhs_survey_id,
                                                        country_name,
                                                        name,
                                                        iso3,
                                                        year,
                                                        survey_label,
                                                        type,
                                                        national_access)]) 

compare_nets_survey_level_wide <- dcast.data.table(compare_nets_survey_level, 
                                              dhs_survey_id + country_name + name + iso3 + year +  survey_label + year ~ type, 
                                              value.var = "national_access")
compare_nets_survey_level_wide[, diff:= `all nets` - `free only`]
compare_nets_survey_level_wide[, percent_of_all:=`free only` / `all nets` * 100]

compare_nets_survey_level_wide <- compare_nets_survey_level_wide[order(percent_of_all)]
compare_nets_survey_level_wide[, ordered_label_percentage:= factor(seq_len(.N), labels=survey_label)]

ggplot(compare_nets_survey_level_wide, aes(x=ordered_label_percentage, y=percent_of_all)) +
  geom_bar(stat='identity', alpha=0.5) + 
  geom_text(aes(label=round(percent_of_all, 0))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x="",
       y="Percent of 'All Nets' Access")

compare_nets_survey_level_wide <- compare_nets_survey_level_wide[order(`all nets`)]
compare_nets_survey_level_wide[, ordered_label_free_access:= factor(seq_len(.N), labels=survey_label)]

ggplot(compare_nets_survey_level_wide, aes(x=ordered_label_free_access),
       ) +
  geom_hline(yintercept=80) + 
  geom_bar(aes(y=`all nets`*100),  stat='identity', alpha=0.85, fill="#00BFC4") + 
  # geom_bar(aes(y=`free only`*100),  stat='identity', alpha=0.75, fill="#00BFC4") + 
  geom_text(aes(label=round(`all nets`*100, 0), y=`all nets`*100-5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x="",
       y="ITN Access")

ggplot(compare_nets_survey_level_wide, aes(x=ordered_label_free_access),
) +
  geom_hline(yintercept=80) + 
  geom_bar(aes(y=`all nets`*100),  stat='identity', alpha=0.85, fill="#f8766d") + 
  geom_bar(aes(y=`free only`*100),  stat='identity', alpha=1, fill="#00BFC4") + 
  geom_text(aes(label=round(percent_of_all, 0), y=`free only`*100-5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x="",
       y="ITN Access")


# now look on the quintile level
ggplot(compare_nets[type=="all nets"], aes(x=wealth_quintile, y=access*100, fill=wealth_quintile)) +
  geom_bar(stat="identity", alpha=0.85) + 
  geom_text(aes(label=round(access*100, 0),
                y=access*100+10)) +
  scale_fill_manual(values = rev(pnw_palette("Bay",5)),
                    name="Wealth Quintile") +
  facet_wrap(~survey_label) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x="",
       y="Access",
       title="Access (all ITNs)")


ggplot(compare_nets[type=="free only"], aes(x=wealth_quintile, y=access*100, fill=wealth_quintile)) +
  geom_bar(stat="identity", alpha=0.85) + 
  geom_text(aes(label=round(access*100, 0),
                y=access*100+10)) +
  scale_fill_manual(values = rev(pnw_palette("Bay",5)),
                    name="Wealth Quintile") +
  facet_wrap(~survey_label) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x="",
       y="Access",
       title="Access (free ITNs only)")

compare_nets_wide <- dcast.data.table(compare_nets,
                                      dhs_survey_id + country_name + name + iso3 + year +  survey_label + wealth_quintile + year ~ type,
                                      value.var = "access")

compare_nets_wide[, diff:= `all nets` - `free only`]
compare_nets_wide[, percent_of_all:=`free only` / `all nets` * 100]

ggplot(compare_nets_wide, aes(x=wealth_quintile, y=percent_of_all, fill=percent_of_all)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label=round(percent_of_all, 0),
                y=percent_of_all-15)) +
  # scale_fill_manual(values = rev(pnw_palette("Bay",5)),
  #                   name="Wealth Quintile") +
  scale_fill_distiller(palette = "RdYlBu", direction = 1, name="Percent") +
  theme_minimal() +
  facet_wrap(~survey_label) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x="",
        y="Percentage of Access Contributed by Free Nets" )


ggplot(compare_nets_wide, aes(x=wealth_quintile, fill=wealth_quintile)) +
  # geom_hline(yintercept=80) + 
  geom_bar(aes(y=`all nets`*100),  stat='identity', alpha=0.65) + 
  geom_bar(aes(y=`free only`*100),  stat='identity', alpha=0.75) + 
  geom_text(aes(label=round(percent_of_all, 0), y=`free only`*100-5)) +
  scale_fill_manual(values = rev(pnw_palette("Bay",5)),
                    name="Wealth Quintile") +
  theme_minimal() +
  facet_wrap(~survey_label) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x="",
       y="ITN Access")

# replicate the access gap plots from last script
ggplot(compare_gaps, aes(x=factor(yrs_since_campaign), 
                         y=access_gap, color=`Highest Access  Wealth Quintile`)) +
  # geom_violin(aes(group=yrs_since_campaign), alpha=0.5) +
  geom_boxplot(aes(group=yrs_since_campaign)) +
  geom_point() +
  # facet_grid(type~`Highest Access  Wealth Quintile`) +
  facet_grid(`Highest Access  Wealth Quintile`~ type) +
  scale_color_manual(values = rev(pnw_palette("Bay",5)),
                     name="Wealth Quintile\nwith Highest Access") +
  theme_minimal() +
  labs(x="Years Since Last Campaign",
       y="Access Gap")

ggplot(compare_nets, aes(x=access_rank_factor, fill=wealth_quintile)) +
  geom_bar()+
  scale_fill_manual(values = rev(pnw_palette("Bay",5)),
                    name="Wealth Quintile") +
  geom_text(aes(label=after_stat(count)), 
            stat='count', 
            position=position_stack(vjust=0.5))+
  facet_grid(.~type) +
  theme_minimal() +
  labs(x="",
       y="Count")

ggplot(compare_nets, aes(x=wealth_quintile, y=access, fill=wealth_quintile)) +
  geom_point(alpha=0.5) +
  geom_violin( alpha=0.5) +
  geom_boxplot(width=0.2) +
  scale_fill_manual(values = rev(pnw_palette("Bay",5)),
                    name="Wealth Quintile") +
  facet_grid(.~type) +
  theme_minimal() +
  labs(x="",
       y="ITN Access")

ggplot(compare_nets[type=="free only"],
       aes(x=year, y=access, color=wealth_quintile)) +
  geom_line() +
  geom_point() +
  facet_geo(~name, grid = ssa_grid, label="name") +
  scale_color_manual(values = rev(pnw_palette("Bay",5)),
                     name="Wealth Quintile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x="", y="ITN Access")


