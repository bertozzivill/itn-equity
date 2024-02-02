###############################################################################################################
## itn_equity_cluster.r
## Amelia Bertozzi-Villa
## January 2024
## 
## Explore the equity between wealth, access, and pfpr at the cluster level
##############################################################################################################
library(data.table)
library(ggplot2)
library(survey)
library(wesanderson)
library(PNWColors)
library(geofacet)
library(rasterVis)

options(digits=4)

rm(list=ls())

# filepaths
parent_dir <- "~/Dropbox (IDM)/Malaria Team Folder/projects/map_general/itn_equity/"
out_dir <- file.path(parent_dir, "results/gr_plots")
# choosing to keep only the surveys for which we have provenance data
input_data_fname <- file.path(parent_dir, "cleaned_input_data/itn_equity_free_nets_cleaned.csv")
function_fname <- "~/repos/itn-equity/itn_equity_functions.r"
source(function_fname)

# load itn data, both for the full time series and for surveys with provenance data only
itn_data <- fread(input_data_fname)
# itn_data_free <- fread(file.path(parent_dir, "cleaned_input_data/itn_equity_free_nets_cleaned.csv"))
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


#### explore cluster-level relationships across surveys

itn_data[, micro_value:= pmin(examined_micro, 1)]
itn_data[, rdt_value:= pmin(examined_rdt, 1)]



# keep only those surveys where they take any malaria tests at all
pfpr_surveys <- unique(itn_data[!is.na(examined_micro) | !is.na(examined_rdt)]$dhs_survey_id)
itn_data <- itn_data[dhs_survey_id %in% pfpr_surveys]

table(itn_data$dhs_survey_id, itn_data$micro_value, useNA = "ifany")
table(itn_data$dhs_survey_id, itn_data$rdt_value, useNA = "ifany")

# also open qs here about how to consider wealth quintile (which is household-level)
# as we explore the relationship with pfpr and access

# to start, just take raw cluster-level access and cluster-level pfpr

sum_vals <- c("itn_theoretical_capacity",
              "itn_theoretical_capacity_orig",
              "n_defacto_pop",
              "examined_micro",
              "positive_micro",
              "examined_rdt",
              "positive_rdt"
              )


# some places have lat/long as 0,0... convert these to null
itn_data[, loc_type:= ifelse(latitude==0 & longitude==0, 0, 
                             ifelse(is.na(latitude), NA,
                                    1))]
prop.table(table(itn_data$dhs_survey_id, itn_data$loc_type, useNA = "ifany"), margin=1)*100

itn_data[loc_type==0, latitude:=NA]
itn_data[loc_type==0, longitude:=NA]
itn_data[, loc_type:= NULL]


itn_by_cluster <- itn_data[, lapply(.SD, sum, na.rm=T),
                   .SDcols=sum_vals,
                   by=list(dhs_survey_id,
                           country_name,
                           year,
                           clusterid,
                           urban_rural,
                           latitude,
                           longitude
                   )]

itn_by_cluster[, access:= itn_theoretical_capacity/n_defacto_pop]
itn_by_cluster[, access_all_nets:= itn_theoretical_capacity_orig/n_defacto_pop]
itn_by_cluster[, prev_micro:=positive_micro/examined_micro]
itn_by_cluster[, prev_rdt:=positive_rdt/examined_rdt]

# what places don't have either rdt or microscopy data?
null_subset <- itn_by_cluster[is.na(prev_micro) & is.na(prev_rdt)]
table(null_subset$dhs_survey_id)
# just a few, except senegal 2020 with 83 null clusters? 40% of total! 
nrow(itn_by_cluster[dhs_survey_id=="SN2020MIS"])
nrow(null_subset[dhs_survey_id=="SN2020MIS"])/nrow(itn_by_cluster[dhs_survey_id=="SN2020MIS"])

# briefly: confirm that clusters always have the same hh_sample_weight... ok good
unique(itn_data[, list(n_weights=length(unique(hh_sample_wt))), 
         by=list(dhs_survey_id,
                 clusterid)]$n_weights)

ggplot(itn_by_cluster, aes(x=prev_micro, y=prev_rdt)) +
  geom_point(alpha=0.35)

ggplot(itn_by_cluster, aes(x=access, y=prev_rdt)) +
  geom_point()


ggplot(itn_by_cluster, aes(x=access, y=prev_micro)) +
  geom_point()

ggplot(itn_by_cluster, aes(x=access, y=prev_rdt, color=urban_rural)) +
  geom_point() +
  facet_wrap(~dhs_survey_id, scales="free")

# todo: map this

# take wealth index by cluster
wealth_by_cluster <- itn_data[, lapply(.SD, mean, na.rm=T),
                           .SDcols=c("wealth_index_score",
                                     "hh_sample_wt"),
                           by=list(dhs_survey_id,
                                   country_name,
                                   year,
                                   clusterid
                           )]


# merge onto itns
itn_by_cluster <- merge(itn_by_cluster, wealth_by_cluster, all.x=T)

itn_by_cluster[, survey_label:= paste(country_name, year)]

# scatter plot in every direction
ggplot(itn_by_cluster, aes(x=wealth_index_score, y=access_all_nets,
                           color=prev_rdt
                           )) +
  geom_point() +
  #geom_smooth(color="black") + 
  facet_wrap(~survey_label, scales="free_x") +
  scale_color_distiller(palette="RdYlBu") + 
  theme_minimal()


ggplot(itn_by_cluster, aes(x=wealth_index_score, y=access_all_nets)) +
  geom_point(aes(color=prev_rdt)) +
  geom_smooth(color="black") + 
  facet_wrap(~survey_label, scales="free") +
  scale_color_distiller(palette="RdYlBu") + 
  theme_minimal()

ggplot(itn_by_cluster, aes(x=longitude, y=latitude)) +
  geom_point(aes(color=prev_rdt)) +
  facet_wrap(~survey_label, scales="free") +
  scale_color_distiller(palette="RdYlBu") + 
  theme_minimal()

ggplot(itn_by_cluster, aes(x=wealth_index_score, y=prev_rdt)) +
  geom_point(aes(color=access)) +
  #geom_smooth(color="black") + 
  facet_wrap(~survey_label, scales="free") +
  scale_color_distiller(palette="YlGnBu", direction=1) + 
  theme_minimal()

ggplot(itn_by_cluster, aes(x=wealth_index_score, y=prev_rdt)) +
  geom_point(aes(color=access_all_nets)) +
  # geom_smooth(color="black") + 
  facet_wrap(~survey_label, scales="free") +
  scale_color_distiller(palette="YlGnBu", direction=1) + 
  theme_minimal()

ggplot(itn_by_cluster, aes(x=access, y=prev_rdt)) +
  geom_point(aes(color=wealth_index_score)) +
  geom_smooth(color="black") + 
  facet_wrap(~survey_label, scales="free") +
  scale_color_distiller(palette="YlGn", direction=1) + 
  theme_minimal()

ggplot(itn_by_cluster, aes(x=access, y=prev_rdt)) +
  geom_point(aes(color=urban_rural)) +
  geom_smooth(color="black") + 
  facet_wrap(~survey_label, scales="free") +
  #scale_color_distiller(palette="YlGn", direction=1) + 
  theme_minimal()

ggplot(itn_by_cluster, aes(x=access_all_nets, y=prev_rdt)) +
  geom_point(aes(color=wealth_index_score)) +
  # geom_smooth(color="black") + 
  facet_wrap(~survey_label, scales="free") +
  scale_color_distiller(palette="YlGn", direction=1) + 
  theme_minimal()



# bivariate heatmaps by quantile
# quantiles for access, wealth, and pfpr

# remove mauritania for now cuz it messes up the breaks
itn_by_cluster <- itn_by_cluster[dhs_survey_id!="MR2020DHS"]

n_quants <- 5
itn_by_cluster[, wealth_quantile:= cut(x = wealth_index_score, 
                                            breaks = wtd.quantile(x = wealth_index_score, 
                                                                  weights=hh_sample_wt,
                                                                  probs=(0:n_quants)/n_quants),
                                            labels = FALSE, include.lowest = TRUE),
         by=dhs_survey_id]

itn_by_cluster[, access_quantile:= cut(x = access, 
                                            breaks = wtd.quantile(x = access, 
                                                                  weights=hh_sample_wt,
                                                                  probs=(0:n_quants)/n_quants),
                                            labels = FALSE, include.lowest = TRUE),
               by=dhs_survey_id]



# some nulls in this caused by rounding errors in the maximum/minimum categories
if (nrow(itn_by_cluster[is.na(access_quantile)])>0){
  itn_by_cluster[, max_access:=max(access), by=.(dhs_survey_id)]
  itn_by_cluster[, min_access:=min(access), by=.(dhs_survey_id)]
  itn_by_cluster[access==max_access & is.na(access_quantile), access_quantile:= n_quants]
  itn_by_cluster[access==min_access & is.na(access_quantile), access_quantile:= 1]
  itn_by_cluster[, c("max_access", "min_access"):=NULL]
}


itn_by_cluster[, access_quantile_all_nets:= cut(x = access_all_nets, 
                                                breaks = wtd.quantile(x = access_all_nets, 
                                                                      weights=hh_sample_wt,
                                                                      probs=(0:n_quants)/n_quants),
                                                labels = FALSE, include.lowest = TRUE),
               by=dhs_survey_id]
if (nrow(itn_by_cluster[is.na(access_quantile_all_nets)])>0){
  itn_by_cluster[, max_access:=max(access_all_nets), by=.(dhs_survey_id)]
  itn_by_cluster[, min_access:=min(access_all_nets), by=.(dhs_survey_id)]
  itn_by_cluster[access_all_nets==max_access & is.na(access_quantile_all_nets), access_quantile_all_nets:= n_quants]
  itn_by_cluster[access_all_nets==min_access & is.na(access_quantile_all_nets), access_quantile_all_nets:= 1]
  itn_by_cluster[, c("max_access", "min_access"):=NULL]
}


quantile_means <- itn_by_cluster[, lapply(.SD, weighted.mean, w=hh_sample_wt, na.rm=T),
                                 .SDcols = c("wealth_index_score", "access", "prev_rdt"),
                                 by=list(dhs_survey_id,
                                         country_name,
                                         year,
                                         survey_label,
                                         wealth_quantile,
                                         access_quantile)]


# find national rdt prevalence by survey
national_prev <- itn_by_cluster[, list(nat_prev=weighted.mean(prev_rdt, hh_sample_wt,
                                                              na.rm=T)),
                                by=list(survey_label)]
national_prev <- national_prev[order(nat_prev)]
national_prev[, prev_order:= factor(seq_len(.N), labels=survey_label)]

quantile_means <- merge(quantile_means, national_prev, all.x=T)

# test_national_prev <- rbindlist(lapply(unique(itn_by_cluster$dhs_survey_id),
#                                        function(this_survey){
#   these_means <- summarize_survey(data=itn_by_cluster[dhs_survey_id==this_survey],
#                                   ids = "clusterid",
#                                   weight_vals = "hh_sample_wt",
#                                   metric_vals = "prev_rdt"
#   )
#   these_means[, dhs_survey_id:=this_survey]
# }))
# 
# test <- merge(national_prev, test_national_prev[, list(dhs_survey_id, test_prev=mean)])
# test[, diff:=nat_prev-test_prev]


######## FOR REVIEW ##################
example_survey <- "Cote d'Ivoire 2021" 
ggplot(quantile_means[survey_label==example_survey], 
       aes(x=wealth_quantile, y=access_quantile, fill=prev_rdt)) +
  geom_tile() + 
  scale_fill_distiller(palette="RdYlBu", 
                       name= "RDT\nPrevalence",
                       limits=c(0, 0.8)) + 
  facet_wrap(~survey_label) + 
  theme_minimal() +
  labs(x= "Wealth Quintile",
       y= "Access Quintile")
ggsave("cluster_grid_civ.svg", path=out_dir, width = 4.5, height=3)

ggplot(quantile_means[survey_label=="Benin 2017"], 
       aes(x=wealth_quantile, y=access_quantile, fill=prev_rdt)) +
  geom_tile() + 
  scale_fill_distiller(palette="RdYlBu", 
                       name= "RDT\nPrevalence",
                       limits=c(0, 0.8)) + 
  facet_wrap(~survey_label) + 
  theme_minimal() +
  labs(x= "Wealth Quintile",
       y= "Access Quintile")
ggsave("cluster_grid_ben.svg", path=out_dir, width = 4.5, height=3)


ggplot(quantile_means[survey_label=="Mozambique 2018"], 
       aes(x=wealth_quantile, y=access_quantile, fill=prev_rdt)) +
  geom_tile() + 
  scale_fill_distiller(palette="RdYlBu", 
                       name= "RDT\nPrevalence",
                       limits=c(0, 0.8)) + 
  facet_wrap(~survey_label) + 
  theme_minimal() +
  labs(x= "Wealth Quintile",
       y= "Access Quintile")
ggsave("cluster_grid_moz.svg", path=out_dir, width = 4.5, height=3)


ggplot(quantile_means[survey_label=="Uganda 2016"], 
       aes(x=wealth_quantile, y=access_quantile, fill=prev_rdt)) +
  geom_tile() + 
  scale_fill_distiller(palette="RdYlBu", 
                       name= "RDT\nPrevalence",
                       limits=c(0, 0.8)) + 
  facet_wrap(~survey_label) + 
  theme_minimal() +
  labs(x= "Wealth Quintile",
       y= "Access Quintile")
ggsave("cluster_grid_uga.svg", path=out_dir, width = 4.5, height=3)

ggplot(quantile_means, 
       aes(x=wealth_quantile, y=access_quantile, fill=prev_rdt)) +
  geom_tile() + 
  scale_fill_distiller(palette="RdYlBu", name= "RDT\nPrevalence") + 
  facet_wrap(~prev_order) + 
  theme_minimal() +
  labs(x= "Wealth Quintile",
       y= "Access Quintile")
ggsave("cluster_grid_all.svg", path=out_dir, width = 12, height=8)


ggplot(quantile_means, 
       aes(x=wealth_quantile, y=access_quantile, fill=prev_rdt)) +
  geom_tile(alpha=0.2) + 
  geom_tile(data=quantile_means[survey_label=="Benin 2017"]) + 
  scale_fill_distiller(palette="RdYlBu", name= "RDT\nPrevalence") + 
  facet_wrap(~prev_order) + 
  theme_minimal() +
  labs(x= "Wealth Quintile",
       y= "Access Quintile")

ggplot(quantile_means, 
       aes(x=wealth_quantile, y=access_quantile, fill=prev_rdt)) +
  geom_tile(alpha=0.2) + 
  geom_tile(data=quantile_means[survey_label=="Mozambique 2018"]) + 
  scale_fill_distiller(palette="RdYlBu", name= "RDT\nPrevalence") + 
  facet_wrap(~prev_order) + 
  theme_minimal() +
  labs(x= "Wealth Quintile",
       y= "Access Quintile")

ggplot(quantile_means, 
       aes(x=wealth_quantile, y=access_quantile, fill=prev_rdt)) +
  geom_tile(alpha=0.2) + 
  geom_tile(data=quantile_means[survey_label=="Uganda 2016"]) + 
  scale_fill_distiller(palette="RdYlBu", name= "RDT\nPrevalence") + 
  facet_wrap(~prev_order) + 
  theme_minimal() +
  labs(x= "Wealth Quintile",
       y= "Access Quintile")

######## FOR REVIEW ##################


ggplot(quantile_means, 
       aes(x=wealth_quantile, y=access_quantile, fill=access)) +
  geom_tile() + 
  scale_fill_distiller(palette="YlGnBu", direction = 1) + 
  facet_wrap(~survey_label) + 
  theme_minimal()

ggplot(quantile_means, 
       aes(x=wealth_quantile, y=access_quantile, fill=wealth_index_score)) +
  geom_tile() + 
  scale_fill_distiller(palette="YlGn", direction = 1) + 
  facet_wrap(~survey_label) + 
  theme_minimal()


quantile_means_all_nets <- itn_by_cluster[, lapply(.SD, weighted.mean, w=hh_sample_wt, na.rm=T),
                                 .SDcols = c("wealth_index_score", "access_all_nets", "prev_rdt"),
                                 by=list(dhs_survey_id,
                                         country_name,
                                         year,
                                         survey_label,
                                         wealth_quantile,
                                         access_quantile_all_nets)]

ggplot(quantile_means_all_nets, 
       aes(x=wealth_quantile, y=access_quantile_all_nets, fill=prev_rdt)) +
  geom_tile() + 
  scale_fill_distiller(palette="RdYlBu") + 
  facet_wrap(~survey_label) + 
  theme_minimal()


quantile_means_urban_rural <- itn_by_cluster[, lapply(.SD, weighted.mean, w=hh_sample_wt, na.rm=T),
                                 .SDcols = c("wealth_index_score", "access", "prev_rdt"),
                                 by=list(dhs_survey_id,
                                         country_name,
                                         year,
                                         survey_label,
                                         urban_rural,
                                         wealth_quantile,
                                         access_quantile)]

ggplot(quantile_means_urban_rural[!is.na(urban_rural)
                                  & country_name %in% c("Senegal")], 
       aes(x=wealth_quantile, y=access_quantile, fill=prev_rdt)) +
  geom_tile() + 
  scale_fill_distiller(palette="RdYlBu") + 
  facet_grid(urban_rural~survey_label) + 
  theme_minimal()


## also look at scatter plots with quantiles



# find cutoffs 
quantile_cutoffs <- itn_by_cluster[, 
                                   list(label="cluster",
                                    cutoff_perc=seq(0, 100, 20),
                                    access_all_nets_cutoff=wtd.quantile(x = access_all_nets, 
                                                        weights=hh_sample_wt,
                                                        probs=(0:n_quants)/n_quants),
                                    wealth_cutoff=wtd.quantile(x = wealth_index_score, 
                                                                  weights=hh_sample_wt,
                                                                  probs=(0:n_quants)/n_quants)
                                    ),
                  by=list(dhs_survey_id, survey_label)]




######## FOR REVIEW ##################
itn_by_cluster <- merge(itn_by_cluster, national_prev, by="survey_label", all.x=T)
quantile_cutoffs <- merge(quantile_cutoffs, national_prev, by="survey_label", all.x=T)


ggplot(itn_by_cluster[survey_label==example_survey], 
       aes(x=wealth_index_score, y=access_all_nets,
                           color=factor(wealth_quantile)
)) +
  geom_point() +
  geom_vline(data=quantile_cutoffs[survey_label==example_survey], 
             aes(xintercept=wealth_cutoff)) + 
  facet_wrap(~survey_label, scales="free") +
  scale_color_manual(values = rev(pnw_palette("Bay",5)),
                     name="Wealth Quintile") +
  theme_minimal() +
  labs(x="Wealth Index Score",
       y="Access (All Nets)")
ggsave("cluster_points_wealth_civ.svg", path=out_dir, width = 4.5, height=3)

ggplot(itn_by_cluster[survey_label==example_survey], 
       aes(x=wealth_index_score, y=access_all_nets,
                           color=factor(access_quantile_all_nets)
)) +
  geom_point() +
  geom_hline(data=quantile_cutoffs[survey_label==example_survey],
             aes(yintercept=access_all_nets_cutoff)) +
  facet_wrap(~survey_label, scales="free") +
  scale_color_manual(values = rev(pnw_palette("Bay",5)),
                     name="Access Quintile") +
  theme_minimal() +
  labs(x="Wealth Index Score",
       y="Access (All Nets)")
ggsave("cluster_points_access_civ.svg", path=out_dir, width = 4.5, height=3)


ggplot(itn_by_cluster[survey_label==example_survey], 
       aes(x=wealth_index_score, y=access_all_nets,
                           color=prev_rdt)) +
  geom_point() +
  geom_vline(data=quantile_cutoffs[survey_label==example_survey],
             aes(xintercept=wealth_cutoff)) + 
  geom_hline(data=quantile_cutoffs[survey_label==example_survey], 
             aes(yintercept=access_all_nets_cutoff)) +
  facet_wrap(~survey_label, scales="free") +
  scale_color_distiller(palette="RdYlBu", name="RDT\nPrevalence") +
  theme_minimal() +
  labs(x="Wealth Index Score",
       y="Access (All Nets)")
ggsave("cluster_points_civ.svg", path=out_dir, width = 4.5, height=3)


ggplot(itn_by_cluster[survey_label=="Benin 2017"], 
       aes(x=wealth_index_score, y=access_all_nets,
           color=prev_rdt)) +
  geom_point() +
  geom_vline(data=quantile_cutoffs[survey_label=="Benin 2017"],
             aes(xintercept=wealth_cutoff)) + 
  geom_hline(data=quantile_cutoffs[survey_label=="Benin 2017"], 
             aes(yintercept=access_all_nets_cutoff)) +
  facet_wrap(~survey_label, scales="free") +
  scale_color_distiller(palette="RdYlBu", name="RDT\nPrevalence") +
  theme_minimal() +
  labs(x="Wealth Index Score",
       y="Access (All Nets)")
ggsave("cluster_points_ben.svg", path=out_dir, width = 4.5, height=3)


ggplot(itn_by_cluster[survey_label=="Mozambique 2018"], 
       aes(x=wealth_index_score, y=access_all_nets,
           color=prev_rdt)) +
  geom_point() +
  geom_vline(data=quantile_cutoffs[survey_label=="Mozambique 2018"],
             aes(xintercept=wealth_cutoff)) + 
  geom_hline(data=quantile_cutoffs[survey_label=="Mozambique 2018"], 
             aes(yintercept=access_all_nets_cutoff)) +
  facet_wrap(~survey_label, scales="free") +
  scale_color_distiller(palette="RdYlBu", name="RDT\nPrevalence") +
  theme_minimal() +
  labs(x="Wealth Index Score",
       y="Access (All Nets)")
ggsave("cluster_points_moz.svg", path=out_dir, width = 4.5, height=3)


ggplot(itn_by_cluster[survey_label=="Uganda 2016"], 
       aes(x=wealth_index_score, y=access_all_nets,
           color=prev_rdt)) +
  geom_point() +
  geom_vline(data=quantile_cutoffs[survey_label=="Uganda 2016"],
             aes(xintercept=wealth_cutoff)) + 
  geom_hline(data=quantile_cutoffs[survey_label=="Uganda 2016"], 
             aes(yintercept=access_all_nets_cutoff)) +
  facet_wrap(~survey_label, scales="free") +
  scale_color_distiller(palette="RdYlBu", name="RDT\nPrevalence") +
  theme_minimal() +
  labs(x="Wealth Index Score",
       y="Access (All Nets)")
ggsave("cluster_points_uga.svg", path=out_dir, width = 4.5, height=3)



ggplot(itn_by_cluster, aes(x=wealth_index_score, y=access_all_nets,
                           color=prev_rdt)) +
  geom_point() +
  geom_vline(data=quantile_cutoffs, aes(xintercept=wealth_cutoff)) + 
  geom_hline(data=quantile_cutoffs, aes(yintercept=access_all_nets_cutoff)) +
  facet_wrap(~prev_order, scales="free") +
  scale_color_distiller(palette="RdYlBu", name="RDT\nPrevalence") +
  theme_minimal() +
  labs(x="Wealth Index Score",
       y="Access (All Nets)")
ggsave("cluster_points_all.svg", path=out_dir, width = 12, height=8)


######## FOR REVIEW ##################

ggplot(itn_by_cluster, aes(x=access, y=prev_rdt,
                           color=factor(wealth_quantile)
)) +
  geom_point() +
  # geom_smooth(color="black") + 
  facet_wrap(~survey_label, scales="free") +
  scale_color_manual(values = rev(pnw_palette("Bay",5)),
                     #name="Wealth Quintile"
                     ) +
  theme_minimal()

ggplot(itn_by_cluster[country_name=="Nigeria"], aes(x=access, y=prev_rdt,
                           color=factor(wealth_quantile)
)) +
  geom_point() +
  # geom_smooth(color="black") + 
  facet_grid(wealth_quantile~survey_label, scales="free") +
  scale_color_manual(values = rev(pnw_palette("Bay",5)),
                     #name="Wealth Quintile"
  ) +
  theme_minimal()


#  let's try keeping wealth in quintiles, but splitting prevalence and itn 
# coverage into absolute cutoffs
access_cutoffs <- seq(0,1,length.out=5)
prev_cutoffs <- c(0, 0.25, 0.75, 1)

itn_by_cluster[, access_group:= cut(x = access, 
                                       breaks = access_cutoffs,
                                       labels = FALSE, include.lowest = TRUE)]

itn_by_cluster[, prev_group:= cut(x = prev_rdt, 
                                    breaks = prev_cutoffs,
                                    labels = FALSE, include.lowest = TRUE)]


ggplot(itn_by_cluster, aes(x=access, y=prev_rdt,
                           color=factor(wealth_quantile)
)) +
  geom_point() +
  # geom_smooth(color="black") + 
  facet_wrap(~survey_label, scales="free") +
  scale_color_manual(values = rev(pnw_palette("Bay",5)), name="Wealth Quintile") +
  theme_minimal()

group_means_access <- itn_by_cluster[, lapply(.SD, weighted.mean, w=hh_sample_wt, na.rm=T),
                                 .SDcols = c("wealth_index_score", "access", "prev_rdt"),
                                 by=list(dhs_survey_id,
                                         country_name,
                                         year,
                                         survey_label,
                                         wealth_quantile,
                                         prev_group)]

ggplot(group_means_access, 
       aes(x=wealth_quantile, y=prev_group, fill=access)) +
  geom_tile() + 
  scale_fill_distiller(palette="YlGnBu", direction=1) + 
  facet_wrap(~survey_label) + 
  theme_minimal()

group_means_prev <- itn_by_cluster[, lapply(.SD, weighted.mean, w=hh_sample_wt, na.rm=T),
                                     .SDcols = c("wealth_index_score", "access", "prev_rdt"),
                                     by=list(dhs_survey_id,
                                             country_name,
                                             year,
                                             survey_label,
                                             wealth_quantile,
                                             access_group)]

# much less interesting, don't
ggplot(group_means_prev, 
       aes(x=wealth_quantile, y=access_group, fill=prev_rdt)) +
  geom_tile() + 
  scale_fill_distiller(palette="RdYlBu") + 
  facet_wrap(~survey_label) + 
  theme_minimal()



# to consider adding wealth quantile: we could either calculate wealth quintile by *cluster*, 
# or we could aggregate up the households.
# what are the quintile cutoffs for this compared to the quintile cutoffs overall?

compare_cutoffs <- rbind(wealth_by_cluster[, list(label="cluster",
                                                  cutoff_perc=seq(0, 100, 20),
                                                  cutoff=wtd.quantile(x=wealth_index_score,
                                                                      weights=hh_sample_wt,
                                                                      probs=0:5/5)),
                                           by=dhs_survey_id],
                         itn_data[, list(label="household",
                                                  cutoff_perc=seq(0, 100, 20),
                                                  cutoff=wtd.quantile(x=wealth_index_score,
                                                                      weights=hh_sample_wt,
                                                                      probs=0:5/5)),
                                           by=dhs_survey_id])

ggplot(compare_cutoffs, aes(x=cutoff_perc, y=cutoff, fill=label)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~dhs_survey_id, scales="free")


